//! Resolution of references that Resolve deferred across replaceable edges.
//!
//! Resolve keeps the replaceable declaration in a deferred reference because
//! the concrete selection is instance dependent. Instantiation owns that
//! selection, so it can prove the final member declarations of equations,
//! statements, and expressions without rewriting source aliases.

use super::component_type_selection::{apply_type_override, is_predefined_identity};
use super::override_map::TypeOverrideMap;
use super::selected_class_members::resolve_member_view_in_class;
use crate::nested_scope::NestedComponentTypeSelections;
use crate::{InstantiateError, InstantiateResult};
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::visitor::{
    CalleeSite, ComponentReferenceSite, ExpressionTransformer, SemanticReferenceEditor,
    transform_callee_in_place, transform_component_reference_in_place,
    transform_expression_in_place, transform_for_index_in_place, transform_subscripts_in_place,
};
use rustc_hash::FxHashMap;
use std::sync::Arc;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum IssuedComponentType {
    /// A direct predefined type has no `ClassDef` and needs no projection.
    DirectPredefined,
    /// Exact selected type identity, including a redeclaration to a predefined
    /// type. Structured identities can prove a deferred member tail.
    Exact(DefId),
}

/// Complete selected-type plan for one class occurrence.
///
/// The map is private so absence cannot be manufactured or interpreted as
/// "keep the declaration type". Its sole production issuer visits the complete
/// effective component inventory before any component expression is consumed.
pub(crate) struct SelectedComponentTypes {
    by_declaration: FxHashMap<DefId, IssuedComponentType>,
}

impl SelectedComponentTypes {
    fn empty() -> Self {
        Self {
            by_declaration: FxHashMap::default(),
        }
    }

    #[cfg(test)]
    pub(crate) fn empty_for_test() -> Self {
        Self::empty()
    }

    fn insert(
        &mut self,
        declaration: DefId,
        selection: IssuedComponentType,
    ) -> Option<IssuedComponentType> {
        self.by_declaration.insert(declaration, selection)
    }

    fn selection(&self, declaration: DefId) -> Option<IssuedComponentType> {
        self.by_declaration.get(&declaration).copied()
    }

    fn selected_class(&self, declaration: DefId) -> Option<DefId> {
        match self.selection(declaration) {
            Some(IssuedComponentType::Exact(selected)) => Some(selected),
            Some(IssuedComponentType::DirectPredefined) | None => None,
        }
    }

    #[cfg(test)]
    pub(super) fn one_structured_for_test(declaration: DefId, selected: DefId) -> Self {
        let mut plan = Self::empty();
        plan.insert(declaration, IssuedComponentType::Exact(selected));
        plan
    }
}

/// Immutable selected-type plans indexed by the structured occurrence that
/// issued them during the one recursive instantiation traversal.
pub(crate) struct SelectedComponentTypeCatalog {
    by_occurrence: FxHashMap<ast::QualifiedName, Arc<SelectedComponentTypes>>,
}

impl SelectedComponentTypeCatalog {
    pub(crate) fn new() -> Self {
        Self {
            by_occurrence: FxHashMap::default(),
        }
    }

    pub(crate) fn issue(
        &mut self,
        occurrence: ast::QualifiedName,
        plan: Arc<SelectedComponentTypes>,
    ) -> InstantiateResult<()> {
        match self.by_occurrence.entry(occurrence) {
            std::collections::hash_map::Entry::Occupied(entry) => {
                Err(Box::new(InstantiateError::missing_source_context(format!(
                    "class occurrence `{}` attempted to issue its selected-type plan twice",
                    entry.key()
                ))))
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(plan);
                Ok(())
            }
        }
    }

    pub(crate) fn plan(&self, occurrence: &ast::QualifiedName) -> Option<&SelectedComponentTypes> {
        self.by_occurrence.get(occurrence).map(Arc::as_ref)
    }

    #[cfg(test)]
    pub(super) fn remove_for_test(
        &mut self,
        occurrence: &ast::QualifiedName,
    ) -> Option<Arc<SelectedComponentTypes>> {
        self.by_occurrence.remove(occurrence)
    }
}

/// Issue the selected class of every component declaration in one class
/// occurrence before any component binding is consumed.
///
/// Modelica permits a modifier on an early component to reference a later
/// sibling, so component iteration order cannot own this fact. The complete
/// effective declaration set and exact type-override catalog do.
pub(crate) fn issue_selected_component_types(
    tree: &ast::ClassTree,
    effective_components: &ast::AstIndexMap<String, ast::Component>,
    overrides: &TypeOverrideMap,
    mut nested_selections: Option<NestedComponentTypeSelections>,
) -> InstantiateResult<SelectedComponentTypes> {
    let mut selections = SelectedComponentTypes::empty();
    for component in effective_components.values() {
        let component_def_id = component.def_id.ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!("component type-selection owner `{}`", component.name),
                component.location.span(),
            ))
        })?;
        let occurrence_selected_type = nested_selections
            .as_mut()
            .and_then(|selections| selections.consume(component_def_id));
        let selected = match occurrence_selected_type {
            Some(selected_type_def_id) => std::borrow::Cow::Owned(ast::Component {
                type_def_id: Some(selected_type_def_id),
                ..component.clone()
            }),
            None => apply_type_override(tree, component, overrides)?,
        };
        let issued_type = match selected.type_def_id {
            Some(selected_type_def_id)
                if tree.get_class_by_def_id(selected_type_def_id).is_some()
                    || is_predefined_identity(tree, selected_type_def_id) =>
            {
                IssuedComponentType::Exact(selected_type_def_id)
            }
            Some(selected_type_def_id) => {
                return Err(Box::new(InstantiateError::missing_resolved_identity(
                    format!(
                        "selected type {selected_type_def_id:?} of component `{}`",
                        component.name
                    ),
                    component.location.span(),
                )));
            }
            None if selected.type_name.name.len() == 1
                && selected
                    .type_name
                    .def_id
                    .is_some_and(|def_id| is_predefined_identity(tree, def_id)) =>
            {
                IssuedComponentType::DirectPredefined
            }
            None => {
                return Err(Box::new(InstantiateError::missing_resolved_identity(
                    format!("selected type of component `{}`", component.name),
                    component.location.span(),
                )));
            }
        };
        if let Some(previous) = selections.insert(component_def_id, issued_type)
            && previous != issued_type
        {
            return Err(Box::new(InstantiateError::redeclare_error(
                component.name.as_str(),
                format!(
                    "one effective component declaration selects conflicting type facts {previous:?} and {issued_type:?}"
                ),
                component.location.span(),
            )));
        }
    }
    if let Some(nested_selections) = nested_selections {
        nested_selections.finish()?;
    }
    Ok(selections)
}

/// Project one component declaration through the already-issued selection
/// plan. This performs no lookup and cannot choose a different type than the
/// class-occurrence owner selected.
pub(crate) fn component_with_issued_type_selection<'a>(
    component: &'a ast::Component,
    selections: &SelectedComponentTypes,
) -> InstantiateResult<std::borrow::Cow<'a, ast::Component>> {
    let component_def_id = component.def_id.ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("component type-selection owner `{}`", component.name),
            component.location.span(),
        ))
    })?;
    let selected_type_def_id = match selections.selection(component_def_id) {
        Some(IssuedComponentType::DirectPredefined) => {
            return Ok(std::borrow::Cow::Borrowed(component));
        }
        Some(IssuedComponentType::Exact(selected_type_def_id)) => selected_type_def_id,
        None => {
            return Err(Box::new(InstantiateError::missing_source_context(format!(
                "component `{}` is absent from its class-occurrence selected-type plan",
                component.name
            ))));
        }
    };
    if component.type_def_id == Some(selected_type_def_id) {
        return Ok(std::borrow::Cow::Borrowed(component));
    }
    let mut selected = component.clone();
    selected.type_def_id = Some(selected_type_def_id);
    Ok(std::borrow::Cow::Owned(selected))
}

pub(crate) fn resolve_dynamic_expression_targets_at_occurrence(
    tree: &ast::ClassTree,
    overrides: &TypeOverrideMap,
    catalog: &SelectedComponentTypeCatalog,
    occurrence: &ast::QualifiedName,
    selected_component_types: &SelectedComponentTypes,
    mut expression: ast::Expression,
) -> InstantiateResult<ast::Expression> {
    let mut resolver = DynamicExpressionTargetResolver::new_at_occurrence(
        tree,
        overrides,
        catalog,
        occurrence,
        selected_component_types,
    );
    transform_expression_in_place(&mut resolver, &mut expression);
    resolver.finish(expression)
}

pub(crate) fn resolve_dynamic_equation_targets_at_occurrence(
    tree: &ast::ClassTree,
    overrides: &TypeOverrideMap,
    catalog: &SelectedComponentTypeCatalog,
    occurrence: &ast::QualifiedName,
    selected_component_types: &SelectedComponentTypes,
    mut equation: ast::Equation,
) -> InstantiateResult<ast::Equation> {
    let mut resolver = DynamicExpressionTargetResolver::new_at_occurrence(
        tree,
        overrides,
        catalog,
        occurrence,
        selected_component_types,
    );
    resolver.resolve_equation(&mut equation);
    resolver.finish(equation)
}

pub(crate) fn resolve_dynamic_statement_targets_at_occurrence(
    tree: &ast::ClassTree,
    overrides: &TypeOverrideMap,
    catalog: &SelectedComponentTypeCatalog,
    occurrence: &ast::QualifiedName,
    selected_component_types: &SelectedComponentTypes,
    mut statement: ast::Statement,
) -> InstantiateResult<ast::Statement> {
    let mut resolver = DynamicExpressionTargetResolver::new_at_occurrence(
        tree,
        overrides,
        catalog,
        occurrence,
        selected_component_types,
    );
    resolver.resolve_statement(&mut statement);
    resolver.finish(statement)
}

pub(crate) fn resolve_dynamic_subscript_targets_at_occurrence(
    tree: &ast::ClassTree,
    overrides: &TypeOverrideMap,
    catalog: &SelectedComponentTypeCatalog,
    occurrence: &ast::QualifiedName,
    selected_component_types: &SelectedComponentTypes,
    mut subscripts: Vec<ast::Subscript>,
) -> InstantiateResult<Vec<ast::Subscript>> {
    let mut resolver = DynamicExpressionTargetResolver::new_at_occurrence(
        tree,
        overrides,
        catalog,
        occurrence,
        selected_component_types,
    );
    transform_subscripts_in_place(&mut resolver, &mut subscripts);
    resolver.finish(subscripts)
}

struct DynamicExpressionTargetResolver<'a> {
    tree: &'a ast::ClassTree,
    overrides: &'a TypeOverrideMap,
    occurrence_catalog: &'a SelectedComponentTypeCatalog,
    source_occurrence: &'a ast::QualifiedName,
    selected_component_types: &'a SelectedComponentTypes,
    error: Option<Box<InstantiateError>>,
}

impl ExpressionTransformer for DynamicExpressionTargetResolver<'_> {
    fn transform_component_reference(
        &mut self,
        reference: SemanticReferenceEditor<'_>,
        _site: ComponentReferenceSite,
    ) {
        self.resolve_reference(reference);
    }

    fn transform_callee(&mut self, callee: SemanticReferenceEditor<'_>, _site: CalleeSite) {
        self.resolve_reference(callee);
    }
}

impl DynamicExpressionTargetResolver<'_> {
    /// Prove the deferred suffix of one reference. Part subscripts are already
    /// traversed by the kernel before either hook lands here.
    fn resolve_reference(&mut self, mut reference: SemanticReferenceEditor<'_>) {
        if self.error.is_some() || reference.view().target_def_id().is_some() {
            return;
        }
        let anchor = match self.selected_resolution_anchor(reference.view()) {
            Ok(Some(anchor)) => anchor,
            Ok(None) => return,
            Err(error) => {
                self.error = Some(error);
                return;
            }
        };
        match resolve_member_view_in_class(
            self.tree,
            anchor.selected_class_def_id,
            reference.view(),
            anchor.suffix_start,
        ) {
            Ok(identities) => {
                for (mut slot, def_id) in reference
                    .part_identity_slots()
                    .skip(anchor.suffix_start)
                    .zip(identities)
                {
                    slot.set_def_id(def_id);
                }
            }
            Err(error) => self.error = Some(error),
        }
    }
}

/// The class from which the unresolved suffix of a deferred reference is
/// proved, together with the index of the first suffix part.
struct ResolutionAnchor {
    selected_class_def_id: DefId,
    suffix_start: usize,
}

/// The deepest part of the contiguous resolved prefix, with its identity.
///
/// Resolve records a deferred reference as a contiguous run of resolved
/// `ComponentRefPart` identities followed by the unresolved suffix; the run's
/// last `DefId` names the instance-dependent edge the traversal stopped at.
fn resolved_prefix_boundary(reference: ast::ComponentReferenceView<'_>) -> Option<(usize, DefId)> {
    let mut boundary = None;
    for (index, part) in reference.parts().enumerate() {
        match part.def_id() {
            Some(def_id) => boundary = Some((index, def_id)),
            None => break,
        }
    }
    boundary
}

impl DynamicExpressionTargetResolver<'_> {
    fn new_at_occurrence<'a>(
        tree: &'a ast::ClassTree,
        overrides: &'a TypeOverrideMap,
        occurrence_catalog: &'a SelectedComponentTypeCatalog,
        source_occurrence: &'a ast::QualifiedName,
        selected_component_types: &'a SelectedComponentTypes,
    ) -> DynamicExpressionTargetResolver<'a> {
        DynamicExpressionTargetResolver {
            tree,
            overrides,
            occurrence_catalog,
            source_occurrence,
            selected_component_types,
            error: None,
        }
    }

    fn finish<T>(self, value: T) -> InstantiateResult<T> {
        match self.error {
            Some(error) => Err(error),
            None => Ok(value),
        }
    }

    /// Choose the proof anchor for a deferred reference from its contiguous
    /// resolved prefix, the identity fact Resolve issued on the reference.
    ///
    /// The deepest prefix part carrying an issued selection wins: a
    /// replaceable class alias selects through the override catalog, a
    /// component through the type of its instantiated occurrence. Without any
    /// issued selection, a boundary that itself denotes a class is its own
    /// exact selection (a replaceable alias left at its declared default).
    /// A component boundary without an issued occurrence selection proves no
    /// member set here: its suffix stays unresolved for the phase that owns
    /// it, never guessed from a spelling or a declared type name.
    fn selected_resolution_anchor(
        &self,
        reference: ast::ComponentReferenceView<'_>,
    ) -> InstantiateResult<Option<ResolutionAnchor>> {
        self.selected_resolution_anchor_in_occurrence(
            reference,
            self.occurrence_catalog,
            self.source_occurrence,
            self.selected_component_types,
        )
    }

    fn selected_resolution_anchor_in_occurrence<'a>(
        &self,
        reference: ast::ComponentReferenceView<'_>,
        catalog: &'a SelectedComponentTypeCatalog,
        source_occurrence: &ast::QualifiedName,
        source_plan: &'a SelectedComponentTypes,
    ) -> InstantiateResult<Option<ResolutionAnchor>> {
        let Some((boundary_index, boundary_def_id)) = resolved_prefix_boundary(reference) else {
            return Ok(None);
        };
        let mut occurrence = source_occurrence.clone();
        let mut occurrence_plan = source_plan;
        let mut deepest = None;

        for (index, part) in reference.parts().enumerate().take(boundary_index + 1) {
            let Some(def_id) = part.def_id() else {
                return Ok(None);
            };
            if let Some(selected_class_def_id) = self.overrides.target_for_alias_def_id(def_id) {
                deepest = Some(ResolutionAnchor {
                    selected_class_def_id,
                    suffix_start: index + 1,
                });
            }
            if let Some(anchor) = advance_occurrence_selection(
                catalog,
                &mut occurrence,
                &mut occurrence_plan,
                part.ident_text(),
                def_id,
                index,
            )? {
                deepest = Some(anchor);
            }
        }

        Ok(deepest.or_else(|| {
            self.tree
                .get_class_by_def_id(boundary_def_id)
                .is_some()
                .then_some(ResolutionAnchor {
                    selected_class_def_id: boundary_def_id,
                    suffix_start: boundary_index + 1,
                })
        }))
    }

    fn resolve_equation(&mut self, equation: &mut ast::Equation) {
        match equation {
            ast::Equation::Empty => {}
            ast::Equation::Simple { lhs, rhs } => {
                transform_expression_in_place(self, lhs);
                transform_expression_in_place(self, rhs);
            }
            ast::Equation::Connect { lhs, rhs } => {
                transform_component_reference_in_place(self, lhs);
                transform_component_reference_in_place(self, rhs);
            }
            ast::Equation::For { indices, equations } => {
                for index in indices {
                    transform_for_index_in_place(self, index);
                }
                self.resolve_equations(equations);
            }
            ast::Equation::When(blocks) => {
                for block in blocks {
                    self.resolve_equation_block(block);
                }
            }
            ast::Equation::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    self.resolve_equation_block(block);
                }
                if let Some(equations) = else_block {
                    self.resolve_equations(equations);
                }
            }
            ast::Equation::FunctionCall { comp, args, .. } => {
                transform_callee_in_place(self, comp);
                for argument in args {
                    transform_expression_in_place(self, argument);
                }
            }
            ast::Equation::Assert {
                condition,
                message,
                level,
            } => {
                transform_expression_in_place(self, condition);
                transform_expression_in_place(self, message);
                if let Some(level) = level {
                    transform_expression_in_place(self, level);
                }
            }
        }
    }

    fn resolve_equations(&mut self, equations: &mut [ast::Equation]) {
        for equation in equations {
            self.resolve_equation(equation);
        }
    }

    fn resolve_equation_block(&mut self, block: &mut ast::EquationBlock) {
        transform_expression_in_place(self, &mut block.cond);
        self.resolve_equations(&mut block.eqs);
    }

    fn resolve_statement(&mut self, statement: &mut ast::Statement) {
        match statement {
            ast::Statement::Empty
            | ast::Statement::Return { .. }
            | ast::Statement::Break { .. } => {}
            ast::Statement::Assignment { comp, value } => {
                transform_component_reference_in_place(self, comp);
                transform_expression_in_place(self, value);
            }
            ast::Statement::For { indices, equations } => {
                for index in indices {
                    transform_for_index_in_place(self, index);
                }
                self.resolve_statements(equations);
            }
            ast::Statement::While(block) => self.resolve_statement_block(block),
            ast::Statement::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    self.resolve_statement_block(block);
                }
                if let Some(statements) = else_block {
                    self.resolve_statements(statements);
                }
            }
            ast::Statement::When(blocks) => {
                for block in blocks {
                    self.resolve_statement_block(block);
                }
            }
            ast::Statement::FunctionCall {
                comp,
                args,
                outputs,
            } => {
                transform_callee_in_place(self, comp);
                for argument in args {
                    transform_expression_in_place(self, argument);
                }
                for output in outputs {
                    transform_expression_in_place(self, output);
                }
            }
            ast::Statement::Reinit { variable, value } => {
                transform_component_reference_in_place(self, variable);
                transform_expression_in_place(self, value);
            }
            ast::Statement::Assert {
                condition,
                message,
                level,
            } => {
                transform_expression_in_place(self, condition);
                transform_expression_in_place(self, message);
                if let Some(level) = level {
                    transform_expression_in_place(self, level.as_mut());
                }
            }
        }
    }

    fn resolve_statements(&mut self, statements: &mut [ast::Statement]) {
        for statement in statements {
            self.resolve_statement(statement);
        }
    }

    fn resolve_statement_block(&mut self, block: &mut ast::StatementBlock) {
        transform_expression_in_place(self, &mut block.cond);
        self.resolve_statements(&mut block.stmts);
    }
}

fn advance_occurrence_selection<'a>(
    catalog: &'a SelectedComponentTypeCatalog,
    occurrence: &mut ast::QualifiedName,
    occurrence_plan: &mut &'a SelectedComponentTypes,
    part_ident: &str,
    def_id: DefId,
    index: usize,
) -> InstantiateResult<Option<ResolutionAnchor>> {
    let Some(selected_class_def_id) = occurrence_plan.selected_class(def_id) else {
        return Ok(None);
    };
    *occurrence = occurrence.child(part_ident);
    *occurrence_plan = catalog.plan(occurrence).ok_or_else(|| {
        Box::new(InstantiateError::missing_source_context(format!(
            "selected component occurrence `{occurrence}` has no exact child component-type plan"
        )))
    })?;
    Ok(Some(ResolutionAnchor {
        selected_class_def_id,
        suffix_start: index + 1,
    }))
}
