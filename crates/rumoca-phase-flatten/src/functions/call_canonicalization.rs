//! Canonicalization of function-call identity in a flat model.
//!
//! Two rewrites live here. The collected-table canonicalizer restates every
//! call site against the exact function the collection phase inserted, so a
//! callee is named by the table's key and carries its exact
//! [`rumoca_core::FunctionInstanceId`]. The scoped canonicalizer resolves a
//! call written in a caller's lexical scope (MLS §5.3) to the qualified name
//! the class tree exposes, before the callable is collected at all.

use super::*;

pub(crate) fn canonicalize_collected_function_calls(
    flat: &mut flat::Model,
    class_index: &ast::ClassDefIndex<'_>,
) -> Result<(), FlattenError> {
    let canonical_functions = collect_canonical_functions(flat)?;
    if canonical_functions.is_empty() {
        return Ok(());
    }
    let mut rewriter = CollectedFunctionCallCanonicalizer {
        canonical_functions,
        class_index,
        nonreplaceability_by_path: HashMap::new(),
        error: None,
    };

    for var in flat.variables.values_mut() {
        if let Some(binding) = &mut var.binding {
            *binding = rewriter.rewrite_expression(binding);
        }
        if let Some(start) = &mut var.start {
            *start = rewriter.rewrite_expression(start);
        }
        if let Some(min) = &mut var.min {
            *min = rewriter.rewrite_expression(min);
        }
        if let Some(max) = &mut var.max {
            *max = rewriter.rewrite_expression(max);
        }
        if let Some(nominal) = &mut var.nominal {
            *nominal = rewriter.rewrite_expression(nominal);
        }
    }
    for eq in &mut flat.equations {
        eq.residual = rewriter.rewrite_expression(&eq.residual);
    }
    for eq in &mut flat.initial_equations {
        eq.residual = rewriter.rewrite_expression(&eq.residual);
    }
    // A structured family's comprehension template is a peer copy of its scalar
    // residual, so its calls must be given the same exact instance identity here.
    // Argument-slot materialization reads templates too, and an uncanonicalized
    // template would reach it carrying only declaration identity.
    for family in flat
        .structured_equations
        .iter_mut()
        .chain(flat.initial_structured_equations.iter_mut())
    {
        if let Some(template) = &mut family.template {
            template.body = rewriter.rewrite_expressions(&template.body);
        }
    }
    for chain in &mut flat.when_chains {
        for branch in chain.branches_mut() {
            branch.condition = rewriter.rewrite_expression(&branch.condition);
            canonicalize_when_equations(&mut branch.equations, &mut rewriter);
        }
    }
    for assertion in &mut flat.assert_equations {
        assertion.condition = rewriter.rewrite_expression(&assertion.condition);
        assertion.message = rewriter.rewrite_expression(&assertion.message);
        if let Some(level) = &mut assertion.level {
            *level = rewriter.rewrite_expression(level);
        }
    }
    for assertion in &mut flat.initial_assert_equations {
        assertion.condition = rewriter.rewrite_expression(&assertion.condition);
        assertion.message = rewriter.rewrite_expression(&assertion.message);
        if let Some(level) = &mut assertion.level {
            *level = rewriter.rewrite_expression(level);
        }
    }
    for algorithm in &mut flat.algorithms {
        for statement in &mut algorithm.statements {
            *statement = rewriter.rewrite_statement(statement);
        }
    }
    for algorithm in &mut flat.initial_algorithms {
        for statement in &mut algorithm.statements {
            *statement = rewriter.rewrite_statement(statement);
        }
    }
    for function in flat.functions.values_mut() {
        for param in function
            .inputs
            .iter_mut()
            .chain(function.outputs.iter_mut())
            .chain(function.locals.iter_mut())
        {
            canonicalize_function_param_expressions(param, &mut rewriter);
        }
        for statement in &mut function.body {
            *statement = rewriter.rewrite_statement(statement);
        }
    }
    match rewriter.error {
        Some(error) => Err(error),
        None => Ok(()),
    }
}

fn collect_canonical_functions(flat: &flat::Model) -> Result<CanonicalFunctionIndex, FlattenError> {
    let mut index = CanonicalFunctionIndex::default();
    for function in flat.functions.values() {
        let instance_id = function
            .instance_id
            .ok_or_else(|| FlattenError::internal("missing function instance identity"))?;
        index.insert(CanonicalFunction {
            name: function.name.clone(),
            def_id: function.def_id,
            instance_id,
            is_constructor: function.is_constructor,
        })?;
    }
    Ok(index)
}

fn canonicalize_when_equations(
    equations: &mut [flat::WhenEquation],
    rewriter: &mut CollectedFunctionCallCanonicalizer,
) {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                *value = rewriter.rewrite_expression(value);
            }
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                *condition = rewriter.rewrite_expression(condition);
                *message = rewriter.rewrite_expression(message);
                if let Some(level) = level.as_deref_mut() {
                    *level = rewriter.rewrite_expression(level);
                }
            }
            flat::WhenEquation::Terminate { message, .. } => {
                *message = rewriter.rewrite_expression(message);
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, branch_equations) in branches {
                    *condition = rewriter.rewrite_expression(condition);
                    canonicalize_when_equations(branch_equations, rewriter);
                }
                if let Some(else_branch) = else_branch {
                    canonicalize_when_equations(else_branch, rewriter);
                }
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                *function = rewriter.rewrite_expression(function);
            }
        }
    }
}

/// Canonicalize every parameter expression argument-slot materialization also
/// visits, so no attribute expression reaches it without exact call identity.
fn canonicalize_function_param_expressions(
    param: &mut rumoca_core::FunctionParam,
    rewriter: &mut CollectedFunctionCallCanonicalizer,
) {
    for expression in [&mut param.default, &mut param.min, &mut param.max]
        .into_iter()
        .flatten()
    {
        *expression = rewriter.rewrite_expression(expression);
    }
    for subscript in &mut param.shape_expr {
        if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
            **expr = rewriter.rewrite_expression(expr);
        }
    }
}

struct CanonicalFunction {
    name: rumoca_core::VarName,
    def_id: Option<rumoca_core::DefId>,
    instance_id: rumoca_core::FunctionInstanceId,
    is_constructor: bool,
}

#[derive(Clone, Copy)]
struct CanonicalFunctionMatch<'a> {
    function: &'a CanonicalFunction,
    exact_instance: bool,
}

#[derive(Default)]
struct CanonicalFunctionIndex {
    entries: Vec<CanonicalFunction>,
    by_name: HashMap<rumoca_core::VarName, usize>,
    by_instance: HashMap<rumoca_core::FunctionInstanceId, usize>,
    unique_by_def: HashMap<rumoca_core::DefId, Option<usize>>,
}

impl CanonicalFunctionIndex {
    fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    fn insert(&mut self, function: CanonicalFunction) -> Result<(), FlattenError> {
        let entry = self.entries.len();
        if self.by_name.insert(function.name.clone(), entry).is_some() {
            return Err(FlattenError::internal(format!(
                "duplicate canonical function name `{}`",
                function.name
            )));
        }
        if self
            .by_instance
            .insert(function.instance_id, entry)
            .is_some()
        {
            return Err(FlattenError::internal(format!(
                "duplicate canonical function instance {}",
                function.instance_id.index()
            )));
        }
        if let Some(def_id) = function.def_id {
            self.unique_by_def
                .entry(def_id)
                .and_modify(|candidate| *candidate = None)
                .or_insert(Some(entry));
        }
        self.entries.push(function);
        Ok(())
    }

    fn by_reference(
        &self,
        reference: &rumoca_core::Reference,
    ) -> Option<CanonicalFunctionMatch<'_>> {
        if let Some(resolved) = reference.resolved_function() {
            return self.by_instance.get(&resolved.instance_id).map(|entry| {
                CanonicalFunctionMatch {
                    function: &self.entries[*entry],
                    exact_instance: true,
                }
            });
        }
        if let Some(entry) = self.by_name.get(reference.var_name()) {
            return Some(CanonicalFunctionMatch {
                function: &self.entries[*entry],
                exact_instance: false,
            });
        }
        self.sole_by_def(reference.target_def_id()?)
            .map(|function| CanonicalFunctionMatch {
                function,
                exact_instance: false,
            })
    }

    fn sole_by_def(&self, def_id: rumoca_core::DefId) -> Option<&CanonicalFunction> {
        self.unique_by_def
            .get(&def_id)
            .copied()
            .flatten()
            .map(|entry| &self.entries[entry])
    }
}

struct CollectedFunctionCallCanonicalizer<'a> {
    canonical_functions: CanonicalFunctionIndex,
    class_index: &'a ast::ClassDefIndex<'a>,
    nonreplaceability_by_path: HashMap<(bool, Vec<rumoca_core::DefId>), bool>,
    error: Option<FlattenError>,
}

impl CollectedFunctionCallCanonicalizer<'_> {
    fn record_call_kind_conflict(
        &mut self,
        name: &rumoca_core::Reference,
        occurrence_is_constructor: bool,
        canonical_instance: rumoca_core::FunctionInstanceId,
        canonical_is_constructor: bool,
        span: rumoca_core::Span,
    ) {
        if occurrence_is_constructor && !canonical_is_constructor {
            self.error.get_or_insert_with(|| {
                FlattenError::inconsistent_function_call_kind(
                    name.as_str(),
                    canonical_instance,
                    span,
                )
            });
        }
    }

    fn canonical_function_for_reference(
        &self,
        reference: &rumoca_core::Reference,
    ) -> Option<CanonicalFunctionMatch<'_>> {
        self.canonical_functions.by_reference(reference)
    }

    fn sole_canonical_function_for_declaration(
        &self,
        def_id: rumoca_core::DefId,
    ) -> Option<&CanonicalFunction> {
        self.canonical_functions.sole_by_def(def_id)
    }

    fn occurrence_proves_transitive_nonreplaceability(
        &mut self,
        reference: &rumoca_core::Reference,
    ) -> bool {
        let Some(component) = reference.component_ref() else {
            return false;
        };
        let is_structured = component
            .parts()
            .iter()
            .all(|part| !part.ident.contains('.'));
        let path = component
            .parts()
            .iter()
            .map(|part| part.def_id)
            .collect::<Vec<_>>();
        let key = (is_structured, path);
        if let Some(proven) = self.nonreplaceability_by_path.get(&key) {
            return *proven;
        }
        let proven = super::request_proves_transitive_non_replaceability(
            self.class_index,
            &FunctionRequest::from_reference(reference),
        );
        self.nonreplaceability_by_path.insert(key, proven);
        proven
    }

    /// Restate a statement call's callee path so it spells the collected
    /// function table's key for that callee.
    ///
    /// A statement call (MLS §11.2.1 zero-output, §11.2.1.1 multi-output) names
    /// its callee by a bare `ComponentReference`: the use-site path, one exact
    /// `DefId` per segment, with no rendered companion to carry the
    /// lookup-qualified spelling an expression call keeps in its `Reference`.
    /// A call written inside the class that declares the callee therefore
    /// spells one segment while the collected table is keyed by the declaration
    /// path, and every consumer that resolves a callee by rendered name — flat
    /// reachability, and the DAE's owner and shape discovery — reads the two as
    /// different callables. Restating the path here closes that gap with the
    /// same exact enclosing `DefId`s the expression form uses, and only when
    /// the rebuilt path spells the collected key exactly.
    fn canonical_callee_path(
        &self,
        callee: &rumoca_core::ComponentReference,
    ) -> Option<rumoca_core::ComponentReference> {
        let rendered = callee.to_var_name();
        if self.canonical_functions.by_name.contains_key(&rendered) {
            return None;
        }
        let canonical = self.sole_canonical_function_for_declaration(callee.target_def_id())?;
        callable_scope_identity::scope_qualified_path(self.class_index, callee, &canonical.name)
    }
}

impl ExpressionRewriter for CollectedFunctionCallCanonicalizer<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } = expr
        else {
            return self.walk_expression(expr);
        };
        let reconciled = callable_scope_identity::scope_qualified_reference(self.class_index, name);
        let name = reconciled.as_ref().unwrap_or(name);
        if name.resolved_function().is_none()
            && let Some(component_ref) = name.component_ref()
            && component_ref.to_var_name() != *name.var_name()
        {
            self.error.get_or_insert_with(|| {
                FlattenError::inconsistent_function_reference(
                    name.as_str(),
                    component_ref.to_var_name().as_str(),
                    *span,
                )
            });
            return self.walk_expression(expr);
        }
        let args = self.rewrite_expressions(args);
        if let Some((
            canonical_name,
            canonical_instance,
            canonical_is_constructor,
            exact_instance,
        )) = self.canonical_function_for_reference(name).map(|matched| {
            (
                matched.function.name.clone(),
                matched.function.instance_id,
                matched.function.is_constructor,
                matched.exact_instance,
            )
        }) {
            if exact_instance {
                self.record_call_kind_conflict(
                    name,
                    *is_constructor,
                    canonical_instance,
                    canonical_is_constructor,
                    *span,
                );
            }
            let base_part_count = name
                .component_ref()
                .map(|reference| reference.parts().len())
                .unwrap_or(0);
            let transitively_non_replaceable =
                self.occurrence_proves_transitive_nonreplaceability(name);
            let call_kind_agrees = *is_constructor == canonical_is_constructor;
            let canonical_name = name.with_var_name(canonical_name);
            // A name-only or declaration-only match may continue carrying an
            // already established call kind, but it cannot mint the exact
            // identity that a later pass could use to launder a contradictory
            // kind into constructor semantics. Exact-instance input, or a
            // fallback whose kind already agrees, remains safe to canonicalize.
            let name = if exact_instance || call_kind_agrees {
                canonical_name.with_resolved_function(rumoca_core::ResolvedFunctionReference {
                    instance_id: canonical_instance,
                    base_part_count,
                    transitively_non_replaceable,
                })
            } else {
                canonical_name
            };
            return rumoca_core::Expression::FunctionCall {
                name,
                args,
                // The exact collected callable owns its kind. This semantic
                // completion is what turns an operator-record conversion
                // occurrence selected through its constructor function into
                // the structural constructor call represented in Flat.
                is_constructor: if exact_instance {
                    canonical_is_constructor
                } else {
                    *is_constructor
                },
                span: *span,
            };
        }
        rumoca_core::Expression::FunctionCall {
            name: name.clone(),
            args,
            is_constructor: *is_constructor,
            span: *span,
        }
    }
}

impl StatementRewriter for CollectedFunctionCallCanonicalizer<'_> {
    fn rewrite_statement(&mut self, statement: &rumoca_core::Statement) -> rumoca_core::Statement {
        let rumoca_core::Statement::FunctionCall {
            comp,
            args,
            outputs,
            span,
        } = statement
        else {
            return self.walk_statement(statement);
        };
        rumoca_core::Statement::FunctionCall {
            comp: self.canonical_callee_path(comp).unwrap_or_else(|| {
                rumoca_core::StatementRewriter::rewrite_component_reference(self, comp)
            }),
            args: self.rewrite_expressions(args),
            outputs: outputs
                .iter()
                .map(|output| {
                    output.as_ref().map(|output| {
                        rumoca_core::StatementRewriter::rewrite_component_reference(self, output)
                    })
                })
                .collect(),
            span: *span,
        }
    }
}

pub(crate) fn canonicalize_function_calls_in_expression_with_scope(
    expr: &mut rumoca_core::Expression,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    caller_scope: Option<&str>,
) {
    if caller_scope.is_none() {
        return;
    }
    *expr = ScopedFunctionCallCanonicalizer {
        tree,
        class_index,
        caller_scope,
    }
    .rewrite_expression(expr);
}

struct ScopedFunctionCallCanonicalizer<'a> {
    tree: &'a ast::ClassTree,
    class_index: &'a ast::ClassDefIndex<'a>,
    caller_scope: Option<&'a str>,
}

impl ExpressionRewriter for ScopedFunctionCallCanonicalizer<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } = expr
        else {
            return self.walk_expression(expr);
        };
        let args = self.rewrite_expressions(args);
        if *is_constructor {
            return rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args,
                is_constructor: *is_constructor,
                span: *span,
            };
        }
        let Some(resolved_name) = canonical_function_name_with_scope(
            name,
            self.tree,
            self.class_index,
            self.caller_scope,
        ) else {
            return rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args,
                is_constructor: *is_constructor,
                span: *span,
            };
        };
        rumoca_core::Expression::FunctionCall {
            name: resolved_function_reference(name, resolved_name, self.tree, self.class_index),
            args,
            is_constructor: *is_constructor,
            span: *span,
        }
    }
}

fn canonical_function_name_with_scope(
    reference: &rumoca_core::Reference,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    caller_scope: Option<&str>,
) -> Option<String> {
    if let Some(def_id) = reference.target_def_id()
        && let Some(class_def) = class_index.get(def_id)
        && class_def.class_type == rumoca_core::ClassType::Function
        && !class_def.partial
    {
        return class_index
            .qualified_name(def_id)
            .map(str::to_string)
            .filter(|name| name != reference.as_str());
    }

    let resolved =
        resolve_function_class_with_scope(tree, class_index, reference.as_str(), caller_scope)?;
    if resolved.class_def.class_type != rumoca_core::ClassType::Function
        || resolved.class_def.partial
    {
        return None;
    }
    (resolved.exposed_name != reference.as_str()).then_some(resolved.exposed_name)
}

fn resolved_function_reference(
    original: &rumoca_core::Reference,
    resolved_name: String,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
) -> rumoca_core::Reference {
    let Some(component_ref) = original.component_ref() else {
        return original.with_var_name(rumoca_core::VarName::new(resolved_name));
    };
    let Some(target) = tree.name_map.get(&resolved_name).copied().or_else(|| {
        class_index
            .get_by_qualified_name(&resolved_name)
            .and_then(|class_def| class_def.def_id)
    }) else {
        return original.clone();
    };
    let mut parts = component_ref.parts().to_vec();
    parts
        .last_mut()
        .expect("checked component reference is nonempty")
        .def_id = target;
    let component_ref = rumoca_core::ComponentReference::construct(
        component_ref.local(),
        component_ref.span(),
        parts,
    )
    .expect("rewriting one identity preserves a checked component reference");
    original.with_rewritten_component_reference(original.as_str(), component_ref)
}
