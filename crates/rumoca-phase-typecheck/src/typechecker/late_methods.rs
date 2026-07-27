//! Entry points and shared scope naming for the late typecheck pass.
//!
//! The pass walks a class tree (or an instanced overlay) after name
//! resolution and reports type, shape, modifier, and variability problems.
//! This module owns the traversal callbacks, the class/component entry
//! points, and the instance-scope naming helpers; the individual checks live
//! in the sibling modules declared below.
use super::traversal_adapter::{
    TypeCheckTraversalCallbacks, walk_equations, walk_expression, walk_statements,
};
use super::*;
use rumoca_core::ComponentPath;

mod component_modifiers;
mod component_refs;
mod dimensions;
#[path = "equation_shape.rs"]
mod equation_shape;
mod expression_types;
mod modifier_spans;
mod operator_checks;
mod parameter_branches;
mod type_resolution;
mod variability;

#[derive(Clone, Copy)]
pub(crate) enum BuiltinModifierExpectedType {
    Component(rumoca_ir_ast::BuiltinType),
    Boolean,
    String,
}

enum ModifierPathAdvance {
    Next(TypeId),
    Complete,
    Invalid,
}

pub(in crate::typechecker) struct MissingComponentMember {
    owner_type: TypeId,
    member_name: String,
    reference: String,
    span: Span,
}

pub(in crate::typechecker) enum ComponentReferenceTypeError {
    MissingMember(MissingComponentMember),
    MissingSourceContext(TypeCheckError),
}

impl TypeCheckTraversalCallbacks for TypeChecker {
    fn on_component_reference(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        context: rumoca_ir_ast::ComponentReferenceContext,
        type_table: &TypeTable,
    ) {
        if !matches!(
            context,
            rumoca_ir_ast::ComponentReferenceContext::Expression
                | rumoca_ir_ast::ComponentReferenceContext::EquationConnectLhs
                | rumoca_ir_ast::ComponentReferenceContext::EquationConnectRhs
                | rumoca_ir_ast::ComponentReferenceContext::AssignmentTarget
                | rumoca_ir_ast::ComponentReferenceContext::ReinitTarget
        ) {
            return;
        }
        self.validate_component_reference(comp, type_table);
    }

    fn on_field_access(&mut self, base: &Expression, field: &str, type_table: &TypeTable) {
        self.validate_field_access(base, field, type_table);
    }

    fn on_simple_equation(&mut self, lhs: &Expression, rhs: &Expression, type_table: &TypeTable) {
        self.check_equation_type_compatibility(lhs, rhs, type_table);
        self.check_power_operators(lhs, type_table);
        self.check_power_operators(rhs, type_table);
    }

    fn on_assignment(
        &mut self,
        target: &rumoca_ir_ast::ComponentReference,
        value: &Expression,
        type_table: &TypeTable,
    ) {
        self.check_assignment_type_compatibility(target, value, type_table);
    }

    fn on_expression(&mut self, expression: &Expression, type_table: &TypeTable) {
        self.check_expression_operator_types(expression, type_table);
    }

    fn push_integer_iterator(&mut self, name: &str) {
        self.current_integer_iterators.push(name.to_string());
    }

    fn pop_integer_iterators(&mut self, count: usize) {
        let keep = self.current_integer_iterators.len().saturating_sub(count);
        self.current_integer_iterators.truncate(keep);
    }

    fn on_expression_function_call(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) {
        self.check_builtin_function_call(comp, args, type_table);
        self.check_user_function_call_argument_types(comp, args, type_table);
    }

    /// MLS §8.3.4: a parameter if-equation contributes only its selected
    /// branch to the model, so only that branch is type checked.
    fn select_if_equation_branch(
        &mut self,
        cond_blocks: &[rumoca_ir_ast::EquationBlock],
        _type_table: &TypeTable,
    ) -> Option<usize> {
        self.select_parameter_if_branch(cond_blocks)
    }

    /// MLS §8.3.5: the condition of a when-equation must be a Boolean
    /// expression (ALG-011/EQN side).
    fn on_when_condition(&mut self, condition: &Expression, type_table: &TypeTable) {
        let Some(found) = self.infer_expression_type(condition, type_table) else {
            return;
        };
        if found.is_unknown() {
            return;
        }
        let root = self.resolve_type_root(type_table, found);
        if matches!(
            type_table.get(root),
            Some(Type::Builtin(rumoca_ir_ast::BuiltinType::Boolean))
        ) {
            return;
        }
        // Arrays of Boolean are also allowed (vector when-conditions); only
        // flag clearly non-Boolean roots.
        if !matches!(
            type_table.get(root),
            Some(Type::Builtin(
                rumoca_ir_ast::BuiltinType::Real
                    | rumoca_ir_ast::BuiltinType::Integer
                    | rumoca_ir_ast::BuiltinType::String
            ))
        ) {
            return;
        }
        let Some(loc) = condition.get_location() else {
            return;
        };
        let Some(span) = self.diagnostic_location_span(loc, "when-condition type validation")
        else {
            return;
        };
        let found_name = Self::format_type_name(type_table, found);
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET002",
            format!("when-equation condition must be Boolean, found `{found_name}` (MLS §8.3.5)"),
            "when condition here",
            span,
        ));
    }
}

impl TypeChecker {
    fn component_scope_name(class: &ClassDef) -> &str {
        class.name.text.as_ref()
    }

    fn component_scope_full_name<'a>(&'a self, class: &ClassDef) -> Option<&'a str> {
        class
            .def_id
            .and_then(|def_id| self.def_qualified_names.get(&def_id))
            .map(String::as_str)
    }

    fn instance_component_path(qualified_name: &rumoca_ir_ast::QualifiedName) -> ComponentPath {
        qualified_name.to_component_path()
    }

    fn enclosing_scope_name(path: &ComponentPath) -> String {
        path.parent()
            .unwrap_or_else(ComponentPath::root)
            .to_flat_string()
    }

    pub(crate) fn instance_binding_scope_name(data: &rumoca_ir_ast::InstanceData) -> String {
        if data.binding_from_modification
            && let Some(scope) = data.binding_source_scope.as_ref()
        {
            return scope.to_flat_string();
        }
        Self::enclosing_scope_name(&Self::instance_component_path(&data.qualified_name))
    }

    pub(crate) fn instance_attribute_scope_name(
        data: &rumoca_ir_ast::InstanceData,
        attribute: &str,
    ) -> String {
        data.attribute_source_scopes
            .get(attribute)
            .map(rumoca_ir_ast::QualifiedName::to_flat_string)
            .unwrap_or_else(|| {
                Self::enclosing_scope_name(&Self::instance_component_path(&data.qualified_name))
            })
    }

    fn insert_visible_component_type(
        scope_types: &mut HashMap<String, TypeId>,
        comp_name: &str,
        type_id: TypeId,
        scope_name: &str,
        full_scope_name: Option<&str>,
    ) {
        scope_types.insert(comp_name.to_string(), type_id);
        if !scope_name.is_empty() {
            scope_types.insert(
                rumoca_core::ComponentPath::from_flat_path(scope_name)
                    .join(&rumoca_core::ComponentPath::from_flat_path(comp_name))
                    .to_flat_string(),
                type_id,
            );
        }
        if let Some(full_scope_name) = full_scope_name
            && !full_scope_name.is_empty()
        {
            scope_types.insert(
                rumoca_core::ComponentPath::from_flat_path(full_scope_name)
                    .join(&rumoca_core::ComponentPath::from_flat_path(comp_name))
                    .to_flat_string(),
                type_id,
            );
        }
    }

    pub(crate) fn alias_field_key_range<'a>(
        sorted_keys: &'a [String],
        target_prefix: &str,
    ) -> &'a [String] {
        let start = sorted_keys.partition_point(|name| name.as_str() < target_prefix);
        let end_rel = sorted_keys[start..].partition_point(|name| name.starts_with(target_prefix));
        &sorted_keys[start..start + end_rel]
    }

    pub(crate) fn queue_alias_root_update<T: Clone + PartialEq>(
        alias_source: &str,
        alias_target: &str,
        values: &rustc_hash::FxHashMap<String, T>,
        updates: &mut rustc_hash::FxHashMap<String, T>,
    ) {
        let Some(value) = values.get(alias_target).cloned() else {
            return;
        };
        if values.get(alias_source) == Some(&value) {
            return;
        }
        updates.entry(alias_source.to_string()).or_insert(value);
    }

    pub(crate) fn queue_alias_field_update<T: Clone + PartialEq>(
        alias_source: &str,
        target_prefix: &str,
        target_field_name: &str,
        values: &rustc_hash::FxHashMap<String, T>,
        updates: &mut rustc_hash::FxHashMap<String, T>,
    ) {
        let Some(field_suffix) = target_field_name.strip_prefix(target_prefix) else {
            return;
        };
        let Some(value) = values.get(target_field_name) else {
            return;
        };
        let alias_name = rumoca_core::ComponentPath::from_flat_path(alias_source)
            .join(&rumoca_core::ComponentPath::from_flat_path(field_suffix))
            .to_flat_string();
        if values.get(alias_name.as_str()) == Some(value) {
            return;
        }
        updates.entry(alias_name).or_insert_with(|| value.clone());
    }

    /// Type check a StoredDefinition.
    pub(crate) fn check_stored_definition(
        &mut self,
        def: &mut StoredDefinition,
        type_table: &mut TypeTable,
    ) {
        for (_name, class) in def.classes.iter_mut() {
            self.check_class(class, type_table);
        }
    }

    /// Type check a ClassDef.
    pub(crate) fn check_class(&mut self, class: &mut ClassDef, type_table: &mut TypeTable) {
        // Collect constants from this class for dimension evaluation
        self.eval_ctx = rumoca_eval_ast::eval::collect_constants(class, "");

        // Resolve component types and evaluate dimensions
        for (name, comp) in class.components.iter_mut() {
            self.check_component(name, comp, type_table);
        }

        // Expose resolved component types for equation compatibility checks in this class.
        let prev_scope_types = std::mem::take(&mut self.current_component_types);
        let mut scope_types = HashMap::new();
        let class_name = Self::component_scope_name(class);
        let full_class_name = self.component_scope_full_name(class);
        for (name, comp) in &class.components {
            let Some(type_id) = comp.type_id else {
                continue;
            };
            // MLS §5.3/§5.6: keep local and qualified component names visible so
            // later typed member lookup can validate each dotted segment.
            Self::insert_visible_component_type(
                &mut scope_types,
                name,
                type_id,
                class_name,
                full_class_name,
            );
        }
        self.current_component_types = scope_types;
        let mut scope_shapes = HashMap::new();
        for (name, comp) in &class.components {
            let shape = if !comp.shape.is_empty() {
                Some(comp.shape.clone())
            } else if comp.shape_expr.is_empty() {
                Some(Vec::new())
            } else {
                None
            };
            scope_shapes.insert(name.clone(), shape);
        }
        self.current_component_shapes = scope_shapes;

        // Validate known builtin modifier value types now that local component
        // types are available in scope (e.g., `Real x(start = y)`).
        self.check_component_modifier_types_in_class(class, type_table);

        // Validate variability constraints (MLS §4.5)
        self.validate_variability_constraints(class);

        // Mark structural parameters (MLS §18.3)
        self.mark_structural_parameters(class);

        // Type check equations
        walk_equations(self, &class.equations, type_table);
        walk_equations(self, &class.initial_equations, type_table);

        // Type check algorithms
        for statements in &class.algorithms {
            walk_statements(self, statements, type_table);
        }
        for statements in &class.initial_algorithms {
            walk_statements(self, statements, type_table);
        }

        // Recursively check nested classes
        for (_name, nested) in class.classes.iter_mut() {
            self.check_class(nested, type_table);
        }

        // Restore parent class scope.
        self.current_component_types = prev_scope_types;
    }

    /// Type check a component declaration.
    pub(crate) fn check_component(
        &mut self,
        name: &str,
        comp: &mut Component,
        type_table: &mut TypeTable,
    ) {
        let type_name = comp.type_name.to_string();
        let type_id = self.resolve_type_name(&type_name, comp.type_def_id, type_table);
        if type_id.is_unknown()
            && let Some(span) = self.diagnostic_location_span(&comp.location, "component type")
        {
            self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
                "ET001",
                format!("undefined type '{}' for component '{}'", type_name, name),
                "type declaration here",
                span,
            ));
        }
        comp.type_id = Some(type_id);

        // Evaluate shape_expr → shape (MLS §10.1)
        self.evaluate_component_dimensions(name, comp);

        // Validate modifier names for builtin and class-typed components.
        self.validate_component_modifier_names(name, comp, type_table, type_id);

        // Type check the start expression if not empty
        if !matches!(comp.start, Expression::Empty { .. }) {
            walk_expression(self, &comp.start, type_table);
        }

        // Type check modification expressions
        for (_name, mod_expr) in &comp.modifications {
            walk_expression(self, mod_expr, type_table);
        }
    }

    /// Check if type checking produced any errors.
    pub fn has_errors(&self) -> bool {
        self.diagnostics.has_errors()
    }

    /// Get the collected diagnostics.
    pub fn diagnostics(&self) -> &Diagnostics {
        &self.diagnostics
    }

    /// Take the diagnostics (consuming them).
    pub fn take_diagnostics(self) -> Diagnostics {
        self.diagnostics
    }
}
