use super::*;
use crate::typechecker::traversal_adapter::TypeCheckTraversalCallbacks;

pub(crate) struct FunctionCalls<'a>(pub(crate) &'a mut TypeChecker);

impl TypeCheckTraversalCallbacks for FunctionCalls<'_> {
    fn on_simple_equation(&mut self, _lhs: &Expression, _rhs: &Expression, _types: &TypeTable) {}

    fn push_integer_iterator(&mut self, index: &rumoca_ir_ast::ForIndex) {
        TypeCheckTraversalCallbacks::push_integer_iterator(self.0, index);
    }

    fn pop_integer_iterators(&mut self, count: usize) {
        TypeCheckTraversalCallbacks::pop_integer_iterators(self.0, count);
    }

    fn on_expression_function_call(
        &mut self,
        call: &rumoca_ir_ast::ComponentReference,
        _args: &[Expression],
        _types: &TypeTable,
    ) {
        self.0.queue_function_bindings(call);
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub(crate) struct FunctionBindingContext {
    function: DefId,
    substitutions: Vec<(DefId, DefId)>,
}

impl TypeChecker {
    pub(crate) fn queue_function_bindings(&mut self, call: &rumoca_ir_ast::ComponentReference) {
        let Some(function) = call
            .target_def_id()
            .filter(|id| self.function_signatures.contains_key(id))
            .or_else(|| {
                call.root_def_id()
                    .filter(|id| self.function_signatures.contains_key(id))
            })
        else {
            return;
        };
        let substitutions = self.current_call_type_overrides.call_substitutions(call);
        if let Some(pending) = &mut self.reachable_function_bindings {
            pending.push(FunctionBindingContext {
                function,
                substitutions,
            });
        }
    }

    pub(crate) fn check_binding_expression(
        &mut self,
        expected: TypeId,
        binding: &Expression,
        location: &rumoca_core::Location,
        type_table: &TypeTable,
    ) {
        walk_expression(self, binding, type_table);
        if let Some(found) = self.infer_expression_type(binding, type_table) {
            self.check_expected_expression_type(
                expected,
                found,
                binding.get_location().or(Some(location)),
                "component binding type compatibility",
                "component binding here",
                type_table,
            );
        }
    }

    pub(crate) fn check_declaration_bindings(&mut self, class: &ClassDef, types: &TypeTable) {
        for component in class.components.values() {
            let Some(binding) = &component.binding else {
                continue;
            };
            let Some(expected) = component
                .def_id
                .and_then(|id| self.current_declaration_semantics.get(&id))
                .map(|semantics| semantics.type_id)
            else {
                continue;
            };
            self.check_binding_expression(expected, binding, &component.location, types);
        }
    }

    pub(crate) fn check_reachable_function_bindings(
        &mut self,
        tree: &ClassTree,
        types: &TypeTable,
    ) {
        let declarations = std::mem::take(&mut self.current_declaration_semantics);
        let instances = std::mem::take(&mut self.current_instance_semantics);
        let scope = self.current_instance_scope.take();
        let class_instance = self.current_class_instance_id.take();
        let domain = std::mem::take(&mut self.current_instance_domain_shape);
        let iterators = std::mem::take(&mut self.current_integer_iterators);
        let overrides = std::mem::take(&mut self.current_call_type_overrides);
        let mut checked = HashSet::new();
        while let Some(context) = self.reachable_function_bindings.as_mut().and_then(Vec::pop) {
            if !checked.insert(context.clone()) {
                continue;
            }
            self.current_declaration_semantics.clear();
            self.check_function_and_bases(
                tree,
                context.function,
                &context.substitutions,
                types,
                &mut HashSet::new(),
            );
        }
        self.current_declaration_semantics = declarations;
        self.current_instance_semantics = instances;
        self.current_instance_scope = scope;
        self.current_class_instance_id = class_instance;
        self.current_instance_domain_shape = domain;
        self.current_integer_iterators = iterators;
        self.current_call_type_overrides = overrides;
    }

    fn install_function_lexical_scope(
        &mut self,
        tree: &ClassTree,
        function: DefId,
        types: &TypeTable,
    ) {
        let Some(name) = tree.def_map.get(&function) else {
            return;
        };
        for enclosing in tree.enclosing_class_names_of(name) {
            let Some(class) = tree.get_class_by_qualified_name(enclosing) else {
                continue;
            };
            self.install_function_scope_hierarchy(tree, class, types);
        }
    }

    fn install_function_scope_hierarchy(
        &mut self,
        tree: &ClassTree,
        class: &ClassDef,
        types: &TypeTable,
    ) {
        let Some(class_id) = class.def_id else {
            return;
        };
        for owner in function_signatures::class_hierarchy(tree, class_id) {
            let Some(class) = tree.get_class_by_def_id(owner) else {
                continue;
            };
            self.install_function_scope_declarations(class, types);
        }
    }

    fn install_function_scope_declarations(&mut self, class: &ClassDef, types: &TypeTable) {
        for component in class.components.values() {
            let Some(id) = component.def_id else { continue };
            let type_id = self.resolve_type_name(
                &component.type_name.to_string(),
                component.type_def_id,
                types,
            );
            self.current_declaration_semantics.insert(
                id,
                ComponentSemantics::from_declaration_with_type(component, type_id),
            );
        }
    }

    fn check_function_and_bases(
        &mut self,
        tree: &ClassTree,
        function: DefId,
        substitutions: &[(DefId, DefId)],
        types: &TypeTable,
        visited: &mut HashSet<DefId>,
    ) {
        if !visited.insert(function) {
            return;
        }
        let Some(class) = tree.get_class_by_def_id(function) else {
            return;
        };
        for base in class
            .extends
            .iter()
            .filter_map(|extend| extend.base_def_id)
            .chain(class.redeclare_target_def_id)
        {
            self.check_function_and_bases(tree, base, substitutions, types, visited);
        }
        self.current_call_type_overrides =
            function_signatures::build_call_type_overrides(tree, function, None);
        self.current_call_type_overrides
            .apply_substitutions(substitutions);
        self.ensure_modifier_member_types(tree, function, types);
        self.install_function_lexical_scope(tree, function, types);
        self.check_instanced_class_declaration(class, None, types);
        self.check_declaration_bindings(class, types);
        self.check_function_binding_modifications(class, types);
        let mut calls = FunctionCalls(self);
        walk_equations(&mut calls, &class.equations, types);
        walk_equations(&mut calls, &class.initial_equations, types);
        for statements in class.algorithms.iter().chain(&class.initial_algorithms) {
            walk_statements(&mut calls, statements, types);
        }
    }

    fn check_function_binding_modifications(&mut self, class: &ClassDef, types: &TypeTable) {
        for modification in class
            .extends
            .iter()
            .flat_map(|extend| &extend.modifications)
        {
            let Expression::Modification { target, value, .. } = &modification.expr else {
                continue;
            };
            let Some(expected) = self.infer_component_ref_type(target, types) else {
                continue;
            };
            let Some(location) = value.get_location() else {
                continue;
            };
            self.check_binding_expression(expected, value, location, types);
        }
    }
}

impl TypeChecker {
    /// Member types of the record and class types a reached function
    /// declares. The instanced roots cover the model's own components only,
    /// so a field path through a function's record input (`previous.a.b`)
    /// needs its owner types registered before the function is checked.
    fn ensure_modifier_member_types(
        &mut self,
        tree: &ClassTree,
        function: DefId,
        types: &TypeTable,
    ) {
        if self.component_modifier_member_types.contains_key(&function) {
            return;
        }
        let mut roots = HashSet::from([function]);
        self.expand_component_type_root_closure(tree, types, &mut roots);
        match modifier_targets::build_component_modifier_member_types_for_def_ids(
            tree,
            types,
            &self.type_ids_by_def_id,
            &self.source_map,
            roots,
        ) {
            Ok(member_types) => {
                for (owner, members) in member_types {
                    self.component_modifier_member_types
                        .entry(owner)
                        .or_insert(members);
                }
            }
            Err(error) => self.emit_typecheck_error(*error),
        }
    }
}
