//! Component-modifier validation for the late typecheck pass: modifier name
//! resolution against builtin attributes and class members (MLS §7.2), and the
//! type/unit-syntax checks applied to each accepted modifier value.

use super::*;

impl TypeChecker {
    pub(crate) fn validate_all_modifier_targets(
        &mut self,
        tree: &rumoca_ir_ast::ClassTree,
        type_table: &TypeTable,
    ) {
        for class_def_id in tree.def_map.keys().copied() {
            if let Some(class) = tree.get_class_by_def_id(class_def_id) {
                self.validate_reachable_modifier_targets(tree, class, type_table);
            }
        }
    }

    pub(crate) fn validate_reachable_modifier_targets(
        &mut self,
        tree: &rumoca_ir_ast::ClassTree,
        class: &ClassDef,
        type_table: &TypeTable,
    ) {
        let Some(class_def_id) = class.def_id else {
            self.emit_unidentified_modifier_owner(class);
            return;
        };
        if !self.validated_modifier_classes.insert(class_def_id) {
            return;
        }
        self.validate_class_modifier_targets(class, type_table);
        for base in class
            .extends
            .iter()
            .filter_map(|extends| extends.base_def_id)
            .chain(
                class
                    .components
                    .values()
                    .filter_map(|component| component.type_def_id),
            )
        {
            if let Some(reachable) = tree.get_class_by_def_id(base) {
                self.validate_reachable_modifier_targets(tree, reachable, type_table);
            }
        }
    }

    fn validate_class_modifier_targets(&mut self, class: &ClassDef, type_table: &TypeTable) {
        self.validate_extends_modifier_targets(class, type_table);
        for (component_name, component) in &class.components {
            let receiver_type = self.resolve_type_name(
                &component.type_name.to_string(),
                component.type_def_id,
                type_table,
            );
            self.validate_component_source_modifier_targets(
                component_name,
                component,
                receiver_type,
                type_table,
            );
        }
    }

    fn validate_extends_modifier_targets(&mut self, class: &ClassDef, type_table: &TypeTable) {
        for extends in &class.extends {
            if extends.modifications.is_empty() {
                continue;
            }
            let Some(receiver_type) = extends
                .base_def_id
                .and_then(|base_def_id| self.type_ids_by_def_id.get(&base_def_id))
                .copied()
            else {
                self.emit_unresolved_extends_modifier_receiver(extends);
                continue;
            };
            let owner = format!("extends {}", extends.base_name);
            for modification in &extends.modifications {
                self.validate_modifier_expression(
                    &owner,
                    &extends.base_name.to_string(),
                    receiver_type,
                    &modification.expr,
                    type_table,
                );
            }
        }
    }

    fn emit_unresolved_extends_modifier_receiver(&mut self, extends: &rumoca_ir_ast::Extend) {
        let Some(span) =
            self.diagnostic_location_span(&extends.location, "extends modifier receiver type")
        else {
            return;
        };
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET001",
            format!(
                "cannot validate modifiers of unresolved base type `{}`",
                extends.base_name
            ),
            "extends base has no exact type identity",
            span,
        ));
    }

    fn validate_component_source_modifier_targets(
        &mut self,
        component_name: &str,
        component: &Component,
        receiver_type: TypeId,
        type_table: &TypeTable,
    ) {
        if component.source_modifications.is_empty() {
            if !component.modifications.is_empty() {
                self.emit_missing_source_modifier_targets(component_name, component);
            }
            return;
        }
        for modification in &component.source_modifications {
            self.validate_modifier_expression(
                component_name,
                &component.type_name.to_string(),
                receiver_type,
                modification,
                type_table,
            );
        }
    }

    fn emit_missing_source_modifier_targets(
        &mut self,
        component_name: &str,
        component: &Component,
    ) {
        let Some(span) = self
            .diagnostic_location_span(&component.location, "component modifier target provenance")
        else {
            return;
        };
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET000",
            format!(
                "component `{component_name}` has applied modifiers without source target identities"
            ),
            "modifier targets are not representable",
            span,
        ));
    }

    fn validate_modifier_expression(
        &mut self,
        owner_name: &str,
        owner_type: &str,
        receiver_type: TypeId,
        expression: &Expression,
        type_table: &TypeTable,
    ) {
        match expression {
            Expression::Modification { target, value, .. } => {
                self.validate_modifier_target(
                    owner_name,
                    owner_type,
                    receiver_type,
                    target,
                    type_table,
                );
                if let Some(value) = value {
                    self.validate_replacement_type_modifiers(owner_name, value, type_table);
                }
            }
            Expression::ClassModification {
                target,
                modifications,
                ..
            } => {
                let Some(target_type) = self.validate_modifier_target(
                    owner_name,
                    owner_type,
                    receiver_type,
                    target,
                    type_table,
                ) else {
                    return;
                };
                self.validate_nested_modifiers(
                    &format!("{owner_name}.{target}"),
                    target_type,
                    modifications,
                    type_table,
                );
            }
            Expression::Binary {
                op: rumoca_core::OpBinary::Assign,
                lhs,
                ..
            } => match lhs.as_ref() {
                Expression::ClassModification { .. } => self.validate_modifier_expression(
                    owner_name,
                    owner_type,
                    receiver_type,
                    lhs,
                    type_table,
                ),
                Expression::ComponentReference(target) => {
                    self.validate_modifier_target(
                        owner_name,
                        owner_type,
                        receiver_type,
                        target,
                        type_table,
                    );
                }
                _ => self.emit_unrepresentable_modifier_target(expression),
            },
            // Enumeration short-class redeclarations retain only the
            // redeclared slot as a call-shaped carrier. The call target is
            // still a modifier target, not an ordinary function reference.
            Expression::FunctionCall {
                comp,
                args,
                is_partial_application: false,
                ..
            } if args.is_empty() => {
                self.validate_modifier_target(
                    owner_name,
                    owner_type,
                    receiver_type,
                    comp,
                    type_table,
                );
            }
            _ => self.emit_unrepresentable_modifier_target(expression),
        }
    }

    fn validate_nested_modifiers(
        &mut self,
        owner_name: &str,
        receiver_type: TypeId,
        modifications: &[Expression],
        type_table: &TypeTable,
    ) {
        let owner_type = Self::format_type_name(type_table, receiver_type);
        for modification in modifications {
            self.validate_modifier_expression(
                owner_name,
                &owner_type,
                receiver_type,
                modification,
                type_table,
            );
        }
    }

    fn validate_replacement_type_modifiers(
        &mut self,
        owner_name: &str,
        value: &Expression,
        type_table: &TypeTable,
    ) {
        let Expression::ClassModification {
            target,
            modifications,
            ..
        } = value
        else {
            return;
        };
        if modifications.is_empty() {
            return;
        }
        let Some(receiver_type) = target
            .target_def_id()
            .and_then(|target_def_id| self.type_ids_by_def_id.get(&target_def_id))
            .copied()
        else {
            self.emit_modifier_target_error(
                owner_name,
                "replacement type",
                target,
                0,
                ModifierPathAdvance::UnresolvedType,
            );
            return;
        };
        self.validate_nested_modifiers(owner_name, receiver_type, modifications, type_table);
    }

    fn validate_modifier_target(
        &mut self,
        owner_name: &str,
        owner_type: &str,
        receiver_type: TypeId,
        target: &rumoca_ir_ast::ComponentReference,
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        if target.parts.is_empty() {
            self.emit_modifier_target_error(
                owner_name,
                owner_type,
                target,
                0,
                ModifierPathAdvance::UnresolvedType,
            );
            return None;
        }
        let mut current_type = receiver_type;
        for (index, part) in target.parts.iter().enumerate() {
            let is_last = index + 1 == target.parts.len();
            match self.advance_modifier_path_segment(
                current_type,
                part.ident.text.as_ref(),
                is_last,
                type_table,
            ) {
                ModifierPathAdvance::Next(next_type) => current_type = next_type,
                ModifierPathAdvance::Complete(attribute_type) => return Some(attribute_type),
                failure @ (ModifierPathAdvance::Missing
                | ModifierPathAdvance::Ambiguous
                | ModifierPathAdvance::UnresolvedType) => {
                    self.emit_modifier_target_error(owner_name, owner_type, target, index, failure);
                    return None;
                }
            }
        }
        Some(current_type)
    }

    fn advance_modifier_path_segment(
        &self,
        current_type_id: TypeId,
        segment: &str,
        is_last: bool,
        type_table: &TypeTable,
    ) -> ModifierPathAdvance {
        if current_type_id.is_unknown() {
            return ModifierPathAdvance::UnresolvedType;
        }
        let mut current_root = self.resolve_type_root(current_type_id);
        loop {
            let Some(current_type) = type_table.get(current_root) else {
                return ModifierPathAdvance::UnresolvedType;
            };
            match current_type {
                Type::Array(array) => {
                    current_root = self.resolve_type_root(array.element);
                }
                _ => break,
            }
        }
        let Some(current_type) = type_table.get(current_root) else {
            return ModifierPathAdvance::UnresolvedType;
        };

        match current_type {
            Type::Builtin(_) | Type::Enumeration(_) => {
                if is_last && Self::is_allowed_builtin_modifier(segment) {
                    ModifierPathAdvance::Complete(current_root)
                } else {
                    ModifierPathAdvance::Missing
                }
            }
            Type::Class(class_type) => {
                let Some(members) = self.class_members.get(&class_type.def_id) else {
                    return ModifierPathAdvance::UnresolvedType;
                };
                match members.get(&rumoca_core::ComponentPath::from_parts([segment])) {
                    Some(crate::modifier_targets::ModifierMember::Typed(type_id)) => {
                        ModifierPathAdvance::Next(*type_id)
                    }
                    Some(crate::modifier_targets::ModifierMember::Ambiguous) => {
                        ModifierPathAdvance::Ambiguous
                    }
                    Some(crate::modifier_targets::ModifierMember::UnresolvedType) => {
                        ModifierPathAdvance::UnresolvedType
                    }
                    None => ModifierPathAdvance::Missing,
                }
            }
            Type::Alias(_) | Type::Function(_) | Type::Unknown | Type::Array(_) => {
                ModifierPathAdvance::UnresolvedType
            }
        }
    }

    fn emit_modifier_target_error(
        &mut self,
        owner_name: &str,
        owner_type: &str,
        target: &rumoca_ir_ast::ComponentReference,
        failing_index: usize,
        failure: ModifierPathAdvance,
    ) {
        let span = if let Some(part) = target.parts.get(failing_index) {
            self.diagnostic_location_span(&part.ident.location, "component modifier target")
        } else {
            self.modifier_provenance_span(target.span, "component modifier target")
        };
        let Some(span) = span else {
            return;
        };
        let (message, label) = match failure {
            ModifierPathAdvance::Missing => (
                format!(
                    "unknown modifier `{target}` for component `{owner_name}` of type `{owner_type}`"
                ),
                "unknown modifier",
            ),
            ModifierPathAdvance::Ambiguous => (
                format!(
                    "modifier target `{target}` for `{owner_name}` of type `{owner_type}` is ambiguous across inherited declarations"
                ),
                "ambiguous modifier target",
            ),
            ModifierPathAdvance::UnresolvedType => (
                format!(
                    "modifier target `{target}` for `{owner_name}` of type `{owner_type}` has no exact receiver type"
                ),
                "modifier receiver type is unresolved",
            ),
            ModifierPathAdvance::Next(_) | ModifierPathAdvance::Complete(_) => {
                unreachable!("successful modifier traversal cannot emit an error")
            }
        };
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET001", message, label, span,
        ));
    }

    fn emit_unidentified_modifier_owner(&mut self, class: &ClassDef) {
        let Some(span) =
            self.diagnostic_location_span(&class.name.location, "class modifier owner identity")
        else {
            return;
        };
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET000",
            format!(
                "class `{}` cannot own modifier validation without an exact definition identity",
                class.name.text
            ),
            "modifier owner has no definition identity",
            span,
        ));
    }

    fn emit_unrepresentable_modifier_target(&mut self, expression: &Expression) {
        let span = if let Some(location) = expression.get_location() {
            self.diagnostic_location_span(location, "modifier target carrier")
        } else {
            self.modifier_provenance_span(expression.span(), "modifier target carrier")
        };
        let Some(span) = span else {
            return;
        };
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET000",
            "modifier target is not represented by a semantic target carrier",
            "modifier target identity was lost",
            span,
        ));
    }

    fn modifier_provenance_span(&mut self, span: Span, context: &str) -> Option<Span> {
        if span.is_dummy() {
            self.emit_typecheck_error(TypeCheckError::missing_source_context(format!(
                "{context} has no source span or token location"
            )));
            return None;
        }
        let recovered = self
            .source_map
            .try_span(span.source, span.start.0, span.end.0);
        if recovered.is_none() {
            let source_name = self
                .source_map
                .name(span.source)
                .unwrap_or(UNKNOWN_SOURCE_DISPLAY_NAME);
            self.emit_typecheck_error(TypeCheckError::missing_source_context(format!(
                "source file `{source_name}` for {context} was not found"
            )));
        }
        recovered
    }

    pub(crate) fn check_component_modifier_types_in_class(
        &mut self,
        class: &ClassDef,
        type_table: &TypeTable,
    ) {
        for (comp_name, comp) in &class.components {
            let type_id = comp.type_id.unwrap_or_else(|| {
                self.resolve_type_name(&comp.type_name.to_string(), comp.type_def_id, type_table)
            });
            self.validate_builtin_component_modifier_types(comp_name, comp, type_table, type_id);
        }
    }

    pub(crate) fn validate_builtin_component_modifier_types(
        &mut self,
        comp_name: &str,
        comp: &Component,
        type_table: &TypeTable,
        type_id: TypeId,
    ) {
        let root_type_id = self.resolve_type_root(type_id);
        let Some(Type::Builtin(builtin_type)) = type_table.get(root_type_id) else {
            return;
        };

        if comp.start_is_modification && !matches!(comp.start, Expression::Empty { .. }) {
            let Some(expected_desc) = Self::builtin_modifier_expected_type(*builtin_type, "start")
            else {
                return;
            };
            self.validate_single_builtin_modifier_type(
                comp_name,
                comp,
                expected_desc,
                "start",
                &comp.start,
                type_table,
            );
        }

        for (modifier_name, modifier_expr) in &comp.modifications {
            if matches!(modifier_name.as_str(), "unit" | "displayUnit") {
                self.validate_unit_modifier_syntax(comp_name, modifier_name, modifier_expr);
            }
            if !Self::is_allowed_builtin_modifier(modifier_name) {
                continue;
            }
            let Some(expected_desc) =
                Self::builtin_modifier_expected_type(*builtin_type, modifier_name)
            else {
                continue;
            };
            self.validate_single_builtin_modifier_type(
                comp_name,
                comp,
                expected_desc,
                modifier_name,
                modifier_expr,
                type_table,
            );
        }
    }

    pub(crate) fn validate_single_builtin_modifier_type(
        &mut self,
        comp_name: &str,
        comp: &Component,
        expected_desc: BuiltinModifierExpectedType,
        modifier_name: &str,
        modifier_expr: &Expression,
        type_table: &TypeTable,
    ) {
        let Some(found_type) = self
            .infer_expression_type(modifier_expr, type_table)
            .value_identity()
        else {
            return;
        };
        if found_type.is_unknown() {
            return;
        }
        let found_root = self.resolve_type_root(found_type);
        if found_root.is_unknown() || Self::is_unresolved_alias_root(type_table, found_root) {
            return;
        }
        if Self::modifier_value_type_matches(expected_desc, found_root, type_table) {
            return;
        }

        let location = modifier_expr.get_location().unwrap_or(&comp.location);
        let Some(span) = self.diagnostic_location_span(location, "builtin modifier value") else {
            return;
        };
        let expected = Self::modifier_expected_type_name(expected_desc);
        let found = Self::format_type_name(type_table, found_type);
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET002",
            format!(
                "modifier `{}` for builtin component `{}` of type `{}` expects `{}`, found `{}`",
                modifier_name, comp_name, comp.type_name, expected, found
            ),
            "modifier value here",
            span,
        ));
    }

    /// MLS Chapter 19: a non-empty `unit`/`displayUnit` string literal must
    /// match the unit-expression grammar.
    fn validate_unit_modifier_syntax(
        &mut self,
        comp_name: &str,
        modifier_name: &str,
        modifier_expr: &Expression,
    ) {
        let Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::String,
            token,
            ..
        } = modifier_expr
        else {
            return;
        };
        let unit = token.text.trim_matches('"');
        if let Err(error) = crate::unit_syntax::validate_unit_expression(unit) {
            let Some(span) = self.diagnostic_location_span(&token.location, "unit modifier syntax")
            else {
                return;
            };
            self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
                "ET010",
                format!("invalid {modifier_name} for `{comp_name}`: {error}"),
                "unit string here",
                span,
            ));
        }
    }

    pub(crate) fn builtin_modifier_expected_type(
        component_builtin_type: rumoca_ir_ast::BuiltinType,
        modifier_name: &str,
    ) -> Option<BuiltinModifierExpectedType> {
        match modifier_name {
            "fixed" | "unbounded" => Some(BuiltinModifierExpectedType::Boolean),
            "unit" | "displayUnit" | "quantity" => Some(BuiltinModifierExpectedType::String),
            "start" | "min" | "max" | "nominal" => Some(BuiltinModifierExpectedType::Component(
                component_builtin_type,
            )),
            // TODO(MLS §4.9): enforce enum/record contracts for stateSelect,
            // uncertain, and distribution when those type identities are tracked.
            _ => None,
        }
    }

    pub(crate) fn modifier_expected_type_name(expected: BuiltinModifierExpectedType) -> String {
        match expected {
            BuiltinModifierExpectedType::Boolean => "Boolean".to_string(),
            BuiltinModifierExpectedType::String => "String".to_string(),
            BuiltinModifierExpectedType::Component(component_builtin_type) => {
                component_builtin_type.name().to_string()
            }
        }
    }

    pub(crate) fn modifier_value_type_matches(
        expected: BuiltinModifierExpectedType,
        found_root_type: TypeId,
        type_table: &TypeTable,
    ) -> bool {
        let Some(found_type) = type_table.get(found_root_type) else {
            return false;
        };
        match expected {
            BuiltinModifierExpectedType::Boolean => {
                matches!(
                    found_type,
                    Type::Builtin(rumoca_ir_ast::BuiltinType::Boolean)
                )
            }
            BuiltinModifierExpectedType::String => {
                matches!(
                    found_type,
                    Type::Builtin(rumoca_ir_ast::BuiltinType::String)
                )
            }
            BuiltinModifierExpectedType::Component(component_builtin) => {
                let matches_component = matches!(
                    (component_builtin, found_type),
                    (
                        rumoca_ir_ast::BuiltinType::Real,
                        Type::Builtin(rumoca_ir_ast::BuiltinType::Real)
                    ) | (
                        rumoca_ir_ast::BuiltinType::Integer,
                        Type::Builtin(rumoca_ir_ast::BuiltinType::Integer)
                    ) | (
                        rumoca_ir_ast::BuiltinType::Boolean,
                        Type::Builtin(rumoca_ir_ast::BuiltinType::Boolean)
                    ) | (
                        rumoca_ir_ast::BuiltinType::String,
                        Type::Builtin(rumoca_ir_ast::BuiltinType::String)
                    ) | (
                        rumoca_ir_ast::BuiltinType::Clock,
                        Type::Builtin(rumoca_ir_ast::BuiltinType::Clock)
                    )
                );
                if matches_component {
                    return true;
                }
                // MLS §6.7: Integer expressions are assignment-compatible with Real.
                matches!(
                    (component_builtin, found_type),
                    (
                        rumoca_ir_ast::BuiltinType::Real,
                        Type::Builtin(rumoca_ir_ast::BuiltinType::Integer)
                    )
                )
            }
        }
    }

    pub(crate) fn is_allowed_builtin_modifier(name: &str) -> bool {
        rumoca_core::is_any_predefined_component_attribute(name)
            || matches!(name, "uncertain" | "distribution")
    }
}
