//! Component-modifier validation for the late typecheck pass: modifier name
//! resolution against builtin attributes and class members (MLS §7.2), and the
//! type/unit-syntax checks applied to each accepted modifier value.

use super::*;

impl TypeChecker {
    pub(crate) fn validate_component_modifier_names(
        &mut self,
        comp_name: &str,
        comp: &Component,
        type_table: &TypeTable,
        type_id: TypeId,
    ) {
        let root_type_id = self.resolve_type_root(type_table, type_id);
        let Some(ty) = type_table.get(root_type_id) else {
            return;
        };

        if matches!(ty, Type::Builtin(_)) {
            self.validate_builtin_component_modifiers(comp_name, comp, type_table, type_id);
            return;
        }

        let Type::Class(class_ty) = ty else {
            return;
        };
        let Some(allowed_roots) = self.component_modifier_targets.get(&class_ty.def_id) else {
            return;
        };
        let allowed_roots = allowed_roots.clone();

        for (modifier_name, modifier_expr) in &comp.modifications {
            if modifier_name.starts_with(rumoca_core::CONSTRAINEDBY_MOD_PREFIX) {
                continue;
            }
            let modifier_root = Self::modifier_root_name(modifier_name);
            if modifier_root.is_empty() {
                continue;
            }
            if !allowed_roots.contains(modifier_root.as_str()) {
                self.emit_unknown_component_modifier(
                    comp_name,
                    comp,
                    modifier_name,
                    modifier_expr,
                    modifier_root.as_str(),
                );
                continue;
            }
            self.validate_class_modifier_path(
                comp_name,
                comp,
                modifier_name,
                modifier_expr,
                class_ty.def_id,
                type_table,
            );
        }
    }

    fn validate_class_modifier_path(
        &mut self,
        comp_name: &str,
        comp: &Component,
        modifier_name: &str,
        modifier_expr: &Expression,
        class_def_id: DefId,
        type_table: &TypeTable,
    ) {
        let segments = Self::modifier_segments(modifier_name);
        if segments.len() <= 1 {
            return;
        }

        let Some(mut current_type_id) = self
            .component_modifier_member_types
            .get(&class_def_id)
            .and_then(|members| members.get(&segments[0]).copied())
        else {
            return;
        };

        for (idx, segment) in segments.iter().enumerate().skip(1) {
            let is_last = idx == segments.len() - 1;
            match self.advance_modifier_path_segment(current_type_id, segment, is_last, type_table)
            {
                ModifierPathAdvance::Next(next_type_id) => current_type_id = next_type_id,
                ModifierPathAdvance::Complete => return,
                ModifierPathAdvance::Invalid => {
                    self.emit_unknown_component_modifier(
                        comp_name,
                        comp,
                        modifier_name,
                        modifier_expr,
                        segment,
                    );
                    return;
                }
            }
        }
    }

    fn advance_modifier_path_segment(
        &self,
        current_type_id: TypeId,
        segment: &str,
        is_last: bool,
        type_table: &TypeTable,
    ) -> ModifierPathAdvance {
        if current_type_id.is_unknown() {
            return ModifierPathAdvance::Invalid;
        }
        let current_root = self.resolve_type_root(type_table, current_type_id);
        let Some(current_type) = type_table.get(current_root) else {
            return ModifierPathAdvance::Invalid;
        };

        match current_type {
            Type::Builtin(_) => {
                if is_last && Self::is_allowed_builtin_modifier(segment) {
                    ModifierPathAdvance::Complete
                } else {
                    ModifierPathAdvance::Invalid
                }
            }
            Type::Class(class_type) => self
                .component_modifier_member_types
                .get(&class_type.def_id)
                .and_then(|members| members.get(segment).copied())
                .map_or(ModifierPathAdvance::Invalid, ModifierPathAdvance::Next),
            _ => ModifierPathAdvance::Invalid,
        }
    }

    fn emit_unknown_component_modifier(
        &mut self,
        comp_name: &str,
        comp: &Component,
        modifier_name: &str,
        modifier_expr: &Expression,
        span_name: &str,
    ) {
        let Some(span) = self.modifier_diagnostic_span(
            comp,
            modifier_expr,
            span_name,
            "unknown component modifier",
        ) else {
            return;
        };
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET001",
            format!(
                "unknown modifier `{}` for component `{}` of type `{}`",
                modifier_name, comp_name, comp.type_name
            ),
            "unknown modifier",
            span,
        ));
    }

    pub(crate) fn validate_builtin_component_modifiers(
        &mut self,
        comp_name: &str,
        comp: &Component,
        type_table: &TypeTable,
        type_id: TypeId,
    ) {
        let root_type_id = self.resolve_type_root(type_table, type_id);
        let Some(ty) = type_table.get(root_type_id) else {
            return;
        };
        if !matches!(ty, Type::Builtin(_)) {
            return;
        }

        for (modifier_name, modifier_expr) in &comp.modifications {
            if modifier_name.starts_with(rumoca_core::CONSTRAINEDBY_MOD_PREFIX) {
                continue;
            }
            if Self::is_allowed_builtin_modifier(modifier_name) {
                continue;
            }
            let Some(span) = self.modifier_diagnostic_span(
                comp,
                modifier_expr,
                modifier_name,
                "builtin component modifier",
            ) else {
                continue;
            };
            self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
                "ET001",
                format!(
                    "unknown modifier `{}` for builtin component `{}` of type `{}`",
                    modifier_name, comp_name, comp.type_name
                ),
                "unknown modifier",
                span,
            ));
        }
    }

    fn modifier_diagnostic_span(
        &mut self,
        comp: &Component,
        modifier_expr: &Expression,
        modifier_name: &str,
        context: &str,
    ) -> Option<Span> {
        self.find_modifier_name_span(comp, modifier_name)
            .or_else(|| {
                let location = match modifier_expr.get_location() {
                    Some(location) => location,
                    None => &comp.location,
                };
                self.diagnostic_location_span(location, context)
            })
    }

    pub(crate) fn check_component_modifier_types_in_class(
        &mut self,
        class: &ClassDef,
        type_table: &TypeTable,
    ) {
        let class_name = Self::component_scope_name(class);
        let full_class_name = self.component_scope_full_name(class).map(ToOwned::to_owned);
        for (comp_name, comp) in &class.components {
            let type_id = comp.type_id.unwrap_or_else(|| {
                self.resolve_type_name(&comp.type_name.to_string(), comp.type_def_id, type_table)
            });
            if type_id.is_unknown() {
                continue;
            }
            let mut visible = HashMap::new();
            Self::insert_visible_component_type(
                &mut visible,
                comp_name,
                type_id,
                class_name,
                full_class_name.as_deref(),
            );
            for (name, visible_type) in visible {
                self.current_component_types
                    .entry(name)
                    .or_insert(visible_type);
            }
        }

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
        let root_type_id = self.resolve_type_root(type_table, type_id);
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
        let Some(found_type) = self.infer_expression_type(modifier_expr, type_table) else {
            return;
        };
        if found_type.is_unknown() {
            return;
        }
        let found_root = self.resolve_type_root(type_table, found_type);
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

    fn modifier_root_name(modifier_name: &str) -> String {
        let path = rumoca_core::ComponentPath::from_flat_path(modifier_name);
        let segment = path.parts().first().map_or(modifier_name, String::as_str);
        rumoca_core::split_trailing_subscript_suffix(segment)
            .map_or(segment, |(root, _subscript)| root)
            .to_string()
    }

    fn modifier_segments(modifier_name: &str) -> Vec<String> {
        rumoca_core::ComponentPath::from_flat_path(modifier_name)
            .parts()
            .iter()
            .map(String::as_str)
            .map(Self::normalize_modifier_segment)
            .filter(|segment| !segment.is_empty())
            .collect()
    }

    fn normalize_modifier_segment(segment: &str) -> String {
        let trimmed = segment.trim();
        let root = rumoca_core::split_trailing_subscript_suffix(trimmed)
            .map_or(trimmed, |(prefix, _subscript)| prefix);
        root.trim().to_string()
    }
}
