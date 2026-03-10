use super::*;
use rumoca_ir_ast::BuiltinType;

struct BuiltinArgumentRule {
    operator: &'static str,
    argument_name: &'static str,
    expected_type: &'static str,
    predicate: fn(TypeId, &TypeTable) -> bool,
}

impl TypeChecker {
    pub(crate) fn check_builtin_function_call(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) {
        let Some(name) = (comp.parts.len() == 1).then(|| comp.parts[0].ident.text.as_ref()) else {
            return;
        };

        match name {
            "integer" => self.check_integer_builtin(comp, args, type_table),
            "delay" => self.check_delay_builtin(comp, args, type_table),
            _ => {}
        }
    }

    fn check_integer_builtin(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) {
        let [arg] = args else {
            return;
        };
        self.require_builtin_argument_type(
            comp,
            arg,
            type_table,
            BuiltinArgumentRule {
                operator: "integer",
                argument_name: "argument",
                expected_type: "Real",
                predicate: |root, type_table| {
                    matches!(type_table.get(root), Some(Type::Builtin(BuiltinType::Real)))
                },
            },
        );
    }

    fn check_delay_builtin(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) {
        match args {
            [value, delay_time] => {
                self.require_builtin_argument_type(
                    comp,
                    value,
                    type_table,
                    BuiltinArgumentRule {
                        operator: "delay",
                        argument_name: "value argument",
                        expected_type: "Real, Integer, Boolean, or enumeration",
                        predicate: Self::is_delay_value_type,
                    },
                );
                self.require_builtin_argument_type(
                    comp,
                    delay_time,
                    type_table,
                    BuiltinArgumentRule {
                        operator: "delay",
                        argument_name: "time argument",
                        expected_type: "Real",
                        predicate: Self::is_real_type,
                    },
                );
            }
            [value, delay_time, delay_max] => {
                self.require_builtin_argument_type(
                    comp,
                    value,
                    type_table,
                    BuiltinArgumentRule {
                        operator: "delay",
                        argument_name: "value argument",
                        expected_type: "Real, Integer, Boolean, or enumeration",
                        predicate: Self::is_delay_value_type,
                    },
                );
                self.require_builtin_argument_type(
                    comp,
                    delay_time,
                    type_table,
                    BuiltinArgumentRule {
                        operator: "delay",
                        argument_name: "time argument",
                        expected_type: "Real",
                        predicate: Self::is_real_type,
                    },
                );
                self.require_builtin_argument_type(
                    comp,
                    delay_max,
                    type_table,
                    BuiltinArgumentRule {
                        operator: "delay",
                        argument_name: "delayMax argument",
                        expected_type: "Real",
                        predicate: Self::is_real_type,
                    },
                );
            }
            _ => {}
        }
    }

    fn require_builtin_argument_type(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        arg: &Expression,
        type_table: &TypeTable,
        rule: BuiltinArgumentRule,
    ) {
        let Some(found_type) = self.infer_expression_type(arg, type_table) else {
            return;
        };
        if found_type.is_unknown() {
            return;
        }

        let found_root = self.resolve_type_root(type_table, found_type);
        if Self::is_unresolved_alias_root(type_table, found_root)
            || (rule.predicate)(found_root, type_table)
        {
            return;
        }

        let location = arg
            .get_location()
            .or_else(|| comp.get_location())
            .unwrap_or(&comp.parts[0].ident.location);
        let span = self.source_map.location_to_span(
            &location.file_name,
            location.start as usize,
            location.end as usize,
        );
        let found = Self::format_type_name(type_table, found_type);
        self.diagnostics.emit(CommonDiagnostic::error(
            "ET002",
            format!(
                "{}() {} must have type `{}`, found `{found}`",
                rule.operator, rule.argument_name, rule.expected_type
            ),
            rumoca_core::PrimaryLabel::new(span).with_message("builtin argument here"),
        ));
    }

    fn is_real_type(root: TypeId, type_table: &TypeTable) -> bool {
        matches!(type_table.get(root), Some(Type::Builtin(BuiltinType::Real)))
    }

    fn is_delay_value_type(root: TypeId, type_table: &TypeTable) -> bool {
        matches!(
            type_table.get(root),
            Some(Type::Builtin(
                BuiltinType::Real | BuiltinType::Integer | BuiltinType::Boolean
            )) | Some(Type::Enumeration(_))
        )
    }
}
