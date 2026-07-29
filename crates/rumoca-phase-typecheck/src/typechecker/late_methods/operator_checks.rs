//! Operator and call-argument type checking for the late typecheck pass
//! (MLS §6.7 / §12.4): operand requirements per operator, if-expression branch
//! agreement, and user-function argument compatibility.

use super::*;

impl TypeChecker {
    pub(crate) fn check_expression_operator_types(
        &mut self,
        expression: &Expression,
        type_table: &TypeTable,
    ) {
        use rumoca_core::{OpBinary, OpUnary};
        match expression {
            Expression::Unary { op, rhs, .. } => match op {
                OpUnary::Not => self.require_boolean_expression(rhs, "not", type_table),
                OpUnary::Minus | OpUnary::Plus | OpUnary::DotMinus | OpUnary::DotPlus => {
                    self.require_numeric_expression(rhs, &op.to_string(), type_table)
                }
                OpUnary::Empty => {}
            },
            Expression::Binary { op, lhs, rhs, .. } => match op {
                OpBinary::And | OpBinary::Or => {
                    let operator = op.to_string();
                    self.require_boolean_expression(lhs, &operator, type_table);
                    self.require_boolean_expression(rhs, &operator, type_table);
                }
                OpBinary::Add => self.require_addition_expressions(lhs, rhs, type_table),
                OpBinary::Mul => {
                    self.check_zero_inner_operator_record_product(expression, lhs, rhs, type_table);
                    let operator = op.to_string();
                    self.require_numeric_expression(lhs, &operator, type_table);
                    self.require_numeric_expression(rhs, &operator, type_table);
                }
                OpBinary::Sub
                | OpBinary::Div
                | OpBinary::Exp
                | OpBinary::ExpElem
                | OpBinary::AddElem
                | OpBinary::SubElem
                | OpBinary::MulElem
                | OpBinary::DivElem => {
                    let operator = op.to_string();
                    self.require_numeric_expression(lhs, &operator, type_table);
                    self.require_numeric_expression(rhs, &operator, type_table);
                }
                OpBinary::Eq | OpBinary::Neq => {
                    let (Some(lhs_ty), Some(rhs_ty)) = (
                        self.infer_expression_type(lhs, type_table),
                        self.infer_expression_type(rhs, type_table),
                    ) else {
                        return;
                    };
                    if self.equation_types_compatible(type_table, lhs_ty, rhs_ty) {
                        return;
                    }
                    self.check_expected_expression_type(
                        lhs_ty,
                        rhs_ty,
                        rhs.get_location().or_else(|| lhs.get_location()),
                        "equality operand type compatibility",
                        "incompatible equality operand",
                        type_table,
                    );
                }
                OpBinary::Lt | OpBinary::Le | OpBinary::Gt | OpBinary::Ge => {}
                OpBinary::Assign | OpBinary::Empty => {}
            },
            Expression::If {
                branches,
                else_branch,
                ..
            } => {
                for (condition, _) in branches {
                    self.require_boolean_expression(condition, "if", type_table);
                }
                if let Some(expected) = self.infer_expression_type(else_branch, type_table) {
                    self.check_if_expression_branch_types(branches, expected, type_table);
                }
            }
            _ => {}
        }
    }

    fn check_zero_inner_operator_record_product(
        &mut self,
        expression: &Expression,
        lhs: &Expression,
        rhs: &Expression,
        type_table: &TypeTable,
    ) {
        let (Some(lhs_shape), Some(rhs_shape)) = (
            self.infer_expression_shape(lhs, type_table),
            self.infer_expression_shape(rhs, type_table),
        ) else {
            return;
        };
        if !Self::has_zero_product_inner_dimension(&lhs_shape, &rhs_shape) {
            return;
        }
        // The result type, not either operand, owns the empty reduction.
        let Some(record) = self.operator_record_identity(expression, type_table) else {
            return;
        };
        if self.operator_record_has_zero(record) {
            return;
        }
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET011",
            "zero-sized operator-record multiplication requires the record's '0' operator",
            "this product has an empty reduction domain",
            expression.span(),
        ));
    }

    fn operator_record_identity(
        &self,
        expression: &Expression,
        type_table: &TypeTable,
    ) -> Option<DefId> {
        let ty = self.infer_expression_type(expression, type_table)?;
        let root = Self::resolve_alias_root(type_table, ty);
        let Type::Class(class) = type_table.get(root)? else {
            return None;
        };
        self.operator_record_zero_capabilities
            .contains_key(&class.def_id)
            .then_some(class.def_id)
    }

    fn operator_record_has_zero(&self, record: DefId) -> bool {
        let mut pending = vec![record];
        let mut seen = HashSet::new();
        while let Some(current) = pending.pop() {
            if !seen.insert(current) {
                continue;
            }
            if self
                .operator_record_zero_capabilities
                .get(&current)
                .copied()
                .unwrap_or(false)
            {
                return true;
            }
            if let Some(bases) = self.class_base_def_ids.get(&current) {
                pending.extend(bases.iter().copied());
            }
        }
        false
    }

    fn has_zero_product_inner_dimension(lhs: &[usize], rhs: &[usize]) -> bool {
        match (lhs, rhs) {
            ([left], [right]) => left == right && *left == 0,
            ([_, inner], [right]) => inner == right && *inner == 0,
            ([left], [inner, _]) => left == inner && *left == 0,
            ([_, inner], [right, _]) => inner == right && *inner == 0,
            _ => false,
        }
    }

    fn check_if_expression_branch_types(
        &mut self,
        branches: &[(Expression, Expression)],
        expected: TypeId,
        type_table: &TypeTable,
    ) {
        for (_, value) in branches {
            let Some(found) = self.infer_expression_type(value, type_table) else {
                continue;
            };
            if self.equation_types_compatible(type_table, expected, found) {
                continue;
            }
            self.check_expected_expression_type(
                expected,
                found,
                value.get_location(),
                "if-expression branch type compatibility",
                "incompatible if-expression branch",
                type_table,
            );
        }
    }

    pub(super) fn check_user_function_call_argument_types(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) {
        let leaf = comp
            .parts
            .last()
            .map(|part| part.ident.text.as_ref())
            .unwrap_or_default();
        if leaf == "String" || rumoca_core::BuiltinFunction::from_name(leaf).is_some() {
            return;
        }
        let dotted_name = Self::component_ref_name(comp);
        let input_components: Vec<_> = if let Some(signature) = comp
            .def_id
            .and_then(|def_id| self.function_signatures.get(&def_id))
        {
            signature
                .inputs
                .iter()
                .map(|(name, component)| (name.as_str(), component))
                .collect()
        } else {
            let Some(function) = self.user_function_definition(comp, &dotted_name) else {
                return;
            };
            function
                .components
                .iter()
                .filter(|(_, component)| {
                    matches!(component.causality, rumoca_core::Causality::Input(_))
                })
                .map(|(name, component)| (name.as_str(), component))
                .collect()
        };
        let inputs: Vec<_> = input_components
            .into_iter()
            .map(|(name, component)| {
                (
                    name.to_string(),
                    self.resolve_function_signature_component_type(comp, component, type_table),
                )
            })
            .collect();

        let mut positional = 0usize;
        for arg in args {
            let Some((expected, value)) =
                Self::expected_function_argument(arg, &inputs, &mut positional)
            else {
                continue;
            };
            let Some(found) = self.infer_expression_type(value, type_table) else {
                continue;
            };
            self.check_expected_expression_type(
                expected,
                found,
                value.get_location(),
                "function argument type compatibility",
                "incompatible function argument",
                type_table,
            );
        }
    }

    fn expected_function_argument<'a>(
        arg: &'a Expression,
        inputs: &[(String, TypeId)],
        positional: &mut usize,
    ) -> Option<(TypeId, &'a Expression)> {
        match arg {
            Expression::NamedArgument { name, value, .. } => inputs
                .iter()
                .find(|(input_name, _)| input_name.as_str() == name.text.as_ref())
                .map(|(_, expected)| (*expected, value.as_ref())),
            value => {
                let expected = inputs.get(*positional)?.1;
                *positional += 1;
                Some((expected, value))
            }
        }
    }

    fn require_boolean_expression(
        &mut self,
        expression: &Expression,
        operator: &str,
        type_table: &TypeTable,
    ) {
        if self.expression_has_root(expression, type_table, rumoca_ir_ast::BuiltinType::Boolean) {
            return;
        }
        let Some(found) = self.infer_expression_type(expression, type_table) else {
            return;
        };
        self.emit_operator_operand_mismatch(expression, operator, "Boolean", found, type_table);
    }

    fn require_numeric_expression(
        &mut self,
        expression: &Expression,
        operator: &str,
        type_table: &TypeTable,
    ) {
        let Some(found) = self.infer_expression_type(expression, type_table) else {
            return;
        };
        let root = self.resolve_type_root(type_table, found);
        match type_table.get(root) {
            Some(Type::Builtin(
                rumoca_ir_ast::BuiltinType::Real | rumoca_ir_ast::BuiltinType::Integer,
            ))
            | Some(Type::Class(_)) => return,
            _ => {}
        }
        self.emit_operator_operand_mismatch(
            expression,
            operator,
            "Real or Integer",
            found,
            type_table,
        );
    }

    fn require_addition_expressions(
        &mut self,
        lhs: &Expression,
        rhs: &Expression,
        type_table: &TypeTable,
    ) {
        let (Some(lhs_type), Some(rhs_type)) = (
            self.infer_expression_type(lhs, type_table),
            self.infer_expression_type(rhs, type_table),
        ) else {
            // Unknown types are diagnosed at name/type resolution. Do not
            // reinterpret the known half of an unresolved string
            // concatenation as a numeric operand.
            return;
        };
        let lhs_root = self.resolve_type_root(type_table, lhs_type);
        let rhs_root = self.resolve_type_root(type_table, rhs_type);
        let both_strings = matches!(
            (type_table.get(lhs_root), type_table.get(rhs_root)),
            (
                Some(Type::Builtin(rumoca_ir_ast::BuiltinType::String)),
                Some(Type::Builtin(rumoca_ir_ast::BuiltinType::String))
            )
        );
        if both_strings {
            return;
        }
        self.require_numeric_expression(lhs, "+", type_table);
        self.require_numeric_expression(rhs, "+", type_table);
    }

    fn expression_has_root(
        &self,
        expression: &Expression,
        type_table: &TypeTable,
        expected: rumoca_ir_ast::BuiltinType,
    ) -> bool {
        self.infer_expression_type(expression, type_table)
            .map(|ty| self.resolve_type_root(type_table, ty))
            .and_then(|root| type_table.get(root))
            .is_some_and(|ty| matches!(ty, Type::Builtin(found) if *found == expected))
    }

    fn emit_operator_operand_mismatch(
        &mut self,
        expression: &Expression,
        operator: &str,
        expected: &str,
        found: TypeId,
        type_table: &TypeTable,
    ) {
        let Some(location) = expression.get_location() else {
            return;
        };
        let Some(span) = self.diagnostic_location_span(location, "operator operand type") else {
            return;
        };
        let found = Self::format_type_name(type_table, found);
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET002",
            format!(
                "operator `{operator}` expects {expected} operand(s), found `{found}` (MLS §6.7)"
            ),
            "invalid operator operand",
            span,
        ));
    }
}
