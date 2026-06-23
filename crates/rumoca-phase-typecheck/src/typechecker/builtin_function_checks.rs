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

        self.check_builtin_arity(comp, name, args);
        self.check_array_builtin_shapes(comp, name, args, type_table);
        match name {
            "integer" => self.check_integer_builtin(comp, args, type_table),
            "delay" => self.check_delay_builtin(comp, args, type_table),
            "der" => self.check_der_builtin(comp, args, type_table),
            "String" => self.check_string_builtin(comp, args, type_table),
            "reinit" => self.check_reinit_builtin(comp, args, type_table),
            "homotopy" => self.check_homotopy_builtin(comp, args, type_table),
            _ => {}
        }
    }

    /// MLS §8.3.6: both reinit() arguments must be (subtypes of) Real
    /// (EQN-031).
    fn check_reinit_builtin(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) {
        let [state, value] = args else {
            return;
        };
        for (arg, which) in [(state, "state argument"), (value, "value argument")] {
            self.require_builtin_argument_type(
                comp,
                arg,
                type_table,
                BuiltinArgumentRule {
                    operator: "reinit",
                    argument_name: which,
                    expected_type: "Real",
                    predicate: Self::is_numeric_type,
                },
            );
        }
    }

    /// MLS §3.7.2.4: homotopy(actual, simplified) takes Real-compatible
    /// scalar expressions (EXPR-034).
    fn check_homotopy_builtin(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) {
        for (arg, which) in args.iter().zip(["actual argument", "simplified argument"]) {
            self.require_builtin_argument_type(
                comp,
                arg,
                type_table,
                BuiltinArgumentRule {
                    operator: "homotopy",
                    argument_name: which,
                    expected_type: "Real",
                    predicate: Self::is_numeric_type,
                },
            );
        }
    }

    fn is_numeric_type(root: TypeId, type_table: &TypeTable) -> bool {
        matches!(
            type_table.get(root),
            Some(Type::Builtin(BuiltinType::Real | BuiltinType::Integer))
        )
    }

    /// MLS §3.7.1: `String(...)` applies no standard type coercion to its
    /// first argument (it must be a Real, Integer, Boolean, or enumeration
    /// value), and `significantDigits` is only meaningful for Real values.
    fn check_string_builtin(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) {
        let Some(first) = args.first() else {
            return;
        };
        self.require_builtin_argument_type(
            comp,
            first,
            type_table,
            BuiltinArgumentRule {
                operator: "String",
                argument_name: "first argument",
                expected_type: "Real, Integer, Boolean, or enumeration",
                predicate: Self::is_delay_value_type,
            },
        );
        let first_root = self
            .infer_expression_type(first, type_table)
            .map(|ty| self.resolve_type_root(type_table, ty));
        let has_significant_digits = args.iter().any(|arg| {
            matches!(
                arg,
                Expression::NamedArgument { name, .. } if name.text.as_ref() == "significantDigits"
            )
        });
        if has_significant_digits
            && matches!(
                first_root.and_then(|root| type_table.get(root)),
                Some(Type::Builtin(BuiltinType::Integer | BuiltinType::Boolean))
            )
        {
            self.emit_array_builtin_error(
                comp,
                "String(): significantDigits is only allowed for Real values (MLS §3.7.1)"
                    .to_string(),
            );
        }
    }

    /// Expected positional-argument count ranges for the elementary builtin
    /// functions (MLS §3.7). Only names with a fixed contract are listed;
    /// anything else is left to later phases.
    fn builtin_arity(name: &str) -> Option<(usize, usize)> {
        Some(match name {
            "abs" | "sign" | "sqrt" | "exp" | "log" | "log10" | "sin" | "cos" | "tan" | "asin"
            | "acos" | "sinh" | "cosh" | "tanh" | "atan" | "der" | "pre" | "edge" | "change"
            | "noEvent" => (1, 1),
            "atan2" | "div" | "mod" | "rem" => (2, 2),
            "semiLinear" => (3, 3),
            "initial" | "terminal" => (0, 0),
            "smooth" => (2, 2),
            _ => return None,
        })
    }

    fn check_builtin_arity(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        name: &str,
        args: &[Expression],
    ) {
        let Some((min, max)) = Self::builtin_arity(name) else {
            return;
        };
        if (min..=max).contains(&args.len()) {
            return;
        }
        let location = &comp.parts[0].ident.location;
        let Some(span) = self.diagnostic_location_span(location, "builtin arity validation") else {
            return;
        };
        let expected = if min == max {
            format!("{min}")
        } else {
            format!("{min}..{max}")
        };
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET008",
            format!(
                "{name}() expects {expected} argument(s), found {}",
                args.len()
            ),
            "builtin call here",
            span,
        ));
    }

    /// MLS §3.7.4: `der(expr)` requires a Real (or Real-rooted) expression.
    fn check_der_builtin(
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
                operator: "der",
                argument_name: "argument",
                expected_type: "Real",
                predicate: Self::is_real_type,
            },
        );
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
                expected_type: "Real or Integer",
                predicate: |root, type_table| {
                    matches!(
                        type_table.get(root),
                        Some(Type::Builtin(BuiltinType::Real | BuiltinType::Integer))
                    )
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
        let Some(span) = self.diagnostic_location_span(location, "builtin argument validation")
        else {
            return;
        };
        let found = Self::format_type_name(type_table, found_type);
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET002",
            format!(
                "{}() {} must have type `{}`, found `{found}`",
                rule.operator, rule.argument_name, rule.expected_type
            ),
            "builtin argument here",
            span,
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

/// Array-builtin shape constraints (MLS §10.3, §10.4).
impl TypeChecker {
    pub(in crate::typechecker) fn check_array_builtin_shapes(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        name: &str,
        args: &[Expression],
        type_table: &TypeTable,
    ) {
        match name {
            "cross" | "skew" => self.check_three_vector_args(comp, name, args, type_table),
            "transpose" => self.check_transpose_arg(comp, args, type_table),
            "size" => self.check_size_args(comp, args, type_table),
            "cat" => self.check_cat_args(comp, args, type_table),
            "scalar" => self.check_scalar_arg(comp, args, type_table),
            "vector" => self.check_vector_arg(comp, args, type_table),
            "matrix" => self.check_matrix_arg(comp, args, type_table),
            "min" | "max" => self.check_min_max_args(comp, name, args, type_table),
            "array" => self.check_array_constructor_args(comp, args, type_table),
            _ => {}
        }
    }

    /// MLS §10.4 / ARR-020: all array() arguments must be type compatible.
    fn check_array_constructor_args(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) {
        let mut first_class: Option<BuiltinTypeClass> = None;
        for arg in args {
            let Some(class) = self.builtin_type_class(arg, type_table) else {
                continue;
            };
            match first_class {
                None => first_class = Some(class),
                Some(existing) if existing != class => {
                    self.emit_array_builtin_error(
                        comp,
                        "array() arguments must be type compatible expressions (MLS §10.4)"
                            .to_string(),
                    );
                    return;
                }
                Some(_) => {}
            }
        }
    }

    /// Classify an expression's builtin type root for compatibility checks;
    /// Real and Integer share the numeric class.
    fn builtin_type_class(
        &mut self,
        expr: &Expression,
        type_table: &TypeTable,
    ) -> Option<BuiltinTypeClass> {
        let found = self.infer_expression_type(expr, type_table)?;
        if found.is_unknown() {
            return None;
        }
        let root = self.resolve_type_root(type_table, found);
        match type_table.get(root) {
            Some(Type::Builtin(BuiltinType::Real | BuiltinType::Integer)) => {
                Some(BuiltinTypeClass::Numeric)
            }
            Some(Type::Builtin(BuiltinType::Boolean)) => Some(BuiltinTypeClass::Boolean),
            Some(Type::Builtin(BuiltinType::String)) => Some(BuiltinTypeClass::String),
            _ => None,
        }
    }

    /// MLS §10.6.7-§10.6.8 / ARR-031, ARR-033: validate `^` and `.^` operands
    /// anywhere in the expression tree.
    pub(in crate::typechecker) fn check_power_operators(
        &mut self,
        expr: &Expression,
        type_table: &TypeTable,
    ) {
        let mut collector = PowerOpCollector {
            found: Vec::new(),
            iterator_ranges: Vec::new(),
            if_branches: Vec::new(),
        };
        let _ = rumoca_ir_ast::Visitor::visit_expression(&mut collector, expr);
        // MLS §10.3.4.1 / ARR-034: expressions in array-comprehension
        // iterators shall be vector expressions.
        for range in collector.iterator_ranges {
            if let Some(shape) = self.infer_expression_shape(&range, type_table)
                && shape.len() > 1
            {
                self.emit_expression_error(
                    &range,
                    format!(
                        "iterator range must be a vector expression, got {} dimension(s) (MLS §10.3.4.1)",
                        shape.len()
                    ),
                );
            }
        }
        // MLS §6.7 / TYPE-032, TYPE-035: branches of an if-expression must
        // have compatible interfaces; two different record types (including
        // operator records) cannot be merged.
        for (lhs, rhs) in collector.if_branches {
            self.check_branch_interface_compatibility(&lhs, &rhs, type_table);
        }
        for (op, lhs, rhs) in collector.found {
            if matches!(
                self.builtin_type_class(&lhs, type_table),
                Some(BuiltinTypeClass::Boolean | BuiltinTypeClass::String)
            ) || matches!(
                self.builtin_type_class(&rhs, type_table),
                Some(BuiltinTypeClass::Boolean | BuiltinTypeClass::String)
            ) {
                self.emit_expression_error(
                    &lhs,
                    "exponentiation operands must be Real or Integer expressions (MLS §10.6.7)"
                        .to_string(),
                );
                continue;
            }
            if op == rumoca_core::OpBinary::Exp {
                self.check_matrix_power(&lhs, &rhs, type_table);
            }
        }
    }

    /// MLS §10.6.8 / ARR-033: `a ^ s` with array `a` requires a square matrix
    /// base and a scalar Integer exponent with s >= 0.
    fn check_matrix_power(
        &mut self,
        base: &Expression,
        exponent: &Expression,
        type_table: &TypeTable,
    ) {
        let Some(shape) = self.infer_expression_shape(base, type_table) else {
            return;
        };
        if shape.is_empty() {
            return;
        }
        if shape.len() != 2 || shape[0] != shape[1] {
            self.emit_expression_error(
                base,
                format!(
                    "matrix power base must be a square matrix, got dimensions {shape:?} (MLS §10.6.8)"
                ),
            );
            return;
        }
        if matches!(
            exponent,
            Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedReal,
                ..
            }
        ) {
            self.emit_expression_error(
                exponent,
                "matrix power exponent must be a scalar Integer (MLS §10.6.8)".to_string(),
            );
            return;
        }
        if let Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs,
            ..
        } = exponent
            && Self::literal_integer(rhs).is_some()
        {
            self.emit_expression_error(
                exponent,
                "matrix power exponent must be >= 0 (MLS §10.6.8)".to_string(),
            );
        }
    }

    /// MLS §6.7 / TYPE-032, TYPE-035: adjacent if-expression branches must
    /// not mix two different record (including operator record) types.
    fn check_branch_interface_compatibility(
        &mut self,
        lhs: &Expression,
        rhs: &Expression,
        type_table: &TypeTable,
    ) {
        let root_of = |checker: &mut Self, expr: &Expression| -> Option<TypeId> {
            let ty = checker.infer_expression_type(expr, type_table)?;
            if ty.is_unknown() {
                return None;
            }
            Some(checker.resolve_type_root(type_table, ty))
        };
        let Some(lhs_root) = root_of(self, lhs) else {
            return;
        };
        let Some(rhs_root) = root_of(self, rhs) else {
            return;
        };
        if lhs_root == rhs_root {
            return;
        }
        let is_record = |root: TypeId| {
            matches!(
                type_table.get(root),
                Some(Type::Class(class_ty)) if class_ty.kind == ClassKind::Record
            )
        };
        if is_record(lhs_root) || is_record(rhs_root) {
            self.emit_expression_error(
                lhs,
                "if-expression branches must have the same record type (MLS §6.7)".to_string(),
            );
        }
    }

    fn emit_expression_error(&mut self, expr: &Expression, message: String) {
        let Some(location) = expr.get_location() else {
            return;
        };
        let Some(span) = self.diagnostic_location_span(location, "expression validation") else {
            return;
        };
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET009",
            message,
            "expression here",
            span,
        ));
    }

    fn emit_array_builtin_error(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        message: String,
    ) {
        let location = &comp.parts[0].ident.location;
        let Some(span) = self.diagnostic_location_span(location, "array builtin validation") else {
            return;
        };
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET009",
            message,
            "builtin call here",
            span,
        ));
    }

    /// MLS §10.4.7: cross/skew are only defined for Real (or Integer)
    /// 3-vectors.
    fn check_three_vector_args(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        name: &str,
        args: &[Expression],
        type_table: &TypeTable,
    ) {
        for arg in args {
            let Some(shape) = self.infer_expression_shape(arg, type_table) else {
                continue;
            };
            if shape != [3] {
                self.emit_array_builtin_error(
                    comp,
                    format!(
                        "{name}() requires 3-vector arguments, found shape `[{}]` (MLS §10.4.7)",
                        shape
                            .iter()
                            .map(usize::to_string)
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                );
            }
        }
    }

    /// MLS §10.3.2: transpose requires at least two dimensions.
    fn check_transpose_arg(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) {
        let [arg] = args else {
            return;
        };
        let Some(shape) = self.infer_expression_shape(arg, type_table) else {
            return;
        };
        if shape.len() < 2 {
            self.emit_array_builtin_error(
                comp,
                format!(
                    "transpose() requires at least 2 dimensions, found {} (MLS §10.3.2)",
                    shape.len()
                ),
            );
        }
    }

    /// MLS §10.3.1: size(A, i) requires 1 <= i <= ndims(A).
    fn check_size_args(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) {
        let [array, index] = args else {
            return;
        };
        let Some(shape) = self.infer_expression_shape(array, type_table) else {
            return;
        };
        let Some(index) = Self::literal_integer(index) else {
            return;
        };
        if index < 1 || index as usize > shape.len() {
            self.emit_array_builtin_error(
                comp,
                format!(
                    "size() dimension index {index} is out of range 1..{} (MLS §10.3.1)",
                    shape.len()
                ),
            );
        }
    }

    /// MLS §10.4.2: cat(k, ...) requires 1 <= k <= ndims, equal dimension
    /// counts, and matching sizes except along k.
    fn check_cat_args(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) {
        let Some((k_expr, arrays)) = args.split_first() else {
            return;
        };
        if matches!(
            k_expr,
            Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedReal,
                ..
            }
        ) {
            // MLS §10.4.2 / ARR-022: k shall be a parameter expression of
            // Integer type.
            self.emit_array_builtin_error(
                comp,
                "cat() dimension argument must be an Integer expression (MLS §10.4.2)".to_string(),
            );
            return;
        }
        let Some(k) = Self::literal_integer(k_expr) else {
            return;
        };
        let shapes: Vec<Option<Vec<usize>>> = arrays
            .iter()
            .map(|arg| self.infer_expression_shape(arg, type_table))
            .collect();
        let known: Vec<&Vec<usize>> = shapes.iter().flatten().collect();
        let Some(first) = known.first() else {
            return;
        };
        if k < 1 || k as usize > first.len() {
            self.emit_array_builtin_error(
                comp,
                format!(
                    "cat() dimension {k} must address an existing dimension 1..{} (MLS §10.4.2)",
                    first.len()
                ),
            );
            return;
        }
        if let Some(message) = cat_shape_mismatch(first, &known[1..], k as usize) {
            self.emit_array_builtin_error(comp, message);
        }
    }

    /// MLS §10.3.2: scalar(A) requires every dimension to be 1.
    fn check_scalar_arg(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) {
        let [arg] = args else {
            return;
        };
        let Some(shape) = self.infer_expression_shape(arg, type_table) else {
            return;
        };
        if shape.iter().any(|&dim| dim != 1) {
            self.emit_array_builtin_error(
                comp,
                "scalar() requires size 1 in every dimension (MLS §10.3.2)".to_string(),
            );
        }
    }

    /// MLS §10.3.2: vector(A) allows at most one dimension larger than 1.
    fn check_vector_arg(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) {
        let [arg] = args else {
            return;
        };
        let Some(shape) = self.infer_expression_shape(arg, type_table) else {
            return;
        };
        if shape.iter().filter(|&&dim| dim > 1).count() > 1 {
            self.emit_array_builtin_error(
                comp,
                "vector() allows at most one dimension with size > 1 (MLS §10.3.2)".to_string(),
            );
        }
    }

    /// MLS §10.3.2: matrix(A) requires size 1 for dimensions beyond 2.
    fn check_matrix_arg(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) {
        let [arg] = args else {
            return;
        };
        let Some(shape) = self.infer_expression_shape(arg, type_table) else {
            return;
        };
        if shape.len() > 2 && shape[2..].iter().any(|&dim| dim != 1) {
            self.emit_array_builtin_error(
                comp,
                "matrix() requires size 1 for dimensions beyond the first two (MLS §10.3.2)"
                    .to_string(),
            );
        }
    }

    /// MLS §10.3.4: min/max work on scalar enumeration, Boolean, Integer, or
    /// Real values — never Strings.
    fn check_min_max_args(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        name: &str,
        args: &[Expression],
        type_table: &TypeTable,
    ) {
        for arg in args {
            let Some(found) = self.infer_expression_type(arg, type_table) else {
                continue;
            };
            let root = self.resolve_type_root(type_table, found);
            if matches!(
                type_table.get(root),
                Some(Type::Builtin(BuiltinType::String))
            ) {
                self.emit_array_builtin_error(
                    comp,
                    format!("{name}() is not defined for String values (MLS §10.3.4)"),
                );
            }
        }
    }

    fn literal_integer(expr: &Expression) -> Option<i64> {
        match expr {
            Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                token,
                ..
            } => token.text.parse::<i64>().ok(),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
enum BuiltinTypeClass {
    Numeric,
    Boolean,
    String,
}

/// Collects `^`/`.^` binary expressions, array-comprehension iterator
/// ranges, and if-expression branch pairs (operands cloned; they are
/// Arc-backed, so this is cheap).
struct PowerOpCollector {
    found: Vec<(rumoca_core::OpBinary, Expression, Expression)>,
    iterator_ranges: Vec<Expression>,
    if_branches: Vec<(Expression, Expression)>,
}

impl rumoca_ir_ast::Visitor for PowerOpCollector {
    fn visit_expression(&mut self, expr: &Expression) -> std::ops::ControlFlow<()> {
        match expr {
            Expression::Binary { op, lhs, rhs, .. }
                if matches!(
                    op,
                    rumoca_core::OpBinary::Exp | rumoca_core::OpBinary::ExpElem
                ) =>
            {
                self.found
                    .push((op.clone(), lhs.as_ref().clone(), rhs.as_ref().clone()));
            }
            Expression::ArrayComprehension { indices, .. } => {
                for index in indices {
                    self.iterator_ranges.push(index.range.clone());
                }
            }
            Expression::If {
                branches,
                else_branch,
                ..
            } => {
                let mut values: Vec<&Expression> =
                    branches.iter().map(|(_, value)| value).collect();
                values.push(else_branch.as_ref());
                for pair in values.windows(2) {
                    self.if_branches.push((pair[0].clone(), pair[1].clone()));
                }
            }
            _ => {}
        }
        rumoca_ir_ast::walk_expression_default(self, expr)
    }
}

/// MLS §10.4.2: every cat() argument must match `first` in dimension count and
/// in every size except along the concatenation dimension `k` (1-based).
fn cat_shape_mismatch(first: &[usize], rest: &[&Vec<usize>], k: usize) -> Option<String> {
    for shape in rest {
        if shape.len() != first.len() {
            return Some(
                "cat() arguments must have the same number of dimensions (MLS §10.4.2)".to_string(),
            );
        }
        let mismatch = shape
            .iter()
            .zip(first.iter())
            .enumerate()
            .any(|(dim, (a, b))| dim + 1 != k && a != b);
        if mismatch {
            return Some(format!(
                "cat() argument sizes must match except along dimension {k} (MLS §10.4.2)"
            ));
        }
    }
    None
}
