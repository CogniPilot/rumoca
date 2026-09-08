use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::typechecker) enum ExpressionShape {
    Known(Vec<usize>),
    Unknown(&'static str),
    Invalid(String),
}

impl TypeChecker {
    pub(in crate::typechecker) fn check_equation_shape_compatibility(
        &mut self,
        lhs: &Expression,
        rhs: &Expression,
        type_table: &TypeTable,
    ) {
        let message = Self::shape_compatibility_error(
            self.inferred_expression_shape(lhs, type_table),
            self.inferred_expression_shape(rhs, type_table),
        );
        if let Some(message) = message {
            self.emit_shape_error(
                lhs.get_location().or_else(|| rhs.get_location()),
                "equation shape compatibility",
                "equation assignment here",
                message,
            );
        }
    }

    pub(in crate::typechecker) fn check_algorithm_assignment_shape_compatibility(
        &mut self,
        target: &rumoca_ir_ast::ComponentReference,
        value: &Expression,
        type_table: &TypeTable,
    ) {
        let target_shape = self.infer_component_ref_shape(target, type_table).map_or(
            ExpressionShape::Unknown("assignment target shape"),
            ExpressionShape::Known,
        );
        let message = Self::shape_compatibility_error(
            target_shape,
            self.inferred_expression_shape(value, type_table),
        );
        if let Some(message) = message {
            self.emit_shape_error(
                target.get_location().or_else(|| value.get_location()),
                "algorithm assignment shape compatibility",
                "algorithm assignment here",
                message,
            );
        }
    }

    pub(in crate::typechecker) fn check_function_argument_shape_compatibility(
        &mut self,
        expected: Option<Vec<usize>>,
        value: &Expression,
        type_table: &TypeTable,
    ) {
        let expected = expected.map_or(
            ExpressionShape::Unknown("function input shape"),
            ExpressionShape::Known,
        );
        let found = self.inferred_expression_shape(value, type_table);
        if matches!(
            (&expected, &found),
            (ExpressionShape::Known(expected), ExpressionShape::Known(found))
                if found.len() > expected.len() && found.ends_with(expected)
        ) {
            // MLS §12.4.6: leading actual dimensions beyond the formal's
            // declared shape request automatic function vectorization. Flatten
            // owns the common vectorization-domain proof (FUNC-027 / EF016).
            return;
        }
        let Some(message) = Self::shape_compatibility_error(expected, found) else {
            return;
        };
        self.emit_shape_error(
            value.get_location(),
            "function argument shape compatibility",
            "incompatible function argument",
            message,
        );
    }

    pub(in crate::typechecker) fn check_expression_shape_validity(
        &mut self,
        expression: &Expression,
        type_table: &TypeTable,
    ) {
        let ExpressionShape::Invalid(reason) = self.expression_shape(expression, type_table) else {
            return;
        };
        self.emit_shape_error(
            expression.get_location(),
            "expression shape validity",
            "invalid expression shape",
            reason,
        );
    }

    fn emit_shape_error(
        &mut self,
        location: Option<&rumoca_core::Location>,
        context: &str,
        label: &str,
        message: String,
    ) {
        let Some(location) = location else {
            return;
        };
        let Some(span) = self.diagnostic_location_span(location, context) else {
            return;
        };
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET002", message, label, span,
        ));
    }

    fn shape_compatibility_error(
        expected: ExpressionShape,
        found: ExpressionShape,
    ) -> Option<String> {
        match (expected, found) {
            (ExpressionShape::Invalid(reason), _) => {
                Some(format!("invalid left-hand shape: {reason}"))
            }
            (_, ExpressionShape::Invalid(reason)) => {
                Some(format!("invalid right-hand shape: {reason}"))
            }
            (ExpressionShape::Known(expected), ExpressionShape::Known(found))
                if expected != found =>
            {
                Some(format!(
                    "array dimension mismatch: expected `{}`, found `{}`",
                    Self::format_shape(&expected),
                    Self::format_shape(&found)
                ))
            }
            (ExpressionShape::Unknown(reason), _) | (_, ExpressionShape::Unknown(reason)) => {
                debug_assert!(!reason.is_empty());
                None
            }
            _ => None,
        }
    }

    fn expression_shape(&self, expr: &Expression, type_table: &TypeTable) -> ExpressionShape {
        if let Err(reason) = self.validate_local_expression_shape(expr, type_table) {
            return ExpressionShape::Invalid(reason);
        }
        self.inferred_expression_shape(expr, type_table)
    }

    fn inferred_expression_shape(
        &self,
        expr: &Expression,
        type_table: &TypeTable,
    ) -> ExpressionShape {
        self.infer_expression_shape(expr, type_table).map_or(
            ExpressionShape::Unknown("expression shape inference"),
            ExpressionShape::Known,
        )
    }

    /// Reject shapes that are provably inconsistent while preserving genuine
    /// uncertainty. Traversal checks every expression after its children, so
    /// each invalid node emits once and wrappers cannot hide it by inferring an
    /// unknown aggregate shape.
    fn validate_local_expression_shape(
        &self,
        expr: &Expression,
        type_table: &TypeTable,
    ) -> Result<(), String> {
        match expr {
            Expression::Binary { op, lhs, rhs, .. } => {
                self.validate_binary_shape(op, lhs, rhs, type_table)
            }
            Expression::If {
                branches,
                else_branch,
                ..
            } => self.validate_if_expression_shape(branches, else_branch, type_table),
            Expression::Array {
                elements,
                is_matrix,
                ..
            } => self.validate_array_literal_shape(elements, *is_matrix, type_table),
            _ => Ok(()),
        }
    }

    fn validate_if_expression_shape(
        &self,
        branches: &[(Expression, Expression)],
        else_branch: &Expression,
        type_table: &TypeTable,
    ) -> Result<(), String> {
        let mut known = self.infer_expression_shape(else_branch, type_table);
        for (_, branch) in branches {
            known = Self::validate_equal_known_shapes(
                known,
                self.infer_expression_shape(branch, type_table),
                "if-expression branches",
            )?;
        }
        Ok(())
    }

    fn validate_binary_shape(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: &Expression,
        rhs: &Expression,
        type_table: &TypeTable,
    ) -> Result<(), String> {
        let lhs = self.infer_expression_shape(lhs, type_table);
        let rhs = self.infer_expression_shape(rhs, type_table);
        let (Some(lhs), Some(rhs)) = (lhs, rhs) else {
            return Ok(());
        };
        match op {
            rumoca_core::OpBinary::Mul => {
                Self::matrix_product_shape(Some(lhs.clone()), Some(rhs.clone())).map_or_else(
                    || {
                        Err(format!(
                            "matrix product dimensions `{}` and `{}` are incompatible",
                            Self::format_shape(&lhs),
                            Self::format_shape(&rhs)
                        ))
                    },
                    |_| Ok(()),
                )
            }
            rumoca_core::OpBinary::Div if !rhs.is_empty() => Err(format!(
                "division denominator must be scalar, found `{}`",
                Self::format_shape(&rhs)
            )),
            rumoca_core::OpBinary::Add
            | rumoca_core::OpBinary::Sub
            | rumoca_core::OpBinary::Eq
            | rumoca_core::OpBinary::Neq
                if lhs != rhs =>
            {
                Err(format!(
                    "binary operand dimensions `{}` and `{}` are incompatible",
                    Self::format_shape(&lhs),
                    Self::format_shape(&rhs)
                ))
            }
            _ if lhs == rhs || lhs.is_empty() || rhs.is_empty() => Ok(()),
            _ => Err(format!(
                "binary operand dimensions `{}` and `{}` are incompatible",
                Self::format_shape(&lhs),
                Self::format_shape(&rhs)
            )),
        }
    }

    fn validate_array_literal_shape(
        &self,
        elements: &[Expression],
        is_matrix: bool,
        type_table: &TypeTable,
    ) -> Result<(), String> {
        if is_matrix {
            return self.validate_matrix_literal_rows(elements, type_table);
        }
        let context = if elements
            .iter()
            .all(|element| matches!(element, Expression::Array { .. }))
        {
            "ragged array literal rows"
        } else {
            "array literal elements"
        };
        let mut known = None;
        for element in elements {
            known = Self::validate_equal_known_shapes(
                known,
                self.infer_expression_shape(element, type_table),
                context,
            )?;
        }
        Ok(())
    }

    fn validate_matrix_literal_rows(
        &self,
        rows: &[Expression],
        type_table: &TypeTable,
    ) -> Result<(), String> {
        if !matches!(rows.first(), Some(Expression::Array { .. })) {
            return self.validate_matrix_scalar_cells(rows, type_table);
        }
        let mut width = None;
        for row in rows {
            let Expression::Array {
                elements: cells, ..
            } = row
            else {
                return Err("matrix literal mixes row arrays and scalar cells".to_string());
            };
            width = Some(Self::validate_matrix_row_width(width, cells.len())?);
            self.validate_matrix_scalar_cells(cells, type_table)?;
        }
        Ok(())
    }

    fn validate_matrix_row_width(expected: Option<usize>, actual: usize) -> Result<usize, String> {
        if let Some(expected) = expected
            && actual != expected
        {
            return Err(format!(
                "ragged matrix literal has row widths `{expected}` and `{actual}`"
            ));
        }
        Ok(actual)
    }

    fn validate_matrix_scalar_cells(
        &self,
        cells: &[Expression],
        type_table: &TypeTable,
    ) -> Result<(), String> {
        if cells.iter().any(|cell| {
            self.infer_expression_shape(cell, type_table)
                .is_some_and(|shape| !shape.is_empty())
        }) {
            return Err("matrix literal contains a non-scalar cell".to_string());
        }
        Ok(())
    }

    fn validate_equal_known_shapes(
        current: Option<Vec<usize>>,
        candidate: Option<Vec<usize>>,
        context: &str,
    ) -> Result<Option<Vec<usize>>, String> {
        match (current, candidate) {
            (Some(current), Some(candidate)) if current != candidate => Err(format!(
                "{context} have incompatible dimensions `{}` and `{}`",
                Self::format_shape(&current),
                Self::format_shape(&candidate)
            )),
            (Some(current), _) => Ok(Some(current)),
            (None, candidate) => Ok(candidate),
        }
    }

    pub(in crate::typechecker) fn infer_expression_shape(
        &self,
        expr: &Expression,
        type_table: &TypeTable,
    ) -> Option<Vec<usize>> {
        match expr {
            Expression::Terminal { .. } => Some(Vec::new()),
            // SPEC_0008: a component reference is scalar only when its
            // declared shape is known to be empty — `[]` must never double
            // as "unknown shape".
            Expression::ComponentReference(cr) => self.infer_component_ref_shape(cr, type_table),
            Expression::Parenthesized { inner, .. } => {
                self.infer_expression_shape(inner, type_table)
            }
            Expression::FieldAccess { .. } => None,
            Expression::Binary { op, lhs, rhs, .. } => {
                self.infer_binary_shape(op, lhs, rhs, type_table)
            }
            Expression::Unary { rhs, .. } => self.infer_expression_shape(rhs, type_table),
            Expression::If {
                branches,
                else_branch,
                ..
            } => {
                let mut shape = self.infer_expression_shape(else_branch, type_table);
                for (_, branch) in branches {
                    shape = Self::merge_binary_shapes(
                        shape,
                        self.infer_expression_shape(branch, type_table),
                    );
                }
                shape
            }
            Expression::FunctionCall { comp, args, .. } => {
                self.infer_function_call_shape(expr, comp, args, type_table)
            }
            Expression::DerivativeCall { args, .. } if args.len() == 1 => {
                self.infer_expression_shape(&args[0], type_table)
            }
            Expression::DerivativeCall { .. } => None,
            Expression::Array {
                elements,
                is_matrix,
                ..
            } => self.infer_array_literal_shape(elements, *is_matrix, type_table),
            Expression::Range {
                start, step, end, ..
            } => Self::integer_literal_range_shape(start, step.as_deref(), end).or_else(|| {
                rumoca_eval_ast::eval::infer_dimensions_from_binding_with_scope(
                    expr,
                    &self.eval_ctx,
                    self.current_instance_scope
                        .as_ref()
                        .map_or("", ComponentPath::as_str),
                )
            }),
            _ => None,
        }
    }

    fn infer_binary_shape(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: &Expression,
        rhs: &Expression,
        type_table: &TypeTable,
    ) -> Option<Vec<usize>> {
        let lhs = self.infer_expression_shape(lhs, type_table);
        let rhs = self.infer_expression_shape(rhs, type_table);
        match op {
            rumoca_core::OpBinary::Mul => Self::matrix_product_shape(lhs, rhs),
            rumoca_core::OpBinary::Div => match (lhs, rhs) {
                (Some(lhs), Some(rhs)) if rhs.is_empty() => Some(lhs),
                _ => None,
            },
            _ => Self::merge_binary_shapes(lhs, rhs),
        }
    }

    fn matrix_product_shape(
        lhs: Option<Vec<usize>>,
        rhs: Option<Vec<usize>>,
    ) -> Option<Vec<usize>> {
        let (Some(lhs), Some(rhs)) = (lhs, rhs) else {
            return None;
        };
        if lhs.is_empty() {
            return Some(rhs);
        }
        if rhs.is_empty() {
            return Some(lhs);
        }
        match (lhs.as_slice(), rhs.as_slice()) {
            ([left], [right]) if left == right => Some(Vec::new()),
            ([rows, inner], [right]) if inner == right => Some(vec![*rows]),
            ([left], [inner, columns]) if left == inner => Some(vec![*columns]),
            ([rows, inner], [right, columns]) if inner == right => Some(vec![*rows, *columns]),
            _ => None,
        }
    }

    fn infer_function_call_shape(
        &self,
        expression: &Expression,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) -> Option<Vec<usize>> {
        let builtin = self.resolved_builtin_function(comp);
        match builtin {
            Some(
                rumoca_core::BuiltinFunction::Sum
                | rumoca_core::BuiltinFunction::Product
                | rumoca_core::BuiltinFunction::Scalar,
            ) if args.len() == 1 => Some(Vec::new()),
            Some(rumoca_core::BuiltinFunction::Pre | rumoca_core::BuiltinFunction::NoEvent)
                if args.len() == 1 =>
            {
                self.infer_expression_shape(&args[0], type_table)
            }
            Some(rumoca_core::BuiltinFunction::Cross) if args.len() == 2 => Some(vec![3]),
            Some(rumoca_core::BuiltinFunction::Fill) if args.len() >= 2 => {
                let mut shape = rumoca_eval_ast::eval::infer_dimensions_from_binding_with_scope(
                    expression,
                    &self.eval_ctx,
                    self.current_instance_scope
                        .as_ref()
                        .map_or("", ComponentPath::as_str),
                )?;
                shape.extend(self.infer_expression_shape(&args[0], type_table)?);
                Some(shape)
            }
            Some(
                rumoca_core::BuiltinFunction::Zeros
                | rumoca_core::BuiltinFunction::Ones
                | rumoca_core::BuiltinFunction::Identity
                | rumoca_core::BuiltinFunction::Diagonal,
            ) => rumoca_eval_ast::eval::infer_dimensions_from_binding_with_scope(
                expression,
                &self.eval_ctx,
                self.current_instance_scope
                    .as_ref()
                    .map_or("", ComponentPath::as_str),
            ),
            _ if comp.root_def_id().is_none()
                && comp
                    .parts
                    .last()
                    .is_some_and(|part| part.ident.text.as_ref() == "actualStream")
                && args.len() == 1 =>
            {
                self.infer_expression_shape(&args[0], type_table)
            }
            _ => self.infer_user_function_output_shape(comp, args, type_table),
        }
    }

    fn infer_user_function_output_shape(
        &self,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) -> Option<Vec<usize>> {
        if args.iter().any(|arg| {
            let value = match arg {
                Expression::NamedArgument { value, .. } => value.as_ref(),
                value => value,
            };
            self.infer_expression_shape(value, type_table)
                .is_some_and(|shape| !shape.is_empty())
        }) {
            // A known array actual may be either a direct array argument or a
            // vectorization domain. Until the FUNC-027 proof is available,
            // presenting the declaration's unprefixed output shape as the
            // call's result would be a fabricated exact answer.
            return None;
        }
        if let Some(output) = comp
            .root_def_id()
            .and_then(|def_id| self.function_signatures.get(&def_id))
            .and_then(|signature| signature.outputs.first())
            .map(|(_, output)| output)
        {
            return Self::declared_component_shape(output);
        }
        let dotted_name = Self::component_ref_name(comp);
        let function = self.user_function_definition(comp, &dotted_name)?;
        function
            .components
            .values()
            .find(|component| matches!(component.causality, rumoca_core::Causality::Output(_)))
            .and_then(Self::declared_component_shape)
    }

    pub(in crate::typechecker) fn declared_component_shape(
        component: &Component,
    ) -> Option<Vec<usize>> {
        if !component.shape_expr.is_empty() && component.shape_expr.len() != component.shape.len() {
            return None;
        }
        if component.shape.is_empty() {
            return component.shape_expr.is_empty().then(Vec::new);
        }
        Some(component.shape.clone())
    }

    /// Shape of a literal integer range like `1:3` (length is only known
    /// for literal bounds).
    fn integer_literal_range_shape(
        start: &Expression,
        step: Option<&Expression>,
        end: &Expression,
    ) -> Option<Vec<usize>> {
        let literal = |expr: &Expression| -> Option<i64> {
            match expr {
                Expression::Terminal {
                    terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                    token,
                    ..
                } => token.text.parse::<i64>().ok(),
                _ => None,
            }
        };
        let start = literal(start)?;
        let end = literal(end)?;
        let step = match step {
            Some(step) => literal(step)?,
            None => 1,
        };
        if step == 0 {
            return None;
        }
        let len = ((end - start) / step + 1).max(0) as usize;
        Some(vec![len])
    }

    /// Declared shape of a component reference, accounting for subscripts.
    fn infer_component_ref_shape(
        &self,
        cr: &rumoca_ir_ast::ComponentReference,
        type_table: &TypeTable,
    ) -> Option<Vec<usize>> {
        // A reference with no parts names nothing, so it has no shape. Every
        // part below either contributes its extents or abstains, which is what
        // makes the accumulated `declared` a complete answer rather than a
        // partial one.
        if cr.parts.is_empty() {
            return None;
        }
        let mut declared = Vec::new();
        let mut subscripts = 0usize;
        for (part_index, part) in cr.parts.iter().enumerate() {
            match self.lookup_component_reference_prefix_shape(cr, part_index + 1) {
                // MLS §10.4.1: subscripting `a.b` where `a` is an array of a
                // class with array member `b` yields the concatenated shape
                // `size(a)` ++ `size(b)`. The instance index stores each path
                // with its own declared extents, so every part contributes its
                // extents; a member whose extents happen to repeat the owner's
                // must not be mistaken for an owner-inclusive row.
                SemanticLookup::Found(Some(local_shape)) => {
                    declared.extend_from_slice(&local_shape);
                }
                // A declared but unevaluated extent makes the complete
                // reference shape unknown. A later scalar member cannot
                // recover the missing owner domain. A part the instance-shape
                // index has no row for (`Missing`) is unknown for the same
                // reason: §10.4.1 composes the shape from every part, so the
                // composition cannot be completed without this one. Skipping
                // it would answer with the shape of the *other* parts and
                // present that as the shape of the whole reference.
                SemanticLookup::Found(None)
                | SemanticLookup::Ambiguous
                | SemanticLookup::InvalidAstSubscript
                | SemanticLookup::Missing => return None,
            }
            let Some(part_subscripts) = part.subs.as_ref() else {
                continue;
            };
            if !part_subscripts.iter().all(|subscript| {
                matches!(subscript, rumoca_ir_ast::Subscript::Expression(index)
                    if self
                        .infer_expression_shape(index, type_table)
                        .is_some_and(|shape| shape.is_empty()))
            }) {
                return None;
            }
            subscripts += part_subscripts.len();
        }
        let domain = &self.current_instance_domain_shape;
        if !domain.is_empty() && declared.starts_with(domain) {
            declared.drain(..domain.len());
        }
        if subscripts == 0 {
            return Some(declared);
        }
        if subscripts > declared.len() {
            return None;
        }
        declared.drain(..subscripts);
        Some(declared)
    }

    /// Shape of an array literal: `{a, b, c}` is `[3]` when the elements are
    /// scalar, `[rows, cols]` for matrix rows of equal known width.
    fn infer_array_literal_shape(
        &self,
        elements: &[Expression],
        is_matrix: bool,
        type_table: &TypeTable,
    ) -> Option<Vec<usize>> {
        if elements.is_empty() {
            return Some(vec![0]);
        }
        if is_matrix {
            return self.infer_matrix_literal_shape(elements, type_table);
        }
        let element_shapes: Option<Vec<Vec<usize>>> = elements
            .iter()
            .map(|element| self.infer_expression_shape(element, type_table))
            .collect();
        let element_shapes = element_shapes?;
        let first = &element_shapes[0];
        if element_shapes.iter().any(|shape| shape != first) {
            return None;
        }
        let mut shape = vec![elements.len()];
        shape.extend_from_slice(first);
        Some(shape)
    }

    fn infer_matrix_literal_shape(
        &self,
        elements: &[Expression],
        type_table: &TypeTable,
    ) -> Option<Vec<usize>> {
        if !matches!(elements.first(), Some(Expression::Array { .. })) {
            let all_scalar = elements.iter().all(|element| {
                self.infer_expression_shape(element, type_table)
                    .is_some_and(|shape| shape.is_empty())
            });
            return all_scalar.then_some(vec![1, elements.len()]);
        }

        let mut column_count = None;
        for row in elements {
            let Expression::Array {
                elements: row_elements,
                ..
            } = row
            else {
                return None;
            };
            if !row_elements.iter().all(|element| {
                self.infer_expression_shape(element, type_table)
                    .is_some_and(|shape| shape.is_empty())
            }) {
                return None;
            }
            match column_count {
                Some(expected) if expected != row_elements.len() => return None,
                None => column_count = Some(row_elements.len()),
                _ => {}
            }
        }
        Some(vec![elements.len(), column_count.unwrap_or(0)])
    }

    fn merge_binary_shapes(
        lhs_shape: Option<Vec<usize>>,
        rhs_shape: Option<Vec<usize>>,
    ) -> Option<Vec<usize>> {
        match (lhs_shape, rhs_shape) {
            (Some(lhs), Some(rhs)) if lhs == rhs => Some(lhs),
            (Some(lhs), Some(rhs)) if lhs.is_empty() => Some(rhs),
            (Some(lhs), Some(rhs)) if rhs.is_empty() => Some(lhs),
            // A missing operand shape is not evidence that it is scalar.
            // Preserve uncertainty so a sliced/vector operand cannot be
            // collapsed to the known scalar operand's shape.
            (Some(_), None) | (None, Some(_)) => None,
            _ => None,
        }
    }

    fn format_shape(shape: &[usize]) -> String {
        if shape.is_empty() {
            return "scalar".to_string();
        }
        format!(
            "[{}]",
            shape
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}
