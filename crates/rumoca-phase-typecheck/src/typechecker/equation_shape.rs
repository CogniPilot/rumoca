use super::*;

impl TypeChecker {
    pub(in crate::typechecker) fn check_equation_shape_compatibility(
        &mut self,
        lhs: &Expression,
        rhs: &Expression,
        type_table: &TypeTable,
    ) {
        let lhs_shape = self.infer_expression_shape(lhs, type_table);
        let rhs_shape = self.infer_expression_shape(rhs, type_table);
        let (Some(lhs_shape), Some(rhs_shape)) = (lhs_shape, rhs_shape) else {
            return;
        };
        if lhs_shape == rhs_shape {
            return;
        }
        let Some(loc) = lhs.get_location().or_else(|| rhs.get_location()) else {
            return;
        };
        let Some(span) = self.diagnostic_location_span(loc, "equation shape compatibility") else {
            return;
        };
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET002",
            format!(
                "array dimension mismatch: expected `{}`, found `{}`",
                Self::format_shape(&lhs_shape),
                Self::format_shape(&rhs_shape)
            ),
            "equation assignment here",
            span,
        ));
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
        let name = comp
            .parts
            .last()
            .map(|part| part.ident.text.as_ref())
            .unwrap_or_default();
        match name {
            "sum" | "product" | "scalar" if args.len() == 1 => Some(Vec::new()),
            "der" | "pre" | "noEvent" | "actualStream" if args.len() == 1 => {
                self.infer_expression_shape(&args[0], type_table)
            }
            "fill" if args.len() >= 2 => {
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
            "zeros" | "ones" | "identity" | "diagonal" => {
                rumoca_eval_ast::eval::infer_dimensions_from_binding_with_scope(
                    expression,
                    &self.eval_ctx,
                    self.current_instance_scope
                        .as_ref()
                        .map_or("", ComponentPath::as_str),
                )
            }
            _ => None,
        }
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
        let mut declared = Vec::new();
        let mut found_shape = false;
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
                    found_shape = true;
                    declared.extend_from_slice(&local_shape);
                }
                // A declared but unevaluated extent makes the complete
                // reference shape unknown. A later scalar member cannot
                // recover the missing owner domain.
                SemanticLookup::Found(None) | SemanticLookup::Ambiguous => return None,
                SemanticLookup::Missing => {}
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
        if !found_shape {
            return None;
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
