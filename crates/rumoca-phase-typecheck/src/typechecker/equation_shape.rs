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
        if let Some(dims) =
            rumoca_eval_ast::eval::infer_dimensions_from_binding(expr, &self.eval_ctx)
        {
            return Some(dims);
        }
        match expr {
            Expression::Terminal { .. } => Some(Vec::new()),
            // SPEC_0008: a component reference is scalar only when its
            // declared shape is known to be empty — `[]` must never double
            // as "unknown shape".
            Expression::ComponentReference(cr) => self.infer_component_ref_shape(cr),
            Expression::Parenthesized { inner, .. } => {
                self.infer_expression_shape(inner, type_table)
            }
            Expression::FieldAccess { .. } => None,
            Expression::Binary { lhs, rhs, .. } => Self::merge_binary_shapes(
                self.infer_expression_shape(lhs, type_table),
                self.infer_expression_shape(rhs, type_table),
            ),
            Expression::Unary { rhs, .. } => self.infer_expression_shape(rhs, type_table),
            Expression::Array {
                elements,
                is_matrix,
                ..
            } => self.infer_array_literal_shape(elements, *is_matrix, type_table),
            Expression::Range {
                start, step, end, ..
            } => Self::integer_literal_range_shape(start, step.as_deref(), end),
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
    ) -> Option<Vec<usize>> {
        let name = Self::component_ref_name(cr);
        let declared = self.current_component_shapes.get(&name)?.clone()?;
        let subscripts: usize = cr
            .parts
            .iter()
            .map(|part| part.subs.as_ref().map_or(0, |subs| subs.len()))
            .sum();
        if subscripts == 0 {
            return Some(declared);
        }
        if subscripts == declared.len()
            && cr.parts.iter().all(|part| {
                part.subs.as_ref().is_none_or(|subs| {
                    subs.iter()
                        .all(|sub| matches!(sub, rumoca_ir_ast::Subscript::Expression(_)))
                })
            })
        {
            // Fully indexed array element is a scalar.
            return Some(Vec::new());
        }
        // Slices and partial subscripting need range evaluation; treat the
        // shape as unknown rather than guessing.
        None
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
        if is_matrix && shape.len() == 1 {
            shape.push(1);
        }
        Some(shape)
    }

    fn merge_binary_shapes(
        lhs_shape: Option<Vec<usize>>,
        rhs_shape: Option<Vec<usize>>,
    ) -> Option<Vec<usize>> {
        match (lhs_shape, rhs_shape) {
            (Some(lhs), Some(rhs)) if lhs == rhs => Some(lhs),
            (Some(lhs), Some(rhs)) if lhs.is_empty() => Some(rhs),
            (Some(lhs), Some(rhs)) if rhs.is_empty() => Some(lhs),
            (Some(shape), None) | (None, Some(shape)) => Some(shape),
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
