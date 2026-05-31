use super::*;

impl TypeChecker {
    pub(super) fn check_equation_shape_compatibility(
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
        let span =
            self.source_map
                .location_to_span(&loc.file_name, loc.start as usize, loc.end as usize);
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

    fn infer_expression_shape(
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
            Expression::ComponentReference(cr) => self
                .infer_component_ref_type(cr, type_table)
                .map(|_| Vec::new()),
            Expression::Parenthesized { inner, .. } => {
                self.infer_expression_shape(inner, type_table)
            }
            Expression::FieldAccess { base, field, .. } => self
                .infer_field_access_type(base, field, type_table)
                .map(|_| Vec::new()),
            Expression::Binary { lhs, rhs, .. } => Self::merge_binary_shapes(
                self.infer_expression_shape(lhs, type_table),
                self.infer_expression_shape(rhs, type_table),
            ),
            Expression::Unary { rhs, .. } => self.infer_expression_shape(rhs, type_table),
            _ => None,
        }
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
