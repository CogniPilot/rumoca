//! Array projection: the scalar of an aggregate operand one output scalar reads.
//!
//! Comprehensions, subscripts, and array literals are projected at translation
//! time wherever the model fixes the index. A subscript only the run knows
//! falls back to an explicit selection chain over the base's checked extent.

use super::*;

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(super) fn comprehension(
        &mut self,
        domain: dae::DomainId<'dae>,
        body: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<solve::Reg, LowerError> {
        let domain_view = self
            .view
            .domain(domain)
            .expect("checked comprehension domain resolves");
        let body_count = scalar_count(self.view, body);
        let point = scalar / body_count;
        let body_scalar = scalar % body_count;
        let values = domain_view
            .structured()
            .index_tuple_at(point)
            .expect("checked domain remains valid")
            .expect("checked comprehension scalar point is in range");
        self.domain_points.push((domain, values));
        let result = self.expression(body, body_scalar);
        self.domain_points.pop();
        result
    }

    pub(super) fn index(
        &mut self,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        dimensions: &[u32],
        scalar: usize,
    ) -> Result<solve::Reg, LowerError> {
        let selector = ScalarSelector::from_points(self.view, &self.domain_points);
        match selector.indexed_base_scalar(base, subscripts, dimensions, scalar) {
            Ok(selected) => self.expression(base, selected),
            Err(LowerError::NonComputable { reason, .. })
                if reason == "array subscript is not compile-time computable" =>
            {
                self.dynamic_vector_index(base, subscripts, dimensions, scalar)
            }
            Err(error) => Err(error),
        }
    }

    fn dynamic_vector_index(
        &mut self,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        result_dimensions: &[u32],
        result_scalar: usize,
    ) -> Result<solve::Reg, LowerError> {
        let span = self.node(base).provenance().span();
        let [extent] = self.node(base).value_type().dimensions() else {
            return Err(LowerError::non_computable(
                "runtime indexing currently requires a rank-one array",
                span,
            ));
        };
        if !result_dimensions.is_empty() || result_scalar != 0 || subscripts.len() != 1 {
            return Err(LowerError::non_computable(
                "runtime indexing currently requires one scalar index and one scalar result",
                span,
            ));
        }
        let Some(dae::SubscriptView::Index {
            expression,
            provenance,
        }) = subscripts.get(0)
        else {
            return Err(LowerError::non_computable(
                "runtime slices do not yet have a computable Solve owner",
                span,
            ));
        };
        let span = provenance.span();
        let runtime_index = self.expression(expression, 0)?;
        let zero = self.constant(0.0, span)?;
        let mut selected = self.binary(dae::BinaryOperator::Divide, zero, zero, span)?;
        for ordinal in 0..*extent as usize {
            let candidate = self.expression(base, ordinal)?;
            let modelica_index = self.constant((ordinal + 1) as f64, span)?;
            let matches = self.binary(
                dae::BinaryOperator::Equal,
                runtime_index,
                modelica_index,
                span,
            )?;
            selected = self.select(matches, candidate, selected, span)?;
        }
        Ok(selected)
    }

    pub(super) fn select_array(
        &self,
        elements: dae::ExpressionOperands<'dae>,
        scalar: usize,
    ) -> (dae::ExprId<'dae>, usize) {
        let first = elements.get(0).expect("checked array is nonempty");
        let element_count = scalar_count(self.view, first);
        (
            elements
                .get(scalar / element_count)
                .expect("checked scalar selects an array element"),
            scalar % element_count,
        )
    }
}
