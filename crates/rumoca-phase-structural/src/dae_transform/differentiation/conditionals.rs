use super::*;

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    /// Retain the issued branch order and invariant guards. Only branch values
    /// receive the requested derivative; tensor values keep their whole shape.
    pub(super) fn materialize_parameter_conditional(
        &mut self,
        operands: dae::ExpressionOperands<'source>,
        derivative_order: Option<u8>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let fallback = self.parameter_branch_value(
            operands.get(operands.len() - 1).unwrap(),
            derivative_order,
            provenance,
        )?;
        let mut branches = Vec::with_capacity((operands.len() - 1) / 2);
        for index in (0..operands.len() - 1).step_by(2) {
            let guard = self.rebuild_instantiated(operands.get(index).unwrap())?;
            let value = self.parameter_branch_value(
                operands.get(index + 1).unwrap(),
                derivative_order,
                provenance,
            )?;
            branches.push((guard, value));
        }
        self.target.at(provenance).conditional(branches, fallback)
    }

    fn parameter_branch_value(
        &mut self,
        source: dae::ExprId<'source>,
        derivative_order: Option<u8>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        match derivative_order {
            Some(order) => {
                let derivative = self.differentiate_order(source, order, provenance)?;
                self.materialize_derivative(derivative, source, provenance)
            }
            None => self.materialize_exact_value(source, provenance),
        }
    }
}
