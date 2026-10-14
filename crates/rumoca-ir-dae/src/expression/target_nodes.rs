use super::*;

impl<'dae> ExpressionAt<'_, 'dae> {
    pub fn discrete_value_target(
        self,
        target: crate::DiscreteValueTarget<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        self.target_coordinate(target, CoordinateInput::DiscreteValue(target.variable()))
    }

    pub fn pre_discrete_value_target(
        self,
        target: crate::DiscreteValueTarget<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        self.target_coordinate(target, CoordinateInput::PreDiscreteValue(target.variable()))
    }

    fn target_coordinate(
        self,
        target: crate::DiscreteValueTarget<'dae>,
        coordinate: CoordinateInput<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let indices =
            crate::discrete_values::target_indices(self.storage, target, self.provenance)?;
        let mut expressions = Expressions {
            source_map: self.source_map,
            storage: self.storage,
            marker: self.marker,
        };
        let base = expressions.at(self.provenance).coordinate(coordinate)?;
        let Some(indices) = indices else {
            return Ok(base);
        };
        let subscripts = indices
            .into_iter()
            .map(|index| {
                Ok(Subscript::Index {
                    expression: expressions
                        .at(self.provenance)
                        .literal(DaeLiteral::Integer(index))?,
                    provenance: self.provenance,
                })
            })
            .collect::<Result<Vec<_>, DaeConstructionError>>()?;
        expressions.at(self.provenance).index(base, subscripts)
    }
}
