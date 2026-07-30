use super::*;

impl<'dae> ExpressionAt<'_, 'dae> {
    pub fn literal(self, value: DaeLiteral) -> Result<ExprId<'dae>, DaeConstructionError> {
        if matches!(value, DaeLiteral::Enumeration(_)) {
            return Err(DaeConstructionError::InvalidEnumerationOrdinal {
                ordinal: 0,
                span: self.provenance.span(),
            });
        }
        if matches!(value, DaeLiteral::Real(value) if !value.is_finite()) {
            return Err(DaeConstructionError::ExpectedNumeric {
                found: ScalarType::Real,
                span: self.provenance.span(),
            });
        }
        let ty = self
            .storage
            .intern_type(ValueType::scalar(value.scalar_type()), self.provenance)?;
        self.insert(
            ExprNode::Literal(value),
            ty,
            ExpressionVariability::Constant,
            None,
        )
    }

    pub fn enumeration_literal(self, ordinal: i64) -> Result<ExprId<'dae>, DaeConstructionError> {
        if ordinal < 1 {
            return Err(DaeConstructionError::InvalidEnumerationOrdinal {
                ordinal,
                span: self.provenance.span(),
            });
        }
        let value_type = self
            .storage
            .intern_type(ValueType::scalar(ScalarType::Enumeration), self.provenance)?;
        self.insert(
            ExprNode::Literal(DaeLiteral::Enumeration(ordinal)),
            value_type,
            ExpressionVariability::Constant,
            None,
        )
    }

    pub fn coordinate(
        self,
        coordinate: CoordinateInput<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let (ty, variability) = self.storage.coordinate_facts(coordinate, self.provenance)?;
        self.insert(
            ExprNode::Coordinate(coordinate.erase()),
            ty,
            variability,
            None,
        )
    }

    /// Construct one runtime-managed fixed transport-delay coordinate.
    ///
    /// The delay owner and its sole coordinate occurrence are committed
    /// together only after all owner and expression facts have been checked.
    pub fn delay(
        self,
        source: ExprId<'dae>,
        delay_time: PositiveParameter<'dae>,
        owner: DaeProvenance,
    ) -> Result<DelayCoordinate<'dae>, DaeConstructionError> {
        self.insert_delay_coordinate(
            source,
            DelayKind::ParameterDelay {
                delay_time: delay_time.entry,
            },
            owner,
        )
    }

    /// Construct one runtime-managed bounded transport-delay coordinate.
    pub fn bounded_delay(
        self,
        source: ExprId<'dae>,
        delay_time: ExprId<'dae>,
        delay_max: PositiveParameter<'dae>,
        owner: DaeProvenance,
    ) -> Result<DelayCoordinate<'dae>, DaeConstructionError> {
        let timing_at = self.storage.expr_provenance(delay_time, owner)?;
        self.storage
            .expect_closed_expression(delay_time, timing_at)?;
        let timing_type = self.storage.expr_type(delay_time, timing_at)?;
        if !timing_type.is_scalar() || timing_type.scalar_type() != ScalarType::Real {
            return Err(DaeConstructionError::TypeMismatch {
                expected: ScalarType::Real,
                found: timing_type.scalar_type(),
                span: timing_at.span(),
            });
        }
        self.insert_delay_coordinate(
            source,
            DelayKind::BoundedDelay {
                delay_time: delay_time.index(),
                delay_max: delay_max.entry,
            },
            owner,
        )
    }

    pub fn function_parameter(
        self,
        parameter: FunctionParameterId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let (ty, variability) = self
            .storage
            .function_parameter_facts(parameter, self.provenance)?;
        self.insert(
            ExprNode::Coordinate(Coordinate::FunctionParameter {
                function: parameter.function().index(),
                ordinal: parameter.ordinal(),
            }),
            ty,
            variability,
            None,
        )
    }
    pub fn binder(
        self,
        binder: DomainBinderId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        self.storage
            .domain_binder(binder.domain().index(), binder.ordinal(), self.provenance)?;
        let ty = self
            .storage
            .intern_type(ValueType::scalar(ScalarType::Integer), self.provenance)?;
        self.insert(
            ExprNode::Coordinate(Coordinate::Binder {
                domain: binder.domain().index(),
                ordinal: binder.ordinal(),
            }),
            ty,
            ExpressionVariability::Constant,
            Some(binder.domain().index()),
        )
    }

    fn insert_delay_coordinate(
        mut self,
        source: ExprId<'dae>,
        kind: DelayKind,
        owner: DaeProvenance,
    ) -> Result<DelayCoordinate<'dae>, DaeConstructionError> {
        crate::model::check_provenance(self.source_map, owner)?;
        let source_at = self.storage.expr_provenance(source, owner)?;
        self.storage.expect_closed_expression(source, source_at)?;
        let source_type = self.storage.expr_type(source, source_at)?;
        if source_type.scalar_type() == ScalarType::String {
            return Err(DaeConstructionError::ExpectedNumeric {
                found: ScalarType::String,
                span: source_at.span(),
            });
        }
        let value_type = self
            .storage
            .expressions
            .value_types
            .get(source.index() as usize)
            .copied()
            .ok_or_else(|| crate::model::unknown("expression", source.index(), source_at))?;
        let variability = self.storage.expr_variability(source, source_at)?;
        let delay = checked_u32(self.storage.delays.len(), "delay arena", owner)?;
        let node = ExprNode::Coordinate(Coordinate::Delay(delay));
        let (expression, facts) =
            self.prepare_insertion(&node, ValueTypeId::from_raw(value_type), variability, None)?;

        self.storage.delays.push(DelayEntry {
            source: source.index(),
            kind,
            provenance: owner,
        });
        let expression = self.commit_insertion(expression, node, facts);
        Ok(DelayCoordinate::new(DelayId::from_raw(delay), expression))
    }
}
