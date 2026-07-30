use super::*;

impl<'dae> ExpressionAt<'_, 'dae> {
    pub fn record(
        self,
        value_type: ValueTypeId<'dae>,
        fields: impl IntoIterator<Item = ExprId<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let fields = fields.into_iter().collect::<Vec<_>>();
        let record = self
            .storage
            .value_type_at(value_type.index(), self.provenance)?
            .clone();
        if !record.is_record() || fields.len() != record.record_field_count() {
            return Err(invalid_arity(
                record.record_field_count(),
                fields.len(),
                self.provenance,
            ));
        }
        let mut variability = ExpressionVariability::Constant;
        for (ordinal, field) in fields.iter().copied().enumerate() {
            self.storage.expect_value_type_compatible(
                record
                    .record_field_type(ordinal)
                    .expect("record field ordinal is in range"),
                definition_type(self.storage, field, self.provenance)?,
                self.provenance,
            )?;
            variability = variability.max(self.storage.expr_variability(field, self.provenance)?);
        }
        let binder_domain =
            merged_binder_domain(self.storage, fields.iter().copied(), self.provenance)?;
        let operands = self
            .storage
            .expressions
            .push_operands(fields.into_iter().map(ExprId::index), self.provenance)?;
        self.insert(
            ExprNode::Record { operands },
            value_type,
            variability,
            binder_domain,
        )
    }

    pub fn field(
        self,
        base: ExprId<'dae>,
        field: usize,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let record = self.storage.expr_type(base, self.provenance)?;
        let field_type = record.record_field_type(field).ok_or_else(|| {
            invalid_arity(record.record_field_count(), field + 1, self.provenance)
        })?;
        let variability = self.storage.expr_variability(base, self.provenance)?;
        let binder_domain = self.storage.expr_binder_domain(base, self.provenance)?;
        let field = checked_u32(field, "record field", self.provenance)?;
        self.insert(
            ExprNode::Field {
                base: base.index(),
                field,
            },
            ValueTypeId::from_raw(field_type),
            variability,
            binder_domain,
        )
    }
}
