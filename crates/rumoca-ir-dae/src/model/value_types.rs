use std::collections::HashSet;

use super::*;

pub struct ValueTypes<'storage, 'dae> {
    pub(super) source_map: &'storage SourceMap,
    pub(super) storage: &'storage mut Storage,
    pub(super) marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> ValueTypes<'_, 'dae> {
    pub fn intern(
        &mut self,
        flat_type: TypeId,
        ty: ValueType,
        provenance: DaeProvenance,
    ) -> Result<ValueTypeId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.storage.intern_flat_type(flat_type, ty, provenance)
    }

    pub fn derived(
        &mut self,
        ty: ValueType,
        provenance: DaeProvenance,
    ) -> Result<ValueTypeId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.storage.intern_type(ty, provenance)
    }

    pub fn record(
        &mut self,
        name: VarName,
        fields: impl IntoIterator<Item = (VarName, ValueTypeId<'dae>)>,
        provenance: DaeProvenance,
    ) -> Result<ValueTypeId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let mut names = HashSet::new();
        let fields = fields
            .into_iter()
            .map(|(field, value_type)| {
                self.storage.value_type_at(value_type.index(), provenance)?;
                if !names.insert(field.clone()) {
                    return Err(DaeConstructionError::DuplicateDefinition {
                        kind: "record field",
                        index: value_type.index(),
                        span: provenance.span(),
                    });
                }
                Ok(crate::expression::RecordFieldType::new(
                    field,
                    value_type.index(),
                ))
            })
            .collect::<Result<Vec<_>, _>>()?;
        if fields.is_empty() {
            return Err(DaeConstructionError::InvalidArity {
                expected: 1,
                found: 0,
                span: provenance.span(),
            });
        }
        self.storage
            .intern_type(ValueType::record(name, fields), provenance)
    }

    pub fn expect_record_layout(
        &self,
        value_type: ValueTypeId<'dae>,
        fields: impl IntoIterator<Item = VarName>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let value_type = self.storage.value_type_at(value_type.index(), provenance)?;
        let fields = fields.into_iter().collect::<Vec<_>>();
        if value_type.is_record()
            && fields.len() == value_type.record_field_count()
            && fields
                .iter()
                .enumerate()
                .all(|(ordinal, field)| value_type.record_field_name(ordinal) == Some(field))
        {
            return Ok(());
        }
        Err(DaeConstructionError::ShapeMismatch {
            span: provenance.span(),
        })
    }
}
