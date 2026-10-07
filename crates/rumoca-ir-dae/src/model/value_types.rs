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

    /// Read an interned value type.
    pub fn value_type(
        &self,
        id: ValueTypeId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<ValueType, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.storage.value_type_at(id.index(), provenance).cloned()
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
        self.record_array(name, fields, Vec::<u32>::new(), provenance)
    }

    pub fn record_array(
        &mut self,
        name: VarName,
        fields: impl IntoIterator<Item = (VarName, ValueTypeId<'dae>)>,
        dimensions: impl Into<Box<[u32]>>,
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
            return Err(invalid_arity(1, 0, provenance));
        }
        self.storage.intern_type(
            ValueType::record_array(name, fields, dimensions),
            provenance,
        )
    }

    /// The declared type of record field `field` of the scalar record type
    /// `record`, so a nested record value assembled field by field is built
    /// at exactly the type its parent declares.
    pub fn record_field(
        &mut self,
        record: ValueTypeId<'dae>,
        field: &VarName,
        provenance: DaeProvenance,
    ) -> Result<ValueTypeId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let record = self.storage.value_type_at(record.index(), provenance)?;
        let field_type = (0..record.record_field_count())
            .find(|ordinal| record.record_field_name(*ordinal) == Some(field))
            .and_then(|ordinal| record.record_field_type(ordinal))
            .filter(|_| record.dimensions().is_empty())
            .ok_or(DaeConstructionError::ShapeMismatch {
                span: provenance.span(),
            })?;
        let field_type = self.storage.value_type_at(field_type, provenance)?.clone();
        self.storage.intern_type(field_type, provenance)
    }

    /// The element type of the record array type `record_array`: the same
    /// record layout with no array extents.
    pub fn record_element(
        &mut self,
        record_array: ValueTypeId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<ValueTypeId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let record = self
            .storage
            .value_type_at(record_array.index(), provenance)?;
        if !record.is_record() {
            return Err(DaeConstructionError::ShapeMismatch {
                span: provenance.span(),
            });
        }
        let element = record.with_dimensions(Vec::<u32>::new());
        self.storage.intern_type(element, provenance)
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
