//! Replay the declaration layer a rebuilt DAE needs before any expression.
//!
//! Value types, structured domains and reserved conditions all precede the
//! expressions that name them, so they are rebuilt first and returned as
//! ordinal-indexed tables. Every table keeps the source ordinal as its index,
//! which is what lets the later stages translate a source identity into its
//! target identity by a plain lookup.

use rumoca_ir_dae as dae;

pub(super) fn rebuild_types<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
) -> Result<Vec<dae::ValueTypeId<'target>>, dae::DaeConstructionError> {
    target.types(|types| {
        let mut rebuilt = Vec::with_capacity(source.value_type_count());
        for index in 0..source.value_type_count() {
            let source_id = source
                .value_type_id(index)
                .expect("finalized value type ordinal resolves");
            let value_type = source
                .value_type(source_id)
                .expect("finalized value type identity resolves");
            let provenance = source
                .value_type_provenance(source_id)
                .expect("finalized value type has provenance");
            let rebuilt_type = if value_type.is_record() {
                rebuild_record_type(source, source_id, value_type, provenance, &rebuilt, types)?
            } else {
                let value_type = value_type.clone();
                match source.effective_flat_type(source_id) {
                    Some(flat_type) => types.intern(flat_type, value_type, provenance)?,
                    None => types.derived(value_type, provenance)?,
                }
            };
            rebuilt.push(rebuilt_type);
        }
        Ok(rebuilt)
    })
}

fn rebuild_record_type<'source, 'target>(
    source: dae::DaeView<'source>,
    source_id: dae::ValueTypeId<'source>,
    value_type: &dae::ValueType,
    provenance: dae::DaeProvenance,
    rebuilt: &[dae::ValueTypeId<'target>],
    types: &mut dae::ValueTypes<'_, 'target>,
) -> Result<dae::ValueTypeId<'target>, dae::DaeConstructionError> {
    let fields = (0..value_type.record_field_count()).map(|ordinal| {
        let (name, field_type) = source
            .record_field(source_id, ordinal)
            .expect("checked record field ordinal resolves");
        let field_type = rebuilt
            .get(field_type.index() as usize)
            .copied()
            .expect("checked record field type precedes its record owner");
        (name.clone(), field_type)
    });
    types.record(
        value_type
            .record_name()
            .expect("checked record has a canonical name")
            .clone(),
        fields,
        provenance,
    )
}

pub(super) struct RebuiltDomain<'dae> {
    pub(super) id: dae::DomainId<'dae>,
    pub(super) binders: Vec<dae::DomainBinderId<'dae>>,
}

pub(super) fn rebuild_domains<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
) -> Result<Vec<RebuiltDomain<'target>>, dae::DaeConstructionError> {
    let mut rebuilt: Vec<RebuiltDomain<'target>> = Vec::with_capacity(source.domain_count());
    for index in 0..source.domain_count() {
        let source_id = source
            .domain_id(index)
            .expect("finalized domain ordinal resolves");
        let domain = source
            .domain(source_id)
            .expect("finalized domain identity resolves");
        let provenance = domain.provenance();
        let id = target.domains(|domains| match domain.parent() {
            Some(parent) => domains.nested(
                rebuilt[parent.index() as usize].id,
                domain.structured().clone(),
                provenance,
            ),
            None => domains.structured(domain.structured().clone(), provenance),
        })?;
        let binders = target.domains(|domains| {
            (0..domain.structured().binders.len())
                .map(|ordinal| domains.binder(id, ordinal, provenance))
                .collect::<Result<Vec<_>, _>>()
        })?;
        rebuilt.push(RebuiltDomain { id, binders });
    }
    Ok(rebuilt)
}

pub(super) fn reserve_conditions<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
) -> Result<Vec<dae::ConditionId<'target>>, dae::DaeConstructionError> {
    (0..source.condition_count())
        .map(|index| {
            let id = source
                .condition_id(index)
                .expect("finalized condition ordinal resolves");
            let condition = source
                .condition(id)
                .expect("finalized condition identity resolves");
            target.conditions(|conditions| conditions.reserve(condition.provenance()))
        })
        .collect()
}
