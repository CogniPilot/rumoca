use super::*;

pub(super) fn validate_occurrence_target(
    name: &VarName,
    instance_id: InstanceId,
    component_ref: &ComponentReference,
    expected_kind: InstanceKind,
    relations: &IndexMap<InstanceId, InstanceRelation, rustc_hash::FxBuildHasher>,
    topology: &OccurrenceTopology,
) -> Result<(), FlatWireError> {
    if component_ref.to_var_name() != *name {
        return Err(FlatWireError::InvalidOccurrenceTarget {
            name: name.clone(),
            instance_id,
            reason: "its cached Flat name contradicts its exact component-reference path",
        });
    }
    let Some(relation) = relations.get(&instance_id) else {
        return Err(FlatWireError::InvalidOccurrenceTarget {
            name: name.clone(),
            instance_id,
            reason: "its occurrence is absent from the occurrence graph",
        });
    };
    if relation.kind != expected_kind {
        return Err(FlatWireError::InvalidOccurrenceTarget {
            name: name.clone(),
            instance_id,
            reason: "its occurrence kind contradicts the materialized target kind",
        });
    }
    validate_occurrence_path(name, instance_id, component_ref, relations, topology)
}

pub(super) fn validate_occurrence_path(
    name: &VarName,
    instance_id: InstanceId,
    component_ref: &ComponentReference,
    relations: &IndexMap<InstanceId, InstanceRelation, rustc_hash::FxBuildHasher>,
    topology: &OccurrenceTopology,
) -> Result<(), FlatWireError> {
    let mut cursor = instance_id;
    for (part_index, part) in component_ref.parts().iter().enumerate().rev() {
        let relation = &relations[&cursor];
        if relation.declaration != Some(part.def_id) {
            return Err(FlatWireError::InvalidOccurrenceTarget {
                name: name.clone(),
                instance_id,
                reason: "its occurrence declaration contradicts its exact component-reference path",
            });
        }
        let Some(indices) = literal_component_indices(&part.subs) else {
            return Err(FlatWireError::InvalidOccurrenceTarget {
                name: name.clone(),
                instance_id,
                reason: "an occurrence path requires literal component indices",
            });
        };
        if indices.as_slice() != relation.indices.as_ref() {
            return Err(FlatWireError::InvalidOccurrenceTarget {
                name: name.clone(),
                instance_id,
                reason: "its occurrence indices contradict its exact component-reference path",
            });
        }
        let preceding = topology.preceding_component[&cursor];
        if part_index == 0 {
            if preceding.is_some() {
                return Err(FlatWireError::InvalidOccurrenceTarget {
                    name: name.clone(),
                    instance_id,
                    reason: "its exact component-reference path omits an owning component",
                });
            }
        } else {
            cursor = preceding.ok_or_else(|| FlatWireError::InvalidOccurrenceTarget {
                name: name.clone(),
                instance_id,
                reason: "its exact component-reference path invents an absent owning component",
            })?;
            if relations[&cursor].kind != InstanceKind::Aggregate {
                return Err(FlatWireError::InvalidOccurrenceTarget {
                    name: name.clone(),
                    instance_id,
                    reason: "only an aggregate component may own another path component",
                });
            }
        }
    }
    Ok(())
}

pub(super) fn literal_component_indices(subscripts: &[Subscript]) -> Option<Vec<i64>> {
    subscripts
        .iter()
        .map(|subscript| match subscript {
            Subscript::Index { value, .. } => Some(*value),
            Subscript::Colon { .. } | Subscript::Expr { .. } => None,
        })
        .collect()
}

pub(super) fn insert_wire_target<'model>(
    targets: &mut rustc_hash::FxHashMap<InstanceId, WireTarget<'model>>,
    instance_id: InstanceId,
    target: WireTarget<'model>,
) -> Result<(), FlatWireError> {
    if targets.insert(instance_id, target).is_none() {
        return Ok(());
    }
    Err(FlatWireError::InvalidOccurrenceTarget {
        name: target.name.clone(),
        instance_id,
        reason: "one occurrence cannot materialize both a variable and a record target",
    })
}

pub(super) fn validate_record_type_shape(
    record_id: DefId,
    record: &RecordType,
    type_ids_by_def_id: &TypeIdentityMap,
    type_roots: &crate::TypeRootMap,
) -> Result<(), FlatWireError> {
    if record_id.index() == 0 || record.name.is_empty() {
        return Err(FlatWireError::InvalidRecordCatalog {
            record: record_id,
            reason: "record layouts require non-global declaration identity and a display name",
        });
    }
    let mut field_names = rustc_hash::FxHashSet::default();
    let mut field_declarations = rustc_hash::FxHashSet::default();
    for field in &record.fields {
        let declared_type = type_ids_by_def_id.get(&field.type_def_id);
        let exact_type = declared_type == Some(&field.effective_type.nominal_type())
            && type_roots.get(&field.effective_type.nominal_type())
                == Some(&field.effective_type.canonical_type())
            && type_roots.get(&field.effective_type.canonical_type())
                == Some(&field.effective_type.canonical_type())
            && field.effective_type.dimensions() == field.dims;
        if field.name.is_empty()
            || field.def_id.index() == 0
            || field.type_def_id.index() == 0
            || invalid_dimensions(&field.dims)
            || !exact_type
        {
            return Err(FlatWireError::InvalidRecordFieldShape {
                record: record_id,
                field: field.name.clone(),
            });
        }
        if !field_names.insert(field.name.clone()) || !field_declarations.insert(field.def_id) {
            return Err(FlatWireError::InvalidRecordCatalog {
                record: record_id,
                reason: "record field names and declaration identities must be unique",
            });
        }
    }
    Ok(())
}
