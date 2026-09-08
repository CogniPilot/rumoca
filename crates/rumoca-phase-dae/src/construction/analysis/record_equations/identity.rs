use super::*;

pub(super) fn unique_instances<'flat, T>(
    values: impl IntoIterator<Item = &'flat T>,
    instance: impl Fn(&T) -> InstanceId,
    name: impl Fn(&T) -> String,
    span: impl Fn(&T) -> Span,
    family: &'static str,
) -> Result<HashMap<InstanceId, &'flat T>, ToDaeError> {
    let mut result = HashMap::new();
    for value in values {
        let id = instance(value);
        if let Some(previous) = result.insert(id, value) {
            return Err(ToDaeError::unsupported_flat(
                family,
                format!(
                    "Flat occurrences `{}` and `{}` share exact instance {}",
                    name(previous),
                    name(value),
                    id.index()
                ),
                span(value),
            ));
        }
    }
    Ok(result)
}

pub(super) fn require_materialized_field(
    flat: &flat::Model,
    owner: &flat::RecordInstance,
    variable: &flat::Variable,
    field: &flat::RecordField,
    span: Span,
) -> Result<(), ToDaeError> {
    let exact = variable.dims == field.dims
        && declared_effective_type(flat, field)
        && exact_effective_type(flat, variable.type_id, &field.effective_type)
        && variable.component_ref.as_ref().is_some_and(|reference| {
            extends_record_reference(&owner.component_ref, reference, field)
        });
    if exact {
        Ok(())
    } else {
        Err(record_layout_error(
            format!(
                "field `{}` disagrees with its exact declaration identity, declared type, or dimensions",
                field.name
            ),
            span,
        ))
    }
}

pub(super) fn declared_effective_type(flat: &flat::Model, field: &flat::RecordField) -> bool {
    flat.type_ids_by_def_id.get(&field.type_def_id) == Some(&field.effective_type.nominal_type())
        && flat.type_roots.get(&field.effective_type.nominal_type())
            == Some(&field.effective_type.canonical_type())
        && flat.type_roots.get(&field.effective_type.canonical_type())
            == Some(&field.effective_type.canonical_type())
}

pub(super) fn exact_effective_type(
    flat: &flat::Model,
    type_id: rumoca_core::TypeId,
    expected: &EffectiveType,
) -> bool {
    flat.effective_types.get(&type_id) == Some(expected)
        && flat.type_roots.get(&type_id) == Some(&expected.canonical_type())
}

pub(super) fn extends_record_reference(
    owner: &rumoca_core::ComponentReference,
    child: &rumoca_core::ComponentReference,
    field: &flat::RecordField,
) -> bool {
    child.local() == owner.local()
        && child.parts().len() == owner.parts().len() + 1
        && child.parts()[..owner.parts().len()]
            .iter()
            .zip(owner.parts())
            .all(|(child, owner)| {
                child.ident == owner.ident
                    && child.def_id == owner.def_id
                    && rumoca_core::subscripts_semantically_equal(&child.subs, &owner.subs)
            })
        && child.parts().last().is_some_and(|part| {
            part.ident == field.name && part.def_id == field.def_id && part.subs.is_empty()
        })
}

pub(super) fn missing_record_identity(
    reference: &rumoca_core::Reference,
    span: Span,
) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "record equation identity",
        format!(
            "`{}` disagrees with its exact Flat occurrence and declaration path",
            reference.as_str()
        ),
        span,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{BytePos, SourceId};

    #[derive(Debug)]
    struct Occurrence {
        id: InstanceId,
        name: &'static str,
        span: Span,
    }

    #[test]
    fn duplicate_identity_refusal_names_both_owners_at_the_second_span() {
        let source = SourceId::from_source_name("duplicate-identity.mo");
        let first = Occurrence {
            id: InstanceId::new(7),
            name: "plant.first",
            span: Span::new(source, BytePos(10), BytePos(21)),
        };
        let second_span = Span::new(source, BytePos(30), BytePos(42));
        let second = Occurrence {
            id: first.id,
            name: "plant.second",
            span: second_span,
        };

        let error = unique_instances(
            [&first, &second],
            |value| value.id,
            |value| value.name.to_owned(),
            |value| value.span,
            "runtime coordinate identity",
        )
        .expect_err("one occurrence identity cannot own two runtime coordinates");

        assert!(matches!(
            error,
            ToDaeError::UnsupportedFlatSemantics {
                feature,
                detail,
                span,
            } if feature == "runtime coordinate identity"
                && detail.contains("plant.first")
                && detail.contains("plant.second")
                && span == second_span
        ));
    }
}
