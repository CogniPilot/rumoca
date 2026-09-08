use rumoca_core::{InstanceId, StateSelect};

use super::*;

const FIRST_OCCURRENCE: InstanceId = InstanceId(41);
const SECOND_OCCURRENCE: InstanceId = InstanceId(42);

#[test]
fn source_occurrence_unset_is_rejected_with_typed_error() {
    let source = TestSource::new("Real x;");
    let declaration = source.source("Real x", 0);

    let result = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                declaration,
            )
        })?;
        dae.variables(|variables| {
            variables
                .algebraic(
                    VarName::new("x"),
                    InstanceId::UNSET,
                    real,
                    declaration,
                    explicit_local_attributes(),
                )
                .map(|_| ())
        })
    });

    assert_eq!(
        result.expect_err("an unset occurrence must not enter a finalized DAE"),
        DaeConstructionError::UnsetSourceOccurrence {
            name: VarName::new("x"),
            span: declaration.span(),
        }
    );
}

#[test]
fn source_occurrence_duplicate_is_rejected_with_typed_error() {
    let source = TestSource::new("Real x; Real y;");
    let x_declaration = source.source("Real x", 0);
    let y_declaration = source.source("Real y", 0);

    let result = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                x_declaration,
            )
        })?;
        dae.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                FIRST_OCCURRENCE,
                real,
                x_declaration,
                explicit_local_attributes(),
            )?;
            variables
                .algebraic(
                    VarName::new("y"),
                    FIRST_OCCURRENCE,
                    real,
                    y_declaration,
                    explicit_local_attributes(),
                )
                .map(|_| ())
        })
    });

    assert_eq!(
        result.expect_err("one occurrence must not identify two DAE variables"),
        DaeConstructionError::DuplicateSourceOccurrence {
            occurrence: FIRST_OCCURRENCE,
            span: y_declaration.span(),
        }
    );
}

#[test]
fn source_occurrence_current_wire_rejects_missing_zero_duplicate_and_legacy_fields() {
    let encoded = serde_json::to_value(two_variable_dae())
        .expect("the valid current-schema fixture serializes");

    let mut missing = encoded.clone();
    variable_wire_mut(&mut missing, 0).remove("source_occurrence");
    assert!(
        serde_json::from_value::<Dae>(missing).is_err(),
        "the current wire requires an occurrence field"
    );

    let mut zero = encoded.clone();
    variable_wire_mut(&mut zero, 0).insert("source_occurrence".into(), serde_json::json!(0));
    assert!(
        serde_json::from_value::<Dae>(zero).is_err(),
        "zero occurrence is rejected by the nonzero wire type"
    );

    let mut duplicate = encoded.clone();
    let first_occurrence = duplicate["storage"]["variables"][0]["source_occurrence"].clone();
    variable_wire_mut(&mut duplicate, 1).insert("source_occurrence".into(), first_occurrence);
    assert!(
        serde_json::from_value::<Dae>(duplicate).is_err(),
        "duplicate occurrence is rejected by checked reconstruction"
    );

    let mut legacy = encoded;
    let occurrence = variable_wire_mut(&mut legacy, 0)
        .remove("source_occurrence")
        .expect("fixture has the current occurrence field");
    variable_wire_mut(&mut legacy, 0).insert("instance_id".into(), occurrence);
    assert!(
        serde_json::from_value::<Dae>(legacy).is_err(),
        "legacy occurrence spelling is rejected, not migrated"
    );
}

fn two_variable_dae() -> Dae {
    let source = TestSource::new("Real x; Real y;");
    let x_declaration = source.source("Real x", 0);
    let y_declaration = source.source("Real y", 0);
    Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                x_declaration,
            )
        })?;
        dae.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                FIRST_OCCURRENCE,
                real,
                x_declaration,
                explicit_local_attributes(),
            )?;
            variables
                .algebraic(
                    VarName::new("y"),
                    SECOND_OCCURRENCE,
                    real,
                    y_declaration,
                    explicit_local_attributes(),
                )
                .map(|_| ())
        })
    })
    .expect("distinct nonzero occurrences construct a valid DAE")
}

fn variable_wire_mut(
    wire: &mut serde_json::Value,
    index: usize,
) -> &mut serde_json::Map<String, serde_json::Value> {
    wire["storage"]["variables"][index]
        .as_object_mut()
        .expect("serialized variable is a wire record")
}

fn explicit_local_attributes<'dae>() -> VariableAttributes<'dae> {
    VariableAttributes {
        component_ref: None,
        binding: None,
        start: None,
        fixed: None,
        min: None,
        max: None,
        nominal: None,
        unit: None,
        state_select: StateSelect::Default,
        description: None,
        causality: VariableCausality::Local,
        is_tunable: false,
        is_held: false,
        origin: VariableOrigin::Source,
    }
}
