//! Current-wire obligations of the total `fixed` attribute.
//!
//! `VariableAttributesWire.fixed` is a bare [`rumoca_core::Fixity`] carried as
//! the strict boolean the MLS attribute means. The wire therefore has no
//! spelling for absence: a payload without the key, or with anything other
//! than a boolean in it, is not a superseded shape to migrate but an invalid
//! document to reject.

use rumoca_core::{InstanceId, StateSelect};

use super::*;

const PARAMETER_OCCURRENCE: InstanceId = InstanceId(51);
const ALGEBRAIC_OCCURRENCE: InstanceId = InstanceId(52);

#[test]
fn fixed_wire_is_a_required_boolean_and_nothing_else() {
    let encoded = serde_json::to_value(defaulted_pair_dae())
        .expect("the valid current-schema fixture serializes");

    // Positive control: the untouched document replays, so each rejection
    // below is attributable to its one-field mutation and nothing else.
    serde_json::from_value::<Dae>(encoded.clone())
        .expect("the unmutated fixture replays through the current wire");

    // Positive control: the node the mutations strike is the role-decided
    // total, inverted across the two roles. This pins the JSON path (a
    // mutation aimed at the wrong node proves nothing) and is itself a wire
    // oracle: both declarations omitted `fixed`, so `true` here can only be
    // the parameter default and `false` only the non-parameter default.
    assert_eq!(
        encoded["storage"]["variables"][0]["attributes"]["fixed"],
        serde_json::json!(true),
        "a defaulted parameter serializes its section 4.8.1 default"
    );
    assert_eq!(
        encoded["storage"]["variables"][1]["attributes"]["fixed"],
        serde_json::json!(false),
        "a defaulted algebraic serializes its section 4.8.1 default"
    );

    for (label, value) in [
        ("an explicit null", serde_json::json!(null)),
        ("a string spelling", serde_json::json!("true")),
        ("an object wrapper", serde_json::json!({ "fixed": true })),
    ] {
        let mut mutated = encoded.clone();
        attributes_wire_mut(&mut mutated, 0).insert("fixed".into(), value);
        assert!(
            serde_json::from_value::<Dae>(mutated).is_err(),
            "the current wire must reject {label} for `fixed`: only a boolean is a value"
        );
    }

    let mut missing = encoded;
    attributes_wire_mut(&mut missing, 0)
        .remove("fixed")
        .expect("the fixture carries the current `fixed` key");
    assert!(
        serde_json::from_value::<Dae>(missing).is_err(),
        "the current wire must reject an omitted `fixed`: absence is not representable"
    );
}

fn attributes_wire_mut(
    wire: &mut serde_json::Value,
    index: usize,
) -> &mut serde_json::Map<String, serde_json::Value> {
    wire["storage"]["variables"][index]["attributes"]
        .as_object_mut()
        .expect("serialized variable attributes are a wire record")
}

/// One defaulted parameter and one defaulted algebraic; both omit `fixed`.
fn defaulted_pair_dae() -> Dae {
    let source = TestSource::new("parameter Real p; Real x;");
    let p_declaration = source.source("parameter Real p", 0);
    let x_declaration = source.source("Real x", 0);
    Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                p_declaration,
            )
        })?;
        dae.variables(|variables| {
            variables.parameter(
                VarName::new("p"),
                PARAMETER_OCCURRENCE,
                real,
                p_declaration,
                defaulted_attributes(),
            )?;
            variables
                .algebraic(
                    VarName::new("x"),
                    ALGEBRAIC_OCCURRENCE,
                    real,
                    x_declaration,
                    defaulted_attributes(),
                )
                .map(|_| ())
        })
    })
    .expect("two defaulted declarations construct a valid DAE")
}

fn defaulted_attributes<'dae>() -> VariableAttributes<'dae> {
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
