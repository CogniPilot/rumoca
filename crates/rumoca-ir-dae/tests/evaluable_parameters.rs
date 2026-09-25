//! The `evaluable` attribute licenses STRUCT-T10(a) folding only for a fixed,
//! non-tunable parameter whose binding reads constants and evaluable parameters.

use rumoca_core::{SourceMap, Span, VarName};
use rumoca_ir_dae::{
    BinaryOperator, CoordinateInput, Dae, DaeConstructionError, DaeLiteral, DaeProvenance,
    ScalarType, ValueType, VariableAttributes,
};

#[derive(Clone, Copy, Debug)]
enum Case {
    Accepted,
    ReadsTunable,
    Tunable,
    NotFixed,
    Unbound,
    State,
}

fn construct(case: Case) -> Result<Dae, DaeConstructionError> {
    let mut sources = SourceMap::new();
    let source = sources.add("evaluable.mo", "final parameter Real b = 2 * a;");
    let owner = DaeProvenance::source(Span::from_offsets(source, 0, 31)).unwrap();
    Dae::construct(sources, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), owner))?;
        let two =
            dae.expressions(|expressions| expressions.at(owner).literal(DaeLiteral::Real(2.0)))?;
        let tunable = dae.variables(|variables| {
            variables.parameter(
                VarName::new("k"),
                real,
                owner,
                VariableAttributes {
                    binding: Some(two),
                    is_tunable: true,
                    ..Default::default()
                },
            )
        })?;
        let evaluable = dae.variables(|variables| {
            variables.parameter(
                VarName::new("a"),
                real,
                owner,
                VariableAttributes {
                    binding: Some(two),
                    evaluable: true,
                    ..Default::default()
                },
            )
        })?;
        let read = if matches!(case, Case::ReadsTunable) {
            tunable
        } else {
            evaluable
        };
        let binding = dae.expressions(|expressions| {
            let value = expressions
                .at(owner)
                .coordinate(CoordinateInput::Parameter(read))?;
            expressions
                .at(owner)
                .binary(BinaryOperator::Multiply, two, value)
        })?;
        let attributes = VariableAttributes {
            binding: (!matches!(case, Case::Unbound | Case::State)).then_some(binding),
            fixed: matches!(case, Case::NotFixed).then(|| vec![false]),
            is_tunable: matches!(case, Case::Tunable),
            evaluable: true,
            ..Default::default()
        };
        dae.variables(|variables| {
            if matches!(case, Case::State) {
                variables
                    .state(VarName::new("b"), real, owner, attributes)
                    .map(drop)
            } else {
                variables
                    .parameter(VarName::new("b"), real, owner, attributes)
                    .map(drop)
            }
        })
    })
}

#[test]
fn an_evaluable_chain_constructs_and_round_trips_its_flag() {
    let dae = construct(Case::Accepted).unwrap();
    let flags = |dae: &Dae| {
        dae.inspect(|view| {
            view.variables()
                .map(|(_, variable)| (variable.name().to_string(), variable.is_evaluable()))
                .collect::<Vec<_>>()
        })
    };
    assert_eq!(
        flags(&dae),
        [
            ("k".to_string(), false),
            ("a".to_string(), true),
            ("b".to_string(), true)
        ]
    );
    let wire = serde_json::to_value(&dae).unwrap();
    let decoded: Dae = serde_json::from_value(wire.clone()).unwrap();
    assert_eq!(flags(&decoded), flags(&dae));
    assert_eq!(serde_json::to_value(decoded).unwrap(), wire);
}

#[test]
fn an_evaluable_flag_without_its_license_is_rejected() {
    for case in [
        Case::ReadsTunable,
        Case::Tunable,
        Case::NotFixed,
        Case::Unbound,
        Case::State,
    ] {
        let result = construct(case);
        assert!(
            matches!(
                result,
                Err(DaeConstructionError::InvalidEvaluableParameter { .. })
            ),
            "{case:?}: {result:?}"
        );
    }
}

#[test]
fn wire_replay_rejects_a_forged_evaluable_flag() {
    let dae = construct(Case::Accepted).unwrap();
    let mut wire = serde_json::to_value(&dae).unwrap();
    let mut forged = false;
    forge_tunable_flag(&mut wire, &mut forged);
    assert!(forged, "the wire names the tunable parameter's attributes");
    let error = serde_json::from_value::<Dae>(wire).unwrap_err().to_string();
    assert!(error.contains("marked evaluable"), "{error}");
}

/// Mark the tunable parameter `k` evaluable in its wire attributes.
fn forge_tunable_flag(value: &mut serde_json::Value, forged: &mut bool) {
    match value {
        serde_json::Value::Object(map) => {
            if map.get("name").and_then(serde_json::Value::as_str) == Some("k")
                && let Some(attributes) = map
                    .get_mut("attributes")
                    .and_then(serde_json::Value::as_object_mut)
            {
                attributes.insert("evaluable".into(), true.into());
                *forged = true;
            }
            for child in map.values_mut() {
                forge_tunable_flag(child, forged);
            }
        }
        serde_json::Value::Array(items) => {
            for child in items {
                forge_tunable_flag(child, forged);
            }
        }
        _ => {}
    }
}
