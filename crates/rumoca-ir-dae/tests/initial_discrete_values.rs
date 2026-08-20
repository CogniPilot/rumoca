//! MLS §8.6 discrete initial-value owner: acceptance, rejection, wire replay.
//!
//! An `initial algorithm` that assigns a discrete-time variable determines the
//! value that variable holds when initialization finishes. The initialization
//! system owns that as a target/value definition, so these tests pin what the
//! constructor accepts, what it refuses, and that the wire cannot forge either.
use rumoca_core::{SourceMap, Span, VarName};
use rumoca_ir_dae::{
    BinaryOperator, CoordinateInput, Dae, DaeConstructionError, DaeLiteral, DaeProvenance,
    ScalarType, ValueType, VariableAttributes,
};

const TEXT: &str = "initial algorithm count := integer((time - startTime)/period);";

fn fixture() -> (SourceMap, DaeProvenance, DaeProvenance) {
    let mut source_map = SourceMap::new();
    let source = source_map.add("initial_discrete.mo", TEXT);
    let declaration =
        DaeProvenance::source(Span::from_offsets(source, 18, 23)).expect("declaration span");
    let owner = DaeProvenance::source(Span::from_offsets(source, 18, 61)).expect("assignment span");
    (source_map, declaration, owner)
}

/// The accepted shape: a scalar discrete coordinate whose value reads only
/// `time` and parameters, exactly what `Modelica.Blocks.Sources.Pulse` writes.
#[test]
fn discrete_initial_values_construct_and_round_trip_through_checked_wire() {
    let (source_map, declaration, owner) = fixture();
    let dae = Dae::construct(source_map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), owner))?;
        let integer =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Integer), owner))?;
        let start_time = dae.variables(|variables| {
            variables.parameter(
                VarName::new("startTime"),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let t_start = dae.variables(|variables| {
            variables.discrete_real(
                VarName::new("T_start"),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let count = dae.variables(|variables| {
            variables.discrete_value(
                VarName::new("count"),
                integer,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let (elapsed, ordinal) = dae.expressions(|expressions| {
            let time = expressions.at(owner).coordinate(CoordinateInput::Time)?;
            let parameter = expressions
                .at(owner)
                .coordinate(CoordinateInput::Parameter(start_time))?;
            let elapsed =
                expressions
                    .at(owner)
                    .binary(BinaryOperator::Subtract, time, parameter)?;
            let ordinal = expressions.at(owner).literal(DaeLiteral::Integer(3))?;
            Ok((elapsed, ordinal))
        })?;
        dae.initialization(|initialization| {
            initialization.discrete_real_initial_value(t_start, elapsed, owner)?;
            initialization.discrete_value_initial_value(count, ordinal, owner)?;
            Ok(())
        })?;
        let previous = dae.expressions(|expressions| {
            expressions
                .at(owner)
                .coordinate(CoordinateInput::PreDiscreteValue(count))
        })?;
        dae.b1c([count], |topology| {
            topology.owner(owner, [count], |staged| {
                staged.always(owner, [(previous, owner)])
            })?;
            Ok(())
        })
    })
    .expect("scalar discrete coordinates accept an initialization-instant value");

    assert_definitions(&dae);
    let encoded = serde_json::to_string(&dae).expect("schema-v12 DAE serializes");
    let decoded: Dae = serde_json::from_str(&encoded).expect("schema-v12 DAE reconstructs");
    assert_definitions(&decoded);

    // The wire cannot rename or drop the target: replay goes through the same
    // checked owner, so a forged column is rejected rather than defaulted.
    let forged = encoded.replacen(
        "\"initial_discrete_values\":[{\"target\"",
        "\"initial_discrete_values\":[{\"variable\"",
        1,
    );
    assert_ne!(
        forged, encoded,
        "wire fixture carries the definition column"
    );
    assert!(
        serde_json::from_str::<Dae>(&forged).is_err(),
        "wire-v12 cannot rename a discrete initial-value target"
    );
}

fn assert_definitions(dae: &Dae) {
    dae.inspect(|view| {
        assert_eq!(view.initial_discrete_value_count(), 2);
        let definitions = view.initial_discrete_values().collect::<Vec<_>>();
        let names = definitions
            .iter()
            .map(|definition| {
                view.variable(definition.target())
                    .expect("checked definition target resolves")
                    .name()
                    .to_string()
            })
            .collect::<Vec<_>>();
        assert_eq!(names, vec!["T_start".to_string(), "count".to_string()]);
        assert_eq!(
            view.source_text(definitions[0].provenance()),
            Some("count := integer((time - startTime)/period)")
        );
    });
}

/// A read that has no proven value at the initialization instant is refused:
/// the update row runs before any trajectory exists.
#[test]
fn discrete_initial_value_rejects_a_read_that_is_not_settled_at_initialization() {
    let (source_map, declaration, owner) = fixture();
    Dae::construct(source_map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), owner))?;
        let state = dae.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let t_start = dae.variables(|variables| {
            variables.discrete_real(
                VarName::new("T_start"),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let value = dae.expressions(|expressions| {
            expressions
                .at(owner)
                .coordinate(CoordinateInput::State(state))
        })?;
        dae.initialization(|initialization| {
            let rejected = initialization.discrete_real_initial_value(t_start, value, owner);
            assert!(
                matches!(
                    rejected,
                    Err(DaeConstructionError::InvalidExpressionForm { span })
                        if span == owner.span()
                ),
                "a state read has no proven initialization-instant value: {rejected:?}"
            );
            Ok(())
        })
    })
    .expect("the rejected definition leaves no partial owner")
    .inspect(|view| assert_eq!(view.initial_discrete_value_count(), 0));
}

/// One coordinate has exactly one initialization-instant value.
#[test]
fn discrete_initial_value_rejects_a_second_definition_of_one_coordinate() {
    let (source_map, declaration, owner) = fixture();
    Dae::construct(source_map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), owner))?;
        let t_start = dae.variables(|variables| {
            variables.discrete_real(
                VarName::new("T_start"),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let value =
            dae.expressions(|expressions| expressions.at(owner).literal(DaeLiteral::Real(1.0)))?;
        dae.initialization(|initialization| {
            initialization.discrete_real_initial_value(t_start, value, owner)?;
            let rejected = initialization.discrete_real_initial_value(t_start, value, owner);
            assert!(
                matches!(
                    rejected,
                    Err(DaeConstructionError::DuplicateDefinition { kind, .. })
                        if kind == "discrete initial value"
                ),
                "a duplicate definition is unrepresentable: {rejected:?}"
            );
            Ok(())
        })
    })
    .expect("the rejected duplicate leaves the first definition intact")
    .inspect(|view| assert_eq!(view.initial_discrete_value_count(), 1));
}

/// The value's primitive type is the coordinate's own type; nothing coerces.
#[test]
fn discrete_initial_value_rejects_a_value_of_another_primitive_type() {
    let (source_map, declaration, owner) = fixture();
    Dae::construct(source_map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), owner))?;
        let t_start = dae.variables(|variables| {
            variables.discrete_real(
                VarName::new("T_start"),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let value =
            dae.expressions(|expressions| expressions.at(owner).literal(DaeLiteral::Integer(1)))?;
        dae.initialization(|initialization| {
            let rejected = initialization.discrete_real_initial_value(t_start, value, owner);
            assert!(
                matches!(
                    rejected,
                    Err(DaeConstructionError::TypeMismatch {
                        expected: ScalarType::Real,
                        found: ScalarType::Integer,
                        ..
                    })
                ),
                "an Integer value cannot define a Real coordinate: {rejected:?}"
            );
            Ok(())
        })
    })
    .expect("the rejected definition leaves no partial owner")
    .inspect(|view| assert_eq!(view.initial_discrete_value_count(), 0));
}
