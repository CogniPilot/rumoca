use super::*;

#[test]
fn exact_clocks_own_each_clocked_variable_once() {
    let source = TestSource::new(
        "discrete Real z; discrete Boolean m; when trigger then z = 1; m = true; end when; \
         previous(z); terminal();",
    );
    let z_at = source.source("discrete Real z", 0);
    let m_at = source.source("discrete Boolean m", 0);
    let trigger_at = source.source("trigger", 0);
    let owner = source.source("when trigger then z = 1; m = true; end when", 0);
    let previous_at = source.source("previous(z)", 0);
    let terminal_at = source.source("terminal()", 0);

    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(TypeId::new(0), ValueType::scalar(ScalarType::Real), z_at)
        })?;
        let boolean = dae.types(|types| {
            types.intern(TypeId::new(1), ValueType::scalar(ScalarType::Boolean), m_at)
        })?;
        let (z, m) = dae.variables(|variables| {
            Ok((
                variables.discrete_real(
                    VarName::new("z"),
                    real,
                    z_at,
                    VariableAttributes::default(),
                )?,
                variables.discrete_value(
                    VarName::new("m"),
                    boolean,
                    m_at,
                    VariableAttributes::default(),
                )?,
            ))
        })?;
        let condition = dae.conditions(|conditions| conditions.reserve(trigger_at))?;
        let trigger = dae.expressions(|expressions| {
            expressions
                .at(trigger_at)
                .literal(DaeLiteral::Boolean(true))
        })?;
        dae.conditions(|conditions| {
            conditions.define(condition, ConditionInput::Discrete(trigger), trigger_at)
        })?;
        dae.discrete(|discrete| discrete.assignment(owner, m, trigger))?;

        let periodic = dae.clocks(|clocks| {
            let lattice = ClockLattice::new(
                ClockRational::new(1, 10).unwrap(),
                ClockRational::new(1, 20).unwrap(),
            )
            .unwrap();
            let periodic = clocks.periodic(lattice, owner)?;
            let triggered = clocks.triggered(condition, trigger_at)?;
            let first_owner = clocks.own_discrete_real(periodic, z, owner)?;
            let repeated_owner = clocks.own_discrete_real(periodic, z, owner)?;
            assert_eq!(first_owner, repeated_owner);
            clocks.own_discrete_value(triggered, m, owner)?;
            let duplicate = clocks.own_discrete_real(triggered, z, owner);
            assert!(matches!(
                duplicate,
                Err(DaeConstructionError::DuplicateKey {
                    kind: "clocked variable owner",
                    ..
                })
            ));
            Ok(periodic)
        })?;
        let (previous, terminal) = dae.temporal(|temporal| {
            Ok((
                temporal.previous_discrete_real(periodic, z, previous_at)?,
                temporal.terminal(terminal_at)?,
            ))
        })?;
        dae.expressions(|expressions| {
            expressions
                .at(previous_at)
                .coordinate(CoordinateInput::Previous(previous))?;
            expressions
                .at(terminal_at)
                .coordinate(CoordinateInput::Terminal(terminal))?;
            Ok(())
        })
    })
    .unwrap();

    dae.inspect(assert_clock_views);
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        assert_eq!(view.clock_count(), 2);
        assert_eq!(view.clock_ownership_count(), 2);
        assert_eq!(view.previous_value_count(), 1);
        assert_eq!(view.terminal_count(), 1);
    });

    let mut forged: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    forged["storage"]["clocks"][0]["kind"]["periodic"]["period"]["den"] = 0.into();
    assert!(matches!(
        serde_json::from_value::<Dae>(forged),
        Err(error) if error.to_string().contains("invalid exact DAE clock value")
    ));
}

#[test]
fn clock_guarded_assignment_requires_matching_clock_ownership() {
    let source = TestSource::new("discrete Real z; when sample(0, 1) then z = 1; end when;");
    let z_at = source.source("discrete Real z", 0);
    let sample_at = source.source("sample(0, 1)", 0);
    let assignment_at = source.source("z = 1", 0);
    let error = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(TypeId::new(0), ValueType::scalar(ScalarType::Real), z_at)
        })?;
        let z = dae.variables(|variables| {
            variables.discrete_real(VarName::new("z"), real, z_at, VariableAttributes::default())
        })?;
        let clock = dae.clocks(|clocks| {
            clocks.periodic(
                ClockLattice::new(ClockRational::ONE, ClockRational::ZERO).unwrap(),
                sample_at,
            )
        })?;
        let guard = dae.conditions(|conditions| conditions.reserve(sample_at))?;
        dae.conditions(|conditions| {
            conditions.define(guard, ConditionInput::Clock(clock), sample_at)
        })?;
        let value = dae.expressions(|expressions| {
            expressions.at(assignment_at).literal(DaeLiteral::Real(1.0))
        })?;
        dae.events(|events| events.assign_discrete_real(guard, guard, z, value, assignment_at))?;
        Ok(())
    })
    .unwrap_err();
    assert!(matches!(
        error,
        DaeConstructionError::MissingClockOwnership { .. }
    ));
}

fn assert_clock_views(view: DaeView<'_>) {
    assert_eq!(view.clock_count(), 2);
    assert_eq!(view.clock_ownership_count(), 2);
    assert_eq!(view.previous_value_count(), 1);
    assert_eq!(view.terminal_count(), 1);
    assert!(matches!(
        view.clock(view.clock_id(0).unwrap()).unwrap().operation(),
        ClockOperation::Periodic(_)
    ));
    assert!(matches!(
        view.clock(view.clock_id(1).unwrap()).unwrap().operation(),
        ClockOperation::Triggered(_)
    ));
    let ownership = view
        .clock_ownership(view.clock_ownership_id(0).unwrap())
        .unwrap();
    assert_eq!(ownership.kind(), ClockedVariableKind::DiscreteReal);
    let previous = view.previous(view.previous_id(0).unwrap()).unwrap();
    assert_eq!(previous.clock(), ownership.clock());
    assert!(view.terminal(view.terminal_id(0).unwrap()).is_some());
}

#[test]
fn wire_v11_round_trip_preserves_checked_quotients_and_their_provenance() {
    let source = TestSource::new("div(-7, 3); mod(-7, 3); rem(-7, 3)");
    let div_owner = source.source("div(-7, 3)", 0);
    let mod_owner = source.source("mod(-7, 3)", 0);
    let rem_owner = source.source("rem(-7, 3)", 0);
    let lhs_at = source.source("-7", 0);
    let rhs_at = source.source("3", 0);
    let dae = Dae::construct(source.map, |dae| {
        let lhs =
            dae.expressions(|expressions| expressions.at(lhs_at).literal(DaeLiteral::Integer(-7)))?;
        let rhs =
            dae.expressions(|expressions| expressions.at(rhs_at).literal(DaeLiteral::Integer(3)))?;
        for (owner, builtin) in [
            (div_owner, PureBuiltin::Div),
            (mod_owner, PureBuiltin::Mod),
            (rem_owner, PureBuiltin::Rem),
        ] {
            dae.expressions(|expressions| expressions.at(owner).builtin(builtin, [lhs, rhs]))?;
        }
        Ok(())
    })
    .unwrap();

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        for (index, builtin, text) in [
            (2, PureBuiltin::Div, "div(-7, 3)"),
            (3, PureBuiltin::Mod, "mod(-7, 3)"),
            (4, PureBuiltin::Rem, "rem(-7, 3)"),
        ] {
            let expression = view
                .expression(view.expression_id(index).unwrap())
                .expect("wire-reconstructed quotient expression");
            assert!(matches!(
                expression.operation(),
                ExpressionOperation::Builtin {
                    builtin: found,
                    ..
                } if found == builtin
            ));
            assert_eq!(view.source_text(expression.provenance()), Some(text));
        }
    });
}

#[test]
fn wire_v11_round_trip_preserves_provenance_without_inline_source_copies() {
    let source = TestSource::new("42");
    let literal = source.source("42", 0);
    let dae = Dae::construct(source.map, |dae| {
        dae.expressions(|expr| {
            expr.at(literal).literal(DaeLiteral::Integer(42))?;
            Ok(())
        })
    })
    .unwrap();

    let json = serde_json::to_string(&dae).unwrap();
    assert_eq!(
        json.matches("42").count(),
        2,
        "source text plus literal value"
    );
    let decoded: Dae = serde_json::from_str(&json).unwrap();
    assert_eq!(decoded.schema_version(), DAE_SCHEMA_VERSION);
    decoded.inspect(|view| {
        let expression = view.expression(view.expression_id(0).unwrap()).unwrap();
        assert_eq!(view.source_text(expression.provenance()), Some("42"));
    });

    let binary = bincode::serialize(&dae).unwrap();
    let decoded_binary: Dae = bincode::deserialize(&binary).unwrap();
    decoded_binary.inspect(|view| {
        let expression = view.expression(view.expression_id(0).unwrap()).unwrap();
        assert_eq!(view.source_text(expression.provenance()), Some("42"));
    });

    let wrong_version = json.replacen(
        &format!("\"schema_version\":{DAE_SCHEMA_VERSION}"),
        "\"schema_version\":10",
        1,
    );
    assert!(matches!(
        serde_json::from_str::<Dae>(&wrong_version),
        Err(error) if error.to_string().contains("unsupported DAE schema version 10")
    ));

    let mut malformed: serde_json::Value = serde_json::from_str(&json).unwrap();
    malformed["storage"]["expressions"]["provenance"]
        .as_array_mut()
        .unwrap()
        .clear();
    assert!(matches!(
        serde_json::from_value::<Dae>(malformed),
        Err(error) if error.to_string().contains("malformed DAE wire column")
    ));
}

#[test]
fn construction_rejects_dummy_unknown_and_out_of_range_provenance() {
    assert!(matches!(
        DaeProvenance::source(Span::DUMMY),
        Err(DaeConstructionError::MissingProvenance { .. })
    ));

    let source = TestSource::new("x");
    let unknown = DaeProvenance::source(Span::from_offsets(
        SourceId::from_source_name("missing.mo"),
        0,
        1,
    ))
    .unwrap();
    let out_of_range =
        DaeProvenance::source(Span::from_offsets(source.id, 0, source.text.len() + 1)).unwrap();

    let result = Dae::construct(source.map, |dae| {
        dae.expressions(|expr| {
            expr.at(unknown).literal(DaeLiteral::Integer(1))?;
            Ok(())
        })
    });
    assert!(matches!(
        result,
        Err(DaeConstructionError::UnknownSource { .. })
    ));

    let mut map = SourceMap::new();
    map.add("construction.mo", "x");
    let result = Dae::construct(map, |dae| {
        dae.expressions(|expr| {
            expr.at(out_of_range).literal(DaeLiteral::Integer(1))?;
            Ok(())
        })
    });
    assert!(matches!(
        result,
        Err(DaeConstructionError::InvalidSourceRange { .. })
    ));
}
