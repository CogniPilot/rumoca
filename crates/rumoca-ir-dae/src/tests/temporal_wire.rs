use super::*;

#[test]
fn exact_clocks_own_each_clocked_variable_once() {
    let dae = exact_clock_fixture();
    dae.inspect(assert_clock_views);
    assert_clock_wire_round_trip(&dae);
}

fn exact_clock_fixture() -> Dae {
    let source = TestSource::new(
        "discrete Real z; discrete Boolean m; when trigger then z = 1; m = true; end when; \
         when trigger then z = 1; end when; when other then z = 2; end when; \
         previous(z); previous(z); interval(z); terminal();",
    );
    let z_at = source.source("discrete Real z", 0);
    let m_at = source.source("discrete Boolean m", 0);
    let trigger_at = source.source("trigger", 0);
    let owner = source.source("when trigger then z = 1; m = true; end when", 0);
    let repeated_owner = source.source("when trigger then z = 1; end when", 0);
    let conflicting_owner = source.source("when other then z = 2; end when", 0);
    let first_previous = source.source("previous(z)", 0);
    let repeated_previous = source.source("previous(z)", 1);
    let interval_at = source.source("interval(z)", 0);
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
        let periodic = dae.clocks(|clocks| {
            let lattice = ClockLattice::new(
                ClockRational::new(1, 10).unwrap(),
                ClockRational::new(1, 20).unwrap(),
            )
            .unwrap();
            let periodic = clocks.periodic(lattice, owner)?;
            let triggered = clocks.triggered(condition, trigger_at)?;
            let first_owner = clocks.own_discrete_real(periodic.into(), z, owner)?;
            let repeated_owner_id = clocks.own_discrete_real(periodic.into(), z, repeated_owner)?;
            assert_eq!(first_owner, repeated_owner_id);
            clocks.own_discrete_value(triggered, m, owner)?;
            let conflict = clocks
                .own_discrete_real(triggered, z, conflicting_owner)
                .expect_err("one variable cannot belong to two clocks");
            assert_clock_conflict(
                conflict,
                (z.index(), periodic.index(), triggered.index()),
                (owner, conflicting_owner),
            );
            Ok(periodic)
        })?;
        dae.b1c([m], |topology| {
            topology.owner(owner, [m], |branch_owner| {
                branch_owner.when(condition, condition, trigger_at, [(trigger, owner)])
            })?;
            Ok(())
        })?;
        let (previous, terminal) = dae.temporal(|temporal| {
            let first = temporal.previous_discrete_real(periodic.into(), z, first_previous)?;
            let repeated =
                temporal.previous_discrete_real(periodic.into(), z, repeated_previous)?;
            assert_eq!(first, repeated);
            Ok((first, temporal.terminal(terminal_at)?))
        })?;
        dae.expressions(|expressions| {
            expressions
                .at(first_previous)
                .coordinate(CoordinateInput::Previous(previous))?;
            expressions
                .at(interval_at)
                .coordinate(CoordinateInput::ClockInterval(periodic))?;
            expressions
                .at(terminal_at)
                .coordinate(CoordinateInput::Terminal(terminal))?;
            Ok(())
        })
    })
    .unwrap();
    assert_first_clock_provenance(&dae, owner, first_previous);
    dae
}

fn assert_clock_conflict(
    conflict: DaeConstructionError,
    expected_ids: (u32, u32, u32),
    expected_provenance: (DaeProvenance, DaeProvenance),
) {
    let DaeConstructionError::ConflictingClockOwnership {
        variable,
        established_clock,
        attempted_clock,
        established,
        attempted,
    } = conflict
    else {
        panic!("clock conflict has a typed construction error");
    };
    assert_eq!((variable, established_clock, attempted_clock), expected_ids);
    assert_eq!((established, attempted), expected_provenance);
}

fn assert_first_clock_provenance(dae: &Dae, owner: DaeProvenance, first_previous: DaeProvenance) {
    dae.inspect(|view| {
        let ownership = view
            .clock_ownership(view.clock_ownership_id(0).unwrap())
            .unwrap();
        assert_eq!(ownership.provenance(), owner);
        let previous = view.previous(view.previous_id(0).unwrap()).unwrap();
        assert_eq!(previous.provenance(), first_previous);
        let interval = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .find(|expression| {
                matches!(
                    expression.operation(),
                    ExpressionOperation::Coordinate(CoordinateView::ClockInterval(clock))
                        if clock.index() == 0
                )
            })
            .expect("clock interval coordinate remains typed");
        assert_eq!(view.source_text(interval.provenance()), Some("interval(z)"));
        assert_eq!(interval.variability(), ExpressionVariability::Discrete);
        assert_eq!(interval.value_type().scalar_type(), ScalarType::Real);
    });
}

fn assert_clock_wire_round_trip(dae: &Dae) {
    let encoded = serde_json::to_string(dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        assert_eq!(view.clock_count(), 2);
        assert_eq!(view.clock_ownership_count(), 2);
        assert_eq!(view.previous_value_count(), 1);
        assert_eq!(view.terminal_count(), 1);
        let interval = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .find(|expression| {
                matches!(
                    expression.operation(),
                    ExpressionOperation::Coordinate(CoordinateView::ClockInterval(clock))
                        if clock.index() == 0
                )
            })
            .expect("wire-v12 reconstructs interval through the checked coordinate operation");
        assert_eq!(view.source_text(interval.provenance()), Some("interval(z)"));
    });

    let mut forged: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    forged["storage"]["clocks"][0]["kind"]["periodic"]["period"]["den"] = 0.into();
    assert!(matches!(
        serde_json::from_value::<Dae>(forged),
        Err(error) if error.to_string().contains("invalid exact DAE clock value")
    ));

    let mut forged: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    let interval = forged["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find(|node| node["coordinate"].get("clock_interval").is_some())
        .expect("fixture serializes one clock interval coordinate");
    interval["coordinate"]["clock_interval"] = 1.into();
    assert!(matches!(
        serde_json::from_value::<Dae>(forged),
        Err(error) if error.to_string().contains("unknown periodic clock identity 1")
    ));

    let mut forged: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    let interval = forged["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find(|node| node["coordinate"].get("clock_interval").is_some())
        .expect("fixture serializes one clock interval coordinate");
    interval["coordinate"]["clock_interval"] = 999.into();
    assert!(matches!(
        serde_json::from_value::<Dae>(forged),
        Err(error) if error.to_string().contains("unknown clock identity 999")
    ));
}

#[test]
fn clock_guarded_b1b_equation_requires_matching_clock_ownership() {
    let error = clock_guarded_b1b_fixture(false).unwrap_err();
    assert!(matches!(
        error,
        DaeConstructionError::MissingClockOwnership { .. }
    ));
}

#[test]
fn clock_guarded_b1b_equation_accepts_matching_clock_ownership() {
    let dae =
        clock_guarded_b1b_fixture(true).expect("matching owner proves the equation is clocked");
    dae.inspect(|view| {
        assert_eq!(view.clock_ownership_count(), 1);
        assert_eq!(view.discrete_real_equation_count(), 1);
    });
}

fn clock_guarded_b1b_fixture(with_ownership: bool) -> Result<Dae, DaeConstructionError> {
    let source = TestSource::new("discrete Real z; when sample(0, 1) then z = 1; end when;");
    let z_at = source.source("discrete Real z", 0);
    let sample_at = source.source("sample(0, 1)", 0);
    let assignment_at = source.source("z = 1", 0);
    Dae::construct(source.map, |dae| {
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
        if with_ownership {
            dae.clocks(|clocks| {
                clocks
                    .own_discrete_real(clock.into(), z, assignment_at)
                    .map(|_| ())
            })?;
        }
        let guard = dae.conditions(|conditions| conditions.reserve(sample_at))?;
        dae.conditions(|conditions| {
            conditions.define(guard, ConditionInput::Clock(clock.into()), sample_at)
        })?;
        let residual = dae.expressions(|expressions| {
            expressions
                .at(assignment_at)
                .coordinate(CoordinateInput::DiscreteReal(z))
        })?;
        dae.discrete(|discrete| {
            discrete.when_real_equation(guard, guard, assignment_at, |equation| {
                equation.residual(residual)
            })
        })?;
        Ok(())
    })
}

#[test]
fn condition_owner_clock_cache_preserves_selection_policy() {
    let source = TestSource::new("Clock c; Boolean b; c and b; c or b; not c; c or c;");
    let clock_at = source.source("Clock c", 0);
    let discrete_at = source.source("Boolean b", 0);
    let and_at = source.source("c and b", 0);
    let or_at = source.source("c or b", 0);
    let not_at = source.source("not c", 0);
    let shared_or_at = source.source("c or c", 0);
    Dae::construct(source.map, |dae| {
        let clock = dae.clocks(|clocks| {
            clocks.periodic(
                ClockLattice::new(ClockRational::ONE, ClockRational::ZERO).unwrap(),
                clock_at,
            )
        })?;
        let discrete = dae.expressions(|expressions| {
            expressions
                .at(discrete_at)
                .literal(DaeLiteral::Boolean(true))
        })?;
        dae.conditions(|conditions| {
            let clock_guard = conditions.reserve(clock_at)?;
            conditions.define(clock_guard, ConditionInput::Clock(clock.into()), clock_at)?;
            let discrete_guard = conditions.reserve(discrete_at)?;
            conditions.define(
                discrete_guard,
                ConditionInput::Discrete(discrete),
                discrete_at,
            )?;
            for (input, at, expected) in [
                (
                    ConditionInput::And(clock_guard, discrete_guard),
                    and_at,
                    Some(clock.index()),
                ),
                (ConditionInput::Or(clock_guard, discrete_guard), or_at, None),
                (ConditionInput::Not(clock_guard), not_at, None),
                (
                    ConditionInput::Or(clock_guard, clock_guard),
                    shared_or_at,
                    Some(clock.index()),
                ),
            ] {
                let condition = conditions.reserve(at)?;
                conditions.define(condition, input, at)?;
                assert_eq!(
                    crate::conditions::condition_owner_clock(
                        conditions.storage,
                        condition.index(),
                        at
                    )?,
                    expected
                );
            }
            Ok(())
        })
    })
    .expect("condition clock policy is cached during checked construction");
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
fn tagged_delay_kinds_round_trip_with_exact_provenance() {
    let dae = tagged_delay_fixture();
    dae.inspect(|view| {
        let parameter = view.delay(view.delay_id(0).unwrap()).unwrap();
        let DelayOperation::ParameterDelay { delay_time } = parameter.operation() else {
            panic!("first delay retains its parameter timing capability");
        };
        assert_eq!(
            view.source_text(parameter.provenance()),
            Some("delay(1, 2)")
        );
        assert_eq!(view.source_text(delay_time.provenance()), Some("2"));
        let bounded = view.delay(view.delay_id(1).unwrap()).unwrap();
        let DelayOperation::BoundedDelay { delay_max, .. } = bounded.operation() else {
            panic!("second delay retains its bounded timing capability");
        };
        assert_eq!(
            view.source_text(bounded.provenance()),
            Some("delay(1, 0.5, 3)")
        );
        assert_eq!(view.source_text(delay_max.provenance()), Some("3"));
        let coordinates = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .filter(|expression| {
                matches!(
                    expression.operation(),
                    ExpressionOperation::Coordinate(CoordinateView::Delay(_))
                )
            })
            .collect::<Vec<_>>();
        assert_eq!(coordinates.len(), 2);
        assert!(coordinates.iter().all(|coordinate| {
            matches!(
                coordinate.provenance().origin(),
                DaeProvenanceOrigin::Generated(DaeGeneration::DelayLowering)
            )
        }));
    });

    let encoded = serde_json::to_string(&dae).unwrap();
    let canonical: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    assert!(canonical["storage"]["delays"][0]["kind"]["parameter_delay"].is_object());
    assert!(canonical["storage"]["delays"][1]["kind"]["bounded_delay"].is_object());
    let decoded: Dae = serde_json::from_str(&encoded).expect("tagged delays replay");
    assert_eq!(serde_json::to_string(&decoded).unwrap(), encoded);
    let binary = bincode::serialize(&dae).unwrap();
    let decoded: Dae = bincode::deserialize(&binary).expect("binary tagged delays replay");
    assert_eq!(bincode::serialize(&decoded).unwrap(), binary);
}

#[test]
fn tagged_delay_wire_rejects_removed_and_malformed_states() {
    let canonical = serde_json::to_value(tagged_delay_fixture()).unwrap();

    let mut old = canonical.clone();
    let delay = old["storage"]["delays"][0].as_object_mut().unwrap();
    delay.remove("kind");
    delay.insert("delay_time".to_owned(), 1.into());
    delay.insert(
        "delay_time_evidence".to_owned(),
        serde_json::json!({"expression": 1, "value": 2.0}),
    );
    delay.insert("delay_max".to_owned(), serde_json::Value::Null);
    assert!(
        serde_json::from_value::<Dae>(old).is_err(),
        "the removed untagged Option-pair shape is not accepted"
    );

    let mut missing_maximum = canonical.clone();
    missing_maximum["storage"]["delays"][1]["kind"]["bounded_delay"]
        .as_object_mut()
        .unwrap()
        .remove("delay_max");
    assert!(serde_json::from_value::<Dae>(missing_maximum).is_err());

    let mut invalid_positive = canonical.clone();
    invalid_positive["storage"]["delays"][0]["kind"]["parameter_delay"]["delay_time"]["value"] =
        0.0.into();
    assert!(serde_json::from_value::<Dae>(invalid_positive).is_err());

    let mut unknown_expression = canonical.clone();
    unknown_expression["storage"]["delays"][1]["kind"]["bounded_delay"]["delay_max"]["expression"] =
        u32::MAX.into();
    assert!(serde_json::from_value::<Dae>(unknown_expression).is_err());

    let mut wrong_order = canonical.clone();
    let first_delay = wrong_order["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .filter_map(|node| node.get_mut("coordinate"))
        .find_map(|coordinate| coordinate.get_mut("delay"))
        .unwrap();
    *first_delay = 1.into();
    assert!(serde_json::from_value::<Dae>(wrong_order).is_err());

    let mut trailing = canonical;
    let extra = trailing["storage"]["delays"][0].clone();
    trailing["storage"]["delays"]
        .as_array_mut()
        .unwrap()
        .push(extra);
    assert!(
        serde_json::from_value::<Dae>(trailing).is_err(),
        "every delay operation requires its ordered coordinate occurrence"
    );
}

fn tagged_delay_fixture() -> Dae {
    let source = TestSource::new("delay(1, 2); delay(1, 0.5, 3)");
    let parameter_owner = source.source("delay(1, 2)", 0);
    let bounded_owner = source.source("delay(1, 0.5, 3)", 0);
    let first_source = source.source("1", 0);
    let parameter_time = source.source("2", 0);
    let second_source = source.source("1", 1);
    let bounded_time = source.source("0.5", 0);
    let maximum = source.source("3", 0);
    let parameter_coordinate =
        DaeProvenance::generated(DaeGeneration::DelayLowering, parameter_owner.span()).unwrap();
    let bounded_coordinate =
        DaeProvenance::generated(DaeGeneration::DelayLowering, bounded_owner.span()).unwrap();
    Dae::construct(source.map, |dae| {
        let values = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(first_source)
                    .literal(DaeLiteral::Real(1.0))?,
                expressions
                    .at(parameter_time)
                    .literal(DaeLiteral::Real(2.0))?,
                expressions
                    .at(second_source)
                    .literal(DaeLiteral::Real(1.0))?,
                expressions
                    .at(bounded_time)
                    .literal(DaeLiteral::Real(0.5))?,
                expressions.at(maximum).literal(DaeLiteral::Real(3.0))?,
            ))
        })?;
        let (delay_time, delay_max) = dae.temporal(|temporal| {
            Ok((
                temporal.positive_parameter(values.1, 2.0, parameter_time)?,
                temporal.positive_parameter(values.4, 3.0, maximum)?,
            ))
        })?;
        dae.expressions(|expressions| {
            expressions
                .at(parameter_coordinate)
                .delay(values.0, delay_time, parameter_owner)?;
            expressions.at(bounded_coordinate).bounded_delay(
                values.2,
                values.3,
                delay_max,
                bounded_owner,
            )?;
            Ok(())
        })
    })
    .expect("both checked delay kinds construct")
}

#[test]
fn wire_v12_round_trip_preserves_checked_quotients_and_their_provenance() {
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
fn wire_v12_round_trip_preserves_provenance_without_inline_source_copies() {
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
fn wire_decode_rejects_the_superseded_coordinate_tagged_schema_version() {
    /// The last wire version without the clock-interval coordinate variant.
    const SUPERSEDED_COORDINATE_VERSION: u16 = 11;

    let dae = exact_clock_fixture();
    let json = serde_json::to_string(&dae).expect("clock fixture serializes");
    assert!(
        json.contains("\"clock_interval\""),
        "the current wire carries the clock-interval coordinate"
    );

    for superseded in [10, SUPERSEDED_COORDINATE_VERSION] {
        let payload = json.replacen(
            &format!("\"schema_version\":{DAE_SCHEMA_VERSION}"),
            &format!("\"schema_version\":{superseded}"),
            1,
        );
        assert!(
            matches!(
                serde_json::from_str::<Dae>(&payload),
                Err(error) if error.to_string().contains(&format!(
                    "unsupported DAE schema version {superseded}; expected {DAE_SCHEMA_VERSION}"
                ))
            ),
            "superseded wire version {superseded} must be rejected, never read"
        );
    }

    // Ordinal-tagged encodings identify coordinate variants positionally, so the
    // superseded wire's `condition` coordinate now occupies the ordinal of the
    // inserted `clock_interval` variant. The leading version field is the only
    // thing separating those two readings, so decode rejects the superseded
    // number before any variant is decoded.
    let mut binary = bincode::serialize(&dae).expect("clock fixture serializes to a binary wire");
    let superseded = SUPERSEDED_COORDINATE_VERSION.to_le_bytes();
    binary[..superseded.len()].copy_from_slice(&superseded);
    assert!(matches!(
        bincode::deserialize::<Dae>(&binary),
        Err(error) if error.to_string().contains(&format!(
            "unsupported DAE schema version {SUPERSEDED_COORDINATE_VERSION}; \
             expected {DAE_SCHEMA_VERSION}"
        ))
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
