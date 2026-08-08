//! History lanes: `pre`, `previous`, and `delay`.
//!
//! Each temporal operator owns storage distinct from the coordinate it reads, and
//! that storage carries the schedule which refreshes it — one fixed whole-equation
//! event pass for `pre`, the owning clock for `previous`, and a runtime history
//! buffer for `delay`.

use super::*;

#[test]
fn ordinary_pre_discrete_value_is_fixed_within_one_whole_event_pass() {
    let source = TestSource::new("discrete Integer count; count = pre(count);");
    let declaration = source.at(0, 22);
    let owner = source.at(24, 42);
    let model = dae::Dae::construct(source.map, |model| {
        let integer = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Integer),
                declaration,
            )
        })?;
        let count = model.variables(|variables| {
            variables.discrete_value(
                VarName::new("count"),
                integer,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let previous = model.expressions(|expressions| {
            expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::PreDiscreteValue(count))
        })?;
        model.b1c([count], |topology| {
            topology.owner(owner, [count], |owner_scope| {
                owner_scope.always(owner, [(previous, owner)])
            })?;
            Ok(())
        })?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert!(matches!(
        solve.layout.binding("count"),
        Some(ScalarSlot::P { index: 0, .. })
    ));
    assert_eq!(solve.solve_layout.compiled_parameter_len, 2);
    let [binding] = solve.solve_layout.pre_param_bindings.as_slice() else {
        panic!("one exact pre-history binding expected");
    };
    assert_eq!(binding.dest_p_index, 1);
    assert!(matches!(
        binding.source,
        rumoca_ir_solve::PreParamSource::P { index: 0 }
    ));
    assert!(binding.clock_schedule.is_none());
    assert_eq!(
        solve.discrete.pre_modes,
        [rumoca_ir_solve::DiscreteEventPreMode::Fixed]
    );
    assert!(
        solve.discrete.rhs.programs()[0]
            .iter()
            .any(|operation| matches!(operation, LinearOp::LoadP { index: 1, .. }))
    );
}

#[test]
fn previous_loads_history_owned_by_its_exact_clock_schedule() {
    let source = TestSource::new(
        "discrete Real x; Clock c=Clock(0.1); when c then x=previous(x)+1; end when;",
    );
    let declaration = source.at(0, 15);
    let clock_at = source.at(25, 35);
    let condition_at = source.at(42, 43);
    let owner = source.at(49, 64);
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10).unwrap();
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let variable = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        model.clocks(|clocks| clocks.own_discrete_real(clock.into(), variable, owner))?;
        let condition = model.conditions(|conditions| conditions.reserve(condition_at))?;
        model.conditions(|conditions| {
            conditions.define(
                condition,
                dae::ConditionInput::Clock(clock.into()),
                condition_at,
            )
        })?;
        let previous = model
            .temporal(|temporal| temporal.previous_discrete_real(clock.into(), variable, owner))?;
        let residual = model.expressions(|expressions| {
            let previous = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Previous(previous))?;
            let one = expressions.at(owner).literal(dae::DaeLiteral::Real(1.0))?;
            let value = expressions
                .at(owner)
                .binary(dae::BinaryOperator::Add, previous, one)?;
            let target = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, target, value)
        })?;
        model.discrete(|discrete| {
            discrete.when_real_equation(condition, condition, owner, |equation| {
                equation.residual(residual)
            })
        })?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    let [ordinary_pre, previous] = solve.solve_layout.pre_param_bindings.as_slice() else {
        panic!("ordinary pre and clock-owned previous history bindings expected");
    };
    assert!(ordinary_pre.clock_schedule.is_none());
    assert_eq!(
        previous
            .clock_schedule
            .as_ref()
            .expect("previous has its owning periodic schedule")
            .lattice(),
        lattice
    );
    assert!(
        solve.discrete.rhs.programs()[0]
            .iter()
            .any(|operation| matches!(operation, LinearOp::LoadP { index: 2, .. }))
    );
}

#[test]
fn delay_lowers_to_runtime_history_programs_and_a_typed_value_slot() {
    let source = TestSource::new("Real x; der(x) = delay(x, 0.5);");
    let declaration = source.at(0, 6);
    let owner = source.at(8, 31);
    let derivative_at = source.at(8, 14);
    let delay_at = source.at(17, 30);
    let source_at = source.at(23, 24);
    let timing_at = source.at(26, 29);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let (source, delay_time) = model.expressions(|expressions| {
            Ok((
                expressions
                    .at(source_at)
                    .coordinate(dae::CoordinateInput::State(state))?,
                expressions
                    .at(timing_at)
                    .literal(dae::DaeLiteral::Real(0.5))?,
            ))
        })?;
        let timing =
            model.temporal(|temporal| temporal.positive_parameter(delay_time, 0.5, timing_at))?;
        let delay = model
            .expressions(|expressions| expressions.at(delay_at).delay(source, timing, delay_at))?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(derivative_at)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            expressions.at(owner).binary(
                dae::BinaryOperator::Subtract,
                derivative,
                delay.expression(),
            )
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    model.inspect(|view| {
        let delay = view
            .delay(view.delay_id(0).expect("dense delay identity"))
            .expect("checked delay owner resolves");
        assert_eq!(view.source_text(delay.provenance()), Some("delay(x, 0.5)"));
        let dae::DelayOperation::ParameterDelay { delay_time } = delay.operation() else {
            panic!("fixed delay has parameter timing evidence");
        };
        assert_eq!(view.source_text(delay_time.provenance()), Some("0.5"));
    });

    let solve = lower_solve_problem(&model).unwrap();
    solve
        .validate()
        .expect("delay lowering produces a computable Solve problem");
    let delays = &solve.events.delays;
    assert_eq!(delays.value_parameter_indices.len(), 1);
    assert_eq!(delays.source_is_discrete, [false]);
    let delay_slot = delays.value_parameter_indices[0];
    assert!(matches!(
        delays.source_rhs.programs()[0][0],
        LinearOp::LoadY { index: 0, .. }
    ));
    assert!(matches!(
        delays.delay_time_rhs.programs()[0][0],
        LinearOp::Const { value: 0.5, .. }
    ));
    assert!(matches!(
        delays.delay_max_rhs.programs()[0][0],
        LinearOp::Const { value: 0.5, .. }
    ));
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.derivative_rhs.nodes.as_slice()
    else {
        panic!("one scalar derivative block expected");
    };
    assert!(rows.programs()[0].iter().any(
        |operation| matches!(operation, LinearOp::LoadP { index, .. } if *index == delay_slot)
    ));
}
