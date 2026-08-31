//! Clocked partitions: row ownership, periodic schedules, and clocked relations.
//!
//! Every row of a clocked partition carries the schedule that activates it, a
//! periodic interval lowers to its exact constant, and a relation of a clocked
//! partition leaves the continuous root set.

use super::*;

#[test]
fn clocked_discrete_definition_lowers_with_exact_row_owner() {
    let source = TestSource::new("Real d; Clock c=Clock(0.1); d=sample(time);");
    let declaration = source.at(0, 6);
    let clock_at = source.at(16, 26);
    let owner = source.at(28, 42);
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
                VarName::new("d"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        model.clocks(|clocks| clocks.own_discrete_real(clock.into(), variable, owner))?;
        let residual = model.expressions(|expressions| {
            let target = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            let time = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Time)?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, target, time)
        })?;
        model.discrete(|discrete| {
            discrete.real_equation(owner, |equation| equation.residual(residual))
        })?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert_eq!(solve.clocks().periodic_event_schedules.len(), 1);
    assert_eq!(
        solve.clocks().periodic_event_schedules[0].lattice(),
        lattice
    );
    assert_eq!(solve.discrete().clock_owners.len(), 1);
    let clock = solve.discrete().clock_owners[0].expect("row has a clock owner");
    assert_eq!(
        solve
            .clocks()
            .periodic_schedule(clock)
            .expect("typed clock owner resolves")
            .lattice(),
        lattice
    );
    assert!(
        solve.discrete().rhs.programs()[0]
            .iter()
            .any(|operation| matches!(operation, LinearOp::LoadTime { .. }))
    );
}

#[test]
fn periodic_clock_interval_lowers_to_an_exact_constant() {
    let source = TestSource::new("Real x; Clock c=Clock(0.1); x=interval();");
    let declaration = source.at(0, 6);
    let clock_at = source.at(16, 26);
    let owner = source.at(28, 41);
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
            variables.algebraic(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        let residual = model.expressions(|expressions| {
            let target = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(variable))?;
            let interval = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::ClockInterval(clock))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, target, interval)
        })?;
        model.continuous(|equations| {
            equations.equation(owner, |equation| equation.residual(residual))
        })?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous().residual.nodes.as_slice() else {
        panic!("one scalar residual block expected");
    };
    assert!(
        rows.programs()[0]
            .iter()
            .any(|operation| matches!(operation, LinearOp::Const { value, .. } if *value == 0.1))
    );
}

/// An always-active B.1c owner inside a clocked partition.
///
/// MLS §16.5 activates every equation of a clocked partition on its clock's ticks, so
/// `counter = previous(counter) + 1` needs no `when` — the target simply carries a clock
/// ownership. Two properties follow, and both were lost for `Integer`/`Boolean` targets:
/// the row must be compiled under its owning clock (or `previous(...)` cannot resolve),
/// and its relation must not become a continuous root, because MLS §16.8.1 raises no
/// state event for a clocked relation — the tick already is the event.
///
/// This is the shape of `Modelica.Clocked.RealSignals.TickBasedSources.Ramp.counter`.
fn clocked_tick_counter_model(lattice: rumoca_core::ClockLattice) -> dae::Dae {
    let source = TestSource::new(
        "discrete Integer counter; Clock c=Clock(0.1); counter=previous(counter)+1;",
    );
    let declaration = source.at(0, 24);
    let clock_at = source.at(34, 44);
    let relation_at = source.at(54, 71);
    let owner = source.at(46, 73);
    dae::Dae::construct(source.map, |model| {
        let integer = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Integer),
                declaration,
            )
        })?;
        let counter = model.variables(|variables| {
            variables.discrete_value(
                VarName::new("counter"),
                integer,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        model.clocks(|clocks| clocks.own_discrete_value(clock.into(), counter, owner))?;
        let previous = model
            .temporal(|temporal| temporal.previous_discrete_value(clock.into(), counter, owner))?;
        // The saturation test `previous(counter) < 5` that the source block writes inside
        // its `if`: a relation of the clocked partition, registered as a root by the
        // checked DAE exactly as a continuous-time relation would be.
        let guard = model.expressions(|expressions| {
            let previous = expressions
                .at(relation_at)
                .coordinate(dae::CoordinateInput::Previous(previous))?;
            let limit = expressions
                .at(relation_at)
                .literal(dae::DaeLiteral::Integer(5))?;
            expressions
                .at(relation_at)
                .binary(dae::BinaryOperator::Less, previous, limit)
        })?;
        let relation = model.conditions(|conditions| conditions.relation(guard, relation_at))?;
        let condition = model.conditions(|conditions| conditions.reserve(relation_at))?;
        model.conditions(|conditions| {
            conditions.define(
                condition,
                dae::ConditionInput::Relation(relation),
                relation_at,
            )
        })?;
        model.conditions(|conditions| conditions.root(relation, condition, relation_at))?;
        let value = model.expressions(|expressions| {
            let previous = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Previous(previous))?;
            let one = expressions.at(owner).literal(dae::DaeLiteral::Integer(1))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Add, previous, one)
        })?;
        model.b1c([counter], |topology| {
            topology.owner(owner, [counter], |owner_scope| {
                owner_scope.always(owner, [(value, owner)])
            })?;
            Ok(())
        })?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn clocked_unconditional_discrete_value_owner_reads_its_previous_history() {
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10).unwrap();
    let model = clocked_tick_counter_model(lattice);

    let solve = lower_solve_problem(&model).unwrap();
    reseal_solve_problem(&solve)
        .expect("the clocked B.1c owner satisfies the Solve shape contract");
    let schedule = solve
        .discrete()
        .clock_owners
        .iter()
        .zip(&solve.discrete().row_roles)
        .find(|(_, role)| **role == rumoca_ir_solve::DiscreteRowRole::Equation)
        .and_then(|(clock, _)| *clock)
        .expect("the always-active clocked owner keeps its periodic row schedule");
    assert_eq!(
        solve
            .clocks()
            .periodic_schedule(schedule)
            .expect("typed clock owner resolves")
            .lattice(),
        lattice
    );
    let equation = solve
        .discrete()
        .row_roles
        .iter()
        .position(|role| *role == rumoca_ir_solve::DiscreteRowRole::Equation)
        .expect("the clocked owner has one equation row");
    assert!(
        solve.discrete().rhs.programs()[equation]
            .iter()
            .all(|operation| !matches!(operation, LinearOp::Select { .. })),
        "typed clock activation makes the Always branch direct before scalar lowering"
    );
}

/// A B.1c branch activated by the same clock that owns its target is
/// unconditional within that clocked partition. Guarded-owner construction
/// must prove that fact before scalar lowering; otherwise the default empty
/// branch plan degenerates into a self-hold and silently drops function
/// Boolean outputs such as the RDD2 estimator status flags.
#[test]
fn clock_owned_b1c_branch_lowers_its_value_instead_of_a_self_hold() {
    let source = TestSource::new(
        "discrete Boolean initialized; Clock c=Clock(0.1); when c then initialized=true; end when;",
    );
    let declaration = source.at(0, 28);
    let clock_at = source.at(38, 48);
    let assignment = source.at(62, 78);
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10).unwrap();
    let model = dae::Dae::construct(source.map, |model| {
        let boolean = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Boolean),
                declaration,
            )
        })?;
        let initialized = model.variables(|variables| {
            variables.discrete_value(
                VarName::new("initialized"),
                boolean,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        model.clocks(|clocks| clocks.own_discrete_value(clock.into(), initialized, assignment))?;
        let activation = model.conditions(|conditions| conditions.reserve(clock_at))?;
        model.conditions(|conditions| {
            conditions.define(
                activation,
                dae::ConditionInput::Clock(clock.into()),
                clock_at,
            )
        })?;
        let value = model.expressions(|expressions| {
            expressions
                .at(assignment)
                .literal(dae::DaeLiteral::Boolean(true))
        })?;
        model.b1c([initialized], |topology| {
            topology.owner(assignment, [initialized], |owner| {
                owner.when(activation, activation, assignment, [(value, assignment)])
            })?;
            Ok(())
        })
    })
    .expect("clock-owned conditional B.1c owner is valid checked DAE");

    let solve = lower_solve_problem(&model).expect("clock-owned B.1c owner lowers");
    let target = solve
        .layout()
        .binding("initialized")
        .expect("the Boolean target has parameter storage");
    let [guarded] = solve.discrete().guarded_assignments.as_slice() else {
        panic!("one compact guarded owner expected");
    };
    assert_eq!(guarded.target_ranges()[0].base(), target);
    assert!(guarded.clock_owner().is_some());
    assert!(
        guarded
            .program()
            .iter()
            .any(|operation| matches!(operation, LinearOp::Const { value, .. } if *value == 1.0)),
        "the clock-unconditional branch writes its true value"
    );
    let ScalarSlot::P { index: target, .. } = target else {
        panic!("a discrete Boolean target occupies parameter storage");
    };
    assert!(
        guarded.program().iter().all(
            |operation| !matches!(operation, LinearOp::LoadP { index, .. } if *index == target)
        ),
        "the branch must not degenerate into a current-value self-hold"
    );
}

/// MLS §16.8.1 raises no state event for a relation of a clocked partition — the clock's
/// tick already is the event, and the relation's `previous(...)` operand only resolves
/// while that schedule is active. So the relation must leave the continuous root set and
/// its condition memory must be scheduled on the owning clock instead.
#[test]
fn clocked_relation_leaves_the_continuous_root_set() {
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10).unwrap();
    let model = clocked_tick_counter_model(lattice);

    let solve = lower_solve_problem(&model).unwrap();
    assert_eq!(
        solve.events().root_conditions.len(),
        0,
        "a clocked relation is evaluated on its tick, not by continuous root finding"
    );
    assert_eq!(solve.events().root_zero_domains.len(), 0);
    assert_eq!(solve.events().root_relation_memory_targets.len(), 0);
    let memory = solve
        .discrete()
        .row_roles
        .iter()
        .position(|role| *role == rumoca_ir_solve::DiscreteRowRole::ConditionMemory)
        .expect("the clocked relation still keeps a condition-memory row");
    assert!(
        solve.discrete().clock_owners[memory].is_some(),
        "the condition-memory row of a clocked relation runs on its clock's ticks"
    );
}

/// MLS §8.5 permits `initial()` as one element of a vector when-activation.
/// Combining it with `sample(start, interval)` produces one `AnyRise` DAG with
/// no unique target clock owner: initialization and the periodic schedule are
/// independent event sources. The clock leaf must therefore read the hidden
/// schedule-derived lane rather than requiring the target row to claim a clock.
#[test]
fn mixed_initial_and_clock_activation_needs_no_target_clock_owner() {
    let source =
        TestSource::new("discrete Real y; when {sample(0,0.1),initial()} then y=1; end when;");
    let declaration = source.at(0, 15);
    let clock_at = source.at(23, 36);
    let initial_at = source.at(38, 47);
    let owner = source.at(54, 57);
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
                VarName::new("y"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        let clock_condition = model.conditions(|conditions| conditions.reserve(clock_at))?;
        model.conditions(|conditions| {
            conditions.define(
                clock_condition,
                dae::ConditionInput::Clock(clock.into()),
                clock_at,
            )
        })?;
        let initial = model.conditions(|conditions| conditions.reserve(initial_at))?;
        model.conditions(|conditions| {
            conditions.define(initial, dae::ConditionInput::Initial, initial_at)
        })?;
        let trigger = model.conditions(|conditions| conditions.reserve(owner))?;
        model.conditions(|conditions| {
            conditions.define(
                trigger,
                dae::ConditionInput::AnyRise(clock_condition, initial),
                owner,
            )
        })?;
        let residual = model.expressions(|expressions| {
            let target = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            let one = expressions.at(owner).literal(dae::DaeLiteral::Real(1.0))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, target, one)
        })?;
        model.discrete(|discrete| {
            discrete.when_real_equation(trigger, trigger, owner, |equation| {
                equation.residual(residual)
            })
        })?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).expect("the mixed event DAG has a checked schedule");
    reseal_solve_problem(&solve).expect("the activation lane is in bounds");
    let equation = solve
        .discrete()
        .guarded_assignments
        .iter()
        .find(|program| program.role() == rumoca_ir_solve::DiscreteRowRole::Equation)
        .expect("one guarded event equation owner");
    assert_eq!(equation.clock_owner(), None);
    let activation = solve.clocks().activation_parameter_indices[0];
    let conditional = equation
        .program()
        .iter()
        .find_map(|operation| match operation {
            LinearOp::FunctionConditional { program, .. } => Some(program),
            _ => None,
        })
        .expect("the mixed activation retains one compact conditional owner");
    assert!(
        conditional
            .arms()
            .iter()
            .flat_map(|arm| arm.condition().iter())
            .any(|op| matches!(op, LinearOp::LoadP { index, .. } if *index == activation)),
        "the clock leaf reads its schedule-derived lane"
    );
    assert!(
        equation
            .program()
            .iter()
            .all(|operation| !matches!(operation, LinearOp::Select { .. })),
        "the correlated activation is not expanded into scalar selections"
    );
}

#[test]
fn unclocked_clock_leaf_alias_is_proven_safe_for_observation_refresh() {
    let source = TestSource::new("discrete Boolean fresh; Clock c=Clock(0.1); fresh=c;");
    let declaration = source.at(0, 22);
    let clock_at = source.at(32, 42);
    let owner = source.at(44, 52);
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10).unwrap();
    let model = dae::Dae::construct(source.map, |model| {
        let boolean = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Boolean),
                declaration,
            )
        })?;
        let fresh = model.variables(|variables| {
            variables.discrete_value(
                VarName::new("fresh"),
                boolean,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        let activation = model.conditions(|conditions| conditions.reserve(clock_at))?;
        model.conditions(|conditions| {
            conditions.define(
                activation,
                dae::ConditionInput::Clock(clock.into()),
                clock_at,
            )
        })?;
        let value = model.expressions(|expressions| {
            expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Condition(activation))
        })?;
        model.b1c([fresh], |topology| {
            topology.owner(owner, [fresh], |owner_scope| {
                owner_scope.always(owner, [(value, owner)])
            })?;
            Ok(())
        })?;
        Ok(())
    })
    .expect("the unclocked clock-leaf alias is valid checked DAE");

    let solve = lower_solve_problem(&model).expect("the alias lowers with a refresh proof");
    let activation = solve.clocks().activation_parameter_indices[0];
    let row = solve
        .discrete()
        .rhs
        .programs()
        .iter()
        .position(|program| {
            program
                .iter()
                .any(|op| matches!(op, LinearOp::LoadP { index, .. } if *index == activation))
        })
        .expect("the alias reads the compiler-owned activation lane");
    assert_eq!(solve.discrete().clock_owners[row], None);
    assert_eq!(
        solve.discrete().pre_modes[row],
        rumoca_ir_solve::DiscreteEventPreMode::FollowCurrent
    );
    assert!(solve.discrete().observation_refresh[row]);
    assert!(!solve.discrete().observation_refresh_reads_y);
}
