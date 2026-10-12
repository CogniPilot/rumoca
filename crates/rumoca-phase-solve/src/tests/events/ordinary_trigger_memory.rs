//! Boolean task schedules preserve ordinary when edges, unlike MLS16 owners.

use super::*;

fn periodic_trigger_model(owned: bool) -> (dae::Dae, usize) {
    let text = if owned {
        "discrete Integer count; Clock c=Clock(0.1); when c then count=1; end when;"
    } else {
        "discrete Integer count; when sample(0,0.1) then count=1; end when;"
    };
    let source = TestSource::new(text);
    let at = source.at(0, text.len());
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10).unwrap();
    let mut trigger_index = None;
    let model = dae::Dae::construct(source.map, |model| {
        let integer = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at))?;
        let count = model.variables(|variables| {
            variables.discrete_value(
                VarName::new("count"),
                integer,
                at,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, at))?;
        if owned {
            model.clocks(|clocks| clocks.own_discrete_value(clock.into(), count, at))?;
        }
        let trigger = model.conditions(|conditions| {
            let trigger = conditions.reserve(at)?;
            conditions.define(trigger, dae::ConditionInput::Clock(clock.into()), at)?;
            if owned {
                return Ok(trigger);
            }
            // The existing Boolean task projection: a tick joined with the
            // internal false activation has no synchronous variable owner.
            let always = conditions.reserve(at)?;
            conditions.define(always, dae::ConditionInput::Always, at)?;
            let never = conditions.reserve(at)?;
            conditions.define(never, dae::ConditionInput::Not(always), at)?;
            let ordinary = conditions.reserve(at)?;
            conditions.define(ordinary, dae::ConditionInput::Or(trigger, never), at)?;
            Ok(ordinary)
        })?;
        trigger_index = Some(trigger.index() as usize);
        let value = model
            .expressions(|expressions| expressions.at(at).literal(dae::DaeLiteral::Integer(1)))?;
        model.b1c([count], |topology| {
            topology.owner(at, [count], |owner| {
                owner.when(trigger, trigger, at, [(value, at)])
            })?;
            Ok(())
        })
    })
    .expect("the periodic branch and optional variable clock ownership are checked");
    (model, trigger_index.unwrap())
}

fn assert_trigger_memory(owned: bool) {
    let (model, trigger) = periodic_trigger_model(owned);
    model.inspect(|view| assert_eq!(view.clock_ownership_count(), usize::from(owned)));
    let solve = lower_solve_problem(&model).expect("the scheduled guarded owner lowers");
    let [guarded] = solve.discrete.guarded_assignments.as_slice() else {
        panic!("one compact guarded owner");
    };
    assert!(
        guarded.clock_owner().is_some(),
        "both use the periodic task"
    );
    let memory = solve.events.condition_memory_parameter_indices[trigger];
    let target = rumoca_ir_solve::scalar_slot_p(memory);
    let buffer = solve
        .discrete
        .update_targets
        .iter()
        .position(|slot| *slot == target);
    assert_eq!(
        buffer.is_some(),
        !owned,
        "only an ordinary branch needs this exact edge buffer"
    );
    if let Some(row) = buffer {
        assert_eq!(
            solve.discrete.row_roles[row],
            rumoca_ir_solve::DiscreteRowRole::PulseConditionMemory
        );
    }
}

#[test]
fn ordinary_periodic_trigger_keeps_an_ordinary_scheduled_branch_edge() {
    assert_trigger_memory(false);
}

#[test]
fn ordinary_periodic_trigger_does_not_add_an_mls16_partition_edge() {
    assert_trigger_memory(true);
}
