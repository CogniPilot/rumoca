//! Internal false activation has no changing edge; authored literals retain one.
use super::*;

#[test]
fn internal_false_condition_omits_only_its_redundant_memory_row() {
    let text = "sample(0, 1); initial(); true; false;";
    let source = TestSource::new(text);
    let sample_at = source.at(0, "sample(0, 1)".len());
    let initial_start = text.find("initial()").unwrap();
    let initial_at = source.at(initial_start, initial_start + "initial()".len());
    let true_start = text.find("true").unwrap();
    let true_at = source.at(true_start, true_start + "true".len());
    let false_start = text.find("false").unwrap();
    let false_at = source.at(false_start, false_start + "false".len());
    let internal_at =
        dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, sample_at.span())
            .unwrap();
    let mut omitted = Vec::new();
    let mut retained = Vec::new();
    let model = dae::Dae::construct(source.map, |model| {
        let literals = model.expressions(|expressions| {
            Ok([
                expressions
                    .at(true_at)
                    .literal(dae::DaeLiteral::Boolean(true))?,
                expressions
                    .at(false_at)
                    .literal(dae::DaeLiteral::Boolean(false))?,
            ])
        })?;
        model.conditions(|conditions| {
            let mut current = conditions.reserve(internal_at)?;
            conditions.define(current, dae::ConditionInput::Always, internal_at)?;
            omitted.push(current.index() as usize);
            for depth in 1..=3 {
                let next = conditions.reserve(internal_at)?;
                conditions.define(next, dae::ConditionInput::Not(current), internal_at)?;
                if depth % 2 == 1 {
                    omitted.push(next.index() as usize);
                } else {
                    retained.push(next.index() as usize);
                }
                current = next;
            }
            for (literal, at) in literals.into_iter().zip([true_at, false_at]) {
                let condition = conditions.reserve(at)?;
                conditions.define(condition, dae::ConditionInput::Discrete(literal), at)?;
                retained.push(condition.index() as usize);
            }
            let initial = conditions.reserve(initial_at)?;
            conditions.define(initial, dae::ConditionInput::Initial, initial_at)?;
            retained.push(initial.index() as usize);
            let vector = conditions.reserve(initial_at)?;
            conditions.define(
                vector,
                dae::ConditionInput::AnyRise(initial, current),
                initial_at,
            )?;
            retained.push(vector.index() as usize);
            Ok(())
        })
    })
    .expect("internal and authored conditions are checked DAE");
    let solve = lower_solve_problem(&model).expect("condition memory lowers");
    let targets = &solve.discrete.update_targets;
    for condition in omitted {
        let slot = solve.events.condition_memory_parameter_indices[condition];
        assert!(
            !targets.contains(&rumoca_ir_solve::scalar_slot_p(slot)),
            "internal false/Always {condition}"
        );
    }
    for condition in retained {
        let slot = solve.events.condition_memory_parameter_indices[condition];
        assert!(
            targets.contains(&rumoca_ir_solve::scalar_slot_p(slot)),
            "authored/initial/vector or non-false {condition}"
        );
    }
    assert_eq!(targets.len(), 5);
}
