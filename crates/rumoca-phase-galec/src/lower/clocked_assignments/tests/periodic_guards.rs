use super::*;

#[test]
fn periodic_guard_totality_reuses_the_checked_boolean_activation() {
    let mut sources = SourceMap::new();
    let text = "when sample(0,1) then y=f(x); end when;";
    let source = sources.add("periodic-call.mo", text);
    let provenance = at(source, text, text);
    let model = dae::Dae::construct(sources, |model| {
        let clock = periodic_clock(model, provenance)?;
        let other = periodic_clock(model, provenance)?;
        model.conditions(|conditions| {
            let tick = conditions.reserve(provenance)?;
            conditions.define(tick, dae::ConditionInput::Clock(clock), provenance)?;
            let always = conditions.reserve(provenance)?;
            conditions.define(always, dae::ConditionInput::Always, provenance)?;
            let never = conditions.reserve(provenance)?;
            conditions.define(never, dae::ConditionInput::Not(always), provenance)?;
            let wrapper = conditions.reserve(provenance)?;
            conditions.define(wrapper, dae::ConditionInput::Or(tick, never), provenance)?;
            let initial = conditions.reserve(provenance)?;
            conditions.define(initial, dae::ConditionInput::Initial, provenance)?;
            let narrow = conditions.reserve(provenance)?;
            conditions.define(narrow, dae::ConditionInput::And(tick, initial), provenance)?;
            let foreign = conditions.reserve(provenance)?;
            conditions.define(foreign, dae::ConditionInput::Clock(other), provenance)?;
            let mixed = conditions.reserve(provenance)?;
            conditions.define(mixed, dae::ConditionInput::Or(tick, foreign), provenance)
        })
    })
    .unwrap();
    model.inspect(|view| {
        let clock = view.clock_id(0).unwrap();
        let conditions = view.conditions().map(|(id, _)| id).collect::<Vec<_>>();
        assert!(
            view.condition(conditions[3])
                .unwrap()
                .is_true_on_clock_tick(clock)
        );
        assert!(view.condition(conditions[3]).unwrap().requires_clock(clock));
        assert!(
            !view
                .condition(conditions[5])
                .unwrap()
                .is_true_on_clock_tick(clock)
        );
        assert!(view.condition(conditions[5]).unwrap().requires_clock(clock));
        assert!(!view.condition(conditions[7]).unwrap().requires_clock(clock));
        assert!(
            !view
                .condition(conditions[6])
                .unwrap()
                .is_true_on_clock_tick(clock)
        );
    });
}
