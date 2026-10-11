//! SOLVE-C58 orders current-value reads across B.1b/B.1c equation owners.

use rumoca::Compiler;
use rumoca_sim::{SimExecutionPolicy, SimOptions, SimulationSession, lower_dae_for_simulation};

const MIXED: &str = r#"
model MixedCurrent
  discrete output Integer count(start=0, fixed=true);
  discrete output Real current(start=0, fixed=true);
  discrete output Real held(start=0, fixed=true);
equation
  when sample(0.1, 0.1) then
    count = pre(count) + 1;
    current = count;
    held = pre(count);
  end when;
end MixedCurrent;
"#;

fn compile(source: &str, model: &str) -> rumoca::CompilationResult {
    Compiler::new()
        .model(model)
        .compile_str(source, "mixed_when.mo")
        .unwrap_or_else(|error| panic!("mixed when equations compile: {error}"))
}

#[test]
fn mixed_when_current_reads_follow_the_integer_producer_and_pre_stays_held() {
    let compiled = compile(MIXED, "MixedCurrent");
    let model = lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let (count, current) = compiled.dae.inspect(|view| {
        let id = |name| {
            view.variables()
                .find(|(_, variable)| variable.name().as_str() == name)
                .unwrap()
                .0
                .index() as usize
        };
        (id("count"), id("current"))
    });
    let layout = &model.problem.solve_layout;
    let count = layout.variable_scalar_slot(count, 0).unwrap();
    let current = layout.variable_scalar_slot(current, 0).unwrap();
    let order = model
        .problem
        .discrete
        .guarded_assignments
        .iter()
        .flat_map(|program| program.target_ranges().iter().map(|range| range.base()))
        .collect::<Vec<_>>();
    eprintln!("issued guarded target order: {order:?}; count={count:?}, current={current:?}");
    assert!(
        order.iter().position(|slot| *slot == count).unwrap()
            < order.iter().position(|slot| *slot == current).unwrap(),
        "the checked Integer producer must precede its Real current-value consumer"
    );
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        check_current_trace(&compiled, policy);
    }
}

fn check_current_trace(compiled: &rumoca::CompilationResult, policy: SimExecutionPolicy) {
    let mut session = SimulationSession::new(
        &compiled.dae,
        SimOptions {
            t_end: 0.5,
            execution_policy: policy,
            ..SimOptions::default()
        },
    )
    .unwrap();
    for sample in 0..=10 {
        let time = f64::from(sample) / 20.0;
        if sample != 0 {
            session.advance_to(time).unwrap();
        }
        let state = session.state().unwrap();
        assert_eq!(state.time, time);
        let count = f64::from(sample / 2);
        assert_eq!(state.values["count"], count, "{policy:?} at {time}");
        assert_eq!(state.values["current"], count, "{policy:?} at {time}");
        assert_eq!(
            state.values["held"],
            (count - 1.0).max(0.0),
            "{policy:?}: pre retains the entry value at {time}"
        );
    }
}

#[test]
fn mixed_when_activation_edges_retain_entry_reads_and_fire_once() {
    let source = r#"
model MixedEdge
  discrete output Integer count(start=0, fixed=true);
  discrete output Real edge(start=0, fixed=true);
equation
  when sample(0.1, 0.1) then
    count = pre(count) + 1;
  end when;
  when count >= 2 then
    edge = count;
  end when;
end MixedEdge;
"#;
    let compiled = compile(source, "MixedEdge");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        check_edge_trace(&compiled, policy);
    }
}

fn check_edge_trace(compiled: &rumoca::CompilationResult, policy: SimExecutionPolicy) {
    let mut session = SimulationSession::new(
        &compiled.dae,
        SimOptions {
            t_end: 0.5,
            execution_policy: policy,
            ..SimOptions::default()
        },
    )
    .unwrap();
    for sample in 0..=10 {
        let time = f64::from(sample) / 20.0;
        if sample != 0 {
            session.advance_to(time).unwrap();
        }
        let state = session.state().unwrap();
        assert_eq!(state.time, time);
        assert_eq!(state.values["count"], f64::from(sample / 2));
        assert_eq!(state.values["edge"], if sample >= 4 { 2.0 } else { 0.0 });
    }
}
