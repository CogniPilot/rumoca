//! Current algebraic definitions participate in the checked same-tick exchange.

use rumoca::Compiler;
use rumoca_sim::{SimExecutionPolicy, SimOptions, SimulationSession};

fn check_trace(source: &str, expected: impl Fn(i32, &rumoca_sim::SessionState)) {
    let compiled = Compiler::new()
        .model("CurrentAlias")
        .compile_str(source, "current_alias.mo")
        .unwrap_or_else(|error| panic!("alias source compiles: {error}"));
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        check_policy(&compiled, policy, &expected);
    }
}

fn check_policy(
    compiled: &rumoca::CompilationResult,
    policy: SimExecutionPolicy,
    expected: &impl Fn(i32, &rumoca_sim::SessionState),
) {
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
        expected(sample, &state);
    }
}

const PREFIX: &str = r#"
model CurrentAlias
  discrete output Integer count(start=0, fixed=true);
  discrete output Real current(start=0, fixed=true);
  discrete output Real held(start=0, fixed=true);
  Real alias;
equation
"#;

fn source(alias: &str, extra: &str) -> String {
    format!(
        "{PREFIX}\n  alias = {alias};\n  when sample(0.1,0.1) then\n    count=pre(count)+1;\n    current=alias;\n    held=pre(count);\n  end when;\n{extra}\nend CurrentAlias;"
    )
}

fn current_and_pre(sample: i32, values: &rumoca_sim::SessionState) {
    let count = f64::from(sample / 2);
    assert_eq!(values.values["count"], count, "sample {sample}");
    assert_eq!(
        values.values["current"],
        2.0 * count,
        "current alias at sample {sample}"
    );
    assert_eq!(
        values.values["held"],
        (count - 1.0).max(0.0),
        "pre at sample {sample}"
    );
}

#[test]
fn when_current_alias_observes_the_same_tick_and_pre_stays_held() {
    check_trace(&source("2.0*count", ""), current_and_pre);
}

#[test]
fn when_current_alias_keeps_inactive_invalid_arithmetic_lazy() {
    check_trace(
        &source("if count>100 then sqrt(-count) else 2.0*count", ""),
        current_and_pre,
    );
}

#[test]
fn when_current_alias_activation_retains_its_entry_lane() {
    let source = r#"
model CurrentAlias
  discrete output Integer count(start=0,fixed=true);
  discrete output Real current(start=0,fixed=true), held(start=0,fixed=true);
  discrete output Real edge(start=0,fixed=true);
  Real alias;
equation
  alias=2.0*count;
  when sample(0.1,0.1) then
    count=pre(count)+1;
    current=alias;
    held=pre(count);
  end when;
  when alias>=4.0 then edge=alias; end when;
end CurrentAlias;
"#;
    check_trace(source, |sample, values| {
        current_and_pre(sample, values);
        assert_eq!(
            values.values["edge"],
            if sample >= 4 { 4.0 } else { 0.0 },
            "edge at sample {sample}"
        );
    });
}
