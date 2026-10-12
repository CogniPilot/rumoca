//! Public original Bug reproductions, beyond compiler admission.

mod guarded_time_deadlines;
mod integer_call_comparison;
mod integer_quotient_comparison;
mod main_dae_integer;
mod scalar_boolean_values;
mod scalar_modulo_integral;
mod static_integer_capture;

use rumoca::{CompilationResult, Compiler};
use rumoca_sim::{SimOptions, SimResult, SimSolverMode, SimulationSession, simulate_dae};
use rumoca_solver::{SimExecutionEngine, SimExecutionPolicy};

const GUARDED: &str =
    include_str!("../../../rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_403.mo");
const INPUT: &str =
    include_str!("../../../rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_405.mo");
const INTEGER: &str =
    include_str!("../../../rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_411.mo");
const ALGEBRAIC: &str =
    include_str!("../../../rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_418.mo");
const NEIGHBOR: &str =
    include_str!("../../../rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_426.mo");
const STATIC_PIE_ARRAY: &str =
    include_str!("../../../rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_422.mo");
const ITERATOR_MOD: &str =
    include_str!("../../../rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_423.mo");

fn compile(source: &str, model: &str) -> CompilationResult {
    Compiler::new()
        .model(model)
        .compile_str(source, "ReportedBug.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"))
}

fn options(execution_policy: SimExecutionPolicy) -> SimOptions {
    SimOptions {
        execution_policy,
        solver_mode: SimSolverMode::RkLike,
        t_end: 1.0,
        dt: Some(0.1),
        rtol: 1e-9,
        atol: 1e-9,
        ..SimOptions::default()
    }
}

fn series<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let column = result
        .names
        .iter()
        .position(|value| value == name)
        .unwrap_or_else(|| panic!("trace contains {name}: {:?}", result.names));
    &result.data[column]
}

fn last(result: &SimResult, name: &str) -> f64 {
    *series(result, name).last().expect("nonempty trace")
}

#[test]
fn issue_403_original_scalar_boolean_guard_short_circuits() {
    let compiled = compile(GUARDED, "GuardedOperands");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        for (index, expected) in [(3.0, 1.0), (4.0, 0.0)] {
            let mut opts = options(policy);
            opts.initial_inputs = vec![("k".into(), index)];
            let result = simulate_dae(&compiled.dae, &opts)
                .unwrap_or_else(|error| panic!("original guard {policy:?}, k={index}: {error:?}"));
            assert_eq!(last(&result, "y"), expected, "{policy:?}, k={index}");
        }
    }
}

#[test]
fn issue_403_original_guard_executes_natively_without_fallback() {
    let compiled = compile(GUARDED, "GuardedOperands");
    let mut opts = options(SimExecutionPolicy::Auto);
    opts.initial_inputs = vec![("k".into(), 4.0)];
    let lowered = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &opts).unwrap();
    assert_eq!(lowered.pure_calls.owners().len(), 1);
    let site = lowered.pure_calls.owners()[0].call_site();
    let calls = rumoca_exec_cranelift::compile_pure_call_table(&lowered.pure_calls)
        .expect("the actual guarded function has a native helper");
    // The unchanged model has no continuous states. Execute its issued native
    // function directly rather than claiming a whole-session native engine.
    let (mut input_cells, mut output_cells) = (Vec::new(), Vec::new());
    let mut call = |index, limit| {
        let mut output = [-123.0];
        let result = calls.call_scalar_payload(
            rumoca_eval_solve::PureCallInvocation::Primal(&site),
            &[1.0, 2.0, 9.0, index, limit],
            &mut output,
            &mut input_cells,
            &mut output_cells,
        );
        (result, output)
    };
    for (index, expected) in [(3.0, 1.0), (4.0, 0.0), (2.0, 0.0)] {
        let (result, output) = call(index, 3.0);
        result.expect("execute the original native guarded function");
        assert_eq!(output, [expected], "k={index}");
    }
    let (result, output) = call(4.0, 4.0);
    assert_index_fault(result.expect_err("the selected invalid access still faults"));
    assert_eq!(output, [-123.0], "a native fault publishes no value");
    let (result, output) = call(3.0, 3.0);
    result.expect("a valid native invocation succeeds after the fault");
    assert_eq!(output, [1.0]);
}

fn assert_index_fault(error: impl std::fmt::Debug) {
    let diagnostic = format!("{error:?}").to_lowercase();
    assert!(
        diagnostic.contains("index") || diagnostic.contains("bounds"),
        "{diagnostic}"
    );
}

#[test]
fn issue_403_explicit_if_control_guarantees_inactive_operand_protection() {
    let safe = GUARDED.replace(
        "if k <= n and a[k] > 5 then 1 else 0",
        "if k <= n then (if a[k] > 5 then 1 else 0) else 0",
    );
    let compiled = compile(&safe, "GuardedOperands");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let mut opts = options(policy);
        opts.initial_inputs = vec![("k".into(), 4.0)];
        let result = simulate_dae(&compiled.dae, &opts)
            .unwrap_or_else(|error| panic!("explicit if control {policy:?}: {error:?}"));
        assert_eq!(last(&result, "y"), 0.0, "{policy:?}");
    }
}

#[test]
fn issue_403_active_invalid_index_keeps_its_fault() {
    let active = GUARDED.replace("k, 3);", "k, 4);");
    let compiled = compile(&active, "GuardedOperands");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let mut opts = options(policy);
        opts.initial_inputs = vec![("k".into(), 4.0)];
        let error = simulate_dae(&compiled.dae, &opts).expect_err("selected invalid access faults");
        assert_index_fault(error);
    }
}

#[test]
fn issue_405_original_input_schedule_switches_at_each_write() {
    check_input_schedule(INPUT, false);
}

#[test]
fn issue_405_reported_counter_changes_only_on_input_rises() {
    check_input_schedule(&input_counter_source(), true);
}

#[test]
fn issue_405_announced_time_schedule_switches_at_each_write() {
    assert_eq!(INPUT.matches("u > 3").count(), 1);
    check_input_schedule(&INPUT.replace("u > 3", "u > time"), false);
    let counter = input_counter_source();
    assert_eq!(counter.matches("u > 3").count(), 2);
    check_input_schedule(&counter.replace("u > 3", "u > time"), true);
}

fn input_counter_source() -> String {
    assert_eq!(INPUT.matches("  output Real z;").count(), 1);
    assert_eq!(INPUT.matches("  z = if u > 3 then 1 else 0;").count(), 1);
    INPUT
        .replace(
            "  output Real z;",
            "  output Real z;\n  discrete Integer d(start=0, fixed=true);",
        )
        .replace(
            "  z = if u > 3 then 1 else 0;",
            "  z = if u > 3 then 1 else 0;\n  when u > 3 then\n    d = pre(d) + 1;\n  end when;",
        )
}

fn check_input_schedule(source: &str, observe_counter: bool) {
    let compiled = compile(source, "TopLevel");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let mut opts = options(policy);
        opts.t_end = 2.0;
        opts.initial_inputs = vec![("u".into(), 0.0)];
        let mut session = SimulationSession::new(&compiled.dae, opts).unwrap();
        check_input_schedule_session(&mut session, observe_counter);
    }
}

fn check_input_schedule_session(session: &mut SimulationSession, observe_counter: bool) {
    let (mut expected_z, mut expected_d) = (0.0, 0.0);
    check_input_values(session, expected_z, expected_d, observe_counter);
    for step in 0..20 {
        if matches!(step, 3 | 8 | 13) {
            let input = if step == 8 { 0.0 } else { 7.0 };
            expected_z = if step == 8 { 0.0 } else { 1.0 };
            if step != 8 {
                expected_d += 1.0;
            }
            session.set_input("u", input).unwrap();
            check_input_values(session, expected_z, expected_d, observe_counter);
            // A bit-identical write must not cause another when/pre increment.
            session.set_input("u", input).unwrap();
            check_input_values(session, expected_z, expected_d, observe_counter);
        }
        session.advance_to((step + 1) as f64 * 0.1).unwrap();
        check_input_values(session, expected_z, expected_d, observe_counter);
    }
}

fn check_input_values(
    session: &SimulationSession,
    expected_z: f64,
    expected_d: f64,
    observe_counter: bool,
) {
    let state = session.state().unwrap();
    assert_eq!(state.values["z"], expected_z, "time {}", state.time);
    if observe_counter {
        assert_eq!(state.values["d"], expected_d, "time {}", state.time);
    }
}

#[test]
fn issue_411_original_main_dae_integer_mod_retains_wide_literal() {
    let compiled = compile(INTEGER, "IntegerPrecision");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        for (input, expected) in [(0.0, 5.0), (1.0, 6.0), (7.0, 5.0)] {
            let mut opts = options(policy);
            opts.initial_inputs = vec![("n".into(), input)];
            let result = simulate_dae(&compiled.dae, &opts).unwrap_or_else(|error| {
                panic!("Integer arithmetic {policy:?}, n={input}: {error:?}")
            });
            assert_eq!(last(&result, "y"), expected, "{policy:?}, n={input}");
        }
    }
}

#[test]
fn issue_411_main_dae_integer_div_retains_wide_literal() {
    let source = INTEGER.replace("mod(", "div(");
    let compiled = compile(&source, "IntegerPrecision");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        // n=2 crosses a quotient boundary that rounding the wide literal to
        // Real before addition would miss; n=0 retains the original control.
        for input in [0_i64, 2] {
            let mut opts = options(policy);
            opts.initial_inputs = vec![("n".into(), input as f64)];
            let result = simulate_dae(&compiled.dae, &opts).unwrap_or_else(|error| {
                panic!("Integer division {policy:?}, n={input}: {error:?}")
            });
            assert_eq!(
                last(&result, "y"),
                ((9_007_199_254_740_993_i64 + input) / 7) as f64,
                "{policy:?}, n={input}"
            );
        }
    }
}

#[test]
fn issue_418_original_explicit_algebraic_model_renders_public_jax_target() {
    let compiled = compile(ALGEBRAIC, "B");
    let files = rumoca::render_target_files(&compiled, "B", "jax-ode", None)
        .expect("exact algebraic assignments render through the public target gate");
    assert!(
        files
            .iter()
            .any(|file| file.path.ends_with(".py") && !file.content.is_empty())
    );
}

#[test]
fn issue_426_original_inactive_neighbor_simulates_its_analytic_solution() {
    let compiled = compile(NEIGHBOR, "G1");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let result = simulate_dae(&compiled.dae, &options(policy))
            .unwrap_or_else(|error| panic!("inactive neighbor {policy:?}: {error:?}"));
        assert_eq!(result.times.first().copied(), Some(0.0), "{policy:?}");
        assert_eq!(result.times.last().copied(), Some(1.0), "{policy:?}");
        assert_eq!(
            result.times.len(),
            11,
            "{policy:?}: complete 0.1 output grid"
        );
        for (sample, &time) in result.times.iter().enumerate() {
            let expected = sample as f64 / 10.0;
            assert!(
                (time - expected).abs() <= f64::EPSILON,
                "{policy:?}: sample {sample} time {time}, expected {expected}"
            );
        }
        for (channel, name) in ["u[1]", "u[2]", "u[3]", "u[4]"].into_iter().enumerate() {
            assert_eq!(
                result.names.iter().filter(|value| *value == name).count(),
                1
            );
            let values = series(&result, name);
            assert_eq!(values.len(), result.times.len(), "{policy:?}: {name}");
            for (&time, &actual) in result.times.iter().zip(values) {
                // The unchanged cascade has u_i(t) = exp(-t) sum(t^k/k!), k=0..i-1.
                let polynomials = [
                    1.0,
                    1.0 + time,
                    1.0 + time + time * time / 2.0,
                    1.0 + time + time * time / 2.0 + time * time * time / 6.0,
                ];
                let expected = (-time).exp() * polynomials[channel];
                assert!(
                    (actual - expected).abs() < 1e-7,
                    "{policy:?}: {name} at {time}: {actual}, expected {expected}"
                );
            }
        }
    }
}

#[test]
fn issue_423_original_iterator_mod_simulates_its_analytic_solution() {
    let compiled = compile(ITERATOR_MOD, "C13");
    // MLS §3.7.2: mod(k - 1, 3) gives {0, 1, 2}; y' = -2*y, y(0) = 1.
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let opts = options(policy);
        let mut session = SimulationSession::new(&compiled.dae, opts.clone())
            .unwrap_or_else(|error| panic!("original C13 initializes under {policy:?}: {error:?}"));
        let initial = session.state().expect("settled initialization state");
        assert_eq!(initial.time, 0.0);
        assert_eq!(initial.values["y"], 1.0);
        for time in [0.02, 0.04, 0.1, 1.0] {
            session.advance_to(time).unwrap_or_else(|error| {
                panic!("original C13 advances under {policy:?}: {error:?}")
            });
            let state = session.state().expect("actual simulation state");
            assert_eq!(state.time, time);
            let actual = state.values["y"];
            let expected = (-2.0 * time).exp();
            let bound = 20.0 * (opts.atol + opts.rtol * expected.abs());
            assert!(
                actual.is_finite() && (actual - expected).abs() <= bound,
                "C13/{policy:?}: y({time})={actual}, expected {expected}, bound {bound}"
            );
        }
        let receipt = session.execution_receipt();
        eprintln!("original C13/{policy:?} after full simulation: {receipt:?}");
        if policy == SimExecutionPolicy::Interpreter {
            assert_eq!(receipt.engine, SimExecutionEngine::Interpreter);
            assert_eq!(receipt.refusal, None);
            assert!(receipt.declined.is_empty());
        } else if receipt.is_compiled() {
            assert_eq!(receipt.refusal, None);
        } else {
            assert!(receipt.refusal.is_some(), "Auto must expose its refusal");
        }
    }
}

#[test]
fn issue_422_original_array_function_runs_without_native_fallback() {
    let compiled = compile(STATIC_PIE_ARRAY, "U4");
    let opts = options(SimExecutionPolicy::Auto);
    let lowered = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &opts)
        .expect("original U4 lowers through the simulation owner");
    eprintln!(
        "original U4 checked pure-call owners: {}",
        lowered.pure_calls.owners().len()
    );
    // Session admission alone does not expose a failed pure-call-table compile.
    let _native_calls = rumoca_exec_cranelift::compile_pure_call_table(&lowered.pure_calls)
        .expect("every retained U4 pure-call owner compiles without fallback");
    let mut session = SimulationSession::new(&compiled.dae, opts.clone())
        .expect("original U4 initializes through the canonical native backend");
    for sample in 0..=10 {
        let time = f64::from(sample) / 10.0;
        if sample != 0 {
            session.advance_to(time).expect("original U4 advances");
        }
        let state = session.state().expect("actual U4 state");
        assert_eq!(state.time, time);
        let actual = state.values["y"];
        let expected = (-time).exp();
        let bound = 20.0 * (opts.atol + opts.rtol * expected.abs());
        assert!(
            actual.is_finite() && (actual - expected).abs() <= bound,
            "U4: y({time})={actual}, expected {expected}, bound {bound}"
        );
        let receipt = session.execution_receipt();
        assert_eq!(receipt.engine, SimExecutionEngine::Cranelift, "{receipt:?}");
        assert_eq!(receipt.refusal, None, "{receipt:?}");
        assert!(receipt.declined.is_empty(), "{receipt:?}");
        eprintln!("original U4/{sample} time={time} y={actual}: {receipt:?}");
    }
}

pub(super) fn issue_425_original_38_source() -> String {
    use sha1::{Digest, Sha1};

    const ORIGINAL: &str = include_str!(
        "../../../rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_425.mo"
    );
    assert_eq!(
        format!("{:x}", Sha1::digest(ORIGINAL.as_bytes())),
        "cafb32e2ce5be3e9ac8e4e28a585fd95ed8d0035",
        "preserve the original reported 30x30 fixture"
    );
    assert_eq!(ORIGINAL.matches("parameter Integer nx = 30;").count(), 1);
    assert_eq!(ORIGINAL.matches("parameter Integer ny = 30;").count(), 1);
    let source = ORIGINAL
        .replace("parameter Integer nx = 30;", "parameter Integer nx = 38;")
        .replace("parameter Integer ny = 30;", "parameter Integer ny = 38;");
    assert_eq!(
        format!("{:x}", Sha1::digest(source.as_bytes())),
        "ff36add31d0cbc593240535501a55a7cecfc8add",
        "only the two grid extents change; model name and equations stay authored"
    );
    source
}

#[test]
fn issue_425_original_38_preserves_quiescent_states_and_native_receipts() {
    let compiled = compile(&issue_425_original_38_source(), "E30x30");
    let expected = compiled.dae.inspect(|view| {
        view.variables()
            .filter(|(_, variable)| variable.role() == rumoca_ir_dae::VariableRole::State)
            .flat_map(|(_, variable)| {
                assert_eq!(variable.scalar_count(), 38 * 38);
                let value = match variable.name().as_str() {
                    "r" => 1.0,
                    "mu" | "mv" => 0.0,
                    "E" => 2.5,
                    name => panic!("unexpected Euler state {name}"),
                };
                variable.scalar_names().map(move |name| (name, value))
            })
            .collect::<Vec<_>>()
    });
    assert_eq!(expected.len(), 4 * 38 * 38);
    let opts = SimOptions {
        t_end: 0.1,
        dt: Some(0.01),
        solver_mode: SimSolverMode::RkLike,
        execution_policy: SimExecutionPolicy::Auto,
        ..SimOptions::default()
    };
    {
        let lowered = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &opts)
            .expect("original 38x38 Euler lowers through the simulation owner");
        let _native_calls = rumoca_exec_cranelift::compile_pure_call_table(&lowered.pure_calls)
            .expect("retained checked Euler helpers compile without hidden fallback");
    }
    let mut session =
        SimulationSession::new(&compiled.dae, opts).expect("original 38x38 Euler initializes");
    for sample in 0..=10 {
        let time = f64::from(sample) / 100.0;
        if sample != 0 {
            session.advance_to(time).expect("original Euler advances");
        }
        let state = session.state().expect("actual original Euler state");
        assert_eq!(state.time, time);
        for (name, value) in &expected {
            let actual = state.values[name];
            assert!(actual.is_finite(), "{name} at {time}: {actual}");
            assert_eq!(actual, *value, "quiescent {name} at {time}");
        }
        let receipt = session.execution_receipt();
        assert_eq!(receipt.engine, SimExecutionEngine::Cranelift, "{receipt:?}");
        assert_eq!(receipt.refusal, None, "{receipt:?}");
        assert!(receipt.declined.is_empty(), "{receipt:?}");
        eprintln!(
            "original Euler38/{sample}: time={time}, states={}, {receipt:?}",
            expected.len()
        );
    }
}
