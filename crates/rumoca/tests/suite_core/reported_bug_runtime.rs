//! Public original Bug reproductions, beyond compiler admission.

use rumoca::{CompilationResult, Compiler};
use rumoca_sim::{SimOptions, SimResult, SimSolverMode, SimulationSession, simulate_dae};
use rumoca_solver::SimExecutionPolicy;

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

fn last(result: &SimResult, name: &str) -> f64 {
    let column = result
        .names
        .iter()
        .position(|value| value == name)
        .unwrap_or_else(|| panic!("trace contains {name}: {:?}", result.names));
    *result.data[column].last().expect("nonempty trace")
}

#[test]
fn issue_403_inactive_boolean_operand_is_lazy_in_public_execution() {
    let compiled = compile(GUARDED, "GuardedOperands");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        for (index, expected) in [(3.0, 1.0), (4.0, 0.0)] {
            let mut opts = options(policy);
            opts.initial_inputs = vec![("k".into(), index)];
            let result = simulate_dae(&compiled.dae, &opts)
                .unwrap_or_else(|error| panic!("guarded operand {policy:?}, k={index}: {error:?}"));
            assert_eq!(last(&result, "y"), expected, "{policy:?}, k={index}");
        }
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
        let diagnostic = format!("{error:?}").to_lowercase();
        assert!(
            diagnostic.contains("index") || diagnostic.contains("bounds"),
            "{diagnostic}"
        );
    }
}

#[test]
fn issue_405_original_input_schedule_switches_at_each_write() {
    let compiled = compile(INPUT, "TopLevel");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let mut opts = options(policy);
        opts.t_end = 2.0;
        opts.initial_inputs = vec![("u".into(), 0.0)];
        let mut session = SimulationSession::new(&compiled.dae, opts).unwrap();
        let mut expected = 0.0;
        for step in 0..20 {
            match step {
                3 | 13 => {
                    session.set_input("u", 7.0).unwrap();
                    expected = 1.0;
                }
                8 => {
                    session.set_input("u", 0.0).unwrap();
                    expected = 0.0;
                }
                _ => {}
            }
            session.advance_to((step + 1) as f64 * 0.1).unwrap();
            assert_eq!(
                session.state().unwrap().values["z"],
                expected,
                "{policy:?}, step {step}"
            );
        }
    }
}

#[test]
fn issue_411_original_main_dae_integer_mod_retains_wide_literal() {
    let compiled = compile(INTEGER, "IntegerPrecision");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let mut opts = options(policy);
        opts.initial_inputs = vec![("n".into(), 0.0)];
        let result = simulate_dae(&compiled.dae, &opts)
            .unwrap_or_else(|error| panic!("Integer arithmetic {policy:?}: {error:?}"));
        assert_eq!(last(&result, "y"), 5.0, "{policy:?}");
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
        for (name, polynomial) in [
            ("u[1]", 1.0),
            ("u[2]", 2.0),
            ("u[3]", 2.5),
            ("u[4]", 8.0 / 3.0),
        ] {
            let expected = (-1.0_f64).exp() * polynomial;
            assert!(
                (last(&result, name) - expected).abs() < 1e-7,
                "{policy:?}: {name}"
            );
        }
    }
}
