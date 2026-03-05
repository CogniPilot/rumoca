//! Shared test helpers for contract tests.
//!
//! Provides convenience functions for compiling Modelica models
//! and asserting success/failure/balance conditions.

use rumoca_phase_parse::{ParseError, parse_to_ast, parse_to_ast_with_errors};
use rumoca_session::{CompilationResult, FailedPhase, PhaseResult, Session, SessionConfig};

/// Compile a model from source, expecting success.
/// Returns the CompilationResult for further assertions.
///
/// # Panics
/// Panics if compilation fails.
pub fn expect_success(source: &str, model: &str) -> CompilationResult {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .unwrap_or_else(|e| panic!("Parse failed for {model}: {e}"));
    session
        .compile_model(model)
        .unwrap_or_else(|e| panic!("Compilation failed for {model}: {e}"))
}

/// Compile a model from source, expecting resolve failure with a specific code
/// (e.g. `ER005`, `rumoca::resolve::ER005`).
///
/// # Panics
/// Panics if parsing fails unexpectedly, resolve succeeds, or no diagnostic code matches.
pub fn expect_resolve_failure_with_code(source: &str, model: &str, expected_code: &str) {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .unwrap_or_else(|e| panic!("Parse failed unexpectedly for {model}: {e}"));

    if let Ok(phase_result) = session.compile_model_phases(model) {
        panic!(
            "Expected resolve failure with code {expected_code} for model {model}, \
             but compile_model_phases returned {:?}",
            phase_result
        );
    }

    let diagnostics = session.compile_model_diagnostics(model);
    let codes: Vec<String> = diagnostics
        .diagnostics
        .iter()
        .filter_map(|d| d.code.clone())
        .collect();
    let matched = codes
        .iter()
        .any(|code| error_code_matches(code.as_str(), expected_code));
    assert!(
        matched,
        "Expected resolve diagnostic code {expected_code} for model {model}, got codes: {:?}",
        codes
    );
}

/// Compile a model from source, expecting failure in a specific compile phase
/// and with a specific error code (e.g. `ET002`).
///
/// # Panics
/// Panics if parsing fails, compilation succeeds, needs synthesized inner bindings,
/// fails in a different phase, or error code does not match.
pub fn expect_failure_in_phase_with_code(
    source: &str,
    model: &str,
    expected_phase: FailedPhase,
    expected_code: &str,
) {
    let phase_result = compile_model_phases_or_panic(source, model);
    let (actual_phase, actual_code) =
        extract_failed_phase_and_code(phase_result, model, expected_code);
    assert_eq!(
        actual_phase, expected_phase,
        "Expected failure in phase {expected_phase} for model {model}, got {actual_phase}"
    );
    assert!(
        error_code_matches(&actual_code, expected_code),
        "Expected error code {expected_code} for model {model}, got {actual_code} (phase={actual_phase})"
    );
}

/// Compile a model from source, expecting failure in a specific compile phase.
///
/// Use this when a contract is known to reject but the exact diagnostic code
/// is not yet stabilized.
///
/// # Panics
/// Panics if parsing fails, compilation succeeds, needs synthesized inner
/// bindings, or fails in a different phase.
pub fn expect_failure_in_phase(source: &str, model: &str, expected_phase: FailedPhase) {
    let phase_result = compile_model_phases_or_panic(source, model);
    let (actual_phase, _) = extract_failed_phase_and_optional_code(phase_result, model);
    assert_eq!(
        actual_phase, expected_phase,
        "Expected failure in phase {expected_phase} for model {model}, got {actual_phase}"
    );
}

fn compile_model_phases_or_panic(source: &str, model: &str) -> PhaseResult {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .unwrap_or_else(|e| panic!("Parse failed for {model}: {e}"));
    session
        .compile_model_phases(model)
        .unwrap_or_else(|e| panic!("compile_model_phases failed for {model}: {e}"))
}

fn extract_failed_phase_and_optional_code(
    phase_result: PhaseResult,
    model: &str,
) -> (FailedPhase, Option<String>) {
    match phase_result {
        PhaseResult::Success(_) => {
            panic!("Expected compilation failure for model {model}, but it succeeded")
        }
        PhaseResult::NeedsInner { .. } => {
            panic!(
                "Expected compile-phase failure for model {model}, got NeedsInner (missing inner declarations)"
            )
        }
        PhaseResult::Failed {
            phase, error_code, ..
        } => (phase, error_code),
    }
}

fn extract_failed_phase_and_code(
    phase_result: PhaseResult,
    model: &str,
    expected_code: &str,
) -> (FailedPhase, String) {
    let (phase, maybe_code) = extract_failed_phase_and_optional_code(phase_result, model);
    let code = maybe_code.unwrap_or_else(|| {
        panic!(
            "Expected error code {expected_code} for model {model}, but compiler returned no error code"
        )
    });
    (phase, code)
}

fn error_code_matches(actual: &str, expected: &str) -> bool {
    actual == expected || actual.ends_with(expected)
}

/// Compile a model from source, expecting success AND a balanced system
/// (balance == 0, i.e., equations == unknowns).
///
/// # Panics
/// Panics if compilation fails or the system is not balanced.
pub fn expect_balanced(source: &str, model: &str) -> CompilationResult {
    let result = expect_success(source, model);
    let balance = result.dae.balance();
    assert_eq!(
        balance, 0,
        "Expected balanced system for {model}, got balance={balance}"
    );
    result
}

/// Returns true when the compiled model is standalone-simulatable with default bindings.
///
/// Current standalone criteria:
/// - not partial
/// - no top-level unbound input variables
/// - no unbound fixed parameters (fixed=true by default for parameters)
pub fn is_standalone_simulatable(result: &CompilationResult) -> bool {
    !result.dae.is_partial
        && result.dae.inputs.is_empty()
        && !result.flat.has_unbound_fixed_parameters()
}

/// Collect unbound fixed parameter names as strings for assertions.
pub fn unbound_fixed_parameter_names(result: &CompilationResult) -> Vec<String> {
    result
        .flat
        .unbound_fixed_parameters()
        .into_iter()
        .map(|n| n.as_str().to_string())
        .collect()
}

/// Assert that the given Modelica source parses successfully.
///
/// # Panics
/// Panics if parsing fails.
pub fn expect_parse_ok(source: &str) {
    parse_to_ast(source, "test.mo")
        .unwrap_or_else(|e| panic!("Expected parse success, got error: {e}"));
}

/// Assert that the given Modelica source fails to parse with a specific code
/// (e.g. `EP001`, `rumoca::parse::EP001`).
///
/// # Panics
/// Panics if parsing succeeds or no parse diagnostic code matches.
pub fn expect_parse_err_with_code(source: &str, expected_code: &str) {
    match parse_to_ast_with_errors(source, "test.mo") {
        Ok(_) => panic!("Expected parse failure with code {expected_code}, but parsing succeeded"),
        Err(parse_errors) => {
            let codes: Vec<String> = parse_errors
                .iter()
                .map(|e| parse_error_code(e).to_string())
                .collect();
            let matched = codes
                .iter()
                .any(|code| error_code_matches(code.as_str(), expected_code));
            assert!(
                matched,
                "Expected parse diagnostic code {expected_code}, got codes: {:?}",
                codes
            );
        }
    }
}

fn parse_error_code(error: &ParseError) -> &'static str {
    match error {
        ParseError::SyntaxError { .. } => "EP001",
        ParseError::NoAstProduced => "EP002",
        ParseError::IoError { .. } => "EP003",
    }
}
