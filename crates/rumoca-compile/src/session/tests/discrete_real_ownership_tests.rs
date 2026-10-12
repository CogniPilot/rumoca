use super::*;

fn compile_source(source: &str) -> CompilationResult {
    let mut session = Session::default();
    session
        .add_document("DiscreteReal.mo", source)
        .expect("source parses");
    session.compile_model("M").expect("legal source compiles")
}

fn assert_missing_when(source: &str) {
    let mut session = Session::default();
    session
        .add_document("DiscreteReal.mo", source)
        .expect("source parses");
    match session.compile_model_phases("M").expect("phase result") {
        PhaseResult::Failed {
            phase,
            error_code,
            error,
            ..
        } => {
            assert_eq!(phase, FailedPhase::ToDae);
            assert_eq!(
                error_code
                    .as_deref()
                    .map(rumoca_core::short_phase_error_code),
                Some("ED024")
            );
            assert!(error.contains("discrete Real `d`"), "{error}");
            assert!(error.contains("when"), "{error}");
        }
        other => panic!("unowned discrete Real must be refused: {other:?}"),
    }
    let diagnostics = session.compile_model_diagnostics("M");
    let error = diagnostics
        .diagnostics
        .iter()
        .find(|error| {
            error
                .code
                .as_deref()
                .map(rumoca_core::short_phase_error_code)
                == Some("ED024")
        })
        .expect("declaration diagnostic");
    let label = error
        .labels
        .iter()
        .find(|label| label.primary)
        .expect("declaration primary label");
    assert_eq!(
        label.span.source,
        rumoca_core::SourceId::from_source_name("DiscreteReal.mo")
    );
    let declaration = &source[label.span.start.0..label.span.end.0];
    assert!(
        declaration.contains('d'),
        "diagnostic labels the declaration: {declaration}"
    );
}

#[test]
fn discrete_real_without_when_rejects_the_original_derivative_alias() {
    assert_missing_when(
        "model M Real x; Real a; discrete Real d; equation a=der(x); der(x)=-x; d=der(x); end M;",
    );
}

#[test]
fn discrete_real_without_when_rejects_continuous_time_definition() {
    assert_missing_when("model M discrete Real d; equation d=time; end M;");
}

#[test]
fn discrete_real_binding_without_when_is_not_a_storage_owner() {
    assert_missing_when("model M discrete Real d=1.0; end M;");
}

#[test]
fn discrete_real_without_when_rejects_a_sample_if_observer() {
    assert_missing_when(
        "model M discrete Real d(start=0); algorithm if sample(0,0.1) then d:=1; end if; end M;",
    );
}

#[test]
fn discrete_real_when_equation_may_read_the_continuous_derivative() {
    let compiled = compile_source(
        "model M Real x; Real a; discrete Real d; equation a=der(x); der(x)=-x; when sample(0,0.1) then d=der(x); end when; end M;",
    );
    assert_eq!(compiled.balance_detail.equations_unknowns(), (3, 3));
}

#[test]
fn discrete_real_when_algorithm_may_read_the_continuous_derivative() {
    let compiled = compile_source(
        "model M Real x(start=1,fixed=true); discrete Real d(start=0); equation der(x)=-x; algorithm when sample(0,0.1) then d:=der(x); end when; end M;",
    );
    assert_eq!(compiled.balance_detail.equations_unknowns(), (2, 2));
}

#[test]
fn discrete_real_literal_false_when_retains_its_source_owner() {
    let compiled = compile_source(
        "model M discrete Real d(start=0,fixed=true); equation when false then d=1; end when; end M;",
    );
    assert_eq!(compiled.balance_detail.equations_unknowns(), (1, 1));
    assert_eq!(compiled.dae.active_discrete_scalar_count(), 1);
}

#[test]
fn discrete_real_inherited_whole_array_keeps_when_ownership() {
    let compiled = compile_source(
        "model Base discrete Real d[2](each start=0); end Base; model M extends Base; Real x; equation der(x)=-x; when sample(0,0.1) then d={1.0,2.0}; end when; end M;",
    );
    assert_eq!(compiled.balance_detail.equations_unknowns(), (3, 3));
}

#[test]
fn discrete_real_partial_algorithm_when_keeps_the_whole_storage_owner() {
    let compiled = compile_source(
        "model M discrete Real d[2](each start=0); algorithm when sample(0,0.1) then d[1]:=1; end when; end M;",
    );
    assert_eq!(compiled.balance_detail.equations_unknowns(), (2, 2));
}

#[test]
fn discrete_real_external_input_still_requires_a_when_owner() {
    assert_missing_when("model M discrete input Real d; Real x; equation der(x)=d; end M;");
}

#[test]
fn continuous_real_external_input_keeps_its_input_owner() {
    let compiled = compile_source("model M input Real u; Real x; equation der(x)=u; end M;");
    assert_eq!(compiled.balance_detail.equations_unknowns(), (1, 1));
}

#[test]
fn integer_external_input_keeps_its_input_owner() {
    let compiled = compile_source("model M input Integer u; Real x; equation der(x)=u; end M;");
    assert_eq!(compiled.balance_detail.equations_unknowns(), (1, 1));
}

#[test]
fn clocked_real_equation_has_its_actual_partition_owner() {
    let compiled = compile_source(
        "model M Clock c=Clock(0.1); Real d(start=0); Real x; equation der(x)=-x; d=sample(x,c); end M;",
    );
    compiled
        .dae
        .inspect(|view| assert!(view.clock_ownership_count() > 0));
}

#[test]
fn discrete_real_empty_domain_needs_no_when_writer() {
    let compiled = compile_source("model M discrete Real d[0]; Real x; equation der(x)=-x; end M;");
    assert_eq!(compiled.balance_detail.equations_unknowns(), (1, 1));
}

#[test]
fn discrete_real_mixed_array_equation_and_when_algorithm_remains_overdetermined() {
    let mut session = Session::default();
    session.add_document("DiscreteReal.mo", "model M discrete Real d[2](each start=0); equation d[2]=time; algorithm when sample(0,0.1) then d[1]:=1; end when; end M;").expect("source parses");
    match session.compile_model_phases("M").expect("phase result") {
        PhaseResult::Failed {
            phase,
            error_code,
            error,
            ..
        } => {
            assert_eq!(phase, FailedPhase::ToDae);
            assert_eq!(
                error_code
                    .as_deref()
                    .map(rumoca_core::short_phase_error_code),
                Some("ED001")
            );
            assert!(error.contains("3 equations, 2 unknowns"), "{error}");
        }
        other => {
            panic!("the atomic algorithm owns both lanes, leaving three definitions: {other:?}")
        }
    }
}

#[test]
fn discrete_real_clock_coordinate_still_requires_a_source_when_owner() {
    assert_missing_when(
        "model M Clock c=Clock(0.1); discrete Real d(start=0); Real x; equation der(x)=-x; d=sample(x,c); end M;",
    );
}

#[test]
fn discrete_real_clock_when_keeps_its_actual_partition_owner() {
    let compiled = compile_source(
        "model M Clock c=Clock(0.1); discrete Real d(start=0); Real x; equation der(x)=-x; when c then d=sample(x,c); end when; end M;",
    );
    compiled
        .dae
        .inspect(|view| assert!(view.clock_ownership_count() > 0));
}
