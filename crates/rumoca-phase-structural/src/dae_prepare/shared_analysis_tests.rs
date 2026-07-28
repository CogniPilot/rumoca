//! Regression tests for the scan-wide [`super::DummyStateAnalysis`] shared by
//! the constrained-dummy reduction.
//!
//! The whole-model tables the scan needs (`build_expression_scalarization_context`
//! and `structural_scalar_bindings`) used to be rebuilt inside every candidate,
//! which made the scan quadratic in model size. They are now built once per
//! demotion round. These tests pin the two properties that makes safe: the
//! shared-analysis entry point agrees with the self-contained one, and a
//! reduction that needs several rounds still demotes exactly the same states
//! (i.e. no round observes a stale analysis built before an earlier round's
//! commit).

use super::*;

fn test_span(offset: usize) -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("dae_prepare_shared_analysis_test.mo"),
        offset,
        offset + 1,
    )
}

fn test_variable(name: &str, offset: usize) -> Variable {
    let mut variable = Variable::new(VarName::new(name), test_span(offset));
    variable.source_span = test_span(offset);
    variable
}

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: vec![],
        span: test_span(7),
    }
}

fn der(name: &str) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var(name)],
        span: test_span(9),
    }
}

fn int(value: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span: test_span(11),
    }
}

fn sub(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(13),
    }
}

fn residual(rhs: Expression) -> Equation {
    Equation {
        lhs: None,
        rhs,
        span: test_span(15),
        origin: "shared analysis test".to_string(),
        scalar_count: 1,
    }
}

/// Two independent constrained-state groups. Each demotion round commits one
/// plan and restarts, so reducing this model takes more than one round and the
/// second round must see an analysis built after the first round's commit.
fn two_round_constrained_dae() -> Dae {
    let mut dae = Dae::new();
    for (index, offset) in [(1usize, 21usize), (2, 31)] {
        push_constrained_group(&mut dae, index, offset);
    }
    dae
}

/// One constrained group: `x = alias`, `alias = q`, `der(x) = der(q)`,
/// `der(q) = 1`, so `x` is a constrained dummy state of the retained state `q`.
fn push_constrained_group(dae: &mut Dae, index: usize, offset: usize) {
    dae.variables.states.insert(
        VarName::new(format!("x{index}")),
        test_variable(&format!("x{index}"), offset),
    );
    dae.variables.states.insert(
        VarName::new(format!("q{index}")),
        test_variable(&format!("q{index}"), offset + 2),
    );
    dae.variables.algebraics.insert(
        VarName::new(format!("alias{index}")),
        test_variable(&format!("alias{index}"), offset + 4),
    );
    dae.continuous.equations.push(residual(sub(
        var(&format!("x{index}")),
        var(&format!("alias{index}")),
    )));
    dae.continuous.equations.push(residual(sub(
        var(&format!("alias{index}")),
        var(&format!("q{index}")),
    )));
    dae.continuous.equations.push(residual(sub(
        der(&format!("x{index}")),
        der(&format!("q{index}")),
    )));
    dae.continuous
        .equations
        .push(residual(sub(der(&format!("q{index}")), int(1))));
}

/// Pin what the scan produces, against an expectation written out here.
///
/// Comparing `constrained_dummy_state_defining_exprs` to
/// `constrained_dummy_state_defining_exprs_with_analysis` would prove nothing:
/// the former *is* `DummyStateAnalysis::build` followed by the latter, so both
/// sides of such an equality run the same code over the same tables and it
/// cannot fail. This asserts the candidate set and each candidate's defining
/// expression directly, so it fails if the shared implementation starts
/// producing something else.
#[test]
fn constrained_dummy_scan_pins_its_candidate_definitions() {
    let dae = two_round_constrained_dae();

    let definitions = constrained_dummy_state_defining_exprs(&dae)
        .expect("constrained-dummy scan should succeed");

    assert_eq!(
        definitions
            .keys()
            .map(VarName::as_str)
            .collect::<Vec<_>>()
            .as_slice(),
        ["q1", "q2", "x1", "x2"],
        "each constrained group contributes both states of its alias chain: \
         `x = alias` assigns x directly and `alias = q` assigns q directly"
    );
    for name in ["x1", "x2", "q1", "q2"] {
        let index = name
            .strip_prefix(['x', 'q'])
            .expect("fixture names end in their group index");
        let state = VarName::new(name);
        let definition = definitions
            .get(&state)
            .expect("both constrained states are candidates");
        assert_eq!(
            definition
                .component_defining_exprs
                .keys()
                .map(VarName::as_str)
                .collect::<Vec<_>>()
                .as_slice(),
            [state.as_str()],
            "a scalar state's definition has one component: itself"
        );
        let component = definition
            .component_defining_exprs
            .get(&state)
            .expect("the scalar component is keyed by the state name");
        assert!(
            rumoca_core::expressions_semantically_equal(component, &var(&format!("alias{index}"))),
            "{name} must be defined by its alias row, got {component:?}"
        );
        let aggregate = definition
            .aggregate_defining_expr
            .as_ref()
            .expect("a directly assigned state records its whole-variable definition");
        assert!(
            rumoca_core::expressions_semantically_equal(aggregate, &var(&format!("alias{index}"))),
            "the aggregate definition of a scalar state is its single component"
        );
        assert!(
            definition.structural_params.is_empty(),
            "no structural parameter is needed to project a scalar state"
        );
    }
}

/// The analysis carries whole-model *tables* only; the scan itself always reads
/// the DAE it is handed. That is exactly why a per-round rebuild is enough, and
/// why a per-candidate rebuild was waste.
///
/// Pin it by mutating the DAE after building the analysis: the scan must report
/// the group that was added afterwards, which it can only do by reading the
/// current equations rather than anything the analysis captured.
#[test]
fn shared_analysis_scan_reads_the_current_dae() {
    let mut dae = two_round_constrained_dae();
    let analysis =
        DummyStateAnalysis::build(&dae).expect("whole-model analysis tables should build");

    push_constrained_group(&mut dae, 3, 41);
    let definitions = constrained_dummy_state_defining_exprs_with_analysis(&dae, &analysis)
        .expect("shared-analysis constrained-dummy scan should succeed");

    assert_eq!(
        definitions
            .keys()
            .map(VarName::as_str)
            .collect::<Vec<_>>()
            .as_slice(),
        ["q1", "q2", "q3", "x1", "x2", "x3"],
        "the scan must see the group added after the analysis was built"
    );
}

#[test]
fn multi_round_reduction_rebuilds_analysis_per_round() {
    let mut dae = two_round_constrained_dae();

    let demoted = reduce_constrained_dummy_derivatives(&mut dae)
        .expect("both constrained groups should reduce");

    assert_eq!(demoted, 2, "each round should demote exactly one state");
    for index in [1usize, 2] {
        assert!(
            dae.variables
                .algebraics
                .contains_key(&VarName::new(format!("x{index}"))),
            "x{index} should have been demoted to the algebraic partition"
        );
        assert!(
            dae.variables
                .states
                .contains_key(&VarName::new(format!("q{index}"))),
            "q{index} should remain the retained state of its group"
        );
    }
    assert!(
        dae.continuous
            .equations
            .iter()
            .all(
                |equation| !expr_contains_der_of(&equation.rhs, &VarName::new("x1"))
                    && !expr_contains_der_of(&equation.rhs, &VarName::new("x2"))
            ),
        "no demoted state derivative should survive the reduction"
    );
}
