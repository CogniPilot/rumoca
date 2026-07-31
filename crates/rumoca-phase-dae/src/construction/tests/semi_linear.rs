//! MLS §3.7.4.5 `semiLinear(x, positiveSlope, negativeSlope)`.
//!
//! The operator returns
//! `smooth(0, if x >= 0 then positiveSlope*x else negativeSlope*x)`, so the
//! checked DAE has to own that conditional exactly — one relation selecting
//! between two linear segments of the *same* operand — and, because the
//! definition is wrapped in `smooth(0, ...)`, the relation must stay a pure
//! branch selector instead of acquiring a state-event root.

use super::super::*;
use super::support::*;

const MODEL_TEXT: &str =
    "model M Real y; Real x; equation 0 = y - semiLinear(x, 2.0, 3.0); x = 1.0; end M;";

fn semi_linear_model(source: &TestSource, arguments: Vec<Expression>) -> flat::Model {
    let mut model = test_model();
    add_primitive_variable(&mut model, source, "y", "Real y", 7, Vec::new(), false);
    add_primitive_variable(&mut model, source, "x", "Real x", 7, Vec::new(), false);

    let call_span = source.span("semiLinear(x, 2.0, 3.0)", 0);
    let residual_span = source.span("y - semiLinear(x, 2.0, 3.0)", 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(variable_reference(source, "y", "y -", 0, Vec::new())),
            rhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::SemiLinear,
                args: arguments,
                span: call_span,
            }),
            span: residual_span,
        },
        residual_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let binding_span = source.span("x = 1.0", 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(variable_reference(source, "x", "x = 1.0", 0, Vec::new())),
            rhs: Box::new(Expression::Literal {
                value: Literal::Real(1.0),
                span: source.span("1.0", 0),
            }),
            span: binding_span,
        },
        binding_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
    model
}

fn default_arguments(source: &TestSource) -> Vec<Expression> {
    vec![
        variable_reference(source, "x", "x, 2.0", 0, Vec::new()),
        Expression::Literal {
            value: Literal::Real(2.0),
            span: source.span("2.0", 0),
        },
        Expression::Literal {
            value: Literal::Real(3.0),
            span: source.span("3.0", 0),
        },
    ]
}

/// The residual is `y - semiLinear(...)`; its right operand is the operator.
fn semi_linear_expression<'dae>(view: dae::DaeView<'dae>) -> dae::ExprId<'dae> {
    let residual = view
        .continuous_equation(0)
        .expect("the semiLinear equation is constructed")
        .residual();
    let dae::ExpressionOperation::Binary { rhs, .. } = view
        .expression(residual)
        .expect("residual is an expression")
        .operation()
    else {
        panic!("residual is `y - semiLinear(...)`");
    };
    rhs
}

#[test]
fn semi_linear_lowers_to_the_checked_conditional_of_its_two_segments() {
    let source = TestSource::new(MODEL_TEXT);
    let model = semi_linear_model(&source, default_arguments(&source));
    let call_span = source.span("semiLinear(x, 2.0, 3.0)", 0);
    let dae = construct(&model, source.map).expect("semiLinear has a checked DAE owner");

    dae.inspect(|view| {
        let operator = semi_linear_expression(view);
        let operator_view = view
            .expression(operator)
            .expect("operator is an expression");
        assert_eq!(
            operator_view.provenance().span(),
            call_span,
            "the conditional keeps the operator's own source span"
        );
        assert_eq!(
            operator_view.provenance().origin(),
            dae::DaeProvenanceOrigin::Source
        );

        let dae::ExpressionOperation::Conditional(operands) = operator_view.operation() else {
            panic!("semiLinear lowers to a conditional");
        };
        assert_eq!(
            operands.len(),
            3,
            "one condition/value pair plus the negative-slope fallback"
        );

        // The condition is `x >= 0`.
        let condition = view
            .expression(operands.get(0).expect("condition operand"))
            .expect("condition is an expression");
        let dae::ExpressionOperation::Binary {
            operator: comparison,
            lhs: operand,
            rhs: zero,
            ..
        } = condition.operation()
        else {
            panic!("the branch condition is a relation");
        };
        assert_eq!(comparison, dae::BinaryOperator::GreaterEqual);
        assert_eq!(
            condition.provenance().origin(),
            dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::SemiLinearLowering)
        );
        assert_eq!(condition.provenance().span(), call_span);
        assert!(matches!(
            view.expression(zero).expect("zero bound").operation(),
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(value)) if *value == 0.0
        ));

        // Both branches scale exactly the operand the relation tests, so the
        // operator is continuous at `x = 0` by construction.
        for (index, slope) in [(1_usize, 2.0_f64), (2, 3.0)] {
            let branch = view
                .expression(operands.get(index).expect("branch operand"))
                .expect("branch is an expression");
            let dae::ExpressionOperation::Binary {
                operator: product,
                lhs: gain,
                rhs: scaled,
                ..
            } = branch.operation()
            else {
                panic!("each branch is a product of a slope and the operand");
            };
            assert_eq!(product, dae::BinaryOperator::Multiply);
            assert_eq!(
                scaled, operand,
                "both segments scale the same lowered operand node"
            );
            assert!(matches!(
                view.expression(gain).expect("slope").operation(),
                dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(value)) if *value == slope
            ));
        }
    });
}

#[test]
fn semi_linear_owns_no_state_event_because_its_definition_is_smooth() {
    let source = TestSource::new(MODEL_TEXT);
    let model = semi_linear_model(&source, default_arguments(&source));
    let dae = construct(&model, source.map).expect("semiLinear has a checked DAE owner");

    dae.inspect(|view| {
        assert_eq!(
            view.relation_count(),
            0,
            "MLS §3.7.4.5 wraps the definition in `smooth(0, ...)`, so the branch \
             relation is not an MLS §8.5 crossing owner"
        );
        assert_eq!(view.root_count(), 0);
        assert_eq!(view.time_event_count(), 0);
    });
}

#[test]
fn semi_linear_without_three_operands_is_rejected() {
    let source = TestSource::new(MODEL_TEXT);
    let mut arguments = default_arguments(&source);
    arguments.pop();
    let model = semi_linear_model(&source, arguments);
    let call_span = source.span("semiLinear(x, 2.0, 3.0)", 0);

    let error =
        construct(&model, source.map).expect_err("semiLinear has a fixed three-operand contract");
    assert!(
        matches!(
            &error,
            ToDaeError::UnsupportedRuntimeOperator { operator, span, .. }
                if operator == "semiLinear" && *span == call_span
        ),
        "unexpected error: {error:?}"
    );
}

// ---------------------------------------------------------------------------
// MLS §3.7.4.5 Rule 1 / Rule 2
// ---------------------------------------------------------------------------

/// The connected shape `Modelica.Thermal.FluidHeatFlow` writes: two ports of one
/// node, each with `H = semiLinear(m, node_h, h)`, joined by the connection
/// balances `ma + mb = 0` and `Ha + Hb = 0`. Read literally the pair leaves the
/// node enthalpy `hn` undetermined at `m = 0`; MLS §3.7.4.5 Rule 1 is the
/// transformation that makes it definite.
const CHAIN_TEXT: &str = "model M Real m; Real ha; Real hb; Real hn; Real Ha; Real Hb; \
                          Real ma; Real mb; equation ma - m = 0; ma + mb = 0; Ha + Hb = 0; \
                          Ha = semiLinear(ma, hn, ha); Hb = semiLinear(mb, hn, hb); \
                          ha - 2.0 = 0; hb - 3.0 = 0; m - 1.0 = 0; end M;";

fn chain_variable(model: &mut flat::Model, source: &TestSource, name: &str) {
    add_primitive_variable(model, source, name, "Real m", 7, Vec::new(), false);
}

fn scalar_equation(
    source: &TestSource,
    owner: &str,
    lhs: Expression,
    rhs: Expression,
) -> flat::Equation {
    let span = source.span(owner, 0);
    flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span,
        },
        span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    )
}

fn reference(source: &TestSource, name: &str, owner: &str) -> Expression {
    variable_reference(source, name, owner, 0, Vec::new())
}

fn sum_equation(source: &TestSource, owner: &str, left: &str, right: &str) -> flat::Equation {
    let span = source.span(owner, 0);
    scalar_equation(
        source,
        owner,
        Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(reference(source, left, owner)),
            rhs: Box::new(reference(source, right, owner)),
            span,
        },
        Expression::Literal {
            value: Literal::Real(0.0),
            span,
        },
    )
}

fn semi_linear_equation(
    source: &TestSource,
    owner: &str,
    value: &str,
    operand: &str,
    slopes: (&str, &str),
) -> flat::Equation {
    let span = source.span(owner, 0);
    scalar_equation(
        source,
        owner,
        reference(source, value, owner),
        Expression::BuiltinCall {
            function: BuiltinFunction::SemiLinear,
            args: vec![
                reference(source, operand, owner),
                reference(source, slopes.0, owner),
                reference(source, slopes.1, owner),
            ],
            span,
        },
    )
}

fn literal_equation(source: &TestSource, owner: &str, name: &str, value: f64) -> flat::Equation {
    let span = source.span(owner, 0);
    scalar_equation(
        source,
        owner,
        reference(source, name, owner),
        Expression::Literal {
            value: Literal::Real(value),
            span,
        },
    )
}

/// Build the two-port node above. `operand` names the driving equation for `m`,
/// so a caller can pin the flow to zero and reach Rule 2 instead of Rule 1.
fn chain_model(source: &TestSource, driver: f64) -> flat::Model {
    let mut model = test_model();
    for name in ["m", "ha", "hb", "hn", "Ha", "Hb", "ma", "mb"] {
        chain_variable(&mut model, source, name);
    }
    model.add_equation(scalar_equation(
        source,
        "ma - m = 0",
        reference(source, "ma", "ma - m = 0"),
        reference(source, "m", "ma - m = 0"),
    ));
    model.add_equation(sum_equation(source, "ma + mb = 0", "ma", "mb"));
    model.add_equation(sum_equation(source, "Ha + Hb = 0", "Ha", "Hb"));
    model.add_equation(semi_linear_equation(
        source,
        "Ha = semiLinear(ma, hn, ha)",
        "Ha",
        "ma",
        ("hn", "ha"),
    ));
    model.add_equation(semi_linear_equation(
        source,
        "Hb = semiLinear(mb, hn, hb)",
        "Hb",
        "mb",
        ("hn", "hb"),
    ));
    model.add_equation(literal_equation(source, "ha - 2.0 = 0", "ha", 2.0));
    model.add_equation(literal_equation(source, "hb - 3.0 = 0", "hb", 3.0));
    model.add_equation(literal_equation(source, "m - 1.0 = 0", "m", driver));
    model
}

/// The two operands of a `chain_model` row's `lhs - rhs` residual.
fn residual_parts<'dae>(
    view: dae::DaeView<'dae>,
    equation: usize,
) -> (dae::ExprId<'dae>, dae::ExprId<'dae>) {
    let residual = view
        .continuous_equation(equation)
        .expect("row is constructed")
        .residual();
    let dae::ExpressionOperation::Binary { lhs, rhs, .. } = view
        .expression(residual)
        .expect("residual is an expression")
        .operation()
    else {
        panic!("every row of the fixture is `lhs - rhs`");
    };
    (lhs, rhs)
}

/// The algebraic coordinate `expression` reads, identified by ordinal so the
/// assertions never inspect a rendered name.
fn algebraic_index<'dae>(view: dae::DaeView<'dae>, expression: dae::ExprId<'dae>) -> u32 {
    match view
        .expression(expression)
        .expect("expression exists")
        .operation()
    {
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(id)) => id.index(),
        _ => panic!("expected a plain algebraic coordinate read"),
    }
}

fn conditional_operands<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> [dae::ExprId<'dae>; 3] {
    let dae::ExpressionOperation::Conditional(operands) = view
        .expression(expression)
        .expect("expression exists")
        .operation()
    else {
        panic!("expected a conditional");
    };
    assert_eq!(operands.len(), 3, "one condition and two branch values");
    [
        operands.get(0).expect("condition"),
        operands.get(1).expect("then"),
        operands.get(2).expect("else"),
    ]
}

#[test]
fn rule_one_rewrites_the_underdetermined_pair_into_a_selector_and_a_collapsed_operator() {
    let source = TestSource::new(CHAIN_TEXT);
    let model = chain_model(&source, 1.0);
    let dae = construct(&model, source.map).expect("the Rule 1 chain has a checked DAE owner");

    dae.inspect(|view| {
        // Row 3 carried `Ha = semiLinear(ma, hn, ha)`. MLS §3.7.4.5 Rule 1 turns
        // it into `s1 = if x >= 0 then sa else sb`, the equation that makes the
        // node enthalpy definite at `x = 0`.
        let (target, selection) = residual_parts(view, 3);
        let node = algebraic_index(view, target);
        let [condition, head, tail] = conditional_operands(view, selection);
        assert!(
            matches!(
                view.expression(condition)
                    .expect("condition is an expression")
                    .operation(),
                dae::ExpressionOperation::Binary {
                    operator: dae::BinaryOperator::GreaterEqual,
                    ..
                }
            ),
            "Rule 1 writes `if x >= 0`"
        );
        let (head, tail) = (algebraic_index(view, head), algebraic_index(view, tail));
        assert_ne!(head, tail, "the chain has two distinct end slopes");
        assert_ne!(
            node, head,
            "the selector determines the intermediate slope, not an end"
        );
        assert_ne!(node, tail);
        assert_eq!(
            view.expression(view.continuous_equation(3).expect("row 3").residual())
                .expect("residual is an expression")
                .provenance()
                .origin(),
            dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::SemiLinearLowering),
            "the rewritten row is not what the source wrote"
        );

        // Row 4 keeps `y = semiLinear(x, sa, sb)` — over the chain's two END
        // slopes, so the node enthalpy no longer appears in the operator and the
        // pair is no longer rank-deficient at `x = 0`.
        let (_, collapsed) = residual_parts(view, 4);
        let [_, positive, negative] = conditional_operands(view, collapsed);
        for (branch, expected) in [(positive, head), (negative, tail)] {
            let dae::ExpressionOperation::Binary {
                operator: dae::BinaryOperator::Multiply,
                lhs: gain,
                ..
            } = view
                .expression(branch)
                .expect("branch is an expression")
                .operation()
            else {
                panic!("each `semiLinear` segment is a slope times the operand");
            };
            assert_eq!(
                algebraic_index(view, gain),
                expected,
                "the surviving operator scales the chain's end slopes only"
            );
            assert_ne!(algebraic_index(view, gain), node);
        }
    });
}

#[test]
fn rule_one_relation_owns_no_state_event_because_the_operator_it_replaces_was_smooth() {
    let source = TestSource::new(CHAIN_TEXT);
    let model = chain_model(&source, 1.0);
    let dae = construct(&model, source.map).expect("the Rule 1 chain has a checked DAE owner");

    dae.inspect(|view| {
        assert_eq!(
            view.relation_count(),
            0,
            "the `x >= 0` Rule 1 lifts out of `semiLinear`'s own `smooth(0, ...)` keeps the \
             MLS §3.7.5 freedom rumoca already takes for the operator; OMC emits no zero \
             crossing for it either"
        );
        assert_eq!(view.root_count(), 0);
        assert_eq!(view.time_event_count(), 0);
    });
}

/// MLS §3.7.4.5 Rule 2's own shape: `x = 0; y = 0; y = semiLinear(x, sa, sb)`.
/// A `FlowPort` connected to nothing states exactly this — MLS §9.2 sets both
/// flow variables of a one-member connection set to zero — and the operator row
/// then says nothing at all about the port enthalpy.
const ZERO_FLOW_TEXT: &str = "model M Real m; Real H; Real hn; Real h; equation m - 0.0 = 0; \
                              H - 0.0 = 0; H = semiLinear(m, hn, h); h - 3.0 = 0; end M;";

fn zero_flow_model(source: &TestSource) -> flat::Model {
    let mut model = test_model();
    for name in ["m", "H", "hn", "h"] {
        chain_variable(&mut model, source, name);
    }
    model.add_equation(literal_equation(source, "m - 0.0 = 0", "m", 0.0));
    model.add_equation(literal_equation(source, "H - 0.0 = 0", "H", 0.0));
    model.add_equation(semi_linear_equation(
        source,
        "H = semiLinear(m, hn, h)",
        "H",
        "m",
        ("hn", "h"),
    ));
    model.add_equation(literal_equation(source, "h - 3.0 = 0", "h", 3.0));
    model
}

#[test]
fn rule_two_replaces_the_operator_with_its_slope_equality_when_both_sides_are_pinned_to_zero() {
    let source = TestSource::new(ZERO_FLOW_TEXT);
    let model = zero_flow_model(&source);
    let dae = construct(&model, source.map).expect("the Rule 2 shape has a checked DAE owner");

    dae.inspect(|view| {
        let residual = view
            .continuous_equation(2)
            .expect("the operator row is constructed")
            .residual();
        assert_eq!(
            view.expression(residual)
                .expect("residual is an expression")
                .provenance()
                .origin(),
            dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::SemiLinearLowering),
            "the row Rule 2 replaced is not what the source wrote"
        );
        let dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Subtract,
            lhs,
            rhs,
        } = view
            .expression(residual)
            .expect("residual is an expression")
            .operation()
        else {
            panic!("Rule 2 writes the difference `sa - sb`");
        };
        for operand in [lhs, rhs] {
            assert!(
                !matches!(
                    view.expression(operand)
                        .expect("operand is an expression")
                        .operation(),
                    dae::ExpressionOperation::Conditional(_)
                ),
                "MLS §3.7.4.5 Rule 2 replaces `y = semiLinear(x, sa, sb)` by `sa = sb`, so the \
                 operator's conditional is gone entirely"
            );
        }
    });
}
