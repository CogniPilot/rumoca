//! MLS §8.5 event-owner classification of `when` activation conditions.
//!
//! # Why this exists
//!
//! A `when` whose activation is a relation over `time` alone has an exactly
//! known instant, and MLS §8.5 says so: *"it is a quality of implementation
//! issue that the following special relations `time >= discrete expression`,
//! `time < discrete expression` trigger a time event at `time = discrete
//! expression`"*. Before these tests the whole activation tree was lowered as
//! zero crossings regardless, so `when time > 0.5` owned a root and no instant.
//! That is not merely inefficient. §8.5 also says *"an event generating
//! expression has an internal buffer, and the value of the expression can only
//! be changed at event instants"*, and the located crossing sits *on*
//! `t = 0.5`, where the strict relation still reads false — the crossing was
//! consumed with the activation never true, so the `when` body never ran.
//!
//! These tests pin the classification, not the numerics: the simulated
//! behaviour and its OpenModelica falsification live in
//! `crates/rumoca/tests/time_event_when_activation.rs`.

use rumoca_core::Reference;

use super::super::*;
use super::support::*;

/// Build `model M discrete Real y; equation when <condition> then y = 1.0; end when; end M;`
/// with `condition` already parsed into `condition_expression`.
fn when_activation_model(
    source: &TestSource,
    condition_expression: Expression,
    condition_span: Span,
) -> flat::Model {
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        source,
        "y",
        "discrete Real y",
        8,
        Vec::new(),
        false,
    );
    model
        .variables
        .get_mut(&VarName::new("y"))
        .unwrap()
        .variability = Variability::Discrete(Default::default());
    let mut branch = flat::WhenBranch::new(condition_expression, condition_span);
    branch.add_equation(flat::WhenEquation::assign(
        VarName::new("y"),
        Expression::Literal {
            value: Literal::Real(1.0),
            span: source.span("1.0", 0),
        },
        source.span("y = 1.0", 0),
        "when assignment",
    ));
    model
        .when_chains
        .push(flat::WhenChain::new(branch, condition_span));
    model
}

/// `time <op> <threshold>`, with the threshold's span taken from `literal` so
/// the caller's own source text carries it.
fn time_relation(
    source: &TestSource,
    text: &str,
    op: OpBinary,
    threshold: f64,
    literal: &str,
) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(Expression::VarRef {
            name: Reference::new("time"),
            subscripts: Vec::new(),
            span: source.span("time", 0),
        }),
        rhs: Box::new(Expression::Literal {
            value: Literal::Real(threshold),
            span: source.span(literal, 0),
        }),
        span: source.span(text, 0),
    }
}

/// Every ordering of a `time` relation is an instant, not a search.
///
/// MLS §8.5 names `time >= discrete expression` and `time < discrete
/// expression`; the other two orderings are their complements about the same
/// point, so each of the four changes value exactly once, at `t = 0.5`.
/// Strictness changes what the relation reads *at* that instant, not where the
/// instant is, so all four own a time event and none owns a root.
///
/// This asserts *where the instant is*, and nothing about what a `when` does
/// with it. The distinction matters for the falling orderings: whether a `when`
/// already true at `t = 0` is activated there is a separate, unrelated defect
/// (see the divergence record in
/// `crates/rumoca/tests/time_event_when_activation.rs`), and this test must not
/// be read as blessing that behaviour. The falling arms are asserted here only
/// because a relation that changes value at `t = 0.5` owns the instant `0.5`
/// whatever any `when` later does with it.
#[test]
fn every_time_ordering_owns_a_scheduled_instant_and_no_root() {
    for (text, op) in [
        ("time > 0.5", OpBinary::Gt),
        ("time >= 0.5", OpBinary::Ge),
        ("time < 0.5", OpBinary::Lt),
        ("time <= 0.5", OpBinary::Le),
    ] {
        let text_owned =
            format!("model M discrete Real y; equation when {text} then y = 1.0; end when; end M;");
        let source = TestSource::new(&text_owned);
        let condition = time_relation(&source, text, op, 0.5, "0.5");
        let span = source.span(text, 0);
        let model = when_activation_model(&source, condition, span);

        let dae = construct(&model, source.map).unwrap();
        dae.inspect(|view| {
            assert_eq!(
                view.time_event_count(),
                1,
                "`when {text}` must own one exactly scheduled instant"
            );
            let instant = view
                .time_event(view.time_event_id(0).unwrap())
                .unwrap()
                .instant()
                .to_f64();
            assert!(
                (instant - 0.5).abs() < 1.0e-12,
                "`when {text}` schedules t = 0.5, got {instant}"
            );
            assert_eq!(
                view.root_count(),
                0,
                "`when {text}` must not also search for the instant it already knows"
            );
        });
    }
}

/// A `time` relation keeps its instant when it shares the activation with a
/// state relation, and the state relation keeps its root.
///
/// This is the shape the classification bug hid behind: `and` is one activation
/// with two relational leaves, so a per-leaf decision is the only correct one.
/// Collecting the state leaf as an owner here as well would build its root
/// twice, which is why the analysis records only the scheduled owners of an
/// activation.
#[test]
fn compound_activation_splits_the_instant_from_the_state_crossing() {
    let text = "model M Real x; discrete Real y; equation \
                der(x) = 1.0; when time > 0.5 and x < 2.0 then y = 1.0; end when; end M;";
    let source = TestSource::new(text);
    let condition_span = source.span("time > 0.5 and x < 2.0", 0);
    let condition = Expression::Binary {
        op: OpBinary::And,
        lhs: Box::new(time_relation(
            &source,
            "time > 0.5",
            OpBinary::Gt,
            0.5,
            "0.5",
        )),
        rhs: Box::new(Expression::Binary {
            op: OpBinary::Lt,
            lhs: Box::new(variable_reference(&source, "x", "x < 2.0", 0, Vec::new())),
            rhs: Box::new(Expression::Literal {
                value: Literal::Real(2.0),
                span: source.span("2.0", 0),
            }),
            span: source.span("x < 2.0", 0),
        }),
        span: condition_span,
    };
    let mut model = when_activation_model(&source, condition, condition_span);
    add_primitive_variable(&mut model, &source, "x", "Real x", 9, Vec::new(), false);
    model.equations.push(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![variable_reference(&source, "x", "der(x)", 0, Vec::new())],
                span: source.span("der(x)", 0),
            }),
            rhs: Box::new(Expression::Literal {
                value: Literal::Real(1.0),
                span: source.span("1.0", 0),
            }),
            span: source.span("der(x) = 1.0", 0),
        },
        source.span("der(x) = 1.0", 0),
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        assert_eq!(
            view.time_event_count(),
            1,
            "the `time` leaf still owns its instant inside an `and`"
        );
        assert_eq!(
            view.root_count(),
            1,
            "the state leaf still owns exactly one zero crossing"
        );
        let root = view.root(view.root_id(0).unwrap()).unwrap();
        let relation = view.relation(root.relation()).unwrap();
        assert_eq!(
            view.source_text(relation.provenance()),
            Some("x < 2.0"),
            "the surviving crossing is the state relation, not the `time` relation"
        );
    });
}

/// A threshold that is only known after parameter evaluation is still exact.
///
/// MLS §8.5 writes the threshold as a *discrete expression*, not as a literal,
/// so a parameter-evaluable right-hand side qualifies.
#[test]
fn parameter_threshold_still_resolves_to_an_exact_instant() {
    let text = "model M parameter Real p = 0.5; discrete Real y; equation \
                when time > p then y = 1.0; end when; end M;";
    let source = TestSource::new(text);
    let condition_span = source.span("time > p", 0);
    let condition = Expression::Binary {
        op: OpBinary::Gt,
        lhs: Box::new(Expression::VarRef {
            name: Reference::new("time"),
            subscripts: Vec::new(),
            span: source.span("time", 0),
        }),
        rhs: Box::new(variable_reference(&source, "p", "time > p", 0, Vec::new())),
        span: condition_span,
    };
    let mut model = when_activation_model(&source, condition, condition_span);
    add_primitive_variable(
        &mut model,
        &source,
        "p",
        "parameter Real p = 0.5",
        9,
        Vec::new(),
        false,
    );
    {
        let parameter = model.variables.get_mut(&VarName::new("p")).unwrap();
        parameter.variability = Variability::Parameter(Default::default());
        parameter.binding = Some(Expression::Literal {
            value: Literal::Real(0.5),
            span: source.span("0.5", 0),
        });
    }

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        assert_eq!(view.time_event_count(), 1);
        assert_eq!(view.root_count(), 0);
        let instant = view
            .time_event(view.time_event_id(0).unwrap())
            .unwrap()
            .instant()
            .to_f64();
        assert!(
            (instant - 0.5).abs() < 1.0e-12,
            "expected t = 0.5, got {instant}"
        );
    });
}

/// A threshold that moves with the model state is not knowable in advance.
///
/// `when time >= pre(y)` reschedules itself, so the instant is not known before
/// the event that sets it and MLS §8.5's advance schedule does not apply: the
/// activation keeps its zero crossing. This is the boundary of the
/// classification — without it the periodic-source counter idiom of
/// `Modelica.Blocks.Sources.Pulse` would lose its event.
#[test]
fn state_dependent_threshold_keeps_its_zero_crossing() {
    let text = "model M discrete Real y; equation \
                when time >= pre(y) then y = 1.0; end when; end M;";
    let source = TestSource::new(text);
    let condition_span = source.span("time >= pre(y)", 0);
    let condition = Expression::Binary {
        op: OpBinary::Ge,
        lhs: Box::new(Expression::VarRef {
            name: Reference::new("time"),
            subscripts: Vec::new(),
            span: source.span("time", 0),
        }),
        rhs: Box::new(Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args: vec![variable_reference(&source, "y", "pre(y)", 0, Vec::new())],
            span: source.span("pre(y)", 0),
        }),
        span: condition_span,
    };
    let model = when_activation_model(&source, condition, condition_span);

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        assert_eq!(
            view.time_event_count(),
            0,
            "a self-rescheduling threshold has no instant known in advance"
        );
        assert_eq!(
            view.root_count(),
            1,
            "so the activation must keep the zero crossing that locates it"
        );
    });
}

/// Append `when time > <threshold> then <target> = 1.0` at `shared_span`.
fn push_shared_span_activation(
    model: &mut flat::Model,
    source: &TestSource,
    shared_span: Span,
    target: &str,
    threshold: &str,
) {
    let condition = Expression::Binary {
        op: OpBinary::Gt,
        lhs: Box::new(Expression::VarRef {
            name: Reference::new("time"),
            subscripts: Vec::new(),
            span: source.span("time", 0),
        }),
        rhs: Box::new(variable_reference(
            source,
            threshold,
            threshold,
            0,
            Vec::new(),
        )),
        span: shared_span,
    };
    let mut branch = flat::WhenBranch::new(condition, shared_span);
    branch.add_equation(flat::WhenEquation::assign(
        VarName::new(target),
        Expression::Literal {
            value: Literal::Real(1.0),
            span: source.span("1.0", 0),
        },
        source.span(&format!("{target} = 1.0"), 0),
        "when assignment",
    ));
    model
        .when_chains
        .push(flat::WhenChain::new(branch, shared_span));
}

/// Two instances of one class own two instants, not one.
///
/// Flattening replicates the single written `time > threshold` across every
/// instance, so both occurrences carry the same source span and differ only in
/// the parameter each instance resolved. Addressing owners by span alone keeps
/// the first instant and drops the second, while lowering reads the surviving
/// plan for *both* occurrences and suppresses both zero crossings — leaving the
/// late instance with no event owner of any kind. This is the shape MSL is made
/// of: every `Pulse`, `Step` and `Trapezoid` in a model is the same source line
/// with a different `startTime`.
#[test]
fn each_instance_of_a_replicated_activation_owns_its_own_instant() {
    let text = "model M discrete Real y; discrete Real z; parameter Real a = 0.33; \
                parameter Real b = 0.73; equation when time > a then y = 1.0; end when; \
                when time > b then z = 1.0; end when; end M;";
    let source = TestSource::new(text);
    let mut model = test_model();
    for (name, declaration, type_id) in
        [("y", "discrete Real y", 8u32), ("z", "discrete Real z", 9)]
    {
        add_primitive_variable(
            &mut model,
            &source,
            name,
            declaration,
            type_id,
            Vec::new(),
            false,
        );
        model
            .variables
            .get_mut(&VarName::new(name))
            .unwrap()
            .variability = Variability::Discrete(Default::default());
    }
    for (name, declaration, type_id, value) in [
        ("a", "parameter Real a = 0.33", 10u32, 0.33),
        ("b", "parameter Real b = 0.73", 11, 0.73),
    ] {
        add_primitive_variable(
            &mut model,
            &source,
            name,
            declaration,
            type_id,
            Vec::new(),
            false,
        );
        let parameter = model.variables.get_mut(&VarName::new(name)).unwrap();
        parameter.variability = Variability::Parameter(Default::default());
        parameter.binding = Some(Expression::Literal {
            value: Literal::Real(value),
            span: source.span(&format!("{value}"), 0),
        });
    }
    // The two activations share one span, exactly as two flattened instances of
    // one class do; only the resolved threshold operand tells them apart.
    let shared_span = source.span("time > a", 0);
    for (target, threshold) in [("y", "a"), ("z", "b")] {
        push_shared_span_activation(&mut model, &source, shared_span, target, threshold);
    }

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        let mut instants = (0..view.time_event_count())
            .map(|index| {
                view.time_event(view.time_event_id(index).unwrap())
                    .unwrap()
                    .instant()
                    .to_f64()
            })
            .collect::<Vec<_>>();
        instants.sort_by(f64::total_cmp);
        assert_eq!(
            instants.len(),
            2,
            "one span shared by two occurrences still owns two instants"
        );
        assert!(
            (instants[0] - 0.33).abs() < 1.0e-12 && (instants[1] - 0.73).abs() < 1.0e-12,
            "expected the instants 0.33 and 0.73, got {instants:?}"
        );
        assert_eq!(
            view.root_count(),
            0,
            "each occurrence's crossing is suppressed by its own instant, not by another's"
        );
    });
}

/// An instant at the start of the interval is not a schedulable stop.
///
/// `when time > 0` has nothing to schedule: the only instant the relation could
/// name is the start, and MLS §8.5 defines an event as the instant a relation
/// *changes* value, which the start is not — initialization has already fixed
/// every buffered value there. Scheduling a stop at the start is worse than not
/// scheduling one, because `time > 0` reads false at that stop and no later
/// event exists to retry it. The crossing owner is the right one: it locates the
/// change at the start and applies it at the right limit, where OpenModelica
/// applies it too.
#[test]
fn an_instant_at_the_start_is_left_to_its_crossing() {
    for (text, op) in [("time > 0", OpBinary::Gt), ("time >= 0", OpBinary::Ge)] {
        let text_owned =
            format!("model M discrete Real y; equation when {text} then y = 1.0; end when; end M;");
        let source = TestSource::new(&text_owned);
        let condition = time_relation(&source, text, op, 0.0, "0");
        let span = source.span(text, 0);
        let model = when_activation_model(&source, condition, span);

        let dae = construct(&model, source.map).unwrap();
        dae.inspect(|view| {
            assert_eq!(
                view.time_event_count(),
                0,
                "`when {text}` must schedule no stop at the start instant"
            );
            assert_eq!(
                view.root_count(),
                1,
                "`when {text}` keeps the crossing that owns the change at the start"
            );
        });
    }
}

/// Build `when sample(<start>, 0.25) then y = 1.0; end when;` over a model that
/// declares `parameter Real t0(fixed = false)` settled by an initial equation.
///
/// `t0` is the shape MSL is made of: `Modelica.Blocks.Math.Mean` and
/// `Modelica.Blocks.Math.SignalExtrema` both declare it and both write
/// `sample(t0 + P, P)`.
fn deferred_start_sample_model(
    source: &TestSource,
    settling: Expression,
    sample_text: &str,
) -> flat::Model {
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        source,
        "y",
        "discrete Real y",
        8,
        Vec::new(),
        false,
    );
    model
        .variables
        .get_mut(&VarName::new("y"))
        .unwrap()
        .variability = Variability::Discrete(Default::default());
    add_primitive_variable(
        &mut model,
        source,
        "t0",
        "parameter Real t0(fixed = false)",
        9,
        Vec::new(),
        false,
    );
    let t0 = model.variables.get_mut(&VarName::new("t0")).unwrap();
    t0.variability = Variability::Parameter(Default::default());
    // The declaration defers the value (MLS §4.4) and carries no binding, so
    // the initial section is its only determining owner (MLS §8.6).
    t0.fixed = Some(false);
    t0.binding = None;

    let settling_span = source.span("t0", 0);
    model.initial_equations.push(flat::Equation {
        residual: Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(variable_reference(source, "t0", "t0", 0, Vec::new())),
            rhs: Box::new(settling),
            span: settling_span,
        },
        span: settling_span,
        origin: flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
        scalar_count: 1,
    });

    let sample_span = source.span(sample_text, 0);
    let condition = Expression::BuiltinCall {
        function: BuiltinFunction::Sample,
        args: vec![
            variable_reference(source, "t0", "t0", 1, Vec::new()),
            Expression::Literal {
                value: Literal::Real(0.25),
                span: source.span("0.25", 0),
            },
        ],
        span: sample_span,
    };
    let mut branch = flat::WhenBranch::new(condition, sample_span);
    branch.add_equation(flat::WhenEquation::assign(
        VarName::new("y"),
        Expression::Literal {
            value: Literal::Real(1.0),
            span: source.span("1.0", 0),
        },
        source.span("y = 1.0", 0),
        "when assignment",
    ));
    model
        .when_chains
        .push(flat::WhenChain::new(branch, sample_span));
    model
}

/// A `sample` start settled by the start instant is refused by that name.
///
/// MLS §3.7.5 requires `start` to be a parameter expression, and a `fixed =
/// false` parameter *is* one — so the model is legal and the refusal is
/// rumoca's, not the language's. What makes it unrepresentable here is which
/// number settles it: MLS §8.6 evaluates the initial section at the
/// initialization instant, so `t0 = time` is the simulation start instant,
/// while a [`rumoca_core::ClockLattice`] phase is an absolute translation-time
/// rational. Naming that is the whole point of this test: the refusal used to
/// read `unknown variable: t0`, which describes a name-resolution defect that
/// does not exist and sends a reader hunting for one.
#[test]
fn a_sample_start_settled_by_the_start_instant_is_refused_by_that_name() {
    let text = "model M discrete Real y; parameter Real t0(fixed = false); \
                initial equation t0 = time; equation when sample(t0, 0.25) then y = 1.0; \
                end when; end M;";
    let source = TestSource::new(text);
    let settling = Expression::VarRef {
        name: Reference::new("time"),
        subscripts: Vec::new(),
        span: source.span("time", 0),
    };
    let model = deferred_start_sample_model(&source, settling, "sample(t0, 0.25)");

    let error = construct(&model, source.map).expect_err("a start with no translation-time value");
    let message = error.to_string();
    assert!(
        message.contains(
            "`t0` is a `fixed = false` parameter determined by the simulation start instant"
        ),
        "the refusal must name the deferred parameter and what settles it, got: {message}"
    );
    assert!(
        !message.contains("unknown variable"),
        "a declared parameter must never be reported as an unresolved name, got: {message}"
    );
}

/// The same declaration settled without a syntactic `time` read falls to the
/// initialization-system tier, not the start-instant one.
///
/// Both are `fixed = false` parameters with no binding, so a refusal keyed on
/// the declaration alone would give them one message; the determining operand
/// is what tells them apart. This pins the *floor* tier, and the floor is all
/// it pins: `InitializationSystem` says the parameter has no translation-time
/// value, and deliberately does not claim the value is independent of the
/// simulation start instant. A start-instant dependency reached indirectly,
/// through an initial algorithm, or through a residual that is not a top-level
/// subtraction on the target also lands here — see `deferred_parameter_source`
/// for the exact shape the stronger tier proves. Here the determining operand
/// is the literal `0.5`, which genuinely needs no start instant.
#[test]
fn a_sample_start_settled_without_time_names_the_initialization_system() {
    let text = "model M discrete Real y; parameter Real t0(fixed = false); \
                initial equation t0 = 0.5; equation when sample(t0, 0.25) then y = 1.0; \
                end when; end M;";
    let source = TestSource::new(text);
    let settling = Expression::Literal {
        value: Literal::Real(0.5),
        span: source.span("0.5", 0),
    };
    let model = deferred_start_sample_model(&source, settling, "sample(t0, 0.25)");

    let error = construct(&model, source.map).expect_err("a start the fold cannot settle");
    let message = error.to_string();
    assert!(
        message.contains(
            "`t0` is a `fixed = false` parameter determined by the initialization system"
        ),
        "the refusal must name the initialization system as the owner, got: {message}"
    );
}
