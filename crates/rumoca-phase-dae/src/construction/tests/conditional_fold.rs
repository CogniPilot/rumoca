//! MLS §3.6.5 conditional-expression folding during canonical DAE construction.
//!
//! MLS §11.5 evaluates the branch conditions of a conditional in order and
//! yields the value of the first `true` one, or the else value when none is.
//! When this scope proves a condition constant, the arms MLS §11.5 would never
//! reach carry no value the program observes, and function-shape discovery has
//! already declined to certify their calls (`discover_conditional_calls` prunes
//! the same arms). The construction seam must prune in step: a proven-dead arm
//! is never lowered, so it never asks the checked constructor for a certificate
//! the pruned discovery never minted. An unproven condition keeps both arms
//! exactly as written.
//!
//! `searchLike` is the probe on both sides of the fold. Its body is a `while`
//! loop this compiler does not reduce, so building its call is rejected
//! wherever the call is actually reached: a proven-dead arm that skipped it
//! constructs, and a runtime arm that keeps it does not. That is the exact
//! shape of `DeadArmReduction` and of the MultiBody `CombiTimeTable`
//! `if tableOnFile then Strings.findLast(...) else false` guard.

use rumoca_core::Reference;

use super::super::*;
use super::support::*;

/// A scalar-Integer function whose `while`-loop body this compiler cannot
/// reduce; its call is certifiable but never constructible.
fn search_like_function(source: &TestSource) -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new("searchLike", source.span("searchLike", 0));
    function.add_input(integer_function_param(
        "n",
        Vec::new(),
        source.span("input Integer n", 0),
    ));
    function.add_output(integer_function_param(
        "p",
        Vec::new(),
        source.span("output Integer p", 0),
    ));
    function.add_local(integer_function_param(
        "i",
        Vec::new(),
        source.span("Integer i", 0),
    ));
    let body_span = source.span("while", 0);
    let integer_ref = |name: &str| Expression::VarRef {
        name: Reference::new(name),
        subscripts: Vec::new(),
        span: body_span,
    };
    let integer_literal = |value: i64| Expression::Literal {
        value: Literal::Integer(value),
        span: body_span,
    };
    let assign = |name: &str, value: Expression| rumoca_core::Statement::Assignment {
        comp: test_component_reference(name, body_span),
        value,
        span: body_span,
    };
    function.body = vec![
        assign("p", integer_literal(0)),
        assign("n", integer_ref("n")),
        rumoca_core::Statement::While {
            block: rumoca_core::StatementBlock {
                cond: Expression::Binary {
                    op: OpBinary::Ge,
                    lhs: Box::new(integer_ref("i")),
                    rhs: Box::new(integer_literal(1)),
                    span: body_span,
                },
                stmts: vec![
                    rumoca_core::Statement::If {
                        cond_blocks: vec![rumoca_core::StatementBlock {
                            cond: Expression::Binary {
                                op: OpBinary::Eq,
                                lhs: Box::new(integer_ref("i")),
                                rhs: Box::new(integer_literal(3)),
                                span: body_span,
                            },
                            stmts: vec![assign("p", integer_ref("i"))],
                        }],
                        else_block: None,
                        span: body_span,
                    },
                    assign(
                        "i",
                        Expression::Binary {
                            op: OpBinary::Sub,
                            lhs: Box::new(integer_ref("i")),
                            rhs: Box::new(integer_literal(1)),
                            span: body_span,
                        },
                    ),
                ],
            },
            span: body_span,
        },
    ];
    function
}

fn search_like_call(source: &TestSource) -> Expression {
    Expression::FunctionCall {
        name: Reference::new("searchLike"),
        args: vec![Expression::Literal {
            value: Literal::Integer(10),
            span: source.span("10", 0),
        }],
        is_constructor: false,
        span: source.span("searchLike(10)", 0),
    }
}

fn real_literal(source: &TestSource, value: f64, needle: &str) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: source.span(needle, 0),
    }
}

fn real_identity_call(source: &TestSource) -> Expression {
    Expression::FunctionCall {
        name: Reference::new("f"),
        args: vec![real_literal(source, 2.0, "2.0")],
        is_constructor: false,
        span: source.span("f(2.0)", 0),
    }
}

/// `f(u) = u`, a scalar-Real identity whose call is always constructible.
fn real_identity_function(source: &TestSource) -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new("f", source.span("function f", 0));
    function.add_input(real_function_param(
        "u",
        Vec::new(),
        source.span("input Real u", 0),
    ));
    function.add_output(real_function_param(
        "y",
        Vec::new(),
        source.span("output Real y", 0),
    ));
    let assignment_span = source.span("y := u", 0);
    function.body.push(rumoca_core::Statement::Assignment {
        comp: test_component_reference("y", assignment_span),
        value: Expression::VarRef {
            name: Reference::new("u"),
            subscripts: Vec::new(),
            span: assignment_span,
        },
        span: assignment_span,
    });
    function
}

/// Add `0 = x - (conditional)` as the model's only equation.
fn add_conditional_equation(
    model: &mut flat::Model,
    source: &TestSource,
    condition: Expression,
    taken: Expression,
    fallback: Expression,
) {
    let conditional = Expression::If {
        branches: vec![(condition, taken)],
        else_branch: Box::new(fallback),
        span: source.span("if", 0),
    };
    let equation_span = source.span("x -", 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(Expression::VarRef {
                name: test_reference("x"),
                subscripts: Vec::new(),
                span: source.span("x", 0),
            }),
            rhs: Box::new(conditional),
            span: equation_span,
        },
        equation_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
}

/// A branch proven `false` is dropped before it is built, so the unreducible
/// call inside it never reaches the checked constructor. Before the fold the
/// dead call was built and rejected with ED020 "function call `searchLike` has
/// no checked call-shape certificate", because discovery had already pruned the
/// same arm and minted no certificate for it.
#[test]
fn a_proven_false_guard_prunes_the_dead_call_arm() {
    let source = TestSource::new(
        "function searchLike input Integer n; output Integer p; protected Integer i; \
         algorithm while end searchLike; \
         model M Real x; equation 0 = x - (if false then searchLike(10) else 1.0); end M;",
    );
    let mut model = test_model();
    model.add_function(search_like_function(&source));
    model.is_partial = true;
    add_primitive_variable(&mut model, &source, "x", "Real x", 7, Vec::new(), false);
    let condition = Expression::Literal {
        value: Literal::Boolean(false),
        span: source.span("false", 0),
    };
    add_conditional_equation(
        &mut model,
        &source,
        condition,
        search_like_call(&source),
        real_literal(&source, 1.0, "1.0"),
    );

    construct(&model, source.map)
        .expect("a proven-false guard prunes the dead call arm before construction");
}

/// A branch proven `true` is the whole result and the trailing arms are dead.
/// The else arm calls the unreducible `searchLike`: selecting the taken arm
/// means it is never built, so folding to the else arm instead (an over-prune)
/// would reject the model. Construction succeeding proves the proven-`true` arm
/// was selected and the else arm pruned.
#[test]
fn a_proven_true_guard_selects_the_call_arm_and_prunes_the_else() {
    let source = TestSource::new(
        "function f input Real u; output Real y; algorithm y := u; end f; \
         function searchLike input Integer n; output Integer p; protected Integer i; \
         algorithm while end searchLike; \
         model M Real x; equation 0 = x - (if true then f(2.0) else searchLike(10)); end M;",
    );
    let mut model = test_model();
    model.add_function(real_identity_function(&source));
    model.add_function(search_like_function(&source));
    model.is_partial = true;
    add_primitive_variable(&mut model, &source, "x", "Real x", 7, Vec::new(), false);
    let condition = Expression::Literal {
        value: Literal::Boolean(true),
        span: source.span("true", 0),
    };
    add_conditional_equation(
        &mut model,
        &source,
        condition,
        real_identity_call(&source),
        search_like_call(&source),
    );

    construct(&model, source.map)
        .expect("a proven-true guard selects the call arm and prunes the else");
}

/// `size(array, 1) == 1`, the MLS §10.3.1 dimension relation the MSL
/// `CombiTimeTable`/`CombiTable` offset idiom guards its broadcast branch with:
/// `if size(offset, 1) == 1 then ones(nout) * offset[1] else offset`.
fn size_first_dimension_equals_one(source: &TestSource, array: &str) -> Expression {
    Expression::Binary {
        op: OpBinary::Eq,
        lhs: Box::new(Expression::BuiltinCall {
            function: BuiltinFunction::Size,
            args: vec![
                Expression::VarRef {
                    name: test_reference(array),
                    subscripts: Vec::new(),
                    span: source.span(array, 0),
                },
                Expression::Literal {
                    value: Literal::Integer(1),
                    span: source.span("1", 0),
                },
            ],
            span: source.span("size", 0),
        }),
        rhs: Box::new(Expression::Literal {
            value: Literal::Integer(1),
            span: source.span("1", 0),
        }),
        span: source.span("==", 0),
    }
}

/// A guard that is a constant relation over a statically-known dimension folds:
/// `size(v, 1)` reads `v`'s proven extent (MLS §10.3.1), so `size(v, 1) == 1`
/// over a length-one `v` is a compile-time `true` (MLS §3.6.5). The taken arm is
/// `f(2.0)`, constructible, and the else arm calls the unreducible `searchLike`;
/// construction succeeding proves the size relation selected the taken arm and
/// pruned the else, exactly as the `CombiTimeTable` offset broadcast requires.
#[test]
fn a_size_guard_over_a_known_dimension_selects_the_taken_arm() {
    let source = TestSource::new(
        "function f input Real u; output Real y; algorithm y := u; end f; \
         function searchLike input Integer n; output Integer p; protected Integer i; \
         algorithm while end searchLike; \
         model M Real x; parameter Real v[1]; \
         equation 0 = x - (if size(v, 1) == 1 then f(2.0) else searchLike(10)); end M;",
    );
    let mut model = test_model();
    model.add_function(real_identity_function(&source));
    model.add_function(search_like_function(&source));
    model.is_partial = true;
    add_primitive_variable(&mut model, &source, "x", "Real x", 7, Vec::new(), false);
    add_primitive_variable(&mut model, &source, "v", "Real v[1]", 8, vec![1], false);
    add_conditional_equation(
        &mut model,
        &source,
        size_first_dimension_equals_one(&source, "v"),
        real_identity_call(&source),
        search_like_call(&source),
    );

    construct(&model, source.map)
        .expect("a size relation over a known dimension selects the taken arm");
}

/// A size guard proven `false` selects the other arm. `size(v, 1) == 1` over a
/// length-two `v` is a compile-time `false` (MLS §10.3.1, §3.6.5), so the taken
/// arm's unreducible `searchLike` is never built and the else literal is the
/// whole result. Construction succeeding proves the proven-`false` size relation
/// pruned the taken arm rather than leaving both to be shape-checked.
#[test]
fn a_proven_false_size_guard_selects_the_other_arm() {
    let source = TestSource::new(
        "function searchLike input Integer n; output Integer p; protected Integer i; \
         algorithm while end searchLike; \
         model M Real x; parameter Real v[2]; \
         equation 0 = x - (if size(v, 1) == 1 then searchLike(10) else 1.0); end M;",
    );
    let mut model = test_model();
    model.add_function(search_like_function(&source));
    model.is_partial = true;
    add_primitive_variable(&mut model, &source, "x", "Real x", 7, Vec::new(), false);
    add_primitive_variable(&mut model, &source, "v", "Real v[2]", 8, vec![2], false);
    add_conditional_equation(
        &mut model,
        &source,
        size_first_dimension_equals_one(&source, "v"),
        search_like_call(&source),
        real_literal(&source, 1.0, "1.0"),
    );

    construct(&model, source.map)
        .expect("a proven-false size relation prunes the taken arm and selects the other");
}

/// An unproven condition keeps both arms. The runtime arm calls the unreducible
/// `searchLike`, so construction still reaches it and rejects the model; the
/// fold must not silence a runtime branch by pruning it. `b` is a discrete
/// Boolean coordinate this scope cannot settle, which is exactly the runtime
/// guard the proven-constant rule does not cover.
#[test]
fn an_unproven_guard_still_builds_both_arms() {
    let source = TestSource::new(
        "function searchLike input Integer n; output Integer p; protected Integer i; \
         algorithm while end searchLike; \
         model M Real x; Boolean b; equation 0 = x - (if b then searchLike(10) else 1.0); end M;",
    );
    let mut model = test_model();
    model.add_function(search_like_function(&source));
    model.is_partial = true;
    add_primitive_variable(&mut model, &source, "x", "Real x", 7, Vec::new(), false);
    add_primitive_variable(&mut model, &source, "b", "Boolean b", 8, Vec::new(), true);
    let condition = Expression::VarRef {
        name: test_reference("b"),
        subscripts: Vec::new(),
        span: source.span("b", 0),
    };
    add_conditional_equation(
        &mut model,
        &source,
        condition,
        search_like_call(&source),
        real_literal(&source, 1.0, "1.0"),
    );

    let error = construct(&model, source.map)
        .expect_err("an unproven guard keeps the runtime branch and reaches its call");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("loop reduction"),
        "the unproven arm must still reach the unreducible call: {rendered}"
    );
}
