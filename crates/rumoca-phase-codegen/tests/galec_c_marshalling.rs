//! Executable evidence for the call-boundary marshalling rewrite.
//!
//! The emitter stages an array actual into a temporary of the caller's, and
//! copies an array result out of the callee's region into another one. When the
//! staged value is already an aggregate the caller holds, and when the result
//! has exactly one consumer of the same shape, both temporaries are pure
//! traffic and the emitter names the aggregate directly instead.
//!
//! What is tested here is the thing the proof exists to protect: the generated
//! C computes the same numbers. Every negative control below is built so that
//! forwarding it would produce a *different answer*, not merely different text,
//! and the fixture is run to show which answer comes out.

use std::fs;
use std::process::Command;

use rumoca_ir_galec::ast as galec;
use rumoca_ir_galec::package::CheckedAlgorithmBlock;
use rumoca_phase_codegen::{render_checked_algorithm_block_template_with_artifact, templates};
use serde_json::json;
use tempfile::tempdir;

const MODEL: &str = "Marshalling";
const EXTENT: i64 = 4;

/// The strict preflight the target documents (SPEC_0034 GAL-029/030).
const STRICT: [&str; 14] = [
    "-O2",
    "-std=c99",
    "-pedantic",
    "-Wall",
    "-Wextra",
    "-Wconversion",
    "-Wsign-conversion",
    "-Wshadow",
    "-Wundef",
    "-Wcast-qual",
    "-Wstrict-prototypes",
    "-Wmissing-prototypes",
    "-Werror",
    "-lm",
];

fn array(name: &str) -> galec::VariableDeclaration {
    let mut declaration =
        galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident(name));
    declaration.dimensions = vec![galec::Dimension::Expr(galec::Expression::Integer(EXTENT))];
    declaration
}

fn matrix(name: &str) -> galec::VariableDeclaration {
    let mut declaration =
        galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident(name));
    declaration.dimensions = vec![
        galec::Dimension::Expr(galec::Expression::Integer(2)),
        galec::Dimension::Expr(galec::Expression::Integer(EXTENT)),
    ];
    declaration
}

fn interface(kind: galec::InterfaceKind, name: &str) -> galec::InterfaceVariable {
    galec::InterfaceVariable {
        kind,
        decl: array(name),
        start: None,
    }
}

fn parameter(direction: galec::Direction, name: &str) -> galec::Parameter {
    galec::Parameter {
        direction,
        decl: array(name),
    }
}

fn local_ref(name: &str) -> galec::Reference {
    galec::Reference::local(galec::Name::ident(name))
}

/// `name[subscripts]`, for the fully subscripted element references a GALEC
/// loop body spells (a partial subscripting is not representable).
fn element_ref(name: &str, subscripts: Vec<galec::Expression>) -> galec::Reference {
    galec::Reference::Local(galec::RefPart {
        name: galec::Name::ident(name),
        subscripts,
        span: rumoca_core::Span::DUMMY,
    })
}

fn element(name: &str, subscripts: Vec<galec::Expression>) -> galec::Expression {
    galec::Expression::Ref(element_ref(name, subscripts))
}

fn local(name: &str) -> galec::Expression {
    galec::Expression::Ref(local_ref(name))
}

fn iterator(name: &str) -> galec::Expression {
    galec::Expression::Ref(local_ref(name))
}

fn state(name: &str) -> galec::Expression {
    galec::Expression::Ref(galec::Reference::state(galec::Name::ident(name)))
}

fn state_element(name: &str, subscripts: Vec<galec::Expression>) -> galec::Expression {
    galec::Expression::Ref(galec::Reference::State(vec![galec::RefPart {
        name: galec::Name::ident(name),
        subscripts,
        span: rumoca_core::Span::DUMMY,
    }]))
}

fn for_loop(
    name: &str,
    stop: i64,
    body: Vec<galec::Spanned<galec::Statement>>,
) -> galec::Spanned<galec::Statement> {
    galec::Spanned::dummy(galec::Statement::For(galec::ForLoop {
        iterator: Some(galec::Name::ident(name)),
        start: galec::Expression::Integer(1),
        step: None,
        stop: galec::Expression::Integer(stop),
        body,
    }))
}

fn scaled(value: galec::Expression, factor: f64) -> galec::Expression {
    galec::Expression::binary(galec::BinaryOp::Mul, value, galec::Expression::Real(factor))
}

fn sum(lhs: galec::Expression, rhs: galec::Expression) -> galec::Expression {
    galec::Expression::binary(galec::BinaryOp::Add, lhs, rhs)
}

fn assign(target: galec::Reference, value: galec::Expression) -> galec::Spanned<galec::Statement> {
    galec::Spanned::dummy(galec::Statement::Assignment { target, value })
}

fn call(
    targets: Vec<galec::Reference>,
    callee: &str,
    argument: galec::Expression,
) -> galec::Spanned<galec::Statement> {
    galec::Spanned::dummy(galec::Statement::MultiAssignment {
        targets,
        call: galec::FunctionCall {
            function: galec::Name::ident(callee),
            arguments: vec![argument],
        },
    })
}

/// `scale3(u) => y : y := 3 * u`.
fn scale3() -> galec::UserFunction {
    galec::UserFunction {
        kind: galec::FunctionKind::Stateless,
        name: galec::Name::ident("scale3"),
        signals: Vec::new(),
        parameters: vec![
            parameter(galec::Direction::Input, "u"),
            parameter(galec::Direction::Output, "y"),
        ],
        locals: Vec::new(),
        statements: vec![assign(local_ref("y"), scaled(local("u"), 3.0))],
        span: rumoca_core::Span::DUMMY,
    }
}

/// `pair(u) => (p, q) : p := 3 * u; q := 5 * u`.
///
/// Two results, so a rewrite that let one result slot write another's object
/// changes which of the two survives.
fn pair() -> galec::UserFunction {
    galec::UserFunction {
        kind: galec::FunctionKind::Stateless,
        name: galec::Name::ident("pair"),
        signals: Vec::new(),
        parameters: vec![
            parameter(galec::Direction::Input, "u"),
            parameter(galec::Direction::Output, "p"),
            parameter(galec::Direction::Output, "q"),
        ],
        locals: Vec::new(),
        statements: vec![
            assign(local_ref("p"), scaled(local("u"), 3.0)),
            assign(local_ref("q"), scaled(local("u"), 5.0)),
        ],
        span: rumoca_core::Span::DUMMY,
    }
}

/// A block whose `DoStep` is the given body, over the given locals.
fn block_of(
    functions: Vec<galec::UserFunction>,
    locals: Vec<galec::VariableDeclaration>,
    statements: Vec<galec::Spanned<galec::Statement>>,
) -> CheckedAlgorithmBlock {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![
        interface(galec::InterfaceKind::Input, "a"),
        interface(galec::InterfaceKind::Output, "result"),
    ];
    block.protected_functions = functions;
    block.do_step.locals = locals;
    block.do_step.statements = statements;
    CheckedAlgorithmBlock::construct(block).expect("marshalling fixture must be valid GALEC")
}

fn render(block: &CheckedAlgorithmBlock, path: &str) -> String {
    render_from("embedded-c-galec", block, path)
}

fn render_from(target: &str, block: &CheckedAlgorithmBlock, path: &str) -> String {
    let template = templates::builtin_template_source(target, path).expect("built-in template");
    render_checked_algorithm_block_template_with_artifact(block, &json!({}), template, MODEL)
        .expect("marshalling fixture must render")
}

/// Compile a generated model beside the shared kernel library and a driver that
/// asserts `result == factor * a`, run it, and return the driver's exit status
/// and output.
fn run_expecting(block: &CheckedAlgorithmBlock, factor: f64) -> (bool, String) {
    let directory = tempdir().expect("temporary generated-C directory");
    let base = directory.path();
    fs::write(
        base.join(format!("{MODEL}.h")),
        render(block, "model.h.jinja"),
    )
    .expect("write generated header");
    fs::write(
        base.join(format!("{MODEL}.c")),
        render(block, "model.c.jinja"),
    )
    .expect("write generated source");
    fs::write(
        base.join("rumoca_galec_kernels.h"),
        render(block, "kernels.h.jinja"),
    )
    .expect("write kernel header");
    fs::write(
        base.join("rumoca_galec_kernels.c"),
        render(block, "kernels.c.jinja"),
    )
    .expect("write kernel library");
    fs::write(base.join("main.c"), driver(factor)).expect("write generated-C driver");

    let executable = base.join("harness");
    let compile = Command::new("cc")
        .args(STRICT)
        .arg(base.join("main.c"))
        .arg(base.join(format!("{MODEL}.c")))
        .arg(base.join("rumoca_galec_kernels.c"))
        .arg("-o")
        .arg(&executable)
        .output()
        .expect("run C compiler");
    assert!(
        compile.status.success(),
        "strict generated-C compile failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&executable)
        .output()
        .expect("execute generated-C harness");
    (
        run.status.success(),
        String::from_utf8_lossy(&run.stdout).into_owned(),
    )
}

fn driver(factor: f64) -> String {
    format!(
        "\
#include <stdio.h>
#include \"{MODEL}.h\"

int main(void) {{
    {MODEL}State model = {{0}};
    int32_t i;
    for (i = 0; i < {EXTENT}; ++i) {{
        model.a[i] = (float)(i + 1);
    }}
    {MODEL}_dostep(&model);
    for (i = 0; i < {EXTENT}; ++i) {{
        const float expected = {factor:?}f * (float)(i + 1);
        if (model.result[i] != expected) {{
            printf(\"result[%d] = %f, expected %f\\n\",
                   (int)i, (double)model.result[i], (double)expected);
            return i + 1;
        }}
    }}
    return 0;
}}
"
    )
}

/// The staged actual and the read-back temporary of one call, both provably
/// redundant.
///
/// ```text
/// source := 2*a; staged := source; (produced) := scale3(staged);
/// kept := produced; result := kept + source
/// ```
///
/// `staged` is a whole-array copy of `source` and is read nowhere else, so the
/// call names `source`. `produced` is read only by `kept := produced`, so the
/// read-back writes `kept`. The answer is `3*(2*a) + 2*a = 8*a` either way.
fn forwardable() -> CheckedAlgorithmBlock {
    block_of(
        vec![scale3()],
        vec![
            array("source"),
            array("staged"),
            array("produced"),
            array("kept"),
        ],
        vec![
            assign(local_ref("source"), scaled(state("a"), 2.0)),
            assign(local_ref("staged"), local("source")),
            call(vec![local_ref("produced")], "scale3", local("staged")),
            assign(local_ref("kept"), local("produced")),
            assign(
                galec::Reference::state(galec::Name::ident("result")),
                sum(local("kept"), local("source")),
            ),
        ],
    )
}

#[test]
fn a_staged_actual_and_a_single_use_result_both_lose_their_copy() {
    let block = forwardable();
    let source = render(&block, "model.c.jinja");
    let header = render(&block, "model.h.jinja");

    assert!(
        source.contains("scale3(\n        self,\n        ctx->source);"),
        "the call must name the aggregate the staging copy read:\n{source}"
    );
    assert!(
        source.contains("rumoca_galec_copy_real(INT32_C(4), ctx->kept,"),
        "the read-back must write the destination directly:\n{source}"
    );
    for retired in ["staged", "produced"] {
        assert!(
            !source.contains(retired),
            "`{retired}` is pure traffic and must not survive:\n{source}"
        );
        assert!(
            !header.contains(retired),
            "`{retired}` must leave the working-memory region:\n{header}"
        );
    }
}

#[test]
fn forwarding_keeps_the_block_computing_the_same_values() {
    let (passed, output) = run_expecting(&forwardable(), 8.0);
    assert!(passed, "forwarding changed the block's values:\n{output}");
}

/// The checked Algorithm Code is what the eFMI conformance surface reads, and
/// it is not a C target: it must still print the call and both copies exactly
/// as the block spells them, with every temporary declared and written.
///
/// This is the invariant the rewrite is carried in a sibling slot to protect.
/// Writing it back over the projected statement instead would leave the `.alg`
/// naming an aggregate at the call and then copying out of a temporary nothing
/// had written.
#[test]
fn the_checked_algorithm_code_still_spells_every_marshalling_copy() {
    let algorithm = render_from("galec", &forwardable(), "model.alg.jinja");
    assert!(
        algorithm.contains("Real staged[4];") && algorithm.contains("Real produced[4];"),
        "the checked Algorithm Code must still declare both temporaries:\n{algorithm}"
    );
    assert!(
        algorithm.contains("staged := source;"),
        "the checked Algorithm Code must still spell the staging copy:\n{algorithm}"
    );
    assert!(
        algorithm.contains("kept := produced;"),
        "the checked Algorithm Code must still spell the read-back copy:\n{algorithm}"
    );
    assert!(
        algorithm.contains("(produced) := scale3(staged);"),
        "the checked Algorithm Code must still call through the temporaries:\n{algorithm}"
    );
}

/// A result with two consumers keeps its read-back.
///
/// ```text
/// source := 2*a; (produced) := scale3(source);
/// kept := produced; result := kept + produced
/// ```
///
/// Writing `kept` in place of `produced` would leave the second consumer
/// reading a buffer the call never wrote, so `produced` stays. The answer is
/// `3*(2*a) + 3*(2*a) = 12*a`.
fn result_consumed_twice() -> CheckedAlgorithmBlock {
    block_of(
        vec![scale3()],
        vec![array("source"), array("produced"), array("kept")],
        vec![
            assign(local_ref("source"), scaled(state("a"), 2.0)),
            call(vec![local_ref("produced")], "scale3", local("source")),
            assign(local_ref("kept"), local("produced")),
            assign(
                galec::Reference::state(galec::Name::ident("result")),
                sum(local("kept"), local("produced")),
            ),
        ],
    )
}

#[test]
fn a_result_consumed_twice_keeps_its_read_back() {
    let block = result_consumed_twice();
    let source = render(&block, "model.c.jinja");
    assert!(
        source.contains("rumoca_galec_copy_real(INT32_C(4), ctx->produced,"),
        "a twice-read result must still be read back into its own slot:\n{source}"
    );
    assert!(
        source.contains("rumoca_galec_copy_real(INT32_C(4), ctx->kept, ctx->produced)"),
        "the copy out of a twice-read result must survive:\n{source}"
    );
    let (passed, output) = run_expecting(&block, 12.0);
    assert!(passed, "the refused rewrite changed the values:\n{output}");
}

/// An actual staged one row at a time out of a wider object keeps its copy.
///
/// ```text
/// for j in 1:4 loop wide[1][j] := 2*a[j]; wide[2][j] := 5*a[j]; end for;
/// for j in 1:4 loop staged[j] := wide[2][j]; end for;
/// (produced) := scale3(staged); result := produced
/// ```
///
/// The staging loop is a whole traversal of `staged`, but its source elements
/// belong to a `[2, 4]` object. A partial subscripting of that object is not
/// even representable in GALEC, so there is no reference the call could be
/// given in place of the temporary, and the shape test refuses the copy rather
/// than inventing one. The answer is `3*(5*a) = 15*a`.
fn shape_mismatched_actual() -> CheckedAlgorithmBlock {
    block_of(
        vec![scale3()],
        vec![matrix("wide"), array("staged"), array("produced")],
        vec![
            for_loop(
                "j",
                EXTENT,
                vec![
                    assign(
                        element_ref("wide", vec![galec::Expression::Integer(1), iterator("j")]),
                        scaled(state_element("a", vec![iterator("j")]), 2.0),
                    ),
                    assign(
                        element_ref("wide", vec![galec::Expression::Integer(2), iterator("j")]),
                        scaled(state_element("a", vec![iterator("j")]), 5.0),
                    ),
                ],
            ),
            for_loop(
                "j",
                EXTENT,
                vec![assign(
                    element_ref("staged", vec![iterator("j")]),
                    element("wide", vec![galec::Expression::Integer(2), iterator("j")]),
                )],
            ),
            call(vec![local_ref("produced")], "scale3", local("staged")),
            assign(
                galec::Reference::state(galec::Name::ident("result")),
                local("produced"),
            ),
        ],
    )
}

#[test]
fn a_shape_mismatched_actual_keeps_its_copy() {
    let block = shape_mismatched_actual();
    let source = render(&block, "model.c.jinja");
    assert!(
        source.contains("ctx->staged[j - 1] = ctx->wide[1][j - 1];"),
        "a staging traversal out of a wider object must keep its copy:\n{source}"
    );
    assert!(
        source.contains("ctx->staged);"),
        "the call must still pass the staged temporary:\n{source}"
    );
    let (passed, output) = run_expecting(&block, 15.0);
    assert!(passed, "the refused rewrite changed the values:\n{output}");
}

/// An actual whose source is rewritten before the call keeps its copy.
///
/// ```text
/// source := 2*a; staged := source; source := 5*a;
/// (produced) := scale3(staged); result := produced + source
/// ```
///
/// The staged value is the *old* `source`. Naming `source` at the call would
/// read the new one, so the answer would be `3*(5*a) + 5*a = 20*a` instead of
/// `3*(2*a) + 5*a = 11*a`.
fn source_rewritten_before_the_call() -> CheckedAlgorithmBlock {
    block_of(
        vec![scale3()],
        vec![array("source"), array("staged"), array("produced")],
        vec![
            assign(local_ref("source"), scaled(state("a"), 2.0)),
            assign(local_ref("staged"), local("source")),
            assign(local_ref("source"), scaled(state("a"), 5.0)),
            call(vec![local_ref("produced")], "scale3", local("staged")),
            assign(
                galec::Reference::state(galec::Name::ident("result")),
                sum(local("produced"), local("source")),
            ),
        ],
    )
}

#[test]
fn an_actual_whose_source_is_rewritten_keeps_its_copy() {
    let block = source_rewritten_before_the_call();
    let source = render(&block, "model.c.jinja");
    assert!(
        source.contains("rumoca_galec_copy_real(INT32_C(4), ctx->staged, ctx->source)"),
        "a staging copy whose source is rewritten before the call must survive:\n{source}"
    );
    let (passed, output) = run_expecting(&block, 11.0);
    assert!(passed, "the refused rewrite changed the values:\n{output}");
}

/// A result whose destination is another result slot of the same call keeps its
/// read-back.
///
/// ```text
/// source := a; (produced, kept) := pair(source); kept := produced; result := kept
/// ```
///
/// The read-backs run in signature order, so `kept` is written from `pair.q`
/// and only then overwritten by the copy from `produced`. Writing `produced`'s
/// read-back into `kept` would put it *before* `kept`'s own read-back, and
/// `pair.q` would win: `5*a` instead of `3*a`.
///
/// This is the aliasing obligation in the only form this call protocol can
/// present it. Two actuals that alias cannot go wrong here, because no actual
/// of the emitted call is writable: the callee's formals are its inputs, and
/// checked construction proves an input is never written. Two *results* that
/// alias can, and this is that pair.
fn destination_is_another_result_slot() -> CheckedAlgorithmBlock {
    block_of(
        vec![pair()],
        vec![array("source"), array("produced"), array("kept")],
        vec![
            assign(local_ref("source"), state("a")),
            call(
                vec![local_ref("produced"), local_ref("kept")],
                "pair",
                local("source"),
            ),
            assign(local_ref("kept"), local("produced")),
            assign(
                galec::Reference::state(galec::Name::ident("result")),
                local("kept"),
            ),
        ],
    )
}

#[test]
fn a_destination_that_is_another_result_slot_keeps_its_read_back() {
    let block = destination_is_another_result_slot();
    let source = render(&block, "model.c.jinja");
    assert!(
        source.contains("rumoca_galec_copy_real(INT32_C(4), ctx->produced,"),
        "a result whose destination is a sibling slot must keep its own slot:\n{source}"
    );
    assert!(
        source.contains("rumoca_galec_copy_real(INT32_C(4), ctx->kept, ctx->produced)"),
        "the copy between two result slots must survive:\n{source}"
    );
    let (passed, output) = run_expecting(&block, 3.0);
    assert!(passed, "the refused rewrite changed the values:\n{output}");
}

/// An actual staged from block state keeps its copy.
///
/// The forwarded aggregate has to be a local of the calling owner, because that
/// is what makes "the callee cannot write it" a structural fact rather than a
/// call-graph argument: a stateful callee may write block state during the
/// call. A state-valued source is refused rather than reasoned about.
fn state_sourced_actual() -> CheckedAlgorithmBlock {
    block_of(
        vec![scale3()],
        vec![array("staged"), array("produced")],
        vec![
            assign(local_ref("staged"), state("a")),
            call(vec![local_ref("produced")], "scale3", local("staged")),
            assign(
                galec::Reference::state(galec::Name::ident("result")),
                local("produced"),
            ),
        ],
    )
}

#[test]
fn an_actual_staged_from_block_state_keeps_its_copy() {
    let block = state_sourced_actual();
    let source = render(&block, "model.c.jinja");
    assert!(
        source.contains("rumoca_galec_copy_real(INT32_C(4), ctx->staged, self->a)"),
        "a state-valued source must keep its staging copy:\n{source}"
    );
    let (passed, output) = run_expecting(&block, 3.0);
    assert!(passed, "the refused rewrite changed the values:\n{output}");
}
