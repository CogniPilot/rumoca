//! Executable evidence for the overlay bound equalization: the declared width
//! of a slot that shares a union address with a call-boundary argument.
//!
//! The rule itself is a property of one group's offsets and is tested as such in
//! `rumoca-phase-codegen/src/views/algorithm_code_bound_equalization.rs`. What
//! is tested HERE is the half that module cannot see: the **detector**. The
//! decision to widen anything depends on the projection knowing which addresses
//! the emitted bodies hand to which declared parameter bounds, and that is a
//! fact about the statement list a target prints, not about the layout.
//!
//! So the two fixtures below are the same block twice, with the same
//! declarations, the same region sizes and the same overlay groups, differing in
//! exactly one thing: **which** of two equally sized locals the inner call is
//! given. In `clashing` the argument is the local at offset zero, where a
//! never-concurrent sibling starts a shorter array, and the sibling is declared
//! wide enough for the bound. In `clash_free` the argument is the local at
//! offset sixteen, the shorter sibling still starts at offset zero beside a
//! longer array, and nothing moves: the clash is left exactly where a C
//! compiler's value numbering finds it, because no declared bound is ever read
//! against that address.
//!
//! The generated C is then compiled under the target's strict assurance
//! preflight and run, because a widened extent that changed an answer would be
//! a wrong-code defect and not a layout decision.

use std::fs;
use std::process::Command;

use rumoca_ir_galec::ast as galec;
use rumoca_ir_galec::package::CheckedAlgorithmBlock;
use rumoca_phase_codegen::{render_checked_algorithm_block_template_with_artifact, templates};
use serde_json::json;
use tempfile::tempdir;

const MODEL: &str = "BoundClash";

/// The note the scratch partial prints beside a widened declaration.
const WIDENED: &str = "widened so this address carries one bound across the overlay";

fn array(name: &str, extent: i64) -> galec::VariableDeclaration {
    let mut declaration =
        galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident(name));
    declaration.dimensions = vec![galec::Dimension::Expr(galec::Expression::Integer(extent))];
    declaration
}

fn interface(kind: galec::InterfaceKind, name: &str, extent: i64) -> galec::InterfaceVariable {
    galec::InterfaceVariable {
        kind,
        decl: array(name, extent),
        start: None,
    }
}

fn parameter(direction: galec::Direction, name: &str, extent: i64) -> galec::Parameter {
    galec::Parameter {
        direction,
        decl: array(name, extent),
    }
}

fn local_ref(name: &str) -> galec::Reference {
    galec::Reference::local(galec::Name::ident(name))
}

fn state_ref(name: &str) -> galec::Reference {
    galec::Reference::state(galec::Name::ident(name))
}

fn local(name: &str) -> galec::Expression {
    galec::Expression::Ref(local_ref(name))
}

fn state(name: &str) -> galec::Expression {
    galec::Expression::Ref(state_ref(name))
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

fn call_into(
    target: galec::Reference,
    callee: &str,
    argument: galec::Expression,
) -> galec::Spanned<galec::Statement> {
    galec::Spanned::dummy(galec::Statement::MultiAssignment {
        targets: vec![target],
        call: galec::FunctionCall {
            function: galec::Name::ident(callee),
            arguments: vec![argument],
        },
    })
}

fn function(
    name: &str,
    extent: i64,
    locals: Vec<galec::VariableDeclaration>,
    statements: Vec<galec::Spanned<galec::Statement>>,
) -> galec::UserFunction {
    galec::UserFunction {
        kind: galec::FunctionKind::Stateless,
        name: galec::Name::ident(name),
        signals: Vec::new(),
        parameters: vec![
            parameter(galec::Direction::Input, "u", extent),
            parameter(galec::Direction::Output, &format!("y{extent}"), extent),
        ],
        locals,
        statements,
        span: rumoca_core::Span::DUMMY,
    }
}

/// The block both fixtures share, parameterised by which of `mid`'s two locals
/// the inner call is handed.
///
/// ```text
/// leaf(u[4])  => y4  : y4 := 5*u
/// short(u[3]) => y3  : y3 := 7*u
/// mid(u[4])   => y4  : first := 2*u; second := 3*u;
///                      (y4) := leaf(<argument>); y4 := y4 + <other>
/// DoStep             : feed := 3*a; (wide) := mid(feed); (narrow) := short(b)
/// ```
///
/// `mid`'s region is `first`, `second`, `y4`: three four-element Reals, so
/// `first` starts at offset 0 and `second` at offset 16. `short`'s region is
/// `y3` alone, three elements at offset 0. The two never reach one another, so
/// the overlay puts them in one group and `y3` starts at the same address as
/// `first`. Which of `first` and `second` the call is given therefore decides
/// whether offset zero is a call-boundary address, and nothing else about the
/// block changes.
fn block_passing(argument: &str, other: &str) -> CheckedAlgorithmBlock {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![
        interface(galec::InterfaceKind::Input, "a", 4),
        interface(galec::InterfaceKind::Input, "b", 3),
        interface(galec::InterfaceKind::Output, "wide", 4),
        interface(galec::InterfaceKind::Output, "narrow", 3),
    ];
    block.protected_functions = vec![
        function(
            "leaf",
            4,
            Vec::new(),
            vec![assign(local_ref("y4"), scaled(local("u"), 5.0))],
        ),
        function(
            "short",
            3,
            Vec::new(),
            vec![assign(local_ref("y3"), scaled(local("u"), 7.0))],
        ),
        function(
            "mid",
            4,
            vec![array("first", 4), array("second", 4)],
            vec![
                assign(local_ref("first"), scaled(local("u"), 2.0)),
                assign(local_ref("second"), scaled(local("u"), 3.0)),
                call_into(local_ref("y4"), "leaf", local(argument)),
                assign(local_ref("y4"), sum(local("y4"), local(other))),
            ],
        ),
    ];
    block.do_step.locals = vec![array("feed", 4)];
    block.do_step.statements = vec![
        assign(local_ref("feed"), scaled(state("a"), 3.0)),
        call_into(state_ref("wide"), "mid", local("feed")),
        call_into(state_ref("narrow"), "short", state("b")),
    ];
    CheckedAlgorithmBlock::construct(block).expect("bound-clash fixture must be valid GALEC")
}

/// The call is handed the local at offset zero, where `short`'s shorter output
/// starts. `wide = 5*(2*feed) + 3*feed = 39*a`, since `feed = 3*a`.
fn clashing() -> CheckedAlgorithmBlock {
    block_passing("first", "second")
}

/// The identical block with the call handed the local at offset sixteen. Same
/// declarations, same region sizes, same groups; offset zero is no longer an
/// address any declared bound is read against.
fn clash_free() -> CheckedAlgorithmBlock {
    block_passing("second", "first")
}

fn render(block: &CheckedAlgorithmBlock, path: &str) -> String {
    let template =
        templates::builtin_template_source("embedded-c-galec", path).expect("built-in template");
    render_checked_algorithm_block_template_with_artifact(block, &json!({}), template, MODEL)
        .expect("bound-clash fixture must render")
}

/// WHAT THE PASS IS FOR. `short`'s `[3]` output shares a union address with the
/// `[4]` local the emitted body hands to `leaf`'s `const float u[4]`, so it is
/// declared `[4]`: no access path at that address is shorter than the bound the
/// code reads against it, and the compiler's value numbering cannot pick a
/// twelve-byte object to diagnose the sixteen-byte read against.
#[test]
fn a_slot_sharing_a_call_boundary_address_is_declared_for_the_bound() {
    let header = render(&clashing(), "model.h.jinja");
    assert!(
        header.contains("float y3[4];"),
        "the shorter sibling must be declared for the bound at its address:\n{header}"
    );
    assert!(
        header.contains(WIDENED),
        "a widened declaration must say so where it is declared:\n{header}"
    );
    assert_eq!(
        header.matches(WIDENED).count(),
        1,
        "exactly one slot shares a call-boundary address here:\n{header}"
    );
}

/// NEGATIVE CONTROL, and the one that tests the DETECTOR rather than the rule.
/// The same clash is present (`y3` is twelve bytes at the offset where `first`
/// is sixteen), and the call is handed the other local instead. Nothing may
/// move: the pass widens for a bound the emitted code reads at that address, not
/// for a difference in sizes.
#[test]
fn the_same_clash_is_untouched_when_no_bound_is_read_at_that_address() {
    let header = render(&clash_free(), "model.h.jinja");
    assert!(
        header.contains("float y3[3];"),
        "the block's own declared extent must survive:\n{header}"
    );
    assert!(
        !header.contains(WIDENED),
        "no address here is handed to a declared bound, so nothing may widen:\n{header}"
    );
    // And the clash really is present, at the coordinates the diagnostic fires
    // at: a twelve-byte array and a sixteen-byte one both starting the members
    // of one overlay group.
    assert!(
        header.contains("float first[4];") && header.contains("float y3[3];"),
        "the control must still hold two differently sized group starts:\n{header}"
    );
}

/// A widened extent is a layout decision, so the numbers must not move. The
/// widened slot is read and written across the whole schedule, and both outputs
/// are checked against hand arithmetic.
#[test]
fn the_widened_layout_computes_the_same_values() {
    let block = clashing();
    let header = render(&block, "model.h.jinja");
    let source = render(&block, "model.c.jinja");
    let (passed, output) = run_generated(&header, &source, DRIVER);
    assert!(
        passed,
        "the widened layout changed the block's values:\n{output}"
    );
}

/// Write the shared array-kernel library beside a generated model and return its
/// translation unit. The library carries no model view, so any fixture renders
/// the same bytes.
fn write_kernel_library(directory: &std::path::Path) -> std::path::PathBuf {
    let block = clashing();
    fs::write(
        directory.join("rumoca_galec_kernels.h"),
        render(&block, "kernels.h.jinja"),
    )
    .expect("write kernel header");
    let path = directory.join("rumoca_galec_kernels.c");
    fs::write(&path, render(&block, "kernels.c.jinja")).expect("write kernel library");
    path
}

/// Compile a generated model beside the shared kernel library and a driver under
/// the strict preflight, run it, and return the driver's status and output.
fn run_generated(header: &str, source: &str, driver: &str) -> (bool, String) {
    let directory = tempdir().expect("temporary generated-C directory");
    let header_path = directory.path().join(format!("{MODEL}.h"));
    let source_path = directory.path().join(format!("{MODEL}.c"));
    let driver_path = directory.path().join("main.c");
    let executable = directory.path().join("harness");
    fs::write(&header_path, header).expect("write generated header");
    fs::write(&source_path, source).expect("write generated source");
    fs::write(&driver_path, driver).expect("write generated-C driver");
    let kernels_path = write_kernel_library(directory.path());

    let compile = Command::new("cc")
        .args(STRICT)
        .arg(&driver_path)
        .arg(&source_path)
        .arg(&kernels_path)
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

const DRIVER: &str = "\
#include <stdio.h>
#include \"BoundClash.h\"

int main(void) {
    BoundClashState st = {0};
    for (int32_t i = 0; i < 4; ++i) {
        st.a[i] = (float)(i + 1);
    }
    for (int32_t i = 0; i < 3; ++i) {
        st.b[i] = (float)(10 * (i + 1));
    }
    for (int32_t iteration = 0; iteration < 100; ++iteration) {
        BoundClash_dostep(&st);
    }
    for (int32_t i = 0; i < 4; ++i) {
        const float expected = 39.0f * (float)(i + 1);
        if (st.wide[i] != expected) {
            printf(\"wide[%d] = %f, expected %f\\n\",
                   (int)i, (double)st.wide[i], (double)expected);
            return 1;
        }
    }
    for (int32_t i = 0; i < 3; ++i) {
        const float expected = 70.0f * (float)(i + 1);
        if (st.narrow[i] != expected) {
            printf(\"narrow[%d] = %f, expected %f\\n\",
                   (int)i, (double)st.narrow[i], (double)expected);
            return 2;
        }
    }
    return 0;
}
";

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
