//! Executable evidence for the overlaid, translation-unit-private working
//! memory the GALEC C targets emit.
//!
//! The layout decision itself — which regions may share storage — is a property
//! of the call graph and is tested as such in
//! `rumoca-phase-codegen/src/views/algorithm_code_typed.rs`. What is tested
//! HERE is the thing those properties exist to protect: that the generated C
//! still computes the right numbers. The fixture is built so that an overlay
//! which put a caller and its callee on the same storage produces a *different
//! answer*, not merely different text — a test that only checked that the unit
//! still compiles would be worthless for this change.

use std::fs;
use std::process::Command;

use rumoca_ir_galec::ast as galec;
use rumoca_ir_galec::package::CheckedAlgorithmBlock;
use rumoca_phase_codegen::{render_checked_algorithm_block_template_with_artifact, templates};
use serde_json::json;
use tempfile::tempdir;

const MODEL: &str = "WorkingMemory";
const EXTENT: i64 = 4;

fn array(name: &str) -> galec::VariableDeclaration {
    let mut declaration =
        galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident(name));
    declaration.dimensions = vec![galec::Dimension::Expr(galec::Expression::Integer(EXTENT))];
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

fn call(target: &str, callee: &str, argument: &str) -> galec::Spanned<galec::Statement> {
    galec::Spanned::dummy(galec::Statement::MultiAssignment {
        targets: vec![local_ref(target)],
        call: galec::FunctionCall {
            function: galec::Name::ident(callee),
            arguments: vec![local(argument)],
        },
    })
}

fn state_call(target: &str, callee: &str, argument: &str) -> galec::Spanned<galec::Statement> {
    galec::Spanned::dummy(galec::Statement::MultiAssignment {
        targets: vec![state_ref(target)],
        call: galec::FunctionCall {
            function: galec::Name::ident(callee),
            arguments: vec![local(argument)],
        },
    })
}

fn function(
    name: &str,
    locals: Vec<galec::VariableDeclaration>,
    statements: Vec<galec::Spanned<galec::Statement>>,
) -> galec::UserFunction {
    galec::UserFunction {
        kind: galec::FunctionKind::Stateless,
        name: galec::Name::ident(name),
        signals: Vec::new(),
        parameters: vec![
            parameter(galec::Direction::Input, "u"),
            parameter(galec::Direction::Output, "y"),
        ],
        locals,
        statements,
        span: rumoca_core::Span::DUMMY,
    }
}

/// A block whose value depends on three different pieces of working memory
/// staying alive across a call.
///
/// ```text
/// inner(u) => y      : y := 3*u
/// outer(u) => y      : keep := u; (y) := inner(keep); y := y + keep
/// DoStep             : staging := a; guard := 5*a;
///                      (result) := outer(staging); result := result + guard
/// ```
///
/// With `a = [1,2,3,4]` the answer is `9*a`, and each of the two overlay
/// mistakes this change could make produces a different number:
///
/// * If **`outer` shared storage with `inner`**, `inner`'s output slot would sit
///   on `outer`'s `keep`. `inner` would leave `keep = 3*a` behind, and
///   `y + keep` would come out `6*a` instead of `4*a` — the read-back-after-
///   return hazard, made observable.
/// * If **`dostep` shared storage with `outer`**, `outer`'s output slot would
///   sit on `guard`, and the final `result + guard` would read back what
///   `outer` just wrote instead of `5*a`.
fn fixture() -> CheckedAlgorithmBlock {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![
        interface(galec::InterfaceKind::Input, "a"),
        interface(galec::InterfaceKind::Output, "result"),
    ];
    block.protected_functions = vec![
        function(
            "inner",
            Vec::new(),
            vec![assign(local_ref("y"), scaled(local("u"), 3.0))],
        ),
        function(
            "outer",
            vec![array("keep")],
            vec![
                assign(local_ref("keep"), local("u")),
                call("y", "inner", "keep"),
                assign(local_ref("y"), sum(local("y"), local("keep"))),
            ],
        ),
    ];
    block.do_step.locals = vec![array("staging"), array("guard")];
    block.do_step.statements = vec![
        assign(local_ref("staging"), state("a")),
        assign(local_ref("guard"), scaled(state("a"), 5.0)),
        state_call("result", "outer", "staging"),
        assign(state_ref("result"), sum(state("result"), local("guard"))),
    ];
    CheckedAlgorithmBlock::construct(block).expect("working-memory fixture must be valid GALEC")
}

/// Write the shared array-kernel library beside a generated model and return
/// its translation unit, which the compile harness must also build. The library
/// carries no model view, so any fixture renders the same bytes.
fn write_kernel_library(directory: &std::path::Path) -> std::path::PathBuf {
    let block = fixture();
    fs::write(
        directory.join("rumoca_galec_kernels.h"),
        render(&block, "kernels.h.jinja"),
    )
    .expect("write kernel header");
    let path = directory.join("rumoca_galec_kernels.c");
    fs::write(&path, render(&block, "kernels.c.jinja")).expect("write kernel library");
    path
}

fn render(block: &CheckedAlgorithmBlock, path: &str) -> String {
    let template =
        templates::builtin_template_source("embedded-c-galec", path).expect("built-in template");
    render_checked_algorithm_block_template_with_artifact(block, &json!({}), template, MODEL)
        .expect("working-memory fixture must render")
}

const DRIVER: &str = "\
#include <stdio.h>
#include \"WorkingMemory.h\"

int main(void) {
    WorkingMemoryState state = {0};
    for (int32_t i = 0; i < 4; ++i) {
        state.a[i] = (float)(i + 1);
    }
    WorkingMemory_dostep(&state);
    for (int32_t i = 0; i < 4; ++i) {
        const float expected = 9.0f * (float)(i + 1);
        if (state.result[i] != expected) {
            printf(\"result[%d] = %f, expected %f\\n\",
                   (int)i, (double)state.result[i], (double)expected);
            return i + 1;
        }
    }
    return 0;
}
";

/// The strict preflight the target documents (SPEC_0034 GAL-029/030), plus the
/// three flags the profile adds beyond the smoke test in `galec_c_arrays`.
const STRICT: [&str; 13] = [
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

/// Values survive across every call that today's emitter leaves them alive
/// across, with the regions overlaid. This is the test that fails — with a
/// wrong number, not a compile error — if a caller and a callee are put on one
/// piece of storage.
#[test]
fn overlaid_working_memory_preserves_values_across_calls() {
    let block = fixture();
    let header = render(&block, "model.h.jinja");
    let source = render(&block, "model.c.jinja");

    let directory = tempdir().expect("temporary generated-C directory");
    let header_path = directory.path().join(format!("{MODEL}.h"));
    let source_path = directory.path().join(format!("{MODEL}.c"));
    let driver_path = directory.path().join("main.c");
    let executable = directory.path().join("working-memory");
    fs::write(&header_path, &header).expect("write generated header");
    fs::write(&source_path, &source).expect("write generated source");
    fs::write(&driver_path, DRIVER).expect("write generated-C driver");
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
        .expect("execute working-memory harness");
    assert!(
        run.status.success(),
        "overlaid working memory changed the block's values:\n{}",
        String::from_utf8_lossy(&run.stdout)
    );
}

/// The working memory is private to the translation unit: not in the header,
/// not in the block state, and its single-instance consequence is *stated*
/// rather than left for a reader to discover.
#[test]
fn working_memory_is_private_to_the_translation_unit() {
    let block = fixture();
    let header = render(&block, "model.h.jinja");
    let source = render(&block, "model.c.jinja");

    assert!(
        !header.contains("rumoca_galec_scratch"),
        "working memory must not appear in the interface header:\n{header}"
    );
    assert!(
        !header.contains("Scratch"),
        "no working-memory type may appear in the interface header:\n{header}"
    );
    assert!(
        header.contains("SINGLE INSTANCE PER PROGRAM"),
        "the single-instance assumption must be stated, not assumed:\n{header}"
    );
    assert!(
        source.contains("static WorkingMemoryScratch rumoca_galec_scratch;"),
        "working memory must be one file-scope object in the source:\n{source}"
    );
    // A caller reads a callee's outputs out of the callee's own region, and a
    // body aliases only its own. Both spellings must name the private object,
    // never a member of the caller-provided state.
    assert!(
        !source.contains("self->rumoca_galec_scratch"),
        "no path into working memory may go through the block state:\n{source}"
    );
    assert!(
        source.contains("*const ctx = &rumoca_galec_scratch.rumoca_galec_g"),
        "a body must alias its own region inside the private object:\n{source}"
    );
}

/// The overlay is visible in the emitted text as unions, and the sizes it
/// achieves are reported rather than left for a reader to derive.
#[test]
fn the_overlay_and_its_cost_are_stated_in_the_generated_source() {
    let source = render(&fixture(), "model.c.jinja");
    assert!(
        source.contains("typedef union WorkingMemoryScratchGroup0Tag"),
        "each overlay group is one union:\n{source}"
    );
    assert!(
        source.contains("typedef union WorkingMemoryScratchGroup2Tag"),
        "the three-deep chain must produce three groups:\n{source}"
    );
    assert!(
        source.contains("heaviest call chain dostep -> outer -> inner"),
        "the chain that determines the total must be named:\n{source}"
    );
    assert!(
        source.contains("overlay group(s)"),
        "the achieved total must be reported:\n{source}"
    );
}

/// Unused-entity suppression is emitted only where the entity is unused.
///
/// Every `(void)x;` is an executable statement a structural-coverage obligation
/// has to account for, and a body that reads its parameters and locals needs
/// none of them. The fixture's `outer` reads both its input and its local, so
/// neither may carry one; `self` keeps its suppression unconditionally, because
/// whether a body reaches `self` depends on target spellings the projection
/// deliberately does not know.
///
/// The load-bearing half of this test is the strict compile in
/// `overlaid_working_memory_preserves_values_across_calls`: a suppression
/// wrongly withheld is a failed build under `-Werror`, not a cosmetic defect.
#[test]
fn unused_entity_suppression_is_emitted_only_where_the_entity_is_unused() {
    let source = render(&fixture(), "model.c.jinja");
    assert!(
        !source.contains("(void)u;"),
        "`outer` and `inner` both read their input parameter:\n{source}"
    );
    assert!(
        !source.contains("(void)&"),
        "no frame local in this fixture is written without being read:\n{source}"
    );
    assert!(
        source.contains("(void)self;"),
        "`self` keeps its suppression, fail-closed:\n{source}"
    );
    // An array local lives in working memory, so it is declared there and
    // NOWHERE else — a second, dead automatic declaration is what the
    // unconditional marker used to hide.
    assert_eq!(
        source.matches("float keep[4];").count(),
        1,
        "an array local is declared once, in its region:\n{source}"
    );
}

/// The Production Code manifest describes the block's data, and working memory
/// is not the block's data. A target-side intermediate has no Algorithm Code
/// variable behind it for a `DataReference` to name and no consumer may bind to
/// one, so it belongs in neither the typedefs nor the state.
#[test]
fn the_production_code_manifest_describes_no_working_memory() {
    let template = templates::builtin_template_source("galec-production", "pc_manifest.xml.jinja")
        .expect("built-in template");
    let manifest = render_checked_algorithm_block_template_with_artifact(
        &fixture(),
        &json!({
            "identities": {"pc_manifest": "id", "ac_manifest": "id"},
            "checksums": {
                "ac_manifest_sha1": "0",
                "c_header_sha1": "0",
                "c_source_sha1": "0",
                "c_kernels_header_sha1": "0",
                "c_kernels_source_sha1": "0",
                "c_format_sha1": "0"
            },
            "generated_at": "1970-01-01T00:00:00Z",
            "generation_tool": "test"
        }),
        template,
        MODEL,
    )
    .expect("manifest must render");
    assert!(!manifest.contains("TD_SCRATCH"), "{manifest}");
    assert!(!manifest.contains("CO_SCRATCH"), "{manifest}");
    assert!(!manifest.contains("rumoca_galec_scratch"), "{manifest}");
    // The interface it DOES describe is untouched.
    assert!(
        manifest.contains("rumoca_galec_error_signal_status"),
        "{manifest}"
    );
    assert!(manifest.contains("TD_STATE"), "{manifest}");
}
