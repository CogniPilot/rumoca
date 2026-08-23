//! Executable evidence for the overlaid, instance-owned working memory the
//! GALEC C targets emit.
//!
//! The layout decision itself — which regions may share storage — is a property
//! of the call graph and is tested as such in
//! `rumoca-phase-codegen/src/views/algorithm_code_typed.rs`. What is tested
//! HERE is the thing those properties exist to protect: that the generated C
//! still computes the right numbers. The fixture is built so that an overlay
//! which put a caller and its callee on the same storage produces a *different
//! answer*, not merely different text — a test that only checked that the unit
//! still compiles would be worthless for this change.
//!
//! The second half of the file does the same job for the ARM overlay, which
//! shares storage between slots inside one region. Its fixture is a correction
//! chain with one buffer per arm and one buffer every arm reads: an overlay that
//! took the second buffer gets a wrong number on every path through the chain.
//! The arm relation itself is tested in `views::algorithm_code_slot_overlay`.

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
    render_as(block, path, MODEL)
}

fn render_as(block: &CheckedAlgorithmBlock, path: &str, model: &str) -> String {
    let template =
        templates::builtin_template_source("embedded-c-galec", path).expect("built-in template");
    render_checked_algorithm_block_template_with_artifact(block, &json!({}), template, model)
        .expect("working-memory fixture must render")
}

/// Compile a generated model beside the shared kernel library and a driver, run
/// it, and return the driver's exit status and output.
fn run_generated(model: &str, source: &str, header: &str, driver: &str) -> (bool, String) {
    let directory = tempdir().expect("temporary generated-C directory");
    let header_path = directory.path().join(format!("{model}.h"));
    let source_path = directory.path().join(format!("{model}.c"));
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
#include <pthread.h>
#include <stdio.h>
#include \"WorkingMemory.h\"

typedef struct ThreadArgumentsTag {
    WorkingMemoryState *state;
} ThreadArguments;

static void *run_model(void *opaque) {
    ThreadArguments *const arguments = (ThreadArguments *)opaque;
    for (int32_t iteration = 0; iteration < 1000; ++iteration) {
        WorkingMemory_dostep(arguments->state);
    }
    return NULL;
}

int main(void) {
    WorkingMemoryState first = {0};
    WorkingMemoryState second = {0};
    for (int32_t i = 0; i < 4; ++i) {
        first.a[i] = (float)(i + 1);
        second.a[i] = 10.0f * (float)(i + 1);
    }
    if ((void *)&first.rumoca_galec_scratch ==
        (void *)&second.rumoca_galec_scratch) {
        return 20;
    }
    ThreadArguments first_arguments = {&first};
    ThreadArguments second_arguments = {&second};
    pthread_t first_thread;
    pthread_t second_thread;
    if (pthread_create(&first_thread, NULL, run_model, &first_arguments) != 0) {
        return 21;
    }
    if (pthread_create(&second_thread, NULL, run_model, &second_arguments) != 0) {
        return 22;
    }
    if (pthread_join(first_thread, NULL) != 0) {
        return 23;
    }
    if (pthread_join(second_thread, NULL) != 0) {
        return 24;
    }
    for (int32_t i = 0; i < 4; ++i) {
        const float first_expected = 9.0f * (float)(i + 1);
        const float second_expected = 90.0f * (float)(i + 1);
        if ((first.result[i] != first_expected) ||
            (second.result[i] != second_expected)) {
            printf(\"result[%d] = (%f, %f), expected (%f, %f)\\n\",
                   (int)i, (double)first.result[i], (double)second.result[i],
                   (double)first_expected, (double)second_expected);
            return i + 1;
        }
    }
    return 0;
}
";

/// The strict preflight the target documents (SPEC_0034 GAL-029/030), plus the
/// three flags the profile adds beyond the smoke test in `galec_c_arrays`.
const STRICT: [&str; 15] = [
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
    "-pthread",
    "-lm",
];

/// Values survive across every call that today's emitter leaves them alive
/// across, with the regions overlaid. Two state objects execute concurrently,
/// so the test also fails if the generated model shares mutable working memory
/// between instances.
#[test]
fn overlaid_working_memory_preserves_values_across_calls() {
    let block = fixture();
    let header = render(&block, "model.h.jinja");
    let source = render(&block, "model.c.jinja");

    let (passed, output) = run_generated(MODEL, &source, &header, DRIVER);
    assert!(
        passed,
        "overlaid working memory changed the block's values:\n{output}"
    );
}

/// Working memory belongs to the caller-provided state, and the generated
/// resource contract states both the checked slot budget and the absence of
/// auxiliary mutable storage.
#[test]
fn working_memory_is_owned_by_each_state_instance() {
    let block = fixture();
    let header = render(&block, "model.h.jinja");
    let source = render(&block, "model.c.jinja");

    assert!(
        header.contains("WorkingMemoryScratch rumoca_galec_scratch;"),
        "each state must own its working memory:\n{header}"
    );
    assert!(
        header.contains("RUMOCA_WORKINGMEMORY_DISTINCT_INSTANCE_REENTRANT UINT32_C(1)"),
        "the distinct-instance concurrency contract must be machine-readable:\n{header}"
    );
    assert!(
        header.contains("RUMOCA_WORKINGMEMORY_CHECKED_SCRATCH_SLOT_BYTES UINT32_C(80)"),
        "the checked scratch-slot budget must be machine-readable:\n{header}"
    );
    assert!(
        header.contains("RUMOCA_WORKINGMEMORY_AUXILIARY_SCRATCH_BYTES UINT32_C(0)"),
        "the absence of hidden auxiliary scratch must be machine-readable:\n{header}"
    );
    assert!(
        !source.contains("static WorkingMemoryScratch rumoca_galec_scratch;"),
        "the translation unit must contain no shared mutable scratch:\n{source}"
    );
    assert!(
        source.contains("*const ctx = &self->rumoca_galec_scratch.rumoca_galec_g"),
        "a body must alias its own region inside the caller's state:\n{source}"
    );
}

/// The overlay is visible in the emitted text as unions, and the sizes it
/// achieves are reported rather than left for a reader to derive.
#[test]
fn the_overlay_and_its_cost_are_stated_in_the_generated_source() {
    let header = render(&fixture(), "model.h.jinja");
    let source = render(&fixture(), "model.c.jinja");
    assert!(
        header.contains("typedef union WorkingMemoryScratchGroup0Tag"),
        "each overlay group is one union:\n{header}"
    );
    assert!(
        header.contains("typedef union WorkingMemoryScratchGroup2Tag"),
        "the three-deep chain must produce three groups:\n{header}"
    );
    assert!(
        source.contains("heaviest call chain dostep -> outer -> inner"),
        "the chain that determines the total must be named:\n{source}"
    );
    assert!(
        source.contains("overlay group(s)"),
        "the achieved total must be reported:\n{source}"
    );
    // The achieved total is stated against the floor, not on its own: a RAM
    // admission argument needs to know whether 80 bytes is the best available
    // or merely what today's placement policy managed. On this block the two
    // meet, and the emitted text says so.
    assert!(
        source.contains(
            "80 bytes of slot storage, the least a never-concurrent overlay of these \
             regions can use"
        ),
        "the achieved total must be stated against the floor:\n{source}"
    );
    // The arm overlay is a second, independent decision and is reported on its
    // own line, including where it saved nothing: silence would read as "not
    // considered". This fixture's bodies have no conditional at all.
    assert!(
        source.contains("no arm overlay: no slot is provably local to one arm"),
        "the arm-overlay decision must be reported even when it is empty:\n{source}"
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
    assert_eq!(source.matches("float keep[4];").count(), 0, "{source}");
    assert_eq!(
        render(&fixture(), "model.h.jinja")
            .matches("float keep[4];")
            .count(),
        1,
        "an array local is declared once, in its instance-owned region"
    );
}

/// The Production Code manifest describes the block's data, and working memory
/// is not the block's data. A target-side intermediate has no Algorithm Code
/// variable behind it for a `DataReference` to name and no consumer may bind to
/// one, so it belongs in neither the manifest typedefs nor manifest objects.
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

// ---------------------------------------------------------------------------
// Arm overlay: slots that share storage INSIDE one region.
// ---------------------------------------------------------------------------

const ARM_MODEL: &str = "ArmOverlay";

fn boolean_input(name: &str) -> galec::InterfaceVariable {
    galec::InterfaceVariable {
        kind: galec::InterfaceKind::Input,
        decl: galec::VariableDeclaration::scalar(
            galec::ScalarType::Boolean,
            galec::Name::ident(name),
        ),
        start: None,
    }
}

fn branch(
    condition: galec::Expression,
    body: Vec<galec::Spanned<galec::Statement>>,
) -> galec::IfBranch {
    galec::IfBranch {
        condition: galec::Condition::Expression(condition),
        body,
        span: rumoca_core::Span::DUMMY,
    }
}

fn arm_body(slot: &str, factor: f64) -> Vec<galec::Spanned<galec::Statement>> {
    vec![
        assign(local_ref(slot), scaled(state("a"), factor)),
        assign(state_ref("result"), sum(local("spanning"), local(slot))),
    ]
}

/// A block whose `DoStep` is a correction chain: one buffer per arm, plus one
/// buffer that every arm reads and no arm owns.
///
/// ```text
/// DoStep : spanning := a;
///          if mode then      arm_a := 2*a;  result := spanning + arm_a   -> 3*a
///          elseif other then arm_b := 3*a;  result := spanning + arm_b   -> 4*a
///          else              arm_c := 4*a;  result := spanning + arm_c   -> 5*a
///          end if
/// ```
///
/// `arm_a`, `arm_b` and `arm_c` are what the arm overlay is for: at most one of
/// them is ever touched, so one piece of storage holds whichever it is.
/// `spanning` is the counterexample in the same fixture: it is written outside
/// the chain and read inside every arm, so it is live across the chain. If it
/// were given an arm's storage, that arm's assignment would overwrite it before
/// the `spanning + arm_x` that reads it, and the answer would come out twice
/// the arm's factor instead of one plus it. That is what the driver checks.
fn arm_fixture() -> CheckedAlgorithmBlock {
    let mut block = galec::Block::new(galec::Name::ident(ARM_MODEL));
    block.interface = vec![
        interface(galec::InterfaceKind::Input, "a"),
        boolean_input("mode"),
        boolean_input("other"),
        interface(galec::InterfaceKind::Output, "result"),
    ];
    block.do_step.locals = vec![
        array("spanning"),
        array("arm_a"),
        array("arm_b"),
        array("arm_c"),
    ];
    block.do_step.statements = vec![
        assign(local_ref("spanning"), state("a")),
        galec::Spanned::dummy(galec::Statement::If(galec::IfStatement {
            branches: vec![
                branch(state("mode"), arm_body("arm_a", 2.0)),
                branch(state("other"), arm_body("arm_b", 3.0)),
            ],
            else_body: Some(arm_body("arm_c", 4.0)),
        })),
    ];
    CheckedAlgorithmBlock::construct(block).expect("arm-overlay fixture must be valid GALEC")
}

const ARM_DRIVER: &str = "\
#include <stdio.h>
#include \"ArmOverlay.h\"

static int check(ArmOverlayState *state, bool mode, bool other, float factor) {
    state->mode = mode;
    state->other = other;
    ArmOverlay_dostep(state);
    for (int32_t i = 0; i < 4; ++i) {
        const float expected = (1.0f + factor) * (float)(i + 1);
        if (state->result[i] != expected) {
            printf(\"mode=%d other=%d result[%d] = %f, expected %f\\n\",
                   (int)mode, (int)other, (int)i,
                   (double)state->result[i], (double)expected);
            return 1;
        }
    }
    return 0;
}

int main(void) {
    ArmOverlayState state = {0};
    for (int32_t i = 0; i < 4; ++i) {
        state.a[i] = (float)(i + 1);
    }
    /* Every arm, twice around, so a value one arm left behind is read by the
       next arm rather than by a fresh instance. */
    for (int32_t round = 0; round < 2; ++round) {
        if (check(&state, true, false, 2.0f) != 0) {
            return 1;
        }
        if (check(&state, false, true, 3.0f) != 0) {
            return 2;
        }
        if (check(&state, false, false, 4.0f) != 0) {
            return 3;
        }
    }
    return 0;
}
";

/// The arm overlay computes the same numbers, in real compiled C, on every path
/// through the chain.
///
/// The fixture is built so a WRONG overlay produces a different answer, not
/// merely different text: `spanning` is live across the chain, so an overlay
/// that gave it an arm's storage would have that arm's assignment clobber it
/// before the sum that reads it.
#[test]
fn arm_overlaid_slots_compute_the_same_values_on_every_arm() {
    let block = arm_fixture();
    let header = render_as(&block, "model.h.jinja", ARM_MODEL);
    let source = render_as(&block, "model.c.jinja", ARM_MODEL);

    let (passed, output) = run_generated(ARM_MODEL, &source, &header, ARM_DRIVER);
    assert!(
        passed,
        "the arm overlay changed the block's values:\n{output}"
    );
}

/// NEGATIVE CONTROL, in the emitted C. The three arm-local buffers land in one
/// union; the buffer that is live across the chain keeps its own storage.
///
/// This is the layout claim the runtime test above depends on: without it that
/// test would pass for the uninteresting reason that nothing was overlaid.
#[test]
fn a_slot_live_across_the_chain_is_refused_an_arm_overlay() {
    let header = render_as(&arm_fixture(), "model.h.jinja", ARM_MODEL);
    let source = render_as(&arm_fixture(), "model.c.jinja", ARM_MODEL);

    let overlay = header
        .split("typedef union ArmOverlayScratchArm0_dostepTag {")
        .nth(1)
        .and_then(|rest| rest.split('}').next())
        .unwrap_or_default();
    for slot in ["arm_a", "arm_b", "arm_c"] {
        assert!(
            overlay.contains(&format!("float {slot}[4];")),
            "`{slot}` is local to one arm and belongs in the overlay:\n{header}"
        );
    }
    assert!(
        !overlay.contains("float spanning[4];"),
        "`spanning` is read in every arm and must keep its own storage:\n{header}"
    );
    assert!(
        header.contains("    float spanning[4];"),
        "`spanning` stays a plain member of the region:\n{header}"
    );
    // And the bodies address each slot through the storage it was given.
    assert!(
        source.contains("ctx->rumoca_galec_arm0.arm_a"),
        "an overlaid slot is reached through its overlay:\n{source}"
    );
    assert!(
        source.contains("ctx->spanning"),
        "a slot that owns its storage is reached directly:\n{source}"
    );
    // Three arm-local buffers of 16 bytes collapse to one, and `spanning` is
    // still counted once: four slots of 16 bytes become 32 bytes, not 64.
    assert!(
        header.contains("RUMOCA_ARMOVERLAY_CHECKED_SCRATCH_SLOT_BYTES UINT32_C(32)"),
        "the arm overlay must show up in the checked slot budget:\n{header}"
    );
}
