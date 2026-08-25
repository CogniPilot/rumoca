//! GALEC production-C review-surface metric gate (ledger #49).
//!
//! `Vehicles.Rdd2.NavigationEstimator` is the largest realistic model we emit
//! eFMI Production Code for, and the whole point of that track is that a human
//! certification reviewer reads the emitted C. "Readable" resisted being a
//! test for a long time, so it drifted: the generated file grew, and the
//! particular shapes that make review expensive (hand-unrolled elementwise
//! copies, `(void)` unused-parameter noise, redundant index casts) accumulated
//! with nobody noticing. This test makes the review surface a number.
//!
//! Every ceiling below is a **RATCHET**: lower it when the emitter improves,
//! and never raise it without a recorded decision saying why the extra review
//! burden is accepted. The measured value is printed into every assertion
//! message so a failure explains itself without anyone re-running the emitter
//! by hand.
//!
//! **Why it is feature-gated.** It needs a release build of the compiler (a
//! debug emit of this model is slow enough to dominate the suite) and the
//! out-of-tree `modelica_models` library, so it cannot run in a default
//! `cargo test`. A Cargo feature is how this repo gates heavy suites;
//! `architecture_hardening_test` bans the attribute-based alternative outright.
//! Run it deliberately:
//!
//! ```text
//! cargo build --release -p rumoca
//! cargo test -p rumoca --features rdd2-metric-gates --test suite_gates -- galec_review_surface
//! ```
//!
//! A missing `modelica_models` checkout is a HARD FAILURE with instructions,
//! not a skip: this gate only compiles when its feature is explicitly
//! requested, so a green "ok" with nothing measured would be the silent-pass
//! failure mode the gate exists to prevent -- and libtest hides a skip message
//! unless `--nocapture` happens to be on. Set `MODELICAPATH` or edit
//! `source_root` in the shared baseline JSON to point at your checkout.
//! Run it from the same shell you built in: the release
//! binary is spawned as a subprocess and inherits this process's dynamic-linker
//! environment, so a stripped-down `LD_LIBRARY_PATH` surfaces as a loader error
//! in the assertion that reports the subprocess's stderr.
//!
//! **NOT WIRED INTO CI -- this is a manual-invocation gate.** No workflow
//! passes `--features rdd2-metric-gates`, so nothing runs this on a push.
//! Unlike the sibling perf guard, there is no hardware reason for that: every
//! metric here is a deterministic function of the emitted text, so a shared
//! runner would produce identical numbers. What is missing is plumbing --
//! ci.yml's `modelica-models-gate` job checks out a *pinned* `CogniPilot/
//! modelica_models` revision and drives the prebuilt `rumoca-msl-profile`
//! binary from a Nix closure, not `cargo test` with a release `rumoca`. Wiring
//! this in means confirming that pinned revision carries
//! `Vehicles.Rdd2.NavigationEstimator` and giving that job a release CLI. Until
//! then, run it by hand after any change to the GALEC C templates.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use tempfile::tempdir;

// Corpus/binary resolution is shared with the sibling `rdd2_perf_guard` gate so
// the two cannot disagree about what they are measuring, and the text metrics
// live in an ungated module so their unit tests always compile.
use super::rdd2_gate_environment::{
    corpus_provenance, read_baseline, release_binary, require_model_library,
};
use super::rdd2_text_metrics::{
    count_elementwise_copy_lines, count_for_loops, count_occurrences, declared_int32_entities,
    redundant_int32_casts,
};

/// The model whose Production Code review surface is gated.
const MODEL: &str = "Vehicles.Rdd2.NavigationEstimator";

/// Package entry point, relative to the `modelica_models` checkout.
const PACKAGE_RELATIVE_PATH: &str = "Vehicles/package.mo";

// Every ceiling below was measured against the post-working-memory-overlay
// emitter (the state this branch merges on top of), not against the older
// pre-overlay numbers an earlier revision of this file quoted. Calibrating
// against a superseded emitter is how a ratchet goes stale without anyone
// noticing: the `(void)` ceiling below was 1 000 against a measured 907, and
// the overlay dropped the real figure to 97 -- a ceiling 10x above reality
// gates nothing at all.

/// Total emitted C lines. Current: 8 897 for the model unit alone (down from
/// 9 414: whole-array moves stopped expanding into loop nests, then each
/// array marshalling copy gained a one-line binding comment).
///
/// The ceiling deliberately stays at 9 500 rather than following the
/// improvement all the way down: the flat-storage and matmul-kernel work
/// this branch feeds is expected to trade lines in both directions, and the
/// for-loop ratchet below is the metric that actually tracks the win. A
/// trip of this ceiling still requires an explicit decision. Note the
/// measured figure is the MODEL unit's: the shared kernel library is gated
/// separately by [`MAX_KERNEL_LIBRARY_LINES`].
///
/// RATCHET: lower when improved, never raise without a recorded decision.
const MAX_TOTAL_LINES: usize = 9_500;

/// `for` loop headers. A reviewer has to follow each one's bounds, so this
/// tracks control-flow review cost rather than raw size. Current: 651, down
/// from 738 — every eliminated header was an identity-copy nest replaced by
/// a whole-array assignment printed as one counted kernel call. Ratcheted
/// 800 -> 700 to follow the measured improvement down.
/// RATCHET: lower when improved, never raise without a recorded decision.
const MAX_FOR_LOOPS: usize = 700;

/// Pure elementwise copy statements with literal indices on both sides
/// (`dst[3][1] = src[3][1];`) -- a hand-unrolled loop or memcpy that carries no
/// information for a reviewer but must still be read. Current: 26 (measured; an
/// earlier revision quoted 17, which was never reproduced). Tightened from 40
/// to 30 now that the real figure is known: 54 % headroom was slack, not a gate.
/// RATCHET: lower when improved, never raise without a recorded decision.
const MAX_ELEMENTWISE_COPY_LINES: usize = 30;

/// `(void)` casts, overwhelmingly unused-parameter suppressions. Pure noise on
/// the review surface. Current: 97, down from 907 now that suppressions are
/// emitted only where the entity is genuinely unused. Ratcheted 1 000 -> 120 to
/// follow that improvement down; leaving it at 1 000 would have left a ceiling
/// an order of magnitude above reality.
/// RATCHET: lower when improved, never raise without a recorded decision.
const MAX_VOID_CASTS: usize = 120;

/// `(int32_t)` casts of an operand the emitted sources already declare
/// `int32_t` -- a cast from a type to itself. The wholesale case was eliminated
/// (1 280 sites in this model) and is emitted as `[i - 1]` now. The ceiling is
/// zero on purpose: it pins the fix so the shape cannot come back.
///
/// The detector matches on the operand's *declared type*, not on a naming
/// convention or a trailing `- 1`, so it covers every spelling: `((int32_t)
/// (i)) - 1` with stray whitespace, `a[((int32_t)(i))]` with no offset, and
/// member paths like `((int32_t)(ctx->n)) - 1`.
///
/// Deliberately NOT counted: `((int32_t)((7 + (i - 1)))) - 1` and friends, i.e.
/// a cast around a *computed* index expression. Those keep the explicit cast by
/// design, because that is where width and signedness genuinely need pinning,
/// and they must not be mistaken for a regression.
///
/// RATCHET: already at the floor. Never raise.
const MAX_REDUNDANT_INT32_CASTS: usize = 0;

/// File stem of the shared array-kernel library TU both C targets emit
/// beside every model unit. It is model-independent, so it is measured under
/// its own ceiling rather than the model unit's.
const KERNEL_LIBRARY_STEM: &str = "rumoca_galec_kernels";

/// The shared kernel library's `.c`, total lines. Current: 247.
///
/// DECISION (recorded here so the split is deliberate): the model-unit
/// ceilings above measure the per-model review surface, and folding a
/// constant model-independent TU into them would let model-unit
/// growth hide behind kernel-library shrinkage (or vice versa). The library
/// is therefore ratcheted separately — it is reviewed once per compiler
/// build, not once per model, but it must not grow silently either: every
/// kernel added to it is new certification surface for every consumer.
/// RATCHET: lower when improved, never raise without a recorded decision.
///
/// DECISION 2026-08-24 (raise 80 -> 260): the library gained fixed-count
/// straight-line specializations of the four Real kernels (copy 3/15/24,
/// fill 3/15, dot 6/12/15, scaled-add 3/6/12/15) for the 1 kHz flight-track
/// budget. Measured on `Vehicles.Rdd2.NavigationEstimator`'s worst
/// correcting step (GPS pos+vel, dossier scenario 6, Cortex-M7 `-Os`): the
/// generic counted loops cost 8 to 9.5 executed instructions per element at
/// the run lengths this class of model produces (12 to 15), while the
/// straight-line bodies cost 3.3 to 4.1; the specializations took the step
/// from 954,168 to under 620,000 executed instructions and were required to
/// fit the block's declared 1 ms period at 600 MHz. Each added line is a
/// single subscripted element operation with no control flow — line count
/// rises 4x while branch count in the library stays exactly what it was
/// (one bounded loop per generic kernel). Every specialization is pinned
/// bit-identical to its generic kernel by the equivalence suites.
const MAX_KERNEL_LIBRARY_LINES: usize = 260;

/// The emitted Production Code sources.
struct ProductionSources {
    /// The model unit's `.c` every model-surface metric is measured over.
    c: String,
    /// The companion `.h`, read only to harvest `int32_t` struct-field
    /// declarations for the redundant-cast metric.
    header: String,
    /// The shared array-kernel library TU, measured only against
    /// [`MAX_KERNEL_LIBRARY_LINES`].
    kernels_c: String,
}

/// Emit `MODEL` with `--target galec-production` into `out_dir` and return the
/// text of the single `ProductionCode/*.c` the container holds.
fn emit_production_c(source_root: &Path, out_dir: &Path) -> ProductionSources {
    let package = source_root.join(PACKAGE_RELATIVE_PATH);
    let output = Command::new(release_binary())
        .arg("compile")
        .arg(&package)
        .arg("--model")
        .arg(MODEL)
        .arg("--source-root")
        .arg(source_root)
        .arg("--target")
        .arg("galec-production")
        .arg("-o")
        .arg(out_dir)
        .output()
        .unwrap_or_else(|error| {
            panic!("spawn `rumoca compile --target galec-production`: {error}")
        });
    assert!(
        output.status.success(),
        "`rumoca compile --target galec-production` failed for {MODEL} ({}):\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    let mut sources = Vec::new();
    collect_production_c(out_dir, &mut sources);
    // The container holds exactly two C translation units: the model unit and
    // the shared array-kernel library beside it. They are gated separately —
    // selecting by stem here is what keeps this gate measuring the model unit
    // rather than panicking on the library's presence.
    let (kernel_paths, model_paths): (Vec<_>, Vec<_>) = sources.into_iter().partition(|path| {
        path.file_stem()
            .is_some_and(|stem| stem == KERNEL_LIBRARY_STEM)
    });
    let ([path], [kernels_path]) = (model_paths.as_slice(), kernel_paths.as_slice()) else {
        panic!(
            "expected exactly one model ProductionCode/*.c and one {KERNEL_LIBRARY_STEM}.c \
             under {}, found model units {model_paths:?} and kernel TUs {kernel_paths:?}",
            out_dir.display()
        );
    };
    let c = fs::read_to_string(path)
        .unwrap_or_else(|error| panic!("read emitted C {}: {error}", path.display()));
    // The header is optional as far as the metrics go: without it the
    // redundant-cast metric simply has fewer declarations to match against,
    // which can only ever under-report, never invent an offender.
    let header = fs::read_to_string(path.with_extension("h")).unwrap_or_default();
    let kernels_c = fs::read_to_string(kernels_path)
        .unwrap_or_else(|error| panic!("read kernel library {}: {error}", kernels_path.display()));
    ProductionSources {
        c,
        header,
        kernels_c,
    }
}

/// Collect every `*.c` living directly in a `ProductionCode/` directory below
/// `dir`. Discovering the file this way keeps the test independent of how the
/// eFMU container names its model directory.
fn collect_production_c(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_production_c(&path, out);
            continue;
        }
        let in_production_code = path
            .parent()
            .and_then(Path::file_name)
            .is_some_and(|name| name == "ProductionCode");
        if in_production_code && path.extension().is_some_and(|ext| ext == "c") {
            out.push(path);
        }
    }
}

#[test]
fn test_navigation_estimator_production_c_review_surface_stays_under_its_ceilings() {
    let baseline = read_baseline();
    let source_root = require_model_library(&baseline);

    let out = tempdir().expect("create temp output dir");
    let emitted = emit_production_c(&source_root, out.path());
    let source = emitted.c.as_str();

    let declared_int32 = declared_int32_entities(&[source, emitted.header.as_str()]);
    let kernel_library_lines = emitted.kernels_c.lines().count();
    let total_lines = source.lines().count();
    let for_loops = count_for_loops(source);
    let elementwise_copies = count_elementwise_copy_lines(source);
    let void_casts = count_occurrences(source, "(void)");
    let redundant_casts = redundant_int32_casts(source, &declared_int32);
    let redundant_index_casts = redundant_casts.len();

    // Print the corpus revision with the numbers: these ceilings are measured
    // against an out-of-tree checkout that nothing in this repo pins, so a
    // failure is ambiguous without knowing which corpus produced it.
    println!("{}", corpus_provenance(&baseline, &source_root));
    println!(
        "GALEC review surface for {MODEL}: lines={total_lines} for-loops={for_loops} \
         elementwise-copies={elementwise_copies} (void)={void_casts} \
         redundant-int32-casts={redundant_index_casts} \
         kernel-library-lines={kernel_library_lines} \
         (matched against {} declared int32_t entities)",
        declared_int32.len()
    );

    const RATCHET: &str = "This is a RATCHET ceiling: lower it when the emitter improves, never \
                           raise it without a recorded decision.";

    assert!(
        total_lines <= MAX_TOTAL_LINES,
        "{MODEL} Production Code grew to {total_lines} lines, over the ceiling of \
         {MAX_TOTAL_LINES} (by {}). {RATCHET}",
        total_lines - MAX_TOTAL_LINES
    );
    assert!(
        kernel_library_lines <= MAX_KERNEL_LIBRARY_LINES,
        "the shared kernel library grew to {kernel_library_lines} lines, over the ceiling of \
         {MAX_KERNEL_LIBRARY_LINES} (by {}). Every kernel in it is certification surface for \
         every consumer of every generated block. {RATCHET}",
        kernel_library_lines - MAX_KERNEL_LIBRARY_LINES
    );
    assert!(
        for_loops <= MAX_FOR_LOOPS,
        "{MODEL} Production Code emits {for_loops} `for (` loops, over the ceiling of \
         {MAX_FOR_LOOPS} (by {}). Each loop is control flow a certification reviewer must \
         follow. {RATCHET}",
        for_loops - MAX_FOR_LOOPS
    );
    assert!(
        elementwise_copies <= MAX_ELEMENTWISE_COPY_LINES,
        "{MODEL} Production Code emits {elementwise_copies} pure elementwise copy statements \
         (`dst[i][j] = src[i][j];` with literal indices), over the ceiling of \
         {MAX_ELEMENTWISE_COPY_LINES} (by {}). Unrolled copies are review surface with no \
         information content -- emit a loop or a whole-array assignment instead. {RATCHET}",
        elementwise_copies - MAX_ELEMENTWISE_COPY_LINES
    );
    assert!(
        void_casts <= MAX_VOID_CASTS,
        "{MODEL} Production Code contains {void_casts} `(void)` casts, over the ceiling of \
         {MAX_VOID_CASTS} (by {}). These are almost all unused-parameter suppressions: pure \
         noise on the review surface. {RATCHET}",
        void_casts - MAX_VOID_CASTS
    );
    assert!(
        redundant_index_casts == MAX_REDUNDANT_INT32_CASTS,
        "{MODEL} Production Code contains {redundant_index_casts} redundant `(int32_t)` casts \
         of entities it already declared `int32_t`, over the ceiling of \
         {MAX_REDUNDANT_INT32_CASTS}. This shape was eliminated (1 280 sites) and the ceiling \
         is zero so it cannot come back -- an already-`int32_t` operand needs no cast, so \
         emit `[i - 1]` (or `[i]`) directly.\n\
         Offending operands (each is declared `int32_t` in the emitted sources): {:?}\n\
         Casts around COMPUTED index expressions are intentional and are NOT counted; if an \
         operand above is not in fact an `int32_t` entity, the declaration harvester has a \
         bug and this is a false positive worth fixing rather than a codegen regression.",
        redundant_casts
    );
}
