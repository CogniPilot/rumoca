//! GALEC three-leg differential-execution harness (SPEC_0034 GAL-027/GAL-038).
// SPEC_0021 file-size exception: the three-leg harness and its fixture catalog
// remain together to share one checked package. split plan: move model fixture
// definitions and CSV comparison helpers into dedicated test support modules.
//!
//! Every fixture is executed three independent ways and all three are compared
//! tick-for-tick:
//!
//! 1. **Generated C.** The `embedded-c-galec` artifacts are rendered
//!    in-process from the fixture's projected package; the generated C is
//!    compiled (`cc -Wall -Werror`), linked (`-lm`), and run through a
//!    per-fixture driver that prints every compared block output plus the
//!    eFMI `ErrorSignalStatus` as one CSV line after `startup`, after
//!    `recalibrate`, and per `dostep` tick.
//! 2. **The `rumoca-eval-galec` oracle.** The checked `AlgorithmCodePackage`
//!    the C template renders (`target.toml` declares `ir =
//!    "algorithm-code"`) is interpreted in-process over the same
//!    `Startup`/`Recalibrate`/`DoStep` schedule. This is the independent
//!    proof oracle GAL-027 requires; without a consumer it never ran, and a
//!    C-vs-reference comparison alone is self-confirming for any defect the
//!    projection and the template share.
//! 3. **The Modelica reference.** The fixture is simulated in-process by
//!    Rumoca's own solver (`rumoca_sim::simulate_dae`) — checked source
//!    semantics — and read at the aligned sample time.
//!
//! **One projection, one shared object (be precise about what leg 2 proves).**
//! Each test lowers its fixture exactly once ([`project_embedded_c`]) and
//! keeps the resulting [`AlgorithmCodePackage`] alive. The C that leg 1
//! compiles is rendered from *that* package handle, through the same
//! `AlgorithmCodeTemplateRenderer` + `[[files]]` walk `rumoca`'s
//! `render_target_files` drives for an `ir = "algorithm-code"` target; leg 2's
//! `Evaluator` is built from the very same package's checked block. A
//! C-vs-oracle divergence is therefore a divergence between the template's C
//! and explicit GALEC semantics over one shared object, and can never be an
//! artifact of two independent lowerings.
//!
//! **The CLI is a separate plumbing smoke.** Because the executing legs no
//! longer run the CLI, every fixture additionally runs `rumoca compile
//! --target embedded-c-galec` once in a child process and byte-compares each
//! emitted file against the in-process render
//! ([`assert_cli_emits_the_rendered_bytes`]); a mismatch fails with a diff
//! excerpt. That equality is what carries the differential result over to the
//! shipped path: it proves the CLI's projection → renderer plumbing produced
//! exactly the artifact the two executing legs were compared on. It is a
//! byte comparison and nothing more — CLI behaviour *after* the write
//! (packaging, completion messages, exit-code policy, diagnostics) is
//! `cli_target_embedded_c_galec`'s job, not this harness's.
//!
//! **Alignment (empirically grounded against `simulate_dae`).** The block
//! life-cycle is `startup` (seeds state) then one `dostep` per fixed sample
//! tick. Both executing legs are compared after `startup` and after
//! `recalibrate` as well, so a seeding or re-tuning divergence cannot hide in
//! the un-observed prologue. For `when sample(t0, period)` the first tick
//! fires *at* `t0`
//! (verified: `sample(0.0, 0.1)` makes `count = 1` at `t = 0`), so the
//! j-th `dostep` (j = 1..N) corresponds to the sample at `t_j = t0 +
//! (j-1)*period`. The reference is read at the strictly-interior hold-
//! interval midpoint `t0 + (j-1)*period + period/2` via a right-continuous
//! hold, so both sides carry the post-tick value regardless of where the
//! solver plants event rows. The startup seed is never compared against the
//! reference (for a `t0 = 0` model no reference row equals it).
//!
//! `cc` is a hard dependency: a missing compiler FAILS, never skips
//! (SPEC_0034 GAL-012). The generated condition vector `c[1]` is
//! projection-internal and absent from the emitted C, so it is never
//! compared — only manifest outputs/states that exist as C struct fields.
//!
//! **Signal/error channel.** Every driver row ends with
//! `self->rumoca_galec_error_signal_status`, and the oracle's
//! `active_signals()` set is folded into that same bit encoding, so the
//! §3.2.5 signal machinery is compared on every fixture and every lifecycle
//! observation — not only on the fixture that raises one
//! (`embedded_c_nan_comparison_signals_match_the_galec_oracle`). This channel
//! exists on the two executing legs only: `simulate_dae` has no
//! `ErrorSignalStatus`, so the reference leg is compared on values alone and
//! is never evidence about signals.
//!
//! Fixtures here are original works authored for the Rumoca test suite; no
//! third-party Modelica sources (e.g. the Modelica Standard Library) are
//! copied. The IIR difference equation `y[k] = a*y[k-1] + b*u[k]` is a
//! standard textbook form, not copyrightable content.

use std::borrow::Cow;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use rumoca_compile::codegen::targets::{TargetBundle, TargetTemplateSource};
use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
use rumoca_ir_galec::package::AlgorithmCodePackage;
use rumoca_phase_codegen::AlgorithmCodeTemplateRenderer;
use rumoca_phase_galec::{GalecInput, GalecOptions, lower_to_algorithm_code};
use rumoca_sim::{SimOptions, SimResult, simulate_dae};

// The shared helpers are declared once by the umbrella binary
// that owns this file (see `suite_galec_fmu/main.rs`), so the sibling suites share one
// copy instead of compiling the same file several times per binary.
use super::{cc_support, cli_support};

/// How a compared block field is checked against the reference.
#[derive(Clone, Copy)]
enum FieldKind {
    /// Discrete Integer state: exact on both sides (`i64` round-trip).
    Integer,
    /// Discrete Real state: `|c - ref| <= ATOL + RTOL*|ref|`.
    Real,
}

/// A block output compared per tick: its reference name in `SimResult.names`
/// (equal to the C struct field name for these fixtures) and its check kind.
///
/// The same string addresses the GALEC declaration in the oracle: a Modelica
/// element name `y[1]` is the checked GALEC array `y` at one-based element 1
/// (see [`split_subscript`]).
struct Field {
    name: &'static str,
    kind: FieldKind,
}

const ATOL: f64 = 2.0e-6;
const RTOL: f64 = 2.0e-6;

/// The target both executing legs are built from. The C leg passes it to
/// `rumoca compile --target`; the oracle reads its manifest for the Integer
/// domain (see [`target_integer_domain`]), so one string names one target.
const EMBEDDED_C_TARGET: &str = "embedded-c-galec";

/// Tolerance for the `rumoca-eval-galec` <-> generated-C leg.
///
/// **This is not float32 profile parity, and this harness must not be read as
/// evidence of it.** SPEC_0034 GAL-024 requires one float32-profile evaluator
/// that rounds *every* Real operation; `rumoca-eval-galec` does not implement
/// that rounding — `Value::Real` is an `f64` and
/// `interpreter.rs::scalar_binary` applies bare `f64` `+ - * / powf`. The
/// generated C, by contrast, is binary32 throughout: `float` struct fields and
/// locals, `f`-suffixed Real literals, and the `f` math variants
/// (`powf`/`sqrtf`/`sinf`/…) selected by `embedded-c-galec/model.c.jinja`.
///
/// So the two legs differ by *accumulated* binary32 representation error over
/// a tick schedule, not by a single rounding, and the honest bound is
/// proportional to `f32::EPSILON`. Measured worst case over these fixtures'
/// tick counts is ~5e-8 relative (the IIR at tick 4); 8 ulp of binary32 keeps
/// a ~20x margin without hiding a real semantic divergence — a wrong
/// recurrence, a dropped statement, or a mis-ordered clock lattice all move
/// the value by far more than this. Tighten it to exact equality the moment
/// GAL-024's rounding evaluator lands; do NOT loosen it to make a fixture
/// pass.
const F32_PROFILE_ATOL: f64 = 8.0 * f32::EPSILON as f64;
const F32_PROFILE_RTOL: f64 = 8.0 * f32::EPSILON as f64;

/// The eFMI predefined `ErrorSignalStatus` bits, in the exact encoding
/// `embedded-c-galec/model.c.jinja` emits into
/// `self->rumoca_galec_error_signal_status`. The oracle reports signals by
/// name, so this table is what makes the two legs comparable.
const SIGNAL_BITS: [(&str, u32); 6] = [
    ("INVALID_ARGUMENT", 1),
    ("OVERFLOW", 2),
    ("NAN", 4),
    ("SOLVE_LINEAR_EQUATIONS_FAILED", 8),
    ("NO_SOLUTION_FOUND", 16),
    ("UNSPECIFIED_ERROR", 32),
];

/// One block-method return as observed at an executing leg: the compared field
/// values in `Field` order plus the `ErrorSignalStatus` bitmask that method
/// raised. Both the generated C and the oracle report in this shape, after
/// `Startup`, after `Recalibrate`, and after every `DoStep`.
struct Tick {
    values: Vec<f64>,
    status: u32,
}

/// One executing leg's whole block life-cycle. `Startup` and `Recalibrate`
/// are observations in their own right — a block that seeds the wrong state or
/// re-tunes it differently in C than in GALEC is a real divergence even when
/// the `DoStep` recurrence later washes it out — so they are compared C vs
/// oracle just like the ticks. They are not compared against the Modelica
/// reference: `simulate_dae` has no block life-cycle to align them to (see the
/// module docs).
struct Run {
    startup: Tick,
    recalibrate: Tick,
    steps: Vec<Tick>,
}

/// Reference value of `name` at the last recorded sample at or before `t`
/// (right-continuous hold). Panics if `name` is absent — a missing signal is
/// a hard failure, never a silent skip (SPEC_0008).
fn value_at(sim: &SimResult, name: &str, t: f64) -> f64 {
    let idx = sim
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("reference must record `{name}`; names = {:?}", sim.names));
    let values = &sim.data[idx];
    let mut held = values[0];
    for (i, &time) in sim.times.iter().enumerate() {
        if time <= t + 1.0e-9 {
            held = values[i];
        } else {
            break;
        }
    }
    held
}

// ===========================================================================
// The single projection both executing legs consume.
// ===========================================================================

/// One rendered `[[files]]` entry: the rendered output `path` (relative to the
/// output directory) and the exact text the template produced.
struct RenderedFile {
    path: String,
    content: String,
}

/// A fixture lowered **once** into a checked GALEC package, together with the
/// `embedded-c-galec` artifacts rendered from that same package handle.
///
/// Both executing legs read out of this one value: [`write_rendered`] puts
/// `files` on disk for the C leg to compile, and [`oracle_ticks`] builds its
/// `Evaluator` on `package`'s checked block. Nothing here re-lowers.
struct Projection {
    /// The on-disk fixture this package was lowered from. The CLI smoke
    /// compiles *this* path, so both sides see one source identity
    /// (`SourceId::from_source_name` hashes the file name, and the generated
    /// C prints it in its `Modelica trace:` comments).
    fixture: PathBuf,
    package: AlgorithmCodePackage,
    files: Vec<RenderedFile>,
}

impl Projection {
    /// The rendered text of one artifact, by its rendered path. A missing
    /// path is a hard failure — an in-source assertion that silently matched
    /// nothing would be worse than no assertion.
    fn rendered(&self, path: &str) -> &str {
        self.files
            .iter()
            .find(|file| file.path == path)
            .map(|file| file.content.as_str())
            .unwrap_or_else(|| {
                panic!(
                    "no rendered artifact `{path}`; rendered = {:?}",
                    self.files.iter().map(|f| &f.path).collect::<Vec<_>>()
                )
            })
    }
}

/// The `embedded-c-galec` bundle, shared by the renderer and the Integer
/// domain reader so one target string names one target.
fn target_bundle() -> TargetBundle {
    TargetBundle::builtin(EMBEDDED_C_TARGET)
        .unwrap_or_else(|| panic!("`{EMBEDDED_C_TARGET}` must be a builtin target"))
}

/// Write the fixture into `work_dir`, lower it to a checked GALEC package, and
/// render the target's artifacts from that exact package instance.
///
/// The fixture goes to disk first and is compiled *by path* rather than from
/// the in-memory string, because the CLI smoke must be able to hand the same
/// path to `rumoca compile`: the source name is what fixes `SourceId`, and the
/// generated C embeds it in every `Modelica trace:` comment, so a different
/// spelling would make the byte comparison fail on identity rather than on
/// plumbing.
///
/// Every failure is a hard panic: a fixture that will not compile, will not
/// project, or will not render is not a skip.
fn project_embedded_c(work_dir: &Path, model: &str, source: &str) -> Projection {
    let fixture = cli_support::write_fixture(work_dir, model, source);
    let compiled = rumoca::Compiler::new()
        .model(model)
        .compile_path(&fixture)
        .expect("fixture model should compile");
    let package = lower_to_algorithm_code(
        &GalecInput::new(&compiled.dae, model),
        &GalecOptions::default(),
    )
    .unwrap_or_else(|errors| panic!("GALEC projection must accept {model}: {errors:?}"));
    let files = render_target_artifacts(&package, compiled.dae.source_map(), model);
    Projection {
        fixture,
        package,
        files,
    }
}

/// Render every `[[files]]` entry the `embedded-c-galec` manifest declares
/// from one renderer over `package` — the in-process twin of the
/// `AlgorithmCode` arm of `rumoca`'s `render_target_files`, which builds the
/// same `AlgorithmCodeTemplateRenderer` and renders each entry's `path`
/// template and body template through it.
///
/// The artifact facts (`ArtifactRenderContext`: session identities, checksum
/// edges) are `()` here because they live behind `rumoca`'s private packaging
/// module. That substitution is fail-closed rather than assumed: the render
/// environment is strict-undefined, so a template that reads any artifact
/// field errors instead of rendering a blank, and
/// [`assert_cli_emits_the_rendered_bytes`] byte-compares this render against
/// the CLI's — which does pass the real facts — on every fixture.
fn render_target_artifacts(
    package: &AlgorithmCodePackage,
    sources: &rumoca_core::SourceMap,
    model: &str,
) -> Vec<RenderedFile> {
    let bundle = target_bundle();
    let manifest = bundle
        .parse_manifest()
        .unwrap_or_else(|error| panic!("parse `{EMBEDDED_C_TARGET}` target.toml: {error}"));
    assert!(
        !manifest.files.is_empty(),
        "`{EMBEDDED_C_TARGET}` must declare rendered [[files]]"
    );
    let renderer = AlgorithmCodeTemplateRenderer::new(package, sources).unwrap_or_else(|error| {
        panic!("{model}: checked package must build a template renderer: {error}")
    });
    manifest
        .files
        .iter()
        .map(|file| RenderedFile {
            path: render_one(&renderer, &bundle, &file.path, model)
                .trim()
                .to_owned(),
            content: render_one(&renderer, &bundle, &file.template, model),
        })
        .collect()
}

/// Render one manifest string: a `[[files]] template` names a bundle
/// template, while a `[[files]] path` is itself the template text (the same
/// resolve-or-inline rule the shipped `algorithm_code_web_render` uses).
fn render_one(
    renderer: &AlgorithmCodeTemplateRenderer,
    bundle: &TargetBundle,
    template_or_path: &str,
    model: &str,
) -> String {
    let source = bundle
        .template_source(template_or_path)
        .unwrap_or(Cow::Borrowed(template_or_path));
    renderer
        .render_with_name_and_artifact(source.as_ref(), model, &())
        .unwrap_or_else(|error| panic!("{model}: render `{template_or_path}`: {error}"))
}

/// Put the in-process render on disk for the C leg, asserting the
/// `<Model>.h` / `<Model>.c` pair the drivers `#include` is present.
fn write_rendered(out_dir: &Path, model: &str, files: &[RenderedFile]) {
    fs::create_dir_all(out_dir).expect("create render output directory");
    for file in files {
        fs::write(out_dir.join(&file.path), &file.content)
            .unwrap_or_else(|error| panic!("write rendered `{}`: {error}", file.path));
    }
    for ext in ["h", "c"] {
        let path = out_dir.join(format!("{model}.{ext}"));
        assert!(path.is_file(), "missing generated {}", path.display());
    }
}

/// **CLI plumbing smoke (not one of the three legs).** Run the real
/// `rumoca compile --target embedded-c-galec` once, on the very fixture path
/// the projection was taken from, and require its emitted bytes to equal the
/// in-process render the executing legs were compared on.
///
/// Equality is the whole claim: it proves the CLI's own projection →
/// renderer path produced the same artifact, so the differential result above
/// transfers to the shipped path. Anything the CLI does after writing those
/// bytes is out of scope here.
fn assert_cli_emits_the_rendered_bytes(projection: &Projection, model: &str) {
    let work_dir = projection
        .fixture
        .parent()
        .expect("fixture path has a directory");
    let cli_out = work_dir.join("cli-out");
    let output = cli_support::run_compile_target(&projection.fixture, EMBEDDED_C_TARGET, &cli_out);
    assert!(
        output.status.success(),
        "`compile --target {EMBEDDED_C_TARGET}` failed for {model} (status {:?}).\nstderr:\n{}",
        output.status.code(),
        cli_support::strip_ansi(&String::from_utf8_lossy(&output.stderr))
    );
    for file in &projection.files {
        let path = cli_out.join(&file.path);
        let emitted = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("CLI must emit {}: {error}", path.display()));
        assert!(
            emitted == file.content,
            "CLI-emitted `{}` differs from the in-process render the legs were \
             compared on (CLI projection/renderer plumbing regression):\n{}",
            file.path,
            first_difference(&file.content, &emitted)
        );
    }
}

/// A short, readable excerpt of where two renders first disagree.
fn first_difference(rendered: &str, emitted: &str) -> String {
    for (index, (want, got)) in rendered.lines().zip(emitted.lines()).enumerate() {
        if want != got {
            return format!(
                "  first differing line {}:\n    in-process: {}\n    CLI       : {}",
                index + 1,
                excerpt(want),
                excerpt(got)
            );
        }
    }
    format!(
        "  line counts differ: in-process {} vs CLI {} (common prefix identical)",
        rendered.lines().count(),
        emitted.lines().count()
    )
}

/// Clip one diff line so a long generated statement cannot bury the message.
fn excerpt(line: &str) -> String {
    const LIMIT: usize = 160;
    match line.char_indices().nth(LIMIT) {
        Some((cut, _)) => format!("{}…", &line[..cut]),
        None => line.to_owned(),
    }
}

/// Build+link the driver against the generated source and run it, returning
/// the whole observed life-cycle. Every driver prints
/// `<label>,<field0>,<field1>,…,<ErrorSignalStatus>`: first `startup`, then
/// `recalibrate`, then one row per `dostep` labelled with the 0-based tick
/// index. Labels are asserted against the expected sequence so a dropped,
/// duplicated, or reordered method call fails loudly.
///
/// **Both emitted translation units are linked.** The generated model source
/// is not self-contained: every whole-array move, fill and inner product in it
/// is a call into `rumoca_galec_kernels.c`, the model-independent kernel
/// library the target emits beside it (`target.toml` declares both, and its
/// completion message tells an integrator to compile both). Linking only the
/// model unit is what an integrator would get wrong, and leaving that out of
/// this harness let an unresolved kernel call read as a fixture failure.
fn run_c_ticks(out_dir: &Path, model: &str, driver: &str, n_fields: usize) -> Run {
    let driver_path = out_dir.join("main.c");
    fs::write(&driver_path, driver).expect("write driver");
    let program = out_dir.join("equiv_block");
    let source = out_dir.join(format!("{model}.c"));
    let kernels = out_dir.join(cc_support::GALEC_KERNEL_LIBRARY);
    assert!(
        kernels.is_file(),
        "the kernel library {} must be emitted beside the model unit",
        kernels.display()
    );

    let compile = cc_support::cc()
        .arg("-std=c99")
        .arg("-pedantic")
        .arg("-Wall")
        .arg("-Wextra")
        .arg("-Wconversion")
        .arg("-Wsign-conversion")
        .arg("-Werror")
        .arg("-o")
        .arg(&program)
        .arg(&driver_path)
        .arg(&source)
        .arg(&kernels)
        .arg("-lm")
        .output()
        .expect("run cc");
    assert!(
        compile.status.success(),
        "strict cc -std=c99 compile failed for {model}.\nstderr:\n{}\nsource:\n{}",
        String::from_utf8_lossy(&compile.stderr),
        fs::read_to_string(&source).unwrap_or_default()
    );

    let run = Command::new(&program)
        .output()
        .expect("run generated block");
    assert!(
        run.status.success(),
        "generated block driver for {model} exited with {:?}",
        run.status.code()
    );
    let stdout = String::from_utf8(run.stdout).expect("driver stdout is UTF-8");
    let mut rows = stdout.lines();
    let startup = parse_c_row(rows.next(), "startup", n_fields);
    let recalibrate = parse_c_row(rows.next(), "recalibrate", n_fields);
    let steps = rows
        .enumerate()
        .map(|(k, line)| parse_c_row(Some(line), &k.to_string(), n_fields))
        .collect();
    Run {
        startup,
        recalibrate,
        steps,
    }
}

/// Parse one driver row into a [`Tick`], checking its label is exactly the
/// `expected` one (`startup`, `recalibrate`, or the decimal `dostep` index).
/// Every column is mandatory: a missing row, a missing value, a short row, or
/// a stray extra column is a hard failure, never a silently dropped channel.
fn parse_c_row(line: Option<&str>, expected: &str, n_fields: usize) -> Tick {
    let line = line.unwrap_or_else(|| panic!("driver printed no `{expected}` row (short output)"));
    let mut columns = line.split(',');
    let label = columns
        .next()
        .unwrap_or_else(|| panic!("driver row `{expected}` is empty"));
    assert_eq!(
        label, expected,
        "driver row out of order (dropped/duplicated method call): {line:?}"
    );
    let values: Vec<f64> = (0..n_fields)
        .map(|i| {
            columns
                .next()
                .and_then(|s| s.parse().ok())
                .unwrap_or_else(|| {
                    panic!("driver row `{expected}` field {i} is missing/unparseable: {line:?}")
                })
        })
        .collect();
    let status: u32 = columns
        .next()
        .and_then(|s| s.parse().ok())
        .unwrap_or_else(|| {
            panic!("driver row `{expected}` has no ErrorSignalStatus column: {line:?}")
        });
    assert!(
        columns.next().is_none(),
        "driver row `{expected}` has more than {n_fields} fields + status: {line:?}"
    );
    Tick { values, status }
}

/// The in-process Rumoca reference: compile the fixture and simulate it, then
/// read each field at the aligned midpoint of every tick's hold interval.
fn reference_ticks(
    model: &str,
    source: &str,
    fields: &[Field],
    t0: f64,
    period: f64,
    n_ticks: usize,
) -> Vec<Vec<f64>> {
    let compiled = rumoca::Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .expect("reference model should compile");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            // Strictly past tick N so its hold interval is fully recorded;
            // tick N+1 lands on the boundary and is never queried.
            t_end: t0 + (n_ticks as f64) * period,
            dt: Some(period / 5.0),
            ..SimOptions::default()
        },
    )
    .expect("reference model should simulate");

    (1..=n_ticks)
        .map(|j| {
            let t = t0 + (j as f64 - 1.0) * period + period / 2.0;
            fields.iter().map(|f| value_at(&sim, f.name, t)).collect()
        })
        .collect()
}

/// Assert the C block reproduces the reference tick-for-tick, and that the
/// sequence is non-vacuous (strictly advancing on the first field, so a
/// seed-only or constant block cannot pass).
///
/// Each test calls [`assert_oracle_agrees`] *before* this: the oracle-vs-C leg
/// is the one GAL-027 adds, and running it first keeps it reported even while
/// a reference-leg alignment defect is outstanding, instead of being masked by
/// the earlier panic.
fn assert_equivalent(c_run: &Run, ref_ticks: &[Vec<f64>], fields: &[Field]) {
    let c_ticks = &c_run.steps;
    assert_eq!(c_ticks.len(), ref_ticks.len(), "tick count mismatch");
    assert!(c_ticks.len() >= 5, "need >= 5 ticks to be non-vacuous");

    for (j, (c_tick, ref_row)) in c_ticks.iter().zip(ref_ticks).enumerate() {
        let c_row = &c_tick.values;
        for (field, (&c, &r)) in fields.iter().zip(c_row.iter().zip(ref_row)) {
            match field.kind {
                FieldKind::Integer => {
                    assert!(
                        (r - r.round()).abs() < 1.0e-9,
                        "reference `{}` not integral at tick {}: {r}",
                        field.name,
                        j + 1
                    );
                    assert_eq!(
                        c as i64,
                        r.round() as i64,
                        "tick {} field `{}`: C {c} != reference {r}",
                        j + 1,
                        field.name
                    );
                }
                FieldKind::Real => {
                    let delta = (c - r).abs();
                    assert!(
                        delta <= ATOL + RTOL * r.abs(),
                        "tick {} field `{}`: C {c} vs reference {r}, delta {delta} exceeds tolerance",
                        j + 1,
                        field.name
                    );
                }
            }
        }
    }

    // Non-vacuous: the first compared field strictly advances across ticks,
    // so a constant/seed-only block (which would trivially "match" an initial
    // value) cannot pass this harness.
    for pair in c_ticks.windows(2) {
        assert!(
            pair[1].values[0] > pair[0].values[0],
            "first field must strictly advance (non-vacuous check): {:?} -> {:?}",
            pair[0].values[0],
            pair[1].values[0]
        );
    }
}

// ===========================================================================
// Leg 2 — the independent `rumoca-eval-galec` oracle (SPEC_0034 GAL-027).
// ===========================================================================

/// The Integer domain the oracle must compute in, read from the *same*
/// `embedded-c-galec/target.toml` the C leg was rendered from (GAL-028).
///
/// Hardcoding `IntegerDomain::signed_32()` here would silently keep the oracle
/// on int32 semantics after a manifest edit moved the target's declared range,
/// so the harness would stop testing what the target actually promises. Both
/// failure modes below are hard panics: a target with no `[integer]` table
/// makes no domain promise at all, and a table the evaluator's checked
/// constructor rejects (it requires `min <= 0 <= max`, `min < max`) is a
/// manifest defect, not something to paper over with a default.
fn target_integer_domain() -> IntegerDomain {
    let bundle = target_bundle();
    let manifest = bundle
        .parse_manifest()
        .unwrap_or_else(|error| panic!("parse `{EMBEDDED_C_TARGET}` target.toml: {error}"));
    let declared = manifest.integer.unwrap_or_else(|| {
        panic!("`{EMBEDDED_C_TARGET}` must declare an [integer] domain (GAL-028)")
    });
    IntegerDomain::new(declared.minimum, declared.maximum).unwrap_or_else(|| {
        panic!(
            "`{EMBEDDED_C_TARGET}` declares an Integer domain the evaluator rejects: [{}, {}]",
            declared.minimum, declared.maximum
        )
    })
}

/// Interpret the checked `AlgorithmCodePackage` the `embedded-c-galec`
/// template renders, over the same lifecycle and tick schedule as the C
/// driver: one [`Tick`] after `Startup`, one after `Recalibrate`, and one per
/// `DoStep`.
///
/// `package` is the *same* [`AlgorithmCodePackage`] instance the C leg's
/// sources were rendered from ([`Projection`]), so nothing here re-lowers and
/// no cross-lowering determinism argument is needed. Every failure is a hard
/// panic — a lifecycle error or a typed `EvaluationError` both mean the oracle
/// could not run, which is exactly the state GAL-027 exists to detect.
fn oracle_ticks(
    package: &AlgorithmCodePackage,
    model: &str,
    fields: &[Field],
    n_ticks: usize,
) -> Run {
    oracle_ticks_driven(package, model, fields, n_ticks, None)
}

/// One externally driven Real declaration, written at exactly the points a C
/// driver can write the matching struct field.
///
/// Block-owned state is not writable from outside on either leg — the
/// oracle's `set_state` rejects it, and a C driver that poked one would be
/// testing something the eFMI contract does not allow. An externally written
/// declaration (a control input or a tunable parameter) is therefore the only
/// channel through which a test can present a method with state it must
/// saturate on ENTRY rather than on return.
struct InputSchedule {
    /// The GALEC declaration name; the C driver writes the same field.
    name: &'static str,
    /// Written before the k-th `DoStep`, one value per tick.
    before_step: Vec<f64>,
}

fn oracle_ticks_driven(
    package: &AlgorithmCodePackage,
    model: &str,
    fields: &[Field],
    n_ticks: usize,
    inputs: Option<&InputSchedule>,
) -> Run {
    let mut evaluator = Evaluator::new(package.checked_block(), target_integer_domain())
        .unwrap_or_else(|error| panic!("{model}: checked block must build an oracle: {error}"));
    if let Some(schedule) = inputs {
        assert_eq!(
            schedule.before_step.len(),
            n_ticks,
            "{model}: the input schedule must cover every tick"
        );
    }
    evaluator
        .startup()
        .unwrap_or_else(|error| panic!("{model}: oracle Startup failed: {error}"));
    let startup = oracle_tick(&evaluator, model, fields);
    evaluator
        .recalibrate()
        .unwrap_or_else(|error| panic!("{model}: oracle Recalibrate failed: {error}"));
    let recalibrate = oracle_tick(&evaluator, model, fields);

    let steps = (0..n_ticks)
        .map(|k| {
            if let Some(schedule) = inputs {
                evaluator
                    .set_state(schedule.name, Value::Real(schedule.before_step[k]))
                    .unwrap_or_else(|error| {
                        panic!("{model}: oracle drives `{}`: {error}", schedule.name)
                    });
            }
            evaluator
                .do_step()
                .unwrap_or_else(|error| panic!("{model}: oracle DoStep {k} failed: {error}"));
            oracle_tick(&evaluator, model, fields)
        })
        .collect();
    Run {
        startup,
        recalibrate,
        steps,
    }
}

/// Snapshot every compared channel plus the signal bitmask the method just
/// returned from raised.
fn oracle_tick(evaluator: &Evaluator<'_>, model: &str, fields: &[Field]) -> Tick {
    Tick {
        values: fields
            .iter()
            .map(|field| oracle_field(evaluator, model, field))
            .collect(),
        status: oracle_status(evaluator, model),
    }
}

/// Read one compared channel out of the oracle's block state.
fn oracle_field(evaluator: &Evaluator<'_>, model: &str, field: &Field) -> f64 {
    let (name, index) = split_subscript(field.name);
    let state = evaluator
        .state(name)
        .unwrap_or_else(|error| panic!("{model}: oracle state `{name}`: {error}"));
    let value = match index {
        Some(index) => oracle_element(state, index, model, field.name),
        None => state,
    };
    match value {
        Value::Integer(value) => *value as f64,
        Value::Real(value) => *value,
        other => panic!(
            "{model}: compared channel `{}` is not numeric: {other:?}",
            field.name
        ),
    }
}

/// Split a Modelica element name (`y[1]`) into the GALEC array declaration it
/// belongs to and its one-based element. Anything that is not a plain rank-1
/// subscript is a hard failure rather than a silently unchecked channel.
fn split_subscript(name: &str) -> (&str, Option<usize>) {
    let Some((base, rest)) = name.split_once('[') else {
        return (name, None);
    };
    let index = rest
        .strip_suffix(']')
        .and_then(|text| text.parse::<usize>().ok())
        .filter(|index| *index >= 1)
        .unwrap_or_else(|| panic!("compared channel `{name}` has no one-based rank-1 subscript"));
    (base, Some(index))
}

fn oracle_element<'a>(value: &'a Value, index: usize, model: &str, name: &str) -> &'a Value {
    let Value::Array(elements) = value else {
        panic!("{model}: oracle state `{name}` is not an array: {value:?}");
    };
    elements
        .get(index - 1)
        .unwrap_or_else(|| panic!("{model}: oracle state `{name}` has no element {index}"))
}

/// Fold the oracle's active signal set into the `uint32_t` bitmask the
/// generated C accumulates, so the two legs' signal channels are directly
/// comparable. An unmapped signal name is a hard failure: silently dropping
/// it would let a real §3.2.5 divergence read as agreement on `0`.
fn oracle_status(evaluator: &Evaluator<'_>, model: &str) -> u32 {
    evaluator
        .active_signals()
        .iter()
        .map(|signal| {
            SIGNAL_BITS
                .iter()
                .find(|(name, _)| name == signal)
                .map(|(_, bit)| *bit)
                .unwrap_or_else(|| {
                    panic!("{model}: oracle raised `{signal}`, which the C template cannot encode")
                })
        })
        .fold(0, |status, bit| status | bit)
}

/// No fixture in this file raises a signal outside `DoStep`, so both executing
/// legs must report a clear `ErrorSignalStatus` after `Startup` and after
/// `Recalibrate`. Anchoring that absolute keeps two distinct defects visible:
/// a template that forgets to clear the accumulator at method entry, and an
/// oracle that leaks a `DoStep` signal into a neighbouring method.
const LIFECYCLE_STATUS: u32 = 0;

/// Assert the oracle agrees with both other legs on every compared channel and
/// on the `ErrorSignalStatus` of every observation (GAL-027/GAL-038), and
/// anchor the absolute status so "both legs quietly report 0" cannot pass for
/// a fixture that must signal.
///
/// `Startup` and `Recalibrate` are compared oracle-vs-C only; the Modelica
/// reference has no counterpart to align them to (see the module docs).
///
/// The two legs are checked in two separate passes, executing-legs first, on
/// purpose: an outstanding reference-alignment defect must not mask the
/// oracle-vs-C result for the *later* ticks, which one interleaved loop would
/// do by panicking on tick 1's reference comparison.
fn assert_oracle_agrees(
    oracle: &Run,
    c_run: &Run,
    ref_ticks: &[Vec<f64>],
    fields: &[Field],
    expected_status: u32,
) {
    // Pass 1 — the two executing legs, over the whole life-cycle.
    assert_executing_legs_agree(oracle, c_run, fields, expected_status);

    // Pass 2 — the oracle against the Modelica reference, ticks only.
    assert_eq!(
        oracle.steps.len(),
        ref_ticks.len(),
        "oracle/reference tick count mismatch"
    );
    for (j, (tick, ref_row)) in oracle.steps.iter().zip(ref_ticks).enumerate() {
        let label = format!("tick {}", j + 1);
        for (i, field) in fields.iter().enumerate() {
            assert_channel(
                field,
                &label,
                "reference",
                tick.values[i],
                ref_row[i],
                F64_LEG,
            );
        }
    }
}

/// Pass 1 of [`assert_oracle_agrees`] on its own: the two *executing* legs —
/// the `rumoca-eval-galec` oracle and the generated C — over the whole block
/// life-cycle, values and `ErrorSignalStatus`.
///
/// Split out because one fixture is deliberately gated on these two legs
/// alone: `LimitSmoke` compares `ySat` oracle-vs-C-vs-closed-form because
/// saturating ranges are precisely where GALEC and Modelica differ (SPEC_0042
/// T3), so `simulate_dae` is not an oracle for that channel. Every other
/// fixture reaches this through [`assert_oracle_agrees`] and keeps all three.
fn assert_executing_legs_agree(oracle: &Run, c_run: &Run, fields: &[Field], expected_status: u32) {
    assert_eq!(
        oracle.steps.len(),
        c_run.steps.len(),
        "oracle/C tick count mismatch"
    );

    let ticks = oracle
        .steps
        .iter()
        .zip(&c_run.steps)
        .enumerate()
        .map(|(j, (tick, c_tick))| (format!("tick {}", j + 1), tick, c_tick, expected_status));
    let lifecycle = [
        (
            "startup".to_owned(),
            &oracle.startup,
            &c_run.startup,
            LIFECYCLE_STATUS,
        ),
        (
            "recalibrate".to_owned(),
            &oracle.recalibrate,
            &c_run.recalibrate,
            LIFECYCLE_STATUS,
        ),
    ];
    for (label, tick, c_tick, status) in lifecycle.into_iter().chain(ticks) {
        assert_status(&label, tick, c_tick, status);
        for (i, field) in fields.iter().enumerate() {
            assert_channel(
                field,
                &label,
                "C",
                tick.values[i],
                c_tick.values[i],
                F32_PROFILE,
            );
        }
    }
}

/// Both executing legs must raise exactly `expected` at this observation.
fn assert_status(label: &str, oracle: &Tick, c_tick: &Tick, expected: u32) {
    assert_eq!(
        oracle.status, expected,
        "{label}: oracle ErrorSignalStatus {} != expected {expected}",
        oracle.status
    );
    assert_eq!(
        c_tick.status, expected,
        "{label}: C ErrorSignalStatus {} != expected {expected}",
        c_tick.status
    );
}

/// `(atol, rtol)` for one comparison leg.
type Tolerance = (f64, f64);

/// Oracle vs generated C: f64 evaluator against binary32 C (see
/// [`F32_PROFILE_ATOL`]).
const F32_PROFILE: Tolerance = (F32_PROFILE_ATOL, F32_PROFILE_RTOL);
/// Oracle vs the Modelica reference: both legs evaluate in f64, but through
/// different engines (checked GALEC block vs the DAE solver), so the shared
/// reference tolerance applies.
const F64_LEG: Tolerance = (ATOL, RTOL);

fn assert_channel(field: &Field, at: &str, leg: &str, got: f64, want: f64, tol: Tolerance) {
    match field.kind {
        FieldKind::Integer => assert_eq!(
            got as i64,
            want.round() as i64,
            "{at} channel `{}`: oracle {got} != {leg} {want}",
            field.name
        ),
        FieldKind::Real => {
            let (atol, rtol) = tol;
            let bound = atol + rtol * want.abs();
            let delta = (got - want).abs();
            assert!(
                delta <= bound,
                "{at} channel `{}`: oracle {got} vs {leg} {want}, delta {delta} exceeds {bound}",
                field.name
            );
        }
    }
}

// ===========================================================================
// Fixture 1 — Integer counter: exact match.
// ===========================================================================

const EQUIV_COUNTER: &str = r#"
model EquivCounter
  constant Real samplePeriod = 0.1;
  discrete Integer count(start = 0, fixed = true);
equation
  when sample(0.0, samplePeriod) then
    count = pre(count) + 1;
  end when;
end EquivCounter;
"#;

// Every driver row is `<label>,<fields…>,<ErrorSignalStatus>`: the leading
// label is `startup`, `recalibrate`, or the 0-based dostep index, so the
// prologue methods are observed and not just the ticks; the trailing column is
// the eFMI ErrorSignalStatus, so the §3.2.5 signal channel is compared on
// every fixture (see the module docs). `%lu` plus an explicit `unsigned long`
// cast keeps the print portable under `-Wconversion -Wsign-conversion
// -Werror`, and `snprintf` into a fixed buffer renders the tick label without
// duplicating the row format per call site.
const COUNTER_DRIVER: &str = r#"#include <stdio.h>
#include "EquivCounter.h"
static void row(const char *label, const EquivCounterState *state) {
    printf("%s,%d,%lu\n", label, (int)state->count,
           (unsigned long)state->rumoca_galec_error_signal_status);
}
int main(void) {
    EquivCounterState state;
    char label[16];
    EquivCounter_startup(&state);
    row("startup", &state);
    EquivCounter_recalibrate(&state);
    row("recalibrate", &state);
    for (int step = 0; step < 5; ++step) {
        EquivCounter_dostep(&state);
        snprintf(label, sizeof label, "%d", step);
        row(label, &state);
    }
    return 0;
}
"#;

#[test]
fn embedded_c_counter_matches_rumoca_evaluation_exactly() {
    let dir = tempfile::tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let fields = [Field {
        name: "count",
        kind: FieldKind::Integer,
    }];

    let projection = project_embedded_c(dir.path(), "EquivCounter", EQUIV_COUNTER);
    write_rendered(&out_dir, "EquivCounter", &projection.files);
    let c_run = run_c_ticks(&out_dir, "EquivCounter", COUNTER_DRIVER, fields.len());
    let ref_ticks = reference_ticks("EquivCounter", EQUIV_COUNTER, &fields, 0.0, 0.1, 5);
    let oracle = oracle_ticks(&projection.package, "EquivCounter", &fields, 5);
    assert_cli_emits_the_rendered_bytes(&projection, "EquivCounter");

    assert_oracle_agrees(&oracle, &c_run, &ref_ticks, &fields, 0);
    assert_equivalent(&c_run, &ref_ticks, &fields);
    // Anchor the absolute values so a coincidental C-vs-ref agreement on a
    // wrong recurrence is still caught, and pin the seed both executing legs
    // must carry out of `startup`.
    assert_eq!(c_run.startup.values[0] as i64, 0, "counter startup seed");
    let counts: Vec<i64> = c_run
        .steps
        .iter()
        .map(|tick| tick.values[0] as i64)
        .collect();
    assert_eq!(counts, vec![1, 2, 3, 4, 5], "counter recurrence");
}

// ===========================================================================
// Fixture 2 — Real IIR: floating-point equivalence within tolerance.
// ===========================================================================

const DISCRETE_IIR: &str = r#"
model DiscreteIirSmoke
  constant Real samplePeriod = 0.1;
  parameter Real a = 0.9;
  parameter Real b = 0.1;
  discrete Real u(start = 0.0);
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    u = pre(u) + 1.0;
    y = a * pre(y) + b * u;
  end when;
end DiscreteIirSmoke;
"#;

const IIR_DRIVER: &str = r#"#include <stdio.h>
#include "DiscreteIirSmoke.h"
static void row(const char *label, const DiscreteIirSmokeState *state) {
    printf("%s,%.17g,%.17g,%lu\n", label, (double)state->u, (double)state->y,
           (unsigned long)state->rumoca_galec_error_signal_status);
}
int main(void) {
    DiscreteIirSmokeState state;
    char label[16];
    DiscreteIirSmoke_startup(&state);
    row("startup", &state);
    DiscreteIirSmoke_recalibrate(&state);
    row("recalibrate", &state);
    for (int step = 0; step < 5; ++step) {
        DiscreteIirSmoke_dostep(&state);
        snprintf(label, sizeof label, "%d", step);
        row(label, &state);
    }
    return 0;
}
"#;

#[test]
fn embedded_c_iir_matches_rumoca_evaluation_within_tolerance() {
    let dir = tempfile::tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let fields = [
        Field {
            name: "u",
            kind: FieldKind::Real,
        },
        Field {
            name: "y",
            kind: FieldKind::Real,
        },
    ];

    let projection = project_embedded_c(dir.path(), "DiscreteIirSmoke", DISCRETE_IIR);
    write_rendered(&out_dir, "DiscreteIirSmoke", &projection.files);
    let c_run = run_c_ticks(&out_dir, "DiscreteIirSmoke", IIR_DRIVER, fields.len());
    let ref_ticks = reference_ticks("DiscreteIirSmoke", DISCRETE_IIR, &fields, 0.0, 0.1, 5);
    let oracle = oracle_ticks(&projection.package, "DiscreteIirSmoke", &fields, 5);
    assert_cli_emits_the_rendered_bytes(&projection, "DiscreteIirSmoke");

    assert_oracle_agrees(&oracle, &c_run, &ref_ticks, &fields, 0);
    assert_equivalent(&c_run, &ref_ticks, &fields);
    // Anchor y against the closed-form recurrence so both sides agreeing on a
    // wrong difference equation is still caught.
    let y: Vec<f64> = c_run.steps.iter().map(|tick| tick.values[1]).collect();
    let expected = [0.1, 0.29, 0.561, 0.9049, 1.31441];
    for (got, want) in y.iter().zip(expected) {
        assert!(
            (got - want).abs() <= ATOL + RTOL * want.abs(),
            "IIR y sequence: got {got}, want {want}"
        );
    }
}

// ===========================================================================
// Fixture 3 — array-valued function calls retain their selected element.
// ===========================================================================

const ARRAY_FUNCTION: &str = r#"
function offsetVector
  input Real u[4];
  output Real y[4];
algorithm
  for i in 1:4 loop
    y[i] := u[i] + i;
  end for;
end offsetVector;

model ArrayFunctionSmoke
  constant Real samplePeriod = 0.1;
  discrete Integer count(start = 0, fixed = true);
  discrete output Real y[4](each start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    count = pre(count) + 1;
    y = offsetVector({count, 2.0 * count, 3.0 * count, 4.0 * count});
  end when;
end ArrayFunctionSmoke;
"#;

const ARRAY_FUNCTION_DRIVER: &str = r#"#include <stdio.h>
#include "ArrayFunctionSmoke.h"
static void row(const char *label, const ArrayFunctionSmokeState *state) {
    printf("%s,%d,%.17g,%.17g,%.17g,%.17g,%lu\n",
           label, (int)state->count,
           (double)state->y[0], (double)state->y[1],
           (double)state->y[2], (double)state->y[3],
           (unsigned long)state->rumoca_galec_error_signal_status);
}
int main(void) {
    ArrayFunctionSmokeState state;
    char label[16];
    ArrayFunctionSmoke_startup(&state);
    row("startup", &state);
    ArrayFunctionSmoke_recalibrate(&state);
    row("recalibrate", &state);
    for (int step = 0; step < 5; ++step) {
        ArrayFunctionSmoke_dostep(&state);
        snprintf(label, sizeof label, "%d", step);
        row(label, &state);
    }
    return 0;
}
"#;

#[test]
fn embedded_c_array_function_preserves_each_selected_element() {
    let dir = tempfile::tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let fields = [
        Field {
            name: "count",
            kind: FieldKind::Integer,
        },
        Field {
            name: "y[1]",
            kind: FieldKind::Real,
        },
        Field {
            name: "y[2]",
            kind: FieldKind::Real,
        },
        Field {
            name: "y[3]",
            kind: FieldKind::Real,
        },
        Field {
            name: "y[4]",
            kind: FieldKind::Real,
        },
    ];

    let projection = project_embedded_c(dir.path(), "ArrayFunctionSmoke", ARRAY_FUNCTION);
    write_rendered(&out_dir, "ArrayFunctionSmoke", &projection.files);
    let c_run = run_c_ticks(
        &out_dir,
        "ArrayFunctionSmoke",
        ARRAY_FUNCTION_DRIVER,
        fields.len(),
    );
    let ref_ticks = reference_ticks("ArrayFunctionSmoke", ARRAY_FUNCTION, &fields, 0.0, 0.1, 5);
    let oracle = oracle_ticks(&projection.package, "ArrayFunctionSmoke", &fields, 5);
    assert_cli_emits_the_rendered_bytes(&projection, "ArrayFunctionSmoke");

    assert_oracle_agrees(&oracle, &c_run, &ref_ticks, &fields, 0);
    assert_equivalent(&c_run, &ref_ticks, &fields);
    assert_eq!(
        c_run.steps[4].values,
        vec![5.0, 6.0, 12.0, 18.0, 24.0],
        "the four selected function-result elements must remain distinct"
    );
    assert_eq!(
        oracle.steps[4].values,
        vec![5.0, 6.0, 12.0, 18.0, 24.0],
        "the oracle must select the same four distinct elements"
    );
}

// ===========================================================================
// Fixture 4 — exact commensurate clocks execute on one base-period lattice.
// ===========================================================================

const MULTIRATE_COUNTER: &str = r#"
model MultirateCounter
  discrete output Real fastValue(start = 0.0, fixed = true);
  discrete output Real slowSnapshot(start = 0.0, fixed = true);
  Real fastAlias;
equation
  fastAlias = fastValue;
  // Deliberately declare the slow clock first. Correct coincident-tick
  // ordering must follow the alias dependency, not source-clock order.
  when sample(0.0, 0.2) then
    slowSnapshot = fastAlias;
  end when;
  when sample(0.0, 0.1) then
    fastValue = pre(fastValue) + 1.0;
  end when;
end MultirateCounter;
"#;

const MULTIRATE_DRIVER: &str = r#"#include <stdio.h>
#include "MultirateCounter.h"
static void row(const char *label, const MultirateCounterState *state) {
    printf("%s,%.17g,%.17g,%lu\n", label,
           (double)state->fastValue, (double)state->slowSnapshot,
           (unsigned long)state->rumoca_galec_error_signal_status);
}
int main(void) {
    MultirateCounterState state;
    char label[16];
    MultirateCounter_startup(&state);
    row("startup", &state);
    MultirateCounter_recalibrate(&state);
    row("recalibrate", &state);
    for (int step = 0; step < 5; ++step) {
        MultirateCounter_dostep(&state);
        snprintf(label, sizeof label, "%d", step);
        row(label, &state);
    }
    return 0;
}
"#;

#[test]
fn embedded_c_commensurate_clocks_match_rumoca_tick_for_tick() {
    let dir = tempfile::tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let fields = [
        Field {
            name: "fastValue",
            kind: FieldKind::Real,
        },
        Field {
            name: "slowSnapshot",
            kind: FieldKind::Real,
        },
    ];

    let projection = project_embedded_c(dir.path(), "MultirateCounter", MULTIRATE_COUNTER);
    let generated = projection.rendered("MultirateCounter.c");
    assert!(
        generated.contains("if (self->clockDivider0_0 == 0)"),
        "native equality conditions must have exactly one parenthesis pair:\n{generated}"
    );
    write_rendered(&out_dir, "MultirateCounter", &projection.files);
    let c_run = run_c_ticks(&out_dir, "MultirateCounter", MULTIRATE_DRIVER, fields.len());
    let ref_ticks = reference_ticks("MultirateCounter", MULTIRATE_COUNTER, &fields, 0.0, 0.1, 5);
    let oracle = oracle_ticks(&projection.package, "MultirateCounter", &fields, 5);
    assert_cli_emits_the_rendered_bytes(&projection, "MultirateCounter");

    // The oracle executes the checked divider lattice independently, so a
    // template that renders the coincident-tick order wrong — or that seeds
    // the divider differently in `startup`/`recalibrate` — cannot agree with
    // it (GAL-016/GAL-038).
    assert_oracle_agrees(&oracle, &c_run, &ref_ticks, &fields, 0);
    assert_equivalent(&c_run, &ref_ticks, &fields);
    let counts = c_run
        .steps
        .iter()
        .map(|tick| (tick.values[0], tick.values[1]))
        .collect::<Vec<_>>();
    assert_eq!(
        counts,
        vec![(1.0, 1.0), (2.0, 1.0), (3.0, 3.0), (4.0, 3.0), (5.0, 5.0)]
    );
}

// ===========================================================================
// Fixture 5 — signal/error case: a NaN Real comparison raises the predefined
// `NAN` signal in both *executing* legs — the generated C and the GALEC oracle
// (SPEC_0034 GAL-018/GAL-027, §3.2.5). The Modelica reference leg has no
// `ErrorSignalStatus` channel at all, so it participates in this fixture only
// through the branch value the NaN forces; nothing here is evidence that
// `simulate_dae` signals anything.
// ===========================================================================

/// `zeroGain` is an independent `parameter`, which GAL-020 forbids the
/// projection from constant-folding, so `zeroGain / zeroGain` is a genuine
/// runtime `0/0` and the comparison below sees a NaN operand at every tick in
/// every leg. The projection derives `signals NAN;` on `DoStep` from that
/// comparison, the C template routes it through
/// `rumoca_galec_compare_gt(&self->rumoca_galec_error_signal_status, …)`, and
/// the oracle raises `"NAN"` in `Evaluator::compare`.
///
/// The quotient is written *inline* in the condition on purpose. Binding it to
/// a `discrete Real ratio` variable first makes the Modelica reference leg
/// refuse to run at all — `simulate_dae` rejects a NaN variable value during
/// event iteration ("event-iteration run 1 has an invalid current value NaN")
/// — which would leave the signal case comparable on only two of the three
/// legs. Inline, no model variable ever holds the NaN, and all three legs
/// execute.
///
/// The NaN itself is therefore never a compared channel (NaN equals nothing,
/// including itself, so comparing it would assert on a float payload rather
/// than on semantics). What is compared is its two observable consequences,
/// and they are compared over different leg sets: the branch every leg must
/// take — GALEC comparisons yield `false` on a NaN operand, so `y` is the
/// else-value 2.0 — is checked on all three legs, while the
/// `ErrorSignalStatus` bit is checked oracle-vs-C only, because the reference
/// leg exposes no such channel to check.
const NAN_SIGNAL: &str = r#"
model NanSignalSmoke
  constant Real samplePeriod = 0.1;
  parameter Real zeroGain = 0.0;
  discrete Integer count(start = 0, fixed = true);
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    count = pre(count) + 1;
    y = if zeroGain / zeroGain > 1.0 then 1.0 else 2.0;
  end when;
end NanSignalSmoke;
"#;

const NAN_SIGNAL_DRIVER: &str = r#"#include <stdio.h>
#include "NanSignalSmoke.h"
static void row(const char *label, const NanSignalSmokeState *state) {
    printf("%s,%d,%.17g,%lu\n", label, (int)state->count, (double)state->y,
           (unsigned long)state->rumoca_galec_error_signal_status);
}
int main(void) {
    NanSignalSmokeState state;
    char label[16];
    NanSignalSmoke_startup(&state);
    row("startup", &state);
    NanSignalSmoke_recalibrate(&state);
    row("recalibrate", &state);
    for (int step = 0; step < 5; ++step) {
        NanSignalSmoke_dostep(&state);
        snprintf(label, sizeof label, "%d", step);
        row(label, &state);
    }
    return 0;
}
"#;

/// The predefined `NAN` bit, as `embedded-c-galec/model.c.jinja` encodes it.
const NAN_SIGNAL_BIT: u32 = 4;

#[test]
fn embedded_c_nan_comparison_signals_match_the_galec_oracle() {
    let dir = tempfile::tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let fields = [
        Field {
            name: "count",
            kind: FieldKind::Integer,
        },
        Field {
            name: "y",
            kind: FieldKind::Real,
        },
    ];

    let projection = project_embedded_c(dir.path(), "NanSignalSmoke", NAN_SIGNAL);
    let generated = projection.rendered("NanSignalSmoke.c");
    assert!(
        generated.contains("rumoca_galec_compare_gt(&self->rumoca_galec_error_signal_status"),
        "the Real comparison must route through the signalling helper:\n{generated}"
    );

    write_rendered(&out_dir, "NanSignalSmoke", &projection.files);
    let c_run = run_c_ticks(&out_dir, "NanSignalSmoke", NAN_SIGNAL_DRIVER, fields.len());
    let ref_ticks = reference_ticks("NanSignalSmoke", NAN_SIGNAL, &fields, 0.0, 0.1, 5);
    let oracle = oracle_ticks(&projection.package, "NanSignalSmoke", &fields, 5);
    assert_cli_emits_the_rendered_bytes(&projection, "NanSignalSmoke");

    // `assert_oracle_agrees` also pins `startup`/`recalibrate` to a clear
    // status, so the NAN bit must be raised by `DoStep` and by nothing else.
    assert_oracle_agrees(&oracle, &c_run, &ref_ticks, &fields, NAN_SIGNAL_BIT);
    assert_equivalent(&c_run, &ref_ticks, &fields);

    // Anchor the absolute values: every tick raises NAN and takes the else
    // branch, so neither "nobody signals" nor "the NaN comparison was folded
    // away" can pass.
    for (j, tick) in oracle.steps.iter().enumerate() {
        assert_eq!(
            tick.status,
            NAN_SIGNAL_BIT,
            "oracle NAN bit at tick {}",
            j + 1
        );
        assert_eq!(
            tick.values[1],
            2.0,
            "oracle else-branch value at tick {}",
            j + 1
        );
    }
    let y: Vec<f64> = c_run.steps.iter().map(|tick| tick.values[1]).collect();
    assert_eq!(y, vec![2.0; 5], "C else-branch value on a NaN comparison");
}

// ===========================================================================
// Fixture 6 — declared `min`/`max` SATURATE at the method boundaries.
//
// SPEC_0042 T3 ("implicit `limit self` at method entry/return; Startup:
// return only") is the settled semantics: a GALEC declared range is a
// saturation, not a Modelica assertion, and it leaves a qNaN as a qNaN.
// `rumoca-eval-galec` is the normative oracle — `interpreter.rs::invoke`
// calls `limit_all()` on entry when `limit_at_entry` and again on return,
// and `execution.rs::limit_all` -> `limit_declaration_value` ->
// `runtime.rs::limit_value` applies the declared bounds to every state slot,
// arrays elementwise, low bound before high bound.
//
// This fixture was previously recorded here as a KNOWN, UNLANDED two-leg
// divergence: the C template emitted no clamp anywhere, so it reported
// 1,2,3,4,5 against the oracle's 1,2,2,2,2. `embedded-c-galec/model.c.jinja`
// now emits `rumoca_galec_limit_self` and calls it at exactly those
// boundaries, so the fixture is landed as a gate. The `statements` macro
// still has no `limit` arm: an *explicit* GALEC `limit` statement is a
// different construct that T3 does not cover, and it keeps failing closed.
//
// **The reference leg is deliberately not a value oracle for `ySat`.**
// Modelica's `min`/`max` are declaration attributes with no saturating
// semantics, so `simulate_dae` never clamps: it tracks `count` unbounded. That
// is not a defect on either side — it is the exact difference T3 records
// ("opposite of Modelica semantics") — so `ySat` is gated as oracle == C ==
// closed-form via [`assert_executing_legs_agree`] plus
// [`LIMIT_SMOKE_SATURATED`], and its reference values are never compared
// rather than being loosened into agreement. What the reference leg IS used
// for here is the negative direction: its `ySat` must still run past the
// declared max, so "all legs agree" can never come from the clamp having
// quietly become a no-op. `count` carries no range and stays a full three-leg
// channel, so the tick schedule itself is still referenced.
//
// Recorded so the numbers are not rediscovered: at the harness's aligned
// sample points the reference reports `ySat` = 0,1,2,3,4 against the executing
// legs' 1,2,2,2,2. Both the missing clamp AND a one-tick lag are visible in
// that row — `ySat := count` is assigned in the same `when` body as `count`,
// yet the reference publishes it one sample late. The lag is a reference-leg
// defect independent of T3 (it shows on every fixture in this file that
// assigns a discrete Real inside the `when`), which is a second reason this
// channel is not compared against `simulate_dae`. Promote `ySat` to a
// three-leg value channel once the reference both saturates and aligns.
// ===========================================================================

const LIMIT_SMOKE: &str = r#"
model LimitSmoke
  constant Real samplePeriod = 0.1;
  discrete Integer count(start = 0, fixed = true);
  discrete output Real ySat(start = 0.0, min = -2.0, max = 2.0);
equation
  when sample(0.0, samplePeriod) then
    count = pre(count) + 1;
    ySat = count;
  end when;
end LimitSmoke;
"#;

const LIMIT_SMOKE_DRIVER: &str = r#"#include <stdio.h>
#include "LimitSmoke.h"
static void row(const char *label, const LimitSmokeState *state) {
    printf("%s,%d,%.17g,%lu\n", label, (int)state->count, (double)state->ySat,
           (unsigned long)state->rumoca_galec_error_signal_status);
}
int main(void) {
    LimitSmokeState state;
    char label[16];
    LimitSmoke_startup(&state);
    row("startup", &state);
    LimitSmoke_recalibrate(&state);
    row("recalibrate", &state);
    for (int step = 0; step < 5; ++step) {
        LimitSmoke_dostep(&state);
        snprintf(label, sizeof label, "%d", step);
        row(label, &state);
    }
    return 0;
}
"#;

/// `ySat = count` walks 1,2,3,4,5; the declared `max = 2.0` saturates it from
/// tick 3 on. Written out rather than computed so a clamp that silently moved
/// to the wrong boundary (or ran one tick late) cannot pass.
const LIMIT_SMOKE_SATURATED: [f64; 5] = [1.0, 2.0, 2.0, 2.0, 2.0];

/// The `max` declared on `ySat`, as the fixture source spells it.
const LIMIT_SMOKE_DECLARED_MAX: f64 = 2.0;

#[test]
fn embedded_c_declared_ranges_saturate_at_method_boundaries() {
    let dir = tempfile::tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let fields = [
        Field {
            name: "count",
            kind: FieldKind::Integer,
        },
        Field {
            name: "ySat",
            kind: FieldKind::Real,
        },
    ];

    let projection = project_embedded_c(dir.path(), "LimitSmoke", LIMIT_SMOKE);
    let generated = projection.rendered("LimitSmoke.c");
    // Structural anchors: the boundary limiter must exist and must be called
    // at all five T3 boundaries (Startup return, Recalibrate entry+return,
    // DoStep entry+return). Values alone would still pass if a later template
    // edit clamped inside the assignment instead, which is a different (and
    // wrong) semantics for any method that reads state before writing it.
    assert!(
        generated.contains("static void rumoca_galec_limit_self(LimitSmokeState *self) {"),
        "declared ranges must emit the boundary limiter:\n{generated}"
    );
    // T3 names five boundaries here (Startup return, Recalibrate entry+return,
    // DoStep entry+return), but this block's `Recalibrate` body is empty:
    // saturation is idempotent, so its return boundary would re-clamp values
    // nothing touched since its entry boundary, and the two collapse to one
    // call. Four calls therefore covers all five boundaries — a fifth would be
    // dead code in flight software. `ySat` is in `Startup`'s definitely-written
    // set, so Startup shares the full limiter rather than getting its own.
    assert_eq!(
        generated
            .matches("    rumoca_galec_limit_self(self);")
            .count(),
        4,
        "T3's five boundaries, with the empty Recalibrate's entry/return pair \
         collapsed:\n{generated}"
    );
    assert!(
        !generated.contains("rumoca_galec_limit_startup"),
        "every ranged declaration here is written by Startup, so one limiter \
         serves both boundary scopes:\n{generated}"
    );

    write_rendered(&out_dir, "LimitSmoke", &projection.files);
    let c_run = run_c_ticks(&out_dir, "LimitSmoke", LIMIT_SMOKE_DRIVER, fields.len());
    let ref_ticks = reference_ticks("LimitSmoke", LIMIT_SMOKE, &fields, 0.0, 0.1, 5);
    let oracle = oracle_ticks(&projection.package, "LimitSmoke", &fields, 5);
    assert_cli_emits_the_rendered_bytes(&projection, "LimitSmoke");

    // Leg 1 vs leg 2, whole life-cycle, values and ErrorSignalStatus: nothing
    // here signals, so a clamp that raised one would fail too.
    assert_executing_legs_agree(&oracle, &c_run, &fields, 0);

    // Leg 3 on the un-ranged channel only (see the fixture comment).
    for (j, (tick, ref_row)) in oracle.steps.iter().zip(&ref_ticks).enumerate() {
        assert_channel(
            &fields[0],
            &format!("tick {}", j + 1),
            "reference",
            tick.values[0],
            ref_row[0],
            F64_LEG,
        );
    }

    // The closed form both executing legs must reproduce. Anchoring it stops
    // "both legs clamp identically wrong" from reading as agreement, and the
    // seed pins that `Startup` limits on return (0.0 is inside the range, so
    // it must be left alone).
    assert_eq!(c_run.startup.values[1], 0.0, "startup seed is unclamped");
    for (leg, run) in [("oracle", &oracle), ("C", &c_run)] {
        let observed: Vec<f64> = run.steps.iter().map(|tick| tick.values[1]).collect();
        assert_eq!(
            observed,
            LIMIT_SMOKE_SATURATED.to_vec(),
            "{leg} `ySat` must saturate at the declared max"
        );
    }

    // And the divergence this fixture exists to police is real: the reference
    // leg runs past the declared max instead of saturating, so an accidental
    // "everything agrees" cannot come from the clamp having become a no-op.
    // Only the peak is asserted, not the whole row — the reference's one-tick
    // lag on this channel (see the fixture comment) is a separate defect and
    // must not be baked into this gate's expectations.
    let unclamped: Vec<f64> = ref_ticks.iter().map(|row| row[1]).collect();
    let reference_peak = unclamped.iter().copied().fold(f64::NEG_INFINITY, f64::max);
    assert!(
        reference_peak > LIMIT_SMOKE_DECLARED_MAX,
        "Modelica reference semantics must stay unclamped (peak {reference_peak} vs declared max \
         {LIMIT_SMOKE_DECLARED_MAX}); if the reference now saturates, promote `ySat` to a \
         three-leg value channel. Observed: {unclamped:?}"
    );
}

// ===========================================================================
// Fixture 7 — the declared-range coverage matrix: Integer bounds, an array
// bounded elementwise, and a `min`-only bound.
//
// SPEC_0042 T3 saturation is one rule over every declared entity, so the
// shapes that can silently lose it are worth executing rather than only
// inspecting: an Integer clamp is a different emitted comparison from a Real
// one (no `isnan` guard, int32 literals), an array clamp has to reach every
// element, and a one-sided declaration must emit exactly the one side it
// declared. All of them are gated oracle-vs-C-vs-closed-form for the same
// reason `LimitSmoke`'s `ySat` is: Modelica `min`/`max` do not saturate, so
// `simulate_dae` is not a value oracle for a ranged channel. `count` carries
// no range and stays a full three-leg channel.
// ===========================================================================

const RANGE_MATRIX: &str = r#"
model RangeMatrixSmoke
  constant Real samplePeriod = 0.1;
  discrete Integer count(start = 0, fixed = true);
  discrete Integer iHigh(start = 0, min = -3, max = 3);
  discrete Integer iLow(start = 0, min = -3, max = 3);
  discrete output Real yFloor(start = 0.0, min = -1.5);
  discrete output Real arr[3](each start = 0.0, each min = -1.0, each max = 4.0);
equation
  when sample(0.0, samplePeriod) then
    count = pre(count) + 1;
    iHigh = count;
    iLow = 4 - 2*count;
    yFloor = -count;
    arr = {count, 2*count, 3*count};
  end when;
end RangeMatrixSmoke;
"#;

const RANGE_MATRIX_DRIVER: &str = r#"#include <stdio.h>
#include "RangeMatrixSmoke.h"
static void row(const char *label, const RangeMatrixSmokeState *state) {
    printf("%s,%d,%d,%d,%.17g,%.17g,%.17g,%.17g,%lu\n", label,
           (int)state->count, (int)state->iHigh, (int)state->iLow,
           (double)state->yFloor,
           (double)state->arr[0], (double)state->arr[1], (double)state->arr[2],
           (unsigned long)state->rumoca_galec_error_signal_status);
}
int main(void) {
    RangeMatrixSmokeState state;
    char label[16];
    RangeMatrixSmoke_startup(&state);
    row("startup", &state);
    RangeMatrixSmoke_recalibrate(&state);
    row("recalibrate", &state);
    for (int step = 0; step < 5; ++step) {
        RangeMatrixSmoke_dostep(&state);
        snprintf(label, sizeof label, "%d", step);
        row(label, &state);
    }
    return 0;
}
"#;

/// The closed form both executing legs must reproduce, per tick, in `fields`
/// order after `count`: `iHigh` saturates at its declared `max`, `iLow` at its
/// declared `min`, `yFloor` at its `min` (it has no `max` at all), and every
/// element of `arr` at its own `max`.
const RANGE_MATRIX_SATURATED: [[f64; 6]; 5] = [
    [1.0, 2.0, -1.0, 1.0, 2.0, 3.0],
    [2.0, 0.0, -1.5, 2.0, 4.0, 4.0],
    [3.0, -2.0, -1.5, 3.0, 4.0, 4.0],
    [3.0, -3.0, -1.5, 4.0, 4.0, 4.0],
    [3.0, -3.0, -1.5, 4.0, 4.0, 4.0],
];

#[test]
fn embedded_c_declared_ranges_saturate_integers_arrays_and_one_sided_bounds() {
    let dir = tempfile::tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let fields = [
        Field {
            name: "count",
            kind: FieldKind::Integer,
        },
        Field {
            name: "iHigh",
            kind: FieldKind::Integer,
        },
        Field {
            name: "iLow",
            kind: FieldKind::Integer,
        },
        Field {
            name: "yFloor",
            kind: FieldKind::Real,
        },
        Field {
            name: "arr[1]",
            kind: FieldKind::Real,
        },
        Field {
            name: "arr[2]",
            kind: FieldKind::Real,
        },
        Field {
            name: "arr[3]",
            kind: FieldKind::Real,
        },
    ];

    let projection = project_embedded_c(dir.path(), "RangeMatrixSmoke", RANGE_MATRIX);
    let generated = projection.rendered("RangeMatrixSmoke.c");
    // Structural anchors for the two shapes a Real clamp cannot stand in for.
    assert!(
        generated.contains("if (self->iHigh > INT32_C(3)) {"),
        "an Integer bound must saturate with int32 literals and no isnan guard:\n{generated}"
    );
    assert!(
        generated.contains("if (self->arr[rumoca_galec_limit_0] > 4.0f) {"),
        "an array bound must saturate every element:\n{generated}"
    );
    assert!(
        !generated.contains("self->yFloor >"),
        "a declaration with no `max` must emit no high-side comparison:\n{generated}"
    );

    write_rendered(&out_dir, "RangeMatrixSmoke", &projection.files);
    let c_run = run_c_ticks(
        &out_dir,
        "RangeMatrixSmoke",
        RANGE_MATRIX_DRIVER,
        fields.len(),
    );
    let ref_ticks = reference_ticks("RangeMatrixSmoke", RANGE_MATRIX, &fields[..1], 0.0, 0.1, 5);
    let oracle = oracle_ticks(&projection.package, "RangeMatrixSmoke", &fields, 5);
    assert_cli_emits_the_rendered_bytes(&projection, "RangeMatrixSmoke");

    assert_executing_legs_agree(&oracle, &c_run, &fields, 0);
    for (j, (tick, ref_row)) in oracle.steps.iter().zip(&ref_ticks).enumerate() {
        assert_channel(
            &fields[0],
            &format!("tick {}", j + 1),
            "reference",
            tick.values[0],
            ref_row[0],
            F64_LEG,
        );
    }

    // Anchor the closed form so "both legs clamp identically wrong" cannot
    // read as agreement, and so a clamp that quietly became a no-op fails.
    for (leg, run) in [("oracle", &oracle), ("C", &c_run)] {
        let observed: Vec<Vec<f64>> = run
            .steps
            .iter()
            .map(|tick| tick.values[1..].to_vec())
            .collect();
        assert_eq!(
            observed,
            RANGE_MATRIX_SATURATED.map(Vec::from).to_vec(),
            "{leg} must saturate every declared range"
        );
    }
    assert_eq!(
        c_run.startup.values[3], 0.0,
        "the startup seed sits inside `yFloor`'s range and must be left alone"
    );
}

// ===========================================================================
// Fixture 8 — a declared range that means UNBOUNDED emits no clamp at all.
//
// Modelica spells "no bound on this side" as `Modelica.Constants.inf`, whose
// MSL value is `1e60`. That is finite in the checked block's f64 and NOT
// representable as the `float` this target executes Real as: rendering it
// produces `1.0e+60f`, which the target's own compile preflight rejects
// (`floating constant exceeds range of 'float'`). Since no binary32 value can
// violate such a bound, the saturation is provably dead and the clamp is
// elided — which is what this fixture executes rather than assumes: `yFree`
// runs past every finite bound the source names, in both executing legs.
// ===========================================================================

const UNBOUNDED_LIMIT: &str = r#"
model UnboundedLimitSmoke
  constant Real samplePeriod = 0.1;
  discrete Integer count(start = 0, fixed = true);
  discrete output Real yFree(start = 0.0, min = -1e60, max = 1e60);
equation
  when sample(0.0, samplePeriod) then
    count = pre(count) + 1;
    yFree = count;
  end when;
end UnboundedLimitSmoke;
"#;

const UNBOUNDED_LIMIT_DRIVER: &str = r#"#include <stdio.h>
#include "UnboundedLimitSmoke.h"
static void row(const char *label, const UnboundedLimitSmokeState *state) {
    printf("%s,%d,%.17g,%lu\n", label, (int)state->count, (double)state->yFree,
           (unsigned long)state->rumoca_galec_error_signal_status);
}
int main(void) {
    UnboundedLimitSmokeState state;
    char label[16];
    UnboundedLimitSmoke_startup(&state);
    row("startup", &state);
    UnboundedLimitSmoke_recalibrate(&state);
    row("recalibrate", &state);
    for (int step = 0; step < 5; ++step) {
        UnboundedLimitSmoke_dostep(&state);
        snprintf(label, sizeof label, "%d", step);
        row(label, &state);
    }
    return 0;
}
"#;

#[test]
fn embedded_c_unbounded_declared_ranges_emit_no_clamp() {
    let dir = tempfile::tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let fields = [
        Field {
            name: "count",
            kind: FieldKind::Integer,
        },
        Field {
            name: "yFree",
            kind: FieldKind::Real,
        },
    ];

    let projection = project_embedded_c(dir.path(), "UnboundedLimitSmoke", UNBOUNDED_LIMIT);
    let generated = projection.rendered("UnboundedLimitSmoke.c");
    assert!(
        !generated.contains("e+60"),
        "an unbounded declaration must never render an out-of-range float \
         literal — the target's own compile preflight rejects it:\n{generated}"
    );
    assert!(
        !generated.contains("rumoca_galec_limit"),
        "a block whose every declared bound is unbounded must emit no limiter \
         and no boundary call:\n{generated}"
    );

    write_rendered(&out_dir, "UnboundedLimitSmoke", &projection.files);
    let c_run = run_c_ticks(
        &out_dir,
        "UnboundedLimitSmoke",
        UNBOUNDED_LIMIT_DRIVER,
        fields.len(),
    );
    let ref_ticks = reference_ticks(
        "UnboundedLimitSmoke",
        UNBOUNDED_LIMIT,
        &fields[..1],
        0.0,
        0.1,
        5,
    );
    let oracle = oracle_ticks(&projection.package, "UnboundedLimitSmoke", &fields, 5);
    assert_cli_emits_the_rendered_bytes(&projection, "UnboundedLimitSmoke");

    assert_executing_legs_agree(&oracle, &c_run, &fields, 0);
    for (j, (tick, ref_row)) in oracle.steps.iter().zip(&ref_ticks).enumerate() {
        assert_channel(
            &fields[0],
            &format!("tick {}", j + 1),
            "reference",
            tick.values[0],
            ref_row[0],
            F64_LEG,
        );
    }
    for (leg, run) in [("oracle", &oracle), ("C", &c_run)] {
        let observed: Vec<f64> = run.steps.iter().map(|tick| tick.values[1]).collect();
        assert_eq!(
            observed,
            vec![1.0, 2.0, 3.0, 4.0, 5.0],
            "{leg} `yFree` must run unclamped: eliding the clamp is only sound \
             because no value of the target type could have violated it"
        );
    }
}

// ===========================================================================
// Fixture 9 — the ENTRY boundary, exercised by state written from outside
// between ticks.
//
// Every other fixture here re-assigns its ranged channels inside the `when`
// body, so a clamp applied on return alone would satisfy them all. This one
// does not: a tunable parameter carries a declared range, the driver re-tunes
// it out of range between ticks, and the value the block computes from it can
// only be right if the ENTRY boundary saturated it before the body ran. An
// externally written declaration is also the only channel that can do this on
// both executing legs — the oracle's `set_state` refuses block-owned state,
// exactly as the eFMI contract does — and unlike a control input it still
// carries a value the Modelica reference can simulate, so `count` stays a
// three-leg channel under the same lag-exclusion convention as `LimitSmoke`.
// ===========================================================================

const ENTRY_CLAMP: &str = r#"
model EntryClampSmoke
  constant Real samplePeriod = 0.1;
  parameter Real u(min = -2.0, max = 2.0) = 0.0;
  discrete Integer count(start = 0, fixed = true);
  discrete output Real yHeld(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    count = pre(count) + 1;
    yHeld = u;
  end when;
end EntryClampSmoke;
"#;

const ENTRY_CLAMP_DRIVER: &str = r#"#include <stdio.h>
#include "EntryClampSmoke.h"
static void row(const char *label, const EntryClampSmokeState *state) {
    printf("%s,%d,%.17g,%.17g,%lu\n", label, (int)state->count,
           (double)state->u, (double)state->yHeld,
           (unsigned long)state->rumoca_galec_error_signal_status);
}
int main(void) {
    EntryClampSmokeState state;
    char label[16];
    static const float driven[5] = {1.5f, 9.0f, -9.0f, 2.5f, 0.5f};
    int step;
    EntryClampSmoke_startup(&state);
    row("startup", &state);
    EntryClampSmoke_recalibrate(&state);
    row("recalibrate", &state);
    for (step = 0; step < 5; ++step) {
        state.u = driven[step];
        EntryClampSmoke_dostep(&state);
        snprintf(label, sizeof label, "%d", step);
        row(label, &state);
    }
    return 0;
}
"#;

/// What the driver writes before each `DoStep`, and what the declared range
/// leaves of it. Ticks 2, 3 and 4 are the ones that only pass if the ENTRY
/// boundary saturated `u` before the body read it.
const ENTRY_CLAMP_DRIVEN: [f64; 5] = [1.5, 9.0, -9.0, 2.5, 0.5];
const ENTRY_CLAMP_SATURATED: [f64; 5] = [1.5, 2.0, -2.0, 2.0, 0.5];

#[test]
fn embedded_c_entry_boundary_saturates_state_written_between_ticks() {
    let dir = tempfile::tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let fields = [
        Field {
            name: "count",
            kind: FieldKind::Integer,
        },
        Field {
            name: "u",
            kind: FieldKind::Real,
        },
        Field {
            name: "yHeld",
            kind: FieldKind::Real,
        },
    ];

    let projection = project_embedded_c(dir.path(), "EntryClampSmoke", ENTRY_CLAMP);
    let generated = projection.rendered("EntryClampSmoke.c");
    assert!(
        generated.contains("if (self->u > 2.0f) {"),
        "the declared range on the tuned declaration must saturate:\n{generated}"
    );

    write_rendered(&out_dir, "EntryClampSmoke", &projection.files);
    let c_run = run_c_ticks(
        &out_dir,
        "EntryClampSmoke",
        ENTRY_CLAMP_DRIVER,
        fields.len(),
    );
    let ref_ticks = reference_ticks("EntryClampSmoke", ENTRY_CLAMP, &fields[..1], 0.0, 0.1, 5);
    let schedule = InputSchedule {
        name: "u",
        before_step: ENTRY_CLAMP_DRIVEN.to_vec(),
    };
    let oracle = oracle_ticks_driven(
        &projection.package,
        "EntryClampSmoke",
        &fields,
        5,
        Some(&schedule),
    );
    assert_cli_emits_the_rendered_bytes(&projection, "EntryClampSmoke");

    assert_executing_legs_agree(&oracle, &c_run, &fields, 0);
    for (j, (tick, ref_row)) in oracle.steps.iter().zip(&ref_ticks).enumerate() {
        assert_channel(
            &fields[0],
            &format!("tick {}", j + 1),
            "reference",
            tick.values[0],
            ref_row[0],
            F64_LEG,
        );
    }

    // The entry clamp must actually have changed a value: the driven sequence
    // and the observed one differ on three of the five ticks.
    assert_ne!(
        ENTRY_CLAMP_DRIVEN.to_vec(),
        ENTRY_CLAMP_SATURATED.to_vec(),
        "this fixture is only meaningful if the clamp changes something"
    );
    for (leg, run) in [("oracle", &oracle), ("C", &c_run)] {
        let seen: Vec<f64> = run.steps.iter().map(|tick| tick.values[1]).collect();
        let held: Vec<f64> = run.steps.iter().map(|tick| tick.values[2]).collect();
        assert_eq!(
            seen,
            ENTRY_CLAMP_SATURATED.to_vec(),
            "{leg} must saturate the re-tuned value at the DoStep entry boundary"
        );
        assert_eq!(
            held,
            ENTRY_CLAMP_SATURATED.to_vec(),
            "{leg} must compute the body from the saturated value, not the raw one"
        );
    }
    assert_eq!(
        c_run.startup.values[1], 0.0,
        "the seeded value sits inside the declared range and is left alone"
    );
}

// ===========================================================================
// Fixture 9b — the Startup return boundary limits only what Startup wrote.
//
// A control input is never assigned by `Startup`, and in C the block state is
// caller-allocated memory with no per-slot initialized flag. Saturating a
// control input at the `Startup` return boundary would therefore read
// indeterminate memory — while the oracle, whose slots do carry that flag,
// simply skips it (`execution.rs::limit_all`). So that boundary must clamp
// only the declarations `Startup` definitely wrote.
//
// This one is a render gate rather than a fourth executing fixture: a Modelica
// model with a free `input` has no reference leg at all (`simulate_dae`
// rejects "input `u` has neither a checked default nor a runtime value"), and
// the property being gated is exactly which fields the generated code touches.
// The CLI byte-equality check still carries it to the shipped path.
// ===========================================================================

const STARTUP_INPUT_SCOPE: &str = r#"
model StartupInputScope
  constant Real samplePeriod = 0.1;
  input Real u(start = 0.0, min = -2.0, max = 2.0);
  discrete Integer count(start = 0, fixed = true);
  discrete output Real yHeld(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    count = pre(count) + 1;
    yHeld = u;
  end when;
end StartupInputScope;
"#;

#[test]
fn embedded_c_startup_boundary_never_reads_state_startup_did_not_write() {
    let dir = tempfile::tempdir().expect("tempdir");
    let projection = project_embedded_c(dir.path(), "StartupInputScope", STARTUP_INPUT_SCOPE);
    let generated = projection.rendered("StartupInputScope.c");
    let startup_body = generated
        .split_once("void StartupInputScope_startup(StartupInputScopeState *self) {")
        .and_then(|(_, rest)| rest.split_once("\n}"))
        .map(|(body, _)| body)
        .unwrap_or_else(|| panic!("generated startup body:\n{generated}"));
    assert!(
        !startup_body.contains("rumoca_galec_limit"),
        "`u` is the only ranged declaration and Startup never assigns it, so \
         the Startup return boundary must touch nothing:\n{generated}"
    );
    assert!(
        generated.contains("if (self->u > 2.0f) {"),
        "the input range must still saturate at the later boundaries, where \
         the caller has necessarily written it:\n{generated}"
    );
    for method in ["recalibrate", "dostep"] {
        let body = generated
            .split_once(&format!(
                "void StartupInputScope_{method}(StartupInputScopeState *self) {{"
            ))
            .and_then(|(_, rest)| rest.split_once("\n}"))
            .map(|(body, _)| body)
            .unwrap_or_else(|| panic!("generated {method} body:\n{generated}"));
        assert!(
            body.contains("rumoca_galec_limit_self(self);"),
            "{method} must still limit the whole block state:\n{generated}"
        );
    }
    assert_cli_emits_the_rendered_bytes(&projection, "StartupInputScope");
}

// ===========================================================================
// Fixture 10 — a declared bound the target cannot express fails CLOSED.
//
// The dual of fixture 8: a `min` at `+inf` (or a `max` at `-inf`) is not a
// weaker bound but an unrepresentable one — every finite value would saturate
// to an infinity the target has no literal for. An Integer bound outside the
// target's declared execution domain (GAL-028) is the same kind of defect: an
// integer domain has no value standing for "beyond the finite range", so such
// a declaration describes a variable domain this target cannot execute.
//
// Neither may render anything. Both are checked on the in-process render and
// on the shipped CLI path, because "emitted an out-of-range constant" and
// "emitted nothing and exited 0" are both failures a value comparison would
// never see.
// ===========================================================================

const UNREPRESENTABLE_REAL_RANGE: &str = r#"
model UnrepresentableRealRange
  constant Real samplePeriod = 0.1;
  discrete Integer count(start = 0, fixed = true);
  discrete output Real y(start = 0.0, min = 1e60);
equation
  when sample(0.0, samplePeriod) then
    count = pre(count) + 1;
    y = count;
  end when;
end UnrepresentableRealRange;
"#;

const OUT_OF_DOMAIN_INTEGER_RANGE: &str = r#"
model OutOfDomainIntegerRange
  constant Real samplePeriod = 0.1;
  discrete Integer count(start = 0, fixed = true);
  discrete Integer wide(start = 0, max = 3000000000);
equation
  when sample(0.0, samplePeriod) then
    count = pre(count) + 1;
    wide = count;
  end when;
end OutOfDomainIntegerRange;
"#;

/// Lower a fixture and render every `[[files]]` entry the target declares,
/// returning the first render failure instead of panicking on it.
fn try_render_embedded_c(work_dir: &Path, model: &str, source: &str) -> Result<(), String> {
    let fixture = cli_support::write_fixture(work_dir, model, source);
    let compiled = rumoca::Compiler::new()
        .model(model)
        .compile_path(&fixture)
        .expect("fixture model should compile");
    let package = lower_to_algorithm_code(
        &GalecInput::new(&compiled.dae, model),
        &GalecOptions::default(),
    )
    .unwrap_or_else(|errors| panic!("GALEC projection must accept {model}: {errors:?}"));
    let bundle = target_bundle();
    let manifest = bundle
        .parse_manifest()
        .unwrap_or_else(|error| panic!("parse `{EMBEDDED_C_TARGET}` target.toml: {error}"));
    let renderer = AlgorithmCodeTemplateRenderer::new(&package, compiled.dae.source_map())
        .unwrap_or_else(|error| panic!("{model}: template renderer: {error}"));
    for file in &manifest.files {
        let template = bundle
            .template_source(&file.template)
            .unwrap_or(Cow::Borrowed(file.template.as_str()));
        renderer
            .render_with_name_and_artifact(template.as_ref(), model, &())
            .map_err(|error| error.to_string())?;
    }
    Ok(())
}

#[test]
fn embedded_c_unrepresentable_declared_ranges_fail_closed() {
    for (model, source, expected) in [
        (
            "UnrepresentableRealRange",
            UNREPRESENTABLE_REAL_RANGE,
            "unsupported-feature:target-real-range",
        ),
        (
            "OutOfDomainIntegerRange",
            OUT_OF_DOMAIN_INTEGER_RANGE,
            "unsupported-feature:target-integer-range",
        ),
    ] {
        let dir = tempfile::tempdir().expect("tempdir");
        let error = try_render_embedded_c(dir.path(), model, source)
            .expect_err("a bound the target cannot express must not render");
        assert!(
            error.contains(expected),
            "{model} must fail closed with `{expected}`: {error}"
        );

        // And the shipped path fails the same way rather than writing a
        // source file the compile preflight would reject.
        let fixture = dir.path().join(format!("{model}.mo"));
        let cli_out = dir.path().join("cli-out");
        let output = cli_support::run_compile_target(&fixture, EMBEDDED_C_TARGET, &cli_out);
        let stderr = cli_support::strip_ansi(&String::from_utf8_lossy(&output.stderr));
        assert!(
            !output.status.success(),
            "`compile --target {EMBEDDED_C_TARGET}` must fail for {model}:\n{stderr}"
        );
        assert!(
            cli_support::diagnostic_contains(&stderr, expected),
            "{model} CLI diagnostic must name the unsupported feature:\n{stderr}"
        );
        assert!(
            !cli_out.join(format!("{model}.c")).exists(),
            "{model} must not leave a source file behind"
        );
    }
}

// ===========================================================================
// Fixture 11 - a guarded element write whose subscript is a proven local.
//
// GALEC statically evaluates an assignment target's subscript (EG022), so a
// function local may not stand there however well its range is proven. The
// write therefore lowers to the statement form of the read path's bounded
// selection: the value is bound once and one branch per candidate coordinate
// stores it through a literal subscript.
//
// The fixture is the dense-solve shape that reaches it: an unconditional index
// assignment, a guarded region around the write, and a nested loop supplying
// the second axis. Every element receives a distinct value derived from both
// axes, so a branch that stored through the wrong coordinate would land on a
// value no leg can reproduce.
//
// The guard is false for the first two ticks and true afterwards, so the run
// compares both the untaken path (the seeded zeros survive) and the taken one.
// ===========================================================================

const GUARDED_SCATTER: &str = r#"
function reverseScatter
  input Real u[6];
  input Boolean enabled;
  output Real y[6];
protected
  Integer row;
algorithm
  y := zeros(6);
  for step in 1:3 loop
    row := 4 - step;
    if enabled then
      for column in 1:2 loop
        y[2 * (row - 1) + column] :=
          u[2 * (row - 1) + column] + row + 10 * column;
      end for;
    end if;
  end for;
end reverseScatter;

model GuardedScatterSmoke
  constant Real samplePeriod = 0.1;
  discrete Integer count(start = 0, fixed = true);
  discrete output Real y[6](each start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    count = pre(count) + 1;
    y = reverseScatter(
      {count, 2.0 * count, 3.0 * count, 4.0 * count, 5.0 * count, 6.0 * count},
      count >= 3);
  end when;
end GuardedScatterSmoke;
"#;

const GUARDED_SCATTER_DRIVER: &str = r#"#include <stdio.h>
#include "GuardedScatterSmoke.h"
static void row(const char *label, const GuardedScatterSmokeState *state) {
    printf("%s,%d,%.17g,%.17g,%.17g,%.17g,%.17g,%.17g,%lu\n",
           label, (int)state->count,
           (double)state->y[0], (double)state->y[1],
           (double)state->y[2], (double)state->y[3],
           (double)state->y[4], (double)state->y[5],
           (unsigned long)state->rumoca_galec_error_signal_status);
}
int main(void) {
    GuardedScatterSmokeState state;
    char label[16];
    GuardedScatterSmoke_startup(&state);
    row("startup", &state);
    GuardedScatterSmoke_recalibrate(&state);
    row("recalibrate", &state);
    for (int step = 0; step < 5; ++step) {
        GuardedScatterSmoke_dostep(&state);
        snprintf(label, sizeof label, "%d", step);
        row(label, &state);
    }
    return 0;
}
"#;

#[test]
fn embedded_c_guarded_dynamic_element_write_stores_every_coordinate_exactly() {
    let dir = tempfile::tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let fields = [
        Field {
            name: "count",
            kind: FieldKind::Integer,
        },
        Field {
            name: "y[1]",
            kind: FieldKind::Real,
        },
        Field {
            name: "y[2]",
            kind: FieldKind::Real,
        },
        Field {
            name: "y[3]",
            kind: FieldKind::Real,
        },
        Field {
            name: "y[4]",
            kind: FieldKind::Real,
        },
        Field {
            name: "y[5]",
            kind: FieldKind::Real,
        },
        Field {
            name: "y[6]",
            kind: FieldKind::Real,
        },
    ];

    let projection = project_embedded_c(dir.path(), "GuardedScatterSmoke", GUARDED_SCATTER);
    write_rendered(&out_dir, "GuardedScatterSmoke", &projection.files);
    let c_run = run_c_ticks(
        &out_dir,
        "GuardedScatterSmoke",
        GUARDED_SCATTER_DRIVER,
        fields.len(),
    );
    let ref_ticks = reference_ticks("GuardedScatterSmoke", GUARDED_SCATTER, &fields, 0.0, 0.1, 5);
    let oracle = oracle_ticks(&projection.package, "GuardedScatterSmoke", &fields, 5);
    assert_cli_emits_the_rendered_bytes(&projection, "GuardedScatterSmoke");

    assert_oracle_agrees(&oracle, &c_run, &ref_ticks, &fields, 0);
    assert_equivalent(&c_run, &ref_ticks, &fields);

    // The untaken guard leaves the seeded zeros in place; the taken one stores
    // `u[k] + row + 10 * column` at every one of the six coordinates.
    assert_eq!(
        c_run.steps[1].values,
        vec![2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
        "the guard is still false at count = 2, so no element is written"
    );
    assert_eq!(
        c_run.steps[4].values,
        vec![5.0, 16.0, 31.0, 27.0, 42.0, 38.0, 53.0],
        "each coordinate must carry the value its own axes select"
    );
    assert_eq!(
        oracle.steps[4].values,
        vec![5.0, 16.0, 31.0, 27.0, 42.0, 38.0, 53.0],
        "the oracle must select the same six coordinates"
    );
}

// ===========================================================================
// Fixture 12 — a scalar local two array updates read is assigned before the
// update that overwrites what its own right-hand side reads.
// ===========================================================================

/// Gaussian elimination in the two shapes `LinearAlgebra.solve` is written in.
///
/// `factor` is one scalar local that both row updates read, and the second of
/// them overwrites `y[row, column]`, which `factor`'s own right-hand side
/// divides. Assigning `factor` after that update, with its right-hand side
/// expanded into the update, makes the division re-read the zero the same
/// element loop has just stored: every column past the pivot then scales by
/// zero, only the pivot column is ever eliminated, and the emitted block still
/// reports a clear `ErrorSignalStatus`.
///
/// `eliminatePlain` puts the three statements directly under the guard.
/// `eliminateGuarded` puts the row loop inside the guard, which is the shape
/// `LinearAlgebra.solve` has: the three values then reach the projection as one
/// correlated group of conditional values instead of three plain ones, and only
/// carrying the assigned local back into its own branch lets the second update
/// read it.
const ELIMINATION_ORDER: &str = r#"
function eliminatePlain
  input Real m[3, 3];
  input Real b[3, 3];
  output Real upperDiagonal[3];
  output Real inverseRow3[3];
protected
  Integer n = 3;
  Real factor;
  Real y[3, 3];
  Real z[3, 3];
algorithm
  y := m;
  z := b;
  for column in 1:2 loop
    for row in 2:3 loop
      if row > column then
        factor := y[row, column] / y[column, column];
        z[row, :] := z[row, :] - factor * z[column, :];
        y[row, column:n] := y[row, column:n] - factor * y[column, column:n];
      end if;
    end for;
  end for;
  upperDiagonal := {y[1, 1], y[2, 2], y[3, 3]};
  inverseRow3 := z[3, :];
end eliminatePlain;

function eliminateGuarded
  input Real m[3, 3];
  input Real b[3, 3];
  output Real upperDiagonal[3];
  output Real inverseRow3[3];
protected
  Integer n = 3;
  Real factor;
  Real pivotMagnitude;
  Boolean accepted;
  Real y[3, 3];
  Real z[3, 3];
algorithm
  y := m;
  z := b;
  accepted := true;
  for column in 1:2 loop
    pivotMagnitude := abs(y[column, column]);
    if pivotMagnitude <= 1e-12 then
      accepted := false;
    elseif accepted then
      for row in column + 1:3 loop
        factor := y[row, column] / y[column, column];
        z[row, :] := z[row, :] - factor * z[column, :];
        y[row, column:n] := y[row, column:n] - factor * y[column, column:n];
      end for;
    end if;
  end for;
  upperDiagonal := {y[1, 1], y[2, 2], y[3, 3]};
  inverseRow3 := z[3, :];
end eliminateGuarded;

model EliminationOrderSmoke
  constant Real samplePeriod = 0.1;
  parameter Real m[3, 3] = [4.0, 2.0, 1.0; 4.0, 5.0, 3.0; 4.0, 7.0, 9.0];
  parameter Real b[3, 3] = [1.0, 0.0, 0.0; 0.0, 1.0, 0.0; 0.0, 0.0, 1.0];
  discrete Real scale(start = 0.0, fixed = true);
  discrete output Real plainDiagonal[3](each start = 0.0);
  discrete output Real plainRow3[3](each start = 0.0);
  discrete output Real guardedDiagonal[3](each start = 0.0);
  discrete output Real guardedRow3[3](each start = 0.0);
algorithm
  when sample(0.0, samplePeriod) then
    scale := pre(scale) + 1.0;
    (plainDiagonal, plainRow3) := eliminatePlain(scale * m, b);
    (guardedDiagonal, guardedRow3) := eliminateGuarded(scale * m, b);
  end when;
end EliminationOrderSmoke;
"#;

const ELIMINATION_ORDER_DRIVER: &str = r#"#include <stdio.h>
#include "EliminationOrderSmoke.h"
static void row(const char *label, const EliminationOrderSmokeState *state) {
    printf("%s,%.17g,%.17g,%.17g,%.17g,%.17g,%.17g,%.17g,%.17g,%.17g,%.17g,%.17g,%.17g,%lu\n",
           label,
           (double)state->plainDiagonal[0], (double)state->plainDiagonal[1],
           (double)state->plainDiagonal[2],
           (double)state->plainRow3[0], (double)state->plainRow3[1],
           (double)state->plainRow3[2],
           (double)state->guardedDiagonal[0], (double)state->guardedDiagonal[1],
           (double)state->guardedDiagonal[2],
           (double)state->guardedRow3[0], (double)state->guardedRow3[1],
           (double)state->guardedRow3[2],
           (unsigned long)state->rumoca_galec_error_signal_status);
}
int main(void) {
    EliminationOrderSmokeState state;
    char label[16];
    EliminationOrderSmoke_startup(&state);
    row("startup", &state);
    EliminationOrderSmoke_recalibrate(&state);
    row("recalibrate", &state);
    for (int step = 0; step < 5; ++step) {
        EliminationOrderSmoke_dostep(&state);
        snprintf(label, sizeof label, "%d", step);
        row(label, &state);
    }
    return 0;
}
"#;

/// The compared channels, in the driver's column order: each function's
/// row-echelon diagonal, then its third accumulator row.
const ELIMINATION_ORDER_FIELDS: [Field; 12] = [
    Field {
        name: "plainDiagonal[1]",
        kind: FieldKind::Real,
    },
    Field {
        name: "plainDiagonal[2]",
        kind: FieldKind::Real,
    },
    Field {
        name: "plainDiagonal[3]",
        kind: FieldKind::Real,
    },
    Field {
        name: "plainRow3[1]",
        kind: FieldKind::Real,
    },
    Field {
        name: "plainRow3[2]",
        kind: FieldKind::Real,
    },
    Field {
        name: "plainRow3[3]",
        kind: FieldKind::Real,
    },
    Field {
        name: "guardedDiagonal[1]",
        kind: FieldKind::Real,
    },
    Field {
        name: "guardedDiagonal[2]",
        kind: FieldKind::Real,
    },
    Field {
        name: "guardedDiagonal[3]",
        kind: FieldKind::Real,
    },
    Field {
        name: "guardedRow3[1]",
        kind: FieldKind::Real,
    },
    Field {
        name: "guardedRow3[2]",
        kind: FieldKind::Real,
    },
    Field {
        name: "guardedRow3[3]",
        kind: FieldKind::Real,
    },
];

/// Gaussian elimination of
/// `[4 2 1; 4 5 3; 4 7 9]` against the identity, by hand and without pivoting,
/// exactly as the fixture's loops run it on the first tick, where `scale` is 1.
///
/// Column 1. Row 2: `factor = 4/4 = 1`, so row 2 becomes `[0 3 2]` and
/// accumulator row 2 becomes `[-1 1 0]`. Row 3: `factor = 4/4 = 1`, so row 3
/// becomes `[0 5 8]` and accumulator row 3 becomes `[-1 0 1]`.
/// Column 2. Row 3: `factor = 5/3`, so row 3 becomes `[0 0 8 - 10/3]` and
/// accumulator row 3 becomes `[-1 + 5/3, -5/3, 1]`.
///
/// The diagonal is therefore `[4, 3, 14/3]` and the third accumulator row is
/// `[2/3, -5/3, 1]`. Scheduling `factor` after the row update instead yields
/// `[4, 5, 9]` and `[0.4, -1.4, 1]`: with `factor` re-read as zero past the
/// pivot, only column 1 is ever eliminated, `y[2,2]` and `y[3,2]` keep their
/// input values, and column 2 then eliminates with `7/5` instead of `5/3`.
///
/// Tick j scales the matrix by j, which scales the row-echelon diagonal by j
/// and leaves the accumulator alone: every factor is a ratio of two scaled
/// entries. That is what makes the first compared channel advance, so the
/// harness's non-vacuity check sees a block that actually recomputes.
const ELIMINATION_DIAGONAL: [f64; 3] = [4.0, 3.0, 14.0 / 3.0];
const ELIMINATION_ROW3: [f64; 3] = [2.0 / 3.0, -5.0 / 3.0, 1.0];

#[test]
fn embedded_c_elimination_assigns_the_shared_factor_before_the_row_it_overwrites() {
    let dir = tempfile::tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let fields = ELIMINATION_ORDER_FIELDS;

    let projection = project_embedded_c(dir.path(), "EliminationOrderSmoke", ELIMINATION_ORDER);
    write_rendered(&out_dir, "EliminationOrderSmoke", &projection.files);
    let c_run = run_c_ticks(
        &out_dir,
        "EliminationOrderSmoke",
        ELIMINATION_ORDER_DRIVER,
        fields.len(),
    );
    let ref_ticks = reference_ticks(
        "EliminationOrderSmoke",
        ELIMINATION_ORDER,
        &fields,
        0.0,
        0.1,
        5,
    );
    let oracle = oracle_ticks(&projection.package, "EliminationOrderSmoke", &fields, 5);
    assert_cli_emits_the_rendered_bytes(&projection, "EliminationOrderSmoke");

    assert_oracle_agrees(&oracle, &c_run, &ref_ticks, &fields, 0);
    assert_equivalent(&c_run, &ref_ticks, &fields);

    for tick in 0..5 {
        let scale = (tick + 1) as f64;
        let expected = expected_elimination_row(scale);
        assert_elimination_row("generated C", &c_run.steps[tick].values, &expected);
        assert_elimination_row("galec oracle", &oracle.steps[tick].values, &expected);
    }
}

/// The twelve compared channels of the tick whose matrix is scaled by `scale`,
/// in the driver's column order: both functions must produce the same pair.
fn expected_elimination_row(scale: f64) -> Vec<f64> {
    let mut expected = Vec::with_capacity(ELIMINATION_ORDER_FIELDS.len());
    for _ in 0..2 {
        expected.extend(ELIMINATION_DIAGONAL.map(|value| scale * value));
        expected.extend(ELIMINATION_ROW3);
    }
    expected
}

/// Compare one leg's tick against the hand-computed elimination.
///
/// The bound is the float32 profile bound, because the generated C carries
/// these values as `float`; it is far tighter than the gap to the misscheduled
/// answer, whose first wrong channel is `3` against `5`.
fn assert_elimination_row(leg: &str, got: &[f64], want: &[f64]) {
    assert_eq!(
        got.len(),
        want.len(),
        "{leg}: compared {} channels, expected {}",
        got.len(),
        want.len()
    );
    for (field, (got, want)) in ELIMINATION_ORDER_FIELDS.iter().zip(got.iter().zip(want)) {
        let (atol, rtol) = F32_PROFILE;
        let bound = atol + rtol * want.abs();
        let delta = (got - want).abs();
        assert!(
            delta <= bound,
            "{leg} channel `{}`: {got} vs hand-computed {want}, delta {delta} exceeds {bound}",
            field.name
        );
    }
}
