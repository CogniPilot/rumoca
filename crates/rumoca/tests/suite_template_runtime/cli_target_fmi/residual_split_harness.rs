//! The generated C kernel's block residual split falls back to the unsplit
//! residual programs exactly where the linked kernel does (SPEC_0043 §6a
//! block residual split row; SPEC_0044 ME-PROJ-001).
//!
//! The generated `AffinePromote` FMI 3 sources carry an affine multi-row block
//! whose residual programs split into invariant and dependent parts. A C
//! harness includes the component's residual-row unit with its dispatchers
//! renamed and installs wrapping dispatchers in their place, so each part can
//! be made to fail on demand. It then checks, on the settled start point, that
//! the split residual equals the unsplit residual bit for bit; that a failed
//! invariant part and an exhausted arena both leave the block unsplit with the
//! same residual; that a failed dependent part hands the row to its unsplit
//! program with the same residual; and that when the unsplit program fails too
//! the row reports that program's failure.

use super::*;

/// Wrappers for the residual-row unit's `rmc_row_call`, `rmc_inv_call`, and
/// `rmc_dep_call` dispatchers, each forwarding to the generated dispatcher
/// unless its family is forced to fail, or forced to ask its generated
/// dispatcher for an index past its programs (`BAD`).
const WRAPPERS: &str = r#"
#define BAD 2
static int fail_row, fail_inv, fail_dep;
static size_t row_calls;
static size_t forward(int fail, size_t program) { return fail == BAD ? (size_t)-1 : program; }
RMC_API int rmc_row_call(size_t program, ModelInstance* m, double* o) { row_calls += 1; return fail_row == 1 ? -1 : rmc_row_call_real(forward(fail_row, program), m, o); }
RMC_API int rmc_inv_call(size_t program, ModelInstance* m, double* o) { return fail_inv == 1 ? -1 : rmc_inv_call_real(forward(fail_inv, program), m, o); }
RMC_API int rmc_dep_call(size_t program, ModelInstance* m, const double* s, double* o) { return fail_dep == 1 ? -1 : rmc_dep_call_real(forward(fail_dep, program), m, s, o); }
"#;

const CHECKS: &str = r#"
#include <stdio.h>
#define CHECK(condition, what) do { if (!(condition)) { printf("FAILED: %s\n", what); return 1; } } while (0)
#define MAX_ROWS 256
static bool same(const double* a, const double* b, size_t n) { return memcmp(a, b, n * sizeof(double)) == 0; }
int main(void) {
    ModelInstance* m = calloc(1, sizeof *m);
    CHECK(m != NULL, "allocate the instance");
    initialize_values(m);
    CHECK(settle_values(m) == fmi3OK, "settle the start point");
    const RmcBlock* b = NULL;
    for (size_t k = 0; k < sizeof(rmc_block_desc) / sizeof(rmc_block_desc[0]); ++k)
        if (!b && rmc_block_desc[k].affine && rmc_block_desc[k].n > 1 && rmc_block_desc[k].nsplit > 0) b = rmc_block(k);
    CHECK(b != NULL, "an affine multi-row block carries a residual split");
    CHECK(RMC_BS(b, n) <= MAX_ROWS, "the block fits the harness buffers");
    double unsplit[MAX_ROWS], split[MAX_ROWS];
    size_t top = m->rmc_dtop;
    CHECK(rmc_rows(m, b, unsplit) == 0, "the unsplit residual evaluates");
    const double* inv = rmc_block_invariants(m, b);
    CHECK(inv != NULL, "the invariant parts evaluate");
    CHECK(rmc_rows_split(m, b, inv, split) == 0 && same(split, unsplit, RMC_BS(b, n)), "the split residual equals the unsplit residual");
    m->rmc_dtop = top;
    fail_inv = 1;
    CHECK(rmc_block_invariants(m, b) == NULL, "a failed invariant part disables the split");
    fail_inv = 0;
    CHECK(rmc_rows_split(m, b, NULL, split) == 0 && same(split, unsplit, RMC_BS(b, n)), "a disabled split evaluates the unsplit residual");
    m->rmc_dtop = top;
    inv = rmc_block_invariants(m, b);
    CHECK(inv != NULL, "the invariant parts evaluate again");
    fail_dep = 1;
    CHECK(rmc_rows_split(m, b, inv, split) == 0 && same(split, unsplit, RMC_BS(b, n)), "a failed dependent part hands the row to its unsplit program");
    fail_row = 1;
    CHECK(rmc_rows_split(m, b, inv, split) != 0, "a failed unsplit program reports its own failure");
    fail_dep = 0;
    fail_row = 0;
    m->rmc_dtop = RMC_DWORK;
    CHECK(rmc_block_invariants(m, b) == NULL, "an exhausted arena disables the split");
    m->rmc_dtop = top;
    printf("ok nsplit=%zu n=%zu\n", RMC_BS(b, nsplit), RMC_BS(b, n));
    free(m);
    return 0;
}
"#;

/// A program index past its family is a fault of the generated tables: the
/// dispatcher answers `RMC_FATAL`, the dependent part does not hand the row
/// to its unsplit program, and the FMI export that ran the refresh returns an
/// error with the fault logged, at that call and every later one until the
/// instance is reset. A debug build (`RMC_DEBUG`) aborts at the first bad
/// index, before any check reports.
const DISPATCH_CHECKS: &str = r#"
#include <stdio.h>
#define CHECK(condition, what) do { if (!(condition)) { printf("FAILED: %s\n", what); return 1; } } while (0)
#define MAX_ROWS 256
static int logged_fault;
static void capture(fmi3InstanceEnvironment environment, fmi3Status status, fmi3String category, fmi3String message) {
    (void)environment; (void)category;
    if (status == fmi3Error && strstr(message, "outside its dispatcher")) logged_fault += 1;
}
int main(void) {
    ModelInstance* m = calloc(1, sizeof *m);
    CHECK(m != NULL, "allocate the instance");
    initialize_values(m);
    m->logger = capture;
    CHECK(settle_values(m) == fmi3OK, "settle the start point");
    const RmcBlock* b = NULL;
    for (size_t k = 0; k < sizeof(rmc_block_desc) / sizeof(rmc_block_desc[0]); ++k)
        if (!b && rmc_block_desc[k].affine && rmc_block_desc[k].n > 1 && rmc_block_desc[k].nsplit > 0) b = rmc_block(k);
    CHECK(b != NULL && RMC_BS(b, n) <= MAX_ROWS, "an affine multi-row block carries a residual split");
    double split[MAX_ROWS];
    CHECK(rmc_row_call_real((size_t)-1, m, split) == RMC_FATAL && m->rmc_fatal, "a bad row index is fatal");
    m->rmc_fatal = false;
    size_t top = m->rmc_dtop;
    const double* inv = rmc_block_invariants(m, b);
    CHECK(inv != NULL, "the invariant parts evaluate");
    fail_dep = BAD; row_calls = 0;
    CHECK(rmc_rows_split(m, b, inv, split) == RMC_FATAL, "a bad dependent index is fatal");
    CHECK(row_calls == 0, "a bad dependent index runs no unsplit program");
    fail_dep = 0; m->rmc_fatal = false; m->rmc_dtop = top;
    printf("ok\n");
    free(m);
    return 0;
}
"#;

/// The export half: on a nonlinear loop whose derivative refresh runs the
/// residual rows of a Newton block, a bad row index fails the derivative
/// export with the fault logged, every later export until a reset, and
/// nothing after it.
const EXPORT_CHECKS: &str = r#"
#include <stdio.h>
#define CHECK(condition, what) do { if (!(condition)) { printf("FAILED: %s\n", what); return 1; } } while (0)
static int logged_fault;
static void capture(fmi3InstanceEnvironment environment, fmi3Status status, fmi3String category, fmi3String message) {
    (void)environment; (void)category;
    if (status == fmi3Error && strstr(message, "outside its dispatcher")) logged_fault += 1;
}
int main(void) {
    /* The instance fmi3InstantiateModelExchange builds, without its token check. */
    ModelInstance* i = calloc(1, sizeof *i);
    CHECK(i != NULL, "allocate the FMI instance");
    initialize_values(i); i->logger = capture; i->type = INTERFACE_ME; i->state = MODEL_INSTANTIATED;
    CHECK(fmi3EnterInitializationMode(i, fmi3False, 0.0, 0.0, fmi3False, 0.0) == fmi3OK, "enter initialization");
    CHECK(fmi3ExitInitializationMode(i) == fmi3OK, "initialize");
    CHECK(fmi3EnterContinuousTimeMode(i) == fmi3OK, "enter continuous-time mode");
    double dx[STATE_LEN], x[STATE_LEN];
    for (size_t k = 0; k < STATE_LEN; ++k) x[k] = 0.75 + 0.125 * (double)k;
    CHECK(fmi3SetTime(i, 0.25) == fmi3OK && fmi3SetContinuousStates(i, x, STATE_LEN) == fmi3OK, "move to a new state");
    fail_row = BAD; logged_fault = 0; row_calls = 0;
    fmi3Status status = fmi3GetContinuousStateDerivatives(i, dx, STATE_LEN);
    CHECK(row_calls > 0, "the derivative refresh evaluates residual rows");
    CHECK(status == fmi3Error, "the export that hits a bad index returns an error");
    CHECK(i->rmc_fatal && logged_fault > 0, "the export logs the fault");
    CHECK(i->rmc_dtop == 0 && i->rmc_stop == 0, "the failed export releases its arena frames");
    fail_row = 0;
    x[0] += 0.25;
    CHECK(fmi3SetTime(i, 0.5) == fmi3OK && fmi3SetContinuousStates(i, x, STATE_LEN) == fmi3OK, "move to another state");
    CHECK(fmi3GetContinuousStateDerivatives(i, dx, STATE_LEN) != fmi3OK, "the fault persists until the instance is reset");
    CHECK(i->rmc_dtop == 0 && i->rmc_stop == 0, "the refused export releases its arena frames");
    CHECK(fmi3Reset(i) == fmi3OK, "reset the instance");
    CHECK(fmi3EnterInitializationMode(i, fmi3False, 0.0, 0.0, fmi3False, 0.0) == fmi3OK, "enter initialization after the reset");
    CHECK(fmi3ExitInitializationMode(i) == fmi3OK && fmi3EnterContinuousTimeMode(i) == fmi3OK, "a reset instance initializes");
    CHECK(fmi3SetTime(i, 0.25) == fmi3OK && fmi3SetContinuousStates(i, x, STATE_LEN) == fmi3OK && fmi3GetContinuousStateDerivatives(i, dx, STATE_LEN) == fmi3OK, "a reset instance evaluates its derivatives");
    fmi3FreeInstance(i);
    printf("ok\n");
    return 0;
}
"#;

/// Two unknowns coupled through `sin`, solved by a Newton block whose rows
/// the derivative refresh evaluates at every new state.
pub(super) const LOOP: &str = "model BadIndexLoop\n  Real x(start = 1, fixed = true);\n  Real z(start = 0.5);\n  Real w(start = 0.5);\nequation\n  der(x) = -0.1 * z - 0.1 * w;\n  z + 0.3 * sin(w) = x;\n  w + 0.3 * sin(z) = 0.5 * x;\nend BadIndexLoop;\n";

/// The harness `checks` linked against the generated `AffinePromote` FMI 3
/// sources, with the residual-row dispatchers wrapped, built with `defines`.
fn build_harness(work: &Path, name: &str, checks: &str, defines: &[&str]) -> PathBuf {
    let compiled = super::affine_promotion::compile();
    build_model_harness(
        work,
        (&compiled, super::affine_promotion::MODEL),
        (name, checks),
        defines,
    )
}

/// The harness `checks` linked against the generated FMI 3 sources of
/// `model`, with the residual-row dispatchers wrapped, built with `defines`.
fn build_model_harness(
    work: &Path,
    (compiled, model): (&rumoca::CompilationResult, &'static str),
    (name, checks): (&str, &str),
    defines: &[&str],
) -> PathBuf {
    let headers = standard_roots().1.root.join("headers");
    let fmu = build_named_fmu(work, compiled, "fmi3", model);
    let sources = fmu.root.join("sources");
    let harness_text = format!(
        "#define rmc_row_call rmc_row_call_real\n#define rmc_inv_call rmc_inv_call_real\n#define rmc_dep_call rmc_dep_call_real\n\
         #include \"rmc_rows.c\"\n#undef rmc_row_call\n#undef rmc_inv_call\n#undef rmc_dep_call\n\
         {WRAPPERS}#include \"model.c\"\n{checks}"
    );
    let harness = work.join(format!("{name}.c"));
    fs::write(&harness, harness_text).expect("write the harness");
    let binary = work.join(name);
    checked_output(
        Command::new("cc")
            .arg("-std=c11")
            .arg("-ffp-contract=off")
            .arg("-Wall")
            .arg("-Wextra")
            .arg("-Werror")
            .args(defines.iter().map(|define| format!("-D{define}")))
            .arg(format!("-I{}", sources.display()))
            .arg(format!("-I{}", headers.display()))
            .arg(&harness)
            .args(
                [
                    "rmc_assign.c",
                    "rmc_jacobian.c",
                    "rmc_isolators.c",
                    "rmc_functions.c",
                ]
                .map(|unit| sources.join(unit)),
            )
            .arg("-lm")
            .arg("-o")
            .arg(&binary),
        "compile the harness",
    );
    binary
}

fn harness_prerequisites() -> bool {
    conformance_prerequisites_are_available()
        && standard_roots()
            .1
            .root
            .join("headers")
            .join("fmi3Functions.h")
            .is_file()
}

#[test]
fn generated_residual_split_falls_back_to_the_unsplit_programs() {
    if !harness_prerequisites() {
        return;
    }
    let work = tempdir().expect("residual split harness work directory");
    let binary = build_harness(work.path(), "residual_split_harness", CHECKS, &[]);
    let output = checked_output(&mut Command::new(&binary), "run the residual split harness");
    assert!(
        String::from_utf8_lossy(&output.stdout).starts_with("ok"),
        "{}",
        String::from_utf8_lossy(&output.stdout)
    );
}

#[test]
fn a_bad_program_index_fails_its_export_without_a_fallback() {
    if !harness_prerequisites() {
        return;
    }
    let work = tempdir().expect("bad index harness work directory");
    let run = |binary: &Path| {
        let output = checked_output(&mut Command::new(binary), "run the bad index harness");
        assert!(
            String::from_utf8_lossy(&output.stdout).starts_with("ok"),
            "{}",
            String::from_utf8_lossy(&output.stdout)
        );
    };
    run(&build_harness(
        work.path(),
        "bad_dispatch_harness",
        DISPATCH_CHECKS,
        &[],
    ));
    let looped = rumoca::Compiler::new()
        .model("BadIndexLoop")
        .compile_str(LOOP, "BadIndexLoop.mo")
        .expect("compile the bad index loop");
    let loop_work = work.path().join("loop");
    fs::create_dir_all(&loop_work).expect("loop harness directory");
    run(&build_model_harness(
        &loop_work,
        (&looped, "BadIndexLoop"),
        ("bad_export_harness", EXPORT_CHECKS),
        &[],
    ));
    let debug = build_harness(
        work.path(),
        "bad_dispatch_debug_harness",
        DISPATCH_CHECKS,
        &["RMC_DEBUG"],
    );
    let aborted = Command::new(&debug)
        .output()
        .expect("run the debug bad index harness");
    assert!(
        !aborted.status.success() && aborted.stdout.is_empty(),
        "a debug build aborts on a bad index: {}",
        String::from_utf8_lossy(&aborted.stdout)
    );
}
