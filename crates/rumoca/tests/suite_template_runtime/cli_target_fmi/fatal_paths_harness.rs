//! A program fault (a dispatcher asked for an index past its family) is never
//! answered by a fallback, and every kernel entry reports it until a reset.
//!
//! Each C harness includes a component's program units with their
//! dispatchers renamed and wraps them, so a family can be made to forward an
//! index past its programs (`BAD`). The checks then drive the kernel and read
//! what it returns, logs, and leaves on the instance.

use super::*;

/// Renames every dispatcher of the program units before they are included,
/// so the wrappers below take their names.
const RENAMES: &str = "#define rmc_row_call rmc_row_call_real\n#define rmc_inv_call rmc_inv_call_real\n#define rmc_dep_call rmc_dep_call_real\n#define rmc_jvp_call rmc_jvp_call_real\n#define rmc_lane_call rmc_lane_call_real\n#define rmc_iso_call rmc_iso_call_real\n#define rmc_causal_call rmc_causal_call_real\n";

const UNDEFS: &str = "#undef rmc_row_call\n#undef rmc_inv_call\n#undef rmc_dep_call\n#undef rmc_jvp_call\n#undef rmc_lane_call\n#undef rmc_iso_call\n#undef rmc_causal_call\n";

/// One wrapper per family: it counts the calls made while its family is
/// faulted, and forwards an index past every family's programs.
const WRAPPERS: &str = r#"
#define BAD ((size_t)-1)
enum { F_ROW, F_INV, F_DEP, F_JVP, F_LANE, F_ISO, F_CAUSAL, FAMILIES, F_ALL };
static int faulted_family = -1;
static size_t faulted_calls, calls_after_fault[FAMILIES], entered_at_fault, spared_calls;
static int fault_seen;
/* The block projections entered so far (projection_fallbacks.rs call counts). */
static size_t blocks_entered(const ModelInstance* m) {
    size_t total = 0;
    for (size_t site = 0; site < sizeof(m->rmc_fb_calls) / sizeof(m->rmc_fb_calls[0]); ++site) total += m->rmc_fb_calls[site];
    return total;
}
static size_t pick(const ModelInstance* m, int family, size_t program) {
    /* Any program called once the fault is raised, of any family: the
       fallback a block would take (the unsplit rows, the row of a dependent
       part, a Newton solve after an isolation, a dense solve after a torn
       one) or a later block. */
    if (fault_seen) calls_after_fault[family] += 1;
    if (family != faulted_family && faulted_family != F_ALL) return program;
    if (spared_calls > 0) { spared_calls -= 1; return program; }
    if (!fault_seen) entered_at_fault = blocks_entered(m);
    faulted_calls += 1; fault_seen = 1;
    return BAD;
}
RMC_API int rmc_row_call(size_t p, ModelInstance* m, double* o) { return rmc_row_call_real(pick(m, F_ROW, p), m, o); }
RMC_API int rmc_inv_call(size_t p, ModelInstance* m, double* o) { return rmc_inv_call_real(pick(m, F_INV, p), m, o); }
RMC_API int rmc_dep_call(size_t p, ModelInstance* m, const double* s, double* o) { return rmc_dep_call_real(pick(m, F_DEP, p), m, s, o); }
RMC_API int rmc_jvp_call(size_t p, ModelInstance* m, const double* s, double* o) { return rmc_jvp_call_real(pick(m, F_JVP, p), m, s, o); }
RMC_API int rmc_lane_call(size_t p, ModelInstance* m, const double* s, double* o) { return rmc_lane_call_real(pick(m, F_LANE, p), m, s, o); }
RMC_API int rmc_iso_call(size_t p, ModelInstance* m, double* o) { return rmc_iso_call_real(pick(m, F_ISO, p), m, o); }
RMC_API int rmc_causal_call(size_t p, ModelInstance* m, const size_t* t) { return rmc_causal_call_real(pick(m, F_CAUSAL, p), m, t); }
RMC_API int rmc_assign(ModelInstance* m, size_t first, size_t count);
/* The assignment wrapper can make the next assignment report a non-finite
   store, which sends a staged refresh to its complete-plan rescue. */
static size_t assign_calls;
#if HAS_ASSIGN
static int assign_nonfinite_once;
RMC_API int rmc_assign(ModelInstance* m, size_t first, size_t count) {
    assign_calls += 1;
    if (assign_nonfinite_once) { assign_nonfinite_once = 0; return 1; }
    return rmc_assign_real(m, first, count);
}
#endif
"#;

/// The checks on one component: each family that the algebraic refresh
/// reaches fails that refresh when faulted, with only the fault logged, no
/// non-finite mark, a released arena, and no row evaluated after the fault;
/// a fault during the complete-plan rescue is not a non-finite failure.
const FAMILY_CHECKS: &str = r#"
#include <stdio.h>
static const char* const family_name[FAMILIES] = { "row", "inv", "dep", "jvp", "lane", "iso", "causal" };
static int failures;
#define CHECK(condition, what) do { if (!(condition)) { printf("FAILED: %s (family %s)\n", what, faulted_family >= 0 ? family_name[faulted_family] : "none"); failures += 1; } } while (0)
static int logged_fault, logged_other;
static void capture(fmi3InstanceEnvironment environment, fmi3Status status, fmi3String category, fmi3String message) {
    (void)environment; (void)category;
    if (status != fmi3Error) return;
    if (strstr(message, "outside its dispatcher")) logged_fault += 1; else { logged_other += 1; printf("logged: %s\n", message); }
}
static void reset(ModelInstance* m, const double* y) {
    memcpy(m->y, y, sizeof(m->y)); m->rmc_fatal = false; m->rmc_nonfinite = false;
    faulted_calls = 0; memset(calls_after_fault, 0, sizeof(calls_after_fault)); entered_at_fault = 0; fault_seen = 0; logged_fault = 0; logged_other = 0;
}
int main(void) {
    ModelInstance* m = calloc(1, sizeof *m);
    CHECK(m != NULL, "allocate the instance");
    initialize_values(m);
    m->logger = capture;
    CHECK(settle_values(m) == fmi3OK, "settle the start point");
    double y[Y_LEN]; memcpy(y, m->y, sizeof(y));
    int reached = 0;
    for (int family = 0; family < FAMILIES; ++family) {
        reset(m, y);
        faulted_family = family;
        fmi3Status status = refresh_algebraics(m);
        faulted_family = -1;
        if (faulted_calls == 0) { printf("family %s not reached\n", family_name[family]); continue; }
        printf("family %s reached\n", family_name[family]);
        reached += 1;
        faulted_family = family;
        CHECK(status == fmi3Error && m->rmc_fatal, "a faulted family fails the refresh");
        CHECK(!m->rmc_nonfinite, "a fault is not a non-finite value");
        CHECK(logged_fault > 0 && logged_other == 0, "only the fault is logged");
        for (int after = 0; after < FAMILIES; ++after)
            if (calls_after_fault[after] != 0) { printf("  %zu %s calls after the fault\n", calls_after_fault[after], family_name[after]); CHECK(0, "no program runs after the fault"); }
        CHECK(blocks_entered(m) == entered_at_fault, "no block projection starts after the fault");
        CHECK(m->rmc_dtop == 0 && m->rmc_stop == 0, "the refresh releases its arena");
        faulted_family = -1;
    }
    CHECK(reached > 0, "some family is reached");
    /* The row wrappers answer a fault with the dispatcher's RMC_FATAL, not
       an ordinary failure. */
    {
        static double rows[Y_LEN + 1]; double value = 0.0;
        reset(m, y); faulted_family = F_ROW;
        CHECK(rmc_row(m, rmc_block(0), 0, &value) == RMC_FATAL, "rmc_row answers a fault with RMC_FATAL");
        reset(m, y); faulted_family = F_ROW;
        CHECK(rmc_rows(m, rmc_block(0), rows) == RMC_FATAL, "rmc_rows answers a fault with RMC_FATAL");
        faulted_family = -1;
    }
    /* A fault left on the instance refuses the refresh at entry: no step
       runs, no program or assignment is called, nothing more is logged, and
       no arena frame is opened. */
    reset(m, y);
    m->rmc_fatal = true; fault_seen = 1; assign_calls = 0;
    size_t entered = blocks_entered(m);
    CHECK(refresh_algebraics(m) == fmi3Error, "a faulted instance fails the refresh");
    for (int after = 0; after < FAMILIES; ++after) CHECK(calls_after_fault[after] == 0, "a faulted refresh calls no program");
    CHECK(assign_calls == 0, "a faulted refresh runs no assignment");
    CHECK(blocks_entered(m) == entered, "a faulted refresh enters no block");
    CHECK(logged_fault == 0 && logged_other == 0, "a faulted refresh logs nothing more");
    CHECK(m->rmc_dtop == 0 && m->rmc_stop == 0, "a faulted refresh opens no frame");
    CHECK(memcmp(m->y, y, sizeof(y)) == 0, "a faulted refresh keeps the coordinate");
#if RESCUE
    /* A fault during the complete-plan rescue of a non-finite assignment. */
    reset(m, y);
    assign_nonfinite_once = 1;
    faulted_family = F_INV;
    fmi3Status status = refresh_algebraics(m);
    CHECK(faulted_calls > 0, "the rescue reaches the invariant part");
    CHECK(blocks_entered(m) == entered_at_fault, "no block projection starts after the fault in the rescue");
    for (int after = 0; after < FAMILIES; ++after) CHECK(calls_after_fault[after] == 0, "no program runs after the fault in the rescue");
    CHECK(status == fmi3Error && m->rmc_fatal, "a fault in the rescue fails the refresh");
    CHECK(!m->rmc_nonfinite, "a fault in the rescue is not a non-finite value");
    CHECK(logged_fault > 0 && logged_other == 0, "the rescue logs only the fault");
    faulted_family = -1;
#endif
    if (failures == 0) printf("ok reached=%d\n", reached);
    free(m);
    return failures != 0;
}
"#;

/// The checks on a loop whose derivative plan has no step: a fault left on
/// the instance fails the stepless refresh and the committed-seed shortcut.
const SEED_CHECKS: &str = r#"
#include <stdio.h>
static int failures;
#define CHECK(condition, what) do { if (!(condition)) { printf("FAILED: %s\n", what); failures += 1; } } while (0)
int main(void) {
    ModelInstance* m = calloc(1, sizeof *m);
    CHECK(m != NULL, "allocate the instance");
    initialize_values(m);
    CHECK(settle_values(m) == {P}OK, "settle the start point");
    CHECK(refresh_derivative_values_plan(m) == {P}OK, "the stepless plan refreshes");
    m->rmc_fatal = true;
    CHECK(refresh_derivative_values_plan(m) == {P}Error, "a fault fails the stepless plan");
    m->rmc_seeding = true; m->rmc_mark_pending = false; m->rmc_mark_resolved = true;
    m->rmc_mark_t = m->time; memcpy(m->rmc_mark_x, m->y, sizeof(m->rmc_mark_x));
    CHECK(refresh_derivative_values(m) == {P}Error, "a fault fails the committed-seed shortcut");
    m->rmc_fatal = false;
    CHECK(refresh_derivative_values(m) == {P}OK, "the committed-seed shortcut answers without a fault");
    if (failures == 0) printf("ok\n");
    free(m);
    return failures != 0;
}
"#;

/// The checks on the split circle: with a fault on the instance and the
/// target chart's refresh forced to answer OK, a basis transfer and a
/// completed-step decision fail before they commit anything.
const CHART_CHECKS: &str = r#"
#include <stdio.h>
static int failures;
#define CHECK(condition, what) do { if (!(condition)) { printf("FAILED: %s\n", what); failures += 1; } } while (0)
static int logged_fault, logged_other, quiet;
static void capture(fmi3InstanceEnvironment environment, fmi3Status status, fmi3String category, fmi3String message) {
    (void)environment; (void)category;
    if (status != fmi3Error) return;
    if (strstr(message, "outside its dispatcher")) logged_fault += 1; else { logged_other += 1; if (!quiet) printf("logged: %s\n", message); }
}
static fmi3Status harness_refresh_algebraics(ModelInstance* m) {
    fmi3Status status = force_ok ? fmi3OK : (refresh_algebraics)(m);
    if (fault_after_refresh) faulted_family = F_ALL;
    return status;
}
typedef struct { size_t chart, target; bool pending; double reference, target_reference; double y[Y_LEN], chart_y[Y_LEN]; } Snapshot;
static Snapshot snapshot(const ModelInstance* m) {
    Snapshot s = { m->rmc_chart, m->rmc_chart_target, m->rmc_chart_pending, m->rmc_chart_reference, m->rmc_chart_target_reference, {0}, {0} };
    memcpy(s.y, m->y, sizeof(s.y)); memcpy(s.chart_y, m->rmc_chart_y, sizeof(s.chart_y));
    return s;
}
static bool unchanged(const ModelInstance* m, const Snapshot* s) {
    Snapshot now = snapshot(m);
    return now.chart == s->chart && now.target == s->target && now.pending == s->pending
        && memcmp(&now.reference, &s->reference, sizeof(double)) == 0
        && memcmp(&now.target_reference, &s->target_reference, sizeof(double)) == 0
        && memcmp(now.y, s->y, sizeof(now.y)) == 0 && memcmp(now.chart_y, s->chart_y, sizeof(now.chart_y)) == 0;
}
int main(void) {
    ModelInstance* m = calloc(1, sizeof *m);
    CHECK(m != NULL, "allocate the instance");
    initialize_values(m);
    m->logger = capture;
    CHECK(settle_values(m) == fmi3OK, "settle the start point");
    /* A latched switch to the other chart at the settled coordinate. */
    m->rmc_chart_target = (m->rmc_chart + 1) % RMC_NCHARTS; m->rmc_chart_target_reference = 0.5;
    m->rmc_chart_pending = true; memcpy(m->rmc_chart_y, m->y, sizeof(m->rmc_chart_y));
    m->rmc_fatal = true; force_ok = 1;
    Snapshot before = snapshot(m);
    bool changed = true, nominals = true;
    CHECK(rmc_chart_transfer(m, &changed, &nominals) == fmi3Error, "a fault fails the transfer");
    CHECK(unchanged(m, &before), "the transfer commits nothing");
    CHECK(!changed && !nominals, "the transfer reports no change");
    CHECK(logged_other == 0, "the transfer does not blame the target chart");
    bool request = true;
    logged_other = 0;
    CHECK(rmc_chart_completed(m, &request) == fmi3Error, "a fault fails the completed step");
    CHECK(unchanged(m, &before), "the completed step commits nothing");
    CHECK(!request, "the completed step requests no switch");
    CHECK(logged_other == 0, "the completed step logs nothing");
    m->rmc_chart_pending = false; before = snapshot(m);
    CHECK(rmc_chart_completed(m, &request) == fmi3Error, "a fault fails the completed step without a pending switch");
    CHECK(unchanged(m, &before), "the completed step without a pending switch commits nothing");
    CHECK(!request, "the completed step without a pending switch requests none");
    /* A fault raised while the completed step reads the conditioning, after
       its refresh answered OK, leaves the decision uncommitted too, and is
       logged as the fault alone. */
    m->rmc_fatal = false; force_ok = 0; fault_after_refresh = 1; logged_fault = 0; logged_other = 0;
    m->rmc_chart_pending = true; before = snapshot(m);
    CHECK(rmc_chart_completed(m, &request) == fmi3Error, "a fault in the conditioning fails the completed step");
    CHECK(m->rmc_fatal && logged_fault > 0 && logged_other == 0, "the conditioning logs only the fault");
    CHECK(unchanged(m, &before), "a fault in the conditioning commits nothing");
    CHECK(!request, "a fault in the conditioning requests no switch");
    /* The same fault while the alternates are read, once the active chart
       fell below its keep threshold. */
    faulted_family = -1; m->rmc_fatal = false; logged_fault = 0; logged_other = 0;
    double kept = m->rmc_chart_reference; m->rmc_chart_reference = 1e300;
    spared_calls = rmc_charts[m->rmc_chart].nentries; before = snapshot(m);
    CHECK(rmc_chart_completed(m, &request) == fmi3Error, "a fault in the alternates fails the completed step");
    CHECK(m->rmc_fatal && spared_calls == 0 && logged_fault > 0 && logged_other == 0, "the alternates log only the fault");
    CHECK(unchanged(m, &before), "a fault in the alternates commits nothing");
    m->rmc_chart_reference = kept;
    fault_after_refresh = 0; faulted_family = -1;
    /* A fault raised while the transfer recovers the target's sources fails
       it, keeps the switch pending, and is logged as the fault alone. */
    m->rmc_fatal = false; force_ok = 0; logged_fault = 0; logged_other = 0;
    m->rmc_chart_pending = true; faulted_family = F_ROW; before = snapshot(m);
    changed = true; nominals = true;
    CHECK(rmc_chart_transfer(m, &changed, &nominals) == fmi3Error, "a fault in the transfer fails it");
    CHECK(m->rmc_fatal && logged_fault > 0 && logged_other == 0, "the transfer logs only the fault");
    CHECK(unchanged(m, &before), "a fault in the transfer commits nothing");
    CHECK(!changed && !nominals, "a fault in the transfer reports no change");
    faulted_family = -1;
    /* A transfer that fails without a fault (its target cannot be
       established from a non-finite latched coordinate) restores the chart
       and the coordinate and consumes the switch. */
    m->rmc_fatal = false; logged_fault = 0; logged_other = 0; quiet = 1;
    m->rmc_chart_pending = true; for (size_t k = 0; k < Y_LEN; ++k) m->rmc_chart_y[k] = NAN;
    before = snapshot(m);
    CHECK(rmc_chart_transfer(m, &changed, &nominals) == fmi3Error, "a transfer that cannot establish the target fails");
    CHECK(!m->rmc_fatal && logged_fault == 0 && logged_other > 0, "the failed transfer is logged as such");
    CHECK(!m->rmc_chart_pending, "a failed transfer consumes the switch");
    CHECK(m->rmc_chart == before.chart && memcmp(m->y, before.y, sizeof(before.y)) == 0, "a failed transfer restores the chart and the coordinate");
    if (failures == 0) printf("ok\n");
    free(m);
    return failures != 0;
}
"#;

/// The standard headers of `target`, when the conformance prerequisites are
/// available.
fn headers(target: &str) -> Option<PathBuf> {
    if !conformance_prerequisites_are_available() {
        return None;
    }
    let (fmi2, fmi3) = standard_roots();
    let (root, header) = if target == "fmi2" {
        (fmi2.root, "fmi2Functions.h")
    } else {
        (fmi3.root, "fmi3Functions.h")
    };
    let headers = root.join("headers");
    headers.join(header).is_file().then_some(headers)
}

/// An RLC ladder in array equations: its coupled nonlinear nodes form one
/// vector block, which the algebraic refresh settles by one complete
/// projection step after its assignments.
const LADDER: &str = "model ArrayLadder\n  parameter Integer N = 3;\n  Real iL[N](each start = 0, each fixed = true);\n  Real vC[N](each start = 0, each fixed = true);\n  Real v[N];\n  Real vs;\nequation\n  vs = 100 * sin(377 * time);\n  1e-3 * der(iL) = cat(1, {vs}, v[1:N - 1]) - v - 0.5 * iL;\n  iL - cat(1, iL[2:N], {0}) = (v - vC) / 10 + 1e-3 * v + 1e-6 * v .^ 3;\n  1e-6 * der(vC) = (v - vC) / 10;\nend ArrayLadder;\n";

fn prerequisites() -> Option<()> {
    headers("fmi2").and(headers("fmi3")).map(|_| ())
}

fn compile_model(source: &str, model: &str) -> rumoca::CompilationResult {
    rumoca::Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("compile {model}: {error:?}"))
}

/// Builds and runs `main` against the generated sources of `model`: with
/// `wrapped`, the program units are included under the dispatcher wrappers;
/// otherwise they are linked as built.
fn run_harness(
    work: &Path,
    (compiled, model, target): (&rumoca::CompilationResult, &'static str, &'static str),
    (prelude, main, wrapped): (&str, &str, bool),
) -> String {
    let headers = headers(target).expect("checked by the caller");
    let fmu = build_named_fmu(work, compiled, target, model);
    let sources = fmu.root.join("sources");
    let units = [
        "rmc_rows.c",
        "rmc_jacobian.c",
        "rmc_isolators.c",
        "rmc_assign.c",
    ];
    let text = if wrapped {
        let assign =
            fs::read_to_string(sources.join("rmc_assign.c")).expect("read the assignment unit");
        let has_assign = u8::from(assign.contains("rmc_assign(ModelInstance"));
        let includes: String = units
            .iter()
            .map(|unit| format!("#include \"{unit}\"\n"))
            .collect();
        format!(
            "#define HAS_ASSIGN {has_assign}\n{RENAMES}#define rmc_assign rmc_assign_real\n{includes}{UNDEFS}#undef rmc_assign\n{WRAPPERS}{prelude}#include \"model.c\"\n{main}"
        )
    } else {
        format!("#include \"model.h\"\n{prelude}#include \"model.c\"\n{main}")
    };
    let harness = work.join(format!("{model}_{target}_fatal.c"));
    fs::write(&harness, text).expect("write the harness");
    let binary = work.join(format!("{model}_{target}_fatal"));
    let mut command = Command::new("cc");
    command
        .args([
            "-std=c11",
            "-ffp-contract=off",
            "-Wall",
            "-Wextra",
            "-Werror",
        ])
        .arg(format!("-I{}", sources.display()))
        .arg(format!("-I{}", headers.display()))
        .arg(&harness);
    let linked: &[&str] = if wrapped {
        &["rmc_functions.c"]
    } else {
        &units
    };
    command.args(linked.iter().map(|unit| sources.join(unit)));
    if !wrapped {
        command.arg(sources.join("rmc_functions.c"));
    }
    command.arg("-lm").arg("-o").arg(&binary);
    checked_output(&mut command, "compile the fault harness");
    let output = Command::new(&binary)
        .output()
        .expect("run the fault harness");
    let mut stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    if !output.status.success() && !stdout.contains("FAILED") {
        stdout.push_str(&format!(
            "FAILED: the harness exited with {:?}\n",
            output.status
        ));
    }
    stdout
}

/// Every family a refresh reaches fails it when faulted, without a fallback,
/// and a fault during the complete-plan rescue is reported as a fault; the
/// three components reach every family between them.
#[test]
fn a_faulted_family_fails_the_refresh_without_a_fallback() {
    if prerequisites().is_none() {
        return;
    }
    let torn = super::torn_tangent_harness::MODEL;
    let models = [
        (
            super::affine_promotion::compile(),
            super::affine_promotion::MODEL,
            true,
        ),
        (
            compile_model(super::residual_split_harness::LOOP, "BadIndexLoop"),
            "BadIndexLoop",
            false,
        ),
        (
            compile_model(&super::torn_tangent_harness::source(), torn),
            torn,
            false,
        ),
        (compile_model(LADDER, "ArrayLadder"), "ArrayLadder", false),
    ];
    let mut reached = std::collections::BTreeSet::new();
    let mut failed = Vec::new();
    for (compiled, model, rescue) in &models {
        let (model, rescue) = (*model, *rescue);
        let work = tempdir().expect("fault harness work directory");
        let main = format!("#define RESCUE {}\n{FAMILY_CHECKS}", u8::from(rescue));
        let output = run_harness(work.path(), (compiled, model, "fmi3"), ("", &main, true));
        if !output.contains("ok reached=") {
            failed.push(format!("{model}:\n{output}"));
        }
        reached.extend(
            output
                .lines()
                .filter_map(|line| line.strip_prefix("family ")?.strip_suffix(" reached"))
                .filter(|family| !family.ends_with(" not"))
                .map(str::to_owned),
        );
    }
    let families = ["row", "inv", "dep", "jvp", "lane", "iso", "causal"];
    let missing: Vec<_> = families
        .iter()
        .filter(|family| !reached.contains(**family))
        .collect();
    assert!(failed.is_empty(), "{}", failed.join("\n"));
    assert!(
        missing.is_empty(),
        "families no refresh reaches: {missing:?}"
    );
}

/// A fault left on the instance fails a refresh whose plan has no step, and
/// the committed-seed shortcut, in FMI 3 and FMI 2.
#[test]
fn a_fault_fails_every_refresh_entry_until_a_reset() {
    if prerequisites().is_none() {
        return;
    }
    let source = "model SeedLoop\n  Real x(start = 1, fixed = true);\n  Real z(start = 0.5);\nequation\n  der(x) = -x;\n  z + 0.3 * sin(z) = x;\nend SeedLoop;\n";
    let compiled = compile_model(source, "SeedLoop");
    let mut failed = Vec::new();
    for target in ["fmi3", "fmi2"] {
        let work = tempdir().expect("seed harness work directory");
        let main = SEED_CHECKS.replace("{P}", target);
        let output = run_harness(
            work.path(),
            (&compiled, "SeedLoop", target),
            ("", &main, false),
        );
        if !output.starts_with("ok") {
            failed.push(format!("{target}:\n{output}"));
        }
    }
    assert!(failed.is_empty(), "{}", failed.join("\n"));
}

/// With a fault on the instance, a chart transfer and a completed-step
/// decision fail before they commit, even when the refresh they run answers
/// OK.
#[test]
fn a_fault_fails_a_chart_transfer_before_it_commits() {
    if prerequisites().is_none() {
        return;
    }
    let source = include_str!("../../fixtures/index_reduction/SplitCircleChart.mo");
    let compiled = compile_model(source, "SplitCircleChart");
    let work = tempdir().expect("chart harness work directory");
    let prelude = "static int force_ok, fault_after_refresh;\nstatic fmi3Status harness_refresh_algebraics(ModelInstance* m);\n#define refresh_algebraics(m) harness_refresh_algebraics(m)\n";
    let output = run_harness(
        work.path(),
        (&compiled, "SplitCircleChart", "fmi3"),
        (prelude, CHART_CHECKS, true),
    );
    assert!(output.starts_with("ok"), "{output}");
}

/// The checks on a component without isolator programs: an isolation its
/// tables name is a fault the dispatcher reports, never a read through the
/// absent isolator tables.
const NO_ISOLATOR_CHECKS: &str = r#"
#include <stdio.h>
static int failures;
#define CHECK(condition, what) do { if (!(condition)) { printf("FAILED: %s\n", what); failures += 1; } } while (0)
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
    double value = 0.0;
    CHECK(rmc_isolate(m, 0, &value) != 0, "an isolation without an isolator program fails");
    CHECK(m->rmc_fatal && logged_fault == 1, "the isolation reports the fault");
    if (failures == 0) printf("ok\n");
    free(m);
    return failures != 0;
}
"#;

/// An isolation in a component with no isolator program reports the fault
/// instead of reading the absent isolator tables.
#[test]
fn an_isolation_without_an_isolator_program_is_a_fault() {
    if prerequisites().is_none() {
        return;
    }
    // A polynomial loop: Newton settles it, and no row isolates its unknown.
    let source = "model Poly\n  Real x(start = 1, fixed = true);\n  Real z(start = 0.5);\nequation\n  der(x) = -z;\n  z^5 + z = x;\nend Poly;\n";
    let compiled = compile_model(source, "Poly");
    let work = tempdir().expect("isolator harness work directory");
    let fmu = build_named_fmu(work.path(), &compiled, "fmi3", "Poly");
    let isolators = fs::read_to_string(fmu.root.join("sources/model.h")).expect("read model.h");
    assert!(
        !isolators.contains("RMC_API int rmc_iso_call("),
        "the component has no isolator program"
    );
    let output = run_harness(
        work.path(),
        (&compiled, "Poly", "fmi3"),
        ("", NO_ISOLATOR_CHECKS, false),
    );
    assert!(output.starts_with("ok"), "{output}");
}
