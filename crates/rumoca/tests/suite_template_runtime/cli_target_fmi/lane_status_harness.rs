//! A lane build of the shared projection kernel (`RMC_LANE_STATUS`, one
//! instance per lane of a device integrator, with no logger) reports what an
//! FMI component logs (SPEC_0044 ME-PROJ-003).
//!
//! One C harness is compiled twice from the same generated sources, as the
//! FMI component and as a lane build. Each scenario drives the kernel into
//! fallbacks or faults on a fresh instance: the FMI build prints what it
//! logs, the lane build prints what it logs and what `rmc_lane_report` gives
//! from a copy of the lane's status taken before the instance is freed, and
//! then every message the kernel can report. The lane logs none of those;
//! its fallback reports are the FMI component's `projection` messages (the
//! first of each site), its first fault is the first `error` message the
//! kernel logged, and its fault count is the number of them.

use super::*;

/// Renames every dispatcher and the assignment entry of the program units
/// before they are included, so the wrappers below take their names.
const RENAMES: &str = "#define rmc_row_call rmc_row_call_real\n#define rmc_inv_call rmc_inv_call_real\n#define rmc_dep_call rmc_dep_call_real\n#define rmc_jvp_call rmc_jvp_call_real\n#define rmc_lane_call rmc_lane_call_real\n#define rmc_iso_call rmc_iso_call_real\n#define rmc_causal_call rmc_causal_call_real\n#define rmc_assign rmc_assign_real\n";

const UNDEFS: &str = "#undef rmc_row_call\n#undef rmc_inv_call\n#undef rmc_dep_call\n#undef rmc_jvp_call\n#undef rmc_lane_call\n#undef rmc_iso_call\n#undef rmc_causal_call\n#undef rmc_assign\n";

/// The residual rows can answer as built, forward an index past their
/// family (a program fault), or poison their first output (a projection
/// that cannot converge); the next assignment can report a non-finite
/// store, which sends a staged refresh to its complete-plan rescue.
const WRAPPERS: &str = r#"
enum { ROWS_AS_BUILT, ROWS_BAD_INDEX, ROWS_POISONED };
static int row_mode;
RMC_API int rmc_row_call(size_t p, ModelInstance* m, double* o) {
    if (row_mode == ROWS_BAD_INDEX) return rmc_row_call_real((size_t)-1, m, o);
    int result = rmc_row_call_real(p, m, o);
    if (row_mode == ROWS_POISONED && result == 0) o[0] = NAN;
    return result;
}
RMC_API int rmc_inv_call(size_t p, ModelInstance* m, double* o) { return rmc_inv_call_real(p, m, o); }
RMC_API int rmc_dep_call(size_t p, ModelInstance* m, const double* s, double* o) { return rmc_dep_call_real(p, m, s, o); }
RMC_API int rmc_jvp_call(size_t p, ModelInstance* m, const double* s, double* o) { return rmc_jvp_call_real(p, m, s, o); }
RMC_API int rmc_lane_call(size_t p, ModelInstance* m, const double* s, double* o) { return rmc_lane_call_real(p, m, s, o); }
RMC_API int rmc_iso_call(size_t p, ModelInstance* m, double* o) { return rmc_iso_call_real(p, m, o); }
RMC_API int rmc_causal_call(size_t p, ModelInstance* m, const size_t* t) { return rmc_causal_call_real(p, m, t); }
RMC_API int rmc_assign(ModelInstance* m, size_t first, size_t count);
#if HAS_ASSIGN
static int assign_nonfinite_once;
RMC_API int rmc_assign(ModelInstance* m, size_t first, size_t count) {
    if (assign_nonfinite_once) { assign_nonfinite_once = 0; return 1; }
    return rmc_assign_real(m, first, count);
}
#endif
"#;

const SCENARIOS: &str = r#"
#include <stdio.h>
static void capture(fmi3InstanceEnvironment environment, fmi3Status status, fmi3String category, fmi3String message) {
    (void)environment; (void)status;
    printf("log %s | %s\n", category, message);
}
#if defined(RMC_LANE_STATUS)
static void sink(void* environment, const char* category, const char* message) {
    (void)environment;
    printf("report %s | %s\n", category, message);
}
#endif
static ModelInstance* begin(const char* scenario, bool settle) {
    printf("scenario %s\n", scenario);
    ModelInstance* m = calloc(1, sizeof *m);
    if (!m) { printf("FAILED: allocate the instance\n"); exit(1); }
    initialize_values(m);
    m->logger = capture;
    if (settle && settle_values(m) != fmi3OK) printf("settle failed\n");
    return m;
}
/* A lane build reports from a copy of the lane's status alone: the instance
   is gone before the report. */
static void finish(ModelInstance* m) {
#if defined(RMC_LANE_STATUS)
    RmcLaneStatus status = m->rmc_lane_status;
    memset((void*)m, 0xA5, sizeof *m);
    free(m);
    printf("faults %zu\n", status.faults);
    size_t reports = rmc_lane_report(&status, sink, NULL);
    printf("reports %zu\n", reports);
#else
    free(m);
#endif
    printf("end\n");
}
#if defined(RMC_FB_PLAN)
/* `quiet` calls of `site`, the first of which falls back, then three calls
   that each fall back twice: the site crosses the policy rate. */
static void cross(ModelInstance* m, size_t site, int quiet) {
    for (int call = 0; call < quiet; ++call) { m->rmc_fb_calls[site] += 1; m->rmc_fb_active[site] = true; if (call == 0) rmc_fb_note(m, site); m->rmc_fb_active[site] = false; }
    for (int call = 0; call < 3; ++call) { m->rmc_fb_calls[site] += 1; m->rmc_fb_active[site] = true; rmc_fb_note(m, site); rmc_fb_note(m, site); m->rmc_fb_active[site] = false; }
}
#endif
int main(void) {
    ModelInstance* m;
#if HAS_ASSIGN && defined(RMC_FB_PLAN)
    /* Every staged refresh rescues a non-finite assignment: the plan's
       fallback rate crosses the policy rate after the minimum of calls. */
    m = begin("rescue", true);
    for (int call = 0; call < 2 * RMC_FALLBACK_MIN_CALLS; ++call) { assign_nonfinite_once = 1; (void)refresh_algebraics(m); }
    assign_nonfinite_once = 0;
    finish(m);
#endif
#if defined(RMC_FB_PLAN)
    /* One fallback in the first forty calls of block 0 stays below the rate;
       three calls that each fall back twice cross it. */
    m = begin("block", true);
    cross(m, 0, 40);
    finish(m);
#if defined(RMC_DWORK)
    /* Block 0 and then the staged plan cross the rate, in site order, and
       then a refresh finds the workspace exhausted: the lane's reports keep that
       order, the order the FMI component logs them in. */
    m = begin("ordered", true);
    cross(m, 0, 40); cross(m, RMC_FB_PLAN, 40);
    m->rmc_dtop = RMC_DWORK; (void)refresh_algebraics(m); m->rmc_dtop = 0;
    finish(m);
    /* The staged plan crosses the rate, then a refresh finds the workspace
       exhausted, then block 0 crosses: the component logs them in that
       order, and the lane reports the sites in site order, then the fault. */
    m = begin("unordered", true);
    cross(m, RMC_FB_PLAN, 40);
    m->rmc_dtop = RMC_DWORK; (void)refresh_algebraics(m); m->rmc_dtop = 0;
    cross(m, 0, 40);
    finish(m);
#endif
    /* Block 0 crosses the rate, an FMU state saved before it is restored,
       and the block crosses again at another call: the component logs both
       crossings, and the lane reports the first. */
    m = begin("recrossed", true);
    {
        fmi3FMUState state = NULL;
        if (fmi3GetFMUState(m, &state) != fmi3OK) { printf("FAILED: get the FMU state\n"); exit(1); }
        cross(m, 0, 40);
        if (fmi3SetFMUState(m, state) != fmi3OK) { printf("FAILED: set the FMU state\n"); exit(1); }
        (void)fmi3FreeFMUState(m, &state);
        cross(m, 0, 50);
    }
    finish(m);
#endif
    m = begin("poisoned", true);
    row_mode = ROWS_POISONED; (void)refresh_algebraics(m); (void)refresh_algebraics(m); row_mode = ROWS_AS_BUILT;
    finish(m);
    m = begin("bad-index", true);
    row_mode = ROWS_BAD_INDEX; (void)refresh_algebraics(m); (void)refresh_derivative_values(m); row_mode = ROWS_AS_BUILT;
    finish(m);
    /* A failed projection between a saved FMU state and its restore: the
       component logged it, and the lane keeps it across the restore. */
    m = begin("restored", true);
    {
        fmi3FMUState state = NULL;
        if (fmi3GetFMUState(m, &state) != fmi3OK) { printf("FAILED: get the FMU state\n"); exit(1); }
        row_mode = ROWS_POISONED; (void)refresh_algebraics(m); row_mode = ROWS_AS_BUILT;
        if (fmi3SetFMUState(m, state) != fmi3OK) { printf("FAILED: set the FMU state\n"); exit(1); }
        (void)fmi3FreeFMUState(m, &state);
    }
    finish(m);
#if defined(TANGENT_UNKNOWN)
    /* A failed projection at the private point of a directional derivative,
       which the component logged through its copy of the logger and the
       lane reports from its instance. */
    m = begin("tangent", false);
    m->type = INTERFACE_ME; m->state = MODEL_CONTINUOUS;
    {
        const fmi3ValueReference unknown = TANGENT_UNKNOWN, known = TANGENT_KNOWN;
        const fmi3Float64 seed = 1.0; fmi3Float64 sensitivity = 0.0;
        row_mode = ROWS_POISONED; (void)fmi3GetDirectionalDerivative(m, &unknown, 1, &known, 1, &seed, 1, &sensitivity, 1); row_mode = ROWS_AS_BUILT;
    }
    finish(m);
#endif
#if defined(RMC_DWORK)
    m = begin("workspace", true);
    size_t top = m->rmc_dtop; m->rmc_dtop = RMC_DWORK; (void)refresh_algebraics(m); m->rmc_dtop = top;
    finish(m);
#endif
#if defined(RMC_INIT_ROWS)
    /* The MLS initialization, whose settled view runs the poisoned or faulted
       refresh, or whose frames find the workspace exhausted. */
    m = begin("initial-poisoned", false);
    row_mode = ROWS_POISONED; (void)rmc_initialize(m, 0.0); row_mode = ROWS_AS_BUILT;
    finish(m);
    m = begin("initial-bad-index", false);
    row_mode = ROWS_BAD_INDEX; (void)rmc_initialize(m, 0.0); row_mode = ROWS_AS_BUILT;
    finish(m);
    m = begin("initial-workspace", false);
    m->rmc_dtop = RMC_DWORK; (void)rmc_initialize(m, 0.0); m->rmc_dtop = 0;
    finish(m);
#endif
#if defined(RMC_LANE_STATUS)
    /* Every message the kernel can report, which the lane must not log. */
    for (unsigned code = RMC_FAULT_NONE + 1; code < RMC_FAULT_COUNT; ++code) printf("kernel error | %s\n", rmc_fault_message(code, RMC_NO_SITE));
#if defined(RMC_FB_PLAN)
    for (size_t block = 0; block < RMC_FB_PLAN; ++block) printf("kernel error | %s\n", rmc_fault_message(RMC_FAULT_STAGE, block));
#endif
#endif
    return 0;
}
"#;

/// A limiter loop whose strict `noEvent` guard reads its own unknown: its
/// block reports its ES016 fold when its projection fails.
const FOLD_LIMITER: &str = "model FoldLimiter\n  parameter Real V0 = 15000;\n  parameter Real k = 0.5;\n  Real v_in;\n  Real v_out;\n  Real x(start = 1, fixed = true);\nequation\n  v_in = k * v_out - 10 * sin(20 * time) + 0.01 * x;\n  v_out = smooth(0, noEvent(if V0 * v_in > 15 then 15 else if V0 * v_in < -15 then -15 else V0 * v_in));\n  der(x) = -x + v_out;\nend FoldLimiter;\n";

/// A cubic block the initialization settles from an initial equation on its
/// unknown: its residual rows run in the initialization's settled view.
const INITIAL_IMPLICIT: &str = "model InitialImplicit\n  Real x(start = 0.3);\n  Real y(start = 0.2);\ninitial equation\n  y = 0.5;\nequation\n  x = y * y * y + y;\n  der(x) = -x;\nend InitialImplicit;\n";

/// Compiles `model` from `source`.
fn compile(source: &str, model: &str) -> rumoca::CompilationResult {
    rumoca::Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("compile {model}: {error:?}"))
}

/// What one scenario printed.
#[derive(Debug, Default)]
struct Scenario {
    name: String,
    logs: Vec<String>,
    reports: Vec<String>,
    faults: Option<usize>,
    report_count: Option<usize>,
}

/// The scenarios one build printed.
fn parse(stdout: &str) -> Vec<Scenario> {
    let mut scenarios = Vec::new();
    let mut current: Option<Scenario> = None;
    for line in stdout.lines() {
        if let Some(name) = line.strip_prefix("scenario ") {
            current = Some(Scenario {
                name: name.to_owned(),
                ..Scenario::default()
            });
        } else if line == "end" {
            scenarios.extend(current.take());
        } else if let Some(scenario) = current.as_mut() {
            if let Some(log) = line.strip_prefix("log ") {
                scenario.logs.push(log.to_owned());
            } else if let Some(report) = line.strip_prefix("report ") {
                scenario.reports.push(report.to_owned());
            } else if let Some(count) = line.strip_prefix("reports ") {
                scenario.report_count = Some(count.parse().expect("a report count"));
            } else if let Some(faults) = line.strip_prefix("faults ") {
                scenario.faults = Some(faults.parse().expect("a fault count"));
            }
        }
    }
    scenarios
}

/// Builds the harness of `model` as the FMI component or as a lane build,
/// and runs it.
fn run(sources: &Path, headers: &Path, work: &Path, model: &str, lane: bool) -> String {
    let assign =
        fs::read_to_string(sources.join("rmc_assign.c")).expect("read the assignment unit");
    let has_assign = u8::from(assign.contains("rmc_assign(ModelInstance"));
    let includes: String = [
        "rmc_rows.c",
        "rmc_jacobian.c",
        "rmc_isolators.c",
        "rmc_assign.c",
    ]
    .iter()
    .map(|unit| format!("#include \"{unit}\"\n"))
    .collect();
    let text = format!(
        "#define HAS_ASSIGN {has_assign}\n{RENAMES}{includes}{UNDEFS}{WRAPPERS}#include \"model.c\"\n{SCENARIOS}"
    );
    let name = format!("{model}_{}", if lane { "lane" } else { "fmi" });
    let harness = work.join(format!("{name}.c"));
    fs::write(&harness, text).expect("write the lane status harness");
    let binary = work.join(&name);
    let mut command = Command::new("cc");
    command.args([
        "-std=c11",
        "-ffp-contract=off",
        "-Wall",
        "-Wextra",
        "-Werror",
    ]);
    if lane {
        command.arg("-DRMC_LANE_STATUS");
    }
    if let Some((unknown, known)) = tangent_references(sources) {
        command
            .arg(format!("-DTANGENT_UNKNOWN={unknown}"))
            .arg(format!("-DTANGENT_KNOWN={known}"));
    }
    command
        .arg(format!("-I{}", sources.display()))
        .arg(format!("-I{}", headers.display()))
        .arg(&harness)
        .arg(sources.join("rmc_functions.c"))
        .arg("-lm")
        .arg("-o")
        .arg(&binary);
    checked_output(&mut command, "compile the lane status harness");
    let output = checked_output(&mut Command::new(&binary), "run the lane status harness");
    String::from_utf8_lossy(&output.stdout).into_owned()
}

/// The value references of the component's first state derivative and its
/// state, when it has one: the unknown and the known of the directional
/// derivative scenario.
fn tangent_references(sources: &Path) -> Option<(String, String)> {
    let description = sources.parent()?.join("modelDescription.xml");
    let xml = fs::read_to_string(description).expect("read the model description");
    let derivative = xml
        .lines()
        .find(|line| line.contains("<ContinuousStateDerivative "))?;
    let unknown = lifecycle::attribute(derivative, "valueReference").to_string();
    let needle = format!("valueReference=\"{unknown}\"");
    let variable = xml
        .lines()
        .find(|line| line.contains(&needle) && line.contains(" derivative=\""))?;
    let known = lifecycle::attribute(variable, "derivative").to_string();
    Some((unknown, known))
}

/// The kernel's part of one FMI scenario's log (what the lane build, which
/// logs only outside the kernel, did not log), split into its `projection`
/// messages and its `error` messages.
fn kernel_log<'a>(fmi: &'a Scenario, lane: &Scenario) -> (Vec<&'a str>, Vec<&'a str>) {
    let mut outside = lane.logs.clone();
    let mut kernel = Vec::new();
    for log in &fmi.logs {
        match outside.iter().position(|other| other == log) {
            Some(index) => {
                outside.remove(index);
            }
            None => kernel.push(log.as_str()),
        }
    }
    assert!(
        outside.is_empty(),
        "{}: the lane build logged what the FMI component did not: {outside:?}",
        fmi.name
    );
    let projection = kernel
        .iter()
        .copied()
        .filter(|log| log.starts_with("projection | "))
        .collect();
    let errors = kernel
        .iter()
        .copied()
        .filter(|log| log.starts_with("error | "))
        .collect();
    (projection, errors)
}

/// The lane build logs no message the kernel reports: neither a fallback
/// report (the kernel's one `projection` message) nor any fault message it
/// printed after its scenarios.
fn assert_lane_logs_no_kernel_message(model: &str, lane: &str) {
    let messages: Vec<&str> = lane
        .lines()
        .filter_map(|line| line.strip_prefix("kernel "))
        .collect();
    assert!(
        messages
            .iter()
            .any(|message| message.contains("workspace exhausted")),
        "{model}: the lane build prints the kernel's messages"
    );
    for scenario in parse(lane) {
        let logged: Vec<&String> = scenario
            .logs
            .iter()
            .filter(|log| log.starts_with("projection | ") || messages.contains(&log.as_str()))
            .collect();
        assert!(
            logged.is_empty(),
            "{model} {}: the lane build logged a kernel message: {logged:?}",
            scenario.name
        );
    }
}

/// Compares the two builds of `model` scenario by scenario, and answers the
/// messages the kernel logged, each after its scenario's name in brackets.
fn compare(model: &str, fmi: &str, lane: &str) -> Vec<String> {
    let (fmi, lane) = (parse(fmi), parse(lane));
    assert_eq!(
        fmi.iter().map(|s| &s.name).collect::<Vec<_>>(),
        lane.iter().map(|s| &s.name).collect::<Vec<_>>(),
        "{model}: both builds run the same scenarios"
    );
    let mut covered = Vec::new();
    for (fmi, lane) in fmi.iter().zip(&lane) {
        let context = format!("{model} {}", fmi.name);
        let (mut projection, errors) = kernel_log(fmi, lane);
        if fmi.name == "ordered" {
            let logged: Vec<&str> = projection.iter().chain(errors.first()).copied().collect();
            assert_eq!(
                lane.reports.iter().map(String::as_str).collect::<Vec<_>>(),
                logged,
                "{context}: the lane reports its sites in site order, then its fault"
            );
        }
        if fmi.name == "unordered" {
            assert_eq!(
                projection.len(),
                2,
                "{context}: the component logs both sites"
            );
            let expected: Vec<&str> = [projection[1], projection[0]]
                .into_iter()
                .chain(errors.first().copied())
                .collect();
            assert_eq!(
                lane.reports.iter().map(String::as_str).collect::<Vec<_>>(),
                expected,
                "{context}: the lane reports its sites in site order, then its fault, whatever order they occurred in"
            );
        }
        if fmi.name == "recrossed" {
            assert_eq!(
                projection.len(),
                2,
                "{context}: the component logs both crossings of the block"
            );
            assert_ne!(
                projection[0], projection[1],
                "{context}: the crossings differ"
            );
            projection.truncate(1);
        }
        let mut fallbacks: Vec<&str> = lane
            .reports
            .iter()
            .map(String::as_str)
            .filter(|report| report.starts_with("projection | "))
            .collect();
        projection.sort_unstable();
        fallbacks.sort_unstable();
        assert_eq!(
            fallbacks, projection,
            "{context}: the lane reports the FMI fallback messages"
        );
        let faults: Vec<&str> = lane
            .reports
            .iter()
            .map(String::as_str)
            .filter(|report| report.starts_with("error | "))
            .collect();
        assert_eq!(
            faults,
            errors.first().copied().into_iter().collect::<Vec<_>>(),
            "{context}: the lane reports the first fault the FMI component logged ({errors:?})"
        );
        assert_eq!(
            lane.faults,
            Some(errors.len()),
            "{context}: the lane counts every fault the FMI component logged"
        );
        assert!(
            lane.reports
                .iter()
                .skip_while(|report| report.starts_with("projection | "))
                .all(|report| report.starts_with("error | ")),
            "{context}: the lane reports its fallbacks before its fault: {:?}",
            lane.reports
        );
        assert_eq!(
            lane.reports.len(),
            fallbacks.len() + faults.len(),
            "{context}: every lane report is a fallback or a fault"
        );
        assert_eq!(
            lane.report_count,
            Some(lane.reports.len()),
            "{context}: rmc_lane_report answers the number of reports it gave"
        );
        covered.extend(
            projection
                .iter()
                .chain(&errors)
                .map(|log| format!("[{}] {log}", fmi.name)),
        );
    }
    covered
}

#[test]
fn a_lane_build_reports_what_the_fmi_component_logs() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    let headers = standard_roots().1.root.join("headers");
    let torn = super::torn_tangent_harness::MODEL;
    let models = [
        (
            super::affine_promotion::compile(),
            super::affine_promotion::MODEL,
        ),
        (
            compile(super::residual_split_harness::LOOP, "BadIndexLoop"),
            "BadIndexLoop",
        ),
        (compile(&super::torn_tangent_harness::source(), torn), torn),
        (compile(FOLD_LIMITER, "FoldLimiter"), "FoldLimiter"),
        (
            compile(INITIAL_IMPLICIT, "InitialImplicit"),
            "InitialImplicit",
        ),
    ];
    let mut covered = Vec::new();
    for (compiled, model) in &models {
        let work = tempdir().expect("lane status work directory");
        let fmu = build_named_fmu(work.path(), compiled, "fmi3", model);
        let sources = fmu.root.join("sources");
        let fmi = run(&sources, &headers, work.path(), model, false);
        let lane = run(&sources, &headers, work.path(), model, true);
        assert_lane_logs_no_kernel_message(model, &lane);
        covered.extend(compare(model, &fmi, &lane));
    }
    // Each scenario reaches its report on some component: a fallback report
    // of a block and of the staged plan, both in order before a fault, a
    // program fault, an exhausted workspace and a failed projection (an
    // ES016 fold among them), in a refresh and in the initialization, and
    // the initialization's own faults.
    assert_scenarios_reached(
        &covered,
        &[
            ("block", "projection | projection block "),
            (
                "rescue",
                "projection | the staged algebraic refresh fell back on ",
            ),
            (
                "ordered",
                "projection | the staged algebraic refresh fell back on ",
            ),
            (
                "ordered",
                "error | algebraic projection workspace exhausted",
            ),
            (
                "poisoned",
                "error | algebraic projection did not establish coordinate convergence",
            ),
            ("poisoned", "ES016"),
            ("restored", "error | algebraic projection "),
            ("unordered", "projection | projection block "),
            (
                "unordered",
                "error | algebraic projection workspace exhausted",
            ),
            ("recrossed", "projection | projection block "),
            ("tangent", "error | algebraic projection "),
            (
                "bad-index",
                "error | projection program index outside its dispatcher",
            ),
            (
                "workspace",
                "error | algebraic projection workspace exhausted",
            ),
            (
                "initial-poisoned",
                "error | algebraic projection did not establish coordinate convergence",
            ),
            (
                "initial-poisoned",
                "error | initial residual evaluation view did not converge",
            ),
            (
                "initial-bad-index",
                "error | projection program index outside its dispatcher",
            ),
            (
                "initial-workspace",
                "error | algebraic projection workspace exhausted",
            ),
            (
                "initial-workspace",
                "error | initial algebraic/update projection did not converge",
            ),
        ],
    );
}

/// Each `(scenario, needle)` names a message some component's `scenario`
/// logged from its kernel: `scenario` is a scenario name or the prefix of
/// numbered ones.
fn assert_scenarios_reached(covered: &[String], expected: &[(&str, &str)]) {
    let missing: Vec<_> = expected
        .iter()
        .filter(|(scenario, needle)| {
            !covered
                .iter()
                .any(|log| log.starts_with(&format!("[{scenario}")) && log.contains(needle))
        })
        .collect();
    assert!(
        missing.is_empty(),
        "reports no scenario reached: {missing:?}\n{covered:#?}"
    );
}

/// The scenarios a device unit runs without wrapped dispatchers, written once
/// for the FMI component (`FMI_BUILD`) and for the device unit compiled as
/// host C++: a block's fallback rate crossing the policy rate, the fault of a
/// failed stage of each block raised directly, an exhausted workspace in a
/// refresh, and one in the initialization.
const DEVICE_SCENARIOS: &str = r#"
#include <stdio.h>
#include <stdlib.h>
#if FMI_BUILD
static void capture(fmi3InstanceEnvironment environment, fmi3Status status, fmi3String category, fmi3String message) {
    (void)environment; (void)status;
    printf("log %s | %s\n", category, message);
}
#else
static void sink(void* environment, const char* category, const char* message) {
    (void)environment;
    printf("report %s | %s\n", category, message);
}
#endif
static ModelInstance* begin(const char* scenario) {
    printf("scenario %s\n", scenario);
#if FMI_BUILD
    ModelInstance* m = (ModelInstance*)calloc(1, sizeof *m);
    if (!m) { printf("FAILED: allocate the instance\n"); exit(1); }
    initialize_values(m);
    m->logger = capture;
    if (refresh_parameters(m) != fmi3OK) printf("parameters failed\n");
#else
    /* Device memory is not zeroed: the lane starts from garbage, which
       rmc_lane_reset clears, its reports included. */
    ModelInstance* m = (ModelInstance*)malloc(sizeof *m);
    if (!m) { printf("FAILED: allocate the instance\n"); exit(1); }
    memset((void*)m, 0xA5, sizeof *m);
    rmc_lane_reset(m); m->time = 0.0; m->parameters_dirty = false;
    for (size_t i = 0; i < Y_LEN; ++i) m->y[i] = rmc_y0[i];
    for (size_t i = 0; i < P_LEN; ++i) m->p[i] = rmc_p0[i];
#endif
    return m;
}
static void finish(ModelInstance* m) {
#if !FMI_BUILD
    RmcLaneStatus status = m->rmc_lane_status;
    memset((void*)m, 0xA5, sizeof *m);
    free(m);
    printf("faults %zu\n", status.faults);
    printf("reports %zu\n", rmc_lane_report(&status, sink, NULL));
#else
    free(m);
#endif
    printf("end\n");
}
int main(void) {
    ModelInstance* m;
#if FMI_BUILD
    m = begin("start");
    printf("Y"); for (size_t i = 0; i < Y_LEN; ++i) printf(" %a", m->y[i]); printf("\n");
    printf("P"); for (size_t i = 0; i < P_LEN; ++i) printf(" %a", m->p[i]); printf("\n");
    finish(m);
#endif
#if defined(RMC_FB_PLAN)
    m = begin("block");
    for (int call = 0; call < 40; ++call) { m->rmc_fb_calls[0] += 1; m->rmc_fb_active[0] = true; if (call == 0) rmc_fb_note(m, 0); m->rmc_fb_active[0] = false; }
    for (int call = 0; call < 3; ++call) { m->rmc_fb_calls[0] += 1; m->rmc_fb_active[0] = true; rmc_fb_note(m, 0); rmc_fb_note(m, 0); m->rmc_fb_active[0] = false; }
    finish(m);
    /* The fault of a failed stage of each block, raised directly: the host
       reads its message from the unit's own fold tables. */
    for (size_t block = 0; block < RMC_FB_PLAN; ++block) {
        char name[32];
        snprintf(name, sizeof name, "stage-%zu", block);
        m = begin(name);
        rmc_fault(m, RMC_FAULT_STAGE, block);
        finish(m);
    }
#endif
#if defined(RMC_DWORK)
    m = begin("workspace");
    m->rmc_dtop = RMC_DWORK; (void)refresh_algebraics(m); m->rmc_dtop = 0;
    finish(m);
#endif
#if defined(RMC_INIT_ROWS)
    m = begin("initial-workspace");
    m->rmc_dtop = RMC_DWORK; (void)rmc_initialize(m, 0.0); m->rmc_dtop = 0;
    finish(m);
#endif
    return 0;
}
"#;

/// Builds the FMI component's report driver from `sources` and runs it.
fn fmi_reports(sources: &Path, headers: &Path, work: &Path) -> String {
    let driver = work.join("fmi_reports.c");
    fs::write(
        &driver,
        format!("#define FMI_BUILD 1\n#include \"model.c\"\n{DEVICE_SCENARIOS}"),
    )
    .expect("write the FMI driver");
    let binary = work.join("fmi_reports");
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
        .arg(&driver)
        .args(
            [
                "rmc_rows.c",
                "rmc_jacobian.c",
                "rmc_isolators.c",
                "rmc_assign.c",
                "rmc_functions.c",
            ]
            .iter()
            .map(|unit| sources.join(unit)),
        )
        .arg("-lm")
        .arg("-o")
        .arg(&binary);
    checked_output(&mut command, "compile the FMI report driver");
    String::from_utf8_lossy(
        &checked_output(&mut Command::new(&binary), "run the FMI report driver").stdout,
    )
    .into_owned()
}

/// Builds the device unit of `compiled` as host C++, its lanes starting from
/// the start point the FMI driver printed (`fmi`), and runs its scenarios.
fn device_reports(compiled: &rumoca::CompilationResult, fmi: &str, work: &Path) -> String {
    let unit = crate::me_projection_device::device_unit(compiled);
    let device_source = format!(
        "#define FMI_BUILD 0\n#include <stdio.h>\n{unit}\nstatic const double rmc_y0[Y_LEN] = {{ {} }};\nstatic const double rmc_p0[P_LEN] = {{ {} }};\n{DEVICE_SCENARIOS}",
        crate::me_projection_device::start_values(fmi, "Y"),
        crate::me_projection_device::start_values(fmi, "P"),
    );
    let harness = work.join("device_reports.cpp");
    fs::write(&harness, device_source).expect("write the device report harness");
    let binary = work.join("device_reports");
    let mut command = Command::new("c++");
    command
        .args([
            "-std=c++17",
            "-ffp-contract=off",
            "-Wall",
            "-Wextra",
            "-Werror",
        ])
        .args(["-D__device__=", "-D__host__="])
        .arg(&harness)
        .arg("-lm")
        .arg("-o")
        .arg(&binary);
    checked_output(
        &mut command,
        "compile the device report harness as host C++",
    );
    String::from_utf8_lossy(
        &checked_output(&mut Command::new(&binary), "run the device report harness").stdout,
    )
    .into_owned()
}

/// The device unit of the ME projection kernel (`device_instance`,
/// `device_kernel`, every device build being a lane build), compiled by the
/// host C++ compiler with the CUDA qualifiers defined away, reports what the
/// FMI component of the same model logs: its lanes log nothing, and the
/// host's `rmc_lane_report` of each lane gives the FMI messages.
#[test]
fn a_device_unit_reports_what_the_fmi_component_logs() {
    let runs = |tool: &str| Command::new(tool).arg("--version").output().is_ok();
    if !conformance_prerequisites_are_available()
        || !crate::template_runtime_policy::prerequisites_are_available(
            "host build of the device unit's lane reports",
            &[("c++", runs("c++"))],
        )
    {
        return;
    }
    let headers = standard_roots().1.root.join("headers");
    let mut covered = Vec::new();
    // The device test models, and a block with an ES016 fold.
    let mut models = crate::me_projection_device::models();
    models.push((compile(FOLD_LIMITER, "FoldLimiter"), "FoldLimiter"));
    for (compiled, model) in models {
        let work = tempdir().expect("device lane status work directory");
        let fmu = build_named_fmu(work.path(), &compiled, "fmi3", model);
        let sources = fmu.root.join("sources");
        let fmi = fmi_reports(&sources, &headers, work.path());
        let device = device_reports(&compiled, &fmi, work.path());
        let fmi_scenarios = fmi
            .split_once("end\n")
            .map_or(fmi.as_str(), |(_, rest)| rest);
        covered.extend(compare(model, fmi_scenarios, &device));
    }
    assert_scenarios_reached(
        &covered,
        &[
            ("block", "projection | projection block "),
            (
                "stage-",
                "error | algebraic projection did not establish coordinate convergence",
            ),
            ("stage-", "ES016"),
            (
                "workspace",
                "error | algebraic projection workspace exhausted",
            ),
            (
                "initial-workspace",
                "error | algebraic projection workspace exhausted",
            ),
        ],
    );
}

/// Compiles every unit of a component's sources to an object, the model
/// unit as a lane build when `model_lane` is set and the others when
/// `others_lane` is, and links them into a shared library that must define
/// every name it uses: the output of the link.
fn link_units(
    sources: &Path,
    headers: &Path,
    work: &Path,
    model_lane: bool,
    others_lane: bool,
) -> Output {
    let mut objects = Vec::new();
    let mut units: Vec<PathBuf> = fs::read_dir(sources)
        .expect("list the component's sources")
        .map(|entry| entry.expect("a source entry").path())
        .filter(|path| path.extension().is_some_and(|extension| extension == "c"))
        .collect();
    units.sort();
    let tag = format!("{}{}", u8::from(model_lane), u8::from(others_lane));
    for unit in &units {
        let stem = unit.file_stem().expect("a unit name").to_string_lossy();
        let object = work.join(format!("{stem}_{tag}.o"));
        let lane = if stem == "model" {
            model_lane
        } else {
            others_lane
        };
        let mut command = Command::new("cc");
        command.args(["-std=c11", "-ffp-contract=off", "-fPIC", "-c"]);
        if lane {
            command.arg("-DRMC_LANE_STATUS");
        }
        command
            .arg(format!("-I{}", sources.display()))
            .arg(format!("-I{}", headers.display()))
            .arg(unit)
            .arg("-o")
            .arg(&object);
        checked_output(&mut command, "compile a component unit");
        objects.push(object);
    }
    Command::new("cc")
        .args(["-shared", "-Wl,-z,defs"])
        .args(&objects)
        .arg("-lm")
        .arg("-o")
        .arg(work.join(format!("component_{tag}.so")))
        .output()
        .expect("start the link")
}

/// A lane build adds its status to `ModelInstance`, so a unit built without
/// `RMC_LANE_STATUS` lays the instance out differently from one built with
/// it: their shared names differ, and the two do not link together.
#[test]
fn units_built_with_and_without_lane_status_do_not_link() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    let headers = standard_roots().1.root.join("headers");
    let compiled = compile(FOLD_LIMITER, "FoldLimiter");
    let work = tempdir().expect("lane link work directory");
    let fmu = build_named_fmu(work.path(), &compiled, "fmi3", "FoldLimiter");
    let sources = fmu.root.join("sources");
    for lane in [false, true] {
        let output = link_units(&sources, &headers, work.path(), lane, lane);
        assert!(
            output.status.success(),
            "units built alike (lane {lane}) link: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
    for model_lane in [false, true] {
        let output = link_units(&sources, &headers, work.path(), model_lane, !model_lane);
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            !output.status.success(),
            "a model unit built with lane status {model_lane} and the others without it link: {stderr}"
        );
    }
}
