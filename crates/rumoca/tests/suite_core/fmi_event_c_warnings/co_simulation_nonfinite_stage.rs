//! Issue #498: a Co-Simulation trial stage that leaves a non-finite value
//! rejects its substep and retries at a smaller step, while a failed
//! assertion still ends the communication step with Error. Only an unrescued
//! non-finite value marks a stage for retry, and a step that runs out of
//! retries says it was retrying a non-finite stage.

use super::pulse_lifecycle::xml_attribute;
use super::*;

const SOURCE: &str = r#"
model SqrtDecay
  "x(t) = exp(-100 t) is positive, but an explicit trial stage at h = 0.1 overshoots it below zero"
  Real x(start = 1, fixed = true);
  Real c;
equation
  c = sqrt(x);
  der(x) = -100 * c * c;
end SqrtDecay;
model GuardedDecay "the same decay with a guard the solution itself violates"
  Real x(start = 1, fixed = true);
equation
  der(x) = -100 * x;
  assert(x > 0.5, "x left its guarded range");
end GuardedDecay;
model Cliff "every trial stage past t = 0.05 is non-finite, so the step cannot finish"
  Real x(start = 0, fixed = true);
equation
  der(x) = sqrt(0.05 - time);
end Cliff;
model RescuedMix "a coupled algebraic pair the refresh can rescue with the complete projection"
  Real x(start = 1, fixed = true);
  Real c;
  Real a(start = 1);
  Real b(start = 1);
equation
  c = sqrt(x);
  a = c + 0.1 * b;
  b = a * a * a - 0.5 * a;
  der(x) = -100 * a * a;
end RescuedMix;
"#;

#[test]
fn generated_fmi_c_retries_a_non_finite_trial_stage() {
    for target in ["fmi2", "fmi3"] {
        let output = run_driver("SqrtDecay", target, 1);
        assert!(
            output.status.success(),
            "{target} SqrtDecay: {:?}\n{}",
            output.status.code(),
            String::from_utf8_lossy(&output.stdout)
        );
    }
}

#[test]
fn generated_fmi_c_keeps_a_failed_assertion_an_error() {
    for target in ["fmi2", "fmi3"] {
        let output = run_driver("GuardedDecay", target, 2);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            output.status.success(),
            "{target} GuardedDecay: {:?}\n{stdout}",
            output.status.code(),
        );
        assert!(
            stdout.contains("log: x left its guarded range") && !stdout.contains("retrying"),
            "{target} GuardedDecay reports the assertion, not a retry:\n{stdout}"
        );
    }
}

#[test]
fn generated_fmi_c_names_a_non_finite_stage_when_retries_run_out() {
    for target in ["fmi2", "fmi3"] {
        let output = run_driver("Cliff", target, 3);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            output.status.success(),
            "{target} Cliff: {:?}\n{stdout}",
            output.status.code(),
        );
        assert!(
            stdout.contains("retrying a non-finite trial stage"),
            "{target} Cliff names the non-finite retry:\n{stdout}"
        );
    }
}

/// The refresh marks a non-finite failure only after the complete projection
/// failed to rescue it numerically: a successful rescue returns with the mark
/// clear, and an evaluation error during the rescue leaves without it, so a
/// later failure in the same stage is never retried as non-finite.
#[test]
fn generated_fmi_c_marks_only_an_unrescued_non_finite_refresh() {
    let work = tempfile::tempdir().unwrap();
    let (sources, _) = render_model(work.path(), "RescuedMix", "fmi3");
    let model = std::fs::read_to_string(sources.join("model.c")).unwrap();
    let start = model
        .find("rmc_refresh(ModelInstance* m,")
        .expect("RescuedMix renders a refresh plan");
    let body = &model[start..];
    let body = &body[..body.find("\n}\n").expect("refresh body ends")];
    let failure = body
        .find("\nfailure:")
        .unwrap_or_else(|| panic!("refresh has a failure path:\n{body}"));
    let at = |needle: &str| {
        body[failure..]
            .find(needle)
            .map(|offset| failure + offset)
            .unwrap_or_else(|| panic!("refresh failure path lacks {needle:?}:\n{body}"))
    };
    let rescued = at("if (rescued == 0) return fmi3OK;");
    let pure_call = at("if (rescued < 0) goto fail;");
    let marked = at("m->rmc_nonfinite = true;");
    assert_eq!(
        body.matches("m->rmc_nonfinite = true;").count(),
        1,
        "{body}"
    );
    assert!(rescued < pure_call && pure_call < marked, "{body}");
    assert!(!body[..failure].contains("rmc_nonfinite"), "{body}");
}

fn run_driver(model: &str, target: &str, mode: u8) -> std::process::Output {
    let work = tempfile::tempdir().unwrap();
    let (sources, units) = render_model(work.path(), model, target);
    let xml =
        std::fs::read_to_string(sources.parent().unwrap().join("modelDescription.xml")).unwrap();
    let (identity_attribute, variable_tag, abi) = match target {
        "fmi2" => (&b"guid"[..], &b"ScalarVariable"[..], FMI2_ABI),
        _ => (&b"instantiationToken"[..], &b"Float64"[..], FMI3_ABI),
    };
    let guid = xml_attribute(&xml, b"fmiModelDescription", identity_attribute, None);
    let identifier = xml_attribute(&xml, b"CoSimulation", b"modelIdentifier", None);
    let x = xml_attribute(&xml, variable_tag, b"valueReference", Some("x"));
    let driver = DRIVER
        .replace("@ABI@", abi)
        .replace("@GUID@", &guid)
        .replace("@IDENTIFIER@", &identifier)
        .replace("@X@", &x)
        .replace("@MODE@", &mode.to_string());
    let source = work.path().join("driver.c");
    let executable = work.path().join("driver");
    std::fs::write(&source, driver).unwrap();
    let build = Command::new("cc")
        .args(["-std=c11", "-O1"])
        .arg(format!("-I{}", sources.display()))
        .arg(format!("-I{}", headers(target).display()))
        .args(units)
        .arg(source)
        .args(["-lm", "-o"])
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    Command::new(executable).output().unwrap()
}

fn render_model(work: &Path, model: &str, target: &str) -> (PathBuf, Vec<PathBuf>) {
    let compiled = rumoca::Compiler::new()
        .model(model)
        .compile_str(SOURCE, "Decay.mo")
        .unwrap_or_else(|error| panic!("compile {model}: {error:#}"));
    let files = rumoca::render_target_files(&compiled, model, target, None)
        .unwrap_or_else(|error| panic!("render {model} {target}: {error:#}"));
    let root = work.join(format!("{model}-{target}"));
    let sources = root.join("sources");
    std::fs::create_dir_all(&sources).unwrap();
    let mut units = Vec::new();
    for file in &files {
        if file.path == "modelDescription.xml" {
            std::fs::write(root.join(&file.path), &file.content).unwrap();
        } else if file.path.starts_with("sources/") {
            let path = root.join(&file.path);
            std::fs::write(&path, &file.content).unwrap();
            if file.path.ends_with(".c") {
                units.push(path);
            }
        }
    }
    (sources, units)
}

/// Mode 1: every 0.1 s communication step from 0 to 0.5 returns OK and x
/// follows exp(-100 t). Mode 2: the first step returns Error, not a retry.
/// Mode 3: the first step returns Discard. Logged messages print as `log: `.
const DRIVER: &str = r#"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
@ABI@
#define CHECK(condition) do { if (!(condition)) { printf("line %d\n", __LINE__); return 1; } } while (0)
int main(void) {
    Instance c = instantiate("@GUID@");
    CHECK(c != NULL);
    CHECK(initialize(c));
    const Reference x = @X@;
    for (int step = 0; step < 5; ++step) {
        const double t = step * 0.1;
        const int status = do_step(c, t, 0.1);
        printf("t=%g status=%d\n", t, status);
        if (@MODE@ == 2) { CHECK(status == STATUS_ERROR); break; }
        if (@MODE@ == 3) { CHECK(status == STATUS_DISCARD); break; }
        CHECK(status == STATUS_OK);
        double value;
        CHECK(read_real(c, x, &value));
        const double exact = exp(-100.0 * (t + 0.1));
        printf("  x=%.17g exact=%.17g\n", value, exact);
        CHECK(fabs(value - exact) < 1e-5);
    }
    free_instance(c);
    return 0;
}
"#;

const FMI2_ABI: &str = r#"
#define FMI2_FUNCTION_PREFIX @IDENTIFIER@_
#include "fmi2Functions.h"
typedef fmi2Component Instance;
typedef fmi2ValueReference Reference;
#define STATUS_OK fmi2OK
#define STATUS_ERROR fmi2Error
#define STATUS_DISCARD fmi2Discard
#include <stdarg.h>
static void log_message(fmi2ComponentEnvironment environment, fmi2String name, fmi2Status status, fmi2String category, fmi2String message, ...) {
    (void)environment; (void)name; (void)status; (void)category;
    va_list args;
    va_start(args, message);
    printf("log: ");
    vprintf(message, args);
    printf("\n");
    va_end(args);
}
static Instance instantiate(const char* identity) {
    static const fmi2CallbackFunctions callbacks = { log_message, calloc, free, NULL, NULL };
    return fmi2Instantiate("decay", fmi2CoSimulation, identity, NULL, &callbacks, 0, 0);
}
static int initialize(Instance c) {
    return fmi2SetupExperiment(c, 0, 0.0, 0.0, 0, 0.0) == fmi2OK &&
        fmi2EnterInitializationMode(c) == fmi2OK && fmi2ExitInitializationMode(c) == fmi2OK;
}
static int do_step(Instance c, double time, double step) {
    return (int)fmi2DoStep(c, time, step, 1);
}
static int read_real(Instance c, Reference reference, double* value) {
    return fmi2GetReal(c, &reference, 1, value) == fmi2OK;
}
#define free_instance fmi2FreeInstance
"#;

const FMI3_ABI: &str = r#"
#define FMI3_FUNCTION_PREFIX @IDENTIFIER@_
#include "fmi3Functions.h"
typedef fmi3Instance Instance;
typedef fmi3ValueReference Reference;
#define STATUS_OK fmi3OK
#define STATUS_ERROR fmi3Error
#define STATUS_DISCARD fmi3Discard
static void log_message(fmi3InstanceEnvironment environment, fmi3Status status, fmi3String category, fmi3String message) {
    (void)environment; (void)status; (void)category;
    printf("log: %s\n", message);
}
static Instance instantiate(const char* identity) {
    return fmi3InstantiateCoSimulation("decay", identity, NULL, 0, 0, 0, 0, NULL, 0, NULL, log_message, NULL);
}
static int initialize(Instance c) {
    return fmi3EnterInitializationMode(c, 0, 0.0, 0.0, 0, 0.0) == fmi3OK &&
        fmi3ExitInitializationMode(c) == fmi3OK;
}
static int do_step(Instance c, double time, double step) {
    fmi3Boolean event_needed, terminate, early_return;
    fmi3Float64 last_time;
    return (int)fmi3DoStep(c, time, step, 1, &event_needed, &terminate, &early_return, &last_time);
}
static int read_real(Instance c, Reference reference, double* value) {
    return fmi3GetFloat64(c, &reference, 1, value, 1) == fmi3OK;
}
#define free_instance fmi3FreeInstance
"#;
