//! Actual FMI C APIs keep raw initialization and held transient truth distinct.

use super::*;

#[test]
fn generated_fmi_c_initial_relations_clear_before_transient_and_reset() {
    for target in ["fmi2", "fmi3"] {
        let work = tempfile::tempdir().unwrap();
        let (sources, units) = render(work.path(), "InitialRelation", target);
        let xml = std::fs::read_to_string(sources.parent().unwrap().join("modelDescription.xml"))
            .unwrap();
        let attribute = if target == "fmi2" {
            b"guid".as_slice()
        } else {
            b"instantiationToken"
        };
        let identity =
            pulse_lifecycle::xml_attribute(&xml, b"fmiModelDescription", attribute, None);
        let identifier =
            pulse_lifecycle::xml_attribute(&xml, b"ModelExchange", b"modelIdentifier", None);
        let driver = DRIVER
            .replace("@ABI@", if target == "fmi2" { FMI2 } else { FMI3 })
            .replace("@IDENTITY@", &identity)
            .replace("@IDENTIFIER@", &identifier);
        let source = work.path().join("initial.c");
        let binary = work.path().join("initial");
        std::fs::write(&source, driver).unwrap();
        let output = Command::new("cc")
            .args(["-std=c11", "-O2", "-Wall", "-Wextra", "-Werror"])
            .arg(format!("-I{}", sources.display()))
            .arg(format!("-I{}", headers(target).display()))
            .args(units)
            .arg(source)
            .args(["-lm", "-o"])
            .arg(&binary)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        let output = Command::new(binary).output().unwrap();
        assert!(
            output.status.success(),
            "{target} initialization lifecycle: {:?}\n{}\n{}",
            output.status.code(),
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

const DRIVER: &str = r#"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
@ABI@
#define CHECK(c) do { if (!(c)) { printf("failed at %d\n", __LINE__); return 1; } } while (0)
int main(void) {
    Instance c = instantiate("@IDENTITY@");
    CHECK(c != NULL);
    for (int run = 0; run < 2; ++run) {
        CHECK(initialize(c));
        double x = -1, derivative = -1;
        CHECK(states(c, &x) == 0 && x == 0);
        CHECK(derivatives(c, &derivative) == 0 && derivative == 1);
        /* A trial across the root cannot change held event truth. Raw truth
           would select sqrt(x-2), which is invalid at this coordinate. */
        x = 0.75;
        CHECK(set_states(c, &x) == 0);
        CHECK(derivatives(c, &derivative) == 0 && derivative == 0.5);
        CHECK(reset(c) == 0);
    }
    release(c);
    return 0;
}
"#;

const FMI2: &str = r#"
#define FMI2_FUNCTION_PREFIX @IDENTIFIER@_
#include "fmi2Functions.h"
typedef fmi2Component Instance;
static Instance instantiate(const char* id) {
    fmi2CallbackFunctions callbacks = { NULL, calloc, free, NULL, NULL };
    return fmi2Instantiate("initial", fmi2ModelExchange, id, NULL, &callbacks, 0, 0);
}
static int initialize(Instance c) {
    if (fmi2SetupExperiment(c, 0, 0, 0, 1, 1) != fmi2OK ||
        fmi2EnterInitializationMode(c) != fmi2OK || fmi2ExitInitializationMode(c) != fmi2OK) return 0;
    fmi2EventInfo event = {0};
    do { if (fmi2NewDiscreteStates(c, &event) != fmi2OK) return 0; } while (event.newDiscreteStatesNeeded);
    return fmi2EnterContinuousTimeMode(c) == fmi2OK;
}
static int states(Instance c, double* x) { return fmi2GetContinuousStates(c, x, 1); }
static int set_states(Instance c, const double* x) { return fmi2SetContinuousStates(c, x, 1); }
static int derivatives(Instance c, double* dx) { return fmi2GetDerivatives(c, dx, 1); }
#define reset fmi2Reset
#define release fmi2FreeInstance
"#;

const FMI3: &str = r#"
#define FMI3_FUNCTION_PREFIX @IDENTIFIER@_
#include "fmi3Functions.h"
typedef fmi3Instance Instance;
static Instance instantiate(const char* id) {
    return fmi3InstantiateModelExchange("initial", id, NULL, 0, 0, NULL, NULL);
}
static int initialize(Instance c) {
    if (fmi3EnterInitializationMode(c, 0, 0, 0, 1, 1) != fmi3OK ||
        fmi3ExitInitializationMode(c) != fmi3OK) return 0;
    fmi3Boolean again, terminate, nominals, states, next;
    fmi3Float64 time;
    do {
        if (fmi3UpdateDiscreteStates(c, &again, &terminate, &nominals, &states, &next, &time) != fmi3OK) return 0;
    } while (again);
    return fmi3EnterContinuousTimeMode(c) == fmi3OK;
}
static int states(Instance c, double* x) { return fmi3GetContinuousStates(c, x, 1); }
static int set_states(Instance c, const double* x) { return fmi3SetContinuousStates(c, x, 1); }
static int derivatives(Instance c, double* dx) { return fmi3GetContinuousStateDerivatives(c, dx, 1); }
#define reset fmi3Reset
#define release fmi3FreeInstance
"#;

#[test]
fn fmi_targets_refuse_authored_initial_events_independently_of_relation_mode() {
    let result = rumoca::Compiler::new().model("InitialEvent").compile_str(
        "model InitialEvent\n Real x(start=0, fixed=true);\n equation\n der(x)=if initial() then 1 else 0;\n end InitialEvent;",
        "InitialEvent.mo",
    ).unwrap();
    for target in ["fmi2", "fmi3"] {
        let error = rumoca::render_target_files(&result, "InitialEvent", target, None).unwrap_err();
        assert!(format!("{error:#}").contains("authored initial()"));
    }
}
