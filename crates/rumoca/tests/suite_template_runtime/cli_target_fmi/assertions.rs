//! Parameter assertions survive source packaging and typed function emission.

use super::*;

#[test]
fn packaged_fmi_parameter_assertions_preserve_validation_and_free_fall() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().expect("assertion FMI work directory");
    let compiled = rumoca::Compiler::new()
        .model("GuardedBody")
        .compile_str(SOURCE, "GuardedBody.mo")
        .expect("compile parameter-guarded tensor functions");
    let driver = work.path().join("assertions.py");
    fs::write(&driver, DRIVER).expect("write independent assertion importer");
    for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
        let fmu = build_named_fmu(work.path(), &compiled, target, "GuardedBody");
        validate_source_package(&fmu, standard);
        checked_output(
            Command::new("python3").arg(&driver).arg(&fmu.archive),
            &format!("{target} assertion validation and analytic free fall"),
        );
    }
}

/// A predicate that changes with time, a state, or an input and that no event
/// tracks, a call or a `noEvent` relation, owns no event (MLS §8.3.7): the
/// component admits it and advertises no indicator for it (#593).
#[test]
fn packaged_fmi_admits_changing_assertion_predicates_without_indicators() {
    for argument in [
        "valid(time)",
        "valid(x)",
        "valid(u)",
        "noEvent(time < 0.5)",
        "noEvent(x < 0.5)",
        "noEvent(u < 0.5)",
    ] {
        let source = format!(
            r#"
function valid
 input Real value;
 output Boolean accepted;
algorithm
 accepted := value > -1;
end valid;
model ChangingAssertion
 input Real u = 0;
 Real x(start=0,fixed=true);
equation
 der(x)=1;
 assert({argument}, "changing predicate");
end ChangingAssertion;
"#
        );
        let compiled = rumoca::Compiler::new()
            .model("ChangingAssertion")
            .compile_str(&source, "ChangingAssertion.mo")
            .expect("compile changing assertion");
        let work = tempdir().expect("changing assertion work directory");
        for target in ["fmi2", "fmi3"] {
            let destination = work.path().join(target);
            rumoca::compile_packaged_target(
                &compiled,
                "ChangingAssertion",
                target,
                destination.clone(),
            )
            .unwrap_or_else(|error| panic!("{target}/{argument}: {error:#}"));
            let description = fs::read_to_string(
                destination
                    .join("ChangingAssertion")
                    .join("modelDescription.xml"),
            )
            .expect("the packaged component describes itself");
            assert_eq!(
                indicator_count(target, &description),
                0,
                "{target}/{argument}: {description}"
            );
        }
    }
}

/// Such an assertion is checked only at accepted points (#593): a trial
/// evaluation past its failure passes, and the first accepted point at or
/// after t = 0.06 fails with the message, in Model Exchange and in
/// Co-Simulation, with no event in the model and next to an unrelated one.
#[test]
fn packaged_fmi_checks_a_noevent_assertion_at_accepted_points() {
    for (label, declaration, event) in [
        ("without_events", "", ""),
        (
            "with_an_event",
            "discrete Real k(start=0,fixed=true);",
            "when x > 0.05 then k = pre(k) + 1; end when;",
        ),
    ] {
        let source = format!(
            r#"
model AcceptedAssertion
 Real x(start=0,fixed=true);
 {declaration}
equation
 der(x)=sin(10*time);
 {event}
 assert(noEvent(time < 0.06 or x > 10), "must hold before 0.06");
end AcceptedAssertion;
"#
        );
        let compiled = rumoca::Compiler::new()
            .model("AcceptedAssertion")
            .compile_str(&source, "AcceptedAssertion.mo")
            .expect("compile accepted-point assertion");
        let work = tempdir().expect("accepted assertion work directory");
        for target in ["fmi2", "fmi3"] {
            let destination = work.path().join(target);
            rumoca::compile_packaged_target(
                &compiled,
                "AcceptedAssertion",
                target,
                destination.clone(),
            )
            .unwrap_or_else(|error| panic!("{label}/{target}: {error:#}"));
            let root = destination.join("AcceptedAssertion");
            let description = fs::read_to_string(root.join("modelDescription.xml"))
                .expect("the packaged component describes itself");
            assert_eq!(
                indicator_count(target, &description),
                usize::from(!event.is_empty()),
                "{label}/{target}: only the when-clause owns an indicator"
            );
            run_accepted_driver(label, target, &root, &description);
        }
    }
}

/// Builds the packaged component in `root` with its Model Exchange and
/// Co-Simulation driver and checks where the assertion fails in each.
fn run_accepted_driver(label: &str, target: &str, root: &std::path::Path, description: &str) {
    let (driver, token, headers) = match target {
        "fmi2" => (
            ACCEPTED_FMI2,
            lifecycle::attribute(description, "guid"),
            "crates/rumoca/tests/fixtures/fmi2-headers",
        ),
        _ => (
            ACCEPTED_FMI3,
            lifecycle::attribute(description, "instantiationToken"),
            "crates/rumoca-phase-codegen/src/templates/fmi-ls-wasm/fmi3-headers",
        ),
    };
    let input = root.join("accepted.c");
    let binary = root.join("accepted");
    fs::write(&input, driver.replace("@TOKEN@", token)).unwrap();
    checked_output(
        Command::new("cc")
            .args([
                "-std=c11",
                "-ffp-contract=off",
                "-Wall",
                "-Wextra",
                "-Werror",
            ])
            .arg(format!("-I{}", root.join("sources").display()))
            .arg(format!("-I{}", workspace_root().join(headers).display()))
            .arg(&input)
            .args(lifecycle::source_units(root))
            .args(["-lm", "-o"])
            .arg(&binary),
        "compile accepted-point assertion driver",
    );
    for mode in ["me", "cs"] {
        let output = checked_output(
            Command::new(&binary).arg(mode),
            "execute accepted-point assertion driver",
        );
        let report = String::from_utf8_lossy(&output.stdout);
        let expected = if mode == "me" {
            "trial ok\nfailed at 0.060\n"
        } else {
            "step 0.02 ok\nstep 0.04 ok\nfailed at 0.060\n"
        };
        assert_eq!(report, expected, "{label}/{target}/{mode}");
    }
}

fn indicator_count(target: &str, description: &str) -> usize {
    if target == "fmi2" {
        lifecycle::attribute(description, "numberOfEventIndicators")
            .parse()
            .expect("an FMI 2 indicator count")
    } else {
        description.matches("<EventIndicator ").count()
    }
}

/// Model Exchange: a trial derivative evaluation at t = 0.07, then explicit
/// Euler steps of 1 ms from 0, each completed; Co-Simulation: steps of 20 ms.
/// The assertion message must reach the logger.
const ACCEPTED_FMI2: &str = r#"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "model.h"
#include "fmi2Functions.h"
static int logged = 0;
static void logger(fmi2ComponentEnvironment e, fmi2String n, fmi2Status s, fmi2String category, fmi2String message, ...) {
    (void)e; (void)n; (void)s; (void)category;
    char text[512];
    va_list arguments;
    va_start(arguments, message);
    vsnprintf(text, sizeof text, message, arguments);
    va_end(arguments);
    if (strstr(text, "must hold before 0.06")) logged = 1;
}
int main(int argc, char** argv) {
    const int cs = argc > 1 && strcmp(argv[1], "cs") == 0;
    fmi2CallbackFunctions callbacks = {0};
    callbacks.logger = logger; callbacks.allocateMemory = calloc; callbacks.freeMemory = free;
    fmi2Component m = fmi2Instantiate("accepted", cs ? fmi2CoSimulation : fmi2ModelExchange, "@TOKEN@", NULL, &callbacks, 0, 1);
    if (!m || fmi2SetupExperiment(m, 0, 0, 0, 0, 0) != fmi2OK || fmi2EnterInitializationMode(m) != fmi2OK || fmi2ExitInitializationMode(m) != fmi2OK) return 2;
    if (cs) {
        for (int k = 0; k < 5; ++k) {
            const double t = 0.02 * k;
            if (fmi2DoStep(m, t, 0.02, fmi2True) != fmi2OK) { printf("failed at %.3f\n", t + 0.02); return logged ? 0 : 3; }
            printf("step %.2f ok\n", t + 0.02);
        }
        return 4;
    }
    fmi2EventInfo info;
    if (fmi2NewDiscreteStates(m, &info) != fmi2OK || fmi2EnterContinuousTimeMode(m) != fmi2OK) return 2;
    double x = 0.0, dx = 0.0;
    fmi2Boolean enter, terminate;
    if (fmi2SetTime(m, 0.07) != fmi2OK || fmi2SetContinuousStates(m, &x, 1) != fmi2OK || fmi2GetDerivatives(m, &dx, 1) != fmi2OK) return 5;
    printf("trial ok\n");
    for (int k = 1; k <= 100; ++k) {
        const double t = 0.001 * k;
        x += 0.001 * dx;
        if (fmi2SetTime(m, t) != fmi2OK || fmi2SetContinuousStates(m, &x, 1) != fmi2OK || fmi2GetDerivatives(m, &dx, 1) != fmi2OK) return 6;
        if (fmi2CompletedIntegratorStep(m, fmi2True, &enter, &terminate) != fmi2OK) { printf("failed at %.3f\n", t); return logged ? 0 : 3; }
    }
    return 4;
}
"#;

const ACCEPTED_FMI3: &str = r#"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "model.h"
#include "fmi3Functions.h"
static int logged = 0;
static void logger(fmi3InstanceEnvironment e, fmi3Status s, fmi3String category, fmi3String message) {
    (void)e; (void)s; (void)category;
    if (strstr(message, "must hold before 0.06")) logged = 1;
}
int main(int argc, char** argv) {
    const int cs = argc > 1 && strcmp(argv[1], "cs") == 0;
    fmi3Instance m = cs
        ? fmi3InstantiateCoSimulation("accepted", "@TOKEN@", NULL, fmi3False, fmi3True, fmi3False, fmi3False, NULL, 0, NULL, logger, NULL)
        : fmi3InstantiateModelExchange("accepted", "@TOKEN@", NULL, fmi3False, fmi3True, NULL, logger);
    if (!m || fmi3EnterInitializationMode(m, fmi3False, 0, 0, fmi3False, 0) != fmi3OK || fmi3ExitInitializationMode(m) != fmi3OK) return 2;
    if (cs) {
        for (int k = 0; k < 5; ++k) {
            const double t = 0.02 * k;
            fmi3Boolean event, terminate, early;
            double last;
            if (fmi3DoStep(m, t, 0.02, fmi3True, &event, &terminate, &early, &last) != fmi3OK) { printf("failed at %.3f\n", t + 0.02); return logged ? 0 : 3; }
            printf("step %.2f ok\n", t + 0.02);
        }
        return 4;
    }
    fmi3Boolean b1, b2, b3, b4, b5;
    double next;
    if (fmi3UpdateDiscreteStates(m, &b1, &b2, &b3, &b4, &b5, &next) != fmi3OK || fmi3EnterContinuousTimeMode(m) != fmi3OK) return 2;
    double x = 0.0, dx = 0.0;
    fmi3Boolean enter, terminate;
    if (fmi3SetTime(m, 0.07) != fmi3OK || fmi3SetContinuousStates(m, &x, 1) != fmi3OK || fmi3GetContinuousStateDerivatives(m, &dx, 1) != fmi3OK) return 5;
    printf("trial ok\n");
    for (int k = 1; k <= 100; ++k) {
        const double t = 0.001 * k;
        x += 0.001 * dx;
        if (fmi3SetTime(m, t) != fmi3OK || fmi3SetContinuousStates(m, &x, 1) != fmi3OK || fmi3GetContinuousStateDerivatives(m, &dx, 1) != fmi3OK) return 6;
        if (fmi3CompletedIntegratorStep(m, fmi3True, &enter, &terminate) != fmi3OK) { printf("failed at %.3f\n", t); return logged ? 0 : 3; }
    }
    return 4;
}
"#;

#[test]
fn packaged_fmi_reports_state_events_independently_of_a_static_assertion() {
    for assertion in ["", "assert(mass > 0, \"mass must be positive\");"] {
        let source = format!(
            r#"
model StaticAssertConditional
 parameter Real mass=1;
 Real z(start=0);
equation
 {assertion}
 der(z)=if z < 0 then 1 else -1;
end StaticAssertConditional;
"#
        );
        let compiled = rumoca::Compiler::new()
            .model("StaticAssertConditional")
            .compile_str(&source, "StaticAssertConditional.mo")
            .unwrap();
        let work = tempdir().unwrap();
        for target in ["fmi2", "fmi3"] {
            let destination = work.path().join(target);
            rumoca::compile_packaged_target(
                &compiled,
                "StaticAssertConditional",
                target,
                destination.clone(),
            )
            .unwrap_or_else(|error| panic!("{target}: {error:#}"));
            let description = fs::read_to_string(
                destination
                    .join("StaticAssertConditional")
                    .join("modelDescription.xml"),
            )
            .expect("the packaged component describes itself");
            let advertised = if target == "fmi2" {
                description.contains("numberOfEventIndicators=\"1\"")
            } else {
                description.matches("<EventIndicator ").count() == 1
            };
            assert!(
                advertised,
                "{target} advertises its one state event: {description}"
            );
        }
    }
}

const SOURCE: &str = r#"
function validParameters
 input Real mass;
 input Real gravity;
 output Boolean accepted;
algorithm
 accepted := mass > 0 and gravity >= 0;
end validParameters;

function solveInertia
 input Real J[3,3];
 input Real b[3];
 output Real x[3];
 output Boolean accepted;
 protected Real L[3,3]; Real z[3]; Real pivot;
algorithm
 L := zeros(3,3);
 x := zeros(3);
 z := zeros(3);
 accepted := true;
 for i in 1:3 loop
   for j in 1:3 loop
    if j <= i then
     pivot := J[i,j];
     for k in 1:3 loop
       if k < j then
         pivot := pivot - L[i,k]*L[j,k];
       end if;
     end for;
     if i == j then
       if pivot > 0 then
         L[i,j] := sqrt(pivot);
       else
         accepted := false;
         L[i,j] := 1;
       end if;
     else
       L[i,j] := pivot/L[j,j];
     end if;
    end if;
   end for;
 end for;
 for i in 1:3 loop
   pivot := b[i];
   for k in 1:3 loop
     if k < i then
       pivot := pivot - L[i,k]*z[k];
     end if;
   end for;
   z[i] := pivot/L[i,i];
 end for;
 for i in 3:-1:1 loop
   pivot := z[i];
   for k in 1:3 loop
     if k > i then
       pivot := pivot - L[k,i]*x[k];
     end if;
   end for;
   x[i] := pivot/L[i,i];
 end for;
end solveInertia;

function acceleration
 input Real mass;
 input Real J[3,3];
 input Real force[3];
 output Real a[3];
 protected Boolean accepted;
algorithm
 assert(mass > 0,"mass must be positive");
 (a,accepted) := solveInertia(J,force);
 assert(accepted,"inertia must be positive definite");
 a := a/mass;
end acceleration;

model GuardedBody
 parameter Real mass = 1;
 parameter Real g = 9.8;
 parameter Real inertia = 2;
 Real p[3](each start=0,each fixed=true);
 Real v[3](each start=0,each fixed=true);
 parameter Real J[3,3] = {{inertia,0,0},{0,3,0},{0,0,4}};
equation
 assert(validParameters(mass,g),"physical parameters must be valid");
 der(p)=v;
 der(v)=acceleration(mass,J,cross(v,v))+{0,0,-g};
end GuardedBody;
"#;

const DRIVER: &str = r#"
import sys
import numpy as np
from fmpy import extract, read_model_description, simulate_fmu
from fmpy.simulation import instantiate_fmu
from fmpy.fmi1 import FMICallException
import shutil

description = read_model_description(sys.argv[1])
variables = {v.name:v for v in description.modelVariables}
fmi3 = description.fmiVersion.startswith('3.')
inertia_vr = variables['inertia'].valueReference
matrix_variables = [variables['J']] if fmi3 else [variables[f'J[{i},{j}]'] for i in range(1,4) for j in range(1,4)]
assert all(v.causality == 'calculatedParameter' for v in matrix_variables)

def check_parameter_lifecycle(interface):
    directory = extract(sys.argv[1])
    fmu = instantiate_fmu(directory, description, fmi_type=interface)
    get = fmu.getFloat64 if fmi3 else fmu.getReal
    set_value = fmu.setFloat64 if fmi3 else fmu.setReal
    def initialize():
        if not fmi3:
            fmu.setupExperiment(startTime=0.0)
        fmu.enterInitializationMode()
        fmu.exitInitializationMode()
    def matrix():
        references = [v.valueReference for v in matrix_variables]
        return get(references, nValues=9) if fmi3 else get(references)
    try:
        initialize()
        assert matrix() == [2,0,0,0,3,0,0,0,4]
        set_value([inertia_vr], [5.0])
        assert matrix() == [5,0,0,0,3,0,0,0,4], (interface, matrix())
        set_value([inertia_vr], [-1.0])
        try:
            matrix()
        except FMICallException as error:
            assert error.status == 3, error
        else:
            raise AssertionError((interface, 'changed invalid inertia was accepted'))
        fmu.reset()
        initialize()
        assert matrix() == [2,0,0,0,3,0,0,0,4]
    finally:
        fmu.freeInstance()
        shutil.rmtree(directory)

for interface in ['ModelExchange', 'CoSimulation']:
    check_parameter_lifecycle(interface)
    result = simulate_fmu(sys.argv[1], fmi_type=interface, stop_time=1.0,
        output_interval=0.01, output=['p', 'v', 'p[1]', 'p[2]', 'p[3]', 'v[1]', 'v[2]', 'v[3]'],
        relative_tolerance=1e-9)
    def vector(row, name):
        return np.asarray(row[name]) if name in result.dtype.names else np.array([row[f'{name}[{i}]'] for i in range(1,4)])
    for row in result:
        t=float(row['time'])
        assert np.max(np.abs(vector(row,'p')-[0,0,-4.9*t*t])) < 1e-6, (interface,t,row)
        assert np.max(np.abs(vector(row,'v')-[0,0,-9.8*t])) < 1e-6, (interface,t,row)
    for parameters in [{'mass':-1}, {'mass':0}, {'g':-1}, {'inertia':-1}, {'inertia':0}]:
        try:
            simulate_fmu(sys.argv[1], fmi_type=interface, stop_time=0.01, start_values=parameters)
        except FMICallException as error:
            assert error.status == 3, (parameters,error)
        else:
            raise AssertionError((interface,parameters,'invalid physical parameters were accepted'))
"#;

#[test]
fn a_retained_assertion_in_a_sampled_call_uses_its_clock_lane() {
    let source = r#"
model ClockedCheck
  function check
    input Real u;
    output Real y;
  algorithm
    assert(u>0,"clocked test");
    y:=u;
  end check;
  input Real u(start=1);
  discrete output Real y(start=0,fixed=true);
equation
  when sample(.5,.5) then y=check(u); end when;
end ClockedCheck;
"#;
    let compiled = rumoca::Compiler::new()
        .model("ClockedCheck")
        .compile_str(source, "ClockedCheck.mo")
        .unwrap();
    let work = tempdir().unwrap();
    for target in ["fmi2", "fmi3"] {
        let destination = work.path().join(target);
        rumoca::compile_packaged_target(&compiled, "ClockedCheck", target, destination.clone())
            .unwrap();
        let code = fs::read_to_string(destination.join("ClockedCheck/sources/model.c")).unwrap();
        assert!(code.contains("if (event_commit && m->p["));
        assert!(
            code.contains(r"\143\154\157\143\153\145\144\040\164\145\163\164"),
            "nested call retains its source assertion message"
        );
    }
}
