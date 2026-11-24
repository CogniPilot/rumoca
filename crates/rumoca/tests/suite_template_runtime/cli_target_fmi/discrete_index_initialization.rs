//! A packaged FMI3 model whose continuous row reads a discrete Integer index
//! defined by an equation initializes with the equation's value (MLS 3.7
//! §8.6), not the declaration default. Needs only a C compiler: the FMI3
//! headers come from the repository.
use super::*;

const SELECTED: &str = r#"
model Selected
  parameter Real p[4] = {1, 2, 3, 4};
  input Integer k(start = 1);
  Integer k2;
  output Real y;
equation
  k2 = k;
  y = p[k2];
end Selected;
"#;

const CHAINED: &str = r#"
model Chained
  parameter Real p[4] = {1, 2, 3, 4};
  input Integer k(start = 1);
  Integer k2;
  Integer k3;
  output Real y;
equation
  k2 = k;
  k3 = k2 + 1;
  y = p[k3];
end Chained;
"#;

/// A sampled body reading `pre` of a seeded equation (`a = 1`) sees the value
/// the initial event published, at the t = 0 tick as at the t = 0.5 tick: the
/// clocked pass runs after the event advanced `pre`, as the simulator does
/// (the initial-instant start is observable only through unclocked rows, which
/// suite_core `when_activation_settled_reads` pins).
const SAMPLED: &str = r#"
model Sampled
  Real t(start = 0, fixed = true);
  Integer a(start = 0, fixed = true);
  output Integer seen(start = -1, fixed = true);
equation
  der(t) = 1;
  a = 1;
  when sample(0, 0.5) then
    seen = pre(a);
  end when;
end Sampled;
"#;

const SAMPLED_DRIVER: &str = r#"
#include <stdio.h>
#define FMI3_FUNCTION_PREFIX @IDENT@_
#include "fmi3Functions.h"
#define CHECK(test) do { if (!(test)) { fprintf(stderr, "failed line %d: %s\\n", __LINE__, #test); return 1; } } while (0)
static int settle(fmi3Instance m, fmi3Float64* next, fmi3Boolean* next_defined) {
    fmi3Boolean need, terminate, nominals, values;
    do { CHECK(fmi3UpdateDiscreteStates(m, &need, &terminate, &nominals, &values, next_defined, next) == fmi3OK); } while (need);
    CHECK(fmi3EnterContinuousTimeMode(m) == fmi3OK);
    return 0;
}
int main(void) {
    fmi3Instance m = fmi3InstantiateModelExchange("sampled", "@TOKEN@", NULL, fmi3False, fmi3False, NULL, NULL);
    CHECK(m != NULL);
    CHECK(fmi3EnterInitializationMode(m, fmi3False, 0.0, 0.0, fmi3True, 1.0) == fmi3OK);
    CHECK(fmi3ExitInitializationMode(m) == fmi3OK);
    fmi3Float64 next = 0.0; fmi3Boolean next_defined = fmi3False;
    CHECK(settle(m, &next, &next_defined) == 0);
    fmi3ValueReference seen = @SVR@;
    fmi3Int64 value = -2;
    CHECK(fmi3GetInt64(m, &seen, 1, &value, 1) == fmi3OK);
    CHECK(value == 1);
    CHECK(next_defined && next == 0.5);
    CHECK(fmi3SetTime(m, next) == fmi3OK);
    CHECK(fmi3EnterEventMode(m) == fmi3OK);
    CHECK(settle(m, &next, &next_defined) == 0);
    CHECK(fmi3GetInt64(m, &seen, 1, &value, 1) == fmi3OK);
    CHECK(value == 1);
    CHECK(fmi3Terminate(m) == fmi3OK);
    fmi3FreeInstance(m);
    return 0;
}
"#;

/// Initialize with the input at 3 and read `y` after the first event; the
/// expected value is substituted per model.
const DRIVER: &str = r#"
#include <stdio.h>
#define FMI3_FUNCTION_PREFIX @IDENT@_
#include "fmi3Functions.h"
#define CHECK(test) do { if (!(test)) { fprintf(stderr, "failed line %d: %s\n", __LINE__, #test); return 1; } } while (0)
int main(void) {
    fmi3Instance m = fmi3InstantiateModelExchange("selected", "@TOKEN@", NULL, fmi3False, fmi3False, NULL, NULL);
    CHECK(m != NULL);
    CHECK(fmi3EnterInitializationMode(m, fmi3False, 0.0, 0.0, fmi3True, 1.0) == fmi3OK);
    fmi3ValueReference k = @KVR@, y = @YVR@;
    fmi3Int64 input = 3;
    CHECK(fmi3SetInt64(m, &k, 1, &input, 1) == fmi3OK);
    CHECK(fmi3ExitInitializationMode(m) == fmi3OK);
    fmi3Boolean need, terminate, nominals, values, next_defined; fmi3Float64 next;
    do { CHECK(fmi3UpdateDiscreteStates(m, &need, &terminate, &nominals, &values, &next_defined, &next) == fmi3OK); } while (need);
    CHECK(fmi3EnterContinuousTimeMode(m) == fmi3OK);
    fmi3Float64 value = -1.0;
    CHECK(fmi3GetFloat64(m, &y, 1, &value, 1) == fmi3OK);
    CHECK(value == @EXPECTED@);
    CHECK(fmi3Terminate(m) == fmi3OK);
    fmi3FreeInstance(m);
    return 0;
}
"#;

fn value_reference(xml: &str, name: &str) -> String {
    let needle = format!("name=\"{name}\"");
    let element = xml
        .lines()
        .find(|line| line.contains(&needle))
        .unwrap_or_else(|| panic!("model description declares {name}"));
    lifecycle::attribute(element, "valueReference").to_string()
}

fn check(source: &str, model: &str, expected: &str) {
    run(
        source,
        model,
        DRIVER,
        &[("@KVR@", "k"), ("@YVR@", "y")],
        expected,
    );
}

fn run(source: &str, model: &str, driver: &str, references: &[(&str, &str)], expected: &str) {
    if !command_is_available("cc") {
        return;
    }
    let compiled = rumoca::Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .expect("compile the discrete index model");
    let work = tempdir().expect("discrete index work directory");
    let fmu = build_named_fmu(work.path(), &compiled, "fmi3", model);
    let xml = fs::read_to_string(fmu.root.join("modelDescription.xml")).unwrap();
    let mut driver = driver
        .replace("@IDENT@", lifecycle::attribute(&xml, "modelIdentifier"))
        .replace("@TOKEN@", lifecycle::attribute(&xml, "instantiationToken"))
        .replace("@EXPECTED@", expected);
    for (marker, name) in references {
        driver = driver.replace(marker, &value_reference(&xml, name));
    }
    let source_path = fmu.root.join("discrete_index.c");
    let binary = fmu.root.join("discrete_index");
    fs::write(&source_path, driver).unwrap();
    let headers =
        workspace_root().join("crates/rumoca-phase-codegen/src/templates/fmi-ls-wasm/fmi3-headers");
    checked_output(
        Command::new("cc")
            .args(["-std=c11", "-Wall", "-Wextra", "-Werror"])
            .arg(format!("-I{}", headers.display()))
            .args(lifecycle::source_units(&fmu.root))
            .arg(&source_path)
            .args(["-lm", "-o"])
            .arg(&binary),
        "compile the discrete index driver",
    );
    checked_output(
        &mut Command::new(binary),
        "execute the discrete index driver",
    );
}

#[test]
fn packaged_fmi3_initializes_a_discrete_integer_index_from_its_equation() {
    check(SELECTED, "Selected", "3.0");
}

#[test]
fn packaged_fmi3_initializes_a_chain_of_discrete_index_equations() {
    check(CHAINED, "Chained", "4.0");
}

#[test]
fn packaged_fmi3_sampled_read_of_a_seeded_equation_matches_the_simulator() {
    run(SAMPLED, "Sampled", SAMPLED_DRIVER, &[("@SVR@", "seen")], "");
}
