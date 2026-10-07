//! Enumeration variables export their type definition and reject an ordinal
//! no literal has (MLS §4.9.5; FMI 2.0.4 section 2.2.3, FMI 3.0 section 2.4.7).
//!
//! FMI 2 declares `<SimpleType><Enumeration>` and reads an enumeration variable
//! with `fmi2GetInteger`; FMI 3 declares `<EnumerationType>` and reads it with
//! `fmi3GetInt64`. Either way the declared type lets an importer show
//! `Flight.Mode.Climb` instead of `2`.

use super::*;

const SOURCE: &str = r#"
package Flight
  type Mode = enumeration(Off, Climb, Cruise);
end Flight;

model EnumeratedMode
  import Flight.Mode;
  parameter Mode initial_mode = Mode.Climb;
  input Mode cmd = Mode.Off;
  output Mode y;
  Real x(start = 0, fixed = true);
equation
  y = cmd;
  der(x) = if y == Mode.Climb then 1 else 0;
end EnumeratedMode;
"#;

/// An importer that reads the model description's types and drives the
/// component through its enumeration variable.
const DRIVER: &str = r#"
import sys
from fmpy import extract, read_model_description

archive, version = sys.argv[1], sys.argv[2]
description = read_model_description(archive)
types = {t.name: t for t in description.typeDefinitions}
mode = types["Flight.Mode"]
assert [(item.name, int(item.value)) for item in mode.items] == [("Off", 1), ("Climb", 2), ("Cruise", 3)], mode.items
variables = {v.name: v for v in description.modelVariables}
for name in ["initial_mode", "cmd", "y"]:
    assert variables[name].type == "Enumeration", (name, variables[name].type)
    assert variables[name].declaredType is mode, name
assert variables["x"].declaredType is None

if version == "fmi2":
    from fmpy.fmi2 import FMU2Slave as Slave
else:
    from fmpy.fmi3 import FMU3Slave as Slave
unzip = extract(archive)
fmu = Slave(guid=description.guid if version == "fmi2" else description.instantiationToken,
            unzipDirectory=unzip, modelIdentifier=description.coSimulation.modelIdentifier,
            instanceName="enumerated")
fmu.instantiate()
if version == "fmi2":
    fmu.setupExperiment(startTime=0.0)
    set_ordinals, get_ordinals, get_reals = fmu.setInteger, fmu.getInteger, fmu.getReal
    fmu.enterInitializationMode()
    fmu.exitInitializationMode()
else:
    set_ordinals, get_ordinals, get_reals = fmu.setInt64, fmu.getInt64, fmu.getFloat64
    fmu.enterInitializationMode(startTime=0.0)
    fmu.exitInitializationMode()
cmd, y, x = (variables[name].valueReference for name in ["cmd", "y", "x"])

assert get_ordinals([cmd]) == [1] and get_ordinals([y]) == [1]
for bad in [0, 4, -1, 1 << 40]:
    try:
        set_ordinals([cmd], [bad])
    except Exception:
        continue
    raise AssertionError(f"ordinal {bad} was accepted")
assert get_ordinals([cmd]) == [1], "a rejected set leaves the value"
for ordinal in [1, 2, 3]:
    set_ordinals([cmd], [ordinal])
    assert get_ordinals([cmd]) == [ordinal]
set_ordinals([cmd], [2])
fmu.doStep(currentCommunicationPoint=0.0, communicationStepSize=1.0)
assert get_ordinals([y]) == [2], get_ordinals([y])
assert abs(get_reals([x])[0] - 1.0) < 1e-6, get_reals([x])
fmu.terminate()
fmu.freeInstance()
"#;

#[test]
fn packaged_fmi_enumerations_declare_their_type_and_reject_a_foreign_ordinal() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().expect("enumeration FMI work directory");
    let driver = work.path().join("enumerations.py");
    fs::write(&driver, DRIVER).expect("write the enumeration importer");
    let compiled = rumoca::Compiler::new()
        .model("EnumeratedMode")
        .compile_str(SOURCE, "EnumeratedMode.mo")
        .expect("compile the enumeration model");
    for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
        let fmu = build_named_fmu(work.path(), &compiled, target, "EnumeratedMode");
        validate_source_package(&fmu, standard);
        checked_output(
            Command::new("python3")
                .arg(&driver)
                .arg(&fmu.archive)
                .arg(target),
            &format!("{target} enumeration type definitions and range check"),
        );
    }
}
