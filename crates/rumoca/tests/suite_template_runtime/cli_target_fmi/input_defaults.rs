//! Getters are legal from initialization mode on (FMI 2.0.4 state table), so
//! No-Set and Reset check XML defaults against both versioned component ABIs.
use super::*;

#[test]
fn packaged_fmi_input_defaults_initialize_and_reset_without_source_starts() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().expect("FMI input-default conformance work");
    let driver = work.path().join("input_defaults.py");
    fs::write(&driver, DRIVER).unwrap();
    let compiled = rumoca::Compiler::new()
        .model("InputDefaults")
        .compile_str(SOURCE, "InputDefaults.mo")
        .unwrap();
    for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
        let fmu = build_named_fmu(work.path(), &compiled, target, "InputDefaults");
        validate_source_package(&fmu, standard);
        checked_output(
            Command::new("python3").arg(&driver).arg(&fmu.archive),
            &format!("{target} input XML/Instantiate/Reset agreement"),
        );
    }
}

const SOURCE: &str = r#"
model InputDefaults
  parameter Real lower = 2;
  input Real u[2](each min=lower);
  input Real z(start=-0.0);
  input Integer index(min=7);
  input Boolean enabled;
  output Real y;
  Real x(start=0,fixed=true);
equation
  der(x)=sum(u);
  y=x+z+index+(if enabled then 1 else 0);
end InputDefaults;
"#;

const DRIVER: &str = r#"
import math
import shutil
import sys
from fmpy import extract, read_model_description
from fmpy.simulation import instantiate_fmu

path = sys.argv[1]
md = read_model_description(path)
fmi3 = md.fmiVersion.startswith('3.')
variables = {v.name: v for v in md.modelVariables}
u = [variables['u']] if fmi3 else [variables['u[1]'], variables['u[2]']]
assert all(v.causality == 'input' for v in u)
assert list(map(float, u[0].start.split())) == [2.0] if fmi3 else all(float(v.start) == 2.0 for v in u)
assert variables['index'].type == ('Int64' if fmi3 else 'Integer')
assert int(variables['index'].start) == 7
assert str(variables['enabled'].start).lower() in ('false', '0')
assert math.copysign(1.0, float(variables['z'].start)) == -1.0

for interface in ['ModelExchange', 'CoSimulation']:
    directory = extract(path)
    fmu = instantiate_fmu(directory, md, fmi_type=interface)
    real_get = fmu.getFloat64 if fmi3 else fmu.getReal
    real_set = fmu.setFloat64 if fmi3 else fmu.setReal
    integer_get = fmu.getInt64 if fmi3 else fmu.getInteger
    integer_set = fmu.setInt64 if fmi3 else fmu.setInteger
    urefs = [v.valueReference for v in u]
    iref = [variables['index'].valueReference]
    bref = [variables['enabled'].valueReference]
    zref = [variables['z'].valueReference]
    def reals(refs, count):
        return real_get(refs, nValues=count) if fmi3 else real_get(refs)
    def defaults():
        assert reals(urefs, 2) == [2.0, 2.0]
        assert integer_get(iref) == [7]
        assert not any(fmu.getBoolean(bref))
        assert math.copysign(1.0, reals(zref, 1)[0]) == -1.0
    def initialize():
        if not fmi3: fmu.setupExperiment(startTime=0.0)
        fmu.enterInitializationMode()
        defaults()
        fmu.exitInitializationMode()
    try:
        initialize()
        defaults()
        real_set(urefs, [4.0, 5.0])
        integer_set(iref, [9])
        fmu.setBoolean(bref, [True])
        assert reals(urefs, 2) == [4.0, 5.0]
        assert integer_get(iref) == [9]
        assert all(fmu.getBoolean(bref))
        fmu.reset()
        initialize()
        defaults()
    finally:
        fmu.freeInstance()
        shutil.rmtree(directory)
"#;
