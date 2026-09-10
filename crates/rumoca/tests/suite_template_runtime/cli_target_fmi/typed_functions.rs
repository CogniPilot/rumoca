//! C failure boundaries and lazy branches through the independent FMI importer.

use super::*;

#[test]
fn packaged_fmi_refuses_public_types_without_a_typed_fmi_abi() {
    let compiled = rumoca::Compiler::new().model("IntegerParameter").compile_str(
        "model IntegerParameter parameter Integer n=2; Real x(start=0,fixed=true); equation der(x)=n; end IntegerParameter;",
        "IntegerParameter.mo",
    ).unwrap();
    let work = tempdir().unwrap();
    for target in ["fmi2", "fmi3"] {
        let error = rumoca::compile_packaged_target(
            &compiled,
            "IntegerParameter",
            target,
            work.path().join(target),
        )
        .expect_err("an Integer value reference cannot silently become Float64");
        assert!(
            format!("{error:#}").contains("only Real public variables"),
            "{error:#}"
        );
    }
}

#[test]
fn packaged_fmi_typed_functions_preserve_integer_bounds_and_lazy_branches() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    let standards = standard_roots();
    let work = tempdir().unwrap();
    let compiled = rumoca::Compiler::new()
        .model("TypedProbe")
        .compile_str(SOURCE, "TypedProbe.mo")
        .unwrap();
    let driver = work.path().join("typed_functions.py");
    fs::write(&driver, DRIVER).unwrap();
    for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
        let fmu = build_named_fmu(work.path(), &compiled, target, "TypedProbe");
        validate_source_package(&fmu, standard);
        checked_output(
            Command::new("python3").arg(&driver).arg(&fmu.archive),
            &format!("{target} typed integer and lazy-branch execution"),
        );
    }
}

const SOURCE: &str = r#"
function probe
 input Real selector;
 output Real value;
 protected Real a[2]; Integer i;
algorithm
 a := {2,4};
 if selector <= 0 then
   value := 0;
 else
   i := integer(selector);
   if selector > 100 then
     value := i*i;
   else
     value := a[i];
   end if;
 end if;
end probe;
model TypedProbe
 parameter Real selector = 1.9;
 output Real value = probe(selector);
 Real x(start=0,fixed=true);
equation
 der(x) = value;
end TypedProbe;
"#;

const DRIVER: &str = r#"
import sys
from fmpy import simulate_fmu
from fmpy.fmi1 import FMICallException

for interface in ['ModelExchange', 'CoSimulation']:
    for selector, expected in [(1.9,2), (2.0,4), (-1.0,0), (1000.0,1000000)]:
        result = simulate_fmu(sys.argv[1], fmi_type=interface, stop_time=0.01,
            output_interval=0.001, start_values={'selector':selector}, output=['x','value'])
        assert max(abs(result['value']-expected)) == 0, (interface,selector,result)
        assert max(abs(result['x']-expected*result['time'])) < 1e-6, (interface,selector,result)
    for selector in [3.0, 1e20, 4e9]:
        try:
            simulate_fmu(sys.argv[1], fmi_type=interface, stop_time=0.01,
                start_values={'selector':selector})
        except FMICallException as error:
            assert error.status == 3, (interface,selector,error)
        else:
            raise AssertionError((interface,selector,'invalid index or integer overflow accepted'))
"#;
