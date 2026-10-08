//! SPEC_0040 SOLVE-C66: typed calls need the canonical Real extremum helpers,
//! including operations inside nested function regions.

use super::*;

#[test]
fn packaged_fmi_real_extrema_in_nested_calls_preserve_values_and_zero_signs() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().unwrap();
    let compiled = rumoca::Compiler::new()
        .model("NestedExtrema")
        .compile_str(SOURCE, "NestedExtrema.mo")
        .expect("compile nested extrema");
    let driver = work.path().join("real_extrema.py");
    fs::write(&driver, DRIVER).unwrap();
    for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
        let fmu = build_named_fmu(work.path(), &compiled, target, "NestedExtrema");
        validate_source_package(&fmu, standard);
        checked_output(
            Command::new("python3").arg(&driver).arg(&fmu.archive),
            &format!("{target} nested Real extrema execution"),
        );
    }
}

const SOURCE: &str = r#"
function extrema
  input Real a;
  input Real b;
  input Boolean nested;
  output Real y[2];
algorithm
  if nested then
    for i in 1:2 loop
      y[i] := if i == 1 then min(a,b) else max(a,b);
    end for;
  else
    y := {min(a,b),max(a,b)};
  end if;
end extrema;
model NestedExtrema
  parameter Real a=3;
  parameter Real b=-2;
  parameter Boolean nested=true;
  Real x(start=1,fixed=true);
  output Real y[2];
equation
  der(x)=1;
  y = extrema(a,b,nested);
end NestedExtrema;
"#;

const DRIVER: &str = r#"
import sys
import numpy as np
from fmpy import read_model_description, simulate_fmu

md = read_model_description(sys.argv[1])
outputs = ['y'] if md.fmiVersion.startswith('3') else ['y[1]','y[2]']
for interface in ['ModelExchange','CoSimulation']:
    for nested in [False,True]:
        for a,b in [(3,-2),(-2,3),(1,1),(-0.0,0.0),(0.0,-0.0)]:
            trace = simulate_fmu(sys.argv[1], fmi_type=interface, stop_time=0.03,
                output_interval=0.01, start_values={'a':a,'b':b,'nested':nested},
                output=outputs)
            expected = np.asarray([a if a<b else b, a if a>b else b])
            for row in trace:
                actual = np.concatenate([np.atleast_1d(row[name]) for name in outputs])
                assert np.array_equal(actual,expected), (interface,nested,a,b,actual,expected)
                assert np.array_equal(np.signbit(actual),np.signbit(expected)), (interface,nested,a,b,actual,expected)
"#;
