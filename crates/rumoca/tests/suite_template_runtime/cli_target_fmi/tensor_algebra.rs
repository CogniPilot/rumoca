//! MLS §10.6: dense tensor owners retain row-major values through FMI 2/3.

use super::*;

#[test]
fn packaged_fmi_tensor_products_and_transpose_match_independent_numpy_values() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().unwrap();
    let compiled = rumoca::Compiler::new()
        .model("TensorAlgebra")
        .compile_str(SOURCE, "TensorAlgebra.mo")
        .expect("compile rectangular tensor algebra");
    let driver = work.path().join("tensor_algebra.py");
    fs::write(&driver, DRIVER).unwrap();
    for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
        let fmu = build_named_fmu(work.path(), &compiled, target, "TensorAlgebra");
        validate_source_package(&fmu, standard);
        checked_output(
            Command::new("python3").arg(&driver).arg(&fmu.archive),
            &format!("{target} rectangular matrix and rank-three transpose execution"),
        );
    }
}

const SOURCE: &str = r#"
model TensorAlgebra
  input Real u=3;
  output Real x(start=1, fixed=true);
  Real a[2,3];
  output Real b[3,2];
  output Real product[2,2];
  output Real left[3];
  output Real right[2];
  output Real transposed[3,2,2];
equation
  der(x)=1;
  a={{x,u,2},{3,4,5}};
  b=transpose(a);
  product=a*b;
  left={2,3}*a;
  right=a*{1,2,3};
  transposed=transpose({{{x,2},{3,4},{5,6}},{{7,8},{9,10},{11,12}}});
end TensorAlgebra;
"#;

const DRIVER: &str = r#"
import itertools
import sys
import numpy as np
from fmpy import read_model_description, simulate_fmu

description = read_model_description(sys.argv[1])
outputs = [v.name for v in description.modelVariables if v.causality == 'output']

def tensor(row, name, shape):
    if name in row.dtype.names:
        return np.asarray(row[name]).reshape(shape)
    names = [name + '[' + ','.join(str(i+1) for i in index) + ']'
             for index in itertools.product(*(range(n) for n in shape))]
    return np.asarray([row[n] for n in names]).reshape(shape)

for interface in ['ModelExchange', 'CoSimulation']:
    for u in [3.0, -2.0]:
        trace = simulate_fmu(sys.argv[1], fmi_type=interface, stop_time=0.05,
            output_interval=0.01, start_values={'u':u}, output=outputs)
        for row in trace:
            x = 1.0 + row['time']
            assert abs(row['x'] - x) < 1e-8
            a = np.asarray([[x,u,2.0],[3.0,4.0,5.0]])
            cube = np.asarray([[[x,2],[3,4],[5,6]],[[7,8],[9,10],[11,12]]])
            expected = {'b':a.T, 'product':a@a.T, 'left':np.asarray([2,3])@a,
                        'right':a@np.asarray([1,2,3]), 'transposed':cube.swapaxes(0,1)}
            for name, value in expected.items():
                actual = tensor(row, name, value.shape)
                assert np.max(np.abs(actual-value)) < 1e-7, (interface,u,name,row['time'],actual,value)
"#;
