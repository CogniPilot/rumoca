//! MLS §§10.5, 10.6.1, 11.2.1: slice stores preserve the unmodified aggregate
//! and evaluate an overlapping right-hand side before updating its target.

use super::*;

#[test]
fn packaged_fmi_slice_updates_preserve_rectangular_values_and_overlapping_reads() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().unwrap();
    let compiled = rumoca::Compiler::new()
        .model("SliceUpdates")
        .compile_str(SOURCE, "SliceUpdates.mo")
        .expect("compile retained slice assignments");
    let driver = work.path().join("slice_updates.py");
    fs::write(&driver, DRIVER).unwrap();
    for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
        let fmu = build_named_fmu(work.path(), &compiled, target, "SliceUpdates");
        validate_source_package(&fmu, standard);
        checked_output(
            Command::new("python3").arg(&driver).arg(&fmu.archive),
            &format!("{target} retained tensor slice execution"),
        );
    }
}

const SOURCE: &str = r#"
function replaceMatrix
  input Real base[3,4];
  input Real replacement[2,2];
  output Real original[3,4];
  output Real updated[3,4];
algorithm
  original := base;
  updated := base;
  updated[2:3,2:3] := replacement;
end replaceMatrix;
function overlap
  input Real base[5];
  output Real updated[5];
algorithm
  updated := base;
  updated[2:5] := updated[1:4];
  updated[1:2] := updated[4:5];
end overlap;
function replaceCube
  input Real base[3,3,4];
  output Real updated[3,3,4];
algorithm
  updated := base;
  updated[2:3,2:3,2:3] := {{{11,12},{13,14}},{{21,22},{23,24}}};
end replaceCube;
function projectCube
  input Real base[3,3,4];
  output Real projected[2,2,2];
algorithm
  projected := base[2:3,2:3,2:3];
end projectCube;
function integerUpdate
  input Integer n;
  output Real updated[4];
protected
  Integer values[4];
algorithm
  values := {1,2,3,4};
  values[2:3] := {n,-n};
  updated := values;
end integerUpdate;
function booleanUpdate
  input Boolean flag;
  output Real updated[4];
protected
  Boolean values[4];
algorithm
  values := {false,false,false,false};
  values[2:3] := {flag,not flag};
  for i in 1:4 loop
    updated[i] := if values[i] then 1 else 0;
  end for;
end booleanUpdate;
model SliceUpdates
  parameter Integer n=7;
  parameter Boolean flag=true;
  output Real x(start=1,fixed=true);
  output Real original[3,4];
  output Real matrix[3,4];
  output Real shifted[5];
  output Real cube[3,3,4];
  output Real projected[2,2,2];
  output Real integers[4];
  output Real booleans[4];
equation
  der(x)=1;
  (original,matrix)=replaceMatrix({{x,2,3,4},{5,6,7,8},{9,10,11,12}},{{-x,20},{30,40}});
  shifted=overlap({x,2,3,4,5});
  cube=replaceCube(fill(x,3,3,4));
  projected=projectCube(cube);
  integers=integerUpdate(n);
  booleans=booleanUpdate(flag);
end SliceUpdates;
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
    for n, flag in [(7,True), (-3,False)]:
        trace = simulate_fmu(sys.argv[1], fmi_type=interface, stop_time=0.03,
            output_interval=0.01, output=outputs, start_values={'n':n,'flag':flag})
        for row in trace:
            x = 1.0 + row['time']
            base = np.asarray([[x,2,3,4],[5,6,7,8],[9,10,11,12]])
            matrix = base.copy()
            matrix[1:3,1:3] = [[-x,20],[30,40]]
            cube = np.full((3,3,4), x)
            cube[1:3,1:3,1:3] = [[[11,12],[13,14]],[[21,22],[23,24]]]
            expected = {'original':base, 'matrix':matrix,
                        'shifted':np.asarray([3,4,2,3,4]), 'cube':cube,
                        'projected':cube[1:3,1:3,1:3],
                        'integers':np.asarray([1,n,-n,4]),
                        'booleans':np.asarray([False,flag,not flag,False])}
            for name, value in expected.items():
                actual = tensor(row, name, value.shape)
                assert np.max(np.abs(actual.astype(float)-value.astype(float))) < 1e-7, (interface,name,row['time'],actual,value)
"#;
