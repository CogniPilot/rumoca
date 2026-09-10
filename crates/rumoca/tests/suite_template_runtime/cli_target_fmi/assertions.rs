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

#[test]
fn packaged_fmi_static_assertion_profile_refuses_continuously_changing_predicates() {
    for argument in ["time", "x", "u"] {
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
 assert(valid({argument}), "changing predicate");
end ChangingAssertion;
"#
        );
        let compiled = rumoca::Compiler::new()
            .model("ChangingAssertion")
            .compile_str(&source, "ChangingAssertion.mo")
            .expect("compile changing assertion");
        let work = tempdir().expect("negative admission work directory");
        for target in ["fmi2", "fmi3"] {
            let error = rumoca::compile_packaged_target(
                &compiled,
                "ChangingAssertion",
                target,
                work.path().join(target),
            )
            .expect_err("a changing predicate requires general event support");
            assert!(
                format!("{error:#}").contains("assertion depends on"),
                "{target}/{argument}: {error:#}"
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
