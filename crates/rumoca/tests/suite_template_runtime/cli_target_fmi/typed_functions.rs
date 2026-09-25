//! C failure boundaries and lazy branches through the independent FMI importer.

use super::*;

/// The C profile publishes Integer, enumeration, Boolean, and String
/// variables under their FMI value types, settles a parameter-determined
/// discrete equation with the parameters, and runs an initialization whose
/// residual reads a reconstructed algebraic (SPEC_0044 ME-PARAM-001). A
/// parameter some program reads stays settable and takes effect; one the
/// compiler folded into its uses is published as a constant the component
/// refuses to set. Solved coordinates are `approx`, equation-defined discrete
/// values `calculated`.
#[test]
fn packaged_fmi_typed_variables_discrete_equations_and_settled_initialization() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    let standards = standard_roots();
    let work = tempdir().unwrap();
    let compiled = rumoca::Compiler::new()
        .model("TypedPublic")
        .compile_str(TYPED_SOURCE, "TypedPublic.mo")
        .unwrap();
    let driver = work.path().join("typed_public.py");
    fs::write(&driver, TYPED_DRIVER).unwrap();
    for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
        let fmu = build_named_fmu(work.path(), &compiled, target, "TypedPublic");
        validate_source_package(&fmu, standard);
        checked_output(
            Command::new("python3").arg(&driver).arg(&fmu.archive),
            &format!("{target} typed variables and settled initialization"),
        );
    }
}

const TYPED_SOURCE: &str = r#"
model TypedPublic
 type Speed = enumeration(Slow, Fast);
 parameter Integer n = 2;
 parameter Real gain = 1;
 parameter Boolean doubled = true;
 parameter Speed speed = Speed.Fast;
 parameter String label = "rumoca <typed>";
 Integer k = if doubled then 2*n else n;
 Real x(start = 0.3);
 Real y;
initial equation
 y = 0.5;
equation
 y = x*x*x + x;
 der(x) = -gain*(if speed == Speed.Fast then 2 else 1)*k*x;
end TypedPublic;
"#;

const TYPED_DRIVER: &str = r#"
import sys
from fmpy import extract, instantiate_fmu, read_model_description, simulate_fmu
from fmpy.fmi1 import FMICallException

path = sys.argv[1]
md = read_model_description(path)
fmi2 = md.fmiVersion.startswith('2')
variables = {v.name: v for v in md.modelVariables}
types = {name: v.type for name, v in variables.items()}
integer = 'Integer' if fmi2 else 'Int32'
assert types['n'] == integer and types['speed'] == integer and types['k'] == integer, types
assert types['doubled'] == 'Boolean' and types['label'] == 'String', types
assert variables['label'].start == 'rumoca <typed>', variables['label'].start

# Read parameters stay settable; folded ones are constants.
for name in ['n', 'gain']:
    assert variables[name].causality == 'parameter', (name, variables[name].causality)
for name in ['doubled', 'speed', 'label']:
    v = variables[name]
    assert (v.causality, v.variability, v.initial) == ('local', 'constant', 'exact'), (name, v.causality, v.variability, v.initial)

# The solved state keeps its start as a guess; the discrete equation defines k.
assert variables['x'].initial == 'approx', variables['x'].initial
assert variables['k'].initial == 'calculated' and variables['k'].start is None, variables['k'].initial
initial_unknowns = {u.variable.name for u in md.initialUnknowns}
assert 'x' in initial_unknowns, initial_unknowns

# The component itself refuses to set a folded parameter.
unzipdir = extract(path)
fmu = instantiate_fmu(unzipdir, md, fmi_type='ModelExchange')
vr = {name: v.valueReference for name, v in variables.items()}
set_integer = fmu.setInteger if fmi2 else fmu.setInt32
set_real = fmu.setReal if fmi2 else fmu.setFloat64
set_real([vr['gain']], [1.0])
for name, setter, value in [('doubled', fmu.setBoolean, [False]), ('speed', set_integer, [1]), ('label', fmu.setString, ['other'])]:
    try:
        setter([vr[name]], value)
    except FMICallException:
        pass
    else:
        raise AssertionError((name, 'a folded parameter accepted a set'))
fmu.freeInstance()

lo, hi = 0.0, 1.0
for _ in range(200):
    mid = 0.5 * (lo + hi)
    lo, hi = (mid, hi) if mid*mid*mid + mid < 0.5 else (lo, mid)
x0 = 0.5 * (lo + hi)
for interface in ['ModelExchange', 'CoSimulation']:
    finals = {}
    for n, gain, expected_k in [(2, 1.0, 4), (3, 1.0, 6), (2, 2.0, 4)]:
        result = simulate_fmu(path, fmi_type=interface, stop_time=0.5, output_interval=0.05,
            start_values={'n': n, 'gain': gain}, output=['x', 'y', 'k', 'n', 'doubled'])
        assert (result['k'] == expected_k).all(), (interface, n, result['k'])
        assert (result['n'] == n).all() and result['doubled'].all(), result
        assert abs(result['x'][0] - x0) < 1e-8, (interface, result['x'][0], x0)
        assert abs(result['y'][0] - 0.5) < 1e-8, (interface, result['y'][0])
        finals[(n, gain)] = result['x'][-1]
    # A set gain changes the trajectory: x(t) = x0*exp(-2*gain*k*t).
    assert abs(finals[(2, 2.0)] - finals[(2, 1.0)]**2 / x0) < 1e-4, (interface, finals)
"#;

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
