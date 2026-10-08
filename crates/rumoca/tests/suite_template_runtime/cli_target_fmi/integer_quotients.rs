//! MLS §3.7.2: div truncates toward zero, rem follows its dividend, and mod
//! follows its divisor, with checked integer domains and lazy branches.

use super::*;

#[test]
fn packaged_fmi_integer_quotients_preserve_signs_domains_and_lazy_branches() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().unwrap();
    let compiled = rumoca::Compiler::new()
        .model("IntegerQuotients")
        .compile_str(SOURCE, "IntegerQuotients.mo")
        .expect("compile integer div, rem and mod");
    let driver = work.path().join("integer_quotients.py");
    fs::write(&driver, DRIVER).unwrap();
    for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
        let fmu = build_named_fmu(work.path(), &compiled, target, "IntegerQuotients");
        validate_source_package(&fmu, standard);
        checked_output(
            Command::new("python3").arg(&driver).arg(&fmu.archive),
            &format!("{target} checked integer quotient, modulo and remainder execution"),
        );
    }
}

const SOURCE: &str = r#"
function quotients
  input Integer a;
  input Integer b;
  input Boolean includeQuotient;
  input Boolean widenDividend;
  output Real quotient;
  output Real modulus;
  output Real remainder;
protected
  Integer dividend;
algorithm
  dividend := if widenDividend then a*4294967296 else a;
  quotient := 0;
  if includeQuotient then
    quotient := div(dividend,b);
  end if;
  modulus := mod(dividend,b);
  remainder := rem(dividend,b);
end quotients;
model IntegerQuotients
  parameter Integer a=7;
  parameter Integer b=3;
  parameter Boolean includeQuotient=true;
  parameter Boolean widenDividend=false;
  Real x(start=0,fixed=true);
  output Real quotient;
  output Real modulus;
  output Real remainder;
equation
  der(x)=1;
  (quotient,modulus,remainder)=quotients(a,b,includeQuotient,widenDividend);
end IntegerQuotients;
"#;

const DRIVER: &str = r#"
import sys
from fmpy import simulate_fmu
from fmpy.fmi1 import FMICallException

lo, hi = -(2**31), 2**31-1
cases = [(a,b,True) for a,b in [
    (7,3),(-7,3),(7,-3),(-7,-3),(0,3),(0,-3),(6,-3),(-6,3),
    (lo,1),(hi,1),(lo,hi),(hi,lo),(lo,lo),(hi,-1),(1,lo),(-1,hi)]]
cases = [(a,b,include,False) for a,b,include in cases]
cases += [(lo,-1,False,True),(lo,1,True,True),(hi,1,True,True),
          (lo,-1,True,False),(7,3,False,False)]
for interface in ['ModelExchange','CoSimulation']:
    for a,b,include,wide in cases:
        dividend = a*2**32 if wide else a
        quotient = abs(dividend)//abs(b) * (-1 if (dividend<0) != (b<0) else 1)
        expected = {'quotient':quotient if include else 0,
                    'modulus':dividend%b, 'remainder':dividend-quotient*b}
        trace = simulate_fmu(sys.argv[1], fmi_type=interface, stop_time=0.01,
            output_interval=0.005, start_values={'a':a,'b':b,'includeQuotient':include,'widenDividend':wide},
            output=list(expected))
        for name,value in expected.items():
            assert (trace[name] == value).all(), (interface,a,b,include,name,trace[name],value)
    for a,b,include,wide in [(lo,-1,True,True),(7,0,True,False),(7,0,False,False)]:
        try:
            simulate_fmu(sys.argv[1], fmi_type=interface, stop_time=0.01,
                start_values={'a':a,'b':b,'includeQuotient':include,'widenDividend':wide})
        except FMICallException as error:
            assert error.status == 3, (interface,a,b,include,error)
        else:
            raise AssertionError((interface,a,b,include,'integer domain failure accepted'))
"#;
