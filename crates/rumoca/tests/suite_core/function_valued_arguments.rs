//! Function-valued arguments (MLS 3.7 §12.4.2.1), as
//! `Modelica.Media.Incompressible.TableBased.T_ph` passes
//! `function f_nonlinear(p = p, h = h)` to `solveOneNonlinearEquation`.
//! A bare function name (`solve(square, ...)`) and a partial application with no
//! bindings (`function square()`) denote the same function. Flattening specializes the callee once per distinct function argument:
//! calls of the formal function call the argument's function directly, and
//! the formals a partial application binds become extra inputs the call site
//! passes, so no function value reaches the executable IR. A formal function
//! passed on to another function is specialized in turn. A value that is not a
//! function (a time-varying Real) at a function input is refused.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package FunctionArgs
  partial function Equation
    input Real u;
    output Real y;
  end Equation;
  function solve
    input Equation f;
    input Real u_min;
    input Real u_max;
    input Real tolerance = 1e-12;
    output Real u;
  protected
    Real a = u_min;
    Real b = u_max;
    Real fa;
    Real m;
  algorithm
    fa := f(a);
    for i in 1:60 loop
      m := (a + b)/2;
      if fa*f(m) <= 0 then
        b := m;
      else
        a := m;
        fa := f(m);
      end if;
    end for;
    u := (a + b)/2;
  end solve;
  function residual
    extends Equation;
    input Real p;
    input Real h;
  algorithm
    y := p*u*u - h;
  end residual;
  function T_ph
    input Real p;
    input Real h;
    output Real T;
  algorithm
    T := solve(function residual(p = p, h = h), 0, 10);
  end T_ph;
  function square
    extends Equation;
  algorithm
    y := u*u - 4;
  end square;
  function nested
    input Equation g;
    input Real hi;
    output Real u;
  algorithm
    u := solve(g, 0, hi);
  end nested;
  model Top
    Real T = T_ph(2, 8 + time);
    Real r = solve(square, 0, 10) + time;
    Real e = solve(function square(), 0, 10) + time;
    Real n = nested(function residual(p = 1, h = 9 + time), 10);
  end Top;
end FunctionArgs;
"#;

#[test]
fn partial_applications_specialize_their_callees() {
    let compiled = Compiler::new()
        .model("FunctionArgs.Top")
        .compile_str(SOURCE, "FunctionArgs.mo")
        .unwrap_or_else(|error| panic!("FunctionArgs.Top compiles: {error:?}"));
    assert!(
        compiled.flat.functions.values().all(|function| function
            .inputs
            .iter()
            .all(|input| input.type_class != Some(rumoca_core::ClassType::Function))),
        "no function value survives flattening"
    );
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("FunctionArgs.Top simulates: {error}"));
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name).expect(name);
        &result.data[index]
    };
    for (row, &time) in result.times.iter().enumerate() {
        // 2*T^2 = 8 + time; root of u^2 - 4 is 2; n^2 = 9 + time.
        assert!((column("T")[row] - ((8.0 + time) / 2.0).sqrt()).abs() < 1e-9);
        assert!((column("r")[row] - (2.0 + time)).abs() < 1e-9);
        assert!((column("e")[row] - (2.0 + time)).abs() < 1e-9);
        assert!((column("n")[row] - (9.0 + time).sqrt()).abs() < 1e-9);
    }
}

#[test]
fn a_non_function_value_at_a_function_input_is_refused() {
    let source = SOURCE.replace(
        "    Real T = T_ph(2, 8 + time);",
        "    Real T = solve(2.0 + time, 0, 10);",
    );
    let error = Compiler::new()
        .model("FunctionArgs.Top")
        .compile_str(&source, "FunctionArgs.mo")
        .expect_err("a time-varying Real is not a function argument");
    assert!(
        error.to_string().contains("partial application"),
        "unexpected diagnostic: {error}"
    );
}
