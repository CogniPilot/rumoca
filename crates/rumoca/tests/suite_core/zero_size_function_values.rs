//! Zero-size arrays inside Modelica functions (MLS 3.7 §10.1, §10.3.4,
//! §12.4.4).
//!
//! `Blocks.Continuous.Filter` passes `Real den2[0, 2]` and zero-size complex
//! pole coefficients through its design functions. A value with a zero extent
//! has no element, so it is defined from function entry; element-wise
//! arithmetic over it is again zero-size; and its `sum` or `product` is the
//! reduction identity.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

const MODELS: &str = r#"
package ZeroSize
  function sumBoth
    input Real a[:];
    input Real b[:, 2];
    output Real s;
  algorithm
    s := sum(a) + sum(b) + product(b[:, 1]);
  end sumBoth;
  function unwrittenLocal
    input Integer n;
    output Real y;
  protected
    Real d1[n];
    Real d2[0, 2];
  algorithm
    for i in 1:n loop
      d1[i] := i;
    end for;
    y := sumBoth(d1, d2);
  end unwrittenLocal;
  function forwardLocal
    input Real a[:];
    output Real y;
  protected
    Real d2[0, 2];
  algorithm
    y := sumBoth(a, d2);
  end forwardLocal;
  function scaled
    input Real cr[:];
    input Real c1[:];
    input Real w;
    output Real r[size(cr, 1)];
    output Real a[size(c1, 1)];
  algorithm
    r := w*cr;
    a := w*c1;
  end scaled;
  model UnwrittenLocal
    parameter Integer n = 3;
    parameter Real p = unwrittenLocal(n);
    Real y = p + time;
  end UnwrittenLocal;
  model ForwardedLocal
    Real y = forwardLocal({1, 2, time});
  end ForwardedLocal;
  model EmptyParameterArgument
    parameter Real e[0, 2] = fill(0.0, 0, 2);
    Real y = sumBoth({1, 2, time}, e);
  end EmptyParameterArgument;
  model InitialCall
    parameter Real w = 3;
    parameter Real cr[2] = {1, 2};
    parameter Real c1[0](each fixed = false);
    parameter Real r[2](each fixed = false);
    parameter Real a[0](each fixed = false);
    Real x[2];
  initial equation
    (r, a) = scaled(cr, c1, w);
    x = zeros(2);
  equation
    der(x) = -r .* (x .- 1);
  end InitialCall;
end ZeroSize;
"#;

fn simulate(model: &str) -> SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(MODELS, "ZeroSize.mo")
        .expect("the model compiles");
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("the model simulates")
}

fn column<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .expect("the result records the column");
    &result.data[index]
}

fn assert_endpoints(result: &SimResult, name: &str, first: f64, last: f64) {
    let values = column(result, name);
    let (start, end) = (values[0], values[values.len() - 1]);
    assert!(
        (start - first).abs() < 1e-9,
        "{name}(0) = {start}, expected {first}"
    );
    assert!(
        (end - last).abs() < 1e-4,
        "{name}(1) = {end}, expected {last}"
    );
}

#[test]
fn an_unwritten_zero_size_local_is_defined_from_entry() {
    // sum({1, 2, 3}) + sum(empty) + product(empty) = 6 + 0 + 1.
    let result = simulate("ZeroSize.UnwrittenLocal");
    assert_endpoints(&result, "y", 7.0, 8.0);
}

#[test]
fn a_zero_size_value_crosses_a_time_varying_call() {
    assert_endpoints(&simulate("ZeroSize.ForwardedLocal"), "y", 4.0, 5.0);
    assert_endpoints(&simulate("ZeroSize.EmptyParameterArgument"), "y", 4.0, 5.0);
}

#[test]
fn scalar_times_a_zero_size_input_is_zero_size() {
    let result = simulate("ZeroSize.InitialCall");
    assert_endpoints(&result, "x[1]", 0.0, 1.0 - (-3.0f64).exp());
    assert_endpoints(&result, "x[2]", 0.0, 1.0 - (-6.0f64).exp());
}

/// The `Modelica.Fluid` single-substance port: `Xi_outflow[Medium.nXi]` is a
/// zero-size array of a type that declares scalar `start` and `nominal`
/// attributes, which apply to each of its (no) elements.
const ZERO_SIZE_TYPED_PORT: &str = r#"
package ZeroSizePort
  constant Integer nXi = 0;
  type MassFraction = Real(min = 0, max = 1, nominal = 0.1, start = 0.5);
  connector Port
    Real p;
    flow Real m_flow;
    stream MassFraction Xi_outflow[nXi];
  end Port;
  model Tank
    Port ports[2];
    MassFraction Xi[nXi];
  equation
    for i in 1:2 loop
      ports[i].p = 1;
      ports[i].Xi_outflow = Xi;
    end for;
  end Tank;
end ZeroSizePort;
model ZeroSizeTypedPort
  ZeroSizePort.Tank tank;
  Real x(start = 1, fixed = true);
equation
  der(x) = -x;
end ZeroSizeTypedPort;
"#;

#[test]
fn a_scalar_type_attribute_covers_no_element_of_a_zero_size_array() {
    let compiled = Compiler::new()
        .model("ZeroSizeTypedPort")
        .compile_str(ZERO_SIZE_TYPED_PORT, "ZeroSizeTypedPort.mo")
        .expect("ZeroSizeTypedPort compiles");
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("ZeroSizeTypedPort simulates: {error}"));
    assert_endpoints(&result, "x", 1.0, (-1.0f64).exp());
}
