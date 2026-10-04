//! MLS 3.7 §14 operator overloading on operator records, resolved at flatten.
//!
//! Each operator of an operator record is a call of the record's operator
//! function that accepts the operands (MLS §14.5): `a + b` calls `'+'`, `-a`
//! the one-input `'-'`, `2*a` converts the Real through `'constructor'`
//! first, and `sum` of an empty record vector is `'0'`. A record equation
//! whose sides no record owner reads directly is one scalar equation per
//! field, so `a + b = C(0, 0)` balances.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

const MODELS: &str = r#"
package Ops
  operator record C "a complex number with the operators the tests exercise"
    Real re;
    Real im;
    encapsulated operator 'constructor'
      function fromReal
        import Ops.C;
        input Real re;
        input Real im = 0;
        output C result(re = re, im = im);
      algorithm
      end fromReal;
    end 'constructor';
    encapsulated operator function '0'
      import Ops.C;
      output C result(re = 0, im = 0);
    algorithm
    end '0';
    encapsulated operator '-'
      function negate
        import Ops.C;
        input C c;
        output C result;
      algorithm
        result := C(-c.re, -c.im);
      end negate;
      function subtract
        import Ops.C;
        input C a;
        input C b;
        output C result;
      algorithm
        result := C(a.re - b.re, a.im - b.im);
      end subtract;
    end '-';
    encapsulated operator function '+'
      import Ops.C;
      input C a;
      input C b;
      output C result;
    algorithm
      result := C(a.re + b.re, a.im + b.im);
    end '+';
    encapsulated operator '*'
      function multiply
        import Ops.C;
        input C a;
        input C b;
        output C result;
      algorithm
        result := C(a.re*b.re - a.im*b.im, a.re*b.im + a.im*b.re);
      end multiply;
      function scalarProduct
        import Ops.C;
        input C c1[:];
        input C c2[size(c1, 1)];
        output C c3;
      algorithm
        c3 := C(0);
        for i in 1:size(c1, 1) loop
          c3 := c3 + c1[i]*c2[i];
        end for;
      end scalarProduct;
    end '*';
    encapsulated operator function '/'
      import Ops.C;
      input C a;
      input C b;
      output C result;
    algorithm
      result := C((a.re*b.re + a.im*b.im)/(b.re*b.re + b.im*b.im),
        (a.im*b.re - a.re*b.im)/(b.re*b.re + b.im*b.im));
    end '/';
    encapsulated operator function '=='
      import Ops.C;
      input C a;
      input C b;
      output Boolean result;
    algorithm
      result := a.re == b.re and a.im == b.im;
    end '==';
  end C;
  model Arithmetic
    C a = C(1, 2);
    C b = C(time, 1);
    C sum = a + b;
    C difference = a - b;
    C product = a*b;
    C quotient = a/b;
    C negated = -a;
    C scaled = 2*b;
    Real same = if a == a then 1 else 0;
  end Arithmetic;
  model Balance
    C a = C(1, time);
    C b;
  equation
    a + b = C(0, 0);
  end Balance;
  model Vectors
    C v[2] = {C(1, 2), C(time, 1)};
    C w[2] = {C(3, 4), C(5, 6)};
    C s[2];
    C total;
    C empty[0];
    C none = sum(empty);
    C opposite[2] = -w;
  equation
    s = v + w;
    total = sum(v);
  end Vectors;
  constant C unit = C(0, 1);
  model Constant
    C a;
  equation
    a = unit;
  end Constant;
  model Dot
    C v[2] = {C(1, 2), C(time, 1)};
    C w[2] = {C(3, 4), C(5, 6)};
    C dot = v*w;
  end Dot;
  connector Pin
    C v;
    flow C i;
  end Pin;
  connector Plug
    parameter Integer m = 2;
    Pin pin[m];
  end Plug;
  partial model TwoPlug
    parameter Integer m = 2;
    Plug plug_p(m = m);
    Plug plug_n(m = m);
    C v[m];
    C i[m];
  equation
    v = plug_p.pin.v - plug_n.pin.v;
    i = plug_p.pin.i;
    plug_p.pin.i + plug_n.pin.i = fill(C(0), m);
  end TwoPlug;
  model Load
    extends TwoPlug;
    parameter Real R = 2;
  equation
    v = {R*i[k] for k in 1:m};
  end Load;
  model Source
    extends TwoPlug;
    parameter Real V[m] = {1, 2};
    parameter Real phi[m] = {0, 1};
  equation
    v = {V[k]*C(cos(phi[k]), sin(phi[k])) for k in 1:m};
  end Source;
  model Reference
    Plug plug(m = 2);
  equation
    plug.pin.v = fill(C(0), 2);
  end Reference;
  model Polyphase
    Source source;
    Load load;
    Reference reference;
  equation
    connect(source.plug_p, load.plug_p);
    connect(load.plug_n, source.plug_n);
    connect(source.plug_n, reference.plug);
  end Polyphase;
  function real
    input C c;
    output Real r;
  algorithm
    r := c.re;
  end real;
  function scalarPower
    input C v[:];
    output Real p;
  algorithm
    p := sum({real(v[k]*v[k]) for k in 1:size(v, 1)});
  end scalarPower;
  function vectorPower
    input C v[:];
    output Real p;
  algorithm
    p := sum(real({v[k]*v[k] for k in 1:size(v, 1)}));
  end vectorPower;
  model Filled
    parameter C k[2] = fill(C(1, 2), 2);
    Real y = k[2].im*time;
  end Filled;
  model Powers
    C c[2] = {C(1 + time, 1), C(2, time)};
    Real scalar = scalarPower(c);
    Real vector = vectorPower(c);
    Real inline = sum({real(c[k]*c[k]) for k in 1:2});
  end Powers;
end Ops;
"#;

fn simulate(model: &str) -> SimResult {
    simulate_source(MODELS, model)
}

fn simulate_source(source: &str, model: &str) -> SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, "Ops.mo")
        .expect("the model compiles");
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.5),
            ..SimOptions::default()
        },
    )
    .expect("the model simulates")
}

fn final_value(result: &SimResult, name: &str) -> f64 {
    let column = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .expect("the result records the column");
    *result.data[column].last().expect("a sample")
}

fn assert_complex(result: &SimResult, name: &str, re: f64, im: f64) {
    let actual = (
        final_value(result, &format!("{name}.re")),
        final_value(result, &format!("{name}.im")),
    );
    assert!(
        (actual.0 - re).abs() < 1e-9 && (actual.1 - im).abs() < 1e-9,
        "{name} = {actual:?}, expected ({re}, {im})"
    );
}

#[test]
fn each_operator_calls_its_operator_function() {
    // At t = 1: a = 1 + 2j, b = 1 + 1j.
    let result = simulate("Ops.Arithmetic");
    assert_complex(&result, "sum", 2.0, 3.0);
    assert_complex(&result, "difference", 0.0, 1.0);
    assert_complex(&result, "product", -1.0, 3.0);
    assert_complex(&result, "quotient", 1.5, 0.5);
    assert_complex(&result, "negated", -1.0, -2.0);
    assert_complex(&result, "scaled", 2.0, 2.0);
    assert!((final_value(&result, "same") - 1.0).abs() < 1e-12);
}

#[test]
fn a_record_equation_over_an_operator_is_one_equation_per_field() {
    let result = simulate("Ops.Balance");
    assert_complex(&result, "b", -1.0, -1.0);
}

#[test]
fn vector_operators_apply_elementwise_and_an_empty_sum_is_zero() {
    let result = simulate("Ops.Vectors");
    assert_complex(&result, "s[1]", 4.0, 6.0);
    assert_complex(&result, "s[2]", 6.0, 7.0);
    assert_complex(&result, "total", 2.0, 3.0);
    assert_complex(&result, "none", 0.0, 0.0);
    assert_complex(&result, "opposite[2]", -5.0, -6.0);
}

#[test]
fn a_record_equation_reads_a_package_constant_record() {
    // `unit` is a package constant record Flat has not injected yet, so its
    // declared record class comes from the declaration itself.
    let result = simulate("Ops.Constant");
    assert_complex(&result, "a", 0.0, 1.0);
}

#[test]
fn a_vector_operand_function_sizes_one_input_by_another() {
    // `'*'.scalarProduct` declares `input C c2[size(c1, 1)]`; record-parameter
    // lowering keeps that extent readable. At t = 1:
    // (1 + 2j)(3 + 4j) + (1 + 1j)(5 + 6j) = (-5 + 10j) + (-1 + 11j).
    let result = simulate("Ops.Dot");
    assert_complex(&result, "dot", -6.0, 21.0);
}

#[test]
fn record_vectors_read_through_component_arrays_and_fill_one_value() {
    // `plug_p.pin.v` of `Pin pin[m]` is the vector of the elements' `v`, and
    // `fill(C(0), m)` pairs `C(0)` with every element. The source drives
    // `V[k]*e^(j*phi[k])` across a load of resistance 2, so
    // `load.i[2] = 2*e^(j)/2`.
    let result = simulate("Ops.Polyphase");
    assert_complex(&result, "load.i[1]", 0.5, 0.0);
    assert_complex(&result, "load.i[2]", 1.0_f64.cos(), 1.0_f64.sin());
    assert_complex(&result, "source.i[2]", -1.0_f64.cos(), -1.0_f64.sin());
}

#[test]
fn comprehensions_over_record_vectors_call_operators_per_element() {
    // At t = 1: c = {2 + j, 2 + j}, so each square has real part 3.
    let result = simulate("Ops.Powers");
    for name in ["scalar", "vector", "inline"] {
        let value = final_value(&result, name);
        assert!((value - 6.0).abs() < 1e-9, "{name} = {value}");
    }
}

/// A chain of operator records whose `'+'` applies the next record's `'+'`,
/// `depth` records deep: each link is collected only after the previous
/// link's operator is resolved.
fn operator_chain(depth: usize) -> String {
    let mut source = String::from("package Chain\n");
    for k in 1..=depth {
        let body = if k < depth {
            let next = k + 1;
            format!(
                "      import Chain.R{next};\n      input R{k} a;\n      input R{k} b;\n      output R{k} c;\n    protected\n      R{next} t;\n    algorithm\n      t := R{next}(a.x) + R{next}(b.x);\n      c := R{k}(t.x);\n"
            )
        } else {
            format!(
                "      input R{k} a;\n      input R{k} b;\n      output R{k} c;\n    algorithm\n      c := R{k}(a.x + b.x);\n"
            )
        };
        source.push_str(&format!(
            "  operator record R{k}\n    Real x;\n    encapsulated operator function '+'\n      import Chain.R{k};\n{body}    end '+';\n  end R{k};\n"
        ));
    }
    source.push_str(
        "  model M\n    R1 a = R1(time);\n    R1 b = R1(2);\n    R1 s = a + b;\n  end M;\nend Chain;\n",
    );
    source
}

#[test]
fn operator_functions_are_collected_through_any_chain_depth() {
    let result = simulate_source(&operator_chain(12), "Chain.M");
    let value = final_value(&result, "s.x");
    assert!((value - 3.0).abs() < 1e-9, "s.x = {value}");
}

#[test]
fn an_element_of_a_filled_record_array_is_the_filled_record() {
    // `k[2]` of `fill(C(1, 2), 2)` is `C(1, 2)` (MLS §10.3.3).
    let result = simulate("Ops.Filled");
    let value = final_value(&result, "y");
    assert!((value - 2.0).abs() < 1e-12, "y = {value}");
}

/// `MODELS` without `'*'.scalarProduct`, so no operator function accepts two
/// vectors and the operators on record vectors are built by MLS 3.7 §14.4.
fn without_scalar_product() -> String {
    let start = MODELS
        .find("      function scalarProduct")
        .expect("the fixture declares scalarProduct");
    let end = MODELS[start..]
        .find("      end scalarProduct;\n")
        .map(|offset| start + offset + "      end scalarProduct;\n".len())
        .expect("scalarProduct ends");
    format!("{}{}", &MODELS[..start], &MODELS[end..])
}

/// With no operator function for two vectors, `v * w` is the scalar product
/// (MLS 3.7 §14.4 with §10.6.4): the chained `'+'` of the element products,
/// and `'0'` for empty vectors, never an element-wise product.
#[test]
fn a_vector_product_without_an_operator_function_is_the_scalar_product() {
    let source = without_scalar_product().replace(
        "    C dot = v*w;\n  end Dot;",
        "    C dot = v*w;\n    C e1[0];\n    C e2[0];\n    C dotEmpty = e1*e2;\n  end Dot;",
    );
    let result = simulate_source(&source, "Ops.Dot");
    assert_complex(&result, "dot", -6.0, 21.0);
    assert_complex(&result, "dotEmpty", 0.0, 0.0);
}

/// No operator between two record vectors other than `+`, `-`, the
/// element-wise operators, and the scalar product is element-wise.
#[test]
fn a_vector_quotient_without_an_operator_function_is_refused() {
    let source = without_scalar_product().replace("    C dot = v*w;\n", "    C dot[2] = v/w;\n");
    assert!(
        Compiler::new()
            .model("Ops.Dot")
            .compile_str(&source, "Ops.mo")
            .is_err(),
        "v / w of two record vectors has no meaning"
    );
}

/// A record function output declared with a nested modification takes only
/// the binding as its constructor argument (MLS 3.7 §7.2): `re(min = -10) =
/// a` binds `re` to `a`, and an attribute-only `im(start = 0)` binds nothing,
/// so `im` keeps the record's own binding.
#[test]
fn a_nested_field_modification_of_a_record_output_binds_only_its_value() {
    let source = MODELS.replace(
        "  model Filled\n",
        "  record P\n    Real re;\n    Real im = 7;\n  end P;\n  function make\n    input Real a;\n    output P result(re(min = -10) = a, im(start = 0));\n  algorithm\n  end make;\n  model Nested\n    P p = make(2*time);\n  end Nested;\n  model Filled\n",
    );
    let result = simulate_source(&source, "Ops.Nested");
    assert!((final_value(&result, "p.re") - 2.0).abs() < 1e-9);
    assert!((final_value(&result, "p.im") - 7.0).abs() < 1e-9);
}
