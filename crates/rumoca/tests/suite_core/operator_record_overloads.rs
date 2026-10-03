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
  equation
    s = v + w;
    total = sum(v);
  end Vectors;
  model Dot
    C v[2] = {C(1, 2), C(time, 1)};
    C w[2] = {C(3, 4), C(5, 6)};
    C dot = v*w;
  end Dot;
end Ops;
"#;

fn simulate(model: &str) -> SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(MODELS, "Ops.mo")
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
}

#[test]
fn a_vector_operand_function_sizes_one_input_by_another() {
    // `'*'.scalarProduct` declares `input C c2[size(c1, 1)]`; record-parameter
    // lowering keeps that extent readable. At t = 1:
    // (1 + 2j)(3 + 4j) + (1 + 1j)(5 + 6j) = (-5 + 10j) + (-1 + 11j).
    let result = simulate("Ops.Dot");
    assert_complex(&result, "dot", -6.0, 21.0);
}
