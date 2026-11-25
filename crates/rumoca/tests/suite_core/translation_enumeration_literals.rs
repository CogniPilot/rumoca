//! Declared enumeration literals in translation-time binding folds (MLS 3.7
//! §4.9.5; SPEC_0040 FLAT-C01).
//!
//! An enumeration literal is a constant of its type. A constant binding that
//! compares a function's enumeration result with a declared literal is a
//! translation-time value, whichever spelling of the type path the model and
//! the function use, and it folds to that value.

use rumoca::Compiler;
use rumoca_core::{Expression, Literal, VarName};

const PACKAGE: &str = r#"
package Lib
  type Sign = enumeration(Negative, Zero, Positive);
  function signOf
    input Integer i;
    output Sign s;
  algorithm
    s := if i < 0 then Sign.Negative elseif i == 0 then Sign.Zero else Sign.Positive;
  end signOf;
  function isNegative
    input Integer i;
    output Boolean r;
  algorithm
    r := signOf(i) == Sign.Negative;
  end isNegative;
  model Folds
    constant Boolean direct = signOf(-1) == Sign.Negative;
    constant Boolean qualified = signOf(2) == Lib.Sign.Positive;
    constant Boolean inside = isNegative(3);
    Real y = if direct and qualified and not inside then 1 else 2;
  end Folds;
end Lib;
"#;

/// The literal a folded binding of `variable` holds, if it folded.
fn folded(variable: &str) -> Option<bool> {
    let compiled = Compiler::new()
        .model("Lib.Folds")
        .compile_str(PACKAGE, "Lib.mo")
        .unwrap_or_else(|error| panic!("Lib.Folds compiles: {error:?}"));
    match compiled.flat.variables[&VarName::new(variable)]
        .binding
        .as_ref()?
    {
        Expression::Literal {
            value: Literal::Boolean(value),
            ..
        } => Some(*value),
        _ => None,
    }
}

#[test]
fn an_enumeration_result_compared_with_a_declared_literal_folds() {
    assert_eq!(folded("direct"), Some(true));
    assert_eq!(folded("qualified"), Some(true));
}

#[test]
fn a_function_comparing_enumeration_literals_folds_to_its_value() {
    assert_eq!(folded("inside"), Some(false));
}
