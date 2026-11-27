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

const IMPORTED: &str = r#"
package P
  package Types
    type MS = enumeration(a, b, c);
  end Types;
  partial model Base
    parameter Boolean e;
    parameter Integer n = if e then 2 else 3;
    Real x[n](each start = 0, each fixed = true);
  equation
    for i in 1:n loop
      der(x[i]) = 1;
    end for;
  end Base;
  model Pipe
    import P.Types.MS;
    parameter Types.MS ms = Types.MS.a;
    extends Base(final e = (ms == MS.c) or (ms == MS.b));
  end Pipe;
  model Top
    Pipe pipe(ms = P.Types.MS.b);
  end Top;
end P;
"#;

/// A literal read through an import in a component's modifier is the declared
/// literal (`MS.c` in `pipe` is `P.Types.MS.c`), even when no parameter holds
/// it: comparing it with `pipe.ms` is `false`, not a type mismatch that leaves
/// the dimension it decides to a wrong fallback.
#[test]
fn an_imported_literal_in_a_modifier_is_its_declared_literal() {
    let compiled = Compiler::new()
        .model("P.Top")
        .compile_str(IMPORTED, "P.mo")
        .unwrap_or_else(|error| panic!("P.Top compiles: {error:?}"));
    let x = &compiled.flat.variables[&VarName::new("pipe.x")];
    assert_eq!(x.dims, vec![2], "ms = b, so e holds and n = 2");
}

const MISMATCHED: &str = r#"
package Q
  package Types
    type MS = enumeration(a, b, c);
    type Other = enumeration(a, b, c);
  end Types;
  partial model Base
    parameter Boolean e;
    parameter Integer n = if e then 2 else 3;
    Real x[n](each start = 0, each fixed = true);
  equation
    for i in 1:n loop
      der(x[i]) = 1;
    end for;
  end Base;
  model Pipe
    parameter Types.MS ms = Types.MS.a;
    extends Base(final e = ms == Types.Other.b);
  end Pipe;
  model Top
    Pipe pipe(ms = Q.Types.MS.b);
  end Top;
end Q;
"#;

/// Comparing values of two different enumeration types is a type error
/// (MLS 3.7 §10.6.6), never `false`: the dimension it decides is refused,
/// not sized by the `else` branch.
#[test]
fn comparing_two_enumeration_types_refuses_the_dimension_it_decides() {
    let result = Compiler::new()
        .model("Q.Top")
        .compile_str(MISMATCHED, "Q.mo");
    let error = match result {
        Ok(compiled) => panic!(
            "refused, not sized: pipe.x has dims {:?}",
            compiled.flat.variables[&VarName::new("pipe.x")].dims
        ),
        Err(error) => format!("{error:?}"),
    };
    assert!(
        error.contains("pipe.x") || error.contains("\"x\""),
        "{error}"
    );
}
