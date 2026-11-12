//! Index functions of loop iterators and the fault-visible-state proof.
//!
//! A loop body that raises NAN (a Real comparison) and then calls a function
//! whose Integer arithmetic could fault would lose the raised bit at that
//! fault, so Solve refinement refuses it. A call cannot fault at its site
//! when it does not on any value its arguments take there: the iterators'
//! static ranges, and the literal every call in the block passes for a
//! function input. Where some value does fault, or an argument has no static
//! range, the refusal stays. (A division by a divisor that is not provably
//! nonzero never reaches this proof: the GALEC projection refuses it.)

use rumoca_eval_galec::Value;
use rumoca_phase_solve::lower_algorithm_block;

use super::galec_guarded_element_writes::{reals, step_with};
use super::galec_solve_refinement::{model_name, profile, project};

/// A model whose function `f` loops over `i in 1:3`: a comparison that raises
/// NAN, then a read of `p` at `pick(i, m)`, where the index function computes
/// `pick` from the iterator and the function input `m`. `calls` are the
/// statements that call `f` at each sample; `declarations` are the block
/// input or the further outputs they need.
fn model(name: &str, pick: &str, declarations: &str, calls: &str) -> String {
    format!(
        r#"
model {name}
  function pick
    input Integer i;
    input Integer m;
    output Integer k;
  algorithm
    k := {pick};
  end pick;
  function f
    input Real p[3];
    input Integer m;
    output Real y[3];
    output Boolean ok;
  algorithm
    y := zeros(3);
    ok := true;
    for i in 1:3 loop
      if p[i] > 0.0 then
        ok := false;
      end if;
      y[i] := p[pick(i, m)];
    end for;
  end f;
  input Real p[3];
{declarations}  output Real y[3](each start = 0.0, each fixed = true);
  output Boolean ok(start = false, fixed = true);
algorithm
  when sample(0, 0.01) then
{calls}  end when;
end {name};
"#
    )
}

/// The outputs of a second call of `f`.
const SECOND_RESULT: &str = "  output Real w[3](each start = 0.0, each fixed = true);\n  \
     output Boolean alsoOk(start = false, fixed = true);\n";

fn refused(source: &str) -> String {
    let name = model_name(source);
    let package = project(source).unwrap_or_else(|error| panic!("{name}: {error}"));
    match lower_algorithm_block(&package, profile()) {
        Ok(_) => panic!("{name}: a call that can fault after a raised signal must be refused"),
        Err(error) => error.to_string(),
    }
}

#[test]
fn an_index_function_that_cannot_fault_on_the_iterator_values_is_admitted() {
    // pick(i, 2) = 4 - i reverses the input; 2 is a literal at the only call.
    let source = model(
        "ReversedPick",
        "div(m * (8 - 2 * i), 4)",
        "",
        "    (y, ok) := f(p, 2);\n",
    );
    step_with(&source, &[1.5, -4.0, 5.0], |oracle| {
        assert_eq!(reals(oracle, "y"), [5.0, -4.0, 1.5]);
        assert_eq!(oracle.state("ok"), Ok(&Value::Boolean(false)));
    });
    step_with(&source, &[-1.5, -4.0, -5.0], |oracle| {
        assert_eq!(reals(oracle, "y"), [-5.0, -4.0, -1.5]);
        assert_eq!(oracle.state("ok"), Ok(&Value::Boolean(true)));
    });
}

#[test]
fn an_index_function_of_a_run_time_argument_is_still_refused() {
    // The same function, but `m` is a block input: `m * (8 - 2 * i)` can
    // overflow for some value of it.
    let source = model(
        "RunTimePick",
        "div(m * (8 - 2 * i), 4)",
        "  input Integer m;\n",
        "    (y, ok) := f(p, m);\n",
    );
    let error = refused(&source);
    assert!(error.contains("fault after"), "{error}");
}

#[test]
fn an_index_function_that_overflows_for_one_iterator_value_is_still_refused() {
    // 1073741824 * i overflows a 32-bit Integer at i = 2.
    let source = model(
        "OverflowingPick",
        "div(1073741824 * i, 1073741824) + m - 2",
        "",
        "    (y, ok) := f(p, 2);\n",
    );
    let error = refused(&source);
    assert!(error.contains("fault after"), "{error}");
}

#[test]
fn every_literal_a_function_input_is_called_with_is_covered() {
    // pick(i, m) = i for m = 1 and for m = 3; both are literals at their calls.
    let source = model(
        "TwoLiteralPicks",
        "div(2 * m * i, 2) - (m - 1) * i",
        SECOND_RESULT,
        "    (y, ok) := f(p, 1);\n    (w, alsoOk) := f(p, 3);\n",
    );
    step_with(&source, &[1.5, -4.0, 5.0], |oracle| {
        assert_eq!(reals(oracle, "y"), [1.5, -4.0, 5.0]);
        assert_eq!(reals(oracle, "w"), [1.5, -4.0, 5.0]);
    });
}

#[test]
fn one_literal_that_overflows_among_the_calls_is_still_refused() {
    // 2 * m * i overflows a 32-bit Integer for m = 536870912 at i = 2.
    let source = model(
        "OneOverflowingLiteral",
        "div(2 * m * i, 2) - (m - 1) * i",
        SECOND_RESULT,
        "    (y, ok) := f(p, 1);\n    (w, alsoOk) := f(p, 536870912);\n",
    );
    let error = refused(&source);
    assert!(error.contains("fault after"), "{error}");
}
