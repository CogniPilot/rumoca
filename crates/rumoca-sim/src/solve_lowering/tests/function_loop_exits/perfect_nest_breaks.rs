//! A loop whose whole body is one nested loop that breaks: the inner `break`
//! ends the inner loop only (MLS §11.2.4), and the outer loop runs its next
//! pass.
//!
//! The outer body then holds nothing but the inner loop and the reset of its
//! break state, so every statement the outer loop's exit derivation looks at
//! waits on that state. It is the inner loop's selection, not the outer
//! loop's: the body resets it on every pass.

use super::{Fixture, Point, SAMPLES};

const PERFECT_NESTS: &str = r#"
function countedAfter
  input Real x[:];
  input Real limit;
  output Real counted;
algorithm
  counted := 0;
  for r in 1:3 loop
    for j in 1:size(x, 1) loop
      if r * x[j] > limit then
        break;
      end if;
      counted := counted + 1;
    end for;
  end for;
end countedAfter;

function countedBefore
  input Real x[:];
  input Real limit;
  output Real passes;
algorithm
  passes := 0;
  for r in 1:3 loop
    for j in 1:size(x, 1) loop
      passes := passes + 1;
      if r * x[j] > limit then
        break;
      end if;
    end for;
  end for;
end countedBefore;
"#;

/// Completed inner passes, and started inner passes, over the three rows.
fn nest(point: Point) -> Vec<f64> {
    let (mut counted, mut passes) = (0.0, 0.0);
    for r in 1..=3 {
        for value in point.x {
            passes += 1.0;
            if f64::from(r) * value > point.limit {
                break;
            }
            counted += 1.0;
        }
    }
    vec![counted, passes]
}

/// The same counts if the first inner break ended the outer loop as well.
fn outer_loop_ended_too(point: Point) -> Vec<f64> {
    let (mut counted, mut passes) = (0.0, 0.0);
    'rows: for r in 1..=3 {
        for value in point.x {
            passes += 1.0;
            if f64::from(r) * value > point.limit {
                break 'rows;
            }
            counted += 1.0;
        }
    }
    vec![counted, passes]
}

#[test]
fn an_inner_break_in_a_perfect_nest_leaves_the_outer_loop_running() {
    let fixture = Fixture::compile(
        PERFECT_NESTS,
        "",
        "",
        &[
            &format!("countedAfter({SAMPLES}, limit)"),
            &format!("countedBefore({SAMPLES}, limit)"),
        ],
    );
    // No row breaks, every row breaks on its first pass, and rows that break
    // at different passes.
    let points = [0.2, 0.9, 1.5, 2.5, 4.5, 8.0, 100.0]
        .map(|limit| Point::new([1.0, 2.0, 0.5, 4.0, 1.5], limit));
    let later_rows_run = points
        .iter()
        .filter(|point| nest(**point) != outer_loop_ended_too(**point))
        .count();
    assert!(
        later_rows_run >= 4,
        "the points must tell the nest from an outer loop that ends too, {later_rows_run} do"
    );
    fixture.check(&points, nest);
}

/// The inner loop's statements wait on `j <= n` before the division. The
/// outer loop's exit leaves that test out (it reads the inner iterator) and
/// the inner break state too, so the division must not become an exit test
/// the outer loop evaluates on its own.
const GATED_INNER_BREAK: &str = r#"
function gatedInnerBreak
  input Real gate;
  input Real divisor;
  output Real y;
protected
  Integer n;
  Integer d;
algorithm
  n := integer(gate);
  d := integer(divisor);
  y := 0;
  for i in 1:2 loop
    for j in 1:2 loop
      if j <= n then
        if div(1, d) > 0 then
          y := y + 1;
          break;
        end if;
      end if;
    end for;
  end for;
end gatedInnerBreak;
"#;

#[test]
fn a_test_guarded_by_what_the_outer_exit_leaves_out_stays_guarded() {
    let fixture = Fixture::compile(
        GATED_INNER_BREAK,
        "",
        "",
        &["gatedInnerBreak(limit, stop)"],
    );
    let at = |gate: f64, divisor: f64| Point::new([0.0; 5], gate).stopping_at(divisor);
    // The gate is closed: the division by zero is never reached.
    assert_eq!(fixture.failure(at(0.0, 0.0)), None);
    assert_eq!(fixture.evaluate(at(0.0, 0.0)), [0.0]);
    // The gate is open and the division holds: each row breaks at once.
    assert_eq!(fixture.evaluate(at(2.0, 1.0)), [2.0]);
    // The gate is open: the division by zero is reached and reported.
    let failure = fixture.failure(at(2.0, 0.0));
    assert!(
        failure
            .as_deref()
            .is_some_and(|failure| failure.contains("Integer binary operation")),
        "an entered division by zero must fail as a division: {failure:?}"
    );
}

/// The same nest without the `break`: the outer loop's exit leaves out only
/// the test on the inner iterator, which still guards the division.
const GATED_INNER_SUM: &str = r#"
function gatedInnerSum
  input Real gate;
  input Real divisor;
  output Real y;
protected
  Integer n;
  Integer d;
algorithm
  n := integer(gate);
  d := integer(divisor);
  y := 0;
  for i in 1:2 loop
    for j in 1:2 loop
      if j <= n then
        if div(1, d) > 0 then
          y := y + 1;
        end if;
      end if;
    end for;
  end for;
end gatedInnerSum;
"#;

#[test]
fn a_test_guarded_by_the_inner_iterator_stays_guarded_without_a_break() {
    let fixture = Fixture::compile(GATED_INNER_SUM, "", "", &["gatedInnerSum(limit, stop)"]);
    let at = |gate: f64, divisor: f64| Point::new([0.0; 5], gate).stopping_at(divisor);
    assert_eq!(fixture.failure(at(0.0, 0.0)), None);
    assert_eq!(fixture.evaluate(at(0.0, 0.0)), [0.0]);
    assert_eq!(fixture.evaluate(at(1.0, 1.0)), [2.0]);
    assert_eq!(fixture.evaluate(at(2.0, 1.0)), [4.0]);
}
