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
