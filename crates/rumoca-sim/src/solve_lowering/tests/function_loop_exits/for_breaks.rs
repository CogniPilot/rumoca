//! `break` in `for` loops: the innermost loop ends at the pass that breaks,
//! and the values it carries out are the ones that pass left at the `break`.

use super::{Fixture, Point, SAMPLES};
use rumoca_ir_dae as dae;

const FIRST_ABOVE: &str = r#"
function firstAbove
  input Real x[:];
  input Real limit;
  output Integer k;
algorithm
  k := 0;
  for i in 1:size(x, 1) loop
    if x[i] > limit then
      k := i;
      break;
    end if;
  end for;
end firstAbove;

function lastAbove
  input Real x[:];
  input Real limit;
  output Integer k;
algorithm
  k := 0;
  for i in size(x, 1):-1:1 loop
    if x[i] > limit then
      k := i;
      break;
    end if;
  end for;
end lastAbove;
"#;

fn first_above(point: Point) -> f64 {
    let mut k = 0;
    for (i, value) in point.x.iter().enumerate() {
        if *value > point.limit {
            k = i + 1;
            break;
        }
    }
    k as f64
}

fn last_above(point: Point) -> f64 {
    let mut k = 0;
    for (i, value) in point.x.iter().enumerate().rev() {
        if *value > point.limit {
            k = i + 1;
            break;
        }
    }
    k as f64
}

const SAMPLE: [f64; 5] = [1.0, 4.0, 2.0, 5.0, 3.0];

/// Limits that break on the first pass, a middle pass, the last pass, and
/// never, in both directions.
fn search_points() -> Vec<Point> {
    [0.5, 1.5, 3.5, 4.5, 5.0, 9.0]
        .into_iter()
        .map(|limit| Point::new(SAMPLE, limit))
        .chain([
            Point::new([0.0, 0.0, 0.0, 0.0, 1.0], 0.5),
            Point::new([1.0, 0.0, 0.0, 0.0, 0.0], 0.5),
        ])
        .collect()
}

#[test]
fn a_for_search_breaks_on_the_first_a_middle_or_the_last_pass_or_never() {
    let first = format!("firstAbove({SAMPLES}, limit)");
    let last = format!("lastAbove({SAMPLES}, limit)");
    let fixture = Fixture::compile(FIRST_ABOVE, "", "", &[&first, &last]);
    let points = search_points();
    // Every pass position is exercised, including no break at all.
    let breaks = points
        .iter()
        .map(|point| first_above(*point))
        .collect::<Vec<_>>();
    for pass in [0.0, 1.0, 2.0, 4.0, 5.0] {
        assert!(breaks.contains(&pass), "no case breaks at pass {pass}");
    }
    fixture.check(&points, |point| vec![first_above(point), last_above(point)]);
}

const PREFIX: &str = r#"
function prefixBelow
  input Real x[:];
  input Real limit;
  output Real total;
  output Real completed;
  output Real last;
algorithm
  total := 0;
  completed := 0;
  last := -1;
  for i in 1:size(x, 1) loop
    last := x[i];
    total := total + x[i];
    if total > limit then
      break;
    end if;
    completed := completed + 1;
  end for;
end prefixBelow;
"#;

fn prefix_below(point: Point) -> Vec<f64> {
    let (mut total, mut completed, mut last) = (0.0, 0.0, -1.0);
    for value in point.x {
        last = value;
        total += value;
        if total > point.limit {
            break;
        }
        completed += 1.0;
    }
    vec![total, completed, last]
}

#[test]
fn the_values_a_broken_for_carries_out_are_the_values_at_its_break() {
    let fixture = Fixture::compile(
        PREFIX,
        "  Real total;\n  Real completed;\n  Real last;\n",
        &format!("  (total, completed, last) = prefixBelow({SAMPLES}, limit);\n"),
        &["total", "completed", "last"],
    );
    let points = [0.5, 1.0, 4.5, 7.5, 13.0, 15.0, 100.0]
        .map(|limit| Point::new([1.0, 4.0, 2.0, 5.0, 3.0], limit));
    fixture.check(&points, prefix_below);
}

const NESTED: &str = r#"
function innerPrefixes
  input Real x[:];
  input Real limit;
  output Real counted;
  output Real rows;
algorithm
  counted := 0;
  rows := 0;
  for r in 1:3 loop
    for j in 1:size(x, 1) loop
      if r * x[j] > limit then
        break;
      end if;
      counted := counted + 1;
    end for;
    rows := rows + 1;
  end for;
end innerPrefixes;

function outerAfterInner
  input Real x[:];
  input Real limit;
  output Real total;
  output Real passes;
algorithm
  total := 0;
  passes := 0;
  for r in 1:size(x, 1) loop
    for j in 1:r loop
      total := total + x[j];
    end for;
    passes := passes + 1;
    if total > limit then
      break;
    end if;
  end for;
end outerAfterInner;

function bothLevels
  input Real x[:];
  input Real limit;
  input Real stop;
  output Real counted;
  output Real rows;
algorithm
  counted := 0;
  rows := 0;
  for r in 1:4 loop
    for j in 1:size(x, 1) loop
      if r * x[j] > limit then
        break;
      end if;
      counted := counted + 1;
    end for;
    rows := rows + 1;
    if counted >= stop then
      break;
    end if;
  end for;
end bothLevels;
"#;

fn inner_prefixes(point: Point) -> Vec<f64> {
    let (mut counted, mut rows) = (0.0, 0.0);
    for r in 1..=3 {
        for value in point.x {
            if f64::from(r) * value > point.limit {
                break;
            }
            counted += 1.0;
        }
        rows += 1.0;
    }
    vec![counted, rows]
}

fn outer_after_inner(point: Point) -> Vec<f64> {
    let (mut total, mut passes) = (0.0, 0.0);
    for r in 1..=point.x.len() {
        for value in &point.x[..r] {
            total += value;
        }
        passes += 1.0;
        if total > point.limit {
            break;
        }
    }
    vec![total, passes]
}

fn both_levels(point: Point) -> Vec<f64> {
    let (mut counted, mut rows) = (0.0, 0.0);
    for r in 1..=4 {
        for value in point.x {
            if f64::from(r) * value > point.limit {
                break;
            }
            counted += 1.0;
        }
        rows += 1.0;
        if counted >= point.stop {
            break;
        }
    }
    vec![counted, rows]
}

fn nested_fixture() -> Fixture {
    Fixture::compile(
        NESTED,
        "  Real counted;\n  Real rows;\n  Real total;\n  Real passes;\n  \
         Real bothCounted;\n  Real bothRows;\n",
        &format!(
            "  (counted, rows) = innerPrefixes({SAMPLES}, limit);\n  \
             (total, passes) = outerAfterInner({SAMPLES}, limit);\n  \
             (bothCounted, bothRows) = bothLevels({SAMPLES}, limit, stop);\n"
        ),
        &[
            "counted",
            "rows",
            "total",
            "passes",
            "bothCounted",
            "bothRows",
        ],
    )
}

#[test]
fn an_inner_break_leaves_only_its_own_loop_which_runs_again_next_pass() {
    let fixture = nested_fixture();
    let points = [0.5, 2.5, 4.5, 8.0, 12.0, 20.0, 100.0]
        .map(|limit| Point::new([1.0, 2.0, 0.5, 4.0, 1.5], limit));
    for point in points {
        let values = fixture.evaluate(point);
        assert_eq!(values[..2].to_vec(), inner_prefixes(point), "{point:?}");
    }
}

#[test]
fn an_outer_break_after_an_inner_loop_ends_the_outer_loop() {
    let fixture = nested_fixture();
    let points =
        [0.5, 1.0, 3.5, 8.0, 15.0, 100.0].map(|limit| Point::new([1.0, 2.0, 0.5, 4.0, 1.5], limit));
    for point in points {
        let values = fixture.evaluate(point);
        assert_eq!(values[2..4].to_vec(), outer_after_inner(point), "{point:?}");
    }
}

#[test]
fn inner_and_outer_breaks_each_leave_their_own_loop() {
    let fixture = nested_fixture();
    let sample = [1.0, 2.0, 0.5, 4.0, 1.5];
    let points = [
        (2.5, 1.0),
        (2.5, 3.0),
        (4.5, 4.0),
        (8.0, 9.0),
        (100.0, 12.0),
    ]
    .map(|(limit, stop)| Point::new(sample, limit).stopping_at(stop));
    for point in points {
        let values = fixture.evaluate(point);
        assert_eq!(values[4..].to_vec(), both_levels(point), "{point:?}");
    }
}

const RETURN_OR_BREAK: &str = r#"
function returnOrBreak
  input Real x[:];
  input Real limit;
  input Real stop;
  output Real total;
algorithm
  total := 0;
  for i in 1:size(x, 1) loop
    if x[i] > stop then
      total := -total;
      return;
    end if;
    if x[i] > limit then
      break;
    end if;
    total := total + x[i];
  end for;
  total := total + 1000;
end returnOrBreak;

function nestedReturn
  input Real x[:];
  input Real limit;
  output Real total;
algorithm
  total := 0;
  for r in 1:3 loop
    for j in 1:size(x, 1) loop
      total := total + r * x[j];
      if total > limit then
        return;
      end if;
    end for;
    total := total + 100;
  end for;
  total := total + 1000;
end nestedReturn;
"#;

fn return_or_break(point: Point) -> f64 {
    let mut total = 0.0;
    for value in point.x {
        if value > point.stop {
            return -total;
        }
        if value > point.limit {
            break;
        }
        total += value;
    }
    total + 1000.0
}

fn nested_return(point: Point) -> f64 {
    let mut total = 0.0;
    for r in 1..=3 {
        for value in point.x {
            total += f64::from(r) * value;
            if total > point.limit {
                return total;
            }
        }
        total += 100.0;
    }
    total + 1000.0
}

fn return_fixture() -> Fixture {
    Fixture::compile(
        RETURN_OR_BREAK,
        "",
        "",
        &[
            &format!("returnOrBreak({SAMPLES}, limit, stop)"),
            &format!("nestedReturn({SAMPLES}, limit)"),
        ],
    )
}

#[test]
fn a_return_and_a_break_in_one_loop_keep_their_own_exits() {
    let fixture = return_fixture();
    let sample = [1.0, 2.0, 3.0, 4.0, 5.0];
    let points = [
        // The return comes first, on the first or a later pass.
        (10.0, 0.5),
        (10.0, 2.5),
        // The break comes first; the statements after the loop run.
        (0.5, 10.0),
        (2.5, 4.5),
        (3.5, 3.5),
        // Neither: the loop completes.
        (10.0, 10.0),
    ]
    .map(|(limit, stop)| Point::new(sample, limit).stopping_at(stop));
    for point in points {
        assert_eq!(
            fixture.evaluate(point)[0],
            return_or_break(point),
            "{point:?}"
        );
    }
}

/// A `return` in an inner loop ends both loops and the function.
#[test]
fn a_return_in_an_inner_loop_ends_the_function() {
    let fixture = return_fixture();
    let points = [0.5, 2.5, 14.0, 120.0, 150.0, 400.0, 1.0e6]
        .map(|limit| Point::new([1.0, 2.0, 3.0, 4.0, 5.0], limit));
    for point in points {
        assert_eq!(
            fixture.evaluate(point)[1],
            nested_return(point),
            "{point:?}"
        );
    }
}

/// The loop stays one compact fold whose continuation ends it at the pass
/// after the `break`, so every executor stops there.
#[test]
fn a_for_loop_that_breaks_is_one_fold_with_a_continuation() {
    let fixture = Fixture::compile(
        FIRST_ABOVE,
        "",
        "",
        &[&format!("firstAbove({SAMPLES}, limit)")],
    );
    fixture.compiled.dae.inspect(|view| {
        let function = (0..view.function_count())
            .filter_map(|index| view.function_id(index).and_then(|id| view.function(id)))
            .find(|function| function.name().as_str().ends_with("firstAbove"))
            .expect("the search function remains visible");
        let folds = function
            .statements()
            .filter_map(|statement| match statement {
                dae::FunctionStatementView::For { fold, .. } => Some(fold),
                _ => None,
            })
            .filter_map(|fold| view.function_fold(fold))
            .collect::<Vec<_>>();
        assert_eq!(folds.len(), 1, "the search loop is one compact fold");
        assert!(
            folds[0].continuation().is_some(),
            "the fold carries its break as a continuation"
        );
    });
}
