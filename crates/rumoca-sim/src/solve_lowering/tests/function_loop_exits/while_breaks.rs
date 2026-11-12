//! `break` and `return` in `while` loops whose pass bound is proven (MLS
//! §11.2.3): the loop ends at the pass that exits or where its condition
//! first fails.

use super::{Fixture, Point, SAMPLES};

const WHILE_BREAKS: &str = r#"
function countedSearch
  input Real x[:];
  input Real limit;
  output Real k;
  output Real visited;
protected
  Integer i;
algorithm
  k := 0;
  i := 1;
  while i <= size(x, 1) loop
    if x[i] > limit then
      k := i;
      break;
    end if;
    i := i + 1;
  end while;
  visited := i;
end countedSearch;

function skipSearch
  input Real x[:];
  input Real limit;
  output Real k;
  output Real steps;
protected
  Integer i;
algorithm
  k := 0;
  steps := 0;
  i := 1;
  while i <= size(x, 1) loop
    steps := steps + 1;
    if x[i] > limit then
      k := i;
      break;
    end if;
    if x[i] < 0 then
      i := i + 2;
    else
      i := i + 1;
    end if;
  end while;
end skipSearch;
"#;

fn counted_search(point: Point) -> Vec<f64> {
    let (mut k, mut i) = (0, 1);
    while i <= point.x.len() {
        if point.x[i - 1] > point.limit {
            k = i;
            break;
        }
        i += 1;
    }
    vec![k as f64, i as f64]
}

fn skip_search(point: Point) -> Vec<f64> {
    let (mut k, mut steps, mut i) = (0, 0, 1);
    while i <= point.x.len() {
        steps += 1;
        if point.x[i - 1] > point.limit {
            k = i;
            break;
        }
        i += if point.x[i - 1] < 0.0 { 2 } else { 1 };
    }
    vec![k as f64, f64::from(steps)]
}

fn while_fixture() -> Fixture {
    Fixture::compile(
        WHILE_BREAKS,
        "  Real k;\n  Real visited;\n  Real skipK;\n  Real steps;\n",
        &format!(
            "  (k, visited) = countedSearch({SAMPLES}, limit);\n  \
             (skipK, steps) = skipSearch({SAMPLES}, limit);\n"
        ),
        &["k", "visited", "skipK", "steps"],
    )
}

fn while_points() -> Vec<Point> {
    let sample = [1.0, -4.0, 2.0, -5.0, 3.0];
    [0.5, 1.5, 2.5, 9.0]
        .into_iter()
        .map(|limit| Point::new(sample, limit))
        .chain([
            Point::new([0.0, 0.0, 0.0, 0.0, 1.0], 0.5),
            Point::new([-1.0, 0.0, -1.0, 0.0, 1.0], 0.5),
            Point::new([-1.0, -1.0, -1.0, -1.0, -1.0], 0.5),
        ])
        .collect()
}

#[test]
fn a_counted_while_ends_at_the_pass_that_breaks() {
    let fixture = while_fixture();
    for point in while_points() {
        assert_eq!(
            fixture.evaluate(point)[..2].to_vec(),
            counted_search(point),
            "{point:?}"
        );
    }
}

#[test]
fn a_while_bounded_by_its_progress_ends_at_the_pass_that_breaks() {
    let fixture = while_fixture();
    for point in while_points() {
        assert_eq!(
            fixture.evaluate(point)[2..].to_vec(),
            skip_search(point),
            "{point:?}"
        );
    }
}

const WHILE_RETURN: &str = r#"
function whileReturn
  input Real x[:];
  input Real limit;
  output Real total;
protected
  Integer i;
algorithm
  total := 0;
  i := 1;
  while i <= size(x, 1) loop
    total := total + x[i];
    if total > limit then
      return;
    end if;
    i := i + 1;
  end while;
  total := total + 1000;
end whileReturn;
"#;

fn while_return(point: Point) -> f64 {
    let mut total = 0.0;
    for value in point.x {
        total += value;
        if total > point.limit {
            return total;
        }
    }
    total + 1000.0
}

/// MLS §11.2.6: a `return` inside a `while` ends the function at its pass.
#[test]
fn a_return_inside_a_while_ends_the_function() {
    let fixture = Fixture::compile(
        WHILE_RETURN,
        "",
        "",
        &[&format!("whileReturn({SAMPLES}, limit)")],
    );
    let points =
        [0.5, 1.5, 4.5, 9.5, 14.5, 100.0].map(|limit| Point::new([1.0, 2.0, 3.0, 4.0, 5.0], limit));
    fixture.check(&points, |point| vec![while_return(point)]);
}

const EXIT_FLAG: &str = r#"
function flaggedSearch
  input Real x[:];
  input Real limit;
  output Real k;
  output Real steps;
protected
  Integer node;
  Boolean done;
algorithm
  k := 0;
  steps := 0;
  node := 1;
  done := false;
  while not done loop
    steps := steps + 1;
    if x[node] > limit then
      k := node;
      done := true;
    elseif node >= size(x, 1) then
      done := true;
    else
      node := node + 1;
    end if;
  end while;
end flaggedSearch;
"#;

fn flagged_search(point: Point) -> Vec<f64> {
    let (mut k, mut steps, mut node) = (0, 0, 1);
    loop {
        steps += 1;
        if point.x[node - 1] > point.limit {
            k = node;
            break;
        } else if node >= point.x.len() {
            break;
        }
        node += 1;
    }
    vec![k as f64, f64::from(steps)]
}

/// A `while not done` loop is bounded by its progress like `while going`.
#[test]
fn a_while_ended_by_a_negated_flag_is_bounded_by_its_progress() {
    let fixture = Fixture::compile(
        EXIT_FLAG,
        "  Real k;\n  Real steps;\n",
        &format!("  (k, steps) = flaggedSearch({SAMPLES}, limit);\n"),
        &["k", "steps"],
    );
    fixture.check(&while_points(), flagged_search);
}
