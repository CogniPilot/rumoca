//! `break` in a `for` loop with several iterators: MLS §11.2.2.3 makes the
//! loop the nest of one loop per iterator, so the `break` (MLS §11.2.4) ends
//! the loop of the last iterator and the next pass of the others runs it again.
//!
//! Every expectation is the nest written in plain Rust; the two-iterator grid
//! is also written in Modelica as the nest it stands for.

use super::{Fixture, Point, SAMPLES};

const GRID: &str = r#"
function gridPrefixes
  input Real x[:];
  input Real limit;
  output Real total;
  output Real passes;
algorithm
  total := 0;
  passes := 0;
  for r in 1:3, j in 1:size(x, 1) loop
    passes := passes + 1;
    if r * x[j] > limit then
      break;
    end if;
    total := total + r * x[j];
  end for;
end gridPrefixes;

function nestPrefixes
  input Real x[:];
  input Real limit;
  output Real total;
  output Real passes;
algorithm
  total := 0;
  passes := 0;
  for r in 1:3 loop
    for j in 1:size(x, 1) loop
      passes := passes + 1;
      if r * x[j] > limit then
        break;
      end if;
      total := total + r * x[j];
    end for;
  end for;
end nestPrefixes;
"#;

const CUBE: &str = r#"
function cubePrefixes
  input Real x[:];
  input Real limit;
  output Real total;
  output Real passes;
algorithm
  total := 0;
  passes := 0;
  for p in 1:2, r in 1:2, j in 1:size(x, 1) loop
    passes := passes + 1;
    if (p + r) * x[j] > limit then
      break;
    end if;
    total := total + (p + r) * x[j];
  end for;
end cubePrefixes;
"#;

const COUNTED: &str = r#"
function countedGrid
  input Real x;
  output Real y;
algorithm
  y := x;
  for i in 1:2, j in 1:3 loop
    y := y + 1;
    if j >= 2 then
      break;
    end if;
  end for;
end countedGrid;
"#;

const LEADING: &str = r#"
function leadingBreakGrid
  input Real x;
  output Real y;
algorithm
  y := 0;
  for i in 1:2, j in 1:4 loop
    if j >= 3 then
      break;
    end if;
    y := y + i * x;
  end for;
end leadingBreakGrid;
"#;

const RETURN_OR_BREAK: &str = r#"
function gridReturnOrBreak
  input Real x[:];
  input Real limit;
  input Real stop;
  output Real total;
algorithm
  total := 0;
  for r in 1:3, j in 1:size(x, 1) loop
    if r * x[j] > stop then
      total := -total;
      return;
    end if;
    if r * x[j] > limit then
      break;
    end if;
    total := total + r * x[j];
  end for;
  total := total + 1000;
end gridReturnOrBreak;
"#;

/// The nest: a break ends the inner loop, and the next row runs it again.
fn nest_prefixes(point: Point) -> Vec<f64> {
    let (mut total, mut passes) = (0.0, 0.0);
    for r in 1..=3 {
        for value in point.x {
            passes += 1.0;
            if f64::from(r) * value > point.limit {
                break;
            }
            total += f64::from(r) * value;
        }
    }
    vec![total, passes]
}

/// What one exit over the whole product of the iterators would compute: the
/// first break ends every later pass. A point where this differs from
/// [`nest_prefixes`] is one where a later row must still run.
fn one_exit_for_the_whole_grid(point: Point) -> Vec<f64> {
    let (mut total, mut passes) = (0.0, 0.0);
    'grid: for r in 1..=3 {
        for value in point.x {
            passes += 1.0;
            if f64::from(r) * value > point.limit {
                break 'grid;
            }
            total += f64::from(r) * value;
        }
    }
    vec![total, passes]
}

fn cube_prefixes(point: Point) -> Vec<f64> {
    let (mut total, mut passes) = (0.0, 0.0);
    // The two outer iterators, in the order the nest runs them.
    for (p, r) in [(1, 1), (1, 2), (2, 1), (2, 2)] {
        for value in point.x {
            passes += 1.0;
            if f64::from(p + r) * value > point.limit {
                break;
            }
            total += f64::from(p + r) * value;
        }
    }
    vec![total, passes]
}

fn grid_return_or_break(point: Point) -> f64 {
    let mut total = 0.0;
    for r in 1..=3 {
        for value in point.x {
            if f64::from(r) * value > point.stop {
                return -total;
            }
            if f64::from(r) * value > point.limit {
                break;
            }
            total += f64::from(r) * value;
        }
    }
    total + 1000.0
}

const SAMPLE: [f64; 5] = [1.0, 2.0, 0.5, 4.0, 1.5];

fn grid_fixture() -> Fixture {
    Fixture::compile(
        GRID,
        "  Real gridTotal;\n  Real gridPasses;\n  Real nestTotal;\n  Real nestPasses;\n",
        &format!(
            "  (gridTotal, gridPasses) = gridPrefixes({SAMPLES}, limit);\n  \
             (nestTotal, nestPasses) = nestPrefixes({SAMPLES}, limit);\n"
        ),
        &["gridTotal", "gridPasses", "nestTotal", "nestPasses"],
    )
}

#[test]
fn a_break_under_several_iterators_ends_the_last_one_and_the_next_row_runs() {
    let fixture = grid_fixture();
    // No row breaks, every row breaks on its first pass, and rows that break
    // at different passes.
    let points = [0.2, 0.9, 1.5, 2.5, 4.5, 8.0, 100.0].map(|limit| Point::new(SAMPLE, limit));
    let later_rows_run = points
        .iter()
        .filter(|point| nest_prefixes(**point) != one_exit_for_the_whole_grid(**point))
        .count();
    assert!(
        later_rows_run >= 4,
        "the points must tell the nest from one exit over the whole grid, {later_rows_run} do"
    );
    for point in points {
        let values = fixture.evaluate(point);
        assert_eq!(values[..2].to_vec(), nest_prefixes(point), "{point:?}");
        assert_eq!(values[2..4].to_vec(), nest_prefixes(point), "{point:?}");
    }
}

#[test]
fn a_break_under_three_iterators_ends_only_the_last_one() {
    let fixture = Fixture::compile(
        CUBE,
        "  Real cubeTotal;\n  Real cubePasses;\n",
        &format!("  (cubeTotal, cubePasses) = cubePrefixes({SAMPLES}, limit);\n"),
        &["cubeTotal", "cubePasses"],
    );
    let points = [0.2, 1.5, 2.5, 5.0, 9.0, 100.0].map(|limit| Point::new(SAMPLE, limit));
    fixture.check(&points, cube_prefixes);
}

/// Each of the two rows runs two passes of the inner iterator and breaks.
#[test]
fn a_break_on_the_iterator_itself_counts_two_passes_in_each_row() {
    let fixture = Fixture::compile(COUNTED, "", "", &["countedGrid(limit)"]);
    let points = [-3.5, 0.0, 7.25].map(|x| Point::new(SAMPLE, x));
    fixture.check(&points, |point| vec![point.limit + 4.0]);
}

/// A break that leads the body on the last iterator alone shortens that
/// iterator's range, and each row still runs the two passes before it.
#[test]
fn a_leading_break_on_the_last_iterator_shortens_every_row() {
    let fixture = Fixture::compile(LEADING, "", "", &["leadingBreakGrid(limit)"]);
    let points = [-3.5, 0.0, 7.25].map(|x| Point::new(SAMPLE, x));
    fixture.check(&points, |point| vec![6.0 * point.limit]);
}

#[test]
fn a_return_under_several_iterators_ends_the_function_and_a_break_one_row() {
    let call = format!("gridReturnOrBreak({SAMPLES}, limit, stop)");
    let fixture = Fixture::compile(RETURN_OR_BREAK, "", "", &[&call]);
    let points = [
        // The return comes first, in the first row or a later one.
        (100.0, 0.5),
        (100.0, 7.0),
        // Two rows break and the third returns.
        (3.5, 5.0),
        // Rows break and none returns: the statements after the loop run.
        (0.2, 1.0e9),
        (2.5, 1.0e9),
        // Neither.
        (100.0, 1.0e9),
    ]
    .map(|(limit, stop)| Point::new(SAMPLE, limit).stopping_at(stop));
    fixture.check(&points, |point| vec![grid_return_or_break(point)]);
}
