//! An algorithm written naturally with `break`.

use super::{Fixture, Point, SAMPLES};

const INSERTION_SORT: &str = r#"
function insertionSort
  input Real x[:];
  output Real y[size(x, 1)];
protected
  Real v;
  Integer j;
algorithm
  y := x;
  for i in 2:size(x, 1) loop
    v := y[i];
    j := i;
    while j > 1 loop
      if y[j - 1] <= v then
        break;
      end if;
      y[j] := y[j - 1];
      j := j - 1;
    end while;
    y[j] := v;
  end for;
end insertionSort;
"#;

fn insertion_sort(point: Point) -> Vec<f64> {
    let mut y = point.x.to_vec();
    for i in 1..y.len() {
        let v = y[i];
        let mut j = i;
        while j > 0 {
            if y[j - 1] <= v {
                break;
            }
            y[j] = y[j - 1];
            j -= 1;
        }
        y[j] = v;
    }
    y
}

#[test]
fn an_insertion_sort_written_with_break_sorts() {
    let fixture = Fixture::compile(
        INSERTION_SORT,
        "  Real sorted[5];\n",
        &format!("  sorted = insertionSort({SAMPLES});\n"),
        &[
            "sorted[1]",
            "sorted[2]",
            "sorted[3]",
            "sorted[4]",
            "sorted[5]",
        ],
    );
    let points = [
        [1.0, 2.0, 3.0, 4.0, 5.0],
        [5.0, 4.0, 3.0, 2.0, 1.0],
        [3.0, -1.0, 4.0, 1.0, -5.0],
        [2.0, 2.0, 1.0, 2.0, 1.0],
        [0.0, 0.0, 0.0, 0.0, 0.0],
    ]
    .map(|x| Point::new(x, 0.0));
    for point in points {
        let mut expected = point.x.to_vec();
        expected.sort_by(f64::total_cmp);
        assert_eq!(insertion_sort(point), expected);
    }
    fixture.check(&points, insertion_sort);
}
