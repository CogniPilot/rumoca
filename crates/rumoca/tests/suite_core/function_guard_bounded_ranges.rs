//! MLS 3.6 §11.2.2 / §11.2.6: a `for` range whose bounds are runtime Integer
//! values is evaluated once on entry. When the conditions guarding the loop
//! bound those values (`radius <= 4`, directly or through a Boolean local
//! assigned from such a relation), the loop iterates the proven envelope
//! compactly and selects the source range with a membership guard. Expected
//! values are computed by hand.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const MODELS: &str = r#"
function directGuard
  input Integer radius;
  output Real count;
  output Real squares;
algorithm
  count := 0.0;
  squares := 0.0;
  if radius <= 4 then
    for d in -radius:radius loop
      count := count + 1;
      squares := squares + d*d;
    end for;
  end if;
end directGuard;

model DirectGuard
  input Integer radius = 2;
  output Real count;
  output Real squares;
equation
  (count, squares) = directGuard(radius);
end DirectGuard;

model DirectGuardNegative
  input Integer radius = -3;
  output Real count;
  output Real squares;
equation
  (count, squares) = directGuard(radius);
end DirectGuardNegative;

model DirectGuardOutside
  input Integer radius = 9;
  output Real count;
  output Real squares;
equation
  (count, squares) = directGuard(radius);
end DirectGuardOutside;

function booleanGuard
  input Real reach;
  input Integer slots;
  output Real cells;
protected
  Boolean linear;
  Integer radius;
  Integer neighborhood;
algorithm
  cells := 0.0;
  radius := integer(ceil(reach));
  linear := radius > 4;
  if not linear then
    neighborhood := (2*radius + 1)*(2*radius + 1);
    linear := neighborhood >= slots;
  end if;
  if linear then
    cells := -1.0;
  else
    for dx in -radius:radius loop
      for dy in -radius:radius loop
        cells := cells + 1;
      end for;
    end for;
  end if;
end booleanGuard;

model BooleanGuard
  input Real reach = 1.5;
  input Integer slots = 100;
  output Real cells;
equation
  cells = booleanGuard(reach, slots);
end BooleanGuard;

model BooleanGuardLinear
  input Real reach = 7.0;
  input Integer slots = 100;
  output Real cells;
equation
  cells = booleanGuard(reach, slots);
end BooleanGuardLinear;

function staleGuard
  input Integer radius;
  output Real count;
protected
  Integer r;
algorithm
  count := 0.0;
  r := radius;
  if r <= 4 then
    r := r + radius;
    for d in -r:r loop
      count := count + 1;
    end for;
  end if;
end staleGuard;

model StaleGuard
  input Integer radius = 2;
  output Real count;
equation
  count = staleGuard(radius);
end StaleGuard;
"#;

fn values(model: &str, names: &[&str]) -> Vec<f64> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(MODELS, "GuardBoundedRanges.mo")
        .unwrap_or_else(|error| panic!("{model} should compile: {error}"));
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .unwrap_or_else(|error| panic!("{model} should evaluate: {error}"));
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    names
        .iter()
        .map(|name| {
            probe
                .report
                .solver_y
                .iter()
                .find(|slot| slot.name == *name)
                .unwrap_or_else(|| panic!("{model} has no {name}"))
                .value
        })
        .collect()
}

#[test]
fn a_range_bounded_by_its_guard_iterates_the_source_range() {
    // -2:2 has 5 elements and squares 4 + 1 + 0 + 1 + 4.
    assert_eq!(
        values("DirectGuard", &["count", "squares"]),
        vec![5.0, 10.0]
    );
    // 3:-3 is empty (MLS §10.4.1).
    assert_eq!(
        values("DirectGuardNegative", &["count", "squares"]),
        vec![0.0, 0.0]
    );
    assert_eq!(
        values("DirectGuardOutside", &["count", "squares"]),
        vec![0.0, 0.0]
    );
}

#[test]
fn a_boolean_local_carries_the_bound_of_the_relation_it_was_assigned() {
    // reach 1.5 gives radius 2 and a 5 x 5 neighborhood of 25 cells.
    assert_eq!(values("BooleanGuard", &["cells"]), vec![25.0]);
    // reach 7 gives radius 7 > 4, so the bounded loop never runs.
    assert_eq!(values("BooleanGuardLinear", &["cells"]), vec![-1.0]);
}

#[test]
fn a_fact_about_a_value_written_after_its_guard_is_not_used() {
    let error = Compiler::new()
        .model("StaleGuard")
        .compile_str(MODELS, "GuardBoundedRanges.mo")
        .map(|_| ())
        .expect_err("`r` changes after `r <= 4`, so nothing bounds `-r:r`");
    assert!(
        error
            .to_string()
            .contains("compact dependent-domain transition"),
        "{error}"
    );
}
