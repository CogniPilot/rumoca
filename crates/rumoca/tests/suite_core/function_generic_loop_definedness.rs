//! MLS 3.6 §12.4.4 definedness of compact function loops, proven from one
//! generic iteration rather than from each domain point. Each case reads an
//! element that the same iteration, an earlier iteration, or the code before
//! the loop defined; the values are computed by hand. A read of an element
//! only a later iteration defines stays refused.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const MODELS: &str = r#"
function prefix
  input Real u;
  output Real y[5];
algorithm
  y[1] := u;
  for i in 2:5 loop
    y[i] := y[i - 1] + 1.0;
  end for;
end prefix;

model Prefix
  input Real u = 10.0;
  output Real y[5];
equation
  y = prefix(u);
end Prefix;

function reversed
  input Real u;
  output Real y[5];
algorithm
  y[5] := u;
  for i in 4:-1:1 loop
    y[i] := 2.0 * y[i + 1];
  end for;
end reversed;

model Reversed
  input Real u = 1.0;
  output Real y[5];
equation
  y = reversed(u);
end Reversed;

function firstElementGuard
  input Real u;
  output Real y[4];
algorithm
  for i in 1:4 loop
    if i == 1 then
      y[i] := u;
    else
      y[i] := y[i - 1] * 3.0;
    end if;
  end for;
end firstElementGuard;

model FirstElementGuard
  input Real u = 2.0;
  output Real y[4];
equation
  y = firstElementGuard(u);
end FirstElementGuard;

function complementaryHalves
  input Real u;
  output Real y[6];
algorithm
  for i in 1:6 loop
    if i <= 3 then
      y[i] := u + i;
    else
      y[i] := u - i;
    end if;
  end for;
end complementaryHalves;

model ComplementaryHalves
  input Real u = 10.0;
  output Real y[6];
equation
  y = complementaryHalves(u);
end ComplementaryHalves;

function pairs
  input Real u;
  output Real y[6];
algorithm
  y[1:2] := {u, u + 1.0};
  for i in 3:2:5 loop
    y[i:i + 1] := y[i - 2:i - 1] * 2.0;
  end for;
end pairs;

model Pairs
  input Real u = 1.0;
  output Real y[6];
equation
  y = pairs(u);
end Pairs;

function grid
  input Real u;
  output Real y[3, 2];
algorithm
  for i in 1:3 loop
    for j in 1:2 loop
      y[i, j] := u * i + j;
    end for;
  end for;
end grid;

model Grid
  input Real u = 10.0;
  output Real y[3, 2];
equation
  y = grid(u);
end Grid;

function laterElement
  input Real u;
  output Real y[3];
algorithm
  y[3] := u;
  for i in 1:2 loop
    y[i] := y[i + 1];
  end for;
end laterElement;

model LaterElement
  input Real u = 1.0;
  output Real y[3];
equation
  y = laterElement(u);
end LaterElement;

function blocks
  input Real u;
  input Integer nBase;
  input Integer mBase;
  output Real y[nBase * mBase, nBase * mBase];
algorithm
  for i in 1:nBase loop
    for j in 1:nBase loop
      for ii in (i - 1) * mBase + 1:i * mBase loop
        for jj in (j - 1) * mBase + 1:j * mBase loop
          y[ii, jj] := if i == j then u + ii else -jj;
        end for;
      end for;
    end for;
  end for;
end blocks;

model Blocks
  input Real u = 10.0;
  output Real y[4, 4];
equation
  y = blocks(u, 2, 2);
end Blocks;
"#;

fn values(model: &str, names: &[&str]) -> Vec<f64> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(MODELS, "GenericLoopDefinedness.mo")
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
                .find(|slot| slot.name.replace(' ', "") == *name)
                .unwrap_or_else(|| panic!("{model} has no {name}"))
                .value
        })
        .collect()
}

fn vector(name: &str, count: usize) -> Vec<String> {
    (1..=count)
        .map(|index| format!("{name}[{index}]"))
        .collect()
}

fn read(model: &str, count: usize) -> Vec<f64> {
    let names = vector("y", count);
    values(model, &names.iter().map(String::as_str).collect::<Vec<_>>())
}

#[test]
fn an_element_an_earlier_iteration_defined_is_readable() {
    assert_eq!(read("Prefix", 5), vec![10.0, 11.0, 12.0, 13.0, 14.0]);
    assert_eq!(read("Reversed", 5), vec![16.0, 8.0, 4.0, 2.0, 1.0]);
}

#[test]
fn a_guard_on_the_first_binder_value_starts_the_recurrence() {
    assert_eq!(read("FirstElementGuard", 4), vec![2.0, 6.0, 18.0, 54.0]);
}

#[test]
fn complementary_binder_guards_define_every_element() {
    assert_eq!(
        read("ComplementaryHalves", 6),
        vec![11.0, 12.0, 13.0, 6.0, 5.0, 4.0]
    );
}

#[test]
fn strided_slices_read_the_pair_an_earlier_iteration_wrote() {
    assert_eq!(read("Pairs", 6), vec![1.0, 2.0, 2.0, 4.0, 4.0, 8.0]);
}

#[test]
fn a_nested_loop_defines_its_whole_grid() {
    let names = ["y[1,1]", "y[1,2]", "y[2,1]", "y[2,2]", "y[3,1]", "y[3,2]"];
    assert_eq!(
        values("Grid", &names),
        vec![11.0, 12.0, 21.0, 22.0, 31.0, 32.0]
    );
}

#[test]
fn an_element_only_a_later_iteration_defines_is_refused() {
    let error = Compiler::new()
        .model("LaterElement")
        .compile_str(MODELS, "GenericLoopDefinedness.mo")
        .map(|_| ())
        .expect_err("y[2] is read before any iteration writes it");
    assert!(
        error
            .to_string()
            .contains("reads elements of `y` that do not all have a definition"),
        "{error}"
    );
}

const LONG_RECURRENCE: &str = r#"
function longRecurrence
  input Real u;
  output Real y[14401];
algorithm
  y[1] := u;
  for i in 2:14401 loop
    y[i] := y[i - 1] + 1.0;
  end for;
end longRecurrence;

model LongRecurrence
  input Real u = 1.0;
  output Real last;
protected
  Real y[14401];
equation
  y = longRecurrence(u);
  last = y[14401];
end LongRecurrence;
"#;

/// The definedness proof of a 14401-point recurrence resolves its body a
/// fixed number of times; resolving it once per point took about 46 s.
#[test]
fn a_long_recurrence_is_proven_without_visiting_each_point() {
    let started = std::time::Instant::now();
    Compiler::new()
        .model("LongRecurrence")
        .compile_str(LONG_RECURRENCE, "LongRecurrence.mo")
        .unwrap_or_else(|error| panic!("LongRecurrence should compile: {error}"));
    assert!(
        started.elapsed() < std::time::Duration::from_secs(20),
        "compile took {:?}",
        started.elapsed()
    );
}

/// Block ranges that depend on outer binders (`(i - 1) * m + 1:i * m`) tile
/// the output, so the nest defines every element even though no single
/// binder range covers an axis.
#[test]
fn dependent_block_ranges_define_the_whole_output() {
    let mut names = Vec::new();
    let mut expected = Vec::new();
    for row in 1..=4 {
        for column in 1..=4 {
            names.push(format!("y[{row},{column}]"));
            let same_block = (row - 1) / 2 == (column - 1) / 2;
            expected.push(if same_block {
                10.0 + f64::from(row)
            } else {
                -f64::from(column)
            });
        }
    }
    let names = names.iter().map(String::as_str).collect::<Vec<_>>();
    assert_eq!(values("Blocks", &names), expected);
}

const DEAD_BRANCH: &str = r#"
function deadBranch
  output Real y[3];
  output Real z;
algorithm
  y := {1.0, 2.0, 3.0};
  z := 0.0;
  for i in 1:3 loop
    if i > 5 then
      y[i] := -1.0;
    else
    end if;
    z := z + y[i];
  end for;
end deadBranch;

model DeadBranch
  output Real y[3];
  output Real z;
equation
  (y, z) = deadBranch();
end DeadBranch;
"#;

/// A branch no binder value selects never runs: the conditional changes
/// nothing, and the reachable empty branch is not a missing definition.
#[test]
fn a_branch_no_iteration_selects_changes_nothing() {
    let compiled = Compiler::new()
        .model("DeadBranch")
        .compile_str(DEAD_BRANCH, "DeadBranch.mo")
        .unwrap_or_else(|error| panic!("DeadBranch should compile: {error}"));
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .unwrap_or_else(|error| panic!("DeadBranch should evaluate: {error}"));
    let value = |name: &str| {
        probe
            .report
            .solver_y
            .iter()
            .find(|slot| slot.name.replace(' ', "") == name)
            .unwrap_or_else(|| panic!("DeadBranch has no {name}"))
            .value
    };
    assert_eq!(
        ["y[1]", "y[2]", "y[3]", "z"].map(value),
        [1.0, 2.0, 3.0, 6.0]
    );
}
