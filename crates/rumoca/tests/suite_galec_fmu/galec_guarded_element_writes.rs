//! An element write through a computed subscript, under a guard that reads
//! the same array, refines into Solve IR and computes what the GALEC oracle
//! computes.
//!
//! The projection stores such a write as a value binding and one branch per
//! candidate element inside the guard's arm. The first candidate store
//! changes what the guard reads, and Solve refinement once forgot the
//! binding with the guard (EL007 "read of a possibly unassigned local"),
//! refusing valid functions: the array is wholly assigned before the guard
//! (MLS §11.2, §12.4.4), so every read is of an assigned value.

use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
use rumoca_eval_solve::{TypedValue, eval_algorithm_method};
use rumoca_ir_galec::package::CheckedAlgorithmBlock;
use rumoca_ir_solve::{SolveBlockMethodKind, SolveValueKind};
use rumoca_phase_solve::lower_algorithm_block;

use super::galec_solve_refinement::{compare, model_name, profile, project, set_external};

/// The reported reproducer `f` (its guard is false: nothing is stored), and
/// `g`, the same shape over the input, whose guard follows `p[1]`.
const GUARDED_CALL_INDEX_WRITE: &str = r#"
model GuardedCallIndexWrite
  function li
    input Integer i;
    input Integer j;
    output Integer k;
  algorithm
    k := div(i * (i - 1), 2) + j;
  end li;
  function f
    input Real p[3];
    output Real y[3];
  algorithm
    y := zeros(3);
    if y[1] > 0.0 then
      y[li(2, 1)] := 1.0;
    end if;
  end f;
  function g
    input Real p[3];
    output Real y[3];
  algorithm
    y := p;
    if y[1] > 0.0 then
      y[li(2, 1)] := 2.0 * y[1];
    end if;
  end g;
  input Real p[3];
  output Real y[3](each start = 0.0, each fixed = true);
  output Real w[3](each start = 0.0, each fixed = true);
algorithm
  when sample(0, 0.01) then
    y := f(p);
    w := g(p);
  end when;
end GuardedCallIndexWrite;
"#;

/// The packed-storage branch of `LinearAlgebra.SymmetricMatrix.factorPSD`:
/// two nested loops, a scalar local assigned at the top of each pass, and an
/// `elseif` guard reading the factor before a store through a call subscript.
/// (The loop form refined end to end, with a subscript function that cannot
/// fault, is pinned in `rumoca-phase-solve`'s algorithm-block unit tests.)
const PACKED_FACTOR_PSD: &str = r#"
model PackedFactorPSD
  function index
    input Integer i;
    input Integer j;
    input Integer n;
    output Integer k;
  protected
    Integer r = min(i, j);
    Integer c = max(i, j);
  algorithm
    k := div((r - 1) * (2 * n - r), 2) + c;
  end index;
  function lowerIndex
    input Integer i;
    input Integer j;
    output Integer k;
  algorithm
    k := div(i * (i - 1), 2) + j;
  end lowerIndex;
  function factorPSD
    input Integer n;
    input Real p[div(n * (n + 1), 2)];
    output Real lower[div(n * (n + 1), 2)];
    output Boolean ok;
  protected
    Real value;
  algorithm
    lower := zeros(div(n * (n + 1), 2));
    ok := true;
    for i in 1:n loop
      for j in 1:i loop
        value := p[index(i, j, n)];
        for k in 1:j - 1 loop
          value := value - lower[lowerIndex(i, k)] * lower[lowerIndex(j, k)];
        end for;
        if i == j then
          if value >= 0.0 and abs(value) < 1.0e30 then
            lower[lowerIndex(i, j)] := sqrt(value);
          else
            ok := false;
          end if;
        elseif lower[lowerIndex(j, j)] > 0.0 then
          lower[lowerIndex(i, j)] := value / lower[lowerIndex(j, j)];
        elseif not abs(value) <= 0.0 then
          ok := false;
        end if;
      end for;
    end for;
  end factorPSD;
  input Real p[6];
  output Real lower[6](each start = 0.0, each fixed = true);
  output Boolean ok(start = false, fixed = true);
algorithm
  when sample(0, 0.01) then
    (lower, ok) := factorPSD(3, p);
  end when;
end PackedFactorPSD;
"#;

/// Refine `source`, run Startup and one DoStep with the Real input `p` on the
/// Solve evaluator and on the GALEC oracle, require every slot to agree bit
/// for bit, and hand the oracle to `check`.
fn step_with(source: &str, p: &[f64], check: impl FnOnce(&Evaluator<'_>)) {
    let name = model_name(source);
    let package = project(source).unwrap_or_else(|error| panic!("{name}: {error}"));
    let solve = lower_algorithm_block(&package, profile())
        .unwrap_or_else(|error| panic!("{name} refines into Solve IR: {error}"));
    let checked = CheckedAlgorithmBlock::construct(package.block().clone())
        .unwrap_or_else(|error| panic!("{name}: checked block: {error:?}"));
    let mut oracle = Evaluator::new(&checked, IntegerDomain::signed_32())
        .unwrap_or_else(|error| panic!("{name}: oracle: {error:?}"));
    let mut state = vec![None; solve.slots().len()];
    set_external(&package, &solve, &mut oracle, &mut state, 0);
    let input = solve
        .slots()
        .iter()
        .position(|slot| slot.source().qualified_name() == "p")
        .expect("the input slot p");
    oracle
        .set_state(
            "p",
            Value::Array(p.iter().copied().map(Value::Real).collect()),
        )
        .unwrap();
    state[input] = Some(
        TypedValue::construct(
            solve.slots()[input].value_type().clone(),
            p.iter()
                .map(|value| SolveValueKind::Real64(value.to_bits()))
                .collect(),
        )
        .unwrap(),
    );
    oracle.startup().expect("oracle Startup");
    eval_algorithm_method(&solve, SolveBlockMethodKind::Startup, &mut state)
        .expect("Solve Startup");
    oracle.do_step().expect("oracle DoStep");
    eval_algorithm_method(&solve, SolveBlockMethodKind::DoStep, &mut state).expect("Solve DoStep");
    compare(name, "DoStep", &solve, &oracle, &state).unwrap_or_else(|error| panic!("{error}"));
    check(&oracle);
}

fn reals(oracle: &Evaluator<'_>, name: &str) -> Vec<f64> {
    let Value::Array(items) = oracle.state(name).expect("an array state") else {
        panic!("{name} is an array");
    };
    items
        .iter()
        .map(|item| match item {
            Value::Real(value) => *value,
            other => panic!("{name} holds Reals, not {other:?}"),
        })
        .collect()
}

#[test]
fn a_guarded_store_through_a_call_subscript_refines_and_matches_the_oracle() {
    // Guard true: g stores 2 * p[1] into element li(2, 1) = 2.
    step_with(GUARDED_CALL_INDEX_WRITE, &[1.5, 4.0, 5.0], |oracle| {
        assert_eq!(reals(oracle, "y"), [0.0, 0.0, 0.0]);
        assert_eq!(reals(oracle, "w"), [1.5, 3.0, 5.0]);
    });
    // Guard false: g stores nothing.
    step_with(GUARDED_CALL_INDEX_WRITE, &[-1.5, 4.0, 5.0], |oracle| {
        assert_eq!(reals(oracle, "y"), [0.0, 0.0, 0.0]);
        assert_eq!(reals(oracle, "w"), [-1.5, 4.0, 5.0]);
    });
}

#[test]
fn the_packed_psd_factorization_passes_the_definedness_proof() {
    // Solve refinement now stops at the fault proof instead: the index
    // functions' Integer arithmetic may overflow for arbitrary arguments, and
    // a loop that raises NAN and then calls them could lose the bit. That is
    // a separate refusal; the definedness proof admits the function.
    let name = model_name(PACKED_FACTOR_PSD);
    let package = project(PACKED_FACTOR_PSD).unwrap_or_else(|error| panic!("{name}: {error}"));
    let Err(error) = lower_algorithm_block(&package, profile()) else {
        panic!("{name}: the fault proof refuses the callee arithmetic in the loop");
    };
    assert!(error.to_string().contains("fault after"), "{name}: {error}");

    // The projected Algorithm Code computes the factor, guards true and false.
    let checked = CheckedAlgorithmBlock::construct(package.block().clone())
        .unwrap_or_else(|error| panic!("{name}: checked block: {error:?}"));
    let factor = |p: &[f64]| {
        let mut oracle = Evaluator::new(&checked, IntegerDomain::signed_32())
            .unwrap_or_else(|error| panic!("{name}: oracle: {error:?}"));
        oracle
            .set_state(
                "p",
                Value::Array(p.iter().copied().map(Value::Real).collect()),
            )
            .unwrap();
        oracle.startup().expect("oracle Startup");
        oracle.do_step().expect("oracle DoStep");
        let Value::Boolean(ok) = *oracle.state("ok").expect("ok") else {
            panic!("ok is Boolean");
        };
        (reals(&oracle, "lower"), ok)
    };
    // p packs the upper triangle row by row; `lower` packs the factor's rows.
    // L = [2 0 0; 1 3 0; 0.5 1 4]: every off-diagonal guard is true.
    assert_eq!(
        factor(&[4.0, 2.0, 1.0, 10.0, 3.5, 17.25]),
        (vec![2.0, 1.0, 3.0, 0.5, 1.0, 4.0], true)
    );
    // L = [0 0 0; 0 2 0; 0 1 3]: column 1 has a zero pivot, so its guards are
    // false and its entries stay zero; column 2's guard is true.
    assert_eq!(
        factor(&[0.0, 0.0, 0.0, 4.0, 2.0, 10.0]),
        (vec![0.0, 0.0, 2.0, 0.0, 1.0, 3.0], true)
    );
    // A zero pivot under a nonzero entry is not positive semidefinite.
    assert_eq!(
        factor(&[0.0, 1.0, 0.0, 1.0, 0.0, 1.0]),
        (vec![0.0, 0.0, 1.0, 0.0, 0.0, 1.0], false)
    );
}
