//! #483: every GALEC equivalence fixture refines into a Solve block
//! (DAE → GALEC → Solve IR) whose lifecycle methods match the
//! `rumoca-eval-galec` oracle bit for bit: every block slot and the
//! error-signal status after `Startup`, `Recalibrate` and each `DoStep`.
//!
//! Inputs and tunable parameters take their declared literal start, or a
//! deterministic value that changes every tick, on both sides alike.

use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
use rumoca_eval_solve::{TypedValue, eval_algorithm_method};
use rumoca_ir_galec::ast::{Expression, InterfaceKind};
use rumoca_ir_galec::package::{AlgorithmCodePackage, CheckedAlgorithmBlock};
use rumoca_ir_solve::{
    SolveAlgorithmBlock, SolveArithmeticProfile, SolveBlockMethodKind, SolveIntegerDomain,
    SolveRealFormat, SolveScalarType, SolveStorageClass, SolveValueKind,
};
use rumoca_phase_galec::{GalecInput, GalecOptions, lower_to_algorithm_code};
use rumoca_phase_solve::lower_algorithm_block;

use super::galec_equivalence::{
    ARRAY_FUNCTION, BRANCH_ALIAS_SHARED_CALL, CONDITIONAL_SHARED_CALL, DESTINATION_ALIAS,
    DISCRETE_IIR, ELIMINATION_ORDER, ENTRY_CLAMP, EQUIV_COUNTER, EXCHANGE_ROWS, GUARDED_SCATTER,
    GUARDED_SHARED_CALL, LIMIT_SMOKE, MULTIRATE_COUNTER, NAN_SIGNAL, PASS_THROUGH_WRAPPER,
    RANGE_MATRIX, SHARED_CALL_ORDER, SHARED_TUPLE_CALL, SHIFT_ROWS, STAGED_GUARD,
    STARTUP_INPUT_SCOPE, SUBSTITUTED_SCRATCH_ELIMINATION, UNBOUNDED_LIMIT,
};

pub(super) const FIXTURES: [&str; 32] = [
    EQUIV_COUNTER,
    DISCRETE_IIR,
    ARRAY_FUNCTION,
    MULTIRATE_COUNTER,
    NAN_SIGNAL,
    LIMIT_SMOKE,
    RANGE_MATRIX,
    UNBOUNDED_LIMIT,
    ENTRY_CLAMP,
    STARTUP_INPUT_SCOPE,
    GUARDED_SCATTER,
    ELIMINATION_ORDER,
    EXCHANGE_ROWS,
    SHIFT_ROWS,
    SUBSTITUTED_SCRATCH_ELIMINATION,
    SHARED_TUPLE_CALL,
    GUARDED_SHARED_CALL,
    CONDITIONAL_SHARED_CALL,
    SHARED_CALL_ORDER,
    STAGED_GUARD,
    DESTINATION_ALIAS,
    PASS_THROUGH_WRAPPER,
    BRANCH_ALIAS_SHARED_CALL,
    SLICE_STORE_AFTER_SIGNAL,
    NEGATED_IDENTITY_AFTER_SIGNAL,
    CONCATENATED_TRANSITION_AFTER_SIGNAL,
    OUT_OF_RANGE_UNDER_GUARD,
    OUT_OF_RANGE_ON_THE_FALSE_PATH,
    UNBOUNDED_INTEGER_INCREMENT,
    NAN_BEFORE_A_REACHABLE_FAULT,
    pub_signals::DISTINCT_SIGNALS_ACROSS_A_REGION_CALL,
    HOISTED_CONDITION_BEFORE_A_FAULT,
];

// Each raises NAN from a Real comparison inside a function region and then
// stores through a selection whose static index range is in bounds only on
// the guarded path: the slice guard `i >= 1 and i <= 3`, the `elseif i <= u`
// chain of a concatenation, and `0 - (if i == j then 1 else 0)` for
// `-identity(3)`. None can fault, so none loses the signal (EL007).

const SLICE_STORE_AFTER_SIGNAL: &str = r#"
model SliceStoreAfterSignal
  function f
    input Real v[3];
    output Real X[4];
  protected
    Real a;
  algorithm
    if v[1] < 0.01 then a := 1.0; else a := 2.0; end if;
    X[1:3] := a * v;
    X[4] := 1.0;
  end f;
  constant Real samplePeriod = 0.001;
  discrete Real x[4](each start = 0.5);
equation
  when sample(0.0, samplePeriod) then
    x = f(pre(x[1:3]));
  end when;
end SliceStoreAfterSignal;
"#;

const NEGATED_IDENTITY_AFTER_SIGNAL: &str = r#"
model NegatedIdentityAfterSignal
  function g
    input Real w[3];
    output Real A[3, 3];
  protected
    Real a;
  algorithm
    if w[1] < 0.01 then a := 1.0; else a := 2.0; end if;
    A := -identity(3);
  end g;
  discrete Real x[3, 3](each start = 0.5);
equation
  when sample(0.0, 0.001) then
    x = g(pre(x[1, 1:3]));
  end when;
end NegatedIdentityAfterSignal;
"#;

const CONCATENATED_TRANSITION_AFTER_SIGNAL: &str = r#"
model ConcatenatedTransitionAfterSignal
  constant Integer TangentLength = 15;
  function wedge
    input Real v[3];
    output Real S[3, 3];
  algorithm
    S := [0, -v[3], v[2]; v[3], 0, -v[1]; -v[2], v[1], 0];
  end wedge;
  function errorDynamics
    input Real w[3];
    input Real force[3];
    output Real A[TangentLength, TangentLength];
  protected
    Real omegaCross[3, 3];
    Real forceCross[3, 3];
  algorithm
    omegaCross := wedge(w);
    forceCross := wedge(force);
    A := cat(1,
      cat(2, -omegaCross, identity(3), zeros(3, 9)),
      cat(2, zeros(3, 3), -omegaCross, -forceCross, zeros(3, 3), -identity(3)),
      cat(2, zeros(3, 6), -omegaCross, -identity(3), zeros(3, 3)),
      zeros(6, TangentLength));
  end errorDynamics;
  function f
    input Real w[3];
    output Real A[TangentLength, TangentLength];
  protected
    Real a;
  algorithm
    if w[1] < 0.01 then a := 1.0; else a := 2.0; end if;
    A := a * errorDynamics(w, w);
  end f;
  discrete Real x[TangentLength, TangentLength](each start = 0.5);
equation
  when sample(0.0, 0.001) then
    x = f(pre(x[1, 1:3]));
  end when;
end ConcatenatedTransitionAfterSignal;
"#;

// A signal followed by a fault that can occur: a guarded read whose guard
// still admits an out-of-range index, on the true path and on the false
// path, and an increment of an unbounded Integer input (overflows at
// INT_MAX). A function's top level stores its status as it goes and a fault
// carries it to the caller (EL007), so these refine and must fault
// exactly as the oracle does.

const OUT_OF_RANGE_UNDER_GUARD: &str = r#"
model OutOfRangeUnderGuard
  function f
    input Real v[3];
    input Real w[2];
    output Real X[5];
  protected
    Real a;
  algorithm
    if v[1] < 0.01 then a := 1.0; else a := 2.0; end if;
    for i in 1:5 loop
      if i >= 2 and i <= 4 then X[i] := a * v[i]; else X[i] := w[1]; end if;
    end for;
  end f;
  discrete Real x[5](each start = 0.5);
equation
  when sample(0.0, 0.001) then
    x = f(pre(x[1:3]), pre(x[4:5]));
  end when;
end OutOfRangeUnderGuard;
"#;

const OUT_OF_RANGE_ON_THE_FALSE_PATH: &str = r#"
model OutOfRangeOnTheFalsePath
  function f
    input Real v[3];
    input Real w[2];
    output Real X[5];
  protected
    Real a;
  algorithm
    if v[1] < 0.01 then a := 1.0; else a := 2.0; end if;
    for i in 1:5 loop
      if i <= 1 then X[i] := w[1]; else X[i] := a * v[i - 1] + v[i - 2]; end if;
    end for;
  end f;
  discrete Real x[5](each start = 0.5);
equation
  when sample(0.0, 0.001) then
    x = f(pre(x[1:3]), pre(x[4:5]));
  end when;
end OutOfRangeOnTheFalsePath;
"#;

const UNBOUNDED_INTEGER_INCREMENT: &str = r#"
model UnboundedIntegerIncrement
  function g
    input Real w[3];
    input Integer k;
    output Real A[3];
  protected
    Real a;
  algorithm
    if w[1] < 0.01 then a := 1.0; else a := 2.0; end if;
    A := {k + 1, a, a};
  end g;
  discrete Real x[3](each start = 0.5);
  discrete Integer n(start = 1);
equation
  when sample(0.0, 0.001) then
    n = pre(n) + 1;
    x = g(pre(x), pre(n));
  end when;
end UnboundedIntegerIncrement;
"#;

const SIGNALS: [&str; 6] = [
    "INVALID_ARGUMENT",
    "OVERFLOW",
    "NAN",
    "SOLVE_LINEAR_EQUATIONS_FAILED",
    "NO_SOLUTION_FOUND",
    "UNSPECIFIED_ERROR",
];

const TICKS: usize = 5;

/// The oracle's binary64 Real with the embedded target's signed32 Integer.
fn profile() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into()).unwrap(),
    )
}

pub(super) fn model_name(source: &str) -> &str {
    source
        .lines()
        .find_map(|line| {
            line.strip_prefix("model ")
                .or_else(|| line.strip_prefix("block "))
        })
        .map(str::trim)
        .expect("fixture declares a model or block")
}

fn project(source: &str) -> Result<AlgorithmCodePackage, String> {
    let name = model_name(source);
    let compiled = rumoca::Compiler::new()
        .model(name)
        .compile_str(source, &format!("{name}.mo"))
        .map_err(|error| format!("compile: {error}"))?;
    lower_to_algorithm_code(
        &GalecInput::new(&compiled.dae, name),
        &GalecOptions::default(),
    )
    .map_err(|errors| format!("GALEC projection: {errors:?}"))
}

/// One flat element of a value: the bits that must agree.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Bits {
    Real(u64),
    Integer(i64),
    Boolean(bool),
}

fn oracle_bits(value: &Value, into: &mut Vec<Bits>) {
    match value {
        Value::Real(value) => into.push(Bits::Real(value.to_bits())),
        Value::Integer(value) => into.push(Bits::Integer(*value)),
        Value::Boolean(value) => into.push(Bits::Boolean(*value)),
        Value::Array(items) => items.iter().for_each(|item| oracle_bits(item, into)),
        other => panic!("unexpected oracle value {other:?}"),
    }
}

fn solve_bits(value: &TypedValue) -> Vec<Bits> {
    value
        .elements()
        .iter()
        .map(|element| match element {
            SolveValueKind::Real64(bits) => Bits::Real(*bits),
            SolveValueKind::Integer(value) => Bits::Integer(*value),
            SolveValueKind::Boolean(value) => Bits::Boolean(*value),
            SolveValueKind::Real32(bits) => panic!("binary32 element {bits:#x}"),
        })
        .collect()
}

/// A deterministic element `index` of an external slot at `tick`.
fn generated(scalar: SolveScalarType, index: usize, tick: usize) -> (SolveValueKind, Value) {
    let seed = (index + 3 * tick + 1) as i64;
    match scalar {
        SolveScalarType::Real { .. } => {
            let value = 0.375 * seed as f64 - 0.5;
            (SolveValueKind::Real64(value.to_bits()), Value::Real(value))
        }
        SolveScalarType::Integer(_) => (SolveValueKind::Integer(seed), Value::Integer(seed)),
        SolveScalarType::Boolean => {
            let value = seed % 2 == 0;
            (SolveValueKind::Boolean(value), Value::Boolean(value))
        }
    }
}

fn literal(start: &Expression) -> Option<(SolveValueKind, Value)> {
    match start {
        Expression::Real(value) => {
            Some((SolveValueKind::Real64(value.to_bits()), Value::Real(*value)))
        }
        Expression::Integer(value) => {
            Some((SolveValueKind::Integer(*value), Value::Integer(*value)))
        }
        Expression::Bool(value) => Some((SolveValueKind::Boolean(*value), Value::Boolean(*value))),
        Expression::Paren(inner) => literal(inner),
        _ => None,
    }
}

/// Write every input (and, before Startup, every tunable) on both sides.
fn set_external(
    package: &AlgorithmCodePackage,
    solve: &SolveAlgorithmBlock,
    oracle: &mut Evaluator<'_>,
    state: &mut [Option<TypedValue>],
    tick: usize,
) {
    let interface = &package.block().interface;
    for (index, (slot, variable)) in solve.slots().iter().zip(interface).enumerate() {
        let tunable = variable.kind == InterfaceKind::TunableParameter;
        if variable.kind == InterfaceKind::Output || (tunable && tick > 0) {
            continue;
        }
        let value_type = slot.value_type();
        let count = value_type.scalar_count() as usize;
        let start = variable
            .start
            .as_ref()
            .filter(|_| count == 1)
            .and_then(literal);
        let elements = (0..count)
            .map(|element| {
                start.clone().unwrap_or_else(|| {
                    generated(value_type.element_type(), index * 7 + element, tick)
                })
            })
            .collect::<Vec<_>>();
        let oracle_value = nest(
            value_type.dimensions(),
            &mut elements.iter().map(|(_, value)| value.clone()),
        );
        oracle
            .set_state(slot.source().qualified_name(), oracle_value)
            .unwrap();
        state[index] = Some(
            TypedValue::construct(
                value_type.clone(),
                elements.into_iter().map(|(kind, _)| kind).collect(),
            )
            .unwrap(),
        );
    }
}

fn nest(dims: &[u32], elements: &mut impl Iterator<Item = Value>) -> Value {
    match dims.split_first() {
        None => elements.next().expect("one element per scalar"),
        Some((extent, rest)) => Value::Array((0..*extent).map(|_| nest(rest, elements)).collect()),
    }
}

fn compare(
    name: &str,
    at: &str,
    solve: &SolveAlgorithmBlock,
    oracle: &Evaluator<'_>,
    state: &[Option<TypedValue>],
) -> Result<(), String> {
    for (slot, value) in solve.slots().iter().zip(state) {
        let source = slot.source().qualified_name();
        if slot.storage() == SolveStorageClass::SignalStatus {
            let signal = source.trim_start_matches("ErrorSignalStatus.");
            let expected = vec![Bits::Boolean(oracle.active_signals().contains(signal))];
            let actual = value.as_ref().map(solve_bits);
            if actual.as_ref() != Some(&expected) {
                return Err(format!("{name} {at}: {source} {actual:?} vs {expected:?}"));
            }
            continue;
        }
        let mut expected = Vec::new();
        oracle_bits(
            oracle
                .state(source)
                .map_err(|error| format!("{name} {at}: {source}: {error}"))?,
            &mut expected,
        );
        let actual = value.as_ref().map(solve_bits).unwrap_or_default();
        if actual != expected {
            return Err(format!("{name} {at}: {source} {actual:?} vs {expected:?}"));
        }
    }
    debug_assert_eq!(SIGNALS.len(), 6);
    Ok(())
}

fn run(
    name: &str,
    package: &AlgorithmCodePackage,
    solve: &SolveAlgorithmBlock,
) -> Result<(), String> {
    let checked = CheckedAlgorithmBlock::construct(package.block().clone())
        .map_err(|error| format!("{name}: checked block: {error:?}"))?;
    let mut oracle = Evaluator::new(&checked, IntegerDomain::signed_32())
        .map_err(|error| format!("{name}: oracle: {error:?}"))?;
    let mut state = vec![None; solve.slots().len()];
    set_external(package, solve, &mut oracle, &mut state, 0);
    let step =
        |method, oracle: &mut Evaluator<'_>, state: &mut Vec<Option<TypedValue>>, at: &str| {
            let expected = match method {
                SolveBlockMethodKind::Startup => oracle.startup(),
                SolveBlockMethodKind::Recalibrate => oracle.recalibrate(),
                SolveBlockMethodKind::DoStep => oracle.do_step(),
            };
            let actual = eval_algorithm_method(solve, method, state);
            match (expected, actual) {
                (Ok(()), Ok(())) => compare(name, at, solve, oracle, state),
                // Both fault and keep what was stored before the fault.
                (Err(_), Err(_)) if method == SolveBlockMethodKind::Startup => Ok(()),
                (Err(_), Err(_)) => compare(name, at, solve, oracle, state),
                (expected, actual) => Err(format!(
                    "{name} {at}: oracle {expected:?} vs Solve {actual:?}"
                )),
            }
        };
    step(
        SolveBlockMethodKind::Startup,
        &mut oracle,
        &mut state,
        "Startup",
    )?;
    step(
        SolveBlockMethodKind::Recalibrate,
        &mut oracle,
        &mut state,
        "Recalibrate",
    )?;
    for tick in 1..=TICKS {
        set_external(package, solve, &mut oracle, &mut state, tick);
        step(
            SolveBlockMethodKind::DoStep,
            &mut oracle,
            &mut state,
            &format!("DoStep {tick}"),
        )?;
    }
    Ok(())
}

/// Fixtures refused today; every generated fixture refines.
const REFUSED: [&str; 0] = [];

#[test]
fn galec_fixtures_refine_into_solve_blocks_that_match_the_oracle() {
    let mut report = Vec::new();
    let mut mismatches = 0;
    let mut refused = Vec::new();
    for source in FIXTURES {
        let name = model_name(source);
        let outcome =
            project(source).and_then(|package| match lower_algorithm_block(&package, profile()) {
                Ok(solve) => run(name, &package, &solve).map(|()| true),
                Err(error) => {
                    report.push(format!("refused {name}: {error}"));
                    Ok(false)
                }
            });
        match outcome {
            Ok(true) => report.push(format!("ok      {name}")),
            Ok(false) => refused.push(name),
            Err(error) => {
                mismatches += 1;
                report.push(format!("FAILED  {error}"));
            }
        }
    }
    let report = report.join("\n");
    eprintln!("{report}");
    assert_eq!(mismatches, 0, "{report}");
    assert_eq!(refused, REFUSED, "{report}");
}

/// NAN raised at a function's top level (from `zero / zero`), then a
/// reachable out-of-range read: the fault keeps the NAN bit.
const NAN_BEFORE_A_REACHABLE_FAULT: &str = r#"
model NanBeforeAReachableFault
  parameter Real zero = 0.0;
  function f
    input Real v[3];
    input Real z;
    output Real X[5];
  protected
    Real a;
  algorithm
    if z / z < 0.01 then a := 1.0; else a := 2.0; end if;
    for i in 1:5 loop
      if i >= 2 and i <= 4 then X[i] := a * v[i]; else X[i] := v[1]; end if;
    end for;
  end f;
  discrete Real x[5](each start = 0.5);
equation
  when sample(0.0, 0.001) then
    x = f(pre(x[1:3]), zero);
  end when;
end NanBeforeAReachableFault;
"#;

/// Fixtures shared with the C suite.
pub(super) mod pub_signals {
    /// The region case: the caller publishes NAN at its top level,
    /// then inside an `if` region (with no earlier signal of its own there)
    /// calls a callee that raises OVERFLOW and then faults. Both bits survive.
    pub(in super::super) const DISTINCT_SIGNALS_ACROSS_A_REGION_CALL: &str = r#"
model DistinctSignalsAcrossARegionCall
  parameter Real zero = 0.0;
  parameter Real big = 1.0e30;
  function g
    input Real v[3];
    input Real b;
    output Real X[5];
  protected
    Integer k;
  algorithm
    k := integer(b);
    for i in 1:5 loop
      X[i] := v[i] + k;
    end for;
  end g;
  function f
    input Real v[3];
    input Real z;
    input Real b;
    input Boolean flag;
    output Real X[5];
  algorithm
    if z / z < 0.01 then X := zeros(5); else X := ones(5); end if;
    if flag then
      X := g(v, b);
    end if;
  end f;
  discrete Real x[5](each start = 0.5);
equation
  when sample(0.0, 0.001) then
    x = f(pre(x[1:3]), zero, big, true);
  end when;
end DistinctSignalsAcrossARegionCall;
"#;
}

/// A loop body whose fault precedes its raise: the second iteration can
/// fault after the first raised, while that bit is still held by the loop.
const RAISE_THEN_FAULT_ACROSS_ITERATIONS: &str = r#"
model RaiseThenFaultAcrossIterations
  parameter Real zero = 0.0;
  function f
    input Real v[3];
    input Real z;
    output Real X[5];
  algorithm
    for i in 1:5 loop
      X[i] := v[i];
      if z / z < 0.01 then X[1] := 0.0; end if;
    end for;
  end f;
  discrete Real x[5](each start = 0.5);
equation
  when sample(0.0, 0.001) then
    x = f(pre(x[1:3]), zero);
  end when;
end RaiseThenFaultAcrossIterations;
"#;

/// A raise written inside a lazy arm before a reachable fault: upstream
/// binds the comparison to a top-level temporary first, so the raise is
/// published before the arm runs and the fault keeps it (admitted; the
/// region form of this case is refused at the Algorithm Code level in
/// rumoca-phase-solve's unit tests).
const HOISTED_CONDITION_BEFORE_A_FAULT: &str = r#"
model HoistedConditionBeforeAFault
  parameter Real zero = 0.0;
  function f
    input Real v[3];
    input Real z;
    input Integer n;
    input Boolean flag;
    output Real X[3];
  algorithm
    X := v;
    X[1] := if flag then (if z / z < 0.01 then v[n] else v[2]) else v[3];
  end f;
  discrete Real x[3](each start = 0.5);
  discrete Integer n(start = 1);
equation
  when sample(0.0, 0.001) then
    n = pre(n) + 1;
    x = f(pre(x), zero, pre(n), pre(n) > 0);
  end when;
end HoistedConditionBeforeAFault;
"#;

/// The loop case stays refused: a bit raised by one iteration is held by the loop
/// when a later iteration faults. Were the guard relaxed, the oracle
/// comparison below shows the lost status (the guard-relaxation negative
/// control).
#[test]
fn a_raise_a_loop_still_holds_at_a_fault_is_refused() {
    let source = RAISE_THEN_FAULT_ACROSS_ITERATIONS;
    let name = model_name(source);
    let package = project(source).unwrap_or_else(|error| panic!("{name}: {error}"));
    match lower_algorithm_block(&package, profile()) {
        Err(error) => assert!(
            error.to_string().contains("fault after"),
            "{name}: refused for the wrong reason: {error}"
        ),
        Ok(solve) => panic!(
            "{name}: admitted a raise its loop still holds at a fault; \
             oracle comparison: {:?}",
            run(name, &package, &solve)
        ),
    }
}
