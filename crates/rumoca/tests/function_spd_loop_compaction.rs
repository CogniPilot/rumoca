//! Regression for compact tensor ownership in a Cholesky solve.
//!
//! The scalar `value` and `solveRow` variables are call-local scratch values,
//! while `L`, `Y`, and `X` are authoritative tensors. Lowering must eliminate
//! the dominated scratch definitions without expanding array assignments or
//! forging a loop-carried scalar transition.

use rumoca::Compiler;
use rumoca_ir_dae as dae;
use rumoca_sim::{SimOptions, eval_dae_at};

const MODEL: &str = r#"
within;

function solveSPD
  input Real A[:, size(A, 1)];
  input Real B[size(A, 1), :];
  output Real X[size(B, 1), size(B, 2)];
protected
  Real L[size(A, 1), size(A, 2)];
  Real Y[size(B, 1), size(B, 2)];
  Real value;
  Real scale;
  Integer solveRow;
  Boolean ok;
algorithm
  L := zeros(size(A, 1), size(A, 2));
  Y := zeros(size(B, 1), size(B, 2));
  X := zeros(size(B, 1), size(B, 2));
  scale := 0.0;
  for i in 1:size(A, 1) loop
    scale := max(scale, abs(A[i, i]));
  end for;
  scale := max(scale, 1.0e-30);
  ok := true;

  for row in 1:size(A, 1) loop
    for column in 1:row loop
      value := 0.5 * (A[row, column] + A[column, row]);
      for k in 1:(column - 1) loop
        value := value - L[row, k] * L[column, k];
      end for;
      if row == column then
        if value <= 1.0e-12 * scale then
          ok := false;
          L[row, column] := 1.0;
        else
          L[row, column] := sqrt(value);
        end if;
      else
        L[row, column] := value / L[column, column];
      end if;
    end for;
  end for;

  if ok then
    for rhs in 1:size(B, 2) loop
      for row in 1:size(A, 1) loop
        value := B[row, rhs];
        for k in 1:(row - 1) loop
          value := value - L[row, k] * Y[k, rhs];
        end for;
        Y[row, rhs] := value / L[row, row];
      end for;
      for reverseRow in 1:size(A, 1) loop
        solveRow := size(A, 1) + 1 - reverseRow;
        value := Y[solveRow, rhs];
        for k in (solveRow + 1):size(A, 1) loop
          value := value - L[k, solveRow] * X[k, rhs];
        end for;
        X[solveRow, rhs] := value / L[solveRow, solveRow];
      end for;
    end for;
  end if;
end solveSPD;

model ObserveSolve
  Real solution[1, 1];
  Real x(start=0.0, fixed=true);
equation
  solution = solveSPD([4.0], identity(1));
  der(x) = sum(solution);
end ObserveSolve;

model ObserveSolve3
  Real solution[3, 1];
  Real state[3](each start=0.0, each fixed=true);
equation
  solution = solveSPD(
    [4.0, 1.0, 0.0; 1.0, 3.0, 0.0; 0.0, 0.0, 2.0],
    [1.0; 2.0; 4.0]);
  der(state) = solution[:, 1];
end ObserveSolve3;
"#;

const RUNTIME_MATRIX_INDEX_MODEL: &str = r#"
within;

function updateAndRead
  input Real source[2, 2];
  input Integer row;
  input Integer column;
  output Real observed;
protected
  Real updated[2, 2];
algorithm
  updated := source;
  updated[row, column] := 9.0;
  observed := updated[row, column];
end updateAndRead;

model RuntimeMatrixIndex
  output Real observed;
equation
  observed = updateAndRead(
    [1.0, 2.0; 3.0, 4.0],
    if time < 1.0 then 2 else 1,
    1);
end RuntimeMatrixIndex;
"#;

const ASSERTION_ONLY_LOOP_MODEL: &str = r#"
within;

function checkSamples
  input Real limit;
  output Real result;
protected
  Real sampleValue;
algorithm
  result := 1.0;
  for sampleIndex in 1:3 loop
    sampleValue := sampleIndex;
    assert(sampleValue <= limit, "sample exceeds limit");
  end for;
end checkSamples;

model AssertionLoopSuccess
  Real state(start=0.0, fixed=true);
equation
  der(state) = checkSamples(3.0);
end AssertionLoopSuccess;

model AssertionLoopFailure
  Real state(start=0.0, fixed=true);
equation
  der(state) = checkSamples(2.0);
end AssertionLoopFailure;
"#;

const PARTIAL_GUARDED_OUTPUT_MODEL: &str = r#"
within;

function guardedFill
  input Boolean enabled;
  output Real values[2];
algorithm
  if enabled then
    for index in 1:2 loop
      values[index] := index;
    end for;
  end if;
end guardedFill;

model PartialGuardedOutput
  output Real values[2];
equation
  values = guardedFill(time > 0.0);
end PartialGuardedOutput;
"#;

#[test]
fn spd_solve_keeps_array_assignments_and_compact_loop_owners() {
    let compiled = Compiler::new()
        .model("ObserveSolve")
        .compile_str(MODEL, "ObserveSolve.mo")
        .expect("the tensor-native SPD solve should compile");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the tensor-native SPD solve should evaluate");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    let derivative = probe
        .report
        .derivatives
        .iter()
        .find(|slot| slot.name == "der(x)")
        .expect("missing der(x)")
        .value;
    assert!((derivative - 0.25).abs() < 1.0e-12);
}

#[test]
fn spd_solve_preserves_sequential_scratch_and_bounds_fold_expansion() {
    let compiled = Compiler::new()
        .model("ObserveSolve3")
        .compile_str(MODEL, "ObserveSolve3.mo")
        .expect("the three-dimensional SPD solve should compile");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the three-dimensional SPD solve should evaluate");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    let derivatives = probe
        .report
        .derivatives
        .iter()
        .map(|slot| slot.value)
        .collect::<Vec<_>>();
    let expected = [1.0 / 11.0, 7.0 / 11.0, 2.0];
    for (actual, expected) in derivatives.iter().zip(expected) {
        assert!(
            (actual - expected).abs() < 1.0e-12,
            "expected derivative {expected}, got {actual}; all derivatives: {derivatives:?}"
        );
    }

    let solve = rumoca_sim::lower_solve_problem(&compiled.dae)
        .expect("the three-dimensional SPD solve should lower to Solve IR");
    let scalar = rumoca_eval_solve::to_scalar_program_block(&solve.continuous.residual)
        .expect("the algebraic residual has one shared scalar view");
    let largest_program = scalar.programs().iter().map(Vec::len).max().unwrap_or(0);
    assert!(
        largest_program < 10_000,
        "a 3x3 Cholesky solve must stay bounded; largest row had {largest_program} operations"
    );
}

#[test]
fn runtime_rank_two_index_reads_and_updates_the_same_coordinate() {
    let compiled = Compiler::new()
        .model("RuntimeMatrixIndex")
        .compile_str(RUNTIME_MATRIX_INDEX_MODEL, "RuntimeMatrixIndex.mo")
        .expect("a rank-two runtime index should compile");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("a rank-two runtime index should lower and evaluate");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    let observed = probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name == "observed")
        .expect("missing observed")
        .value;
    assert_eq!(observed, 9.0);
}

#[test]
fn assertion_only_loop_preserves_each_call_scoped_action() {
    let success = Compiler::new()
        .model("AssertionLoopSuccess")
        .compile_str(ASSERTION_ONLY_LOOP_MODEL, "AssertionOnlyLoop.mo")
        .expect("an assertion-only function loop should compile");
    success.dae.inspect(|view| {
        let function = (0..view.function_count())
            .filter_map(|index| view.function_id(index).and_then(|id| view.function(id)))
            .find(|function| function.name().as_str().ends_with("checkSamples"))
            .expect("the checked function remains visible");
        assert!(
            function_statements_have_assertion(function.statements()),
            "the compact loop must retain its call-scoped assertion"
        );
    });
    let success_solve = rumoca_sim::lower_solve_problem(&success.dae)
        .expect("every assertion iteration should have an exact Solve schedule");
    assert_eq!(success_solve.events.actions.len(), 3);
    assert_eq!(success_solve.events.root_conditions.len(), 3);
    let success_request = rumoca_eval_solve::eval_event_action_request(
        &success_solve.events,
        &vec![0.0; success_solve.layout.y_scalars()],
        &vec![0.0; success_solve.layout.p_scalars()],
        0.0,
        rumoca_eval_solve::RowEvalContext::default(),
    )
    .expect("the successful assertion loop should be evaluable");
    assert_eq!(
        success_request,
        rumoca_eval_solve::EventActionRequest::Continue
    );

    let failure = Compiler::new()
        .model("AssertionLoopFailure")
        .compile_str(ASSERTION_ONLY_LOOP_MODEL, "AssertionOnlyLoop.mo")
        .expect("a runtime-failing assertion loop should still compile");
    let failure_solve = rumoca_sim::lower_solve_problem(&failure.dae)
        .expect("a failing assertion still has a computable Solve schedule");
    assert_eq!(failure_solve.events.actions.len(), 3);
    assert_eq!(failure_solve.events.root_conditions.len(), 3);
    let failure_request = rumoca_eval_solve::eval_event_action_request(
        &failure_solve.events,
        &vec![0.0; failure_solve.layout.y_scalars()],
        &vec![0.0; failure_solve.layout.p_scalars()],
        0.0,
        rumoca_eval_solve::RowEvalContext::default(),
    )
    .expect("the failing assertion action should evaluate deterministically");
    assert!(
        matches!(
            failure_request,
            rumoca_eval_solve::EventActionRequest::AssertionFailed { ref message }
                if message == "sample exceeds limit"
        ),
        "the third iteration must request the source assertion: {failure_request:?}"
    );
}

#[test]
fn guarded_output_loop_requires_a_fallthrough_value() {
    let error = Compiler::new()
        .model("PartialGuardedOutput")
        .compile_str(PARTIAL_GUARDED_OUTPUT_MODEL, "PartialGuardedOutput.mo")
        .expect_err("predicate pushdown must not invent an output value on the false path");
    assert!(
        error
            .to_string()
            .contains("without defining every declared element")
            || error
                .to_string()
                .contains("without a definition on some branch")
            || error
                .to_string()
                .contains("requires assignments or nested conditionals"),
        "unexpected diagnostic: {error}"
    );
}

fn function_statements_have_assertion(statements: dae::FunctionStatements<'_>) -> bool {
    statements.into_iter().any(|statement| match statement {
        dae::FunctionStatementView::Assignment { .. } => false,
        dae::FunctionStatementView::Assertion { .. } => true,
        dae::FunctionStatementView::For { statements, .. } => {
            function_statements_have_assertion(statements)
        }
    })
}
