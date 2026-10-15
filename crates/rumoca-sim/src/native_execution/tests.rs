mod selected_jacobian;

use std::{cell::Cell, num::NonZeroU64};

use rumoca_core::{SourceId, Span};
use rumoca_eval_solve::{
    EvalSolveError, PreparedScalarProgramBlock, PureCallExecution, PureCallInvocation,
    RowEvalContext, TargetAssignmentOutputRequest,
};
use rumoca_ir_solve as solve;

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("native_projection.mo"), 1, 2)
}

fn quadratic_table() -> solve::SolvePureCallTable {
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
    solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![real.clone()],
            vec![solve::SolvePureCallOutput::result(real)],
            span(),
            |builder, inputs, outputs| {
                let x = builder.load(inputs[0], span())?;
                let square = builder.binary(solve::SolveBinaryOperator::Multiply, x, x, span())?;
                let two = builder.constant(solve::SolveValue::real(arithmetic, 2.0), span())?;
                let value = builder.binary(solve::SolveBinaryOperator::Add, square, two, span())?;
                builder.store(outputs[0], value, span())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn native_table_call_row(table: &solve::SolvePureCallTable) -> solve::ScalarProgramBlock {
    solve::ScalarProgramBlock::with_program_spans(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 0 },
            solve::LinearOp::PureCall {
                dst_start: 1,
                input_starts: vec![0].into(),
                site: table.owners()[0].call_site(),
            },
            solve::LinearOp::LoadP { dst: 2, index: 1 },
            solve::LinearOp::LoadP { dst: 3, index: 2 },
            solve::LinearOp::TableLookup {
                dst: 4,
                table_id: 2,
                column: 3,
                input: 1,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ]],
        vec![span()],
    )
    .unwrap()
}

#[test]
fn native_table_with_pure_call_preserves_valid_interpolation() {
    let table = quadratic_table();
    let block = native_table_call_row(&table);
    let backend = super::backend(&table).unwrap();
    let native = backend.compile_expression(&block).unwrap();
    let reference = PreparedScalarProgramBlock::new(block).unwrap();
    let tables = [rumoca_core::ExternalTableData {
        id: 42,
        data: vec![vec![0.0, 10.0], vec![4.0, 18.0]],
        columns: vec![2],
        smoothness: 1,
        extrapolation: 1,
    }];
    for input in [0.0, 0.5, 1.0] {
        let p = [input, 42.0, 1.0];
        let mut expected = [0.0];
        reference
            .eval_with_context(
                &[],
                &p,
                0.0,
                RowEvalContext {
                    pure_calls: Some(&table),
                    external_tables: Some(&tables),
                    ..Default::default()
                },
                &mut expected,
            )
            .unwrap();
        let mut actual = [0.0];
        native.call(&[], &p, 0.0, &tables, &mut actual).unwrap();
        assert_eq!(actual.map(f64::to_bits), expected.map(f64::to_bits));
    }
}

#[test]
fn native_table_with_pure_call_propagates_table_source_fault() {
    let table = quadratic_table();
    let block = native_table_call_row(&table);
    let backend = super::backend(&table).unwrap();
    let native = backend.compile_expression(&block).unwrap();
    let reference = PreparedScalarProgramBlock::new(block).unwrap();
    let p = [0.5, 42.0, 1.0];
    let error = reference
        .eval_with_context(
            &[],
            &p,
            0.0,
            RowEvalContext {
                pure_calls: Some(&table),
                ..Default::default()
            },
            &mut [0.0],
        )
        .unwrap_err();
    assert!(matches!(error, EvalSolveError::ExternalTable { .. }));
    let mut actual = [99.0];
    let error = native.call(&[], &p, 0.0, &[], &mut actual).unwrap_err();
    assert!(
        matches!(error, rumoca_solver::RuntimeSolveError::SourceFault { .. }),
        "{error}"
    );
    assert_eq!(
        actual,
        [99.0],
        "failed table callback must not publish output"
    );
}

#[test]
fn native_table_mirror_preserves_values_and_source_fault_category() {
    let table = quadratic_table();
    let mut ops = native_table_call_row(&table).programs()[0].clone();
    ops[1] = solve::LinearOp::Move { dst: 1, src: 0 };
    let block = solve::ScalarProgramBlock::with_program_spans(vec![ops], vec![span()]).unwrap();
    let backend = super::backend(&table).unwrap();
    let native = backend.compile_expression(&block).unwrap();
    let reference = PreparedScalarProgramBlock::new(block).unwrap();
    let tables = [rumoca_core::ExternalTableData {
        id: 42,
        data: vec![vec![0.0, 10.0], vec![4.0, 18.0]],
        columns: vec![2],
        smoothness: 1,
        extrapolation: 1,
    }];
    let p = [0.5, 42.0, 1.0];
    let mut expected = [0.0];
    reference
        .eval_with_context(
            &[],
            &p,
            0.0,
            RowEvalContext {
                external_tables: Some(&tables),
                ..Default::default()
            },
            &mut expected,
        )
        .unwrap();
    let mut actual = [0.0];
    native.call(&[], &p, 0.0, &tables, &mut actual).unwrap();
    assert_eq!(actual.map(f64::to_bits), expected.map(f64::to_bits));
    for p in [[0.5, 43.0, 1.0], [0.5, 42.0, 2.0]] {
        let error = reference
            .eval_with_context(
                &[],
                &p,
                0.0,
                RowEvalContext {
                    external_tables: Some(&tables),
                    ..Default::default()
                },
                &mut expected,
            )
            .unwrap_err();
        assert!(matches!(error, EvalSolveError::ExternalTable { .. }));
        actual = [99.0];
        let error = native.call(&[], &p, 0.0, &tables, &mut actual).unwrap_err();
        assert!(
            matches!(error, rumoca_solver::RuntimeSolveError::SourceFault { .. }),
            "{error}"
        );
        assert_eq!(actual, [99.0]);
    }
}

#[test]
fn native_backend_rejects_a_foreign_call_table_during_context_admission() {
    let table = quadratic_table();
    let backend = super::backend(&table).unwrap();
    let model = solve::SolveModel {
        pure_calls: table.clone(),
        ..Default::default()
    };
    assert!(backend.validate_model_context(&model).is_ok());
    assert!(backend.validate_model_context(&model.clone()).is_ok());
    let foreign = solve::SolveModel {
        pure_calls: quadratic_table(),
        ..Default::default()
    };
    assert_eq!(
        foreign.pure_calls, table,
        "the foreign table has identical contents"
    );
    assert!(!foreign.pure_calls.shares_table_owner(&table));
    assert!(
        backend.validate_model_context(&foreign).is_err(),
        "native code must remain bound to its original call-table owner"
    );
    let error = rumoca_solver::SolveRuntime::new_with_execution_backend(&foreign, Some(backend))
        .err()
        .expect("foreign native context must reject runtime construction");
    assert!(matches!(error, EvalSolveError::CompiledExecution { .. }));
}

#[test]
fn selected_residual_keeps_the_attached_native_pure_call_owner() {
    let table = quadratic_table();
    let backend = super::backend(&table).unwrap();
    let block = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::PureCall {
                dst_start: 1,
                input_starts: vec![0].into(),
                site: table.owners()[0].call_site(),
            },
            solve::LinearOp::StoreOutput { src: 1 },
        ]],
        span()
            .require_provenance("selected native residual")
            .unwrap(),
    )
    .unwrap();
    let native = backend.compile_selectable_expression(&block).unwrap();
    let interpreted = PreparedScalarProgramBlock::new(block).unwrap();
    for x in [3.0, -4.0] {
        let expected = interpreted
            .eval_row_output_unchecked_with_context(
                0,
                0,
                &[x],
                &[],
                0.0,
                RowEvalContext {
                    pure_calls: Some(&table),
                    ..Default::default()
                },
            )
            .unwrap();
        assert_eq!(
            native
                .call_program_output((0, 0), &[x], &[], 0.0, &[])
                .unwrap(),
            Some(expected)
        );
        let mut full = [0.0];
        native.call(&[x], &[], 0.0, &[], &mut full).unwrap();
        assert_eq!(full, [expected]);
    }
}

struct CountedExecution<'a> {
    inner: &'a dyn PureCallExecution,
    calls: Cell<usize>,
}

impl PureCallExecution for CountedExecution<'_> {
    fn executes(&self, invocation: PureCallInvocation<'_>) -> bool {
        self.inner.executes(invocation)
    }

    fn call(
        &self,
        invocation: PureCallInvocation<'_>,
        input: &[f64],
        output: &mut [f64],
    ) -> Result<(), EvalSolveError> {
        self.calls.set(self.calls.get() + 1);
        self.inner.call(invocation, input, output)
    }
}

#[test]
fn projected_target_uses_the_attached_native_call_owner() {
    let table = quadratic_table();
    let backend = super::backend(&table).unwrap();
    let execution = CountedExecution {
        inner: backend
            .pure_call_execution()
            .expect("native table compiles"),
        calls: Cell::new(0),
    };
    let block = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::PureCall {
                dst_start: 1,
                input_starts: vec![0].into(),
                site: table.owners()[0].call_site(),
            },
            solve::LinearOp::LoadY { dst: 2, index: 1 },
            solve::LinearOp::Binary {
                dst: 3,
                op: solve::BinaryOp::Sub,
                lhs: 2,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 3 },
        ]],
        span()
            .require_provenance("native target projection")
            .unwrap(),
    )
    .unwrap();
    let prepared = PreparedScalarProgramBlock::new(block).unwrap();
    for (x, expected) in [(3.0, 11.0), (4.0, 18.0)] {
        let value = prepared
            .eval_target_assignment_output_unchecked_with_context(TargetAssignmentOutputRequest {
                row_idx: 0,
                output_offset: 0,
                target_y_index: 1,
                y: &[x, 0.0],
                p: &[],
                t: 0.0,
                context: RowEvalContext {
                    pure_calls: Some(&table),
                    pure_call_execution: Some(&execution),
                    ..Default::default()
                },
            })
            .unwrap();
        assert_eq!(value, Some(expected));
    }
    assert_eq!(
        execution.calls.get(),
        2,
        "the issued native owner must execute"
    );
}

#[test]
fn native_directional_calls_agree_with_reference_for_changed_inputs_and_seeds() {
    let table = quadratic_table();
    let backend = super::backend(&table).unwrap();
    let execution = CountedExecution {
        inner: backend
            .pure_call_execution()
            .expect("native table compiles"),
        calls: Cell::new(0),
    };
    let site = table.owners()[0].call_site().directional().unwrap().clone();
    let block = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::LoadY { dst: 1, index: 1 },
            solve::LinearOp::PureCallDirectional {
                dst_start: 2,
                input_starts: vec![0, 1].into(),
                site,
            },
            solve::LinearOp::StoreOutputRange {
                start: 2,
                count: 2,
                stride: 1,
            },
        ]],
        span()
            .require_provenance("native directional call")
            .unwrap(),
    )
    .unwrap();
    let prepared = PreparedScalarProgramBlock::new(block.clone()).unwrap();
    for (inputs, expected) in [([3.0, 2.0], [11.0, 12.0]), ([4.0, -1.0], [18.0, -8.0])] {
        let reference_context = RowEvalContext {
            pure_calls: Some(&table),
            ..Default::default()
        };
        let mut reference = [0.0; 2];
        prepared
            .eval_with_context(&inputs, &[], 0.0, reference_context, &mut reference)
            .unwrap();
        assert_eq!(reference, expected);
        let native_context = RowEvalContext {
            pure_call_execution: Some(&execution),
            ..reference_context
        };
        let mut native = [0.0; 2];
        prepared
            .eval_with_context(&inputs, &[], 0.0, native_context, &mut native)
            .unwrap();
        assert_eq!(native, reference);
        rumoca_eval_solve::eval_scalar_program_block_with_context(
            &block,
            &inputs,
            &[],
            0.0,
            native_context,
            &mut native,
        )
        .unwrap();
        assert_eq!(native, reference);
    }
    assert_eq!(execution.calls.get(), 4);
}

struct FailedExecution {
    calls: Cell<usize>,
}

impl PureCallExecution for FailedExecution {
    fn executes(&self, _invocation: PureCallInvocation<'_>) -> bool {
        true
    }

    fn call(
        &self,
        _invocation: PureCallInvocation<'_>,
        _input: &[f64],
        output: &mut [f64],
    ) -> Result<(), EvalSolveError> {
        self.calls.set(self.calls.get() + 1);
        output.fill(123.0);
        Err(EvalSolveError::CompiledExecution {
            message: "injected execution failure".into(),
            span: None,
        })
    }
}

#[test]
fn native_failure_is_propagated_without_publishing_partial_outputs_or_retrying() {
    let table = quadratic_table();
    let block = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::PureCall {
                dst_start: 1,
                input_starts: vec![0].into(),
                site: table.owners()[0].call_site(),
            },
            solve::LinearOp::StoreOutput { src: 1 },
        ]],
        span()
            .require_provenance("native failure propagation")
            .unwrap(),
    )
    .unwrap();
    let prepared = PreparedScalarProgramBlock::new(block).unwrap();
    let failed = FailedExecution {
        calls: Cell::new(0),
    };
    let mut output = [0.0];
    let error = prepared
        .eval_with_context(
            &[3.0],
            &[],
            0.0,
            RowEvalContext {
                pure_calls: Some(&table),
                pure_call_execution: Some(&failed),
                ..Default::default()
            },
            &mut output,
        )
        .unwrap_err();
    assert_eq!(failed.calls.get(), 1);
    assert!(matches!(&error, EvalSolveError::CompiledExecution { .. }));
    let runtime: rumoca_solver::RuntimeSolveError = error.clone().into();
    assert!(matches!(
        &runtime,
        rumoca_solver::RuntimeSolveError::CompiledExecution { .. }
    ));
    assert_eq!(runtime.source_span(), Some(span()));
    assert!(error.to_string().contains("injected execution failure"));
    assert_eq!(error.source_span(), Some(span()));
    assert_eq!(output, [0.0]);
}

#[test]
fn a_compact_map_compiles_as_one_native_kernel_without_per_row_code() {
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: 64,
            step: 1,
        }],
    };
    let unit = vec![solve::AffineStencilIndexStrideTerm {
        dimension: 0,
        stride: 1,
    }];
    let block = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::Map {
            output_map: solve::TensorOutputMap {
                start: 0,
                strides: unit.clone(),
            },
            domain,
            base_ops: vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::Unary {
                    dst: 1,
                    op: solve::UnaryOp::Neg,
                    arg: 0,
                },
                solve::LinearOp::StoreOutput { src: 1 },
            ],
            load_strides: vec![solve::AffineStencilLoadStride {
                op_position: 0,
                terms: unit,
            }],
            const_strides: Vec::new(),
            metadata: solve::TensorNodeMetadata::default(),
            span: span(),
        }],
    };
    assert_eq!(
        super::native_compute_inventory(&block).unwrap(),
        super::NativeComputeInventory {
            kernels: 1,
            compiled_rows: 0,
        }
    );
}

#[test]
fn native_status_source_faults_remain_distinct_from_backend_failure() {
    use rumoca_exec_cranelift::{CompileError, NativeSourceFault};
    let index = super::compiled_execution_error(CompileError::SourceOperation {
        kind: NativeSourceFault::TensorIndex,
        message: "issued source index fault".into(),
        span: None,
    });
    assert!(matches!(
        index,
        rumoca_solver::RuntimeSolveError::SourceFault { .. }
    ));
    let numerical = super::compiled_execution_error(CompileError::SourceOperation {
        kind: NativeSourceFault::LinearSolve,
        message: "issued singular trial".into(),
        span: None,
    });
    assert!(matches!(
        numerical,
        rumoca_solver::RuntimeSolveError::SolveIr { .. }
    ));
    let backend =
        super::compiled_execution_error(CompileError::Backend("issued interface fault".into()));
    assert!(matches!(
        backend,
        rumoca_solver::RuntimeSolveError::CompiledExecution { .. }
    ));
}

/// Owns the quadratic call form only when `admits`; a call it receives fails.
struct GatedExecution {
    admits: bool,
    calls: Cell<usize>,
}

impl PureCallExecution for GatedExecution {
    fn executes(&self, _invocation: PureCallInvocation<'_>) -> bool {
        self.admits
    }

    fn call(
        &self,
        _invocation: PureCallInvocation<'_>,
        _input: &[f64],
        _output: &mut [f64],
    ) -> Result<(), EvalSolveError> {
        self.calls.set(self.calls.get() + 1);
        Err(EvalSolveError::CompiledExecution {
            message: "the admitted owner was called".into(),
            span: None,
        })
    }
}

fn quadratic_call_program() -> (solve::SolvePureCallTable, PreparedScalarProgramBlock) {
    let table = quadratic_table();
    let block = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::PureCall {
                dst_start: 1,
                input_starts: vec![0].into(),
                site: table.owners()[0].call_site(),
            },
            solve::LinearOp::StoreOutput { src: 1 },
        ]],
        span().require_provenance("gated owner").unwrap(),
    )
    .unwrap();
    (table, PreparedScalarProgramBlock::new(block).unwrap())
}

#[test]
fn an_owner_the_execution_does_not_admit_runs_in_the_typed_interpreter() {
    let (table, prepared) = quadratic_call_program();
    let execution = GatedExecution {
        admits: false,
        calls: Cell::new(0),
    };
    let reference_context = RowEvalContext {
        pure_calls: Some(&table),
        ..Default::default()
    };
    let mut expected = [0.0];
    prepared
        .eval_with_context(&[3.0], &[], 0.0, reference_context, &mut expected)
        .unwrap();
    let mut actual = [0.0];
    prepared
        .eval_with_context(
            &[3.0],
            &[],
            0.0,
            RowEvalContext {
                pure_call_execution: Some(&execution),
                ..reference_context
            },
            &mut actual,
        )
        .unwrap();
    assert_eq!(actual, expected);
    assert_eq!(execution.calls.get(), 0);
}

#[test]
fn an_owner_the_execution_admits_is_called_and_its_failure_surfaces() {
    let (table, prepared) = quadratic_call_program();
    let execution = GatedExecution {
        admits: true,
        calls: Cell::new(0),
    };
    let mut out = [0.0];
    let error = prepared
        .eval_with_context(
            &[3.0],
            &[],
            0.0,
            RowEvalContext {
                pure_calls: Some(&table),
                pure_call_execution: Some(&execution),
                ..Default::default()
            },
            &mut out,
        )
        .unwrap_err();
    assert!(error.to_string().contains("the admitted owner was called"));
    assert_eq!(execution.calls.get(), 1);
}

#[test]
fn compiled_nested_warning_captures_the_first_actual_loop_frame() {
    let source = r#"
function child
  input Real q;
  input Integer index;
  output Real y;
algorithm
  assert(q < 0, "frame " + String(q + index, minimumLength=5,
    leftJustified=false, significantDigits=3), level=AssertionLevel.warning);
  y := q + index;
end child;
function accumulateWarnings
  input Real q;
  output Real y;
algorithm
  y := 0;
  for i in 1:2 loop
    for j in 1:2 loop
      y := y + child(q, 10*i+j);
    end for;
  end for;
end accumulateWarnings;
model NestedWarnings
  Real x(start=0, fixed=true);
equation
  der(x) = accumulateWarnings(time);
end NestedWarnings;
"#;
    let mut compiler = rumoca_compile::Session::new(Default::default());
    compiler.update_document("nested_warning_frames.mo", source);
    let compiled = compiler.compile_model("NestedWarnings").unwrap();
    let options = crate::SimOptions {
        execution_policy: crate::SimExecutionPolicy::NativeRequired,
        solver_mode: crate::SimSolverMode::RkLike,
        dt: Some(0.05),
        t_end: 0.1,
        ..Default::default()
    };
    let session = crate::SimulationSession::new(&compiled.dae, options.clone()).unwrap();
    assert_eq!(
        session.execution_receipt().engine,
        crate::SimExecutionEngine::Cranelift
    );
    let native = crate::simulate_with_diagnostics(&compiled.dae, &options).unwrap();
    let reference = crate::simulate_with_diagnostics(
        &compiled.dae,
        &crate::SimOptions {
            execution_policy: crate::SimExecutionPolicy::Interpreter,
            ..options
        },
    )
    .unwrap();
    assert_eq!(native.names, reference.names);
    assert_eq!(native.times, reference.times);
    assert_eq!(native.data, reference.data);
    assert_eq!(native.diagnostics.len(), 1);
    assert_eq!(native.diagnostics[0].message, "frame    11");
    assert_eq!(native.diagnostics[0].time, 0.0);
    assert_eq!(
        native.diagnostics[0].message,
        reference.diagnostics[0].message
    );
}

#[test]
fn compiled_ordinary_message_numeric_options_are_dynamic_and_failed_only() {
    let source = r#"
model OrdinaryMessages
  Real x(start=0, fixed=true);
  parameter Integer width=7;
equation
  der(x)=1;
  assert(time < 0.05, "x=" + String(x, minimumLength=width,
    leftJustified=false, significantDigits=2), level=AssertionLevel.warning);
  assert(time < 1, "inactive=" + String(x, minimumLength=-1), level=AssertionLevel.warning);
end OrdinaryMessages;
"#;
    let mut compiler = rumoca_compile::Session::new(Default::default());
    compiler.update_document("ordinary_message_options.mo", source);
    let compiled = compiler.compile_model("OrdinaryMessages").unwrap();
    let options = crate::SimOptions {
        execution_policy: crate::SimExecutionPolicy::NativeRequired,
        solver_mode: crate::SimSolverMode::RkLike,
        dt: Some(0.05),
        t_end: 0.1,
        ..Default::default()
    };
    let native = crate::simulate_with_diagnostics(&compiled.dae, &options).unwrap();
    assert_eq!(native.diagnostics.len(), 1);
    assert_eq!(native.diagnostics[0].message, "x=   0.05");
}

#[test]
fn compiled_warning_can_call_an_assertion_free_recursive_value_owner() {
    let source = r#"
function sumPositive
  input Integer n;
  output Integer y;
algorithm
  y := if n <= 0 then 0 else n + sumPositive(n-1);
end sumPositive;
function warnAfterCount
  input Integer n;
  input Real q;
  output Real y;
protected
  Integer subtotal;
algorithm
  subtotal := sumPositive(n);
  assert(q < 0, "count " + String(subtotal), level=AssertionLevel.warning);
  y := subtotal + q;
end warnAfterCount;
model RecursiveValueAndWarning
  parameter Integer n=3;
  Real x(start=0, fixed=true);
equation
  der(x) = warnAfterCount(n, time);
end RecursiveValueAndWarning;
"#;
    let mut compiler = rumoca_compile::Session::new(Default::default());
    compiler.update_document("recursive_warning_value.mo", source);
    let compiled = compiler.compile_model("RecursiveValueAndWarning").unwrap();
    let options = crate::SimOptions {
        execution_policy: crate::SimExecutionPolicy::NativeRequired,
        solver_mode: crate::SimSolverMode::RkLike,
        dt: Some(0.05),
        t_end: 0.1,
        ..Default::default()
    };
    let native = crate::simulate_with_diagnostics(&compiled.dae, &options).unwrap();
    let reference = crate::simulate_with_diagnostics(
        &compiled.dae,
        &crate::SimOptions {
            execution_policy: crate::SimExecutionPolicy::Interpreter,
            ..options
        },
    )
    .unwrap();
    assert_eq!(native.times, reference.times);
    assert_eq!(native.data, reference.data);
    assert_eq!(native.diagnostics.len(), 1);
    assert_eq!(native.diagnostics[0].message, "count 6");
    assert_eq!(native.diagnostics[0].time, 0.0);
}

#[test]
fn compiled_message_option_fault_preserves_the_reference_error() {
    let mut compiler = rumoca_compile::Session::new(Default::default());
    compiler.update_document(
        "native_bad_message.mo",
        r#"
function badWarning
  input Real q;
  output Real y;
algorithm
  assert(q < 0, "value=" + String(q, minimumLength=-1), level=AssertionLevel.warning);
  y := q;
end badWarning;
model NativeMessageFault
  Real x(start=0, fixed=true);
equation
  der(x)=badWarning(time);
end NativeMessageFault;
"#,
    );
    let compiled = compiler.compile_model("NativeMessageFault").unwrap();
    let lowered = crate::lower_correlated_for_simulation_with_overrides(
        &compiled.dae,
        &rumoca_solver::SimOptions::default(),
    )
    .unwrap();
    let source = solve::SolveExecutionSource::new(lowered.model().clone());
    let model = source.model();
    let calls = rumoca_exec_cranelift::compile_pure_call_table(&model.pure_calls).unwrap();
    let mut checked = 0;
    for index in 0..model.problem.events.action_conditions.row_count() {
        let program = source
            .event_program(solve::RetainedAssertionSource::ActionConditions, index)
            .unwrap();
        if !program.has_observations() {
            continue;
        }
        let block = solve::ScalarProgramBlock::with_program_spans(
            vec![program.program().to_vec()],
            vec![program.span()],
        )
        .unwrap();
        let code = rumoca_exec_cranelift::compile_event_observation(program, &calls).unwrap();
        let observations = rumoca_eval_solve::CheckedEventObservationContext::construct(
            &model.pure_calls,
            &block,
            &model.problem.events.actions,
        )
        .unwrap();
        let y = vec![0.0; model.problem.layout.y_scalars()];
        let p = vec![0.0; model.problem.layout.p_scalars()];
        let mut out = vec![19.0; block.output_count()];
        code.call(&y, &p, -1.0, &[], &mut out).unwrap();
        out.fill(19.0);
        let error = code.call(&y, &p, 0.0, &[], &mut out).unwrap_err();
        let mut reference = out.clone();
        let expected = rumoca_eval_solve::eval_scalar_program_block_with_context(
            &block,
            &y,
            &p,
            0.0,
            RowEvalContext {
                pure_calls: Some(&model.pure_calls),
                event_observations: Some(&observations),
                ..Default::default()
            },
            &mut reference,
        )
        .unwrap_err();
        assert!(matches!(expected, EvalSolveError::InvalidRow { .. }));
        assert!(expected.to_string().contains("minimumLength"));
        assert!(expected.source_span().is_some());
        assert_eq!(error.to_string(), expected.to_string());
        assert_eq!(super::pure_call_execution_error(error), expected);
        assert_eq!(out, vec![19.0; block.output_count()]);
        assert!(
            model
                .problem
                .events
                .actions
                .iter()
                .all(|action| code.message_for_action(action).is_none())
        );
        checked += 1;
    }
    assert!(checked > 0);
}
