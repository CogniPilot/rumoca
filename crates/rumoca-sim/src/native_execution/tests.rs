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

#[test]
fn selected_residual_keeps_the_attached_native_pure_call_owner() {
    let table = quadratic_table();
    let backend = super::backend(&table);
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
    let backend = super::backend(&table);
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
    let backend = super::backend(&table);
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

struct FailedExecution;

impl PureCallExecution for FailedExecution {
    fn call(
        &self,
        _invocation: PureCallInvocation<'_>,
        _input: &[f64],
        output: &mut [f64],
    ) -> Result<(), EvalSolveError> {
        output.fill(123.0);
        Err(EvalSolveError::InvalidRow {
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
    let mut output = [0.0];
    let error = prepared
        .eval_with_context(
            &[3.0],
            &[],
            0.0,
            RowEvalContext {
                pure_calls: Some(&table),
                pure_call_execution: Some(&FailedExecution),
                ..Default::default()
            },
            &mut output,
        )
        .unwrap_err();
    assert!(error.to_string().contains("injected execution failure"));
    assert_eq!(error.source_span(), Some(span()));
    assert_eq!(output, [0.0]);
}
