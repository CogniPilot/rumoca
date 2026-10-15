use super::*;
use rumoca_eval_solve::JacobianEvalInputs;
use rumoca_ir_solve as solve;

#[test]
fn bound_numeric_and_directional_index_faults_keep_original_program_spans_and_recover() {
    let source = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::Const { dst: 0, value: 7.0 },
            LinearOp::LoadY { dst: 1, index: 0 },
            LinearOp::LoadIndexedRegister {
                dst: 2,
                base: 0,
                stride: 1,
                dimensions: vec![1].into(),
                indices: vec![solve::TensorIndex::Runtime(1)].into(),
            },
            LinearOp::StoreOutput { src: 2 },
        ]],
        vec![fixture_span()],
    )
    .unwrap();
    let primal = compile_selectable_expression_scalar_program_block(&source, None)
        .unwrap()
        .bind_program(&source, 0)
        .unwrap()
        .unwrap();
    let directional = compile_jacobian_scalar_program_block(&source)
        .unwrap()
        .bind_program(&source, 0)
        .unwrap();
    let mut output = [19.0];
    let error = primal.call(&[2.0], &[], 0.0, &[], &mut output).unwrap_err();
    assert!(
        matches!(error, CompileError::SourceOperation { kind: NativeSourceFault::TensorIndex, span: Some(span), .. } if span == fixture_span())
    );
    assert_eq!(output, [19.0]);
    let error = directional
        .call(
            JacobianEvalInputs {
                y: &[2.0],
                p: &[],
                t: 0.0,
                seed: &[],
            },
            &[],
            &mut output,
        )
        .unwrap_err();
    assert!(
        matches!(error, CompileError::SourceOperation { kind: NativeSourceFault::TensorIndex, span: Some(span), .. } if span == fixture_span())
    );
    assert_eq!(output, [19.0]);
    assert!(matches!(
        primal.call(&[], &[], 0.0, &[], &mut output),
        Err(CompileError::Input(_))
    ));
    primal.call(&[1.0], &[], 0.0, &[], &mut output).unwrap();
    assert_eq!(output, [7.0]);
    directional
        .call(
            JacobianEvalInputs {
                y: &[1.0],
                p: &[],
                t: 0.0,
                seed: &[],
            },
            &[],
            &mut output,
        )
        .unwrap();
    assert_eq!(output, [7.0]);
}

fn indexed_owner() -> solve::SolvePureCallTable {
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
    let tensor =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(arithmetic), vec![2]).unwrap();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(arithmetic));
    let inner = rumoca_core::Span::from_offsets(fixture_span().source, 23, 29);
    solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![tensor, integer],
            vec![solve::SolvePureCallOutput::result(real)],
            inner,
            |body, inputs, outputs| {
                let aggregate = body.load(inputs[0], inner)?;
                let index = body.load(inputs[1], inner)?;
                let result = body.project_element_dynamic(aggregate, &[index], inner)?;
                body.store(outputs[0], result, inner)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn bound_program_fallback_never_replaces_an_exact_callee_operation_span() {
    let table = indexed_owner();
    let calls = compile_pure_call_table(&table).unwrap();
    let inner = table.owners()[0].provenance();
    assert_ne!(inner, fixture_span());
    let source = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::LoadY { dst: 1, index: 1 },
            LinearOp::LoadY { dst: 2, index: 2 },
            LinearOp::PureCall {
                dst_start: 3,
                input_starts: vec![0, 2].into(),
                site: table.owners()[0].call_site(),
            },
            LinearOp::StoreOutput { src: 3 },
        ]],
        vec![fixture_span()],
    )
    .unwrap();
    let compiled =
        compile_selectable_expression_scalar_program_block(&source, Some(&calls)).unwrap();
    let mut output = [19.0];
    let CompileError::SourceOperation {
        kind,
        message,
        span: Some(span),
    } = compiled
        .call(&[7.0, 11.0, 3.0], &[], 0.0, &mut output)
        .unwrap_err()
    else {
        panic!("callee reports its exact source operation");
    };
    assert_eq!(span, inner);
    let primal = compiled.bind_program(&source, 0).unwrap().unwrap();
    let jvp = compile_jacobian_scalar_program_block_with_pure_calls(&source, &calls)
        .unwrap()
        .bind_program(&source, 0)
        .unwrap();
    for error in [
        primal
            .call(&[7.0, 11.0, 3.0], &[], 0.0, &[], &mut output)
            .unwrap_err(),
        jvp.call(
            JacobianEvalInputs {
                y: &[7.0, 11.0, 3.0],
                p: &[],
                t: 0.0,
                seed: &[],
            },
            &[],
            &mut output,
        )
        .unwrap_err(),
    ] {
        assert!(
            matches!(error, CompileError::SourceOperation { kind: actual, message: text, span: Some(span) } if actual == kind && span == inner && text == message)
        );
        assert_eq!(output, [19.0]);
    }
    primal
        .call(&[7.0, 11.0, 2.0], &[], 0.0, &[], &mut output)
        .unwrap();
    assert_eq!(output, [11.0]);
}
