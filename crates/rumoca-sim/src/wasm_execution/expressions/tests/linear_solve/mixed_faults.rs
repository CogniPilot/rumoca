use super::*;
use rumoca_ir_solve as solve;

fn call_table() -> (solve::SolvePureCallTable, solve::SolvePureCallSite, Span) {
    let fault_span = solve::source_span_from_offsets(448, 31, 33);
    let profile = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile));
    let mut builder = solve::SolvePureCallTable::builder(profile);
    let owner = builder
        .add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(448).unwrap()),
            vec![real.clone()],
            vec![solve::SolvePureCallOutput::result(real)],
            span(),
            |body, input, output| {
                let value = body.load(input[0], span())?;
                let integer = body.convert(
                    solve::SolveConversionOperator::RealToIntegerTowardZero,
                    value,
                    fault_span,
                )?;
                let real = body.convert(
                    solve::SolveConversionOperator::IntegerToReal,
                    integer,
                    span(),
                )?;
                body.store(output[0], real, span())
            },
        )
        .unwrap();
    let site = builder.call_site(owner).unwrap();
    (builder.finish(), site, fault_span)
}

fn gather(dst: u32, index: u32) -> LinearOp {
    LinearOp::LoadIndexedRegister {
        dst,
        base: 1,
        stride: 1,
        dimensions: vec![1].into(),
        indices: vec![solve::TensorIndex::Runtime(index)].into(),
    }
}

fn mixed_source(site: solve::SolvePureCallSite) -> ScalarProgramBlock {
    let mut operations = vec![
        LinearOp::Const {
            dst: 0,
            value: -0.0,
        },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::Const {
            dst: 1,
            value: 13.0,
        },
        LinearOp::LoadY { dst: 2, index: 0 },
        gather(3, 2),
        LinearOp::StoreOutput { src: 3 },
    ];
    operations.extend((0..6).map(|index| LinearOp::LoadY {
        dst: 4 + index as u32,
        index: 1 + index,
    }));
    operations.extend([
        LinearOp::LinearSolveComponent {
            dst: 10,
            matrix_start: 4,
            rhs_start: 8,
            n: 2,
            component: 0,
        },
        LinearOp::StoreOutput { src: 10 },
        LinearOp::LoadY { dst: 11, index: 7 },
        LinearOp::PureCall {
            dst_start: 12,
            input_starts: vec![11].into(),
            site,
        },
        LinearOp::StoreOutput { src: 12 },
        LinearOp::LoadY { dst: 13, index: 8 },
        gather(14, 13),
        LinearOp::StoreOutput { src: 14 },
    ]);
    ScalarProgramBlock::with_program_spans(vec![operations], vec![span()]).unwrap()
}

#[test]
fn mixed_gather_dense_and_typed_helper_faults_keep_distinct_statuses_and_atomic_outputs() {
    let (table, site, typed_span) = call_table();
    let source = mixed_source(site);
    let code = WasmExpression::compile(
        &source,
        &VarLayout::from_parts(Default::default(), 9, 0),
        &table,
    )
    .unwrap();
    let valid = [1.0, 1.0, 2.0, 3.0, 7.0, 5.0, 11.0, -3.75, 1.0];
    let mut expected = [0.0; 5];
    rumoca_eval_solve::PreparedScalarProgramBlock::new(source.clone())
        .unwrap()
        .eval_with_context(
            &valid,
            &[],
            0.0,
            rumoca_eval_solve::RowEvalContext {
                pure_calls: Some(&table),
                ..Default::default()
            },
            &mut expected,
        )
        .unwrap();
    let mut output = [19.0; 5];
    let cases = [
        (0, 2.0, span(), true),
        (1, f64::NAN, span(), false),
        (7, f64::NAN, typed_span, false),
        (8, 0.0, span(), true),
        (1, f64::INFINITY, span(), false),
        (8, f64::NAN, span(), true),
    ];
    for (index, value, expected_span, source_fault) in cases {
        let mut y = valid;
        y[index] = value;
        let before = bits(&output);
        let error = code.call(&y, &[], 0.0, &[], &mut output).unwrap_err();
        assert_eq!(error.source_span(), Some(expected_span));
        assert_eq!(
            matches!(error, rumoca_solver::RuntimeSolveError::SourceFault { .. }),
            source_fault
        );
        assert_eq!(bits(&output), before);
        code.call(&valid, &[], 0.0, &[], &mut output).unwrap();
        assert_eq!(bits(&output), bits(&expected));
    }
}
