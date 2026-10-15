use super::*;
use rumoca_ir_solve as solve;
use rumoca_solver::CompiledSolveJacobianExpression;
use std::num::NonZeroU64;

fn span(offset: usize) -> rumoca_core::Span {
    solve::source_span_from_offsets(391, offset, offset + 1)
}

fn profile() -> solve::SolveArithmeticProfile {
    solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    )
}

fn nested_table(count: u32) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    nested_table_with_profile(count, profile())
}

fn nested_table_with_profile(
    count: u32,
    p: solve::SolveArithmeticProfile,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let array =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![count]).unwrap();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let inputs = vec![array, integer, boolean, real];
    let outputs = inputs
        .iter()
        .cloned()
        .map(solve::SolvePureCallOutput::result)
        .collect::<Vec<_>>();
    let mut table = solve::SolvePureCallTable::builder(p);
    let child = table
        .add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(391).unwrap()),
            inputs.clone(),
            outputs.clone(),
            span(1),
            |b, inputs, outputs| {
                let real = b.load(inputs[0], span(2))?;
                let squared =
                    b.binary(solve::SolveBinaryOperator::Multiply, real, real, span(3))?;
                b.store(outputs[0], squared, span(5))?;
                let scalar = b.load(inputs[3], span(8))?;
                let sine = b.unary(solve::SolveUnaryOperator::Sin, scalar, span(9))?;
                b.store(outputs[3], sine, span(10))?;
                for index in 1..3 {
                    let value = b.load(inputs[index], span(6))?;
                    b.store(outputs[index], value, span(7))?;
                }
                Ok(())
            },
        )
        .unwrap();
    let parent = table
        .add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(392).unwrap()),
            inputs,
            outputs,
            span(11),
            |b, inputs, outputs| {
                let arguments = inputs
                    .iter()
                    .map(|slot| b.load(*slot, span(12)))
                    .collect::<Result<Vec<_>, _>>()?;
                let values = b.call(child, &arguments, span(13))?;
                for (output, value) in outputs.iter().zip(values) {
                    b.store(*output, value, span(14))?;
                }
                Ok(())
            },
        )
        .unwrap();
    let site = table.call_site(parent).unwrap();
    (table.finish(), site)
}

fn nested_source(site: &solve::SolvePureCallSite, count: u32) -> ScalarProgramBlock {
    let tangent = count + 3;
    let discrete = 2 * count + 7;
    let result = discrete + 5;
    let primal = result + 2 * count + 7;
    let mut operations = Vec::new();
    for index in 0..count {
        operations.push(LinearOp::LoadY {
            dst: index,
            index: index as usize,
        });
        operations.push(LinearOp::LoadSeed {
            dst: tangent + index,
            index: index as usize,
        });
    }
    operations.extend([
        LinearOp::LoadP {
            dst: discrete,
            index: 0,
        },
        LinearOp::LoadP {
            dst: discrete + 1,
            index: 1,
        },
        LinearOp::LoadY {
            dst: discrete + 2,
            index: count as usize,
        },
        LinearOp::LoadSeed {
            dst: discrete + 3,
            index: count as usize,
        },
        LinearOp::PureCall {
            dst_start: primal,
            input_starts: vec![0, discrete, discrete + 1, discrete + 2].into(),
            site: site.clone(),
        },
        LinearOp::PureCallDirectional {
            dst_start: result,
            input_starts: vec![
                0,
                tangent,
                discrete,
                discrete + 1,
                discrete + 2,
                discrete + 3,
            ]
            .into(),
            site: site.directional().unwrap().clone(),
        },
        LinearOp::StoreOutputRange {
            start: result,
            count: (2 * count + 4) as usize,
            stride: 1,
        },
        LinearOp::StoreOutputRange {
            start: primal,
            count: (count + 3) as usize,
            stride: 1,
        },
    ]);
    ScalarProgramBlock::with_program_spans(vec![operations], vec![span(20)]).unwrap()
}

#[test]
fn emitted_nested_directional_calls_preserve_aggregate_leaf_order_and_changed_seeds() {
    for count in [2, 128] {
        let (table, site) = nested_table(count);
        let source = nested_source(&site, count);
        let layout = VarLayout::from_parts(Default::default(), count as usize + 1, 2);
        let code = WasmJacobian::compile(&source, &layout, &table).unwrap();
        let reference = rumoca_eval_solve::PreparedScalarProgramBlock::new(source.clone()).unwrap();
        let y = (0..count)
            .map(|index| {
                if index == 0 {
                    -0.0
                } else {
                    f64::from(index) / 16.0
                }
            })
            .chain(std::iter::once(0.0))
            .collect::<Vec<_>>();
        for factor in [0.0, -0.0, 2.0, -3.0] {
            let seed = (0..=count)
                .map(|index| factor * f64::from(index + 1))
                .collect::<Vec<_>>();
            let p = [-7.0, 1.0];
            let mut expected = vec![0.0; source.output_count()];
            reference
                .eval_with_context(
                    &y,
                    &p,
                    0.0,
                    rumoca_eval_solve::RowEvalContext {
                        seed: Some(&seed),
                        pure_calls: Some(&table),
                        ..Default::default()
                    },
                    &mut expected,
                )
                .unwrap();
            let mut actual = vec![19.0; expected.len()];
            code.call(&y, &p, 0.0, &seed, &[], &mut actual).unwrap();
            assert_eq!(
                actual
                    .iter()
                    .map(|value| value.to_bits())
                    .collect::<Vec<_>>(),
                expected
                    .iter()
                    .map(|value| value.to_bits())
                    .collect::<Vec<_>>()
            );
        }
        let mut output = vec![19.0; source.output_count()];
        assert!(
            code.call(
                &y,
                &[-7.5, 1.0],
                0.0,
                &vec![1.0; count as usize + 1],
                &[],
                &mut output
            )
            .is_err()
        );
        assert!(output.iter().all(|value| *value == 19.0));
        let seed = vec![1.0; count as usize + 1];
        let p = [-7.0, 2.0];
        let mut expected = vec![0.0; source.output_count()];
        reference
            .eval_with_context(
                &y,
                &p,
                0.0,
                rumoca_eval_solve::RowEvalContext {
                    seed: Some(&seed),
                    pure_calls: Some(&table),
                    ..Default::default()
                },
                &mut expected,
            )
            .unwrap();
        code.call(&y, &p, 0.0, &seed, &[], &mut output).unwrap();
        assert_eq!(
            output
                .iter()
                .map(|value| value.to_bits())
                .collect::<Vec<_>>(),
            expected
                .iter()
                .map(|value| value.to_bits())
                .collect::<Vec<_>>()
        );
    }
}

#[test]
fn directional_call_admission_rejects_a_foreign_typed_interface_and_missing_arguments() {
    let (table, site) = nested_table(2);
    let foreign_profile = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::construct(-128, 127).unwrap(),
    );
    let (_, foreign) = nested_table_with_profile(2, foreign_profile);
    let mut program = nested_source(&site, 2).programs()[0].clone();
    for operation in &mut program {
        if let LinearOp::PureCallDirectional { site, .. } = operation {
            *site = foreign.directional().unwrap().clone();
        }
    }
    let source = ScalarProgramBlock::with_program_spans(vec![program], vec![span(21)]).unwrap();
    let layout = VarLayout::from_parts(Default::default(), 3, 2);
    assert!(WasmJacobian::compile(&source, &layout, &table).is_err());
    let mut program = nested_source(&site, 2).programs()[0].clone();
    for operation in &mut program {
        if let LinearOp::PureCallDirectional { input_starts, .. } = operation {
            *input_starts = vec![0].into();
        }
    }
    assert!(ScalarProgramBlock::with_program_spans(vec![program], vec![span(22)]).is_err());
}

#[test]
fn emitted_directional_source_fault_preserves_span_and_commits_no_partial_tuple() {
    let p = profile();
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(393).unwrap()),
            vec![real.clone()],
            vec![solve::SolvePureCallOutput::result(real)],
            span(30),
            |b, inputs, outputs| {
                let real = b.load(inputs[0], span(31))?;
                let integer = b.convert(
                    solve::SolveConversionOperator::RealToIntegerTowardZero,
                    real,
                    span(32),
                )?;
                let result = b.convert(
                    solve::SolveConversionOperator::IntegerToReal,
                    integer,
                    span(33),
                )?;
                b.store(outputs[0], result, span(34))
            },
        )
        .unwrap();
    let site = table
        .call_site(owner)
        .unwrap()
        .directional()
        .unwrap()
        .clone();
    let table = table.finish();
    let source = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::Const {
                dst: 0,
                value: -0.0,
            },
            LinearOp::StoreOutput { src: 0 },
            LinearOp::LoadY { dst: 1, index: 0 },
            LinearOp::LoadSeed { dst: 2, index: 0 },
            LinearOp::PureCallDirectional {
                dst_start: 3,
                input_starts: vec![1, 2].into(),
                site,
            },
            LinearOp::StoreOutput { src: 3 },
            LinearOp::StoreOutput { src: 4 },
        ]],
        vec![span(35)],
    )
    .unwrap();
    let code = WasmJacobian::compile(
        &source,
        &VarLayout::from_parts(Default::default(), 1, 0),
        &table,
    )
    .unwrap();
    let mut output = [19.0; 3];
    let error = code
        .call(&[f64::NAN], &[], 0.0, &[1.0], &[], &mut output)
        .unwrap_err();
    assert!(
        matches!(&error, rumoca_solver::RuntimeSolveError::SolveIr { .. }),
        "{error:?}"
    );
    assert_eq!(error.source_span(), Some(span(32)));
    assert_eq!(output, [19.0; 3]);
    code.call(&[-3.75], &[], 0.0, &[2.0], &[], &mut output)
        .unwrap();
    assert_eq!(output[0].to_bits(), (-0.0f64).to_bits());
    assert_eq!(&output[1..], &[-3.0, 0.0]);
}
