//! Checked compact calls evaluate changing coordinates without scalar memo reuse.
use super::*;
use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};

pub(super) fn table(failing: bool) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let real_type = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(12000),
            vec![real_type.clone()],
            vec![
                solve::SolvePureCallOutput::result(real_type),
                solve::SolvePureCallOutput::result(boolean),
            ],
            span(12000),
            |b, inputs, outputs| {
                let input = b.load(inputs[0], span(12001))?;
                let value = if failing {
                    let integer = b.convert(
                        solve::SolveConversionOperator::RealToIntegerTowardZero,
                        input,
                        span(12002),
                    )?;
                    b.convert(
                        solve::SolveConversionOperator::IntegerToReal,
                        integer,
                        span(12003),
                    )?
                } else {
                    input
                };
                let zero = b.constant(solve::SolveValue::real(p, 0.), span(12004))?;
                let equal =
                    b.compare(solve::SolveCompareOperator::Equal, input, zero, span(12005))?;
                b.store(outputs[0], value, span(12006))?;
                b.store(outputs[1], equal, span(12007))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

fn family(
    count: usize,
    target: usize,
    input: solve::LinearOp,
    site: &solve::SolvePureCallSite,
    value: solve::Reg,
) -> solve::ComputeNode {
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: count as i64,
            step: 1,
        }],
    };
    let terms = vec![solve::AffineStencilIndexStrideTerm {
        dimension: 0,
        stride: 1,
    }];
    solve::ComputeNode::Map {
        output_map: solve::TensorOutputMap::dense_contiguous(target, &domain).unwrap(),
        domain,
        base_ops: vec![
            solve::LinearOp::LoadY {
                dst: 0,
                index: target,
            },
            input,
            solve::LinearOp::PureCall {
                dst_start: 2,
                input_starts: vec![1].into_boxed_slice(),
                site: site.clone(),
            },
            solve::LinearOp::Unary {
                dst: 4,
                op: solve::UnaryOp::Neg,
                arg: 2,
            },
            solve::LinearOp::Select {
                dst: 5,
                cond: 3,
                if_true: 1,
                if_false: 4,
            },
            solve::LinearOp::Binary {
                dst: 6,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: value,
            },
            solve::LinearOp::StoreOutput { src: 6 },
        ],
        load_strides: vec![
            solve::AffineStencilLoadStride {
                op_position: 0,
                terms: terms.clone(),
            },
            solve::AffineStencilLoadStride {
                op_position: 1,
                terms,
            },
        ],
        const_strides: vec![],
        metadata: solve::TensorNodeMetadata::default(),
        span: span(12010),
    }
}

fn fixture(
    count: usize,
    failing: bool,
    stencil: bool,
) -> (
    solve::NativeRefreshAssignmentSchedule,
    solve::VarLayout,
    solve::SolvePureCallTable,
    solve::SolvePureCallSite,
) {
    let (table, site) = table(failing);
    let value = if failing { 1 } else { 5 };
    let layout = solve::VarLayout::from_parts(Default::default(), 2 * count, count);
    let mut source = solve::ComputeBlock {
        nodes: vec![
            family(
                count,
                count,
                solve::LinearOp::LoadY { dst: 1, index: 0 },
                &site,
                value,
            ),
            family(
                count,
                0,
                solve::LinearOp::LoadP { dst: 1, index: 0 },
                &site,
                value,
            ),
        ],
    };
    if stencil {
        source.nodes = source
            .nodes
            .into_iter()
            .map(|node| {
                let solve::ComputeNode::Map {
                    domain,
                    output_map,
                    base_ops,
                    load_strides,
                    const_strides,
                    metadata,
                    span,
                } = node
                else {
                    panic!("map")
                };
                solve::ComputeNode::AffineStencil {
                    domain,
                    output_map,
                    base_ops,
                    load_strides,
                    const_strides,
                    metadata,
                    span,
                }
            })
            .collect();
    }
    let targets = (0..2 * count)
        .map(|i| Some(solve::scalar_slot_y(i)))
        .collect::<Vec<_>>();
    let mut owners = solve::ContinuousRefreshOwners::default();
    owners
        .issue_native_assignment_schedule(&source, &targets, &layout)
        .unwrap();
    (
        owners.native_assignment_schedule().unwrap().clone(),
        layout,
        table,
        site,
    )
}

fn expected(table: &solve::SolvePureCallTable, site: &solve::SolvePureCallSite, input: f64) -> f64 {
    let bytes = oracle(table, site, &[vec![real(input)]]).unwrap();
    let value = f64::from_le_bytes(bytes[..8].try_into().unwrap());
    let equal = u64::from_le_bytes(bytes[8..16].try_into().unwrap()) != 0;
    if equal { input } else { -value }
}

#[test]
fn compact_map_and_stencil_calls_match_complete_canonical_real_boolean_coordinates() {
    for stencil in [false, true] {
        let (schedule, layout, table, site) = fixture(14_400, false, stencil);
        assert_eq!(
            schedule
                .stages()
                .iter()
                .map(|s| s.source_node())
                .collect::<Vec<_>>(),
            [1, 0]
        );
        let compiled =
            compile_native_assignment_schedule_with_calls_wasm(&schedule, &layout, &table).unwrap();
        let mut runner = ProgramRunner::new(&compiled, &layout);
        let samples = [
            0.,
            -0.,
            1.,
            -2.5,
            f64::INFINITY,
            f64::NEG_INFINITY,
            f64::from_bits(1),
            f64::from_bits(0x7ff8_dead_beef_1234),
        ];
        for frame in 0..2 {
            let parameters = (0..14_400)
                .map(|i| samples[(i + frame) % samples.len()])
                .collect::<Vec<_>>();
            let (status, output) = runner.run(&parameters);
            assert_eq!(status, 0);
            for (i, value) in parameters.into_iter().enumerate() {
                let first = expected(&table, &site, value);
                assert_eq!(output[i].to_bits(), first.to_bits(), "first {i}");
                assert_eq!(
                    output[14_400 + i].to_bits(),
                    expected(&table, &site, first).to_bits(),
                    "second {i}"
                );
            }
        }
    }
}

#[test]
fn later_compact_call_fault_retains_source_and_rolls_back_full_y_then_recovers() {
    let (schedule, layout, table, site) = fixture(17, true, false);
    // Call results are unused by these assignments. Its original unconditional
    // prefix still must execute and fail before publishing any target value.
    let compiled =
        compile_native_assignment_schedule_with_calls_wasm(&schedule, &layout, &table).unwrap();
    let fault = compiled
        .faults()
        .iter()
        .find(|f| f.kind == TypedCallFaultKind::IntegerConversion)
        .unwrap();
    assert_eq!(fault.provenance, span(12002));
    let mut runner = ProgramRunner::new(&compiled, &layout);
    let mut parameters = (0..17).map(|i| i as f64 + 0.25).collect::<Vec<_>>();
    let valid = parameters.clone();
    parameters[16] = f64::INFINITY;
    let (status, _) = runner.run(&parameters);
    assert_eq!(status as u32, fault.status);
    assert!(oracle(&table, &site, &[vec![real(parameters[16])]]).is_err());
    let (status, output) = runner.run(&valid);
    assert_eq!(status, 0);
    for (i, value) in valid.into_iter().enumerate() {
        oracle(&table, &site, &[vec![real(value)]]).unwrap();
        assert_eq!(output[i].to_bits(), value.to_bits());
        assert_eq!(output[17 + i].to_bits(), value.to_bits());
    }
}
