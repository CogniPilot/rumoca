//! Scalar LinearOp imports and reachable typed imports share exact function indices.
use super::*;

#[test]
fn scalar_and_nested_typed_math_share_catalog_relocation_and_atomic_y_recovery() {
    let (table, site) = suite_typed_calls::math::nested_math();
    let layout = solve::VarLayout::from_parts(Default::default(), 2, 1);
    let programs = (0..2)
        .map(|target| {
            let mut ops = vec![
                solve::LinearOp::LoadY {
                    dst: 0,
                    index: target,
                },
                solve::LinearOp::LoadP { dst: 1, index: 0 },
                solve::LinearOp::PureCall {
                    dst_start: 2,
                    input_starts: vec![1].into_boxed_slice(),
                    site: site.clone(),
                },
            ];
            let value = if target == 0 {
                ops.push(solve::LinearOp::Unary {
                    dst: 4,
                    op: solve::UnaryOp::Sin,
                    arg: 1,
                });
                ops.push(solve::LinearOp::Binary {
                    dst: 5,
                    op: solve::BinaryOp::Add,
                    lhs: 2,
                    rhs: 4,
                });
                5
            } else {
                3
            };
            ops.push(solve::LinearOp::Binary {
                dst: 6,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: value,
            });
            ops.push(solve::LinearOp::StoreOutput { src: 6 });
            ops
        })
        .collect();
    let source = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::with_program_spans(programs, vec![span(11200); 2]).unwrap(),
        )],
    };
    let mut owners = solve::ContinuousRefreshOwners::default();
    owners
        .issue_native_assignment_schedule(
            &source,
            &[Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))],
            &layout,
        )
        .unwrap();
    let compiled = compile_native_assignment_schedule_with_calls_wasm(
        owners.native_assignment_schedule().unwrap(),
        &layout,
        &table,
    )
    .unwrap();
    assert_eq!(compiled.math_imports(), ["sin", "cos", "log", "pow"]);
    let mut runner = ProgramRunner::new(&compiled, &layout);
    for value in [0.5, f64::INFINITY, f64::NAN, -0.0, 2.0] {
        let (status, output) = runner.run(&[value]);
        match oracle(&table, &site, &[vec![real(value)]]) {
            Ok(expected) => {
                assert_eq!(status, 0);
                let math = f64::from_le_bytes(expected[..8].try_into().unwrap());
                let integer = i64::from_le_bytes(expected[8..16].try_into().unwrap());
                let expected = [math + value.sin(), integer as f64];
                for (actual, expected) in output.into_iter().zip(expected) {
                    assert_eq!(actual.to_bits(), expected.to_bits());
                }
            }
            Err(_) => assert!(status > 0),
        }
    }
}
