use super::*;
use crate::native_execution::{
    CraneliftComputeJacobian, CraneliftJacobianExpression, compiled_execution_error,
};
use rumoca_solver::CompiledSolveJacobianExpression;
use std::rc::Rc;

#[test]
fn selected_native_jvp_retains_its_source_seed_contract_and_lazy_original_fault() {
    let first = vec![
        solve::LinearOp::LoadSeed { dst: 0, index: 0 },
        solve::LinearOp::StoreOutput { src: 0 },
        solve::LinearOp::Const {
            dst: 1,
            value: -0.0,
        },
        solve::LinearOp::StoreOutput { src: 1 },
    ];
    let second = vec![
        solve::LinearOp::Const { dst: 0, value: 7.0 },
        solve::LinearOp::LoadY { dst: 1, index: 0 },
        solve::LinearOp::LoadIndexedRegister {
            dst: 2,
            base: 0,
            stride: 1,
            dimensions: vec![1].into(),
            indices: vec![solve::TensorIndex::Runtime(1)].into(),
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ];
    let source =
        solve::ScalarProgramBlock::with_program_spans(vec![first, second], vec![span(); 2])
            .unwrap();
    let code = Rc::new(CraneliftJacobianExpression(
        rumoca_exec_cranelift::compile_jacobian_scalar_program_block(&source).unwrap(),
    ));
    let foreign: solve::ScalarProgramBlock =
        serde_json::from_value(serde_json::to_value(&source).unwrap()).unwrap();
    assert!(code.bind_program(&foreign, 0).is_err());
    let first = code.bind_program(&source, 0).unwrap().unwrap();
    assert!(Rc::clone(&first).programs_committing().is_none());
    let second = code.bind_program(&source, 1).unwrap().unwrap();
    drop(code);
    drop(source);
    let mut tuple = [19.0; 2];
    for seed in [3.0_f64, -0.0] {
        first
            .call(&[2.0], &[], 0.0, &[seed], &[], &mut tuple)
            .unwrap();
        assert_eq!(
            tuple.map(f64::to_bits),
            [seed.to_bits(), (-0.0_f64).to_bits()]
        );
    }
    let before = tuple.map(f64::to_bits);
    assert!(first.call(&[2.0], &[], 0.0, &[], &[], &mut tuple).is_err());
    assert_eq!(tuple.map(f64::to_bits), before);
    let mut scalar = [19.0];
    let fault = second
        .call(&[2.0], &[], 0.0, &[1.0], &[], &mut scalar)
        .unwrap_err();
    assert_eq!(fault.source_span(), Some(span()));
    assert_eq!(scalar, [19.0]);
    second
        .call(&[1.0], &[], 0.0, &[1.0], &[], &mut scalar)
        .unwrap();
    assert_eq!(scalar, [7.0]);
}

/// A native refusal inside the committing entry reaches the solver through
/// `From<CompileError> for SolveFailure`, mapped exactly as every other native
/// compile error is, and nothing is committed.
#[test]
fn a_native_refusal_in_the_committing_entry_keeps_the_compile_error_mapping() {
    let program = vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ];
    let source =
        solve::ScalarProgramBlock::with_program_spans(vec![program], vec![span()]).unwrap();
    let code = Rc::new(CraneliftJacobianExpression(
        rumoca_exec_cranelift::compile_jacobian_scalar_program_block(&source).unwrap(),
    ));
    let raw = code
        .0
        .call_selected_programs_committing::<rumoca_exec_cranelift::CompileError>(
            &[0],
            (&[], &[], 0.0),
            &mut [],
            &[],
            &mut |_, _, _| panic!("a refused block commits nothing"),
        )
        .unwrap_err();
    let error = code
        .programs_committing()
        .unwrap()
        .call_selected_programs_committing(&[0], (&[], &[], 0.0), &mut [], &[], &mut |_, _, _| {
            panic!("a refused block commits nothing")
        })
        .unwrap_err();
    assert_eq!(
        format!("{error:?}"),
        format!("{:?}", compiled_execution_error(raw))
    );
}

#[test]
fn compact_native_jvp_issues_only_whole_block_execution() {
    let unit = vec![solve::AffineStencilIndexStrideTerm {
        dimension: 0,
        stride: 1,
    }];
    let primal = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::Map {
            output_map: solve::TensorOutputMap {
                start: 0,
                strides: unit.clone(),
            },
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    provenance: rumoca_core::IndexBinderProvenance::Generated,
                    id: 0,
                    display_name: "i".into(),
                    lower: 1,
                    upper: 3,
                    step: 1,
                }],
            },
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
    let directional = rumoca_eval_solve::ad::lower_compute_block_jvp(&primal).unwrap();
    let compiled = rumoca_exec_cranelift::compile_jacobian_compute_block(&directional, None)
        .unwrap()
        .unwrap();
    assert_eq!(compiled.kernel_count(), 1);
    let code = Rc::new(CraneliftComputeJacobian(compiled));
    assert!(Rc::clone(&code).programs_committing().is_none());
    let entry: &dyn CompiledSolveJacobianExpression = code.as_ref();
    let mut output = [19.0; 3];
    entry
        .call(
            &[1.0, 2.0, 3.0],
            &[],
            0.0,
            &[2.0, 3.0, 5.0],
            &[],
            &mut output,
        )
        .unwrap();
    assert_eq!(output, [-2.0, -3.0, -5.0]);
}

#[test]
fn scalar_native_selection_commits_in_list_order_and_skips_unlisted_programs() {
    let source = solve::ScalarProgramBlock::with_program_spans(
        vec![
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 1 },
                solve::LinearOp::Unary {
                    dst: 1,
                    op: solve::UnaryOp::Neg,
                    arg: 0,
                },
                solve::LinearOp::StoreOutput { src: 1 },
            ],
            vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::TableBounds {
                    dst: 1,
                    table_id: 0,
                    max: true,
                },
                solve::LinearOp::StoreOutput { src: 1 },
            ],
        ],
        vec![span(); 3],
    )
    .unwrap();
    let code = Rc::new(CraneliftJacobianExpression(
        rumoca_exec_cranelift::compile_jacobian_scalar_program_block(&source).unwrap(),
    ));
    let entry = code.programs_committing().unwrap();
    let mut seed = [2.0, -3.0];
    let mut commits = Vec::new();
    entry
        .call_selected_programs_committing(
            &[1, 0, 1],
            (&[], &[], 0.0),
            &mut seed,
            &[],
            &mut |program, values, seed| {
                commits.push((program, values[0]));
                seed[if program == 0 { 1 } else { 0 }] = values[0];
                Ok(())
            },
        )
        .unwrap();
    assert_eq!(commits, [(1, 3.0), (0, 3.0), (1, -3.0)]);
    assert_eq!(seed, [-3.0, 3.0]);
    let before = seed.map(f64::to_bits);
    assert!(
        entry
            .call_selected_programs_committing(
                &[0, 3],
                (&[], &[], 0.0),
                &mut seed,
                &[],
                &mut |_, _, _| panic!("an invalid list executes and commits nothing"),
            )
            .is_err()
    );
    assert_eq!(seed.map(f64::to_bits), before);
}
