//! Negative controls for the compiled-only row boundary; source only, not run here.
use super::*;

fn scalar_block(y_index: usize) -> ScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("compiled_only_row.mo"),
        0,
        1,
    );
    ScalarProgramBlock::with_source_span(
        vec![vec![
            LinearOp::LoadY {
                dst: 0,
                index: y_index,
            },
            LinearOp::StoreOutput { src: 0 },
        ]],
        span.require_provenance("compiled-only row fixture")
            .unwrap(),
    )
    .unwrap()
}

#[test]
fn scalar_row_refusal_precedes_operand_and_output_validation() {
    let prepared = PreparedScalarProgramBlock::new(scalar_block(7)).unwrap();
    let mut out = [37.0, 41.0];
    let error = prepared
        .eval_with_context(&[], &[], 0.0, RowEvalContext::default(), &mut out)
        .unwrap_err();
    assert!(matches!(error, EvalSolveError::CompiledExecution { .. }));
    assert_eq!(out, [37.0, 41.0]);
    assert!(matches!(
        crate::eval_row_with_context(&[], &[], &[], 0.0, RowEvalContext::default()),
        Err(EvalSolveError::CompiledExecution { .. })
    ));
}

#[test]
fn replacement_retains_validation_and_refreshes_row_requirements() {
    let base = PreparedScalarProgramBlock::new(scalar_block(1)).unwrap();
    let target = scalar_block(7);
    assert!(
        PreparedScalarProgramBlock::with_replaced_programs(&base, target.clone(), &[]).is_err()
    );
    let replaced = PreparedScalarProgramBlock::with_replaced_programs(&base, target, &[0]).unwrap();
    assert_eq!(base.requirements.y_len, 2);
    assert_eq!(replaced.requirements.y_len, 8);
    assert!(!replaced.row_reads_y(0, 1));
    assert!(replaced.row_reads_y(0, 7));
    assert_eq!(
        replaced.row_requirements.len(),
        replaced.block.programs().len()
    );
    assert_eq!(
        replaced.row_execution.len(),
        replaced.block.programs().len()
    );
    assert_eq!(
        replaced.row_assignment_shapes.len(),
        replaced.block.programs().len()
    );
}

#[test]
fn shared_compute_reuse_retains_scalar_nodes_and_new_requirements() {
    let base_source = ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(scalar_block(1))],
    };
    let target = ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(scalar_block(7))],
    };
    let base = PreparedComputeBlock::new_with_label(&base_source, "base").unwrap();
    let reused = PreparedComputeBlock::with_shared_programs(&base, &target, "target").unwrap();
    assert_eq!(base.requirements.y_len, 2);
    assert_eq!(reused.requirements.y_len, 8);
    let [PreparedComputeNode::ScalarPrograms(programs)] = reused.nodes.as_slice() else {
        panic!("shared construction must retain its scalar owner");
    };
    assert!(programs.row_reads_y(0, 7));
    let mut out = vec![43.0];
    assert!(matches!(
        reused.eval_with_context(&[], &[], 0.0, RowEvalContext::default(), &mut out),
        Err(EvalSolveError::CompiledExecution { .. })
    ));
    assert_eq!(out, vec![43.0]);
}

#[test]
fn compiled_lazy_eligibility_is_checked_and_refreshes_on_replacement() {
    let branch = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::Const { dst: 2, value: 3.0 },
        LinearOp::Select {
            dst: 3,
            cond: 0,
            if_true: 1,
            if_false: 2,
        },
        LinearOp::StoreOutput { src: 3 },
    ];
    assert!(crate::lazy_row_plan_eligible(&branch));
    assert!(!crate::lazy_row_plan_eligible(&branch[3..]));
    let mut unsupported = branch.clone();
    unsupported[2] = LinearOp::LoadSeed { dst: 2, index: 0 };
    assert!(!crate::lazy_row_plan_eligible(&unsupported));
    let base = PreparedScalarProgramBlock::new(scalar_block(1)).unwrap();
    assert!(!base.has_lazy_row_plan(0));
    assert!(!base.has_lazy_row_plan(1));
    let source =
        ScalarProgramBlock::with_program_spans(vec![branch], base.block().program_spans().to_vec())
            .unwrap();
    let replaced = PreparedScalarProgramBlock::with_replaced_programs(&base, source, &[0]).unwrap();
    assert!(replaced.has_lazy_row_plan(0));
    assert!(replaced.clone().has_lazy_row_plan(0));
    assert!(!base.has_lazy_row_plan(0));
}
