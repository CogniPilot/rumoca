//! Immutable owner facts and explicit reference execution retain source coordinates.
use super::*;

fn range_request(
    start: usize,
    len: usize,
    out: &mut Vec<f64>,
) -> solve_eval::ComputeNodeOutputRangeRequest<'_> {
    solve_eval::ComputeNodeOutputRangeRequest {
        start,
        len,
        y: &[2.0, -0.0],
        p: &[],
        t: 0.0,
        context: RowEvalContext::default(),
        out,
    }
}

#[test]
fn admitted_fixture_selection_decline_is_an_error_with_the_original_source() {
    let source = spanned_block(vec![const_visible_value_row(3.0)], "fixture-decline.mo");
    let whole = super::super::test_kernels::expression(&source).unwrap();
    let owner = prepare_mock_primal(source, whole, false).unwrap();
    let mut output = vec![-0.0];
    let error = owner
        .eval_program_outputs(
            0,
            RowEvalPoint {
                y: &[],
                p: &[],
                t: 0.0,
            },
            RowEvalContext::default(),
            &mut output,
        )
        .unwrap_err();
    assert_eq!(error.source_span(), Some(test_span("fixture-decline.mo")));
    assert!(
        error
            .to_string()
            .contains("accepted fixture kernel declined numeric execution")
    );
    assert_eq!(output[0].to_bits(), (-0.0f64).to_bits());
}

#[test]
fn reference_bulk_selection_keeps_local_offsets_and_commits_only_complete_results() {
    use solve::LinearOp as L;
    let source = solve::ScalarProgramBlock::with_output_indices(
        vec![
            vec![
                L::Const {
                    dst: 0,
                    value: -0.0,
                },
                L::LoadY { dst: 1, index: 0 },
                L::StoreOutputRange {
                    start: 0,
                    count: 2,
                    stride: 1,
                },
            ],
            vec![
                L::LoadTime { dst: 0 },
                L::TensorLoad {
                    dst_start: 1,
                    input: solve::TensorInputKind::P,
                    input_start: 0,
                    count: 2,
                    seed_start: None,
                    lanes: 1,
                },
                L::LoadIndexedRegister {
                    dst: 3,
                    base: 1,
                    stride: 1,
                    dimensions: vec![2].into(),
                    indices: vec![solve::TensorIndex::Runtime(0)].into(),
                },
                L::StoreOutput { src: 3 },
            ],
        ],
        vec![test_span("bulk-reference.mo"); 2],
        vec![4, 1, 3],
    )
    .unwrap();
    let owner = PrimalProgram::prepare("bulk reference", source, None, None).unwrap();
    let context = RowEvalContext::default();
    let mut output = [73.0; 4];
    let coordinates = [(1, 0), (0, 1), (0, 0), (1, 0)];
    owner
        .eval_outputs_at(
            &coordinates,
            RowEvalPoint {
                y: &[11.0],
                p: &[7.0, 19.0],
                t: 2.0,
            },
            context,
            &mut output,
        )
        .unwrap();
    assert_eq!(
        output.map(f64::to_bits),
        [19.0, 11.0, -0.0, 19.0].map(f64::to_bits)
    );
    let before = output.map(f64::to_bits);
    for coordinates in [
        &[(0, 0)][..],
        &[(0, 0), (0, 1), (1, 1), (0, 0)][..],
        &[(0, 0), (0, 1), (2, 0), (0, 0)][..],
    ] {
        assert!(
            owner
                .eval_outputs_at(
                    coordinates,
                    RowEvalPoint {
                        y: &[11.0],
                        p: &[7.0, 19.0],
                        t: 1.0
                    },
                    context,
                    &mut output
                )
                .is_err()
        );
        assert_eq!(output.map(f64::to_bits), before);
    }
    let error = owner
        .eval_outputs_at(
            &coordinates,
            RowEvalPoint {
                y: &[11.0],
                p: &[7.0, 19.0],
                t: 3.0,
            },
            context,
            &mut output,
        )
        .unwrap_err();
    assert_eq!(error.source_span(), Some(test_span("bulk-reference.mo")));
    assert_eq!(output.map(f64::to_bits), before);
}

#[test]
fn reference_compact_range_uses_its_owned_compute_node_and_declines_missing_ranges() {
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: 2,
            step: 1,
        }],
    };
    let compute = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::Map {
            output_map: solve::TensorOutputMap::dense_contiguous(0, &domain).unwrap(),
            domain,
            base_ops: vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: vec![solve::AffineStencilLoadStride {
                op_position: 0,
                terms: vec![solve::AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 1,
                }],
            }],
            const_strides: vec![],
            metadata: solve::TensorNodeMetadata::default(),
            span: test_span("compact-range.mo"),
        }],
    };
    let source = to_scalar_program_block(&compute).unwrap();
    let owner =
        PrimalProgram::prepare_compact("reference range", source, &compute, None, None).unwrap();
    let reference = owner.reference().unwrap();
    let mut output = vec![83.0];
    assert_eq!(
        reference
            .eval_node_covering_output_range_with_context(range_request(0, 2, &mut output))
            .unwrap(),
        Some(true)
    );
    assert_eq!(
        output.iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
        [2.0, -0.0].map(f64::to_bits)
    );
    assert_eq!(
        reference
            .eval_node_covering_output_range_with_context(range_request(2, 1, &mut output))
            .unwrap(),
        Some(false)
    );
}

#[test]
fn owner_analysis_retains_exact_tensor_reads_and_assignment_shape() {
    let source = spanned_block(
        vec![
            vec![
                solve::LinearOp::LoadY { dst: 0, index: 4 },
                solve::LinearOp::TensorLoad {
                    dst_start: 1,
                    input: solve::TensorInputKind::Y,
                    input_start: 1,
                    count: 3,
                    seed_start: None,
                    lanes: 1,
                },
                solve::LinearOp::StoreOutputRange {
                    start: 0,
                    count: 4,
                    stride: 1,
                },
            ],
            const_visible_value_row(7.0),
        ],
        "analysis-reads.mo",
    );
    let owner = PrimalProgram::prepare("source facts", source.clone(), None, None).unwrap();
    let facts = owner.source();
    assert!(facts.shares_program_owner(&source));
    assert!(facts.row_reads_y(0, 2));
    assert!(facts.row_reads_y(0, 4));
    assert!(!facts.row_reads_y(0, 0));
    assert!(!facts.row_reads_y(7, 2));
    // A direct Y residual owns the canonical Zero isolator for that Y slot.
    assert!(facts.row_has_assignment_shape(0));
    assert!(facts.certifies_exact_target_assignment_output(0, 0, 4));
    assert!(!facts.row_has_assignment_shape(1));
    assert_eq!(
        facts.nonfinite_y_reads(0).unwrap().collect::<Vec<_>>(),
        [4, 1, 2, 3]
    );
    assert_eq!(facts.nonfinite_y_reads(1).unwrap().count(), 0);
    assert!(facts.nonfinite_y_reads(2).is_none());
    let kinds = facts
        .reverse_row_unsupported_op_kinds(0)
        .collect::<Vec<_>>();
    assert!(kinds.contains(&"TensorLoad"));
    assert!(facts.reverse_row_unsupported_op_kinds(1).next().is_none());
    assert!(facts.reverse_row_unsupported_op_kinds(2).next().is_none());
}

#[test]
fn mode_parameter_queries_include_all_lazy_regions_without_executing_them() {
    let region = |index| {
        vec![
            solve::LinearOp::LoadP { dst: 0, index },
            solve::LinearOp::StoreOutput { src: 0 },
        ]
    };
    let conditional =
        solve::FunctionConditionalProgram::checked(0, vec![1], [(region(2), region(3))], region(4))
            .unwrap();
    let source = spanned_block(
        vec![vec![
            solve::LinearOp::FunctionConditional {
                dst_start: 0,
                capture_start: 0,
                program: std::sync::Arc::new(conditional),
            },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "mode-reads.mo",
    );
    let owner = PrimalProgram::prepare("mode dependencies", source, None, None).unwrap();
    assert_eq!(owner.source().mode_parameter_reads(0), Some(vec![2, 3, 4]));
    assert_eq!(owner.source().mode_parameter_reads(1), None);
}
