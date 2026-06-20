use super::*;

fn tensor_domain(count: usize) -> rumoca_core::StructuredIndexDomain {
    rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: count as i64,
            step: 1,
        }],
    }
}

fn native_family_with_base_ops(base_ops: Vec<solve::LinearOp>) -> RenderNativeAffineFamily {
    let domain = tensor_domain(2);
    let output_map =
        solve::TensorOutputMap::dense_contiguous(0, &domain).expect("valid dense output map");
    RenderNativeAffineFamily {
        kind: "map",
        domain,
        output_offset: 0,
        count: 2,
        output_map,
        base_ops,
        load_strides: Vec::new(),
        const_strides: Vec::new(),
    }
}

fn fixture_span(name: &'static str) -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(name), 5, 13)
}

fn one_by_one_matmul_node(lhs: f64, rhs: f64, span: rumoca_core::Span) -> solve::ComputeNode {
    solve::ComputeNode::MatMul {
        lhs_ops: vec![solve::LinearOp::Const { dst: 0, value: lhs }],
        lhs_start: 0,
        rhs_ops: vec![solve::LinearOp::Const { dst: 1, value: rhs }],
        rhs_start: 1,
        m: 1,
        k: 1,
        n: 1,
        lhs_sparsity: solve::SparsityPattern::Dense,
        rhs_sparsity: solve::SparsityPattern::Dense,
        metadata: solve::TensorNodeMetadata::default(),
        span,
    }
}

fn const_store_row(value: f64) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::Const { dst: 0, value },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

#[test]
fn template_partition_places_mixed_scalar_and_tensor_outputs() {
    let span = fixture_span("mixed_template_partition.mo");
    let block = solve::ComputeBlock {
        nodes: vec![
            one_by_one_matmul_node(2.0, 3.0, span),
            solve::ComputeNode::ScalarPrograms(
                solve::ScalarProgramBlock::with_output_indices(
                    vec![const_store_row(7.0)],
                    vec![span],
                    vec![3],
                )
                .expect("mixed scalar/tensor fixture metadata should match row count"),
            ),
            one_by_one_matmul_node(5.0, 11.0, span),
        ],
    };

    let partition = native_family_template_partition(&block)
        .expect("valid mixed block should partition for native template context");
    let output_indices = partition
        .scalar_fallback_rows
        .iter()
        .map(|row| row.output_index)
        .collect::<Vec<_>>();
    assert_eq!(output_indices, vec![0, 3, 4]);
}

#[test]
fn template_partition_tracks_multi_output_tensor_fallback_program() {
    let span = fixture_span("multi_output_tensor_template_partition.mo");
    let block = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::MatMul {
            lhs_ops: vec![solve::LinearOp::Const { dst: 0, value: 2.0 }],
            lhs_start: 0,
            rhs_ops: vec![solve::LinearOp::Const { dst: 1, value: 3.0 }],
            rhs_start: 1,
            m: 2,
            k: 1,
            n: 2,
            lhs_sparsity: solve::SparsityPattern::Dense,
            rhs_sparsity: solve::SparsityPattern::Dense,
            metadata: solve::TensorNodeMetadata::default(),
            span,
        }],
    };

    let partition = native_family_template_partition(&block)
        .expect("valid multi-output tensor fallback should partition");
    let rows = partition
        .scalar_fallback_rows
        .iter()
        .map(|row| (row.row_index, row.output_index, row.output_ordinal))
        .collect::<Vec<_>>();

    assert_eq!(rows, vec![(0, 0, 0), (0, 1, 1), (0, 2, 2), (0, 3, 3)]);
}

#[test]
fn template_partition_tracks_multi_output_scalar_program() {
    let span = fixture_span("multi_output_scalar_template_partition.mo");
    let block = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::with_output_indices(
                vec![vec![
                    solve::LinearOp::Const { dst: 0, value: 2.0 },
                    solve::LinearOp::Const { dst: 1, value: 3.0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                    solve::LinearOp::StoreOutput { src: 1 },
                ]],
                vec![span],
                vec![3, 4],
            )
            .expect("multi-output scalar fixture metadata should match output count"),
        )],
    };

    let partition = native_family_template_partition(&block)
        .expect("valid multi-output scalar fallback should partition");
    let rows = partition
        .scalar_fallback_rows
        .iter()
        .map(|row| (row.row_index, row.output_index, row.output_ordinal))
        .collect::<Vec<_>>();

    assert_eq!(rows, vec![(0, 3, 0), (0, 4, 1)]);
}

#[test]
fn template_partition_rejects_tensor_fallback_without_source_span() {
    let block = solve::ComputeBlock {
        nodes: vec![one_by_one_matmul_node(2.0, 3.0, rumoca_core::Span::DUMMY)],
    };

    let err = match native_family_template_partition(&block) {
        Ok(_) => panic!("template partitioning should reject unspanned tensor fallback nodes"),
        Err(err) => err,
    };

    assert_eq!(err.source_span(), None);
    assert!(
        err.to_string().contains("missing source span metadata"),
        "error should explain missing source span metadata: {err}"
    );
}

#[test]
fn template_partition_rejects_scalar_block_without_source_span() {
    let block = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::with_source_span(
                vec![const_store_row(7.0)],
                rumoca_core::Span::source_free_serde_default(),
            ),
        )],
    };

    let err = match native_family_template_partition(&block) {
        Ok(_) => panic!("template partitioning should reject unspanned scalar fallback rows"),
        Err(err) => err,
    };

    assert_eq!(err.source_span(), None);
    assert!(
        err.to_string().contains("missing source span metadata"),
        "error should explain missing source span metadata: {err}"
    );
}

#[test]
fn scalar_program_block_source_span_skips_dummy_first_row() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("render_scalar_source.mo"),
        17,
        29,
    );
    let block = solve::ScalarProgramBlock::with_program_spans(
        vec![const_store_row(1.0), const_store_row(2.0)],
        vec![rumoca_core::Span::DUMMY, span],
    )
    .expect("render scalar span fixture metadata should match row count");

    assert_eq!(
        scalar_program_block_source_span(&block).expect("source span"),
        span
    );
    assert_eq!(scalar_program_row_span(&block, 0, span), span);
    assert_eq!(scalar_program_row_span(&block, 1, span), span);
}

#[test]
fn wgsl_kernel_schedule_entry_count_rejects_overflow() {
    let err = wgsl_kernel_schedule_entry_count(usize::MAX, 1, 1, false)
        .expect_err("native plus scalar schedule entries should reject host overflow")
        .to_string();

    assert!(err.contains("WGSL kernel schedule entry count overflows host index range"));
}

#[test]
fn scalar_kernel_chunk_count_rejects_zero_workgroup_size() {
    let err = scalar_kernel_chunk_count(1, 0)
        .expect_err("zero workgroup size should be rejected defensively")
        .to_string();

    assert!(err.contains("workgroup_size must be greater than zero"));
}

#[test]
fn wgsl_kernel_workgroup_total_rejects_zero_workgroup_size() {
    let err = wgsl_kernel_workgroup_total(&[], &[], 0)
        .expect_err("zero workgroup size should be rejected defensively")
        .to_string();

    assert!(err.contains("workgroup_size must be greater than zero"));
}

#[test]
fn kernel_workgroup_count_rejects_overflow() {
    let err = kernel_workgroup_count(usize::MAX, 2, "test kernel")
        .expect_err("overflowing workgroup count should fail")
        .to_string();

    assert!(err.contains("test kernel workgroup count overflows host index range"));
}

#[test]
fn structured_binder_value_count_accepts_descending_domain() {
    let binder = rumoca_core::StructuredIndexBinder {
        id: 0,
        display_name: "i".to_string(),
        lower: 5,
        upper: 1,
        step: -2,
    };

    assert_eq!(binder.value_count_for_render().expect("valid count"), 3);
}

#[test]
fn structured_binder_value_count_rejects_extent_overflow() {
    let binder = rumoca_core::StructuredIndexBinder {
        id: 0,
        display_name: "i".to_string(),
        lower: i64::MIN,
        upper: i64::MAX,
        step: 1,
    };

    let err = binder
        .value_count_for_render()
        .expect_err("overflowing structured domain extent should fail")
        .to_string();

    assert!(err.contains("structured domain extent overflows"));
}

#[test]
fn structured_binder_value_count_rejects_step_magnitude_overflow() {
    let binder = rumoca_core::StructuredIndexBinder {
        id: 0,
        display_name: "i".to_string(),
        lower: 1,
        upper: 1,
        step: i64::MIN,
    };

    let err = binder
        .value_count_for_render()
        .expect_err("i64::MIN negative step magnitude should fail")
        .to_string();

    assert!(err.contains("structured domain step magnitude overflows"));
}

#[test]
fn native_family_inventory_rejects_invalid_domain_metadata() {
    let mut family =
        native_family_with_base_ops(vec![solve::LinearOp::Const { dst: 0, value: 1.0 }]);
    family.domain.binders[0].step = 0;
    let native_families = Value::from_object(SolveNativeFamiliesValue::new(vec![family]));

    let err = render_wgsl_native_family_inventory_json_function(native_families)
        .expect_err("invalid native family domain metadata should fail inventory rendering")
        .to_string();

    assert!(err.contains("structured domain step cannot be zero"));
}

#[test]
fn matmul_render_shape_rejects_output_count_overflow() {
    let shape = MatMulRenderShape {
        lhs_start: 0,
        rhs_start: 0,
        m: usize::MAX,
        k: 1,
        n: 2,
        offset: 0,
    };

    let err = shape
        .output_count()
        .expect_err("MatMul output count should reject host index overflow")
        .to_string();

    assert!(err.contains("MatMul output count overflows host index range"));
}

#[test]
fn linsolve_render_shape_rejects_matrix_count_overflow() {
    let shape = LinSolveRenderShape {
        matrix_start: 0,
        rhs_start: 0,
        n: usize::MAX,
        output_offset: 0,
    };

    let err = shape
        .matrix_count()
        .expect_err("LinSolve matrix count should reject host index overflow")
        .to_string();

    assert!(err.contains("LinSolve matrix element count overflows host index range"));
}

#[test]
fn solve_read_counts_cover_write_only_registers() {
    let op = Value::from_serialize(solve::LinearOp::Const { dst: 5, value: 1.0 });
    let counts = collect_solve_read_counts(&[op]).expect("read counts should stage");

    assert_eq!(counts.len(), 6);
    assert_eq!(counts[5], 0);
}

#[test]
fn linsolve_read_regs_rejects_matrix_count_overflow() {
    let op = Value::from_serialize(solve::LinearOp::LinearSolveComponent {
        dst: 0,
        matrix_start: 0,
        rhs_start: 0,
        n: usize::MAX,
        component: 0,
    });

    let err = solve_op_read_regs(&op)
        .expect_err("LinSolve read-register analysis should reject host overflow")
        .to_string();

    assert!(err.contains("LinearSolveComponent read matrix count overflows host index range"));
}

#[test]
fn solve_output_targets_accept_explicit_indices() {
    let targets = solve_output_targets(Some(Value::from_serialize(vec![3usize, 1])))
        .expect("explicit output indices should parse");

    assert_eq!(targets.target_for(0).expect("first target"), 3);
    assert_eq!(targets.target_for(1).expect("second target"), 1);
}

#[test]
fn solve_block_output_count_counts_all_store_outputs() {
    let programs = Value::from_serialize(serde_json::json!([
        [
            {"Const": {"dst": 0, "value": 1.0}},
            {"StoreOutput": {"src": 0}},
            {"StoreOutput": {"src": 0}}
        ],
        [
            {"Const": {"dst": 1, "value": 2.0}},
            {"StoreOutput": {"src": 1}}
        ]
    ]));

    let count =
        solve_block_output_count_function(programs).expect("StoreOutput count should render");

    assert_eq!(count, 3);
}

#[test]
fn solve_program_render_rejects_temp_counter_overflow() {
    let op = Value::from_serialize(solve::LinearOp::Binary {
        dst: 2,
        op: solve::BinaryOp::Add,
        lhs: 0,
        rhs: 1,
    });
    let cfg = SolveRowCConfig::from_value(&Value::from_serialize(()));
    let mut body = String::new();
    let mut temp_counter = usize::MAX;
    let mut output_ordinal = 0usize;
    let output_targets = SolveOutputTargets::DenseOffset(0);
    let mut render = SolveProgramRender {
        body: &mut body,
        regs: vec!["a".to_string(), "b".to_string()],
        temp_counter: &mut temp_counter,
        output_ordinal: &mut output_ordinal,
        output_targets: &output_targets,
    };

    let err = render
        .render_op(&op, &cfg, SolveRowDialect::C, "out[{}] = {}", &[0, 0, 1])
        .expect_err("temporary counter overflow should fail before emitting");

    assert!(
        err.to_string()
            .contains("temporary register count overflow")
    );
    assert!(body.is_empty());
}

#[test]
fn solve_program_render_rejects_output_ordinal_overflow() {
    let op = Value::from_serialize(solve::LinearOp::StoreOutput { src: 0 });
    let cfg = SolveRowCConfig::from_value(&Value::from_serialize(()));
    let mut body = String::new();
    let mut temp_counter = 0usize;
    let mut output_ordinal = usize::MAX;
    let output_targets = SolveOutputTargets::DenseOffset(0);
    let mut render = SolveProgramRender {
        body: &mut body,
        regs: vec!["value".to_string()],
        temp_counter: &mut temp_counter,
        output_ordinal: &mut output_ordinal,
        output_targets: &output_targets,
    };

    let err = render
        .render_op(&op, &cfg, SolveRowDialect::C, "out[{}] = {}", &[1])
        .expect_err("output ordinal overflow should fail before emitting");

    assert!(err.to_string().contains("StoreOutput ordinal overflows"));
    assert!(body.is_empty());
}

#[test]
fn native_family_stride_error_reports_op_kind() {
    let mut family =
        native_family_with_base_ops(vec![solve::LinearOp::Const { dst: 0, value: 1.0 }]);
    family.load_strides = vec![solve::AffineStencilLoadStride {
        op_position: 0,
        terms: vec![solve::AffineStencilIndexStrideTerm {
            dimension: 0,
            stride: 1,
        }],
    }];

    let err = render_native_family_expr_wgsl(&family, &Value::from_serialize(()))
        .expect_err("non-load stride target should fail rendering")
        .to_string();

    assert!(err.contains("native family stride targets a non-load op: Const"));
    assert!(!err.contains("Const {"));
}

#[test]
fn native_family_const_stride_error_reports_op_kind() {
    let mut family = native_family_with_base_ops(vec![solve::LinearOp::LoadY { dst: 0, index: 0 }]);
    family.const_strides = vec![solve::AffineStencilConstStride {
        op_position: 0,
        terms: vec![solve::AffineStencilConstStrideTerm {
            dimension: 0,
            stride: 1.0,
        }],
    }];

    let err = render_native_family_expr_wgsl(&family, &Value::from_serialize(()))
        .expect_err("non-const stride target should fail rendering")
        .to_string();

    assert!(err.contains("native family const stride targets a non-const op: LoadY"));
    assert!(!err.contains("LoadY {"));
}

#[test]
fn render_solve_row_output_wgsl_selects_store_output_ordinal() {
    let cfg = SolveRowCConfig::from_value(&Value::from_serialize(()));
    let row = Value::from_serialize(vec![
        solve::LinearOp::Const { dst: 0, value: 2.0 },
        solve::LinearOp::Const { dst: 1, value: 3.0 },
        solve::LinearOp::StoreOutput { src: 0 },
        solve::LinearOp::StoreOutput { src: 1 },
    ]);

    let first = render_solve_row_output_for(&row, 0, &cfg, SolveRowDialect::Wgsl)
        .expect("first StoreOutput should render");
    let second = render_solve_row_output_for(&row, 1, &cfg, SolveRowDialect::Wgsl)
        .expect("second StoreOutput should render");

    assert_ne!(first, second);
    assert!(first.contains("2.0"), "first output should use r0: {first}");
    assert!(
        second.contains("3.0"),
        "second output should use r1: {second}"
    );
}

#[test]
fn native_family_override_error_reports_op_kind() {
    let overrides = std::collections::HashMap::from([(0usize, "42.0".to_string())]);
    let cfg = SolveRowCConfig::from_value(&Value::from_serialize(()));
    let ops = vec![solve::LinearOp::Binary {
        dst: 0,
        op: solve::BinaryOp::Add,
        lhs: 0,
        rhs: 0,
    }];

    let err = render_solve_row_typed_with_overrides(&ops, &overrides, &cfg, SolveRowDialect::Wgsl)
        .expect_err("override on non-load op should fail rendering")
        .to_string();

    assert!(err.contains("native family override targets a non-load/non-const op: Binary"));
    assert!(!err.contains("Binary {"));
}

#[test]
fn unsupported_typed_op_error_reports_op_kind() {
    let cfg = SolveRowCConfig::from_value(&Value::from_serialize(()));
    let ops = vec![solve::LinearOp::TableNextEvent {
        dst: 0,
        table_id: 0,
        time: 1,
    }];

    let err = render_solve_row_typed_with_overrides(
        &ops,
        &std::collections::HashMap::new(),
        &cfg,
        SolveRowDialect::Wgsl,
    )
    .expect_err("unsupported typed op should fail rendering")
    .to_string();

    assert!(err.contains("unsupported solve LinearOp: TableNextEvent"));
    assert!(!err.contains("TableNextEvent {"));
}
