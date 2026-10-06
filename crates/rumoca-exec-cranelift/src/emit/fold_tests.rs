use super::*;

#[test]
fn compiled_function_fold_executes_a_retained_native_loop() {
    let row = vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::FunctionFold {
            dst_start: 1,
            initial_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(
                rumoca_ir_solve::FunctionFoldProgram::checked(
                    rumoca_core::StructuredIndexDomain {
                        binders: vec![rumoca_core::StructuredIndexBinder {
                            id: 0,
                            display_name: "i".to_string(),
                            lower: 1,
                            upper: 4,
                            step: 1,
                        }],
                    },
                    1,
                    0,
                    vec![
                        LinearOp::LoadFoldCarried { dst: 0, index: 0 },
                        LinearOp::LoadFoldIndex {
                            dst: 1,
                            dimension: 0,
                        },
                        LinearOp::Binary {
                            dst: 2,
                            op: BinaryOp::Add,
                            lhs: 0,
                            rhs: 1,
                        },
                        LinearOp::StoreOutput { src: 2 },
                    ],
                )
                .expect("construct compact fold"),
            ),
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile compact fold");
    let mut out = [0.0];

    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate compact fold");

    assert_eq!(out, [10.0]);
}

#[test]
fn compiled_function_fold_ends_at_its_first_false_continuation() {
    // `s := s + i` over `i in 1:100` while `s < 10`: 0, 1, 3, 6, 10, then stop.
    let program = rumoca_ir_solve::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 100,
                step: 1,
            }],
        },
        1,
        0,
        vec![
            LinearOp::LoadFoldCarried { dst: 0, index: 0 },
            LinearOp::LoadFoldIndex {
                dst: 1,
                dimension: 0,
            },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ],
    )
    .and_then(|program| {
        program.with_continuation(vec![
            LinearOp::LoadFoldCarried { dst: 0, index: 0 },
            LinearOp::Const {
                dst: 1,
                value: 10.0,
            },
            LinearOp::Compare {
                dst: 2,
                op: rumoca_ir_solve::CompareOp::Lt,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ])
    })
    .expect("construct continued fold");
    let row = vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::FunctionFold {
            dst_start: 1,
            initial_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(program),
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile continued fold");
    let mut out = [0.0];
    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate continued fold");
    assert_eq!(out, [10.0]);
}

#[test]
fn compiled_function_fold_executes_matrix_multiply_as_a_native_loop() {
    let mut update = Vec::new();
    for index in 0..8 {
        update.push(LinearOp::LoadFoldCarried {
            dst: index as u32,
            index,
        });
    }
    update.push(LinearOp::MatrixMultiply {
        dst_start: 8,
        lhs_start: 0,
        rhs_start: 4,
        rows: 2,
        inner: 2,
        columns: 2,
        lanes: 1,
    });
    for src in 0..12 {
        update.push(LinearOp::StoreOutput { src });
    }
    let program = std::sync::Arc::new(
        rumoca_ir_solve::FunctionFoldProgram::checked(
            rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 1,
                    step: 1,
                }],
            },
            12,
            0,
            update,
        )
        .expect("construct matrix fold"),
    );
    let mut row = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::Const { dst: 2, value: 3.0 },
        LinearOp::Const { dst: 3, value: 4.0 },
        LinearOp::Const { dst: 4, value: 5.0 },
        LinearOp::Const { dst: 5, value: 6.0 },
        LinearOp::Const { dst: 6, value: 7.0 },
        LinearOp::Const { dst: 7, value: 8.0 },
    ];
    for dst in 8..12 {
        row.push(LinearOp::Const { dst, value: 0.0 });
    }
    row.push(LinearOp::FunctionFold {
        dst_start: 12,
        initial_start: 0,
        capture_start: 0,
        program,
    });
    for src in 20..24 {
        row.push(LinearOp::StoreOutput { src });
    }

    let compiled = compile_residual_rows(&[row]).expect("compile matrix fold");
    let mut out = [0.0; 4];
    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate matrix fold");

    assert_eq!(out, [19.0, 22.0, 43.0, 50.0]);
}

#[test]
fn compiled_function_fold_executes_tensor_binary_as_a_native_loop() {
    let mut update = Vec::new();
    for index in 0..10 {
        update.push(LinearOp::LoadFoldCarried {
            dst: index as u32,
            index,
        });
    }
    update.push(LinearOp::TensorBinary {
        dst_start: 6,
        op: BinaryOp::Mul,
        lhs_start: 0,
        rhs_start: 4,
        count: 2,
        lhs_stride: 1,
        rhs_stride: 0,
        lanes: 2,
    });
    for src in 0..10 {
        update.push(LinearOp::StoreOutput { src });
    }
    let program = std::sync::Arc::new(
        rumoca_ir_solve::FunctionFoldProgram::checked(
            rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 1,
                    step: 1,
                }],
            },
            10,
            0,
            update,
        )
        .expect("construct tensor-binary fold"),
    );
    let initial = [1.0, 10.0, 2.0, 20.0, 3.0, 30.0, 0.0, 0.0, 0.0, 0.0];
    let mut row = initial
        .into_iter()
        .enumerate()
        .map(|(dst, value)| LinearOp::Const {
            dst: dst as u32,
            value,
        })
        .collect::<Vec<_>>();
    row.push(LinearOp::FunctionFold {
        dst_start: 10,
        initial_start: 0,
        capture_start: 0,
        program,
    });
    for src in 16..20 {
        row.push(LinearOp::StoreOutput { src });
    }

    let compiled = compile_residual_rows(&[row]).expect("compile tensor-binary fold");
    let mut out = [0.0; 4];
    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate tensor-binary fold");

    assert_eq!(out, [3.0, 60.0, 6.0, 120.0]);
}

#[test]
fn compiled_tensor_cross_matches_interleaved_dual_semantics() {
    let inputs = [1.0, 0.1, 2.0, 0.2, 3.0, 0.3, 4.0, 0.4, 5.0, 0.5, 6.0, 0.6];
    let mut row = inputs
        .into_iter()
        .enumerate()
        .map(|(dst, value)| LinearOp::Const {
            dst: dst as u32,
            value,
        })
        .collect::<Vec<_>>();
    row.push(LinearOp::TensorCross {
        dst_start: 12,
        lhs_start: 0,
        rhs_start: 6,
        lanes: 2,
    });
    row.push(LinearOp::StoreOutputRange {
        start: 12,
        count: 6,
        stride: 1,
    });

    let compiled = compile_residual_rows(&[row]).expect("compile tensor cross product");
    let mut out = [0.0; 6];
    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate tensor cross product");

    let expected = [-3.0, -0.6, 6.0, 1.2, -3.0, -0.6];
    for (actual, expected) in out.into_iter().zip(expected) {
        assert!((actual - expected).abs() < 1e-12, "{actual} != {expected}");
    }
}

#[test]
fn compiled_tensor_division_matches_interleaved_dual_semantics() {
    let inputs = [4.0, 1.0, 6.0, 2.0, 2.0, 0.5];
    let mut row = inputs
        .into_iter()
        .enumerate()
        .map(|(dst, value)| LinearOp::Const {
            dst: dst as u32,
            value,
        })
        .collect::<Vec<_>>();
    row.push(LinearOp::TensorBinary {
        dst_start: 6,
        op: BinaryOp::Div,
        lhs_start: 0,
        rhs_start: 4,
        count: 2,
        lhs_stride: 1,
        rhs_stride: 0,
        lanes: 2,
    });
    row.push(LinearOp::StoreOutputRange {
        start: 6,
        count: 4,
        stride: 1,
    });

    let compiled = compile_residual_rows(&[row]).expect("compile tensor division");
    let mut out = [0.0; 4];
    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate tensor division");

    assert_eq!(out, [2.0, 0.0, 3.0, 0.25]);
}

#[test]
fn compiled_function_fold_executes_rank_three_tensor_transpose_as_a_native_loop() {
    let mut update = Vec::new();
    for index in 0..24 {
        update.push(LinearOp::LoadFoldCarried {
            dst: index as u32,
            index,
        });
    }
    update.push(LinearOp::TensorTranspose {
        dst_start: 12,
        src_start: 0,
        rows: 3,
        columns: 2,
        element_width: 2,
        lanes: 1,
    });
    for src in 0..24 {
        update.push(LinearOp::StoreOutput { src });
    }
    let program = std::sync::Arc::new(
        rumoca_ir_solve::FunctionFoldProgram::checked(
            rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 1,
                    step: 1,
                }],
            },
            24,
            0,
            update,
        )
        .expect("construct tensor-transpose fold"),
    );
    let mut row = (0..12)
        .map(|dst| LinearOp::Const {
            dst,
            value: f64::from(dst + 1),
        })
        .collect::<Vec<_>>();
    for dst in 12..24 {
        row.push(LinearOp::Const { dst, value: 0.0 });
    }
    row.push(LinearOp::FunctionFold {
        dst_start: 24,
        initial_start: 0,
        capture_start: 0,
        program,
    });
    for src in 36..48 {
        row.push(LinearOp::StoreOutput { src });
    }

    let compiled = compile_residual_rows(&[row]).expect("compile tensor-transpose fold");
    let mut out = [0.0; 12];
    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate tensor-transpose fold");

    assert_eq!(
        out,
        [
            1.0, 2.0, 7.0, 8.0, 3.0, 4.0, 9.0, 10.0, 5.0, 6.0, 11.0, 12.0
        ]
    );
}

#[test]
fn compiled_function_fold_executes_tensor_concatenation_as_a_native_loop() {
    let mut update = Vec::new();
    for index in 0..20 {
        update.push(LinearOp::LoadFoldCarried {
            dst: index as u32,
            index,
        });
    }
    update.push(LinearOp::TensorConcatenate {
        dst_start: 8,
        sources: vec![
            rumoca_ir_solve::TensorConcatenateSource {
                start: 0,
                dimensions: vec![2, 1].into_boxed_slice(),
            },
            rumoca_ir_solve::TensorConcatenateSource {
                start: 4,
                dimensions: vec![2, 1].into_boxed_slice(),
            },
        ]
        .into_boxed_slice(),
        dimensions: vec![2, 2].into_boxed_slice(),
        axis: 1,
        lanes: 2,
    });
    for src in 0..20 {
        update.push(LinearOp::StoreOutput { src });
    }
    let program = std::sync::Arc::new(
        rumoca_ir_solve::FunctionFoldProgram::checked(
            rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 1,
                    step: 1,
                }],
            },
            20,
            0,
            update,
        )
        .expect("construct tensor-concatenation fold"),
    );
    let initial = [1.0, 10.0, 2.0, 20.0, 3.0, 30.0, 4.0, 40.0];
    let mut row = initial
        .into_iter()
        .enumerate()
        .map(|(dst, value)| LinearOp::Const {
            dst: dst as u32,
            value,
        })
        .collect::<Vec<_>>();
    for dst in 8..20 {
        row.push(LinearOp::Const { dst, value: 0.0 });
    }
    row.push(LinearOp::FunctionFold {
        dst_start: 20,
        initial_start: 0,
        capture_start: 0,
        program,
    });
    for src in 28..36 {
        row.push(LinearOp::StoreOutput { src });
    }

    let compiled = compile_residual_rows(&[row]).expect("compile tensor-concatenation fold");
    let mut out = [0.0; 8];
    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate tensor-concatenation fold");

    assert_eq!(out, [1.0, 10.0, 3.0, 30.0, 2.0, 20.0, 4.0, 40.0]);
}

#[test]
fn compiled_function_fold_executes_tensor_update_without_scalar_selection() {
    let mut update = Vec::new();
    for index in 0..21 {
        update.push(LinearOp::LoadFoldCarried {
            dst: index as u32,
            index,
        });
    }
    update.push(LinearOp::TensorUpdate {
        dst_start: 13,
        base_start: 0,
        value_start: 8,
        dimensions: vec![2, 2].into_boxed_slice(),
        subscripts: vec![
            rumoca_ir_solve::TensorUpdateSubscript::Index(rumoca_ir_solve::TensorIndex::Runtime(
                12,
            )),
            rumoca_ir_solve::TensorUpdateSubscript::Whole,
        ]
        .into_boxed_slice(),
        lanes: 2,
    });
    for src in 0..21 {
        update.push(LinearOp::StoreOutput { src });
    }
    let program = std::sync::Arc::new(
        rumoca_ir_solve::FunctionFoldProgram::checked(
            rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 1,
                    step: 1,
                }],
            },
            21,
            0,
            update,
        )
        .expect("construct tensor-update fold"),
    );
    let initial = [
        1.0, 10.0, 2.0, 20.0, 3.0, 30.0, 4.0, 40.0, 9.0, 90.0, 8.0, 80.0, 2.0,
    ];
    let mut row = initial
        .into_iter()
        .enumerate()
        .map(|(dst, value)| LinearOp::Const {
            dst: dst as u32,
            value,
        })
        .collect::<Vec<_>>();
    for dst in 13..21 {
        row.push(LinearOp::Const { dst, value: 0.0 });
    }
    row.push(LinearOp::FunctionFold {
        dst_start: 21,
        initial_start: 0,
        capture_start: 0,
        program,
    });
    for src in 34..42 {
        row.push(LinearOp::StoreOutput { src });
    }

    let compiled = compile_residual_rows(&[row]).expect("compile tensor-update fold");
    let mut out = [0.0; 8];
    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate tensor-update fold");

    assert_eq!(out, [1.0, 10.0, 2.0, 20.0, 9.0, 90.0, 8.0, 80.0]);
}

#[test]
fn compiled_function_fold_executes_tensor_update_slice_as_retained_scan() {
    let mut update = (0..14)
        .map(|index| LinearOp::LoadFoldCarried {
            dst: index as u32,
            index,
        })
        .collect::<Vec<_>>();
    update.push(LinearOp::TensorUpdate {
        dst_start: 10,
        base_start: 0,
        value_start: 4,
        dimensions: Box::new([2, 2]),
        subscripts: Box::new([
            rumoca_ir_solve::TensorUpdateSubscript::Slice {
                start: 8,
                dimensions: Box::new([2]),
            },
            rumoca_ir_solve::TensorUpdateSubscript::Whole,
        ]),
        lanes: 1,
    });
    for src in 0..14 {
        update.push(LinearOp::StoreOutput { src });
    }
    let program = std::sync::Arc::new(
        rumoca_ir_solve::FunctionFoldProgram::checked(
            rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 1,
                    step: 1,
                }],
            },
            14,
            0,
            update,
        )
        .expect("construct sliced tensor-update fold"),
    );
    let initial = [
        1.0, 2.0, 3.0, 4.0, 10.0, 20.0, 30.0, 40.0, 2.0, 1.0, 0.0, 0.0, 0.0, 0.0,
    ];
    let mut row = initial
        .into_iter()
        .enumerate()
        .map(|(dst, value)| LinearOp::Const {
            dst: dst as u32,
            value,
        })
        .collect::<Vec<_>>();
    row.push(LinearOp::FunctionFold {
        dst_start: 14,
        initial_start: 0,
        capture_start: 0,
        program,
    });
    for src in 24..28 {
        row.push(LinearOp::StoreOutput { src });
    }

    let compiled = compile_residual_rows(&[row]).expect("compile sliced tensor-update fold");
    let mut out = [0.0; 4];
    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate sliced tensor-update fold");

    assert_eq!(out, [30.0, 40.0, 10.0, 20.0]);
}

#[test]
fn compiled_runtime_tensor_projection_uses_compact_affine_indexing() {
    let row = vec![
        LinearOp::Const {
            dst: 0,
            value: 10.0,
        },
        LinearOp::Const {
            dst: 1,
            value: 20.0,
        },
        LinearOp::Const {
            dst: 2,
            value: 30.0,
        },
        LinearOp::Const {
            dst: 3,
            value: 40.0,
        },
        LinearOp::Const { dst: 4, value: 2.0 },
        LinearOp::LoadIndexedRegister {
            dst: 5,
            base: 0,
            stride: 1,
            dimensions: Box::new([2, 2]),
            indices: Box::new([
                rumoca_ir_solve::TensorIndex::Runtime(4),
                rumoca_ir_solve::TensorIndex::Constant(0),
            ]),
        },
        LinearOp::StoreOutput { src: 5 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile compact tensor projection");
    let mut out = [0.0];

    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate compact tensor projection");

    assert_eq!(out, [30.0]);
    assert_eq!(compiled.jit_call_count(), 1);
}

#[test]
fn compiled_function_fold_projects_directly_from_carried_tensor_memory() {
    let row = vec![
        LinearOp::Const {
            dst: 0,
            value: 10.0,
        },
        LinearOp::Const {
            dst: 1,
            value: 20.0,
        },
        LinearOp::Const {
            dst: 2,
            value: 30.0,
        },
        LinearOp::Const {
            dst: 3,
            value: 40.0,
        },
        LinearOp::FunctionFold {
            dst_start: 4,
            initial_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(
                rumoca_ir_solve::FunctionFoldProgram::checked(
                    rumoca_core::StructuredIndexDomain {
                        binders: vec![rumoca_core::StructuredIndexBinder {
                            id: 0,
                            display_name: "i".to_string(),
                            lower: 1,
                            upper: 2,
                            step: 1,
                        }],
                    },
                    4,
                    0,
                    vec![
                        LinearOp::LoadFoldIndex {
                            dst: 0,
                            dimension: 0,
                        },
                        LinearOp::LoadIndexedFoldCarried {
                            dst: 1,
                            base: 0,
                            stride: 1,
                            dimensions: Box::new([2, 2]),
                            indices: Box::new([
                                rumoca_ir_solve::TensorIndex::Runtime(0),
                                rumoca_ir_solve::TensorIndex::Constant(0),
                            ]),
                        },
                        LinearOp::StoreOutput { src: 1 },
                        LinearOp::LoadFoldCarried { dst: 2, index: 1 },
                        LinearOp::StoreOutput { src: 2 },
                        LinearOp::LoadFoldCarried { dst: 3, index: 2 },
                        LinearOp::StoreOutput { src: 3 },
                        LinearOp::LoadFoldCarried { dst: 4, index: 3 },
                        LinearOp::StoreOutput { src: 4 },
                    ],
                )
                .expect("construct compact tensor fold"),
            ),
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile compact tensor fold");
    let mut out = [0.0];

    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate compact tensor fold");

    assert_eq!(out, [30.0]);
    assert_eq!(compiled.jit_call_count(), 1);
}

#[test]
fn compiled_function_fold_updates_a_tensor_slice_without_scalar_selects() {
    let row = vec![
        LinearOp::Const {
            dst: 0,
            value: 10.0,
        },
        LinearOp::Const {
            dst: 1,
            value: 20.0,
        },
        LinearOp::Const {
            dst: 2,
            value: 30.0,
        },
        LinearOp::Const {
            dst: 3,
            value: 40.0,
        },
        LinearOp::FunctionFold {
            dst_start: 4,
            initial_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(
                rumoca_ir_solve::FunctionFoldProgram::checked(
                    rumoca_core::StructuredIndexDomain {
                        binders: vec![rumoca_core::StructuredIndexBinder {
                            id: 0,
                            display_name: "column".to_string(),
                            lower: 1,
                            upper: 2,
                            step: 1,
                        }],
                    },
                    4,
                    0,
                    vec![
                        LinearOp::LoadFoldIndex {
                            dst: 0,
                            dimension: 0,
                        },
                        LinearOp::Const {
                            dst: 1,
                            value: 100.0,
                        },
                        LinearOp::Const {
                            dst: 2,
                            value: 200.0,
                        },
                        LinearOp::StoreOutputFoldTensorUpdate {
                            source_base: 0,
                            source_stride: 1,
                            dimensions: Box::new([2, 2]),
                            updates: Box::new([rumoca_ir_solve::FoldTensorUpdate {
                                subscripts: Box::new([
                                    rumoca_ir_solve::TensorSubscript::Whole,
                                    rumoca_ir_solve::TensorSubscript::Index(
                                        rumoca_ir_solve::TensorIndex::Runtime(0),
                                    ),
                                ]),
                                condition: None,
                                value_start: 1,
                                value_stride: 1,
                            }]),
                            nodes: Box::new([rumoca_ir_solve::FoldTensorNode::Update {
                                base: 0,
                                update: 0,
                            }]),
                            result: 1,
                            lanes: 1,
                        },
                    ],
                )
                .expect("construct compact tensor update fold"),
            ),
        },
        LinearOp::StoreOutput { src: 4 },
        LinearOp::StoreOutput { src: 5 },
        LinearOp::StoreOutput { src: 6 },
        LinearOp::StoreOutput { src: 7 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile compact tensor update fold");
    let mut out = [0.0; 4];

    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate compact tensor update fold");

    assert_eq!(out, [100.0, 100.0, 200.0, 200.0]);
    assert_eq!(compiled.jit_call_count(), 1);
}

#[test]
fn compiled_nested_fold_copies_parent_tensor_ranges_without_register_expansion() {
    let nested = four_lane_nested_fold();
    let outer = rumoca_ir_solve::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 1,
                display_name: "outer".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        4,
        0,
        vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutputFunctionFold {
                initial: Box::new([rumoca_ir_solve::FoldInitialSource::ParentCarried {
                    base: 0,
                    count: 4,
                }]),
                capture_start: 0,
                program: std::sync::Arc::new(nested),
                result_base: 0,
                count: 4,
                condition: Some(0),
                nested_when_true: false,
            },
        ],
    )
    .expect("construct outer compact fold");
    let row = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::Const { dst: 2, value: 3.0 },
        LinearOp::Const { dst: 3, value: 4.0 },
        LinearOp::FunctionFold {
            dst_start: 4,
            initial_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(outer),
        },
        LinearOp::StoreOutput { src: 4 },
        LinearOp::StoreOutput { src: 5 },
        LinearOp::StoreOutput { src: 6 },
        LinearOp::StoreOutput { src: 7 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile nested compact fold");
    let mut out = [0.0; 4];

    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate nested compact fold");

    assert_eq!(out, [4.0, 5.0, 6.0, 7.0]);
    assert_eq!(compiled.jit_call_count(), 1);
}

fn four_lane_nested_fold() -> rumoca_ir_solve::FunctionFoldProgram {
    rumoca_ir_solve::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "inner".to_string(),
                lower: 1,
                upper: 2,
                step: 1,
            }],
        },
        4,
        0,
        vec![
            LinearOp::LoadFoldIndex {
                dst: 0,
                dimension: 0,
            },
            LinearOp::LoadFoldCarried { dst: 1, index: 0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 1,
                rhs: 0,
            },
            LinearOp::LoadFoldCarried { dst: 3, index: 1 },
            LinearOp::Binary {
                dst: 4,
                op: BinaryOp::Add,
                lhs: 3,
                rhs: 0,
            },
            LinearOp::LoadFoldCarried { dst: 5, index: 2 },
            LinearOp::Binary {
                dst: 6,
                op: BinaryOp::Add,
                lhs: 5,
                rhs: 0,
            },
            LinearOp::LoadFoldCarried { dst: 7, index: 3 },
            LinearOp::Binary {
                dst: 8,
                op: BinaryOp::Add,
                lhs: 7,
                rhs: 0,
            },
            LinearOp::StoreOutput { src: 2 },
            LinearOp::StoreOutput { src: 4 },
            LinearOp::StoreOutput { src: 6 },
            LinearOp::StoreOutput { src: 8 },
        ],
    )
    .expect("construct inner compact fold")
}

#[test]
fn compiled_guarded_nested_fold_selects_at_native_control_flow_boundary() {
    let nested = rumoca_ir_solve::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "inner".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        1,
        0,
        vec![
            LinearOp::LoadFoldCarried { dst: 0, index: 0 },
            LinearOp::Const { dst: 1, value: 2.0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ],
    )
    .expect("construct guarded inner fold");
    let outer = rumoca_ir_solve::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 1,
                display_name: "outer".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        1,
        1,
        vec![
            LinearOp::LoadFoldCapture { dst: 0, index: 0 },
            LinearOp::StoreOutputFunctionFold {
                initial: Box::new([rumoca_ir_solve::FoldInitialSource::ParentCarried {
                    base: 0,
                    count: 1,
                }]),
                capture_start: 0,
                program: std::sync::Arc::new(nested),
                result_base: 0,
                count: 1,
                condition: Some(0),
                nested_when_true: true,
            },
        ],
    )
    .expect("construct guarded outer fold");
    let row = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 3.0 },
        LinearOp::FunctionFold {
            dst_start: 2,
            initial_start: 1,
            capture_start: 0,
            program: std::sync::Arc::new(outer),
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile guarded compact fold");
    let mut out = [0.0];

    compiled
        .call(&[], &[0.0], 0.0, &mut out)
        .expect("evaluate inactive guarded fold");
    assert_eq!(out, [3.0]);

    compiled
        .call(&[], &[1.0], 0.0, &mut out)
        .expect("evaluate active guarded fold");
    assert_eq!(out, [5.0]);
}

#[test]
fn compiled_guarded_function_fold_preserves_inactive_initial_tuple() {
    let fold = rumoca_ir_solve::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        1,
        0,
        vec![
            LinearOp::LoadFoldCarried { dst: 0, index: 0 },
            LinearOp::Const { dst: 1, value: 2.0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ],
    )
    .expect("construct compact guarded fold");
    let row = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 3.0 },
        LinearOp::GuardedFunctionFold {
            dst_start: 2,
            initial_start: 1,
            capture_start: 0,
            activation: 0,
            program: std::sync::Arc::new(fold),
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile guarded compact fold");
    let mut out = [0.0];

    compiled
        .call(&[], &[0.0], 0.0, &mut out)
        .expect("evaluate inactive guarded fold");
    assert_eq!(out, [3.0]);

    compiled
        .call(&[], &[1.0], 0.0, &mut out)
        .expect("evaluate active guarded fold");
    assert_eq!(out, [5.0]);
}

#[test]
fn compiled_function_conditional_selects_one_correlated_result_tuple() {
    let program = rumoca_ir_solve::FunctionConditionalProgram::checked(
        1,
        [1, 1],
        [(
            vec![
                LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                LinearOp::StoreOutputRange {
                    start: 0,
                    count: 1,
                    stride: 1,
                },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 2.0 },
                LinearOp::TensorFill {
                    dst_start: 1,
                    value_start: 0,
                    count: 2,
                    lanes: 1,
                },
                LinearOp::StoreOutputRange {
                    start: 1,
                    count: 2,
                    stride: 1,
                },
            ],
        )],
        vec![
            LinearOp::Const { dst: 0, value: 4.0 },
            LinearOp::TensorFill {
                dst_start: 1,
                value_start: 0,
                count: 2,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 1,
                count: 2,
                stride: 1,
            },
        ],
    )
    .expect("construct correlated conditional");
    let row = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::FunctionConditional {
            dst_start: 1,
            capture_start: 0,
            program: std::sync::Arc::new(program),
        },
        LinearOp::StoreOutput { src: 1 },
        LinearOp::StoreOutput { src: 2 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile correlated conditional");
    let mut out = [0.0; 2];

    compiled
        .call(&[], &[0.0], 0.0, &mut out)
        .expect("evaluate fallback tuple");
    assert_eq!(out, [4.0, 4.0]);

    compiled
        .call(&[], &[1.0], 0.0, &mut out)
        .expect("evaluate selected tuple");
    assert_eq!(out, [2.0, 2.0]);
}

#[test]
fn compiled_guarded_assignment_preserves_one_compact_owner_until_native_execution() {
    let conditional = rumoca_ir_solve::FunctionConditionalProgram::checked_owned(
        rumoca_ir_solve::FunctionConditionalOwnerId::checked(7).expect("nonzero compact owner id"),
        0,
        [2],
        [(
            vec![
                LinearOp::LoadP { dst: 0, index: 2 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 7.0 },
                LinearOp::Const { dst: 1, value: 8.0 },
                LinearOp::StoreOutputRange {
                    start: 0,
                    count: 2,
                    stride: 1,
                },
            ],
        )],
        vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: rumoca_ir_solve::TensorInputKind::P,
                input_start: 0,
                count: 2,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 1,
            },
        ],
    )
    .expect("checked lazy tuple");
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("guarded_assignment_native.mo"),
        0,
        1,
    );
    let owner = rumoca_ir_solve::GuardedAssignmentProgram::checked(
        vec![
            LinearOp::FunctionConditional {
                dst_start: 0,
                capture_start: 0,
                program: std::sync::Arc::new(conditional),
            },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 1,
            },
        ],
        span.require_provenance("native guarded assignment fixture")
            .expect("fixture provenance"),
        [(rumoca_ir_solve::scalar_slot_p(0), 2)],
        rumoca_ir_solve::DiscreteRowRole::EventAction,
        rumoca_ir_solve::DiscreteEventPreMode::FollowCurrent,
        false,
        rumoca_ir_solve::IntegratorHistoryEffect::Preserve,
        None,
    )
    .expect("checked compact owner");
    let program = owner.program().to_vec();
    let compiled = compile_residual_rows(&[program]).expect("compile compact owner");
    let mut out = [0.0; 2];

    compiled
        .call(&[], &[1.0, 2.0, 0.0], 0.0, &mut out)
        .expect("inactive owner holds its tensor");
    assert_eq!(out, [1.0, 2.0]);
    compiled
        .call(&[], &[1.0, 2.0, 1.0], 0.0, &mut out)
        .expect("active owner updates its tensor");
    assert_eq!(out, [7.0, 8.0]);
}

#[test]
fn compiled_function_conditional_projects_one_compact_capture_range() {
    let program = rumoca_ir_solve::FunctionConditionalProgram::checked(
        4,
        [3],
        [(
            vec![
                LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::LoadFunctionConditionalCaptureRange {
                    dst_start: 0,
                    index_start: 1,
                    count: 3,
                },
                LinearOp::StoreOutputRange {
                    start: 0,
                    count: 3,
                    stride: 1,
                },
            ],
        )],
        vec![
            LinearOp::Const { dst: 0, value: 7.0 },
            LinearOp::TensorFill {
                dst_start: 1,
                value_start: 0,
                count: 3,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 1,
                count: 3,
                stride: 1,
            },
        ],
    )
    .expect("construct compact capture-range conditional");
    let row = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::LoadP { dst: 1, index: 1 },
        LinearOp::LoadP { dst: 2, index: 2 },
        LinearOp::LoadP { dst: 3, index: 3 },
        LinearOp::FunctionConditional {
            dst_start: 4,
            capture_start: 0,
            program: std::sync::Arc::new(program),
        },
        LinearOp::StoreOutputRange {
            start: 4,
            count: 3,
            stride: 1,
        },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile capture-range conditional");
    let mut out = [0.0; 3];

    compiled
        .call(&[], &[0.0, 1.0, 2.0, 3.0], 0.0, &mut out)
        .expect("evaluate fallback tuple");
    assert_eq!(out, [7.0, 7.0, 7.0]);

    compiled
        .call(&[], &[1.0, 1.0, 2.0, 3.0], 0.0, &mut out)
        .expect("evaluate selected capture range");
    assert_eq!(out, [1.0, 2.0, 3.0]);
}

#[test]
fn compiled_function_conditional_calls_fold_only_from_selected_region() {
    let fold = rumoca_ir_solve::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 2,
                step: 1,
            }],
        },
        1,
        0,
        vec![
            LinearOp::LoadFoldCarried { dst: 0, index: 0 },
            LinearOp::Const { dst: 1, value: 2.0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ],
    )
    .expect("construct compact fold inside conditional region");
    let program = rumoca_ir_solve::FunctionConditionalProgram::checked_owned(
        rumoca_ir_solve::FunctionConditionalOwnerId::checked(11)
            .expect("nonzero conditional-fold owner id"),
        1,
        [1],
        [(
            vec![
                LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const {
                    dst: 100,
                    value: 3.0,
                },
                LinearOp::FunctionFold {
                    dst_start: 101,
                    initial_start: 100,
                    capture_start: 0,
                    program: std::sync::Arc::new(fold),
                },
                LinearOp::StoreOutput { src: 101 },
            ],
        )],
        vec![
            LinearOp::Const {
                dst: 0,
                value: 11.0,
            },
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .expect("construct conditional containing compact fold");
    let row = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::FunctionConditional {
            dst_start: 1,
            capture_start: 0,
            program: std::sync::Arc::new(program),
        },
        LinearOp::TensorFill {
            dst_start: 2,
            value_start: 1,
            count: 1,
            lanes: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile conditional compact fold");
    let mut out = [0.0];

    compiled
        .call(&[], &[0.0], 0.0, &mut out)
        .expect("evaluate fallback without fold call");
    assert_eq!(out, [11.0]);

    compiled
        .call(&[], &[1.0], 0.0, &mut out)
        .expect("evaluate selected compact fold");
    assert_eq!(out, [7.0]);
}
