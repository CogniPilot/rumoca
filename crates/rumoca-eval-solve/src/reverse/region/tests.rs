use super::*;
use crate::prepared::PreparedScalarProgramBlock;
use rumoca_core::{IndexBinderProvenance, StructuredIndexBinder, StructuredIndexDomain};
use rumoca_ir_solve::{
    CompareOp, FoldInitialSource, FoldTensorNode, FoldTensorUpdate, LinearOp::*, TensorIndex,
    TensorUpdateSubscript,
};
use std::sync::Arc;

fn domain(n: i64) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: n,
            step: 1,
        }],
    }
}
fn block(row: Vec<LinearOp>) -> ScalarProgramBlock {
    ScalarProgramBlock::with_program_spans(
        vec![row],
        vec![rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            0,
            1,
        )],
    )
    .unwrap()
}
fn check_gradient(row: Vec<LinearOp>, y: &[f64], p: &[f64], w: &[f64], expected: &[f64]) {
    let primal = block(row);
    let prepared = PreparedScalarProgramBlock::new(primal.clone()).unwrap();
    let inputs = ReverseInputs {
        y,
        p,
        t: 0.,
        context: Default::default(),
    };
    let (mut gy, mut gp) = (vec![0.; y.len()], vec![0.; p.len()]);
    prepared
        .reverse_vjp(
            &inputs,
            w,
            &mut ReverseCotangents {
                y: &mut gy,
                p: &mut gp,
                seed: &mut [],
            },
            &mut Default::default(),
        )
        .unwrap();
    gy.extend(gp);
    assert_eq!(gy, expected);
    let dual = crate::ad::lower_scalar_program_block_full_jvp(&primal, y.len()).unwrap();
    let x: Vec<_> = y.iter().chain(p).copied().collect();
    let objective = |x: &[f64]| {
        let mut out = vec![0.; w.len()];
        crate::eval_scalar_program_block(&primal, &x[..y.len()], &x[y.len()..], 0., None, &mut out)
            .unwrap();
        out.iter().zip(w).map(|(v, w)| v * w).sum::<f64>()
    };
    for (i, &gradient) in gy.iter().enumerate() {
        let mut seed = vec![0.; x.len()];
        seed[i] = 1.;
        let mut tangent = vec![0.; w.len()];
        crate::eval_scalar_program_block(&dual, y, p, 0., Some(&seed), &mut tangent).unwrap();
        let forward = tangent.iter().zip(w).map(|(t, w)| t * w).sum::<f64>();
        assert_eq!(forward, gradient, "forward AD coordinate {i}");
        let mut a = x.clone();
        let mut b = x.clone();
        a[i] += 1e-5;
        b[i] -= 1e-5;
        let fd = (objective(&a) - objective(&b)) / 2e-5;
        assert!(
            (fd - gradient).abs() < 1e-6 * gradient.abs().max(1.),
            "coordinate {i}: AD={gradient}, FD={fd}"
        );
    }
}

fn prod(dst: Reg, lhs: Reg, rhs: Reg) -> LinearOp {
    Binary {
        dst,
        op: BinaryOp::Mul,
        lhs,
        rhs,
    }
}
fn sum(dst: Reg, lhs: Reg, rhs: Reg) -> LinearOp {
    Binary {
        dst,
        op: BinaryOp::Add,
        lhs,
        rhs,
    }
}

#[test]
fn bounded_while_reverses_exactly_the_executed_updates_and_keeps_shared_inputs() {
    // x <- x*p + q. The bound permits ten iterations, the predicate stops after two.
    let program = FunctionFoldProgram::checked(
        domain(10),
        1,
        1,
        vec![
            LoadFoldCarried { dst: 0, index: 0 },
            LoadFoldCapture { dst: 1, index: 0 },
            LoadP { dst: 2, index: 0 },
            prod(3, 0, 1),
            sum(4, 3, 2),
            StoreOutput { src: 4 },
        ],
    )
    .unwrap()
    .with_continuation(vec![
        LoadFoldCarried { dst: 0, index: 0 },
        Const { dst: 1, value: 4. },
        Compare {
            dst: 2,
            op: CompareOp::Lt,
            lhs: 0,
            rhs: 1,
        },
        StoreOutput { src: 2 },
    ])
    .unwrap();
    let row = vec![
        LoadY { dst: 0, index: 0 },
        LoadY { dst: 1, index: 1 },
        LoadY { dst: 2, index: 2 },
        GuardedFunctionFold {
            dst_start: 0,
            initial_start: 0,
            capture_start: 1,
            activation: 2,
            program: Arc::new(program),
        },
        StoreOutput { src: 0 },
    ];
    check_gradient(row.clone(), &[1., 2., 1.], &[1.], &[1.], &[4., 5., 0., 3.]);
    // A zero activation is a switching point; test the identity with a constant false guard.
    let mut off = row;
    off[2] = Const { dst: 2, value: 0. };
    check_gradient(off, &[1., 2., 1.], &[1.], &[1.], &[1., 0., 0., 0.]);
}

#[test]
fn nested_projected_folds_route_captures_and_parent_tuple_at_both_guards() {
    let inner = Arc::new(
        FunctionFoldProgram::checked(
            domain(2),
            2,
            1,
            vec![
                LoadFoldCarried { dst: 0, index: 0 },
                LoadFoldCarried { dst: 1, index: 1 },
                LoadFoldCapture { dst: 2, index: 0 },
                sum(3, 0, 2),
                prod(4, 1, 0),
                StoreOutputRange {
                    start: 3,
                    count: 2,
                    stride: 1,
                },
            ],
        )
        .unwrap(),
    );
    let nested = |enabled| {
        Arc::new(
            FunctionFoldProgram::checked(
                domain(1),
                2,
                1,
                vec![
                    LoadFoldCapture { dst: 0, index: 0 },
                    LoadFoldCarried { dst: 1, index: 1 },
                    Const {
                        dst: 2,
                        value: enabled,
                    },
                    StoreOutputFunctionFold {
                        initial: vec![
                            FoldInitialSource::ParentCarried { base: 0, count: 1 },
                            FoldInitialSource::Registers { start: 1, count: 1 },
                        ]
                        .into(),
                        capture_start: 0,
                        program: inner.clone(),
                        result_base: 1,
                        count: 1,
                        condition: Some(2),
                        nested_when_true: true,
                    },
                    StoreOutput { src: 0 },
                ],
            )
            .unwrap(),
        )
    };
    for (enabled, expected) in [(1., vec![7., 10., 3.]), (0., vec![1., 0., 1.])] {
        let row = vec![
            LoadY { dst: 0, index: 0 },
            LoadY { dst: 1, index: 1 },
            LoadY { dst: 2, index: 2 },
            FunctionFold {
                dst_start: 3,
                initial_start: 0,
                capture_start: 2,
                program: nested(enabled),
            },
            StoreOutputRange {
                start: 3,
                count: 2,
                stride: 1,
            },
        ];
        // First output is b*a*(a+c), second is c.
        check_gradient(row, &[2., 1., 3.], &[], &[1., 1.], &expected);
    }
}

#[test]
fn a_function_conditional_inside_a_fold_routes_correlated_results_and_external_parameters() {
    let pred = vec![
        LoadFunctionConditionalCapture { dst: 0, index: 1 },
        Const { dst: 1, value: 0. },
        Compare {
            dst: 2,
            op: CompareOp::Gt,
            lhs: 0,
            rhs: 1,
        },
        StoreOutput { src: 2 },
    ];
    let active = vec![
        LoadFunctionConditionalCaptureRange {
            dst_start: 0,
            index_start: 0,
            count: 2,
        },
        prod(2, 0, 1),
        LoadP { dst: 3, index: 0 },
        sum(4, 2, 3),
        StoreOutput { src: 4 },
        StoreOutput { src: 0 },
    ];
    let fallback = vec![
        LoadFunctionConditionalCapture { dst: 0, index: 0 },
        LoadP { dst: 1, index: 0 },
        StoreOutputRange {
            start: 0,
            count: 2,
            stride: 1,
        },
    ];
    let branch = Arc::new(
        FunctionConditionalProgram::checked(2, [1, 1], [(pred, active)], fallback).unwrap(),
    );
    let outer = Arc::new(
        FunctionFoldProgram::checked(
            domain(2),
            2,
            1,
            vec![
                LoadFoldCarried { dst: 0, index: 0 },
                LoadFoldCapture { dst: 1, index: 0 },
                FunctionConditional {
                    dst_start: 2,
                    capture_start: 0,
                    program: branch,
                },
                StoreOutputRange {
                    start: 2,
                    count: 2,
                    stride: 1,
                },
            ],
        )
        .unwrap(),
    );
    let row = vec![
        LoadY { dst: 0, index: 0 },
        LoadY { dst: 1, index: 1 },
        LoadY { dst: 2, index: 2 },
        FunctionFold {
            dst_start: 3,
            initial_start: 0,
            capture_start: 2,
            program: outer,
        },
        StoreOutputRange {
            start: 3,
            count: 2,
            stride: 1,
        },
    ];
    check_gradient(
        row.clone(),
        &[2., 1., 3.],
        &[1.],
        &[2., -1.],
        &[15., 0., 24., 7.],
    );
    check_gradient(row, &[2., 1., -3.], &[1.], &[2., -1.], &[2., 0., 0., -1.]);
}

#[test]
fn aggregate_patches_and_runtime_gathers_reverse_to_the_selected_elements() {
    let idx = vec![TensorIndex::Runtime(0)].into_boxed_slice();
    let program = Arc::new(
        FunctionFoldProgram::checked(
            domain(2),
            3,
            2,
            vec![
                LoadFoldIndex {
                    dst: 0,
                    dimension: 0,
                },
                LoadIndexedFoldCarried {
                    dst: 1,
                    base: 0,
                    stride: 1,
                    dimensions: vec![3].into(),
                    indices: idx.clone(),
                },
                LoadIndexedFoldCapture {
                    dst: 2,
                    base: 0,
                    stride: 1,
                    dimensions: vec![2].into(),
                    indices: idx,
                },
                prod(3, 1, 2),
                StoreOutputFoldTensorUpdate {
                    source_base: 0,
                    source_stride: 1,
                    dimensions: vec![3].into(),
                    updates: vec![FoldTensorUpdate {
                        subscripts: vec![TensorUpdateSubscript::Index(TensorIndex::Runtime(0))]
                            .into(),
                        condition: None,
                        value_start: 3,
                        value_stride: 1,
                    }]
                    .into(),
                    nodes: vec![
                        FoldTensorNode::Update { base: 0, update: 0 },
                        FoldTensorNode::Select {
                            condition: 0,
                            if_true: 1,
                            if_false: 0,
                        },
                    ]
                    .into(),
                    result: 2,
                    lanes: 1,
                },
            ],
        )
        .unwrap(),
    );
    let mut row = (0..5)
        .map(|i| LoadY {
            dst: i,
            index: i as usize,
        })
        .collect::<Vec<_>>();
    row.extend([
        FunctionFold {
            dst_start: 5,
            initial_start: 0,
            capture_start: 3,
            program,
        },
        StoreOutputRange {
            start: 5,
            count: 3,
            stride: 1,
        },
    ]);
    check_gradient(
        row,
        &[2., 3., 4., 5., 6.],
        &[],
        &[2., 3., 4.],
        &[10., 18., 4., 4., 9.],
    );
}

#[test]
fn a_lazy_read_in_a_selected_region_only_faults_when_demanded() {
    let result = vec![
        LoadFunctionConditionalCapture { dst: 0, index: 0 },
        Const { dst: 1, value: 99. },
        LoadIndexedRegister {
            dst: 2,
            base: 0,
            stride: 1,
            dimensions: vec![1].into(),
            indices: vec![TensorIndex::Runtime(1)].into(),
        },
        Select {
            dst: 3,
            cond: 0,
            if_true: 0,
            if_false: 2,
        },
        StoreOutput { src: 3 },
    ];
    let branch = Arc::new(
        FunctionConditionalProgram::checked(
            1,
            [1],
            [(
                vec![Const { dst: 0, value: 1. }, StoreOutput { src: 0 }],
                result,
            )],
            vec![Const { dst: 0, value: 0. }, StoreOutput { src: 0 }],
        )
        .unwrap(),
    );
    let prepared = PreparedScalarProgramBlock::new(block(vec![
        LoadY { dst: 0, index: 0 },
        FunctionConditional {
            dst_start: 1,
            capture_start: 0,
            program: branch,
        },
        StoreOutput { src: 1 },
    ]))
    .unwrap();
    let mut scratch = ReverseScratch::default();
    for active in [1., 0., 2.] {
        let mut gy = [4.];
        let answer = prepared.reverse_vjp(
            &ReverseInputs {
                y: &[active],
                p: &[],
                t: 0.,
                context: Default::default(),
            },
            &[3.],
            &mut ReverseCotangents {
                y: &mut gy,
                p: &mut [],
                seed: &mut [],
            },
            &mut scratch,
        );
        if active == 0. {
            assert!(answer.is_err());
            assert_eq!(gy, [4.]);
        } else {
            answer.unwrap();
            assert_eq!(gy, [7.]);
        }
    }
}

#[test]
fn inactive_guard_does_not_hide_an_unimplemented_nested_rule() {
    let program = Arc::new(
        FunctionFoldProgram::checked(
            domain(1),
            1,
            0,
            vec![
                Const { dst: 0, value: 1. },
                ImpureRandomInit { dst: 1, seed: 0 },
                StoreOutput { src: 1 },
            ],
        )
        .unwrap(),
    );
    let prepared = PreparedScalarProgramBlock::new(block(vec![
        LoadY { dst: 0, index: 0 },
        Const { dst: 1, value: 0. },
        GuardedFunctionFold {
            dst_start: 2,
            initial_start: 0,
            capture_start: 0,
            activation: 1,
            program,
        },
        StoreOutput { src: 2 },
    ]))
    .unwrap();
    let mut gy = [8.];
    let e = prepared
        .reverse_vjp(
            &ReverseInputs {
                y: &[2.],
                p: &[],
                t: 0.,
                context: Default::default(),
            },
            &[3.],
            &mut ReverseCotangents {
                y: &mut gy,
                p: &mut [],
                seed: &mut [],
            },
            &mut Default::default(),
        )
        .unwrap_err();
    assert!(e.to_string().contains("ImpureRandomInit"), "{e}");
    assert!(e.source_span().is_some());
    assert_eq!(gy, [8.]);
    let e = prepared
        .reverse_row_y_gradient(
            0,
            &ReverseInputs {
                y: &[2.],
                p: &[],
                t: 0.,
                context: Default::default(),
            },
            &mut gy,
            &mut Default::default(),
        )
        .unwrap_err();
    assert!(e.to_string().contains("ImpureRandomInit"), "{e}");
    assert!(e.source_span().is_some());
    assert_eq!(gy, [8.]);
}

#[test]
fn unselected_singular_output_of_nested_fold_does_not_poison_selected_output() {
    let inner = Arc::new(
        FunctionFoldProgram::checked(
            domain(1),
            2,
            0,
            vec![
                LoadFoldCarried { dst: 0, index: 0 },
                LoadFoldCarried { dst: 1, index: 1 },
                Unary {
                    dst: 2,
                    op: UnaryOp::Exp,
                    arg: 0,
                },
                prod(3, 1, 1),
                Select {
                    dst: 4,
                    cond: 1,
                    if_true: 2,
                    if_false: 1,
                },
                StoreOutput { src: 4 },
                StoreOutput { src: 3 },
            ],
        )
        .unwrap(),
    );
    let outer = Arc::new(
        FunctionFoldProgram::checked(
            domain(1),
            1,
            1,
            vec![
                LoadFoldCapture { dst: 0, index: 0 },
                LoadFoldCarried { dst: 1, index: 0 },
                FunctionFold {
                    dst_start: 2,
                    initial_start: 0,
                    capture_start: 0,
                    program: inner,
                },
                Const { dst: 4, value: 1. },
                Select {
                    dst: 5,
                    cond: 4,
                    if_true: 3,
                    if_false: 2,
                },
                StoreOutput { src: 5 },
            ],
        )
        .unwrap(),
    );
    check_gradient(
        vec![
            LoadY { dst: 0, index: 0 },
            LoadY { dst: 1, index: 1 },
            FunctionFold {
                dst_start: 2,
                initial_start: 1,
                capture_start: 0,
                program: outer,
            },
            StoreOutput { src: 2 },
        ],
        &[1000., 3.],
        &[],
        &[1.],
        &[0., 6.],
    );
}
