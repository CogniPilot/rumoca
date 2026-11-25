//! Strict requests, register versions and continuous-table derivatives.
use super::*;
use rumoca_ir_solve::{LinearOp::*, TensorIndex};

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 10, 20)
}

fn pullback(
    block: &ScalarProgramBlock,
    y: &[f64],
    context: RowEvalContext<'_>,
    weights: &[f64],
    gradient: &mut [f64],
    seeds: &mut [f64],
) -> Result<(), EvalSolveError> {
    let registers = block
        .programs()
        .iter()
        .map(|row| crate::required_registers(row))
        .collect::<Result<Vec<_>, _>>()?;
    reverse_scalar_block_vjp(
        &ScalarVjpProgram {
            block,
            row_registers: &registers,
            requirements: crate::scalar_program_block_input_requirements(block)?,
        },
        &ReverseInputs {
            y,
            p: &[],
            t: 0.,
            context,
        },
        weights,
        &mut ReverseCotangents {
            y: gradient,
            p: &mut [],
            seed: seeds,
        },
        &mut ReverseScratch::default(),
    )
}

#[test]
fn a_missing_rule_fails_before_an_earlier_row_can_publish_a_partial_gradient() {
    let rows = vec![
        vec![LoadY { dst: 0, index: 0 }, StoreOutput { src: 0 }],
        vec![
            Const { dst: 0, value: 1. },
            ImpureRandomInit { dst: 1, seed: 0 },
            StoreOutput { src: 1 },
        ],
    ];
    let block = ScalarProgramBlock::with_output_indices(rows, vec![span(); 2], vec![0, 1]).unwrap();
    let mut gradient = [4.];
    let error = pullback(
        &block,
        &[2.],
        Default::default(),
        &[2., 1.],
        &mut gradient,
        &mut [],
    )
    .unwrap_err();
    assert_eq!(gradient, [4.]);
    assert_eq!(error.source_span(), Some(span()));
    assert!(
        error
            .to_string()
            .contains("ImpureRandomInit at row 1, instruction 1"),
        "{error}"
    );
}

#[test]
fn output_written_before_a_register_is_reused_keeps_its_own_seed() {
    let row = vec![
        LoadY { dst: 0, index: 0 },
        StoreOutput { src: 0 },
        Binary {
            dst: 0,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        StoreOutputRange {
            start: 0,
            count: 1,
            stride: 1,
        },
    ];
    let block =
        ScalarProgramBlock::with_output_indices(vec![row], vec![span()], vec![0, 1]).unwrap();
    let mut gradient = [0.];
    pullback(
        &block,
        &[3.],
        Default::default(),
        &[4., 5.],
        &mut gradient,
        &mut [],
    )
    .unwrap();
    assert_eq!(gradient, [34.]);
}

#[test]
fn indexed_read_selects_one_strided_register_and_one_seed() {
    let row = vec![
        LoadY { dst: 0, index: 0 },
        Const { dst: 1, value: 0. },
        LoadY { dst: 2, index: 1 },
        Const { dst: 3, value: 2. },
        LoadIndexedRegister {
            dst: 4,
            base: 0,
            stride: 2,
            dimensions: vec![2].into(),
            indices: vec![TensorIndex::Runtime(3)].into(),
        },
        LoadIndexedSeed {
            dst: 5,
            base: 0,
            count: 2,
            index: 3,
        },
        Binary {
            dst: 6,
            op: BinaryOp::Mul,
            lhs: 4,
            rhs: 5,
        },
        StoreOutput { src: 6 },
    ];
    let block = ScalarProgramBlock::with_output_indices(vec![row], vec![span()], vec![0]).unwrap();
    let mut gy = [0.; 2];
    let mut gs = [0.; 2];
    pullback(
        &block,
        &[2., 3.],
        RowEvalContext {
            seed: Some(&[4., 5.]),
            ..Default::default()
        },
        &[1.],
        &mut gy,
        &mut gs,
    )
    .unwrap();
    assert_eq!(gy, [0., 5.]);
    assert_eq!(gs, [0., 3.]);
}

#[test]
fn akima_value_and_slope_pullbacks_follow_the_actual_interpolant() {
    let table = rumoca_core::ExternalTableData {
        id: 1,
        data: (0..5).map(|i| vec![i as f64, (i * i) as f64]).collect(),
        columns: vec![2],
        smoothness: 2,
        extrapolation: 2,
    };
    let tables = [table];
    let context = RowEvalContext {
        external_tables: Some(&tables),
        ..Default::default()
    };
    let row = vec![
        LoadY { dst: 0, index: 0 },
        Const { dst: 1, value: 1. },
        TableLookup {
            dst: 2,
            table_id: 1,
            column: 1,
            input: 0,
        },
        TableLookupSlope {
            dst: 3,
            table_id: 1,
            column: 1,
            input: 0,
        },
        StoreOutputRange {
            start: 2,
            count: 2,
            stride: 1,
        },
    ];
    let block =
        ScalarProgramBlock::with_output_indices(vec![row], vec![span()], vec![0, 1]).unwrap();
    for (x, expected) in [(1.5, 11.), (-0.5, 0.), (4.5, 8.)] {
        let mut gy = [0.];
        pullback(&block, &[x], context, &[1., 4.], &mut gy, &mut []).unwrap();
        assert!((gy[0] - expected).abs() < 1e-10, "x={x}, gradient={gy:?}");
    }
}

#[test]
fn duplicated_dense_output_uses_the_last_writer_and_checks_gradient_capacity() {
    let rows = vec![
        vec![LoadY { dst: 0, index: 0 }, StoreOutput { src: 0 }],
        vec![LoadY { dst: 0, index: 1 }, StoreOutput { src: 0 }],
    ];
    let block = ScalarProgramBlock::with_output_indices(rows, vec![span(); 2], vec![0, 0]).unwrap();
    let mut gy = [0.; 2];
    pullback(
        &block,
        &[5., 6.],
        Default::default(),
        &[3.],
        &mut gy,
        &mut [],
    )
    .unwrap();
    assert_eq!(gy, [0., 3.]);
    let mut short = [11.];
    assert!(
        pullback(
            &block,
            &[5., 6.],
            Default::default(),
            &[3.],
            &mut short,
            &mut []
        )
        .is_err()
    );
    assert_eq!(short, [11.]);
}

#[test]
fn a_missing_runtime_table_is_atomic_across_rows() {
    let rows = vec![
        vec![LoadY { dst: 0, index: 0 }, StoreOutput { src: 0 }],
        vec![
            Const { dst: 0, value: 1. },
            LoadY { dst: 1, index: 0 },
            TableLookup {
                dst: 2,
                table_id: 0,
                column: 0,
                input: 1,
            },
            StoreOutput { src: 2 },
        ],
    ];
    let block = ScalarProgramBlock::with_output_indices(rows, vec![span(); 2], vec![0, 1]).unwrap();
    let mut gy = [17.];
    let error = pullback(
        &block,
        &[1.],
        Default::default(),
        &[1., 1.],
        &mut gy,
        &mut [],
    )
    .unwrap_err();
    assert!(matches!(
        error,
        EvalSolveError::ExternalTable {
            operation: "lookup",
            ..
        }
    ));
    assert_eq!(gy, [17.]);
}
