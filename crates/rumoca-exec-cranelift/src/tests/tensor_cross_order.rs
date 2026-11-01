//! Already numerical root lanes retain the reference instruction's term order.
use super::*;
use rumoca_ir_solve::TensorInputKind;

#[test]
fn root_cross_tangent_keeps_the_original_four_term_order() {
    let source = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: TensorInputKind::Y,
                input_start: 0,
                count: 12,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::TensorCross {
                dst_start: 12,
                lhs_start: 0,
                rhs_start: 6,
                lanes: 2,
            },
            LinearOp::StoreOutput { src: 13 },
        ]],
        vec![fixture_span()],
    )
    .unwrap();
    // Component zero has terms 1e16, 1, 1e16, 1. Ordered subtraction
    // returns -1; regrouping the two negative terms returns zero.
    let packed = [0.0, 0.0, 1.0, 1e16, 1.0, 1e16, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0];
    let mut expected = [0.0];
    rumoca_eval_solve::eval_scalar_program_block(&source, &packed, &[], 0.0, None, &mut expected)
        .unwrap();
    assert_eq!(expected[0].to_bits(), (-1.0_f64).to_bits());
    let compiled = compile_expression_scalar_program_block(&source).unwrap();
    let mut actual = [0.0];
    compiled.call(&packed, &[], 0.0, &mut actual).unwrap();
    assert_eq!(actual.map(f64::to_bits), expected.map(f64::to_bits));
}
