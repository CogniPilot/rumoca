use super::capability_tests::take_causality_visits;
use super::*;

fn block(rows: Vec<Vec<LinearOp>>) -> ScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("replaced_programs.mo"),
        0,
        1,
    );
    ScalarProgramBlock::with_source_span(
        rows,
        span.require_provenance("replaced program fixture").unwrap(),
    )
    .unwrap()
}

fn difference(y: usize, p: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: y },
        LinearOp::LoadP { dst: 1, index: p },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

fn offset(y: usize, value: f64) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: y },
        LinearOp::Const { dst: 1, value },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

fn eval(prepared: &PreparedScalarProgramBlock) -> Vec<f64> {
    let mut out = vec![0.0; prepared.len()];
    prepared
        .eval_with_context(
            &[1.0, 2.0, 3.0],
            &[0.5, 0.25],
            0.0,
            RowEvalContext::default(),
            &mut out,
        )
        .unwrap();
    out
}

#[test]
fn only_replaced_programs_are_prepared_and_the_result_matches_a_fresh_preparation() {
    let base = PreparedScalarProgramBlock::new(block(vec![
        difference(0, 0),
        difference(1, 1),
        offset(2, 1.0),
    ]))
    .unwrap();
    let target = block(vec![difference(0, 0), offset(1, 3.0), offset(2, 1.0)]);
    assert_eq!(replaced_programs(base.block(), &target), Some(vec![1]));

    // Preparing the replaced program alone costs as many causality visits as
    // the replacement preparation: no other program is prepared again.
    take_causality_visits();
    PreparedScalarProgramBlock::new(block(vec![offset(1, 3.0)])).unwrap();
    let alone = take_causality_visits();
    let replaced =
        PreparedScalarProgramBlock::with_replaced_programs(&base, target.clone(), &[1]).unwrap();
    assert_eq!(take_causality_visits(), alone);

    let fresh = PreparedScalarProgramBlock::new(target.clone()).unwrap();
    assert!(replaced.block().shares_program_owner(&target));
    assert_eq!(replaced.row_registers, fresh.row_registers);
    assert_eq!(replaced.row_requirements, fresh.row_requirements);
    assert_eq!(
        replaced.row_reverse_y_gradient_supported,
        fresh.row_reverse_y_gradient_supported
    );
    assert_eq!(replaced.row_is_causal, fresh.row_is_causal);
    assert_eq!(replaced.row_parameter_indices, fresh.row_parameter_indices);
    assert_eq!(
        replaced.row_parameter_static_y_gradient_params,
        fresh.row_parameter_static_y_gradient_params
    );
    assert_eq!(replaced.requirements, fresh.requirements);
    assert_eq!(eval(&replaced), eval(&fresh));
    assert_eq!(eval(&replaced), vec![0.5, -1.0, 2.0]);
}

#[test]
fn a_replacement_must_name_exactly_the_differing_programs_of_one_layout() {
    let base =
        PreparedScalarProgramBlock::new(block(vec![difference(0, 0), difference(1, 1)])).unwrap();
    let target = block(vec![difference(0, 0), offset(1, 3.0)]);
    assert!(
        PreparedScalarProgramBlock::with_replaced_programs(&base, target.clone(), &[0]).is_err()
    );
    assert!(PreparedScalarProgramBlock::with_replaced_programs(&base, target, &[]).is_err());
    let longer = block(vec![difference(0, 0), difference(1, 1), offset(1, 3.0)]);
    assert_eq!(replaced_programs(base.block(), &longer), None);
    let same = block(vec![difference(0, 0), difference(1, 1)]);
    assert_eq!(replaced_programs(base.block(), &same), Some(Vec::new()));
}
