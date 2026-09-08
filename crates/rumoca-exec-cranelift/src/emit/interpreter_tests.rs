use super::*;

fn empty_inputs() -> RowInputs<'static> {
    RowInputs {
        y: &[],
        p: &[],
        t: 0.0,
        seed: None,
    }
}

#[test]
fn interpreter_rejects_discrete_random_ops_instead_of_inventing_a_value() {
    let row = RowPlan::General(GeneralRowPlan {
        ops: vec![LinearOp::ImpureRandomInit { dst: 0, seed: 0 }].into_boxed_slice(),
        reg_count: 1,
        output_srcs: vec![0].into_boxed_slice(),
        input_requirements: InputRequirements::default(),
    });
    let mut scratch = Vec::new();
    let mut output = [0.0];
    let error = execute_row(&row, &mut scratch, empty_inputs(), &mut output)
        .expect_err("unsupported random execution must fail closed");
    assert!(
        error
            .to_string()
            .contains("does not support discrete random")
    );
}
