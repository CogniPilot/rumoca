use super::*;

fn empty_inputs() -> RowInputs<'static> {
    RowInputs {
        y: &[],
        p: &[],
        t: 0.0,
        seed: None,
        external_tables: &[],
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

#[test]
fn interpreter_rejects_a_malformed_empty_conditional_condition() {
    let result = vec![
        LinearOp::Const { dst: 0, value: 2.0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let program = rumoca_ir_solve::FunctionConditionalProgram {
        owner: None,
        capture_count: 0,
        target_widths: vec![1].into_boxed_slice(),
        result_count: 1,
        arms: vec![rumoca_ir_solve::FunctionConditionalArmProgram {
            condition_register_count: 0,
            result_register_count: 1,
            condition: Vec::new(),
            result: result.clone(),
        }]
        .into_boxed_slice(),
        fallback_register_count: 1,
        fallback: result,
    };
    let row = RowPlan::General(GeneralRowPlan {
        ops: vec![LinearOp::FunctionConditional {
            dst_start: 0,
            capture_start: 0,
            program: Arc::new(program),
        }]
        .into_boxed_slice(),
        reg_count: 1,
        output_srcs: vec![0].into_boxed_slice(),
        input_requirements: InputRequirements::default(),
    });
    let mut scratch = Vec::new();
    let mut output = [0.0];
    let error = execute_row(&row, &mut scratch, empty_inputs(), &mut output)
        .expect_err("a malformed checked conditional must not select its fallback");
    assert!(
        error
            .to_string()
            .contains("produced 0 values instead of one")
    );
}
