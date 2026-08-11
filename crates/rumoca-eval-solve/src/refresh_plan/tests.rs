use super::*;
use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};

fn checked(program: Vec<solve::LinearOp>) -> Vec<solve::LinearOp> {
    solve::ScalarProgramRegisterFlow::derive(&program)
        .expect("parameter-static fixture must be a checked register program");
    program
}

fn certifies(program: &[solve::LinearOp]) -> bool {
    parameter_static_refresh_program(program, 10, 2, &BTreeSet::from([11, 12]))
}

#[test]
fn compact_tensor_inputs_preserve_the_parameter_static_certificate() {
    let parameter_tensor = checked(vec![
        solve::LinearOp::TensorLoad {
            dst_start: 0,
            input: solve::TensorInputKind::P,
            input_start: 4,
            count: 3,
            seed_start: None,
            lanes: 1,
        },
        solve::LinearOp::TensorFill {
            dst_start: 3,
            value_start: 0,
            count: 3,
            lanes: 1,
        },
        solve::LinearOp::StoreOutputRange {
            start: 3,
            count: 3,
            stride: 1,
        },
    ]);
    assert!(certifies(&parameter_tensor));

    let certified_y_tensor = checked(vec![
        solve::LinearOp::TensorLoad {
            dst_start: 0,
            input: solve::TensorInputKind::Y,
            input_start: 10,
            count: 3,
            seed_start: None,
            lanes: 1,
        },
        solve::LinearOp::StoreOutput { src: 0 },
    ]);
    assert!(certifies(&certified_y_tensor));

    let dynamic_y_tensor = checked(vec![
        solve::LinearOp::TensorLoad {
            dst_start: 0,
            input: solve::TensorInputKind::Y,
            input_start: 10,
            count: 4,
            seed_start: None,
            lanes: 1,
        },
        solve::LinearOp::StoreOutput { src: 0 },
    ]);
    assert!(!certifies(&dynamic_y_tensor));
}

#[test]
fn seed_dependent_tensor_load_is_never_parameter_static() {
    let seeded_parameter_tensor = checked(vec![
        solve::LinearOp::TensorLoad {
            dst_start: 0,
            input: solve::TensorInputKind::P,
            input_start: 4,
            count: 1,
            seed_start: Some(0),
            lanes: 2,
        },
        solve::LinearOp::StoreOutput { src: 1 },
    ]);
    assert!(!certifies(&seeded_parameter_tensor));
}

#[test]
fn compact_fold_owner_is_certified_without_domain_expansion() {
    let fold = solve::FunctionFoldProgram::checked(
        StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 3,
                step: 1,
            }],
        },
        1,
        1,
        vec![
            solve::LinearOp::LoadFoldCarried { dst: 0, index: 0 },
            solve::LinearOp::LoadFoldCapture { dst: 1, index: 0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ],
    )
    .expect("compact fold fixture has a checked carried/capture ABI");
    let program = checked(vec![
        solve::LinearOp::LoadP { dst: 0, index: 4 },
        solve::LinearOp::Const { dst: 1, value: 0.0 },
        solve::LinearOp::FunctionFold {
            dst_start: 2,
            initial_start: 1,
            capture_start: 0,
            program: Arc::new(fold),
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]);
    assert!(certifies(&program));
}

#[test]
fn lazy_conditional_regions_are_recursively_fail_closed() {
    let conditional = solve::FunctionConditionalProgram::checked(
        0,
        [1],
        [(
            vec![
                solve::LinearOp::LoadTime { dst: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        )],
        vec![
            solve::LinearOp::Const { dst: 0, value: 0.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
    )
    .expect("lazy conditional fixture has checked correlated regions");
    let program = checked(vec![
        solve::LinearOp::FunctionConditional {
            dst_start: 0,
            capture_start: 0,
            program: Arc::new(conditional),
        },
        solve::LinearOp::StoreOutput { src: 0 },
    ]);
    assert!(!certifies(&program));
}
