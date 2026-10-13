use std::sync::Arc;

use crate::{FunctionConditionalProgram, LinearOp, RegisterVersionProgram, TensorInputKind};

#[test]
fn captured_register_versions_follow_range_pieces_instead_of_tensor_extent() {
    for count in [32_usize, 131_072] {
        let conditional = FunctionConditionalProgram::checked(
            count,
            [1],
            [(
                vec![
                    LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                    LinearOp::StoreOutput { src: 0 },
                ],
                vec![
                    LinearOp::Const { dst: 0, value: 2.0 },
                    LinearOp::StoreOutput { src: 0 },
                ],
            )],
            vec![
                LinearOp::Const { dst: 0, value: 3.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
        )
        .unwrap();
        let operations = vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: TensorInputKind::Y,
                input_start: 0,
                count,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::Move { dst: 0, src: 1 },
            LinearOp::FunctionConditional {
                dst_start: count as u32,
                capture_start: 0,
                program: Arc::new(conditional),
            },
            LinearOp::StoreOutput { src: count as u32 },
        ];
        let plan = RegisterVersionProgram::derive(&operations).unwrap();
        assert!(!plan.single_writer());
        assert_eq!(plan.operations().len(), 4);
        assert_eq!(plan.operations()[2].operands().len(), 2);
        assert_eq!(plan.operations()[2].source(0).unwrap().0, 1);
        assert_eq!(plan.operations()[2].source(count as u32 - 1).unwrap().0, 0);
        assert_eq!(plan.storage_count(), count + 2);
    }
}
