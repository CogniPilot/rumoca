//! Bounded composition checks over original candidate instructions, not facts.

mod fact_contract;

use super::{check_full_jacobian, check_kernel, check_visible_rows, project_operations};
use rumoca_ir_solve::LinearOp;

fn original_kernel_shape(programs: &[Vec<LinearOp>], expected_bits: u64) -> bool {
    let [program] = programs else {
        return false;
    };
    matches!(program.as_slice(), [
        LinearOp::Const { dst: 0, value },
        LinearOp::StoreOutput { src: 0 },
    ] if value.to_bits() == expected_bits)
}

fn original_jacobian_shape(programs: &[Vec<LinearOp>], expected_bits: u64) -> bool {
    let [program] = programs else {
        return false;
    };
    matches!(program.as_slice(), [
        LinearOp::Const { dst: 0, value: primal },
        LinearOp::Const { dst: 1, value: tangent },
        LinearOp::StoreOutput { src: 1 },
    ] if primal.to_bits() == expected_bits && tangent.to_bits() == 0)
}

fn original_visible_shape(programs: &[Vec<LinearOp>]) -> bool {
    let [program] = programs else {
        return false;
    };
    matches!(
        program.as_slice(),
        [
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ]
    )
}

/// All three calls use the shipping selector, opcode projection and checkers.
/// Expected acceptance is derived separately from complete original programs.
fn assert_projection_contract(programs: &[Vec<LinearOp>], bits: u64) -> (bool, bool, bool) {
    let kernel = check_kernel(project_operations(programs), bits).is_ok();
    let jacobian = check_full_jacobian(project_operations(programs), bits).is_ok();
    let visible = check_visible_rows(project_operations(programs)).is_ok();
    assert_eq!(kernel, original_kernel_shape(programs, bits));
    assert_eq!(jacobian, original_jacobian_shape(programs, bits));
    assert_eq!(visible, original_visible_shape(programs));
    (kernel, jacobian, visible)
}

#[cfg(kani)]
mod proof {
    use super::{LinearOp, assert_projection_contract};

    // These six kinds are the declared domain, not an arbitrary LinearOp
    // generator. Other instruction kinds receive no proof credit here.
    fn candidate_operation() -> LinearOp {
        let kind: u8 = kani::any();
        kani::assume(kind < 6);
        let register = kani::any();
        let index = kani::any();
        let bits = kani::any();
        match kind {
            0 => LinearOp::Const {
                dst: register,
                value: f64::from_bits(bits),
            },
            1 => LinearOp::LoadY {
                dst: register,
                index,
            },
            2 => LinearOp::StoreOutput { src: register },
            3 => LinearOp::LoadP {
                dst: register,
                index,
            },
            4 => LinearOp::LoadTime { dst: register },
            _ => LinearOp::LoadSeed {
                dst: register,
                index,
            },
        }
    }

    fn candidate_program() -> Vec<LinearOp> {
        let length: u8 = kani::any();
        kani::assume(length <= 4);
        let mut program = Vec::new();
        for _ in 0..length {
            program.push(candidate_operation());
        }
        program
    }

    #[kani::proof]
    #[kani::unwind(8)]
    fn bounded_c61_instruction_projection_matches_original_programs() {
        let count: u8 = kani::any();
        kani::assume(count <= 2);
        let mut programs = Vec::new();
        for _ in 0..count {
            programs.push(candidate_program());
        }
        let (kernel, jacobian, visible) = assert_projection_contract(&programs, kani::any());
        kani::cover!(kernel, "constant derivative program accepts");
        kani::cover!(jacobian, "constant derivative JVP accepts");
        kani::cover!(visible, "state-visible program accepts");
        kani::cover!(count == 2, "multiple programs are rejected");
        kani::cover!(
            matches!(programs.as_slice(), [program] if program.len() == 4),
            "an overlong program is rejected"
        );
        kani::cover!(
            matches!(programs.as_slice(), [program] if matches!(program.as_slice(), [
                LinearOp::LoadP { dst: 0, index: 0 }, LinearOp::StoreOutput { src: 0 }
            ])),
            "a parameter load cannot impersonate the state-visible program"
        );
        // The subjects borrow these candidates. Their input/output contract
        // ends before caller-owned input teardown; the recursive destructor
        // for other LinearOp variants is a separate, unclaimed obligation.
        // Retain the same generated inputs and all assertions above.
        std::mem::forget(programs);
    }
}

#[cfg(all(test, not(kani)))]
mod tests {
    use super::{LinearOp, assert_projection_contract};

    fn constant(dst: u32, bits: u64) -> LinearOp {
        LinearOp::Const {
            dst,
            value: f64::from_bits(bits),
        }
    }

    #[test]
    fn actual_projection_preserves_constant_bits_and_distinguishes_signed_zero() {
        for bits in [
            0,
            1,
            1.0_f64.to_bits(),
            1 << 63,
            0x7ff8_0000_0000_0042,
            u64::MAX,
        ] {
            let kernel = vec![vec![constant(0, bits), LinearOp::StoreOutput { src: 0 }]];
            assert_eq!(
                assert_projection_contract(&kernel, bits),
                (true, false, false)
            );
            assert_eq!(
                assert_projection_contract(&kernel, bits ^ 1),
                (false, false, false)
            );
            let jvp = vec![vec![
                constant(0, bits),
                constant(1, 0),
                LinearOp::StoreOutput { src: 1 },
            ]];
            assert_eq!(assert_projection_contract(&jvp, bits), (false, true, false));
            let negative_zero = vec![vec![
                constant(0, bits),
                constant(1, 1 << 63),
                LinearOp::StoreOutput { src: 1 },
            ]];
            assert_eq!(
                assert_projection_contract(&negative_zero, bits),
                (false, false, false)
            );
        }
    }

    #[test]
    fn actual_projection_preserves_complete_program_shape() {
        let bits = 1.0_f64.to_bits();
        assert_eq!(assert_projection_contract(&[], bits), (false, false, false));
        assert_eq!(
            assert_projection_contract(&[vec![]], bits),
            (false, false, false)
        );
        let overlong = vec![vec![
            constant(0, bits),
            LinearOp::StoreOutput { src: 0 },
            constant(1, 0),
            constant(2, 0),
        ]];
        assert_eq!(
            assert_projection_contract(&overlong, bits),
            (false, false, false)
        );
        let multiple = vec![
            vec![constant(0, bits), LinearOp::StoreOutput { src: 0 }],
            vec![],
        ];
        assert_eq!(
            assert_projection_contract(&multiple, bits),
            (false, false, false)
        );
    }

    #[test]
    fn actual_projection_rejects_wrong_storage_and_registers() {
        for load in [
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::LoadTime { dst: 0 },
            LinearOp::LoadSeed { dst: 0, index: 0 },
            LinearOp::LoadY {
                dst: 0,
                index: usize::MAX,
            },
            LinearOp::LoadY {
                dst: u32::MAX,
                index: 0,
            },
        ] {
            let is_state = matches!(load, LinearOp::LoadY { dst: 0, index: 0 });
            let programs = vec![vec![load, LinearOp::StoreOutput { src: 0 }]];
            assert_eq!(
                assert_projection_contract(&programs, 0),
                (false, false, is_state)
            );
        }
        let wrong_register = vec![vec![
            constant(0, 0),
            LinearOp::StoreOutput { src: u32::MAX },
        ]];
        assert_eq!(
            assert_projection_contract(&wrong_register, 0),
            (false, false, false)
        );
    }
}
