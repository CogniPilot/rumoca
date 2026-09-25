use super::*;

impl ScalarProgramRegisterFlow {
    /// Whole-operation input invariance at fixed Y/P/time/external coordinates.
    /// Repeatability of executed effects is a separate application obligation.
    /// Whether every source register of `op` is marked in `readable`.
    pub(crate) fn op_reads_only(op: &LinearOp, op_index: usize, readable: &[bool]) -> bool {
        let mut validation = ScalarProgramValidationCache::default();
        validate_op_sources(op, op_index, readable, None, None, &mut validation).is_ok()
    }

    pub(crate) fn seed_invariant_operations(program: &[LinearOp]) -> Option<Box<[bool]>> {
        let flow = Self::derive(program).ok()?;
        let mut invariant = vec![false; flow.register_count()];
        let mut validation = ScalarProgramValidationCache::default();
        let mut operations = Vec::with_capacity(program.len());
        for (index, op) in program.iter().enumerate() {
            let reads_seed = matches!(
                op,
                LinearOp::LoadSeed { .. }
                    | LinearOp::LoadIndexedSeed { .. }
                    | LinearOp::TensorLoad {
                        seed_start: Some(_),
                        ..
                    }
            );
            let reusable = !reads_seed
                && validate_op_sources(op, index, &invariant, None, None, &mut validation).is_ok();
            if let Some(start) = op.dst_register() {
                invariant[start as usize..start as usize + op.dst_register_count()].fill(reusable);
            }
            operations.push(reusable && op.dst_register().is_some());
        }
        Some(operations.into_boxed_slice())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn seed_invariance_tracks_overwritten_register_versions_and_all_select_inputs() {
        use LinearOp as L;
        let ops = vec![
            L::LoadY { dst: 0, index: 0 },
            L::Move { dst: 1, src: 0 },
            L::LoadSeed { dst: 0, index: 0 },
            L::Move { dst: 2, src: 0 },
            L::Select {
                dst: 3,
                cond: 0,
                if_true: 1,
                if_false: 1,
            },
            L::LoadP { dst: 0, index: 0 },
            L::Move { dst: 4, src: 0 },
            L::StoreOutput { src: 4 },
        ];
        let issued = ScalarProgramRegisterFlow::seed_invariant_operations(&ops).unwrap();
        assert_eq!(
            &*issued,
            &[true, true, false, false, false, true, true, false]
        );
    }

    #[test]
    fn seed_invariance_preserves_complete_tensor_operations() {
        use LinearOp as L;
        let ops = vec![
            L::TensorLoad {
                dst_start: 0,
                input: TensorInputKind::Y,
                input_start: 0,
                count: 2,
                seed_start: None,
                lanes: 2,
            },
            L::TensorTranspose {
                dst_start: 4,
                src_start: 0,
                rows: 1,
                columns: 2,
                element_width: 1,
                lanes: 2,
            },
            L::TensorLoad {
                dst_start: 0,
                input: TensorInputKind::Y,
                input_start: 0,
                count: 2,
                seed_start: Some(0),
                lanes: 2,
            },
            L::TensorTranspose {
                dst_start: 4,
                src_start: 0,
                rows: 1,
                columns: 2,
                element_width: 1,
                lanes: 2,
            },
            L::StoreOutputRange {
                start: 4,
                count: 4,
                stride: 1,
            },
        ];
        let issued = ScalarProgramRegisterFlow::seed_invariant_operations(&ops).unwrap();
        assert_eq!(&*issued, &[true, true, false, false, false]);
    }
}
