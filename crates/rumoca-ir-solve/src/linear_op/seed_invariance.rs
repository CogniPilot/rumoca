use super::*;

impl ScalarProgramRegisterFlow {
    /// Whether every source register of `op` is marked in `readable`.
    pub fn op_reads_only(op: &LinearOp, op_index: usize, readable: &[bool]) -> bool {
        let readable = DefinedRegisters::from_flags(readable);
        let mut validation = ScalarProgramValidationCache::default();
        validate_op_sources(op, op_index, &readable, None, None, &mut validation).is_ok()
    }

    /// Whole-operation input invariance at fixed Y/P/time/external coordinates.
    /// Repeatability of executed effects is a separate application obligation.
    pub(crate) fn seed_invariant_operations(program: &[LinearOp]) -> Option<Box<[bool]>> {
        Self::derive(program).ok()?;
        let mut invariant = DefinedRegisters::default();
        let mut validation = ScalarProgramValidationCache::default();
        let mut operations = Vec::with_capacity(program.len());
        for (index, op) in program.iter().enumerate() {
            let reusable = !loads_seed(op)
                && validate_op_sources(op, index, &invariant, None, None, &mut validation).is_ok();
            if let Some(start) = op.dst_register() {
                invariant.set(start, op.dst_register_count(), reusable);
            }
            operations.push(reusable && op.dst_register().is_some());
        }
        Some(operations.into_boxed_slice())
    }
}

/// Whether `op` loads a seed itself or in one of its nested regions.
///
/// A fold update and each region of a function conditional may load seeds
/// directly, not only through the registers the operation reads, so a nested
/// program that does is as seed-dependent as a top-level seed load.
fn loads_seed(op: &LinearOp) -> bool {
    match op {
        LinearOp::LoadSeed { .. }
        | LinearOp::LoadIndexedSeed { .. }
        | LinearOp::TensorLoad {
            seed_start: Some(_),
            ..
        } => true,
        LinearOp::FunctionFold { program, .. }
        | LinearOp::GuardedFunctionFold { program, .. }
        | LinearOp::StoreOutputFunctionFold { program, .. } => program
            .regions()
            .any(|region| region.iter().any(loads_seed)),
        LinearOp::FunctionConditional { program, .. } => {
            program.arms.iter().any(|arm| {
                arm.condition.iter().any(loads_seed) || arm.result.iter().any(loads_seed)
            }) || program.fallback.iter().any(loads_seed)
        }
        _ => false,
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

    /// A conditional choosing `if capture then <arm> else 3`, its capture read
    /// from register `capture`, writing register `dst`.
    fn conditional(dst: u32, capture: u32, arm: LinearOp) -> LinearOp {
        let condition = vec![
            LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ];
        let selected = vec![arm, LinearOp::StoreOutput { src: 0 }];
        let fallback = vec![
            LinearOp::Const { dst: 0, value: 3.0 },
            LinearOp::StoreOutput { src: 0 },
        ];
        let program =
            crate::FunctionConditionalProgram::checked(1, [1], [(condition, selected)], fallback)
                .expect("a checked conditional");
        LinearOp::FunctionConditional {
            dst_start: dst,
            capture_start: capture,
            program: std::sync::Arc::new(program),
        }
    }

    /// A conditional region that loads a seed makes the conditional, and what
    /// reads its result, seed-dependent even though every register the
    /// conditional reads is seed-invariant.
    #[test]
    fn a_seed_load_inside_a_conditional_region_is_not_seed_invariant() {
        use LinearOp as L;
        let ops = vec![
            L::LoadY { dst: 0, index: 0 },
            conditional(1, 0, L::LoadSeed { dst: 0, index: 0 }),
            L::Move { dst: 2, src: 1 },
            L::StoreOutput { src: 2 },
        ];
        let issued = ScalarProgramRegisterFlow::seed_invariant_operations(&ops).unwrap();
        assert_eq!(&*issued, &[true, false, false, false]);

        let constant = vec![
            L::LoadY { dst: 0, index: 0 },
            conditional(1, 0, L::Const { dst: 0, value: 2.0 }),
            L::Move { dst: 2, src: 1 },
            L::StoreOutput { src: 2 },
        ];
        let issued = ScalarProgramRegisterFlow::seed_invariant_operations(&constant).unwrap();
        assert_eq!(
            &*issued,
            &[true, true, true, false],
            "a conditional over seed-invariant values stays invariant"
        );
    }

    /// The same holds for a fold whose update loads a seed.
    #[test]
    fn a_seed_load_inside_a_fold_update_is_not_seed_invariant() {
        use LinearOp as L;
        let fold = crate::FunctionFoldProgram::checked(
            crate::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    provenance: rumoca_core::IndexBinderProvenance::Generated,
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 2,
                    step: 1,
                }],
            },
            1,
            0,
            vec![
                L::LoadFoldCarried { dst: 0, index: 0 },
                L::LoadSeed { dst: 1, index: 0 },
                L::Binary {
                    dst: 2,
                    op: BinaryOp::Add,
                    lhs: 0,
                    rhs: 1,
                },
                L::StoreOutput { src: 2 },
            ],
        )
        .expect("a checked fold");
        let ops = vec![
            L::LoadY { dst: 0, index: 0 },
            L::FunctionFold {
                dst_start: 1,
                initial_start: 0,
                capture_start: 0,
                program: std::sync::Arc::new(fold),
            },
            L::StoreOutput { src: 1 },
        ];
        let issued = ScalarProgramRegisterFlow::seed_invariant_operations(&ops).unwrap();
        assert_eq!(&*issued, &[true, false, false]);
    }
}
