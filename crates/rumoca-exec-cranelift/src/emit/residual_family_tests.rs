use super::*;

#[test]
fn residual_family_shares_arena_but_keeps_complete_entries_and_lifetime() {
    let safe = vec![vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ]];
    let faulty = vec![vec![
        LinearOp::LoadY { dst: 0, index: 2 },
        LinearOp::StoreOutput { src: 0 },
    ]];
    let mut entries = compile_residual_family(&[&safe, &faulty], None).unwrap();
    assert!(
        std::ptr::eq(&*entries[0]._module, &*entries[1]._module),
        "one module must own the complete family"
    );
    let mut out = [-99.0];
    entries[0].call(&[7.0], &[], 0.0, &mut out).unwrap();
    assert_eq!(out, [7.0]);
    assert!(entries[1].call(&[7.0], &[], 0.0, &mut out).is_err());
    assert_eq!(out, [7.0]);
    let survivor = entries.remove(0);
    drop(entries);
    survivor.call(&[11.0], &[], 0.0, &mut out).unwrap();
    assert_eq!(out, [11.0]);
}

#[test]
fn residual_family_scopes_same_conditional_owner_to_each_source_block() {
    fn block(value: f64) -> Vec<Vec<LinearOp>> {
        let conditional = rumoca_ir_solve::FunctionConditionalProgram::checked_owned(
            rumoca_ir_solve::FunctionConditionalOwnerId::checked(7).unwrap(),
            0,
            [1],
            [(
                vec![
                    LinearOp::LoadP { dst: 0, index: 0 },
                    LinearOp::StoreOutput { src: 0 },
                ],
                vec![
                    LinearOp::Const { dst: 0, value },
                    LinearOp::StoreOutput { src: 0 },
                ],
            )],
            vec![
                LinearOp::Const {
                    dst: 0,
                    value: -value,
                },
                LinearOp::StoreOutput { src: 0 },
            ],
        )
        .unwrap();
        vec![vec![
            LinearOp::FunctionConditional {
                dst_start: 0,
                capture_start: 0,
                program: std::sync::Arc::new(conditional),
            },
            LinearOp::StoreOutput { src: 0 },
        ]]
    }
    let first = block(7.0);
    let second = block(13.0);
    let entries = compile_residual_family(&[&first, &second], None).unwrap();
    for (entry, value) in entries.iter().zip([7.0, 13.0]) {
        let mut out = [99.0];
        entry.call(&[], &[1.0], 0.0, &mut out).unwrap();
        assert_eq!(out, [value]);
        entry.call(&[], &[0.0], 0.0, &mut out).unwrap();
        assert_eq!(out, [-value]);
    }
}

#[test]
fn residual_family_compile_failure_names_entry_and_preserves_other_arenas() {
    let safe = vec![vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ]];
    let bad = vec![vec![
        LinearOp::LoadSeed { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ]];
    let survivor = compile_residual_family(&[&safe], None).unwrap().remove(0);
    let error = compile_residual_family(&[&safe, &bad], None).err().unwrap();
    assert_eq!(error.0, Some(1));
    let mut out = [-1.0];
    survivor.call(&[13.0], &[], 0.0, &mut out).unwrap();
    assert_eq!(out, [13.0]);
}

fn constant_fold(value: f64) -> Arc<rumoca_ir_solve::FunctionFoldProgram> {
    fold_program(vec![
        LinearOp::Const { dst: 0, value },
        LinearOp::StoreOutput { src: 0 },
    ])
}

fn fold_program(update: Vec<LinearOp>) -> Arc<rumoca_ir_solve::FunctionFoldProgram> {
    let binder = rumoca_core::StructuredIndexBinder {
        provenance: rumoca_core::IndexBinderProvenance::Generated,
        id: 0,
        display_name: "i".into(),
        lower: 1,
        upper: 1,
        step: 1,
    };
    Arc::new(
        rumoca_ir_solve::FunctionFoldProgram::checked(
            rumoca_core::StructuredIndexDomain {
                binders: vec![binder],
            },
            1,
            0,
            update,
        )
        .unwrap(),
    )
}

fn fold_row(program: &Arc<rumoca_ir_solve::FunctionFoldProgram>) -> Vec<LinearOp> {
    vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::FunctionFold {
            dst_start: 1,
            initial_start: 0,
            capture_start: 0,
            program: program.clone(),
        },
        LinearOp::StoreOutput { src: 1 },
    ]
}

fn output_bits(entry: &CompiledResidualRows, outputs: usize) -> Vec<u64> {
    let mut out = vec![17.0; outputs];
    entry.call(&[], &[], 0.0, &mut out).unwrap();
    out.iter().map(|value| value.to_bits()).collect()
}

/// FAMILY-FOLD-BITS1: `+0.0` and `-0.0` fold programs compare equal, and a
/// family must still return what each block returns compiled alone.
#[test]
fn residual_family_keeps_signed_zero_folds_of_separate_blocks_apart() {
    let positive = vec![fold_row(&constant_fold(0.0))];
    let negative = vec![fold_row(&constant_fold(-0.0))];
    let alone =
        [&positive, &negative].map(|rows| output_bits(&compile_residual_rows(rows).unwrap(), 1));
    assert_eq!(alone, [vec![0], vec![0x8000_0000_0000_0000]]);
    let family = compile_residual_family(&[&positive, &negative], None).unwrap();
    let shared: Vec<_> = family.iter().map(|entry| output_bits(entry, 1)).collect();
    assert_eq!(shared, alone);
}

/// A program compiled in an earlier block is reused only where the block
/// compiled alone would compile it afresh; an in-block structural match
/// still decides first, exactly as in separate compilation.
#[test]
fn residual_family_fold_reuse_matches_each_block_compiled_alone() {
    let negative = constant_fold(-0.0);
    let positive = constant_fold(0.0);
    let first = vec![fold_row(&negative)];
    let second = vec![fold_row(&positive), fold_row(&negative)];
    let third = vec![fold_row(&negative), fold_row(&positive)];
    let blocks: [&[Vec<LinearOp>]; 3] = [&first, &second, &third];
    let family = compile_residual_family(&blocks, None).unwrap();
    for (rows, entry) in blocks.iter().zip(&family) {
        let alone = compile_residual_rows(rows).unwrap();
        assert_eq!(
            output_bits(entry, rows.len()),
            output_bits(&alone, rows.len())
        );
    }
}

fn assert_family_matches_alone(blocks: &[&[Vec<LinearOp>]]) {
    let family = compile_residual_family(blocks, None).unwrap();
    for (rows, entry) in blocks.iter().zip(&family) {
        let alone = compile_residual_rows(rows).unwrap();
        assert_eq!(
            output_bits(entry, rows.len()),
            output_bits(&alone, rows.len())
        );
    }
}

/// FAMILY-FOLD-NESTED-SCOPE1: an outer fold's kernel depends on the
/// structural matches made while compiling its nested fold, so a later block
/// must not reuse it; each block reproduces its separate compilation.
#[test]
fn residual_family_nested_folds_match_each_block_compiled_alone() {
    let positive = constant_fold(0.0);
    let negative = constant_fold(-0.0);

    // Reused outer +0.0 fold, then a -0.0 inner fold in the same block.
    let outer = fold_program(fold_row(&positive));
    let first = vec![fold_row(&outer)];
    let second = vec![fold_row(&outer), fold_row(&negative)];
    assert_family_matches_alone(&[&first, &second]);

    // Outer -0.0 fold first compiled after a +0.0 inner fold, then alone.
    let outer_negative = fold_program(fold_row(&negative));
    let first = vec![fold_row(&positive), fold_row(&outer_negative)];
    let second = vec![fold_row(&outer_negative)];
    assert_family_matches_alone(&[&first, &second]);
}
