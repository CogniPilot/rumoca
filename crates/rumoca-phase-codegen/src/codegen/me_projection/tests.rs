//! Render-time refusals of the ME projection view.

use rumoca_eval_solve::TargetIsolationProgram;

use super::block::{
    KEEP_MAX_BYTES, grant_retention, isolation_kind, retained_bytes, retained_storage,
};

/// An isolation the evaluator answers but no scalar program reproduces
/// refuses its block before any byte, and never interns an isolator.
///
/// `TargetIsolationProgram::Unrepresentable` arises only when an assignment
/// shape's materialization does not fit its row (a register range overflow or
/// a tensor-affine lane outside its source range), which no shape derived from
/// a lowered model produces today; this pins the refusal the renderer applies
/// if one ever does.
#[test]
fn an_unrepresentable_row_isolation_refuses_its_block() {
    let mut interned = 0;
    let mut intern = || {
        interned += 1;
        3
    };
    let error = isolation_kind(7, &TargetIsolationProgram::Unrepresentable, &mut intern)
        .expect_err("an unrepresentable isolation must refuse the block");
    assert_eq!(
        isolation_kind(
            7,
            &TargetIsolationProgram::Isolator(Vec::new()),
            &mut intern
        )
        .ok(),
        Some((2, 3)),
        "a materialized isolator is interned exactly once"
    );
    let message = error.to_string();
    assert!(
        message.contains("unsupported-feature:algebraic_projection")
            && message.contains("projection block 7")
            && message.contains("row isolation no scalar program reproduces"),
        "{message}"
    );
    for (program, kind) in [
        (TargetIsolationProgram::Unavailable, 0),
        (TargetIsolationProgram::OutputValue, 1),
    ] {
        assert_eq!(
            isolation_kind(7, &program, &mut intern).ok(),
            Some((kind, 0))
        );
    }
    assert_eq!(
        interned, 1,
        "only the materialized isolator is interned; the refused isolation interns nothing"
    );
}

/// Lane seeds are checked in their own units: a `LoadSeed` index is a
/// position among `seed_len * lanes`, a tensor load's seed range is in seeds.
#[test]
fn lane_seed_loads_are_checked_in_their_own_units() {
    use rumoca_ir_solve::{LinearOp, TensorInputKind};

    use super::block::check_seed_loads;

    let (seed_len, lanes) = (4, 3);
    let load = |index| vec![LinearOp::LoadSeed { dst: 0, index }];
    let tensor = |seed_start| {
        vec![LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 2,
            seed_start: Some(seed_start),
            lanes: lanes + 1,
        }]
    };
    assert!(check_seed_loads(0, &load(seed_len * lanes - 1), seed_len, lanes).is_ok());
    assert!(check_seed_loads(0, &load(seed_len * lanes), seed_len, lanes).is_err());
    assert!(check_seed_loads(0, &tensor(seed_len - 2), seed_len, lanes).is_ok());
    // Within `seed_len * lanes` positions but past the last seed.
    assert!(check_seed_loads(0, &tensor(seed_len - 1), seed_len, lanes).is_err());
}

/// A dispatched program stays out of line past the inlining bound only, so a
/// small program may be inlined into its case and a large one keeps its own
/// function.
#[test]
fn a_dispatched_program_stays_out_of_line_only_past_the_inlining_bound() {
    let program = |ops: usize| {
        minijinja::context! {
            temporary_count => 1,
            ops => vec![minijinja::context! { kind => "Move", dst => 0, src => 0 }; ops],
        }
    };
    let mut environment = crate::codegen::create_environment();
    environment
        .add_template(
            "inlining.c",
            r#"{%- from "fmi-me-projection.jinja" import program_function -%}
{{ program_function("rmc_row_p0", small, "ModelInstance* m, double* o", false, true) }}
{{ program_function("rmc_row_p1", large, "ModelInstance* m, double* o", false, true) }}"#,
        )
        .expect("the inlining template parses");
    let rendered = environment
        .get_template("inlining.c")
        .expect("the inlining template")
        .render(minijinja::context! {
            small => program(super::INLINE_PROGRAM_MAX_OPS),
            large => program(super::INLINE_PROGRAM_MAX_OPS + 1),
        })
        .expect("render the two programs");
    assert!(rendered.contains("static int rmc_row_p0("), "{rendered}");
    assert!(
        rendered.contains("static RMC_NOINLINE int rmc_row_p1("),
        "{rendered}"
    );
}

/// Isolator groups slice within a bound on their code: slots that each
/// repeat a long shared prefix stay one shared group, while slots that each
/// read their own element of a tensor operation, as a ladder's do, slice.
mod isolation_slices {
    use rumoca_ir_solve::{BinaryOp, LinearOp, TensorInputKind};

    use super::super::table::isolation_slices;

    /// A chain of `prefix` products every one of `slots` stores reads, each
    /// slot adding its own parameter.
    fn shared_prefix(prefix: u32, slots: u32) -> Vec<LinearOp> {
        let mut program = vec![LinearOp::LoadY { dst: 0, index: 0 }];
        for k in 1..=prefix {
            program.push(LinearOp::Binary {
                dst: k,
                op: BinaryOp::Mul,
                lhs: k - 1,
                rhs: 0,
            });
        }
        for slot in 0..slots {
            let base = 100 + 2 * slot;
            program.extend([
                LinearOp::LoadP {
                    dst: base,
                    index: slot as usize,
                },
                LinearOp::Binary {
                    dst: base + 1,
                    op: BinaryOp::Add,
                    lhs: prefix,
                    rhs: base,
                },
                LinearOp::StoreOutput { src: base + 1 },
            ]);
        }
        program
    }

    /// A load of `slots` elements, then for each slot a chain of `chain`
    /// products of its own element, which it stores: the shape of a ladder
    /// cell's isolations.
    fn per_slot_chains(slots: usize, chain: u32) -> Vec<LinearOp> {
        let mut program = vec![LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: slots,
            seed_start: None,
            lanes: 1,
        }];
        for slot in 0..slots as u32 {
            let base = 1000 + slot * (chain + 1);
            program.push(LinearOp::Move {
                dst: base,
                src: slot,
            });
            for k in 1..=chain {
                program.push(LinearOp::Binary {
                    dst: base + k,
                    op: BinaryOp::Mul,
                    lhs: base + k - 1,
                    rhs: slot,
                });
            }
            program.push(LinearOp::StoreOutput { src: base + chain });
        }
        program
    }

    #[test]
    fn slots_sharing_a_long_prefix_stay_one_group() {
        let program = shared_prefix(40, 8);
        let slices = isolation_slices(&program, 8);
        assert_eq!(slices.len(), 8);
        assert!(slices.iter().all(Option::is_none), "{slices:?}");
        // Two slots repeat the prefix within the bound and slice.
        assert!(
            isolation_slices(&shared_prefix(40, 2), 2)
                .iter()
                .all(Option::is_some)
        );
    }

    #[test]
    fn slots_reading_their_own_elements_slice() {
        let program = per_slot_chains(50, 18);
        let slices = isolation_slices(&program, 50);
        assert!(slices.iter().all(Option::is_some));
        let code: usize = slices.iter().flatten().map(Vec::len).sum();
        assert!(code <= 2 * program.len(), "{code} for {}", program.len());
        assert!(slices.iter().flatten().all(|slice| slice.len() == 21));
    }
}

/// A block alone fits the retention budget as `KEEP_MAX_BYTES` states: a
/// dense LU up to 208 unknowns with a full Jacobian and 254 with a
/// tridiagonal one, and an elimination whatever the size of its block.
#[test]
fn a_dense_affine_block_over_the_retention_budget_keeps_nothing() {
    let fits = |n: usize, nnz: usize| {
        let storage = retained_storage(n, nnz, None).expect("no overflow");
        retained_bytes(storage).expect("no overflow") <= KEEP_MAX_BYTES
    };
    assert!(fits(208, 208 * 208) && !fits(209, 209 * 209));
    assert!(fits(254, 3 * 254 - 2) && !fits(255, 3 * 255 - 2));
    let elimination = retained_storage(300, 3 * 300 - 2, Some((4, 296))).expect("no overflow");
    assert!(retained_bytes(elimination).expect("no overflow") <= KEEP_MAX_BYTES);
}

/// A size that overflows `usize`, as a large block can on a 32-bit host,
/// retains nothing rather than a wrapped, too small storage.
#[test]
fn an_overflowing_retained_size_keeps_nothing() {
    let huge = usize::MAX / 2;
    assert_eq!(retained_storage(huge, 3, None), None);
    assert_eq!(
        retained_storage(4, huge, None).and_then(retained_bytes),
        None
    );
    assert_eq!(retained_storage(4, 10, Some((huge, 2))), None);
    assert_eq!(retained_bytes((usize::MAX / 8, 1)), None);
    assert_eq!(
        grant_retention(&[Some((usize::MAX, 0))]),
        (vec![None], (0, 0))
    );
}

/// The instance budget is shared: blocks are granted in order while their
/// storage still fits with the ones before them, a block that does not fit
/// keeps nothing, and a later smaller one still fits after the granted ones.
#[test]
fn retaining_blocks_share_one_instance_budget() {
    let words = |bytes: usize| (bytes / 8 - 1, 1);
    let (offsets, total) = grant_retention(&[
        Some(words(600 << 10)),
        None,
        Some(words(600 << 10)),
        Some(words(300 << 10)),
    ]);
    let first = words(600 << 10);
    assert_eq!(offsets, vec![Some((0, 0)), None, None, Some(first)]);
    assert_eq!(total, (first.0 + words(300 << 10).0, 2));
    assert!(retained_bytes(total).expect("no overflow") <= KEEP_MAX_BYTES);
}
