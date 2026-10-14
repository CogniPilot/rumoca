use super::*;

#[test]
fn parent_cells_stay_stable_across_site_and_nested_frame_growth() {
    let mut scratch = NativeScratch::default();
    scratch.enter().unwrap();
    let parent = scratch.allocate(0, 512 * 512).unwrap();
    let exact = [i64::MAX as u64, (-0.0_f64).to_bits(), 0x7ff8_0000_0000_0123];
    // SAFETY: the active site's allocation contains these three cells. Arena
    // metadata growth below never moves or borrows that allocation's elements.
    unsafe { std::ptr::copy_nonoverlapping(exact.as_ptr(), parent, exact.len()) };
    for site in 1..257 {
        scratch.allocate(site, 1).unwrap();
    }
    for depth in 0..128 {
        scratch.enter().unwrap();
        scratch.allocate(0, depth + 1).unwrap();
    }
    for _ in 0..128 {
        scratch.leave();
    }
    assert_eq!(scratch.allocate(0, 512 * 512), Some(parent));
    assert!(scratch.allocate(0, 512 * 512 + 1).is_none());
    // SAFETY: the parent frame is still active with the same exact extent.
    assert_eq!(
        unsafe { std::slice::from_raw_parts(parent, exact.len()) },
        exact
    );
    scratch.leave();
    assert_eq!(scratch.depth, 0);
    assert!(!scratch.invalid);
}

#[test]
fn repeated_fold_sites_reuse_storage_and_inactive_frames_may_change_shape() {
    let mut scratch = NativeScratch::default();
    scratch.enter().unwrap();
    let first = scratch.allocate(0, 4096).unwrap();
    for _ in 0..10_000 {
        assert_eq!(scratch.allocate(0, 4096), Some(first));
    }
    assert!(scratch.frames.is_empty());
    assert_eq!(scratch.root.buffers.len(), 1);
    scratch.leave();
    scratch.enter().unwrap();
    scratch.allocate(0, 17).unwrap();
    assert_eq!(scratch.root.buffers[0].cells.len(), 17);
    scratch.leave();
}

#[test]
fn invalid_physical_requests_refuse_without_losing_the_active_frame() {
    let mut scratch = NativeScratch::default();
    assert!(scratch.allocate(0, 1).is_none());
    scratch.enter().unwrap();
    assert!(scratch.allocate(0, u64::MAX).is_none());
    assert!(scratch.allocate(u64::MAX, 1).is_none());
    assert_eq!(scratch.depth, 1);
    scratch.leave();
    let owner = ScratchOwner::default();
    assert_eq!(owner.invoke(|_| 3), 3);
    assert_eq!(
        owner.invoke(|arena| unsafe { enter_native(arena) }),
        FAILURE
    );
    assert_eq!(unsafe { enter_native(std::ptr::null_mut()) }, FAILURE);
}

#[test]
fn zero_slot_calls_need_no_backing_allocation() {
    let owner = ScratchOwner::default();
    for _ in 0..100 {
        assert_eq!(
            owner.invoke(|arena| {
                // SAFETY: the owner holds the arena's exclusive invocation lease.
                unsafe {
                    enter_native(arena);
                    leave_native(arena);
                }
                0
            }),
            0
        );
    }
    let arena = owner.0.borrow();
    assert!(arena.frames.is_empty());
    assert!(arena.root.buffers.is_empty());
}

#[test]
fn retained_calls_reuse_cells_after_success_and_balanced_source_failure() {
    let owner = ScratchOwner::default();
    let mut original: *mut u64 = std::ptr::null_mut();
    for status in [0, 3, 0] {
        assert_eq!(
            owner.invoke(|arena| {
                // SAFETY: these private callbacks borrow the exclusively leased arena.
                unsafe {
                    assert_eq!(enter_native(arena), 0);
                    let cells = allocate_native(arena, 0, 3);
                    assert!(!cells.is_null());
                    if original.is_null() {
                        original = cells;
                    } else {
                        assert_eq!(cells, original);
                    }
                    leave_native(arena);
                }
                status
            }),
            status
        );
    }
}

#[test]
fn active_owner_refuses_reentry_and_other_owners_remain_independent() {
    let owner = ScratchOwner::default();
    let other = ScratchOwner::default();
    assert_eq!(
        owner.invoke(|arena| {
            // SAFETY: the outer lease stays live while the inner lease is refused.
            unsafe {
                assert_eq!(enter_native(arena), 0);
            }
            assert_eq!(
                owner.invoke(|_| panic!("overlapping callback must not run")),
                FAILURE
            );
            assert_eq!(other.invoke(|_| 0), 0);
            // SAFETY: reentry did not acquire or change the parent arena.
            unsafe {
                leave_native(arena);
            }
            0
        }),
        0
    );
    assert_eq!(owner.invoke(|_| 0), 0);
}

#[test]
fn separately_owned_concurrent_arenas_preserve_their_active_cells() {
    let barrier = std::sync::Barrier::new(2);
    std::thread::scope(|scope| {
        for lane in 0..2_u64 {
            let barrier = &barrier;
            scope.spawn(move || run_concurrent_owner(barrier, lane));
        }
    });
}

fn run_concurrent_owner(barrier: &std::sync::Barrier, lane: u64) {
    let owner = ScratchOwner::default();
    for call in 0..20_u64 {
        assert_eq!(
            owner.invoke(|arena| {
                let value = (lane << 32) | call;
                // SAFETY: this thread owns this exclusively leased arena and cell.
                let cell = unsafe {
                    assert_eq!(enter_native(arena), 0);
                    let cell = allocate_native(arena, 0, 1);
                    assert!(!cell.is_null());
                    cell.write(value);
                    cell
                };
                barrier.wait();
                // SAFETY: the other owner cannot acquire or mutate this cell.
                unsafe {
                    assert_eq!(cell.read(), value);
                }
                barrier.wait();
                // SAFETY: both checks finished while this frame stayed active.
                unsafe {
                    leave_native(arena);
                }
                0
            }),
            0
        );
    }
}

#[test]
fn damaged_and_unwound_leases_discard_storage_and_release_entry() {
    let owner = ScratchOwner::default();
    assert_eq!(
        owner.invoke(|arena| {
            // SAFETY: callback deliberately leaves its private frame unbalanced.
            unsafe {
                enter_native(arena);
                allocate_native(arena, 0, 8);
            }
            0
        }),
        FAILURE
    );
    assert!(owner.0.borrow().root.buffers.is_empty());
    let unwound = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        owner.invoke(|arena| {
            // SAFETY: unwind happens in this Rust test closure, outside generated C ABI.
            unsafe {
                enter_native(arena);
                allocate_native(arena, 0, 8);
            }
            panic!("test cancellation");
        });
    }));
    assert!(unwound.is_err());
    assert!(owner.0.borrow().root.buffers.is_empty());
    assert_eq!(owner.invoke(|_| 0), 0);
}

#[test]
fn repeated_native_calls_read_current_inputs_and_selected_lazy_regions() {
    use rumoca_ir_solve::{CompareOp, FunctionConditionalProgram, LinearOp as L, UnaryOp};
    let program = FunctionConditionalProgram::checked(
        0,
        [1],
        [(
            vec![L::LoadP { dst: 0, index: 0 }, L::StoreOutput { src: 0 }],
            vec![
                L::LoadY { dst: 0, index: 0 },
                L::Unary {
                    dst: 1,
                    op: UnaryOp::Sqrt,
                    arg: 0,
                },
                L::LoadTime { dst: 2 },
                L::Compare {
                    dst: 3,
                    op: CompareOp::Lt,
                    lhs: 1,
                    rhs: 2,
                },
                L::StoreOutput { src: 3 },
            ],
        )],
        vec![L::LoadP { dst: 0, index: 1 }, L::StoreOutput { src: 0 }],
    )
    .unwrap();
    let row = vec![
        L::FunctionConditional {
            dst_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(program),
        },
        L::StoreOutput { src: 0 },
    ];
    let residual = crate::emit::compile_residual_rows(&[row]).unwrap();
    let jacobian = crate::emit::compile_jacobian_rows(&[vec![
        L::LoadSeed { dst: 0, index: 0 },
        L::StoreOutput { src: 0 },
    ]])
    .unwrap();
    let mut output = [0.0];
    for i in 0..100 {
        let (y, p, time, expected): (f64, [f64; 2], f64, f64) = match i % 4 {
            0 => (4.0, [1.0, 0.0], 3.0, 1.0),
            1 => (4.0, [1.0, 1.0], 1.0, 0.0),
            2 => (-1.0, [0.0, 1.0], 3.0, 1.0),
            _ => (-1.0, [0.0, -0.0], 3.0, -0.0),
        };
        residual.call(&[y], &p, time, &mut output).unwrap();
        assert_eq!(output[0].to_bits(), expected.to_bits());
        let seed = if i % 2 == 0 {
            -0.0_f64
        } else {
            f64::from_bits(0x7ff8_0000_0000_0123)
        };
        jacobian.call(&[y], &p, time, &[seed], &mut output).unwrap();
        assert_eq!(output[0].to_bits(), seed.to_bits());
    }
}

#[test]
fn materialized_tensor_extents_only_allocate_pointer_sized_stack_handles() {
    let mut context = cranelift_codegen::Context::new();
    context
        .func
        .signature
        .params
        .push(AbiParam::new(types::I64));
    context
        .func
        .signature
        .returns
        .push(AbiParam::new(types::I8));
    let mut frontend = cranelift_frontend::FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut context.func, &mut frontend);
    let entry = builder.create_block();
    builder.append_block_params_for_function_params(entry);
    builder.switch_to_block(entry);
    builder.seal_block(entry);
    let arena = builder.block_params(entry)[0];
    assert!(builder.func.layout.entry_block().is_none());
    enter(&mut builder);
    assert_eq!(builder.func.layout.entry_block(), Some(entry));
    assert_ne!(builder.current_block(), Some(entry));
    assert_eq!(parameter(&builder), arena);
    create_slot(&mut builder, 512 * 512 * 8).unwrap();
    create_slot(&mut builder, 8).unwrap();
    assert!(create_slot(&mut builder, 7).is_err());
    status::succeed(&mut builder);
    builder.finalize();
    assert_eq!(context.func.sized_stack_slots.len(), 2);
    assert!(
        context
            .func
            .sized_stack_slots
            .values()
            .all(|slot| slot.size == 8)
    );
}
