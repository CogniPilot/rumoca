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
    assert_eq!(scratch.frames.len(), 1);
    assert_eq!(scratch.frames[0].buffers.len(), 1);
    scratch.leave();
    scratch.enter().unwrap();
    scratch.allocate(0, 17).unwrap();
    assert_eq!(scratch.frames[0].buffers[0].cells.len(), 17);
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
    assert_eq!(invoke(|_| 3), 3);
    assert_eq!(invoke(|arena| unsafe { enter_native(arena) }), FAILURE);
    assert_eq!(unsafe { enter_native(std::ptr::null_mut()) }, FAILURE);
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
