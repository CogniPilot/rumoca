use super::ChunkedMemory;
use crate::emit::host_runtime::host_jit_builder;
use crate::emit::owned_jit_module::OwnedJitModule;
use cranelift_codegen::ir::{AbiParam, InstBuilder, types};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{BranchProtection, JITMemoryProvider, JITModule};
use cranelift_module::{FuncId, Linkage, Module};
use region::Protection;
use std::collections::BTreeSet;

fn protection(pointer: *const u8) -> Protection {
    region::query(pointer)
        .expect("mapped JIT allocation")
        .protection()
}

#[test]
fn protection_pools_seal_without_reopening_earlier_code() {
    let mut memory = ChunkedMemory::default();
    let code = memory.allocate_readexec(32, 64).expect("code allocation");
    let constant = memory
        .allocate_readonly(32, 8)
        .expect("constant allocation");
    let data = memory.allocate_readwrite(32, 8).expect("data allocation");
    for pointer in [code, constant, data] {
        assert_eq!(protection(pointer), Protection::READ_WRITE);
        // SAFETY: all three allocations are writable and hold at least one byte.
        unsafe { pointer.write(37) };
    }
    assert_eq!(code.addr() % 64, 0);
    assert_eq!(constant.addr() % 8, 0);
    assert_eq!(data.addr() % 8, 0);
    memory
        .finalize(BranchProtection::None)
        .expect("seal first batch");
    assert_eq!(protection(code), Protection::READ_EXECUTE);
    assert_eq!(protection(constant), Protection::READ);
    assert_eq!(protection(data), Protection::READ_WRITE);
    let next = memory.allocate_readexec(32, 64).expect("incremental code");
    assert_ne!(next, code);
    assert_eq!(protection(next), Protection::READ_WRITE);
    assert_eq!(protection(code), Protection::READ_EXECUTE);
    memory
        .finalize(BranchProtection::None)
        .expect("seal next batch");
    for pointer in [code, constant, data] {
        // SAFETY: finalized allocations retain readable storage until free_memory.
        assert_eq!(unsafe { pointer.read() }, 37);
    }
    assert_eq!(protection(next), Protection::READ_EXECUTE);
    // SAFETY: no function pointers were created and no allocation is accessed again.
    unsafe { memory.free_memory() };
    assert!(memory.pools.iter().all(|pool| pool.chunks.is_empty()));
}

#[test]
fn many_small_entries_share_pages_and_survive_chunk_rollover() {
    let mut memory = ChunkedMemory::default();
    let mut entries = Vec::new();
    let page = region::page::size();
    let mut pages = BTreeSet::new();
    for index in 0..10_000_u32 {
        let pointer = memory.allocate_readexec(32, 16).expect("small entry");
        assert_eq!(pointer.addr() % 16, 0);
        pages.insert(pointer.addr() / page);
        // SAFETY: this distinct aligned allocation holds 32 writable bytes.
        unsafe { pointer.cast::<u32>().write(index) };
        entries.push(pointer);
    }
    // The resource bound follows emitted bytes, rather than one page per function.
    assert!(pages.len() < entries.len() / 8);
    assert!(memory.pools[0].chunks.len() > 1);
    memory
        .finalize(BranchProtection::None)
        .expect("seal entries");
    for (index, pointer) in entries.into_iter().enumerate() {
        assert_eq!(protection(pointer), Protection::READ_EXECUTE);
        // SAFETY: each allocation remains readable and its initialized u32 is aligned.
        assert_eq!(unsafe { pointer.cast::<u32>().read() }, index as u32);
    }
    // SAFETY: all allocation accesses have completed and no callable pointers exist.
    unsafe { memory.free_memory() };
}

#[test]
fn invalid_layouts_refuse_without_reserving_and_large_requests_remain_supported() {
    let mut memory = ChunkedMemory::default();
    let page = region::page::size();
    for alignment in [0, 3, (page as u64) * 2] {
        assert!(memory.allocate_readexec(32, alignment).is_err());
    }
    assert!(memory.allocate_readexec(usize::MAX, 1).is_err());
    assert!(memory.pools.iter().all(|pool| pool.chunks.is_empty()));
    let size = 3 * super::CHUNK_GRANULARITY_BYTES + 13;
    let pointer = memory
        .allocate_readwrite(size, page as u64)
        .expect("large request");
    assert_eq!(pointer.addr() % page, 0);
    // SAFETY: both ends lie in the returned writable allocation.
    unsafe {
        pointer.write(11);
        pointer.add(size - 1).write(23);
    }
    memory
        .finalize(BranchProtection::None)
        .expect("finalize large data");
    // SAFETY: data stays readable and writable after finalization.
    unsafe {
        assert_eq!(pointer.read(), 11);
        assert_eq!(pointer.add(size - 1).read(), 23);
        memory.free_memory();
    }
    // An unfinalized reservation also follows the same explicit free path.
    memory
        .allocate_readexec(32, 16)
        .expect("new unsealed allocation");
    // SAFETY: the returned allocation was never published or accessed after this call.
    unsafe { memory.free_memory() };
    assert!(memory.pools.iter().all(|pool| pool.chunks.is_empty()));
}

#[test]
fn upstream_allocation_unwind_poisoning_preserves_cleanup_and_refuses_reuse() {
    let mut memory = ChunkedMemory::default();
    memory.allocate_readexec(32, 16).expect("first reservation");
    let error = memory
        .allocation::<()>(|pools| {
            pools[0].allocate(100_000, 16)?;
            panic!("synthetic upstream protection error after reservation")
        })
        .expect_err("upstream unwind becomes an allocation error");
    assert!(
        error
            .to_string()
            .contains("synthetic upstream protection error")
    );
    assert!(memory.allocate_readexec(32, 16).is_err());
    assert!(memory.finalize(BranchProtection::None).is_err());
    assert!(!memory.pools[0].chunks.is_empty());
    // SAFETY: the failed compilation published no callable pointer and is abandoned.
    unsafe { memory.free_memory() };
    assert!(memory.pools.iter().all(|pool| pool.chunks.is_empty()));
}

fn define_constant(module: &mut OwnedJitModule, name: &str, value: i64) -> FuncId {
    let mut context = module.make_context();
    context
        .func
        .signature
        .returns
        .push(AbiParam::new(types::I64));
    let function = module
        .declare_function(name, Linkage::Local, &context.func.signature)
        .expect("declare constant");
    let mut frontend = FunctionBuilderContext::new();
    {
        let mut builder = FunctionBuilder::new(&mut context.func, &mut frontend);
        let entry = builder.create_block();
        builder.switch_to_block(entry);
        builder.seal_block(entry);
        let result = builder.ins().iconst(types::I64, value);
        builder.ins().return_(&[result]);
        builder.finalize();
    }
    module
        .define_function(function, &mut context)
        .expect("define constant");
    function
}

#[test]
fn incremental_finalization_keeps_actual_entries_stable_until_owner_drop() {
    let mut module = OwnedJitModule::new(JITModule::new(host_jit_builder().expect("host builder")));
    let first = define_constant(&mut module, "first", 37);
    module
        .finalize_definitions()
        .expect("finalize first function");
    let original_address = module.get_finalized_function(first);
    // SAFETY: the declared no-argument C ABI returns i64 and module owns the entry.
    let first_call: unsafe extern "C" fn() -> i64 =
        unsafe { std::mem::transmute(original_address) };
    let second = define_constant(&mut module, "second", 73);
    module
        .finalize_definitions()
        .expect("finalize second function");
    assert_eq!(module.get_finalized_function(first), original_address);
    // SAFETY: the second function has the same declared ABI and its module is live.
    let second_call: unsafe extern "C" fn() -> i64 =
        unsafe { std::mem::transmute(module.get_finalized_function(second)) };
    // SAFETY: both entries remain owned by module through their last invocation.
    unsafe {
        assert_eq!(first_call(), 37);
        assert_eq!(second_call(), 73);
    }
    drop(module);
}

fn reserved_bytes(memory: &ChunkedMemory) -> usize {
    memory
        .pools
        .iter()
        .flat_map(|pool| &pool.chunks)
        .map(|chunk| chunk.capacity)
        .sum()
}

#[test]
fn independent_tiny_owners_and_incremental_batches_reserve_page_sized_storage() {
    let page = region::page::size();
    let mut owners = Vec::new();
    for _ in 0..32 {
        let mut memory = ChunkedMemory::default();
        assert_eq!(reserved_bytes(&memory), 0);
        let code = memory.allocate_readexec(32, 16).expect("tiny code");
        let constant = memory.allocate_readonly(8, 8).expect("tiny constant");
        let data = memory.allocate_readwrite(8, 8).expect("tiny data");
        memory
            .finalize(BranchProtection::None)
            .expect("seal tiny owner");
        assert_eq!(protection(code), Protection::READ_EXECUTE);
        assert_eq!(protection(constant), Protection::READ);
        assert_eq!(protection(data), Protection::READ_WRITE);
        owners.push(memory);
    }
    // Many separately owned kernels must not accumulate a large fixed reserve
    // for a few bytes. Permit at most two pages per populated protection pool.
    let reserved: usize = owners.iter().map(reserved_bytes).sum();
    assert!(reserved <= owners.len() * 3 * 2 * page);
    for memory in &mut owners {
        let large = 2 * super::CHUNK_GRANULARITY_BYTES + 1;
        memory
            .allocate_readexec(large, 16)
            .expect("larger batch remains admitted");
        memory
            .finalize(BranchProtection::None)
            .expect("seal larger batch");
        let before = reserved_bytes(memory);
        let code = memory.allocate_readexec(32, 16).expect("new tiny batch");
        assert!(reserved_bytes(memory) - before <= 2 * page);
        assert_eq!(protection(code), Protection::READ_WRITE);
        memory
            .finalize(BranchProtection::None)
            .expect("seal incremental batch");
        assert_eq!(protection(code), Protection::READ_EXECUTE);
        // SAFETY: no callable pointers escaped; all allocation accesses are done.
        unsafe { memory.free_memory() };
        assert_eq!(reserved_bytes(memory), 0);
    }
}
