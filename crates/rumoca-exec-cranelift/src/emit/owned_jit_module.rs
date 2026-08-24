use std::ops::{Deref, DerefMut};

use cranelift_codegen::ir;
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Module};

/// Sole owner of one Cranelift executable-memory arena.
///
/// `JITModule` intentionally does not release code memory from `Drop`; its
/// unsafe consuming `free_memory` operation must be called after every
/// function pointer into the module has become unreachable. Compiled Rumoca
/// owners keep this field last, so their pointer metadata and attached call
/// tables are dropped before this guard releases the arena.
pub(super) struct OwnedJitModule(Option<JITModule>);

impl OwnedJitModule {
    pub(super) fn new(module: JITModule) -> Self {
        Self(Some(module))
    }
}

/// Import `func_id` into `func` as a callee that is reached through its full
/// 64-bit address rather than a program-counter-relative branch.
///
/// `Module::declare_func_in_func` marks a callee colocated whenever its linkage
/// is final, which on AArch64 lowers the call to `bl` and a 26-bit
/// `Reloc::Arm64Call`. That branch reaches only +/-128 MB, and the JIT places
/// each batch of compiled code in its own arena, so two functions of one model
/// can land further apart than the branch can encode; `cranelift-jit` then
/// fails the range assertion while relocating. x86-64 hides the same layout
/// behind a 32-bit displacement, so the overflow only ever appears on ARM
/// hosts. Clearing `colocated` selects the far-call sequence, which has no
/// range limit and costs one address materialization per call.
///
/// Every call emitted into JIT code goes through here so that no future callee
/// can reintroduce a range-limited relocation by declaring itself final.
pub(super) fn declare_far_call_in_func(
    module: &mut JITModule,
    func_id: FuncId,
    func: &mut ir::Function,
) -> ir::FuncRef {
    let callee = module.declare_func_in_func(func_id, func);
    func.dfg.ext_funcs[callee].colocated = false;
    callee
}

impl Deref for OwnedJitModule {
    type Target = JITModule;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref().expect("owned JIT module is present")
    }
}

impl DerefMut for OwnedJitModule {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.as_mut().expect("owned JIT module is present")
    }
}

impl Drop for OwnedJitModule {
    fn drop(&mut self) {
        let Some(module) = self.0.take() else {
            return;
        };
        // SAFETY: the guard is the last field of every compiled owner. Dropping
        // that owner makes all function pointers into this module unreachable;
        // attached owners retain their own independently guarded modules.
        unsafe { module.free_memory() };
    }
}

#[cfg(test)]
mod tests {
    use std::io;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicUsize, Ordering};

    use cranelift_jit::{BranchProtection, JITBuilder, JITMemoryProvider, JITModule};
    use cranelift_module::ModuleResult;

    use super::OwnedJitModule;

    struct CountingMemory {
        frees: Arc<AtomicUsize>,
    }

    impl JITMemoryProvider for CountingMemory {
        fn allocate_readexec(&mut self, _size: usize, _align: u64) -> io::Result<*mut u8> {
            unreachable!("the ownership test does not compile code")
        }

        fn allocate_readwrite(&mut self, _size: usize, _align: u64) -> io::Result<*mut u8> {
            unreachable!("the ownership test does not declare data")
        }

        fn allocate_readonly(&mut self, _size: usize, _align: u64) -> io::Result<*mut u8> {
            unreachable!("the ownership test does not declare data")
        }

        unsafe fn free_memory(&mut self) {
            self.frees.fetch_add(1, Ordering::SeqCst);
        }

        fn finalize(&mut self, _branch_protection: BranchProtection) -> ModuleResult<()> {
            Ok(())
        }
    }

    fn guarded_module(frees: Arc<AtomicUsize>) -> OwnedJitModule {
        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())
            .expect("construct test JIT builder");
        builder.memory_provider(Box::new(CountingMemory { frees }));
        OwnedJitModule::new(JITModule::new(builder))
    }

    #[test]
    fn each_independent_owner_releases_exactly_its_own_arena() {
        let frees = Arc::new(AtomicUsize::new(0));
        let first = guarded_module(frees.clone());
        let second = guarded_module(frees.clone());

        drop(first);
        assert_eq!(frees.load(Ordering::SeqCst), 1);
        drop(second);
        assert_eq!(frees.load(Ordering::SeqCst), 2);
    }
}
