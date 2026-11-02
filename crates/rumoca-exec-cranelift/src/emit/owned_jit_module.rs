use std::ops::{Deref, DerefMut};

use cranelift_codegen::ir;
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Module};

use super::register_allocation::QueuedFunctions;
use super::{CompileError, define_queued_functions};

/// Sole owner of one Cranelift executable-memory arena.
///
/// `JITModule` intentionally does not release code memory from `Drop`; its
/// unsafe consuming `free_memory` operation must be called after every
/// function pointer into the module has become unreachable. Compiled Rumoca
/// owners keep this field last, so their pointer metadata and attached call
/// tables are dropped before this guard releases the arena.
///
/// It also holds the functions built for the module but not yet compiled;
/// [`super::finalize_jit_module`] defines them before finalizing.
pub(super) struct OwnedJitModule {
    module: Option<JITModule>,
    queued: QueuedFunctions,
}

impl OwnedJitModule {
    pub(super) fn new(module: JITModule) -> Self {
        Self {
            module: Some(module),
            queued: QueuedFunctions::default(),
        }
    }

    /// Queue `function`, built in `context`, to compile with the module's
    /// other functions; `context` is left empty for the next function. A
    /// round compiled here is guarded as finalization's is.
    pub(super) fn queue_function(
        &mut self,
        function: FuncId,
        context: &mut cranelift_codegen::Context,
    ) -> Result<(), CompileError> {
        let fresh = self.make_context();
        if self.queued.push(function, context, fresh) {
            define_queued_functions(self)?;
        }
        Ok(())
    }

    /// Compile and define every queued function, in queue order.
    pub(super) fn define_queued(&mut self) -> Result<(), String> {
        let module = self.module.as_mut().expect("owned JIT module is present");
        self.queued.define_into(module)
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
        self.module.as_ref().expect("owned JIT module is present")
    }
}

impl DerefMut for OwnedJitModule {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.module.as_mut().expect("owned JIT module is present")
    }
}

impl Drop for OwnedJitModule {
    fn drop(&mut self) {
        let Some(module) = self.module.take() else {
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

    use cranelift_codegen::ir::{AbiParam, InstBuilder, types};
    use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
    use cranelift_module::{Linkage, Module, ModuleResult};

    use super::OwnedJitModule;
    use crate::emit::CompileError;
    use crate::emit::register_allocation::QUEUE_ROUND;

    struct CountingMemory {
        frees: Arc<AtomicUsize>,
    }

    impl JITMemoryProvider for CountingMemory {
        fn allocate_readexec(&mut self, _size: usize, _align: u64) -> io::Result<*mut u8> {
            panic!("these tests provide no code memory")
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

    /// Queue a function `() -> i64` returning 0 in `module`.
    fn queue_zero(module: &mut OwnedJitModule, index: usize) -> Result<(), CompileError> {
        let mut context = module.make_context();
        context
            .func
            .signature
            .returns
            .push(AbiParam::new(types::I64));
        let function = module
            .declare_function(
                &format!("zero_{index}"),
                Linkage::Local,
                &context.func.signature,
            )
            .expect("declare zero function");
        let mut frontend = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut context.func, &mut frontend);
        let entry = builder.create_block();
        builder.switch_to_block(entry);
        builder.seal_block(entry);
        let zero = builder.ins().iconst(types::I64, 0);
        builder.ins().return_(&[zero]);
        builder.finalize();
        module.queue_function(function, &mut context)
    }

    #[test]
    fn a_panic_defining_a_queued_round_is_an_error_on_both_paths() {
        // The code allocator panics, as a defect below the queue might.
        let mut early = guarded_module(Arc::new(AtomicUsize::new(0)));
        let results = (0..QUEUE_ROUND)
            .map(|index| queue_zero(&mut early, index))
            .collect::<Vec<_>>();
        assert!(results[..QUEUE_ROUND - 1].iter().all(Result::is_ok));
        assert!(
            matches!(results[QUEUE_ROUND - 1], Err(CompileError::Backend(_))),
            "{:?}",
            results[QUEUE_ROUND - 1]
        );

        let mut tail = guarded_module(Arc::new(AtomicUsize::new(0)));
        queue_zero(&mut tail, 0).expect("one function waits in the queue");
        assert!(matches!(
            crate::emit::finalize_jit_module(&mut tail),
            Err(CompileError::Backend(_))
        ));
    }
}
