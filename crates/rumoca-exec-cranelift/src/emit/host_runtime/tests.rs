use super::host_jit_builder;
use crate::emit::owned_jit_module::OwnedJitModule;
use cranelift_codegen::ir::{AbiParam, InstBuilder, types};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::JITModule;
use cranelift_module::{Linkage, Module};

#[test]
fn issue_422_memcmp_relocation_needs_no_process_symbol_export() {
    let mut builder = host_jit_builder().expect("host JIT configuration");
    builder.symbol_lookup_fn(Box::new(|name| {
        panic!("the canonical host builder must register libcall {name} explicitly")
    }));
    let mut module = OwnedJitModule::new(JITModule::new(builder));
    let config = module.target_config();
    let mut context = module.make_context();
    context.func.signature.params = vec![AbiParam::new(config.pointer_type()); 3];
    context
        .func
        .signature
        .returns
        .push(AbiParam::new(types::I32));
    let function = module
        .declare_function("reported_memcmp", Linkage::Local, &context.func.signature)
        .expect("declare memory comparison");
    let mut frontend = FunctionBuilderContext::new();
    {
        let mut function_builder = FunctionBuilder::new(&mut context.func, &mut frontend);
        let entry = function_builder.create_block();
        function_builder.append_block_params_for_function_params(entry);
        function_builder.switch_to_block(entry);
        function_builder.seal_block(entry);
        let arguments = function_builder.block_params(entry).to_vec();
        let result = function_builder.call_memcmp(config, arguments[0], arguments[1], arguments[2]);
        function_builder.ins().return_(&[result]);
        function_builder.finalize();
    }
    module
        .define_function(function, &mut context)
        .expect("define comparison");
    module
        .finalize_definitions()
        .expect("relocate registered memory libcall");
    // SAFETY: the compiled function has the declared pointer/pointer/size ABI;
    // both slices below hold size readable bytes, and module owns the code
    // until after every call completes.
    let compare: unsafe extern "C" fn(*const u8, *const u8, usize) -> i32 =
        unsafe { std::mem::transmute(module.get_finalized_function(function)) };
    let left = [1_u8, 2, 3, 4, 5, 6, 7, 8];
    let right = [1_u8, 2, 3, 4, 5, 6, 7, 9];
    // SAFETY: the arrays have the supplied readable length and module is live.
    assert_eq!(
        unsafe { compare(left.as_ptr(), left.as_ptr(), left.len()) },
        0
    );
    // SAFETY: the arrays have the supplied readable length and module is live.
    assert!(unsafe { compare(left.as_ptr(), right.as_ptr(), left.len()) } < 0);
}

fn compile_memory_libcall(libcall: cranelift_codegen::ir::LibCall) -> (OwnedJitModule, *const u8) {
    use cranelift_codegen::ir::{ExtFuncData, ExternalName};

    let mut builder = host_jit_builder().expect("host JIT configuration");
    builder.symbol_lookup_fn(Box::new(|name| {
        panic!("the canonical host builder must register libcall {name} explicitly")
    }));
    let mut module = OwnedJitModule::new(JITModule::new(builder));
    let config = module.target_config();
    let mut context = module.make_context();
    context.func.signature = libcall.signature(config.default_call_conv, config.pointer_type());
    let function = module
        .declare_function("memory_libcall", Linkage::Local, &context.func.signature)
        .expect("declare native memory ABI");
    let mut frontend = FunctionBuilderContext::new();
    {
        let mut builder = FunctionBuilder::new(&mut context.func, &mut frontend);
        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);
        let arguments = builder.block_params(entry).to_vec();
        let signature = builder
            .import_signature(libcall.signature(config.default_call_conv, config.pointer_type()));
        let callee = builder.import_function(ExtFuncData {
            name: ExternalName::LibCall(libcall),
            signature,
            colocated: false,
        });
        let call = builder.ins().call(callee, &arguments);
        let result = builder.inst_results(call)[0];
        builder.ins().return_(&[result]);
        builder.finalize();
    }
    module
        .define_function(function, &mut context)
        .expect("define memory call");
    module
        .finalize_definitions()
        .expect("relocate native memory call");
    let address = module.get_finalized_function(function);
    (module, address)
}

#[test]
fn issue_422_memory_libcalls_relocate_without_process_symbol_export() {
    use cranelift_codegen::ir::LibCall;

    for libcall in [LibCall::Memcpy, LibCall::Memmove] {
        let (_module, address) = compile_memory_libcall(libcall);
        // SAFETY: LibCall's checked signature is the native C pointer/pointer/size
        // ABI, and the owning module remains live for all calls below.
        let copy: unsafe extern "C" fn(*mut u8, *const u8, usize) -> *mut u8 =
            unsafe { std::mem::transmute(address) };
        let source = [1_u8, 2, 3, 4, 5, 6, 7, 8];
        let mut destination = [99_u8; 10];
        let dst = destination[1..].as_mut_ptr();
        // SAFETY: these disjoint slices have eight writable/readable bytes.
        assert_eq!(unsafe { copy(dst, source.as_ptr(), source.len()) }, dst);
        assert_eq!(destination, [99, 1, 2, 3, 4, 5, 6, 7, 8, 99]);
        if libcall == LibCall::Memmove {
            let mut overlap = source;
            let ptr = overlap.as_mut_ptr();
            // SAFETY: both overlapping ranges lie within the array; memmove
            // supports overlap in either direction.
            assert_eq!(unsafe { copy(ptr.add(2), ptr, 6) }, unsafe { ptr.add(2) });
            assert_eq!(overlap, [1, 2, 1, 2, 3, 4, 5, 6]);
            // SAFETY: both six-byte ranges are within the same live array.
            assert_eq!(unsafe { copy(ptr, ptr.add(2), 6) }, ptr);
            assert_eq!(overlap, [1, 2, 3, 4, 5, 6, 5, 6]);
        }
    }
    let (_module, address) = compile_memory_libcall(LibCall::Memset);
    // SAFETY: LibCall::Memset's native C ABI uses an i32 fill argument;
    // the owning module remains live and the destination is writable.
    let fill: unsafe extern "C" fn(*mut u8, i32, usize) -> *mut u8 =
        unsafe { std::mem::transmute(address) };
    let mut bytes = [99_u8; 8];
    let dst = bytes[1..].as_mut_ptr();
    // SAFETY: the six-byte destination lies within bytes. C memset truncates
    // the int argument to unsigned char, independently checked below.
    assert_eq!(unsafe { fill(dst, 0x1ab, 6) }, dst);
    assert_eq!(bytes, [99, 0xab, 0xab, 0xab, 0xab, 0xab, 0xab, 99]);
    // SAFETY: dst remains valid; a zero count writes no bytes.
    assert_eq!(unsafe { fill(dst, 0, 0) }, dst);
    assert_eq!(bytes, [99, 0xab, 0xab, 0xab, 0xab, 0xab, 0xab, 99]);
}
