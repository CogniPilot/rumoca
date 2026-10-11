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
