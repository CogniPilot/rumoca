use super::{SINGLE_PASS_VALUE_LIMIT, define_function, single_pass_for, uses_single_pass};
use crate::emit::host_runtime::host_jit_builder;
use crate::emit::owned_jit_module::OwnedJitModule;
use cranelift_codegen::ir::{AbiParam, InstBuilder, MemFlags, StackSlotData, StackSlotKind, types};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::JITModule;
use cranelift_module::{Linkage, Module};

fn host_module() -> OwnedJitModule {
    OwnedJitModule::new(JITModule::new(
        host_jit_builder().expect("host JIT configuration"),
    ))
}

#[test]
fn the_allocator_switches_at_the_value_limit() {
    assert!(uses_single_pass(SINGLE_PASS_VALUE_LIMIT));
    assert!(!uses_single_pass(SINGLE_PASS_VALUE_LIMIT + 1));
}

#[test]
fn every_host_frame_beyond_the_guard_page_is_probed_inline() {
    let module = host_module();
    let flags = module.isa().flags();
    assert!(flags.enable_probestack());
    assert_eq!(
        flags.probestack_strategy(),
        cranelift_codegen::settings::ProbestackStrategy::Inline
    );
    assert_eq!(flags.probestack_size_log2(), 12);
}

/// `() -> f64` summing `count` distinct constants in reverse definition
/// order, so every constant stays live until the sum reaches it.
fn live_values_sum(count: u32) -> (f64, usize) {
    let mut module = host_module();
    let mut context = module.make_context();
    context
        .func
        .signature
        .returns
        .push(AbiParam::new(types::F64));
    let function = module
        .declare_function("live_values", Linkage::Local, &context.func.signature)
        .expect("declare live-value function");
    let mut frontend = FunctionBuilderContext::new();
    {
        let mut builder = FunctionBuilder::new(&mut context.func, &mut frontend);
        let entry = builder.create_block();
        builder.switch_to_block(entry);
        builder.seal_block(entry);
        let values = (1..=count)
            .map(|index| builder.ins().f64const(f64::from(index) * 0.5))
            .collect::<Vec<_>>();
        let sum = values
            .iter()
            .rev()
            .copied()
            .reduce(|sum, value| builder.ins().fadd(sum, value))
            .expect("at least one value");
        builder.ins().return_(&[sum]);
        builder.finalize();
    }
    let values = context.func.dfg.num_values();
    define_function(&mut *module, function, &mut context).expect("define live-value function");
    module.finalize_definitions().expect("finalize");
    // SAFETY: the function takes no arguments and returns one f64; the module
    // owns its code until after the call.
    let call: extern "C" fn() -> f64 =
        unsafe { std::mem::transmute(module.get_finalized_function(function)) };
    (call(), values)
}

#[test]
fn single_pass_and_backtracking_functions_compute_the_same_sums() {
    for count in [8, 200, 256, 400] {
        let (sum, values) = live_values_sum(count);
        let expected = (1..=count)
            .rev()
            .map(|index| f64::from(index) * 0.5)
            .reduce(|sum, value| sum + value)
            .unwrap();
        assert_eq!(sum.to_bits(), expected.to_bits(), "{count} values");
        assert_eq!(uses_single_pass(values), values <= SINGLE_PASS_VALUE_LIMIT);
    }
}

#[test]
fn a_frame_far_beyond_the_guard_page_runs_under_probes() {
    const CELLS: u32 = 8192;
    let mut module = host_module();
    let mut context = module.make_context();
    context
        .func
        .signature
        .returns
        .push(AbiParam::new(types::F64));
    let function = module
        .declare_function("large_frame", Linkage::Local, &context.func.signature)
        .expect("declare large-frame function");
    let mut frontend = FunctionBuilderContext::new();
    {
        let mut builder = FunctionBuilder::new(&mut context.func, &mut frontend);
        let slot = builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            CELLS * 8,
            3,
        ));
        let entry = builder.create_block();
        builder.switch_to_block(entry);
        builder.seal_block(entry);
        let first = builder.ins().f64const(1.5);
        builder.ins().stack_store(first, slot, 0);
        let last = builder.ins().f64const(2.25);
        builder
            .ins()
            .stack_store(last, slot, ((CELLS - 1) * 8) as i32);
        let read_first = builder.ins().stack_load(types::F64, slot, 0);
        let read_last = builder
            .ins()
            .stack_load(types::F64, slot, ((CELLS - 1) * 8) as i32);
        let sum = builder.ins().fadd(read_first, read_last);
        builder.ins().return_(&[sum]);
        builder.finalize();
    }
    define_function(&mut *module, function, &mut context).expect("define large-frame function");
    let frame = context
        .compiled_code()
        .expect("compiled large-frame function")
        .frame_size;
    assert!(
        frame > 1 << 14,
        "the frame spans several pages: {frame} bytes"
    );
    module.finalize_definitions().expect("finalize");
    // SAFETY: the function takes no arguments and returns one f64; the module
    // owns its code until after the call.
    let call: extern "C" fn() -> f64 =
        unsafe { std::mem::transmute(module.get_finalized_function(function)) };
    assert_eq!(call(), 3.75);
    let _ = MemFlags::trusted();
}

#[test]
fn the_single_pass_target_keeps_every_module_flag_but_the_allocator() {
    let module = host_module();
    let target = module.isa();
    let single_pass = single_pass_for(target).expect("single-pass target");
    assert_eq!(single_pass.triple(), target.triple());
    let isa_flags = |isa: &dyn cranelift_codegen::isa::TargetIsa| {
        isa.isa_flags()
            .iter()
            .map(|value| (value.name, value.value_string()))
            .collect::<Vec<_>>()
    };
    assert_eq!(isa_flags(&*single_pass), isa_flags(target));
    let shared = |isa: &dyn cranelift_codegen::isa::TargetIsa| {
        isa.flags()
            .iter()
            .filter(|value| value.name != "regalloc_algorithm")
            .map(|value| (value.name, value.value_string()))
            .collect::<Vec<_>>()
    };
    assert_eq!(shared(&*single_pass), shared(target));
    assert_eq!(
        single_pass.flags().regalloc_algorithm(),
        cranelift_codegen::settings::RegallocAlgorithm::SinglePass
    );
    assert_eq!(
        target.flags().regalloc_algorithm(),
        cranelift_codegen::settings::RegallocAlgorithm::Backtracking
    );
    assert!(single_pass.flags().enable_probestack());
}

/// `target` with one boolean ISA flag inverted and every shared flag kept.
fn with_isa_flag_inverted(
    target: &dyn cranelift_codegen::isa::TargetIsa,
    name: &str,
) -> cranelift_codegen::isa::OwnedTargetIsa {
    use cranelift_codegen::settings::{self, Configurable};
    let mut shared = settings::builder();
    for value in target.flags().iter() {
        shared.set(value.name, &value.value_string()).unwrap();
    }
    let mut isa = cranelift_codegen::isa::lookup(target.triple().clone()).unwrap();
    for value in target.isa_flags() {
        let text = value.value_string();
        let text = if value.name == name {
            if text == "true" {
                "false".to_string()
            } else {
                "true".to_string()
            }
        } else {
            text
        };
        isa.set(value.name, &text).unwrap();
    }
    isa.finish(settings::Flags::new(shared)).unwrap()
}

fn isa_flag_values(target: &dyn cranelift_codegen::isa::TargetIsa) -> Vec<(&'static str, String)> {
    target
        .isa_flags()
        .iter()
        .map(|value| (value.name, value.value_string()))
        .collect()
}

#[test]
fn targets_sharing_shared_flags_get_their_own_single_pass_counterparts() {
    let module = host_module();
    let first = module.isa();
    let flag = first
        .isa_flags()
        .into_iter()
        .find(|value| value.as_bool().is_some())
        .expect("a native target has a boolean ISA flag")
        .name;
    let second = with_isa_flag_inverted(first, flag);
    assert_eq!(first.flags().to_string(), second.flags().to_string());
    assert_ne!(isa_flag_values(first), isa_flag_values(&*second));
    for target in [first, &*second, first] {
        let single_pass = single_pass_for(target).expect("single-pass target");
        assert_eq!(isa_flag_values(&*single_pass), isa_flag_values(target));
    }
}
