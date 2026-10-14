use super::*;
use rumoca_ir_solve::{BinaryOp, LinearOp};
use wasmi::{Engine, Linker, Memory, MemoryType, Module, Store};

#[test]
fn directional_kernel_executes_emitted_wasm_and_rejects_missing_seed() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("directional.mo"),
        1,
        2,
    );
    let source = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::LoadSeed { dst: 0, index: 0 },
            LinearOp::Const {
                dst: 1,
                value: -2.0,
            },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ]],
        vec![span],
    )
    .unwrap();
    let layout = VarLayout::from_parts(Default::default(), 1, 0);
    let code = WasmJacobian::compile(&source, &layout).unwrap();
    assert!(code.validate(&[1.5], &[], &[], &[]).is_err());
    assert!(code.validate(&[1.5], &[], &[3.0], &[]).is_ok());
    let engine = Engine::default();
    let module = Module::new(&engine, code.full.module_bytes()).unwrap();
    let mut store = Store::new(&engine, ());
    let memory = Memory::new(&mut store, MemoryType::new(1, None)).unwrap();
    let mut linker = Linker::new(&engine);
    linker.define("env", "memory", memory).unwrap();
    let instance = linker
        .instantiate(&mut store, &module)
        .unwrap()
        .start(&mut store)
        .unwrap();
    memory.write(&mut store, 0, &1.5f64.to_le_bytes()).unwrap();
    memory.write(&mut store, 16, &3.0f64.to_le_bytes()).unwrap();
    let entry = instance
        .get_typed_func::<(i32, i32, f64, i32, i32), ()>(&store, "eval_residual")
        .unwrap();
    entry.call(&mut store, (0, 8, 0.0, 16, 24)).unwrap();
    let mut bytes = [0; 8];
    memory.read(&store, 24, &mut bytes).unwrap();
    assert_eq!(f64::from_le_bytes(bytes), -6.0);
}
