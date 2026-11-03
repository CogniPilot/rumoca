use rumoca_eval_solve::{PreparedScalarProgramBlock, RowEvalContext, to_scalar_program_block};
use rumoca_ir_solve::*;
use wasmi::{Engine, Linker, Memory, MemoryType, Module, Store};

pub(super) fn scalar_reference(block: &ComputeBlock, y: &[f64], p: &[f64]) -> Vec<f64> {
    let scalar = to_scalar_program_block(block).expect("shared scalarization");
    let prepared = PreparedScalarProgramBlock::new(scalar).expect("checked scalar reference");
    let mut out = vec![123.; prepared.len()];
    prepared
        .eval_with_context(y, p, 0.25, RowEvalContext::default(), &mut out)
        .unwrap();
    out
}

pub(super) fn execute(bytes: &[u8], y: &[f64], p: &[f64], outputs: usize) -> Vec<f64> {
    let engine = Engine::default();
    let module = Module::new(&engine, bytes).expect("real binary compiles");
    let mut store = Store::new(&engine, ());
    let mut linker = Linker::new(&engine);
    let p_start = y.len() * 8;
    let out_start = p_start + p.len() * 8;
    let total = out_start + outputs * 8;
    let pages = total.div_ceil(65536).max(1) as u32;
    let memory = Memory::new(&mut store, MemoryType::new(pages, None)).unwrap();
    linker.define("env", "memory", memory).unwrap();
    let inputs: Vec<u8> = y
        .iter()
        .chain(p)
        .flat_map(|value| value.to_le_bytes())
        .collect();
    memory.write(&mut store, 0, &inputs).unwrap();
    // Deliberately dirty output: sparse holes must be cleared by the kernel.
    memory
        .write(&mut store, out_start, &vec![0xab; outputs * 8])
        .unwrap();
    let instance = linker
        .instantiate(&mut store, &module)
        .unwrap()
        .start(&mut store)
        .unwrap();
    instance
        .get_typed_func::<(i32, i32, f64, i32, i32), ()>(&store, "eval_residual")
        .unwrap()
        .call(&mut store, (0, p_start as i32, 0.25, 0, out_start as i32))
        .unwrap();
    let mut out = vec![0u8; outputs * 8];
    memory.read(&store, out_start, &mut out).unwrap();
    out.chunks_exact(8)
        .map(|bytes| f64::from_le_bytes(bytes.try_into().unwrap()))
        .collect()
}

pub(super) fn assert_bits(actual: &[f64], expected: &[f64]) {
    assert_eq!(actual.len(), expected.len());
    for (i, (actual, expected)) in actual.iter().zip(expected).enumerate() {
        assert_eq!(
            actual.to_bits(),
            expected.to_bits(),
            "different binary64 output at{i}"
        );
    }
}
