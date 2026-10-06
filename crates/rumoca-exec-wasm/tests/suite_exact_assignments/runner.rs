//! Wasmi executes actual emitted bytes with owned input, scratch and guards.
use super::*;
use wasmi::{Engine, Linker, Memory, MemoryType, Module, Store, TypedFunc};

pub(super) struct Runner {
    pub(super) store: Store<()>,
    pub(super) memory: Memory,
    pub(super) call: TypedFunc<(i32, i32, f64, i32, i32), i32>,
    pub(super) p: usize,
    pub(super) scratch: usize,
    guard: usize,
    y_count: usize,
}

impl Runner {
    pub(super) fn new(
        compiled: &rumoca_exec_wasm::CompiledExactAssignmentWasm,
        layout: &solve::VarLayout,
    ) -> Self {
        Self::new_entry(
            compiled.module_bytes(),
            compiled.scratch_bytes(),
            layout,
            "eval_assignments",
        )
    }

    pub(super) fn new_private(
        compiled: &rumoca_exec_wasm::CompiledPrivateProgramWasm,
        layout: &solve::VarLayout,
    ) -> Self {
        Self::new_entry(
            compiled.module_bytes(),
            compiled.scratch_bytes(),
            layout,
            "eval_private",
        )
    }

    fn new_entry(
        bytes: &[u8],
        scratch_bytes: u32,
        layout: &solve::VarLayout,
        export: &str,
    ) -> Self {
        let engine = Engine::default();
        let module = Module::new(&engine, bytes).unwrap();
        let mut store = Store::new(&engine, ());
        let p = layout.y_scalars() * 8 + 16;
        let scratch = p + layout.p_scalars() * 8 + 16;
        let guard = scratch + scratch_bytes as usize;
        let memory = Memory::new(
            &mut store,
            MemoryType::new((guard + 16).div_ceil(65536) as u32, None),
        )
        .unwrap();
        let mut linker = Linker::new(&engine);
        linker.define("env", "memory", memory).unwrap();
        let instance = linker
            .instantiate(&mut store, &module)
            .unwrap()
            .start(&mut store)
            .unwrap();
        let call = instance.get_typed_func(&store, export).unwrap();
        Self {
            store,
            memory,
            call,
            p,
            scratch,
            guard,
            y_count: layout.y_scalars(),
        }
    }

    pub(super) fn run(&mut self, y: &[f64], p: &[f64]) -> (i32, Vec<f64>) {
        assert_eq!(y.len(), self.y_count);
        let y_bytes = bytes(y);
        let p_bytes = bytes(p);
        self.memory
            .write(&mut self.store, 0, &vec![0xa5; self.guard + 16])
            .unwrap();
        self.memory.write(&mut self.store, 0, &y_bytes).unwrap();
        self.memory
            .write(&mut self.store, self.p, &p_bytes)
            .unwrap();
        let status = self
            .call
            .call(
                &mut self.store,
                (0, self.p as i32, 0., self.scratch as i32, 0),
            )
            .unwrap();
        let mut output = vec![0; y.len() * 8];
        self.memory.read(&self.store, 0, &mut output).unwrap();
        assert_eq!(
            &self.memory.data(&self.store)[self.p..self.p + p_bytes.len()],
            &p_bytes
        );
        for range in [
            y_bytes.len()..self.p,
            self.p + p_bytes.len()..self.scratch,
            self.guard..self.guard + 16,
        ] {
            assert!(
                self.memory.data(&self.store)[range]
                    .iter()
                    .all(|&b| b == 0xa5)
            );
        }
        (
            status,
            output
                .chunks_exact(8)
                .map(|cell| f64::from_le_bytes(cell.try_into().unwrap()))
                .collect(),
        )
    }

    pub(super) fn run_private(&mut self, y: &[f64], p: &[f64], count: usize) -> (i32, Vec<f64>) {
        let (status, after) = self.run(y, p);
        assert_eq!(
            bytes(y),
            bytes(&after),
            "private entry must not publish Y even on fault"
        );
        if status != 0 {
            return (status, Vec::new());
        }
        let mut output = vec![0; count * 8];
        self.memory
            .read(&self.store, self.scratch, &mut output)
            .unwrap();
        (
            status,
            output
                .chunks_exact(8)
                .map(|cell| f64::from_le_bytes(cell.try_into().unwrap()))
                .collect(),
        )
    }

    pub(super) fn invalid_scratch(&mut self, scratch: i32) -> i32 {
        let before = self.memory.data(&self.store).to_vec();
        let status = self
            .call
            .call(&mut self.store, (0, self.p as i32, 0., scratch, 0))
            .unwrap();
        assert_eq!(self.memory.data(&self.store), before);
        status
    }
}

pub(super) fn bytes(values: &[f64]) -> Vec<u8> {
    values
        .iter()
        .flat_map(|value| value.to_le_bytes())
        .collect()
}
