//! Test-only consumer: one published function call, with no stage scheduling.

type Kernel = wasmi::TypedFunc<(i32, i32, f64, i32, i32), ()>;

pub(super) struct ProgramExecution {
    store: wasmi::Store<()>,
    memory: wasmi::Memory,
    kernel: Kernel,
    y_offset: usize,
    p_offset: usize,
    y_count: usize,
}

impl ProgramExecution {
    pub(super) fn new(artifact: &serde_json::Value) -> Self {
        let engine = wasmi::Engine::default();
        let mut store = wasmi::Store::new(&engine, ());
        let memory = wasmi::Memory::new(
            &mut store,
            wasmi::MemoryType::new(
                artifact["abi"]["memory_pages"].as_u64().unwrap() as u32,
                None,
            ),
        )
        .unwrap();
        let mut linker = wasmi::Linker::new(&engine);
        linker.define("env", "memory", memory).unwrap();
        linker
            .func_wrap("env", "sin", |value: f64| value.sin())
            .unwrap();
        linker
            .func_wrap("env", "exp", |value: f64| value.exp())
            .unwrap();
        let bytes = artifact["module_bytes"]
            .as_array()
            .unwrap()
            .iter()
            .map(|value| value.as_u64().unwrap() as u8)
            .collect::<Vec<_>>();
        let module = wasmi::Module::new(&engine, &bytes[..]).unwrap();
        let instance = linker
            .instantiate(&mut store, &module)
            .unwrap()
            .start(&mut store)
            .unwrap();
        let kernel = instance.get_typed_func(&store, "eval_assignments").unwrap();
        Self {
            store,
            memory,
            kernel,
            y_offset: artifact["abi"]["y_offset"].as_u64().unwrap() as usize,
            p_offset: artifact["abi"]["p_offset"].as_u64().unwrap() as usize,
            y_count: artifact["abi"]["y_count"].as_u64().unwrap() as usize,
        }
    }

    pub(super) fn evaluate(&mut self, parameters: &[f64], time: f64) -> Vec<f64> {
        self.write(self.y_offset, &vec![f64::NAN; self.y_count]);
        self.write(self.p_offset, parameters);
        self.kernel
            .call(
                &mut self.store,
                (self.y_offset as i32, self.p_offset as i32, time, 0, 0),
            )
            .unwrap();
        let mut bytes = vec![0; self.y_count * 8];
        self.memory
            .read(&self.store, self.y_offset, &mut bytes)
            .unwrap();
        bytes
            .chunks_exact(8)
            .map(|bytes| f64::from_le_bytes(bytes.try_into().unwrap()))
            .collect()
    }

    fn write(&mut self, offset: usize, values: &[f64]) {
        let bytes = values
            .iter()
            .flat_map(|value| value.to_le_bytes())
            .collect::<Vec<_>>();
        self.memory.write(&mut self.store, offset, &bytes).unwrap();
    }
}
