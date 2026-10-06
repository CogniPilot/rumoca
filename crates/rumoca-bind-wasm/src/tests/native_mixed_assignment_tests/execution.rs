//! Test-only executor implementing only the published stage invocation/copy ABI.

type Kernel = wasmi::TypedFunc<(i32, i32, f64, i32, i32), ()>;

pub(super) struct NativeExecution {
    store: wasmi::Store<()>,
    memory: wasmi::Memory,
    pointers: [i32; 4],
    stages: Vec<(Kernel, usize, usize)>,
    y_count: usize,
}

impl NativeExecution {
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
        let pointers = ["y_offset", "p_offset", "seed_offset", "output_offset"]
            .map(|name| artifact["abi"][name].as_i64().unwrap() as i32);
        let stages = artifact["stages"]
            .as_array()
            .unwrap()
            .iter()
            .map(|stage| {
                let bytes = stage["module_bytes"]
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
                (
                    instance
                        .get_typed_func::<(i32, i32, f64, i32, i32), ()>(&store, "eval_residual")
                        .unwrap(),
                    stage["target_start"].as_u64().unwrap() as usize,
                    stage["target_count"].as_u64().unwrap() as usize,
                )
            })
            .collect();
        Self {
            store,
            memory,
            pointers,
            stages,
            y_count: artifact["abi"]["y_count"].as_u64().unwrap() as usize,
        }
    }

    pub(super) fn evaluate(&mut self, parameters: &[f64], time: f64) -> Vec<f64> {
        self.write(self.pointers[0] as usize, &vec![f64::NAN; self.y_count]);
        self.write(self.pointers[1] as usize, parameters);
        for (kernel, target, count) in &self.stages {
            kernel
                .call(
                    &mut self.store,
                    (
                        self.pointers[0],
                        self.pointers[1],
                        time,
                        self.pointers[2],
                        self.pointers[3],
                    ),
                )
                .unwrap();
            let mut bytes = vec![0; count * 8];
            self.memory
                .read(&self.store, self.pointers[3] as usize, &mut bytes)
                .unwrap();
            self.memory
                .write(
                    &mut self.store,
                    self.pointers[0] as usize + target * 8,
                    &bytes,
                )
                .unwrap();
        }
        let mut bytes = vec![0; self.y_count * 8];
        self.memory
            .read(&self.store, self.pointers[0] as usize, &mut bytes)
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
