//! Native model call scheduling, from the unchanged source and checked owners.
mod derived_discrete;
mod gathers;
mod lazy_windows;
mod maps;
mod packed;
mod registration;
mod return_arguments;
mod returns;
mod typed_maps;

use super::*;

enum CallEntry {
    Direct(wasmi::TypedFunc<(i32, i32, f64, i32, i32), ()>),
    Checked(wasmi::TypedFunc<(i32, i32, f64, i32, i32), i32>),
}

struct CallExecution {
    store: wasmi::Store<()>,
    memory: wasmi::Memory,
    call: CallEntry,
    p: usize,
    scratch: usize,
    y: usize,
    /// Typed output-lane buffer (offset, bytes); zero when none is published.
    lanes: (usize, usize),
}

impl CallExecution {
    fn new(artifact: &serde_json::Value) -> Self {
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
            .func_wrap("env", "pow", |b: f64, e: f64| b.powf(e))
            .unwrap();
        linker.func_wrap("env", "abs", |v: f64| v.abs()).unwrap();
        let bytes = artifact["module_bytes"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_u64().unwrap() as u8)
            .collect::<Vec<_>>();
        let module = wasmi::Module::new(&engine, &bytes[..]).unwrap();
        let instance = linker
            .instantiate(&mut store, &module)
            .unwrap()
            .start(&mut store)
            .unwrap();
        let call = match artifact["profile"].as_str().unwrap() {
            "native-direct-program-f64-v2" => {
                CallEntry::Direct(instance.get_typed_func(&store, "eval_assignments").unwrap())
            }
            "native-direct-program-f64-v3" => {
                CallEntry::Checked(instance.get_typed_func(&store, "eval_assignments").unwrap())
            }
            profile => panic!("unsupported fixture native program profile {profile}"),
        };
        Self {
            store,
            memory,
            call,
            p: artifact["abi"]["p_offset"].as_u64().unwrap() as usize,
            scratch: artifact["abi"]["scratch_offset"].as_u64().unwrap_or(0) as usize,
            y: artifact["abi"]["y_count"].as_u64().unwrap() as usize,
            lanes: (
                artifact["abi"]["output_lanes_offset"].as_u64().unwrap_or(0) as usize,
                artifact["abi"]["output_lanes_bytes"].as_u64().unwrap_or(0) as usize,
            ),
        }
    }
    fn run(&mut self, p: &[f64]) -> Vec<f64> {
        let bytes = p.iter().flat_map(|v| v.to_le_bytes()).collect::<Vec<_>>();
        self.memory.write(&mut self.store, self.p, &bytes).unwrap();
        let arguments = (
            0,
            self.p as i32,
            0.,
            self.scratch as i32,
            self.lanes.0 as i32,
        );
        match self.call {
            CallEntry::Direct(call) => call.call(&mut self.store, arguments).unwrap(),
            CallEntry::Checked(call) => assert_eq!(
                call.call(&mut self.store, arguments).unwrap(),
                0,
                "geometric refusal is a successful typed result, not a failed helper"
            ),
        }
        let mut y = vec![0; self.y * 8];
        self.memory.read(&self.store, 0, &mut y).unwrap();
        let mut unchanged = vec![0; bytes.len()];
        self.memory
            .read(&self.store, self.p, &mut unchanged)
            .unwrap();
        assert_eq!(bytes, unchanged);
        y.chunks_exact(8)
            .map(|c| f64::from_le_bytes(c.try_into().unwrap()))
            .collect()
    }
}

impl CallExecution {
    /// The published typed output lanes of the last successful call.
    fn lanes(&self) -> Vec<u8> {
        let mut bytes = vec![0; self.lanes.1];
        self.memory
            .read(&self.store, self.lanes.0, &mut bytes)
            .unwrap();
        bytes
    }
}
