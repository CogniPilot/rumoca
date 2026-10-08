//! Native model call scheduling, from the unchanged source and checked owners.
mod derived_discrete;
mod families;
mod gathers;
mod lazy_windows;
mod maps;
mod packed;
mod registration;
mod return_arguments;
mod returns;
mod scratch_report;
mod typed_inputs;
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
    /// Start of the typed lane buffer the entry receives: the typed input
    /// lanes, then the output lanes.
    typed: usize,
    /// Typed input lanes written before every call, and their names.
    inputs: Vec<u8>,
    input_lanes: Vec<(String, String, usize, usize)>,
}

impl CallExecution {
    fn new(artifact: &serde_json::Value) -> Self {
        let mut execution = Self::uninitialized(artifact);
        // Typed input lanes start at their declared start values.
        for (name, _, _, p_index) in execution.input_lanes.clone() {
            let start = artifact["parameters"][p_index].as_f64().unwrap();
            execution.set_input(&name, start as i64);
        }
        execution
    }

    fn uninitialized(artifact: &serde_json::Value) -> Self {
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
            typed: artifact["abi"]["typed_lanes_offset"].as_u64().unwrap_or(0) as usize,
            inputs: vec![0; artifact["abi"]["input_lanes_bytes"].as_u64().unwrap_or(0) as usize],
            input_lanes: artifact["input_lanes"]
                .as_array()
                .into_iter()
                .flatten()
                .map(|lane| {
                    (
                        lane["name"].as_str().unwrap().to_owned(),
                        lane["representation"].as_str().unwrap().to_owned(),
                        lane["byte_offset"].as_u64().unwrap() as usize,
                        lane["p_index"].as_u64().unwrap() as usize,
                    )
                })
                .collect(),
        }
    }
    fn run(&mut self, p: &[f64]) -> Vec<f64> {
        let bytes = p.iter().flat_map(|v| v.to_le_bytes()).collect::<Vec<_>>();
        self.memory.write(&mut self.store, self.p, &bytes).unwrap();
        self.memory
            .write(&mut self.store, self.typed, &self.inputs)
            .unwrap();
        let arguments = (0, self.p as i32, 0., self.scratch as i32, self.typed as i32);
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

impl CallExecution {
    /// Write P and the typed input lanes, run the checked entry once and
    /// return its status with the host Y it published.
    fn run_typed(&mut self, p: &[f64]) -> (i32, Vec<f64>) {
        let CallEntry::Checked(call) = self.call else {
            panic!("typed input lanes require the checked entry");
        };
        let bytes = p.iter().flat_map(|v| v.to_le_bytes()).collect::<Vec<_>>();
        self.memory.write(&mut self.store, self.p, &bytes).unwrap();
        self.memory
            .write(&mut self.store, self.typed, &self.inputs)
            .unwrap();
        let arguments = (0, self.p as i32, 0., self.scratch as i32, self.typed as i32);
        let status = call.call(&mut self.store, arguments).unwrap();
        let mut y = vec![0; self.y * 8];
        self.memory.read(&self.store, 0, &mut y).unwrap();
        let y = y
            .chunks_exact(8)
            .map(|c| f64::from_le_bytes(c.try_into().unwrap()))
            .collect();
        (status, y)
    }
}

impl CallExecution {
    /// Write an Integer (`i64`) or Boolean (`u8`) input to its typed lane
    /// for every following call.
    fn set_input(&mut self, name: &str, value: i64) {
        let (_, representation, offset, _) = self
            .input_lanes
            .iter()
            .find(|(lane, ..)| lane == name)
            .unwrap_or_else(|| panic!("{name} has a typed input lane"))
            .clone();
        match representation.as_str() {
            "i64" => self.inputs[offset..offset + 8].copy_from_slice(&value.to_le_bytes()),
            "u8" => self.inputs[offset] = u8::try_from(value).unwrap(),
            other => panic!("unexpected input lane representation {other}"),
        }
    }
}
