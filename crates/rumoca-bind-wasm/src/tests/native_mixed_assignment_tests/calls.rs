//! Native model call scheduling, from the unchanged source and checked owners.
mod gathers;
mod inventory;
mod lazy_windows;
mod maps;
mod packed;
mod return_arguments;
mod returns;
mod typed_maps;

use super::*;

#[test]
fn native_registration_whole_program_source_inventory() {
    let Ok(path) = std::env::var("RUMOCA_NATIVE_REGISTRATION_SOURCE_FIXTURE") else {
        return;
    };
    let _lock = session_test_guard();
    let source = std::fs::read_to_string(path).unwrap();
    let model_name = std::env::var("RUMOCA_NATIVE_SOURCE_MODEL")
        .unwrap_or_else(|_| "RigidPointRegistration".into());
    crate::native_assignment_api::with_prepared_native_model(
        &source,
        &model_name,
        |model, source, model_name| {
            let inventory = inventory::capture(model)?;
            eprintln!(
                "NATIVE_CALL_REFUSAL {:?}",
                model
                    .problem
                    .continuous
                    .refresh_owners
                    .native_assignment_refusal()
            );
            if let Ok(path) = std::env::var("RUMOCA_NATIVE_PROGRAM_ARTIFACT") {
                let artifact =
                    crate::native_program_api::model_artifact(model, source, model_name)?;
                std::fs::write(path, artifact).unwrap();
            }
            Ok(inventory)
        },
    )
    .unwrap();
}

#[test]
fn native_unchanged_full_registration_admits_status_program_and_exports_exact_source() {
    let Ok(path) = std::env::var("RUMOCA_NATIVE_REGISTRATION_SOURCE_FIXTURE") else {
        return;
    };
    let _lock = session_test_guard();
    let source = std::fs::read_to_string(path).unwrap();
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_program_api::prepare_native_program_impl(&source, "RigidPointRegistration")
            .unwrap(),
    )
    .unwrap();
    assert_eq!(artifact["profile"], "native-direct-program-f64-v3");
    assert_eq!(artifact["abi"]["result"], "status:i32");
    assert_eq!(artifact["abi"]["transactional_y"], true);
    assert_eq!(artifact["issued_schedule"].as_array().unwrap().len(), 12);
    if let Ok(path) = std::env::var("RUMOCA_NATIVE_PROGRAM_ARTIFACT") {
        std::fs::write(path, serde_json::to_string(&artifact).unwrap()).unwrap();
    }
    verify_full_registration(&artifact);
}

fn verify_full_registration(artifact: &serde_json::Value) {
    let mut execution = CallExecution::new(artifact);
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|v| v.as_f64().unwrap())
        .collect::<Vec<_>>();
    let n = 14400;
    let c = 0.31_f64.cos();
    let s = 0.31_f64.sin();
    parameters[slot(artifact, "activeCount", "P")] = n as f64;
    for i in 1..=n {
        let p = [
            (i % 37) as f64 / 7. - 2.,
            (i % 41) as f64 / 11. - 1.,
            (i % 43) as f64 / 13. - 3.,
        ];
        let q = [
            c * p[0] - s * p[1] + 0.7,
            s * p[0] + c * p[1] - 0.2,
            p[2] + 1.1,
        ];
        for j in 1..=3 {
            parameters[slot(artifact, &format!("sourcePoint[{i},{j}]"), "P")] = p[j - 1];
            parameters[slot(artifact, &format!("targetPoint[{i},{j}]"), "P")] = q[j - 1];
        }
        parameters[slot(artifact, &format!("pairEnabled[{i}]"), "P")] = 1.;
    }
    let outputs = execution.run(&parameters);
    assert_eq!(outputs[slot(artifact, "accepted", "Y")], 1.);
    assert_eq!(outputs[slot(artifact, "validCount", "Y")], 14400.);
    assert_eq!(outputs[slot(artifact, "rejectionReason", "Y")], 0.);
    assert!(outputs[slot(artifact, "rms", "Y")] < 1e-9);
    for (i, expected) in [0.7, -0.2, 1.1].iter().enumerate() {
        assert!(
            (outputs[slot(artifact, &format!("translation[{}]", i + 1), "Y")] - expected).abs()
                < 1e-9
        );
    }
    for (name, value) in [("activeCount", -1.), ("maximumRms", -1.)] {
        let index = slot(artifact, name, "P");
        let saved = parameters[index];
        parameters[index] = value;
        let rejected = execution.run(&parameters);
        assert_eq!(rejected[slot(artifact, "accepted", "Y")], 0.);
        assert!(rejected.iter().all(|v| v.is_finite()));
        parameters[index] = saved;
        assert_eq!(
            execution.run(&parameters)[slot(artifact, "accepted", "Y")],
            1.
        );
    }
}

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
        }
    }
    fn run(&mut self, p: &[f64]) -> Vec<f64> {
        let bytes = p.iter().flat_map(|v| v.to_le_bytes()).collect::<Vec<_>>();
        self.memory.write(&mut self.store, self.p, &bytes).unwrap();
        let arguments = (0, self.p as i32, 0., self.scratch as i32, 0);
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
