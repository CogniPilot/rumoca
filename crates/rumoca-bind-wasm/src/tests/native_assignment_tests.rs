use super::*;

const TWO_STAGE: &str = r#"
model NativeImage
  input Real rgb[48] = fill(0.0, 48);
  Real gray[16];
  output Real score[16];
equation
  for i in 1:16 loop
    gray[i] = (rgb[3*i-2] + rgb[3*i-1] + rgb[3*i])/3;
  end for;
  for i in 1:16 loop
    score[i] = gray[i]*gray[i] + 2;
  end for;
end NativeImage;
"#;

#[test]
// SPEC_0021: Exception - one fixture checks issued order, portable execution, changing inputs, and result bits.
#[allow(clippy::too_many_lines)]
fn prepare_native_assignments_uses_issued_two_stage_modelica_values() {
    let _lock = session_test_guard();
    let text =
        crate::native_assignment_api::prepare_native_assignments_impl(TWO_STAGE, "NativeImage")
            .unwrap();
    let artifact: serde_json::Value = serde_json::from_str(&text).unwrap();
    assert_eq!(artifact["profile"], "native-direct-assignments-f64-v1");
    assert_eq!(artifact["abi"]["y_count"], 32);
    // Every unknown has exactly one issued owner.
    assert_eq!(issued_target_count(&artifact), 32);
    assert_eq!(artifact["abi"]["p_count"], 48);
    assert_eq!(artifact["source_sha256"].as_str().unwrap().len(), 64);
    let edited = TWO_STAGE.replace("+ 2;", "+ 3;");
    let changed: serde_json::Value = serde_json::from_str(
        &crate::native_assignment_api::prepare_native_assignments_impl(&edited, "NativeImage")
            .unwrap(),
    )
    .unwrap();
    assert_ne!(changed["source_sha256"], artifact["source_sha256"]);
    assert_ne!(stage_modules(&changed), stage_modules(&artifact));
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
        .collect::<Vec<_>>();
    let write =
        |memory: wasmi::Memory, store: &mut wasmi::Store<()>, offset: usize, values: &[f64]| {
            let bytes = values
                .iter()
                .flat_map(|value| value.to_le_bytes())
                .collect::<Vec<_>>();
            memory.write(store, offset, &bytes).unwrap();
        };
    for frame in 0..8 {
        let rgb = (0..48)
            .map(|index| ((index * 7 + frame * 13) % 255) as f64 / 255.0)
            .collect::<Vec<_>>();
        write(memory, &mut store, pointers[0] as usize, &[f64::NAN; 32]);
        write(memory, &mut store, pointers[1] as usize, &rgb);
        for (kernel, target, count) in &stages {
            kernel
                .call(
                    &mut store,
                    (
                        pointers[0],
                        pointers[1],
                        frame as f64,
                        pointers[2],
                        pointers[3],
                    ),
                )
                .unwrap();
            let mut bytes = vec![0; count * 8];
            memory
                .read(&store, pointers[3] as usize, &mut bytes)
                .unwrap();
            memory
                .write(&mut store, pointers[0] as usize + target * 8, &bytes)
                .unwrap();
        }
        let mut bytes = vec![0; 32 * 8];
        memory
            .read(&store, pointers[0] as usize, &mut bytes)
            .unwrap();
        let values = bytes
            .chunks_exact(8)
            .map(|bytes| f64::from_le_bytes(bytes.try_into().unwrap()))
            .collect::<Vec<_>>();
        let layout = &artifact["var_layout"]["bindings"];
        let gray_start = layout["gray"]["Y"]["index"].as_u64().unwrap() as usize;
        let score_start = layout["score"]["Y"]["index"].as_u64().unwrap() as usize;
        for index in 0..16 {
            let gray = (rgb[index * 3] + rgb[index * 3 + 1] + rgb[index * 3 + 2]) / 3.0;
            assert_eq!(values[gray_start + index].to_bits(), gray.to_bits());
            assert_eq!(
                values[score_start + index].to_bits(),
                (gray * gray + 2.0).to_bits()
            );
        }
    }
}

#[test]
fn native_preparation_preserves_declared_host_input_start() {
    let _lock = session_test_guard();
    let source = "model NativeStart input Real u[16](each start=3); output Real y[16]; equation for i in 1:16 loop y[i]=2*u[i]; end for; end NativeStart;";
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_assignment_api::prepare_native_assignments_impl(source, "NativeStart")
            .unwrap(),
    )
    .unwrap();
    let first = artifact["var_layout"]["bindings"]["u"]["P"]["index"]
        .as_u64()
        .unwrap() as usize;
    let parameters = artifact["parameters"].as_array().unwrap();
    for value in &parameters[first..first + 16] {
        assert_eq!(value.as_f64(), Some(3.0));
    }
    assert_eq!(issued_target_count(&artifact), 16);
}

/// Total unknowns the issued stages assign. An algebraic family issues one
/// stage per scalar row; compact families return with Map-row exact
/// certification on the AffineKernelPlan owner.
pub(super) fn issued_target_count(artifact: &serde_json::Value) -> u64 {
    artifact["stages"]
        .as_array()
        .unwrap()
        .iter()
        .map(|stage| stage["target_count"].as_u64().unwrap())
        .sum()
}

fn stage_modules(artifact: &serde_json::Value) -> Vec<&serde_json::Value> {
    artifact["stages"]
        .as_array()
        .unwrap()
        .iter()
        .map(|stage| &stage["module_sha256"])
        .collect()
}

#[test]
fn native_preparation_refuses_coupled_scalar_and_stateful_models() {
    let _lock = session_test_guard();
    for (name, source) in [
        (
            "Scalar",
            "model Scalar output Real y; equation y*y=2; end Scalar;",
        ),
        (
            "State",
            "model State Real x(start=0); equation der(x)=1; end State;",
        ),
    ] {
        assert!(
            crate::native_assignment_api::prepare_native_assignments_impl(source, name).is_err()
        );
    }
}

#[test]
fn native_model_wire_reissues_source_bound_schedule() {
    let mut session = Session::default();
    session.update_document("input.mo", TWO_STAGE);
    let compilation = compile_requested_model(&mut session, "NativeImage").unwrap();
    let problem = rumoca_sim::lower_solve_problem(&compilation.dae).unwrap();
    let original = issued_stage_ranges(&problem);
    assert_eq!(
        original
            .iter()
            .map(|(_, targets)| targets.len())
            .sum::<usize>(),
        32
    );
    let wire = serde_json::to_string(&problem).unwrap();
    assert!(!wire.contains("native_assignment_schedule"));
    let replay: rumoca_ir_solve::SolveProblem = serde_json::from_str(&wire).unwrap();
    assert_eq!(issued_stage_ranges(&replay), original);
}

/// The issued native stages as (canonical source node, target range).
pub(super) fn issued_stage_ranges(
    problem: &rumoca_ir_solve::SolveProblem,
) -> Vec<(usize, std::ops::Range<usize>)> {
    problem
        .continuous
        .refresh_owners
        .native_assignment_schedule()
        .unwrap()
        .stages()
        .iter()
        .map(|stage| (stage.source_node(), stage.target_range().unwrap()))
        .collect()
}
