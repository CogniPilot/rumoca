use super::*;

struct ArtifactDirectory(PathBuf);

impl ArtifactDirectory {
    fn new(name: &str) -> Self {
        let path =
            std::env::temp_dir().join(format!("rumoca-worker-{name}-{}", std::process::id()));
        fs::create_dir_all(&path).expect("create artifact directory");
        Self(path)
    }
}

impl Drop for ArtifactDirectory {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn compile(source: &str, name: &str) -> Box<DaeCompilationResult> {
    let mut session = Session::default();
    session.add_document("artifact.mo", source).unwrap();
    session
        .compile_model_dae_strict_reachable_uncached_with_recovery(name)
        .expect("compile source DAE")
}

fn state_names(dae: &rumoca_compile::compile::Dae) -> Vec<String> {
    dae.inspect(|view| {
        view.variables()
            .filter(|(_, variable)| variable.role() == VariableRole::State)
            .map(|(_, variable)| variable.name().to_string())
            .collect()
    })
}

#[test]
fn structural_artifact_records_the_dae_that_produced_solve() {
    // MLS §8.6: preserve x's fixed initial value while eliminating v = -x.
    let compiled = compile(
        "model SelectedDerivative
          Real x(start=1, fixed=true);
          Real v;
          Real a;
        equation
          der(x) = v;
          der(v) = a;
          v = -x;
        end SelectedDerivative;",
        "SelectedDerivative",
    );
    assert_eq!(state_names(&compiled.dae), ["x", "v"]);
    let directory = ArtifactDirectory::new("structural-artifact");
    let mut request = tests::simulation_request("SelectedDerivative");
    request.output_dir = directory.0.clone();
    request.emit_json = true;
    request.emit_modelica = true;
    let progress = ProgressLog::new(&request.model_name, directory.0.join("progress.jsonl"));
    let result = build_worker_prepared_simulation(
        &compiled.dae,
        &SimOptions::default(),
        &progress,
        &request,
    )
    .unwrap_or_else(|failure| panic!("build failed: {}", failure.err));
    assert!(result.solve_error.is_none(), "{:?}", result.solve_error);
    let artifact: rumoca_compile::compile::Dae =
        serde_json::from_slice(&fs::read(directory.0.join("ir-structural-dae.json")).unwrap())
            .expect("structural artifact replays checked constructors");
    assert_eq!(state_names(&artifact), ["x"]);
    let bytes = fs::read(directory.0.join("ir-solve.json")).unwrap();
    let solve = rumoca_phase_solve::deserialize_solve_model(
        &mut serde_json::Deserializer::from_slice(&bytes),
    )
    .expect("Solve artifact replays");
    assert_eq!(solve.state_scalar_count(), 1);
    assert_eq!(solve.problem.solve_layout.solver_maps.names[0], "x");
}

#[test]
fn failed_reduction_does_not_publish_an_input_dae_as_structural_output() {
    let compiled = compile(
        "model Underdetermined
          Real x;
          Real y;
        equation
          der(x) = 1;
          der(x) = 2;
        end Underdetermined;",
        "Underdetermined",
    );
    let directory = ArtifactDirectory::new("failed-structural-artifact");
    let mut request = tests::simulation_request("Underdetermined");
    request.output_dir = directory.0.clone();
    request.emit_json = true;
    request.emit_modelica = true;
    let outputs = [
        "ir-structural-dae.json",
        "ir-structural-dae.mo",
        "ir-solve.json",
    ];
    for output in outputs {
        fs::write(directory.0.join(output), "prior result").unwrap();
    }
    let foreign = directory.0.join("notes.txt");
    fs::write(&foreign, "keep").unwrap();
    remove_stale_stage_artifacts(&request);
    let progress = ProgressLog::new(&request.model_name, directory.0.join("progress.jsonl"));
    assert!(
        build_worker_prepared_simulation(
            &compiled.dae,
            &SimOptions::default(),
            &progress,
            &request,
        )
        .is_err()
    );
    for output in outputs {
        assert!(!directory.0.join(output).exists(), "published {output}");
    }
    assert_eq!(fs::read_to_string(foreign).unwrap(), "keep");
}
