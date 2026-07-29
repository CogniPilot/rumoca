use rumoca_compile::compile::{Session, SessionConfig};

fn compile_decay() -> rumoca_compile::compile::CompilationResult {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "pipeline.mo",
            "model PipelineDecay Real x(start=1); equation der(x)=-x; end PipelineDecay;",
        )
        .expect("fixture parses");
    session
        .compile_model("PipelineDecay")
        .expect("full pipeline reaches checked DAE")
}

#[test]
fn checked_dae_round_trips_wire_v11_and_preserves_source_text() {
    let result = compile_decay();
    let encoded = serde_json::to_string(&result.dae).expect("wire-v11 encoding succeeds");
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_str(&encoded).expect("wire-v11 decodes through checked construction");

    decoded.inspect(|view| {
        assert_eq!(view.continuous_owner_count(), 1);
        let equation = view
            .continuous_equation(0)
            .expect("checked continuous equation exists");
        assert_eq!(view.source_text(equation.provenance()), Some("der(x)=-x"));
    });
}

#[test]
fn checked_dae_flows_through_structural_and_solve_phases() {
    let result = compile_decay();
    result.dae.inspect(|view| {
        let structural = rumoca_compile::phase_structural::sort(view)
            .expect("checked DAE has a perfect structural matching");
        assert_eq!(structural.matching.len(), 1);
    });

    let solve = rumoca_sim::lower_solve_problem(&result.dae)
        .expect("checked DAE lowers to computable Solve IR");
    assert_eq!(solve.layout.y_scalars(), 1);
}

#[test]
fn codegen_context_is_the_checked_wire_shape() {
    let result = compile_decay();
    let json = rumoca_compile::codegen::dae_to_template_json(&result.dae)
        .expect("checked DAE template projection serializes");

    assert_eq!(json["schema_version"], rumoca_ir_dae::DAE_SCHEMA_VERSION);
    assert!(json.get("storage").is_some());
    assert!(json.get("source_map").is_some());
}
