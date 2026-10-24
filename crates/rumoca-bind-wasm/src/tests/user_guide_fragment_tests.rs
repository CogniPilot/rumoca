//! Teaching excerpts compile in their documented complete source context.

use super::user_guide_sources::{GuideFence, guide_fences, repository_root};
use super::*;

fn fragment(context: &str) -> GuideFence {
    guide_fences()
        .into_iter()
        .find(|f| f.fragment_context() == Some(context))
        .unwrap()
}

fn model_source(model: &str) -> String {
    std::fs::read_to_string(repository_root().join(format!("examples/models/{model}.mo"))).unwrap()
}

fn assert_excerpt_in_order(excerpt: &str, source: &str) {
    let mut actual = source.lines();
    for line in excerpt.lines().filter(|line| !line.trim().is_empty()) {
        assert!(
            actual
                .by_ref()
                .any(|candidate| candidate.trim() == line.trim()),
            "exact teaching line absent from complete context: {line}"
        );
    }
}

fn compile_balanced(source: &str, model: &str) {
    let json = compile(source, model).unwrap_or_else(|error| panic!("{model}: {error}"));
    let result: serde_json::Value = serde_json::from_str(&json).unwrap();
    assert_eq!(result["balance"]["is_balanced"], true, "{model}");
}

fn neural_context(context: &str) -> (String, &'static str) {
    let model = match context {
        "neural-tensor" | "neural-width" => "NeuralODETensor",
        "neural-population" => "NeuralPredatorPrey",
        "neural-observed" | "neural-dynamics" => "NeuralLatentOscillator",
        _ => panic!("unclassified neural fragment {context}"),
    };
    let mut source = model_source(model);
    let excerpt = fragment(context).source;
    if context == "neural-width" {
        let original = "parameter Integer nHidden(min = 2) = 32\n    \"Hidden width; nHidden=314 gives about 100k trainable parameters\";";
        assert_eq!(source.matches(original).count(), 1);
        source = source.replacen(original, excerpt.trim(), 1);
    }
    assert_excerpt_in_order(&excerpt, &source);
    (source, model)
}

#[test]
fn user_guide_neural_fragments_compile_in_the_complete_authored_models() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    for context in [
        "neural-tensor",
        "neural-width",
        "neural-population",
        "neural-observed",
        "neural-dynamics",
    ] {
        let (source, model) = neural_context(context);
        let started = std::time::Instant::now();
        compile_balanced(&source, model);
        let mut session = WasmSimulationSession::with_interactive_options(
            &source, model, 0.01, "rk-like", 1e-8, 1e-8, "[]",
        )
        .unwrap();
        for time in [0.0, 0.01, 0.02] {
            session.advance_to(time).unwrap();
            let names = match model {
                "NeuralPredatorPrey" => ["population[1]", "population[2]"],
                "NeuralLatentOscillator" => ["latent[1]", "latent[2]"],
                _ => ["x[1]", "x[2]"],
            };
            for name in names {
                assert!(
                    session.get(name).unwrap().unwrap().is_finite(),
                    "{context}: {name}"
                );
            }
        }
        eprintln!(
            "[guide-fragment] {context} {:.6}s",
            started.elapsed().as_secs_f64()
        );
    }
    clear_source_root_cache().unwrap();
}

fn annotated_decay_context() -> String {
    let source = model_source("SympyDecay");
    assert_eq!(source.matches("Real x(start=1);").count(), 1);
    let source = source.replacen("Real x(start=1);", fragment("state").source.trim(), 1);
    source.replacen(
        "end SympyDecay;",
        &format!("{}end SympyDecay;", fragment("experiment").source),
        1,
    )
}

#[test]
fn user_guide_state_and_experiment_fragments_preserve_start_and_full_curve() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    let source = annotated_decay_context();
    compile_balanced(&source, "SympyDecay");
    let json = simulate_model(&source, "SympyDecay", 0.0, 0.0, "", "{}").unwrap();
    let result: serde_json::Value = serde_json::from_str(&json).unwrap();
    let columns = result["payload"]["allData"].as_array().unwrap();
    let times = columns[0].as_array().unwrap();
    let values = columns[1].as_array().unwrap();
    assert_eq!(times.len(), 3001);
    assert_eq!(times.first().unwrap(), 0.0);
    assert_eq!(times.last().unwrap(), 30.0);
    assert_eq!(values.first().unwrap(), 1.0);
    for (time, value) in times.iter().zip(values) {
        let expected = (-0.5 * time.as_f64().unwrap()).exp();
        assert!((value.as_f64().unwrap() - expected).abs() < 1e-5);
    }
    clear_source_root_cache().unwrap();
}

#[test]
fn user_guide_tank_composition_uses_the_documented_connector_and_modifications() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    let context = guide_fences()
        .into_iter()
        .find(|f| f.model_name().as_deref() == Some("Tank"))
        .unwrap();
    let source = format!("{}{}", context.source, fragment("tanks").source);
    compile_balanced(&source, "TwoTanks");
    let mut session = WasmSimulationSession::with_interactive_options(
        &source, "TwoTanks", 0.01, "rk-like", 1e-10, 1e-10, "[]",
    )
    .unwrap();
    for time in [0.0, 0.5, 1.0] {
        session.advance_to(time).unwrap();
        assert_eq!(session.get("tank1.h").unwrap(), Some(1.0));
        assert_eq!(session.get("tank2.h").unwrap(), Some(1.0));
    }
    let metadata = model_parameter_metadata(&source, "TwoTanks").unwrap();
    let metadata: serde_json::Value = serde_json::from_str(&metadata).unwrap();
    for (name, value) in [("tank1.area", 2.0), ("tank2.area", 0.5)] {
        let parameter = metadata
            .as_array()
            .unwrap()
            .iter()
            .find(|parameter| parameter["name"] == name)
            .unwrap();
        assert_eq!(parameter["defaultValue"], value, "{name}");
    }
    clear_source_root_cache().unwrap();
}

#[test]
fn user_guide_latent_width_variants_use_the_full_authored_source() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    let page = std::fs::read_to_string(
        repository_root().join("docs/user-guide/src/language/neural-odes.md"),
    )
    .unwrap();
    let original = model_source("NeuralLatentOscillator");
    let declaration = "parameter Integer nHidden(min = nFeature) = 128";
    assert_eq!(original.matches(declaration).count(), 1);
    for (width, count) in [(256, 71435), (512, 273931)] {
        assert!(page.contains(&format!("`nHidden = {width}`")));
        assert!(page.contains(&format!("`{count}`")));
        let source = original.replacen(
            declaration,
            &format!("parameter Integer nHidden(min = nFeature) = {width}"),
            1,
        );
        let started = std::time::Instant::now();
        compile_balanced(&source, "NeuralLatentOscillator");
        let mut session = WasmSimulationSession::with_interactive_options(
            &source,
            "NeuralLatentOscillator",
            0.01,
            "rk-like",
            1e-8,
            1e-8,
            "[]",
        )
        .unwrap();
        for time in [0.0, 0.01, 0.02] {
            session.advance_to(time).unwrap();
            for index in 1..=8 {
                assert!(
                    session
                        .get(&format!("latent[{index}]"))
                        .unwrap()
                        .unwrap()
                        .is_finite()
                );
            }
        }
        eprintln!(
            "[guide-prose-width] {width} {count}parameters {:.6}s",
            started.elapsed().as_secs_f64()
        );
    }
    clear_source_root_cache().unwrap();
}

#[test]
fn user_guide_predator_width_313_uses_the_full_authored_source() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    let page = std::fs::read_to_string(
        repository_root().join("docs/user-guide/src/language/neural-odes.md"),
    )
    .unwrap();
    assert!(page.contains("`H = 313`"));
    assert!(page.contains("`100162`"));
    let original = model_source("NeuralPredatorPrey");
    let declaration = "parameter Integer nHidden(min = 3) = 24";
    assert_eq!(original.matches(declaration).count(), 1);
    let source = original.replacen(declaration, "parameter Integer nHidden(min = 3) = 313", 1);
    let started = std::time::Instant::now();
    compile_balanced(&source, "NeuralPredatorPrey");
    assert_predator_parameter_cardinality(&source);
    let mut session = WasmSimulationSession::with_interactive_options(
        &source,
        "NeuralPredatorPrey",
        0.01,
        "rk-like",
        1e-8,
        1e-8,
        "[]",
    )
    .unwrap();
    for time in [0.0, 0.01, 0.02] {
        session.advance_to(time).unwrap();
        for index in 1..=2 {
            assert!(
                session
                    .get(&format!("population[{index}]"))
                    .unwrap()
                    .unwrap()
                    .is_finite()
            );
        }
    }
    eprintln!(
        "[guide-prose-width] PredatorPrey313 100162parameters {:.6}s",
        started.elapsed().as_secs_f64()
    );
    clear_source_root_cache().unwrap();
}

/// The canyon-run excerpts are exact, in order, lines of the shipped example;
/// the example itself compiles through the scenario test.
#[test]
fn user_guide_canyon_run_fragments_are_lines_of_the_shipped_example() {
    let source = std::fs::read_to_string(
        repository_root().join("examples/interactive/canyon_run/CanyonRun.mo"),
    )
    .unwrap();
    for context in ["canyon-forward", "canyon-agent"] {
        assert_excerpt_in_order(&fragment(context).source, &source);
    }
}

fn assert_predator_parameter_cardinality(source: &str) {
    crate::with_singleton_session(|session| {
        session.update_document("input.mo", source);
        let requested = crate::qualify_input_model_name(session, "NeuralPredatorPrey");
        let compilation = crate::compile_requested_model(session, &requested)?;
        compilation.dae.inspect(|view| {
            let mut scalar_count = 0;
            for (name, dimensions) in [
                ("W1", &[313, 3][..]),
                ("b1", &[313][..]),
                ("Wmid", &[313, 313][..]),
                ("bmid", &[313][..]),
                ("W2", &[2, 313][..]),
                ("b2", &[2][..]),
            ] {
                let (_, parameter) = view
                    .variables()
                    .find(|(_, variable)| variable.name().as_str() == name)
                    .unwrap_or_else(|| panic!("missing authored network parameter {name}"));
                assert_eq!(
                    parameter.role(),
                    rumoca_compile::compile::VariableRole::Parameter,
                    "{name}"
                );
                assert_eq!(
                    serde_json::to_value(parameter.value_type().scalar_type()).unwrap(),
                    "real",
                    "{name}"
                );
                assert_eq!(parameter.value_type().dimensions(), dimensions, "{name}");
                scalar_count += parameter.scalar_count();
            }
            assert_eq!(scalar_count, 100162);
        });
        Ok(())
    })
    .unwrap();
}
