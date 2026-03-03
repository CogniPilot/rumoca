use super::*;

// =============================================================================
// Result JSON write + balance summary printing
// =============================================================================

/// Write MSL test results to a JSON file.
pub(super) fn write_msl_results(summary: &MslSummary) -> io::Result<()> {
    let results_dir = get_msl_cache_dir().join("results");
    fs::create_dir_all(&results_dir)?;

    let json = serde_json::to_string_pretty(summary)?;
    let mut file = File::create(results_dir.join("msl_results.json"))?;
    file.write_all(json.as_bytes())?;

    #[derive(Serialize)]
    struct MslTargetModelList<'a> {
        git_commit: &'a str,
        msl_version: &'a str,
        selection_kind: &'a str,
        model_count: usize,
        model_names: &'a [String],
    }

    let mut compile_target_models: Vec<String> = summary
        .model_results
        .iter()
        .filter(|result| result.phase_reached != "NonSim")
        .map(|result| result.model_name.clone())
        .collect();
    compile_target_models.sort();
    compile_target_models.dedup();
    let compile_targets = MslTargetModelList {
        git_commit: &summary.git_commit,
        msl_version: &summary.msl_version,
        selection_kind: "all_models_except_nonsim",
        model_count: compile_target_models.len(),
        model_names: &compile_target_models,
    };
    let compile_targets_json = serde_json::to_string_pretty(&compile_targets)?;
    let mut compile_target_file = File::create(results_dir.join("msl_compile_targets.json"))?;
    compile_target_file.write_all(compile_targets_json.as_bytes())?;

    let sim_targets = MslTargetModelList {
        git_commit: &summary.git_commit,
        msl_version: &summary.msl_version,
        selection_kind: "standalone_root_examples_without_unbound_inputs_or_unbound_fixed_parameters",
        model_count: summary.sim_target_models.len(),
        model_names: &summary.sim_target_models,
    };
    let sim_targets_json = serde_json::to_string_pretty(&sim_targets)?;
    let mut sim_target_file = File::create(results_dir.join("msl_simulation_targets.json"))?;
    sim_target_file.write_all(sim_targets_json.as_bytes())?;

    println!("\nResults written to {:?}", results_dir);
    Ok(())
}

pub(super) fn print_msl_balance_summary(summary: &MslSummary) {
    println!("\n=== MSL Test Summary ===");
    println!("MSL Version: {}", summary.msl_version);
    println!("Total .mo files: {}", summary.total_mo_files);
    println!();

    // Print class type breakdown
    if !summary.class_type_counts.is_empty() {
        let total_classes: usize = summary.class_type_counts.values().sum();
        println!("Class Types in MSL ({} total):", total_classes);
        let mut sorted: Vec<_> = summary.class_type_counts.iter().collect();
        sorted.sort_by_key(|(_, count)| std::cmp::Reverse(**count));
        for (class_type, count) in &sorted {
            println!("  - {}: {}", class_type, count);
        }
    }
    println!("Simulatable (model+block+class): {}", summary.total_models);
    println!();

    println!("Compilation:");
    println!("  - Needs inner: {} (not failures)", summary.needs_inner);
    println!("  - Instantiate failed: {}", summary.instantiate_failed);
    println!("  - Typecheck failed: {}", summary.typecheck_failed);
    println!("  - Flatten failed: {}", summary.flatten_failed);
    println!("  - ToDae failed: {}", summary.todae_failed);
    println!("  - Non-simulatable: {}", summary.non_sim_models);
    println!("  - Successfully compiled: {}", summary.compiled_models);
    println!();
    println!("Balance Results:");
    println!("  - Balanced: {}", summary.balanced_models);
    println!("  - Unbalanced: {}", summary.unbalanced_models);
    println!(
        "  - Partial (intentionally incomplete): {}",
        summary.partial_models
    );
    println!("Initialization Balance:");
    println!(
        "  - Initial balance OK: {}",
        summary.initial_balanced_models
    );
    println!(
        "  - Initial balance deficit: {}",
        summary.initial_unbalanced_models
    );

    // Calculate balance rate excluding partial models
    let simulatable_models = summary.compiled_models - summary.partial_models;
    if simulatable_models > 0 {
        let balance_rate = (summary.balanced_models as f64 / simulatable_models as f64) * 100.0;
        println!(
            "  - Balance rate: {:.1}% (of {} simulatable models)",
            balance_rate, simulatable_models
        );
        let initial_balance_rate =
            (summary.initial_balanced_models as f64 / simulatable_models as f64) * 100.0;
        println!(
            "  - Initial balance rate: {:.1}% (of {} simulatable models)",
            initial_balance_rate, simulatable_models
        );
    }
    println!();
}
