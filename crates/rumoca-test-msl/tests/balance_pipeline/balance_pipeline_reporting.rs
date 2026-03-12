use super::*;

// =============================================================================
// Result JSON write + balance summary printing
// =============================================================================

#[derive(Serialize)]
struct MslTargetModelList<'a> {
    git_commit: &'a str,
    msl_version: &'a str,
    selection_kind: &'a str,
    model_count: usize,
    model_names: &'a [String],
    #[serde(skip_serializing_if = "Vec::is_empty")]
    records: Vec<MslTargetModelRecord<'a>>,
}

#[derive(Serialize)]
struct MslTargetModelRecord<'a> {
    model_name: &'a str,
    scheduled_index: usize,
    schedule_lane: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    num_states: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    num_algebraics: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    num_f_x: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    scalar_equations: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    scalar_unknowns: Option<usize>,
}

fn compile_target_lookup(summary: &MslSummary) -> HashMap<&str, &MslModelResult> {
    summary
        .model_results
        .iter()
        .map(|result| (result.model_name.as_str(), result))
        .collect()
}

fn collect_compile_target_models(summary: &MslSummary) -> Vec<String> {
    let mut compile_target_models = Vec::new();
    let mut seen_compile_targets = HashSet::new();
    for result in summary
        .model_results
        .iter()
        .filter(|result| result.phase_reached != "NonSim")
    {
        if seen_compile_targets.insert(result.model_name.clone()) {
            compile_target_models.push(result.model_name.clone());
        }
    }
    compile_target_models
}

fn build_target_records<'a>(
    model_names: &'a [String],
    target_lane_count: usize,
    target_lookup: &HashMap<&'a str, &'a MslModelResult>,
) -> Vec<MslTargetModelRecord<'a>> {
    model_names
        .iter()
        .enumerate()
        .map(|(scheduled_index, model_name)| {
            let metrics = target_lookup.get(model_name.as_str()).copied();
            MslTargetModelRecord {
                model_name,
                scheduled_index,
                schedule_lane: scheduled_index % target_lane_count,
                num_states: metrics.and_then(|result| result.num_states),
                num_algebraics: metrics.and_then(|result| result.num_algebraics),
                num_f_x: metrics.and_then(|result| result.num_f_x),
                scalar_equations: metrics.and_then(|result| result.scalar_equations),
                scalar_unknowns: metrics.and_then(|result| result.scalar_unknowns),
            }
        })
        .collect()
}

fn write_target_model_list(
    results_dir: &Path,
    file_name: &str,
    list: &MslTargetModelList<'_>,
) -> io::Result<()> {
    let json = serde_json::to_string_pretty(list)?;
    let mut file = File::create(results_dir.join(file_name))?;
    file.write_all(json.as_bytes())
}

/// Write MSL test results to a JSON file.
pub(super) fn write_msl_results(summary: &MslSummary) -> io::Result<()> {
    let results_dir = get_msl_cache_dir().join("results");
    fs::create_dir_all(&results_dir)?;

    let json = serde_json::to_string_pretty(summary)?;
    let mut file = File::create(results_dir.join("msl_results.json"))?;
    file.write_all(json.as_bytes())?;
    let compile_target_models = collect_compile_target_models(summary);
    let compile_target_lookup = compile_target_lookup(summary);
    let compile_target_lane_count = msl_stage_parallelism().max(1);
    let compile_target_records = build_target_records(
        &compile_target_models,
        compile_target_lane_count,
        &compile_target_lookup,
    );
    let compile_targets = MslTargetModelList {
        git_commit: &summary.git_commit,
        msl_version: &summary.msl_version,
        selection_kind: "all_models_except_nonsim",
        model_count: compile_target_models.len(),
        model_names: &compile_target_models,
        records: compile_target_records,
    };
    let sim_target_lane_count = simulation_parallelism().max(1);
    let sim_target_records = build_target_records(
        &summary.sim_target_models,
        sim_target_lane_count,
        &compile_target_lookup,
    );
    let sim_targets = MslTargetModelList {
        git_commit: &summary.git_commit,
        msl_version: &summary.msl_version,
        selection_kind: "standalone_root_examples_without_unbound_inputs_or_unbound_fixed_parameters",
        model_count: summary.sim_target_models.len(),
        model_names: &summary.sim_target_models,
        records: sim_target_records,
    };
    write_target_model_list(&results_dir, "msl_compile_targets.json", &compile_targets)?;
    write_target_model_list(&results_dir, "msl_simulation_targets.json", &sim_targets)?;

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
    println!("  - Resolve failed: {}", summary.resolve_failed);
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
