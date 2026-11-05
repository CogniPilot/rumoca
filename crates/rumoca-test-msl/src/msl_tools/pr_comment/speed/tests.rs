use super::*;

/// A (high, 5 equations) is faster than OMC everywhere; B (near, 12
/// equations) is slower than OMC in simulation; C is in the
/// deviation band and D's OMC timing was taken under another worker count, so
/// neither is timed.
fn write_inputs(results: &Path, omc_workers: u64, rumoca_workers: u64, stage_workers: u64) {
    let rumoca_row =
        |name: &str, eqs: u64, front: f64, solve: f64, build: f64, ic: f64, run: f64| {
            serde_json::json!({
                "model_name": name,
                "scalar_equations": eqs,
                "compile_seconds": front,
                "instantiate_seconds": front * 0.1,
                "typecheck_seconds": front * 0.1,
                "flatten_seconds": front * 0.6,
                "dae_seconds": front * 0.2,
                "ir_solve_seconds": solve,
                "sim_backend_build_seconds": build - solve,
                "sim_build_seconds": build,
                "ic_seconds": ic,
                "sim_run_seconds": run,
                "strict_plan_warm": true,
                "sim_settings": {
                    "requested_solver": "auto", "integrator": "bdf", "rtol": 1e-6, "atol": 1e-6,
                    "output_intervals": 500, "output_points": 501, "steps": 40, "events": 2
                }
            })
        };
    let msl = serde_json::json!({ "model_results": [
        rumoca_row("A", 5, 1.0, 0.2, 0.5, 0.1, 0.5),
        rumoca_row("B", 12, 2.0, 1.0, 1.5, 0.5, 3.0),
        rumoca_row("C", 12, 2.0, 1.0, 1.5, 0.5, 3.0),
        rumoca_row("D", 12, 2.0, 1.0, 1.5, 0.5, 3.0),
    ]});
    let host = serde_json::json!({
        "logical_cpus": 4, "physical_cores": 2, "image": "ubuntu24",
        "runner_environment": "github-hosted"
    });
    let omc_row = |total: f64, sim: f64, work: f64, workers: u64| {
        serde_json::json!({
            "status": "success",
            "total_system_seconds": total,
            "sim_system_seconds": sim,
            "omc_phases": {
                "frontend": work * 0.5, "backend": work * 0.3, "sim_code": work * 0.1,
                "templates": work * 0.1, "compile": total - sim - work,
                "simulation": sim, "total": total
            },
            "omc_settings": {
                "method": "dassl", "tolerance": 1e-6, "number_of_intervals": 500,
                "start_time": 0.0, "stop_time": 1.0
            },
            "omc_timing_context": { "workers": workers, "omc_threads": 1, "host": host }
        })
    };
    let omc = serde_json::json!({
        "models": {
            "A": omc_row(5.0, 1.0, 2.4, omc_workers),
            "B": omc_row(8.0, 2.0, 2.0, omc_workers),
            "C": omc_row(8.0, 2.0, 2.0, omc_workers),
            "D": omc_row(8.0, 2.0, 2.0, omc_workers + 5)
        },
        "timing": {
            "workers_used": omc_workers, "rumoca_sim_workers": rumoca_workers,
            "rumoca_stage_workers": stage_workers, "omc_threads": 1, "shards": 4,
            "batches_total": 4, "batches_ran": 3, "host": host
        }
    });
    let bands = serde_json::json!({ "rows": [
        { "model_name": "A", "band": "high" },
        { "model_name": "B", "band": "near" },
        { "model_name": "C", "band": "deviation" },
        { "model_name": "D", "band": "high" }
    ]});
    for (name, value) in [
        ("msl_results.json", msl),
        ("omc_simulation_reference.json", omc),
        ("msl_band_table.json", bands),
    ] {
        fs::write(results.join(name), value.to_string()).expect("write fixture");
    }
}

fn render_with(omc_workers: u64, rumoca_workers: u64, stage_workers: u64) -> String {
    let temp = tempfile::tempdir().expect("tempdir");
    write_inputs(temp.path(), omc_workers, rumoca_workers, stage_workers);
    render_speed_section(temp.path()).expect("render")
}

#[test]
fn every_aggregate_is_over_the_comparators_agreeing_set_with_a_high_only_line() {
    let rendered = render_with(2, 2, 2);
    assert!(
        rendered.contains("**2 trace-agreeing models**"),
        "{rendered}"
    );
    assert!(rendered.contains("1 high, 1 near"));
    // Total: rumoca 2.1 + 7.0 (initialization included), OMC 5 + 8.
    assert!(
        rendered.contains("| Total (model to results) | 2 | 9.1 | 13.0 | **1.43** | **1.76** |")
    );
    // Runnable: rumoca 1.5 + 3.5, OMC 4 + 6.
    assert!(
        rendered.contains("| Time to runnable (JIT vs C toolchain) | 2 | 5.0 | 10.0 | **2.00** |")
    );
    // Simulation: rumoca 0.6 + 3.5 (initialization included), OMC 1 + 2.
    assert!(rendered.contains("| Simulation | 2 | 4.1 | 3.0 | **0.73** |"));
    assert!(rendered.contains("High band only (1 models)"));
    assert!(rendered.contains("Time to runnable (JIT vs C toolchain) 2.67×"));
    assert!(rendered.contains("**FMU path:** not measured in CI"));
    assert!(rendered.contains("`timeTotal - timeSimulation`, which includes `timeCompile`"));
    assert!(rendered.contains("OMC loads the library once per session with `loadModel`"));
    assert!(!rendered.contains("Compiler work"), "{rendered}");
    assert!(!rendered.contains("timeFrontend"), "{rendered}");
    assert!(rendered.contains("`timeSimulation`, which includes its initialization"));
}

#[test]
fn the_size_tables_and_slower_list_name_the_dominant_cost() {
    let rendered = render_with(2, 2, 2);
    assert!(rendered.contains("| 1–9 | 1 |"));
    assert!(rendered.contains("| 10–24 | 1 |"));
    assert!(rendered.contains("##### Simulation (1 slower)"));
    assert!(rendered.contains("| `B` | 12 | 0.57 | 40 steps, 2 events, 501 output points |"));
    assert!(rendered.contains("##### Time to runnable (JIT vs C toolchain) (0 slower)"));
    assert!(!rendered.contains("`C`"));
    assert!(!rendered.contains("`D`"));
}

#[test]
fn the_methodology_states_recorded_conditions_and_flags_unequal_conditions() {
    let rendered = render_with(2, 2, 2);
    assert!(rendered.contains(
        "image ubuntu24, environment github-hosted, 4 logical CPUs, 2 physical cores; 4 shard(s)"
    ));
    assert!(rendered.contains(
        "compile contention equal on both tools; simulation contention equal on both tools"
    ));
    assert!(rendered.contains("rumoca integrator bdf (2/2), requested_solver auto (2/2)"));
    assert!(rendered.contains("OMC method dassl (2/2)"));
    assert!(
        rendered.contains("Output density: equal on every model: 2 models"),
        "{rendered}"
    );
    assert!(rendered.contains("Initialization: 15% of rumoca's simulation seconds"));
    assert!(rendered.contains("OMC references ran for 3 of 4 models this run (1 reused from cache); 1 agreeing models are excluded"));
    assert!(rendered.contains("Front end scope: rumoca"));

    let omc_heavier = render_with(8, 3, 3);
    assert!(
        omc_heavier.contains(
            "OMC ran 8 simulation tasks at a time against rumoca's 3, which favours rumoca"
        )
    );
    let rumoca_heavier = render_with(2, 3, 3);
    assert!(
        rumoca_heavier
            .contains("rumoca ran 3 compile tasks at a time against OMC's 2, which favours OMC")
    );
    assert!(
        rumoca_heavier
            .contains("rumoca ran 3 simulation tasks at a time against OMC's 2, which favours OMC")
    );
}

#[test]
fn unequal_output_grids_are_flagged() {
    let temp = tempfile::tempdir().expect("tempdir");
    write_inputs(temp.path(), 2, 2, 2);
    let path = temp.path().join("msl_results.json");
    let text = fs::read_to_string(&path).expect("read").replacen(
        "\"output_intervals\":500",
        "\"output_intervals\":400",
        1,
    );
    fs::write(&path, text).expect("write");
    let rendered = render_speed_section(temp.path()).expect("render");
    assert!(rendered.contains("**unequal or unrecorded on 1 models**: 1 models with the same number of grid intervals on both tools, 1 different"), "{rendered}");
}

#[test]
fn missing_inputs_report_not_measured() {
    let temp = tempfile::tempdir().expect("tempdir");
    let rendered = render_speed_section(temp.path()).expect("render");
    assert!(rendered.contains("Speed vs OMC not measured"));
}

#[test]
fn a_compile_that_built_the_plan_is_counted_and_still_timed() {
    let temp = tempfile::tempdir().expect("tempdir");
    write_inputs(temp.path(), 2, 2, 2);
    let path = temp.path().join("msl_results.json");
    let mut msl: Value =
        serde_json::from_str(&fs::read_to_string(&path).expect("read")).expect("json");
    for row in msl["model_results"].as_array_mut().expect("rows") {
        if row["model_name"] == "B" {
            row["strict_plan_warm"] = Value::Bool(false);
        }
    }
    fs::write(&path, msl.to_string()).expect("write");
    let rendered = render_speed_section(temp.path()).expect("render");
    assert!(
        rendered.contains(
            "1 timed rumoca compile(s) built the plan themselves and 0 row(s) do not record it; \
             a plan built in the compile is in that model's `compile_seconds`."
        ),
        "{rendered}"
    );
    assert!(!rendered.contains("Every timed rumoca compile started from the prepared plan"));
    // Every comparison still times both models.
    assert!(rendered.contains("| Total (model to results) | 2 |"));
    assert!(rendered.contains("| Time to runnable (JIT vs C toolchain) | 2 |"));
    assert!(rendered.contains("| Simulation | 2 |"));
}

#[test]
fn warm_plans_are_stated_from_the_rows() {
    let rendered = render_with(2, 2, 2);
    assert!(rendered.contains(
        "Every timed rumoca compile started from the prepared plan, so neither one-time load is \
         in a per-model timer."
    ));
}

#[test]
fn every_timing_table_names_the_actual_backend_settings_groups() {
    let temp = tempfile::tempdir().expect("tempdir");
    write_inputs(temp.path(), 2, 2, 2);
    let msl_path = temp.path().join("msl_results.json");
    let mut msl: Value = serde_json::from_str(&fs::read_to_string(&msl_path).unwrap()).unwrap();
    msl["model_results"][0]["sim_settings"]["execution"] =
        serde_json::json!({"engine":"cranelift"});
    msl["model_results"][1]["sim_settings"]["execution"] =
        serde_json::json!({"engine":"interpreter"});
    msl["model_results"][1]["sim_settings"]["integrator"] = "rk45".into();
    fs::write(&msl_path, msl.to_string()).unwrap();
    let omc_path = temp.path().join("omc_simulation_reference.json");
    let mut omc: Value = serde_json::from_str(&fs::read_to_string(&omc_path).unwrap()).unwrap();
    omc["omc_version"] = "OpenModelica recorded-test-version".into();
    omc["models"]["A"]["omc_settings"]["cflags"] = "".into();
    omc["models"]["A"]["omc_settings"]["simflags"] = "".into();
    omc["models"]["B"]["omc_settings"]["method"] = "cvode".into();
    omc["models"]["B"]["omc_settings"]["cflags"] = "-O2".into();
    omc["models"]["B"]["omc_settings"]["simflags"] = "-s=cvode".into();
    fs::write(&omc_path, omc.to_string()).unwrap();
    let rendered = render_speed_section(temp.path()).unwrap();
    assert_eq!(
        rendered
            .matches("Recorded execution backends for this table")
            .count(),
        5
    );
    assert!(rendered.contains("mixed paired groups"));
    assert!(rendered.contains("Rumoca engine cranelift, solver bdf"));
    assert!(rendered.contains("Rumoca engine interpreter, solver rk45"));
    assert!(rendered.contains(
        "OpenModelica recorded-test-version, solver cvode, cflags -O2, simflags -s=cvode"
    ));
    assert!(rendered.contains("solver dassl, cflags (empty), simflags (empty)"));
    // Existing timings/statistics/cohort remain exact despite metadata changes.
    assert!(rendered.contains("| Simulation | 2 | 4.1 | 3.0 | **0.73** |"));
}

#[test]
fn historical_backend_metadata_is_unrecorded_without_guessing_current_engine() {
    let rendered = render_with(2, 2, 2);
    assert!(rendered.contains("Rumoca engine unrecorded, solver bdf"));
    assert!(
        rendered.contains(
            "OpenModelica unrecorded, solver dassl, cflags unrecorded, simflags unrecorded"
        )
    );
    assert!(!rendered.contains("Rumoca engine cranelift"));
    assert_eq!(backend_label(Some("a|b`c\nd")), "a\\|b\\`c d");
}

#[test]
fn slower_table_header_excludes_the_eleventh_hidden_backend() {
    let temp = tempfile::tempdir().expect("tempdir");
    write_inputs(temp.path(), 2, 2, 2);
    let (base, _) = SpeedInputs::read(temp.path()).unwrap().unwrap().records();
    let records = (0..11)
        .map(|index| {
            let mut record = base[0].clone();
            let hidden = index == 10;
            let seconds = if hidden { 3.0 } else { 20.0 + f64::from(index) };
            record.model_name = format!("Model{index:02}");
            record.rumoca.front_end = seconds;
            record.rumoca.sim_build = seconds;
            record.rumoca.initialization = 1.0;
            record.rumoca.run = seconds;
            record.omc.runnable = 1.0;
            record.omc.simulation = 1.0;
            record.omc.total = 2.0;
            record.rumoca_settings.as_mut().unwrap()["execution"] = serde_json::json!({
                "engine": if hidden { "hidden-wasm" } else { "cranelift" },
            });
            record
        })
        .collect::<Vec<_>>();
    let rendered = render_slower_than_omc(&records);
    // The heading reports the full slower cohort, while the table and its
    // backend caption both describe the ten displayed rows, in every phase.
    assert_eq!(rendered.matches("(11 slower)").count(), 3);
    assert_eq!(
        rendered
            .matches("10 model(s): Rumoca engine cranelift")
            .count(),
        3
    );
    assert!(!rendered.contains("hidden-wasm"));
    assert!(!rendered.contains("`Model10`"));
    assert_eq!(rendered.matches("| `Model").count(), 30);
}
