use super::*;

pub(crate) fn run_lsp_msl_completion_timings(
    root: &Path,
    msl_archive_root: &Path,
    install_vscode_prereqs: bool,
) -> Result<()> {
    let lsp_binary = build_lsp_binary(root)?;
    let modelica_paths = msl_library_paths(msl_archive_root)?;
    let report_dir = root.join("target/lsp-msl-benchmark");
    fs::create_dir_all(&report_dir)
        .with_context(|| format!("failed to create {}", report_dir.display()))?;

    let mut probe_results = Vec::new();
    for probe in FULL_MSL_COMPLETION_PROBES {
        probe_results.push(run_completion_probe(
            root,
            &lsp_binary,
            &modelica_paths,
            msl_archive_root,
            &report_dir,
            *probe,
        )?);
    }
    let lsp_api_validation = match run_lsp_api_validation(
        root,
        &lsp_binary,
        &modelica_paths,
        msl_archive_root,
        &report_dir,
    ) {
        Ok(report) => report,
        Err(error) => {
            print_partial_completion_report(
                &lsp_binary,
                &modelica_paths,
                probe_results,
                empty_lsp_api_validation_report(),
                &format!("LSP API validation failed: {error}"),
            );
            return Err(error);
        }
    };
    let vscode_runtime_smoke =
        run_vscode_runtime_smoke(root, msl_archive_root, &report_dir, install_vscode_prereqs)?;
    let wasm_runtime_smoke = run_wasm_runtime_smoke(root, msl_archive_root, &report_dir)?;
    let wasm_surface_validation = build_surface_coverage_report(root, "WASM", WASM_SURFACE_SPECS)?;
    let vscode_surface_validation =
        build_surface_coverage_report(root, "VS Code", VSCODE_SURFACE_SPECS)?;

    let report = LspMslCompletionBenchmarkReport {
        lsp_binary: lsp_binary.display().to_string(),
        modelica_paths,
        probe_results,
        lsp_api_validation,
        vscode_runtime_smoke,
        wasm_runtime_smoke,
        wasm_surface_validation,
        vscode_surface_validation,
    };
    let report_path = report_dir.join("lsp-msl-completion-report.json");
    fs::write(
        &report_path,
        format!(
            "{}\n",
            serde_json::to_string_pretty(&report).context("failed to encode benchmark report")?
        ),
    )
    .with_context(|| format!("failed to write {}", report_path.display()))?;

    println!("{}", render_lsp_completion_table(&report));
    println!("Report JSON: {}", display_output_path(root, &report_path));
    println!("Timing Dir: {}", display_output_path(root, &report_dir));
    Ok(())
}

fn empty_lsp_api_validation_report() -> LspApiValidationReport {
    LspApiValidationReport {
        operations: Vec::new(),
        warm_latency_snapshot: WarmLatencySnapshot {
            measurements: Vec::new(),
        },
    }
}

fn empty_runtime_smoke_report(area: &str) -> RuntimeSmokeReport {
    RuntimeSmokeReport {
        area: area.to_string(),
        status: "not-run".to_string(),
        note: "not run".to_string(),
        entries: Vec::new(),
    }
}

fn empty_surface_coverage_report(area: &str) -> SurfaceCoverageReport {
    SurfaceCoverageReport {
        area: area.to_string(),
        entries: Vec::new(),
    }
}

fn print_partial_completion_report(
    lsp_binary: &Path,
    modelica_paths: &[String],
    probe_results: Vec<LspCompletionProbeReport>,
    lsp_api_validation: LspApiValidationReport,
    reason: &str,
) {
    let report = LspMslCompletionBenchmarkReport {
        lsp_binary: lsp_binary.display().to_string(),
        modelica_paths: modelica_paths.to_vec(),
        probe_results,
        lsp_api_validation,
        vscode_runtime_smoke: empty_runtime_smoke_report("VS Code"),
        wasm_runtime_smoke: empty_runtime_smoke_report("WASM"),
        wasm_surface_validation: empty_surface_coverage_report("WASM"),
        vscode_surface_validation: empty_surface_coverage_report("VS Code"),
    };
    println!("{}", runtime::render_lsp_completion_table(&report));
    eprintln!("Partial completion report only: {reason}");
}

fn build_lsp_binary(root: &Path) -> Result<PathBuf> {
    println!("Building rumoca-lsp (debug)...");
    let mut cargo_build = Command::new("cargo");
    cargo_build
        .arg("build")
        .arg("--bin")
        .arg("rumoca-lsp")
        .current_dir(root);
    run_status_quiet(cargo_build)?;
    let lsp_binary = root.join("target/debug").join(exe_name("rumoca-lsp"));
    ensure!(
        lsp_binary.is_file(),
        "missing rumoca-lsp binary at {}",
        lsp_binary.display()
    );
    Ok(lsp_binary)
}

fn msl_library_paths(msl_archive_root: &Path) -> Result<Vec<String>> {
    let mut paths = Vec::new();
    for path in [
        msl_archive_root.join("Modelica 4.1.0"),
        msl_archive_root.join("ModelicaServices 4.1.0"),
        msl_archive_root.join("Complex.mo"),
    ] {
        ensure!(
            path.exists(),
            "missing required MSL path {}",
            path.display()
        );
        paths.push(canonicalize_string(&path)?);
    }
    Ok(paths)
}
