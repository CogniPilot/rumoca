//! FMU target discovery — determines which of the 180 MSL simulation targets
//! can successfully pass through the FMI2 and FMI3 C template pipelines.
//!
//! For each model, tests: compile → has states → render template → compile C.
//! Outputs two JSON files with the passing model lists.
//!
//! Run with:
//! ```text
//! cargo test --release --package rumoca-test-msl --test fmu_target_discovery -- --ignored --nocapture
//! ```

use flate2::read::GzDecoder;
use rumoca_session::compile::{CompilationResult, CompiledLibrary, PhaseResult};
use rumoca_session::parsing::parse_files_parallel_lenient;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::Command;
use tar::Archive;
use tempfile::tempdir;
use walkdir::WalkDir;

// =============================================================================
// MSL infrastructure (same as other MSL test files)
// =============================================================================

const MSL_VERSION: &str = "v4.1.0";
const MSL_URL: &str =
    "https://github.com/modelica/ModelicaStandardLibrary/archive/refs/tags/v4.1.0.tar.gz";

fn get_msl_cache_dir() -> PathBuf {
    let cache_dir =
        rumoca_session::compile::core::msl_cache_dir_from_manifest(env!("CARGO_MANIFEST_DIR"));
    fs::create_dir_all(&cache_dir).expect("Failed to create MSL cache directory");
    cache_dir
}

fn get_msl_dir() -> PathBuf {
    get_msl_cache_dir().join(format!("ModelicaStandardLibrary-{}", &MSL_VERSION[1..]))
}

fn msl_exists() -> bool {
    let msl_dir = get_msl_dir();
    msl_dir.exists() && msl_dir.join("Modelica").exists()
}

fn ensure_msl_downloaded() -> std::io::Result<PathBuf> {
    let msl_dir = get_msl_dir();
    if msl_exists() {
        println!("MSL {} already cached at {:?}", MSL_VERSION, msl_dir);
        return Ok(msl_dir);
    }
    println!("Downloading MSL {} from GitHub...", MSL_VERSION);
    let response = ureq::get(MSL_URL)
        .call()
        .map_err(|e| std::io::Error::other(format!("Download failed: {e}")))?;
    let mut data = Vec::new();
    response
        .into_reader()
        .read_to_end(&mut data)
        .map_err(|e| std::io::Error::other(format!("Read failed: {e}")))?;
    println!("Downloaded {} bytes, extracting...", data.len());
    let tar = GzDecoder::new(&data[..]);
    let mut archive = Archive::new(tar);
    archive.unpack(get_msl_cache_dir())?;
    println!("Extracted MSL to {:?}", msl_dir);
    Ok(msl_dir)
}

fn find_mo_files(msl_dir: &Path) -> Vec<PathBuf> {
    let has_modelica_versioned = msl_dir.join("Modelica 4.1.0").is_dir();
    let has_modelica_services_versioned = msl_dir.join("ModelicaServices 4.1.0").is_dir();
    let has_modelica_reference_versioned = msl_dir.join("ModelicaReference 4.1.0").is_dir();

    WalkDir::new(msl_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| {
            let path = e.path();
            let path_str = path.to_string_lossy();
            let is_unversioned_alias = path
                .strip_prefix(msl_dir)
                .ok()
                .and_then(|relative| relative.components().next())
                .and_then(|component| component.as_os_str().to_str())
                .is_some_and(|top| {
                    (top == "Modelica" && has_modelica_versioned)
                        || (top == "ModelicaServices" && has_modelica_services_versioned)
                        || (top == "ModelicaReference" && has_modelica_reference_versioned)
                });
            path.is_file()
                && path.extension().is_some_and(|ext| ext == "mo")
                && !path_str.contains("Obsolete")
                && !path_str.contains("ModelicaTestConversion")
                && !path_str.contains("ModelicaTest/")
                && !is_unversioned_alias
        })
        .map(|e| e.path().to_path_buf())
        .collect()
}

fn load_target_models() -> Vec<String> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/msl_tests/msl_simulation_targets_180.json");
    let raw = fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("failed to read target list {}: {e}", path.display()));
    let value: serde_json::Value =
        serde_json::from_str(&raw).expect("failed to parse target list JSON");
    value
        .as_array()
        .expect("target list must be a JSON array")
        .iter()
        .map(|v| v.as_str().expect("each entry must be a string").to_string())
        .collect()
}

// =============================================================================
// Pipeline probing — render template + compile C (no simulation)
// =============================================================================

fn try_fmi_c_compile(
    dae: &rumoca_ir_dae::Dae,
    model_name: &str,
    model_template: &str,
    driver_template: &str,
) -> Result<(), String> {
    let model_c = rumoca_phase_codegen::render_template_with_name(dae, model_template, model_name)
        .map_err(|e| format!("render model: {e}"))?;

    let driver_c =
        rumoca_phase_codegen::render_template_with_name(dae, driver_template, model_name)
            .map_err(|e| format!("render driver: {e}"))?;

    let dir = tempdir().map_err(|e| format!("tempdir: {e}"))?;
    let model_path = dir.path().join("model.c");
    let driver_path = dir.path().join("driver.c");
    let binary_path = dir.path().join("test_fmu");

    fs::write(&model_path, &model_c).map_err(|e| format!("write model.c: {e}"))?;
    fs::write(&driver_path, &driver_c).map_err(|e| format!("write driver.c: {e}"))?;

    let compile_output = Command::new("cc")
        .args([
            "-O2",
            "-Wall",
            "-Wno-unused-variable",
            "-Wno-unused-function",
            "-o",
            binary_path.to_str().unwrap(),
            model_path.to_str().unwrap(),
            driver_path.to_str().unwrap(),
            "-lm",
        ])
        .output()
        .map_err(|e| format!("cc invoke: {e}"))?;

    if !compile_output.status.success() {
        let stderr = String::from_utf8_lossy(&compile_output.stderr);
        let truncated: String = stderr.lines().take(20).collect::<Vec<_>>().join("\n");
        // Save debug files for failed compilations
        let debug_dir = std::path::Path::new("/tmp/fmi2_debug");
        let _ = fs::create_dir_all(debug_dir);
        let safe_name = model_name.replace('.', "_");
        let _ = fs::write(debug_dir.join(format!("{safe_name}.c")), &model_c);
        let _ = fs::write(debug_dir.join(format!("{safe_name}_driver.c")), &driver_c);
        let _ = fs::write(
            debug_dir.join(format!("{safe_name}_errors.txt")),
            &truncated,
        );
        return Err(format!("C compile failed:\n{truncated}"));
    }

    Ok(())
}

// =============================================================================
// Discovery
// =============================================================================

#[derive(Debug)]
enum Stage {
    CompileFail(String),
    NoStates,
    RenderFail(String),
    CCompileFail(String),
    Pass,
}

impl std::fmt::Display for Stage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stage::CompileFail(e) => {
                let short = if e.len() > 80 { &e[..80] } else { e };
                write!(f, "compile_fail: {short}")
            }
            Stage::NoStates => write!(f, "no_states"),
            Stage::RenderFail(e) => {
                let short = if e.len() > 80 { &e[..80] } else { e };
                write!(f, "render_fail: {short}")
            }
            Stage::CCompileFail(e) => {
                let short = if e.len() > 80 { &e[..80] } else { e };
                write!(f, "c_compile_fail: {short}")
            }
            Stage::Pass => write!(f, "pass"),
        }
    }
}

fn probe_model(
    library: &CompiledLibrary,
    model_name: &str,
    model_template: &str,
    driver_template: &str,
) -> Stage {
    // 1. Compile
    let report = library.compile_model_strict_reachable_with_recovery(model_name);
    let result: CompilationResult = match report.requested_result {
        Some(PhaseResult::Success(boxed)) => *boxed,
        Some(PhaseResult::Failed { error, .. }) => return Stage::CompileFail(error),
        _ => return Stage::CompileFail(report.failure_summary(0)),
    };

    let dae = &result.dae;

    // 2. Must have states for forward Euler integration
    if dae.states.is_empty() {
        return Stage::NoStates;
    }

    // 3. Render + compile C
    match try_fmi_c_compile(dae, model_name, model_template, driver_template) {
        Ok(()) => Stage::Pass,
        Err(e) => {
            if e.starts_with("render") {
                Stage::RenderFail(e)
            } else {
                Stage::CCompileFail(e)
            }
        }
    }
}

#[test]
#[ignore]
fn discover_fmu_targets() {
    #[cfg(debug_assertions)]
    {
        panic!(
            "\n\nERROR: must be run in RELEASE mode!\n\
             cargo test --release --package rumoca-test-msl --test fmu_target_discovery -- --ignored --nocapture\n"
        );
    }

    use rumoca_phase_codegen::templates;

    // 1. Download/cache MSL
    let msl_dir = ensure_msl_downloaded().expect("Failed to download MSL");

    // 2. Parse and build library
    let mo_files = find_mo_files(&msl_dir);
    println!("Parsing {} MSL files...", mo_files.len());
    let (successes, failures) = parse_files_parallel_lenient(&mo_files);
    println!("Parsed {} OK, {} failures", successes.len(), failures.len());
    let library = CompiledLibrary::from_parsed_batch_tolerant(successes)
        .expect("failed to build library index");

    // 3. Load targets
    let targets = load_target_models();
    println!("Probing {} models through FMI2 and FMI3 pipelines\n", targets.len());

    let mut fmi2_pass: Vec<String> = Vec::new();
    let mut fmi3_pass: Vec<String> = Vec::new();

    let mut fmi2_counts = [0usize; 5]; // compile_fail, no_states, render_fail, c_compile_fail, pass
    let mut fmi3_counts = [0usize; 5];

    for (i, model_name) in targets.iter().enumerate() {
        let fmi2_stage = probe_model(
            &library,
            model_name,
            templates::FMI2_MODEL,
            templates::FMI2_TEST_DRIVER,
        );
        let fmi3_stage = probe_model(
            &library,
            model_name,
            templates::FMI3_MODEL,
            templates::FMI3_TEST_DRIVER,
        );

        let idx = |s: &Stage| match s {
            Stage::CompileFail(_) => 0,
            Stage::NoStates => 1,
            Stage::RenderFail(_) => 2,
            Stage::CCompileFail(_) => 3,
            Stage::Pass => 4,
        };
        fmi2_counts[idx(&fmi2_stage)] += 1;
        fmi3_counts[idx(&fmi3_stage)] += 1;

        if matches!(fmi2_stage, Stage::Pass) {
            fmi2_pass.push(model_name.clone());
        }
        if matches!(fmi3_stage, Stage::Pass) {
            fmi3_pass.push(model_name.clone());
        }

        let f2_tag = if matches!(fmi2_stage, Stage::Pass) { "OK" } else { "  " };
        let f3_tag = if matches!(fmi3_stage, Stage::Pass) { "OK" } else { "  " };

        println!(
            "[{:>3}/{}] FMI2={f2_tag} FMI3={f3_tag}  {model_name}",
            i + 1,
            targets.len()
        );
        if !matches!(fmi2_stage, Stage::Pass) {
            println!("         FMI2: {fmi2_stage}");
        }
        if !matches!(fmi3_stage, Stage::Pass) && format!("{fmi3_stage}") != format!("{fmi2_stage}") {
            println!("         FMI3: {fmi3_stage}");
        }
    }

    // 4. Summary
    let labels = ["compile_fail", "no_states", "render_fail", "c_compile_fail", "pass"];
    println!("\n{:=<70}", "");
    println!("FMU Target Discovery Summary ({} models)", targets.len());
    println!("{:=<70}", "");
    println!("{:>20}  {:>6}  {:>6}", "stage", "FMI2", "FMI3");
    for (i, label) in labels.iter().enumerate() {
        println!("{:>20}  {:>6}  {:>6}", label, fmi2_counts[i], fmi3_counts[i]);
    }
    println!("{:=<70}\n", "");

    // 5. Write target list JSON files
    fmi2_pass.sort();
    fmi3_pass.sort();

    let out_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/msl_tests");
    let fmi2_path = out_dir.join("fmi2_msl_targets.json");
    let fmi3_path = out_dir.join("fmi3_msl_targets.json");

    let fmi2_json = serde_json::to_string_pretty(&fmi2_pass).unwrap();
    let fmi3_json = serde_json::to_string_pretty(&fmi3_pass).unwrap();

    fs::write(&fmi2_path, format!("{fmi2_json}\n")).expect("write fmi2 targets");
    fs::write(&fmi3_path, format!("{fmi3_json}\n")).expect("write fmi3 targets");

    println!("FMI2 targets: {} models → {}", fmi2_pass.len(), fmi2_path.display());
    println!("FMI3 targets: {} models → {}", fmi3_pass.len(), fmi3_path.display());
}
