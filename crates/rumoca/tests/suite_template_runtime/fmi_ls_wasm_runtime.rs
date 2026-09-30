//! Executable contract test for the pinned, non-normative FMI-LS-Wasm target.
//!
//! The target renders the shared FMI 3 C kernel and adapts the pinned WIT world
//! onto its ABI. These tests build the generated component for `wasm32-wasip2`,
//! validate it, and run it under Wasmtime, comparing its trace against the
//! native linked runtime (`rumoca_sim`).

use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use rumoca_sim::{SimOptions, SimResult, SimSolverMode, simulate_dae_with_diagnostics};
use sha1::{Digest, Sha1};
use tempfile::{TempDir, tempdir};
use walkdir::WalkDir;

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("rumoca crate is two levels below the workspace root")
        .to_path_buf()
}

fn msl_root() -> Option<PathBuf> {
    if let Some(raw) = std::env::var_os("MODELICAPATH")
        && let Some(first) = std::env::split_paths(&raw).next()
        && first.is_dir()
    {
        return Some(first);
    }
    let root = workspace_root().join("target/msl/ModelicaStandardLibrary-4.1.0");
    root.is_dir().then_some(root)
}

fn wasm_prerequisites(check: &str) -> bool {
    let wasm_tools = Command::new("wasm-tools").arg("--version").output().is_ok();
    let cc = std::env::var_os("CC_wasm32_wasip2").is_some();
    super::template_runtime_policy::prerequisites_are_available(
        check,
        &[("wasm-tools", wasm_tools), ("CC_wasm32_wasip2", cc)],
    )
}

fn checked_output(command: &mut Command, context: &str) -> Output {
    let output = command
        .output()
        .unwrap_or_else(|error| panic!("start {context}: {error}"));
    assert!(
        output.status.success(),
        "{context} failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    output
}

fn copy_tree(source: &Path, destination: &Path) {
    for entry in WalkDir::new(source) {
        let entry = entry.expect("walk FMI-LS host fixture");
        let relative = entry
            .path()
            .strip_prefix(source)
            .expect("fixture-relative path");
        let output = destination.join(relative);
        if entry.file_type().is_dir() {
            fs::create_dir_all(&output).expect("create copied fixture directory");
        } else {
            fs::copy(entry.path(), &output).expect("copy fixture file");
        }
    }
}

fn only_wasm(directory: &Path) -> PathBuf {
    let files = fs::read_dir(directory)
        .expect("read generated wasm release directory")
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| {
            path.extension()
                .is_some_and(|extension| extension == "wasm")
        })
        .collect::<Vec<_>>();
    assert_eq!(
        files.len(),
        1,
        "expected one generated component: {files:?}"
    );
    files[0].clone()
}

/// The dashed instantiation token the C kernel checks, read from the generated
/// kernel source (the identity is per-target, so it must come from this crate).
fn instantiation_token(crate_root: &Path) -> String {
    let model_c =
        fs::read_to_string(crate_root.join("csrc/model.c")).expect("read generated kernel source");
    model_c
        .split_once("strcmp(token,\"")
        .and_then(|(_, suffix)| suffix.split_once('"'))
        .map(|(token, _)| token.to_string())
        .expect("generated kernel embeds the checked instantiation token")
}

/// Map each FMI variable name to its FMI 3 value reference by rendering the
/// `fmi3` model description. The numbering is target-agnostic, so it addresses
/// the same quantities in the `fmi-ls-wasm` component.
fn value_reference_map(result: &rumoca::CompilationResult, model: &str) -> HashMap<String, u32> {
    let files = rumoca::render_target_files(result, model, "fmi3", None)
        .expect("fmi3 model description renders");
    let xml = files
        .iter()
        .find(|file| file.path == "modelDescription.xml")
        .expect("fmi3 emits a model description");
    let mut map = HashMap::new();
    for chunk in xml.content.split(" name=\"").skip(1) {
        let Some((name, rest)) = chunk.split_once('"') else {
            continue;
        };
        if let Some((_, after)) = rest.split_once("valueReference=\"")
            && let Some((reference, _)) = after.split_once('"')
            && let Ok(reference) = reference.parse::<u32>()
        {
            map.insert(name.to_string(), reference);
        }
    }
    map
}

fn build_component(work: &Path, crate_root: &Path) -> PathBuf {
    checked_output(
        Command::new("wasm-tools")
            .args(["component", "wit"])
            .arg(crate_root.join("wit")),
        "parse pinned FMI-LS WIT package",
    );
    let component_target = work.join("component-target");
    checked_output(
        Command::new("cargo")
            .args(["build", "--release", "--target", "wasm32-wasip2"])
            .arg("--manifest-path")
            .arg(crate_root.join("Cargo.toml"))
            .env("RUSTFLAGS", "-Dwarnings")
            .env("CARGO_TARGET_DIR", &component_target),
        "build generated wasm32-wasip2 component",
    );
    let component = only_wasm(&component_target.join("wasm32-wasip2/release"));
    checked_output(
        Command::new("wasm-tools").arg("validate").arg(&component),
        "validate generated WebAssembly component",
    );
    component
}

fn host_dir(work: &Path, crate_root: &Path) -> PathBuf {
    let host = work.join("host");
    copy_tree(
        &workspace_root().join("crates/rumoca/tests/fixtures/fmi-ls-wasm-host"),
        &host,
    );
    copy_tree(&crate_root.join("wit"), &host.join("wit"));
    host
}

fn run_host(host: &Path, work: &Path, args: &[&str]) -> String {
    let output = checked_output(
        Command::new("cargo")
            .args(["run", "--locked", "--manifest-path"])
            .arg(host.join("Cargo.toml"))
            .args(["--"])
            .args(args)
            .env("CARGO_TARGET_DIR", work.join("host-target")),
        "execute generated FMI-LS component through Wasmtime",
    );
    String::from_utf8(output.stdout).expect("host prints UTF-8")
}

/// Parse the host trace CSV into (time, per-column values) rows.
fn parse_trace(csv: &str) -> Vec<Vec<f64>> {
    csv.lines()
        .skip(1)
        .filter(|line| !line.is_empty())
        .map(|line| {
            line.split(',')
                .map(|value| value.parse::<f64>().expect("numeric trace cell"))
                .collect()
        })
        .collect()
}

/// Linear interpolation of a native series at `time`.
fn native_at(times: &[f64], values: &[f64], time: f64) -> f64 {
    match times.binary_search_by(|probe| probe.partial_cmp(&time).unwrap()) {
        Ok(index) => values[index],
        Err(0) => values[0],
        Err(index) if index >= times.len() => values[values.len() - 1],
        Err(index) => {
            let (t0, t1) = (times[index - 1], times[index]);
            let (v0, v1) = (values[index - 1], values[index]);
            v0 + (v1 - v0) * (time - t0) / (t1 - t0)
        }
    }
}

fn native_series<'a>(native: &'a SimResult, name: &str) -> &'a [f64] {
    let index = native
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| {
            panic!(
                "native trace has no series {name}; names: {:?}",
                native.names
            )
        });
    &native.data[index]
}

/// One component trace compared against the native run.
struct Track<'a> {
    model: &'a str,
    /// (component variable, native variable) pairs.
    channels: &'a [(&'a str, &'a str)],
    inputs: &'a [(&'a str, f64)],
    t_start: f64,
    t_end: f64,
    dt: f64,
    solver_mode: SimSolverMode,
    /// Largest magnitude of the compared channels.
    scale: f64,
    /// Fastest rate (1/s) of the compared dynamics.
    rate: f64,
}

/// The trace agreement the component can promise against the adaptive native
/// run. The component integrates each `dt` by its `CoSimulationStepPlan`,
/// error-controlled substeps whose local error stays within the plan's default
/// tolerance (the host sets none), so the global error grows at most with
/// `horizon * rate` local tolerances in units of `scale`; the native run is
/// adaptive to its `rtol`/`atol`. The sum, with a factor of ten for the
/// constants the bound leaves out, is the tolerance.
fn co_simulation_tolerance(track: &Track<'_>) -> f64 {
    const PLAN_TOLERANCE: f64 = 1.0e-6;
    let native = SimOptions::default();
    let horizon = track.t_end - track.t_start;
    let component = PLAN_TOLERANCE * (horizon * track.rate).max(1.0);
    10.0 * (track.scale * (component + native.rtol) + native.atol)
}

/// Build, validate, run the component over a do-step grid, and assert each
/// requested channel tracks the native series within the
/// [`co_simulation_tolerance`] of the track.
fn assert_tracks_native(
    result: &rumoca::CompilationResult,
    track: Track<'_>,
) -> (TempDir, Vec<Vec<f64>>) {
    let tolerance = co_simulation_tolerance(&track);
    let Track {
        model,
        channels,
        inputs,
        t_start,
        t_end,
        dt,
        solver_mode,
        ..
    } = track;
    let opts = SimOptions {
        t_start,
        t_end,
        dt: Some(dt),
        solver_mode,
        initial_inputs: inputs
            .iter()
            .map(|(name, value)| ((*name).to_string(), *value))
            .collect(),
        ..SimOptions::default()
    };
    let native = simulate_dae_with_diagnostics(&result.dae, &opts).expect("native simulation");

    let work = tempdir().expect("create FMI-LS-Wasm test directory");
    let generated = work.path().join("generated");
    rumoca::compile_packaged_target(result, model, "fmi-ls-wasm", generated.clone())
        .expect("render complete FMI-LS-Wasm component crate");
    let crate_root = generated.join(model);
    let component = build_component(work.path(), &crate_root);
    let token = instantiation_token(&crate_root);
    let references = value_reference_map(result, model);

    let host = host_dir(work.path(), &crate_root);
    let mut trace_args: Vec<String> = vec![
        "trace".into(),
        component.to_string_lossy().into_owned(),
        token,
        format!("{t_start}"),
        format!("{t_end}"),
        format!("{dt}"),
    ];
    for (variable, _) in channels {
        let reference = references
            .get(*variable)
            .unwrap_or_else(|| panic!("no value reference for {variable}"));
        trace_args.push(reference.to_string());
    }
    let arg_refs: Vec<&str> = trace_args.iter().map(String::as_str).collect();
    let csv = run_host(&host, work.path(), &arg_refs);
    let rows = parse_trace(&csv);
    assert!(rows.len() > 2, "trace is too short: {}", rows.len());

    for (column, (variable, native_name)) in channels.iter().enumerate() {
        let series = native_series(&native, native_name);
        let mut max_error = 0.0f64;
        for row in &rows {
            let time = row[0];
            let wasm_value = row[column + 1];
            let reference = native_at(&native.times, series, time);
            max_error = max_error.max((wasm_value - reference).abs());
        }
        assert!(
            max_error <= tolerance,
            "{model}: channel {variable} deviates from native {native_name} by {max_error} > {tolerance}"
        );
    }
    (work, rows)
}

const DECAY_MODEL: &str = "FmiLsDecay";
const DECAY_SOURCE: &str = r#"
model FmiLsDecay
  input Real u(start = 0.0);
  output Real x(start = 1.0);
equation
  der(x) = -x + u;
end FmiLsDecay;
"#;

#[test]
fn fmi_ls_wasm_component_validates_and_executes_pinned_lifecycle() {
    if !wasm_prerequisites("FMI-LS-Wasm lifecycle check") {
        return;
    }
    let result = rumoca::Compiler::new()
        .model(DECAY_MODEL)
        .compile_str(DECAY_SOURCE, "FmiLsDecay.mo")
        .expect("compile FMI-LS-Wasm fixture");

    // Trace parity against the native linked runtime (and, for this closed-form
    // model, the analytic decay exp(-t)).
    let (work, rows) = assert_tracks_native(
        &result,
        Track {
            model: DECAY_MODEL,
            channels: &[("x", "x")],
            inputs: &[("u", 0.0)],
            t_start: 0.0,
            t_end: 0.5,
            dt: 0.1,
            solver_mode: SimSolverMode::RkLike,
            scale: 1.0,
            rate: 1.0,
        },
    );
    for row in &rows {
        let analytic = (-row[0]).exp();
        assert!(
            (row[1] - analytic).abs() < 1.0e-5,
            "decay state {} deviates from exp(-t)={analytic} at t={}",
            row[1],
            row[0]
        );
    }

    // Lifecycle negative controls: rejected optional calls are transactional.
    let generated = work.path().join("generated");
    let crate_root = generated.join(DECAY_MODEL);
    let component = only_wasm(&work.path().join("component-target/wasm32-wasip2/release"));
    let token = instantiation_token(&crate_root);
    let host = work.path().join("host");
    let lifecycle = run_host(
        &host,
        work.path(),
        &["lifecycle", &component.to_string_lossy(), &token],
    );
    assert!(
        lifecycle.contains("OK"),
        "lifecycle negative controls failed: {lifecycle}"
    );
}

#[test]
fn fmi_ls_wasm_bouncing_ball_matches_native_state_event_trace() {
    if !wasm_prerequisites("FMI-LS-Wasm bouncing-ball check") {
        return;
    }
    let result = rumoca::Compiler::new()
        .model("BouncingBall")
        .compile_str(
            r#"
model BouncingBall
  parameter Real e = 0.8;
  parameter Real g = 9.81;
  Real h(start = 1.0, fixed = true);
  Real v(start = 0.0, fixed = true);
equation
  der(h) = v;
  der(v) = -g;
  when h < 0 then
    reinit(v, -e * pre(v));
  end when;
end BouncingBall;
"#,
            "BouncingBall.mo",
        )
        .expect("compile bouncing-ball fixture");

    let (_work, rows) = assert_tracks_native(
        &result,
        Track {
            model: "BouncingBall",
            channels: &[("h", "h"), ("v", "v")],
            inputs: &[],
            t_start: 0.0,
            t_end: 0.8,
            dt: 0.01,
            solver_mode: SimSolverMode::Bdf,
            scale: 5.0,
            rate: 1.0,
        },
    );
    for row in &rows {
        assert!(
            row[1] > -1.0e-2,
            "ball fell through the floor: h={}",
            row[1]
        );
    }
}

#[test]
fn fmi_ls_wasm_fourbar1_matches_native_multibody_trace() {
    if !wasm_prerequisites("FMI-LS-Wasm Fourbar1 check") {
        return;
    }
    let Some(msl) = msl_root() else {
        // Fails in the strict CI lane; skips only an ordinary local run.
        super::template_runtime_policy::prerequisites_are_available(
            "FMI-LS-Wasm Fourbar1 check",
            &[("MSL 4.1.0 checkout", false)],
        );
        return;
    };
    let result = rumoca::Compiler::new()
        .model("Fourbar1Wrap")
        .source_root(msl.to_string_lossy().as_ref())
        .compile_str(
            "model Fourbar1Wrap\n  extends Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1;\nend Fourbar1Wrap;\n",
            "Fourbar1Wrap.mo",
        )
        .expect("compile MSL Fourbar1");

    assert_tracks_native(
        &result,
        Track {
            model: "Fourbar1Wrap",
            channels: &[("j1_phi", "j1.phi"), ("j1_w", "j1.w")],
            inputs: &[],
            t_start: 0.0,
            t_end: 0.1,
            dt: 0.005,
            solver_mode: SimSolverMode::Bdf,
            scale: 20.0,
            rate: 10.0,
        },
    );
}

#[test]
fn fmi_ls_wasm_vendored_contract_matches_pinned_upstream_bytes() {
    let root = workspace_root().join("crates/rumoca-phase-codegen/src/templates/fmi-ls-wasm");
    let expected = [
        (
            "wit/fmi3-callbacks.wit",
            "3c245a828c438a9ba3629c1fd163f776898cfe2e",
        ),
        (
            "wit/fmi3-co-simulation.wit",
            "41f9753c7614a25d7197c8a78a3724beefbd5250",
        ),
        (
            "wit/fmi3-common.wit",
            "7dbe9aaa3788303237c8b08ab10b0e277df68cb7",
        ),
        (
            "wit/fmi3-model-exchange.wit",
            "0372cacf36db9a717ef094488f233747001fe18a",
        ),
        (
            "wit/fmi3-scheduled-execution.wit",
            "ecb813ced9ec36f6beb55330fc83d19a8d4f34b7",
        ),
        (
            "wit/fmi3-types.wit",
            "3ef57aba19886e110253f103a1ca5e6a0cb0684c",
        ),
        ("wit/world.wit", "4c4f31bdd797bd2e6703b08ba4e7bd56c89be5d7"),
        (
            "upstream/LICENSE.txt",
            "2f6d404a9e3b153b04498beb18c6da1c833e3bbd",
        ),
        (
            "fmi3-headers/fmi3Functions.h",
            "89588ef290ba97ab5fe7fb6c1a4db9ac045de9ad",
        ),
        (
            "fmi3-headers/fmi3FunctionTypes.h",
            "574bbfdcacca30efb85b6f0c78087ee833da9f69",
        ),
        (
            "fmi3-headers/fmi3PlatformTypes.h",
            "575d48fcd85b74dab0936b2e20ea3ed8e66de090",
        ),
    ];
    for (path, digest) in expected {
        let bytes = fs::read(root.join(path)).expect("read pinned FMI-LS contract file");
        assert_eq!(
            format!("{:x}", Sha1::digest(bytes)),
            digest,
            "changed {path}"
        );
    }
}

/// The checks above skip only in an ordinary local run. The CI wasm lane must
/// provision every prerequisite and run strict, so a missing tool or MSL
/// checkout fails there instead of passing silently.
#[test]
fn fmi_ls_wasm_ci_lane_provisions_its_prerequisites_and_runs_strict() {
    let root = workspace_root();
    let ci = fs::read_to_string(root.join(".github/workflows/ci.yml")).expect("read ci.yml");
    let flake = fs::read_to_string(root.join("flake.nix")).expect("read flake.nix");
    assert!(
        ci.contains("- backend: wasm\n            nix_shell: ci-template-wasm"),
        "the wasm template-runtime lane must run in the ci-template-wasm shell"
    );
    assert!(
        ci.contains("--backend \"$TEMPLATE_BACKEND\" \\\n            --require-external-tools"),
        "template-runtime lanes must run with --require-external-tools"
    );
    assert!(
        ci.contains(
            "- name: Ensure MSL (fmi-ls-wasm Fourbar1 check)\n        if: matrix.backend == 'wasm'"
        ),
        "the wasm lane must provide the MSL checkout the Fourbar1 check reads"
    );
    let shell = flake
        .split("devShells.ci-template-wasm =")
        .nth(1)
        .and_then(|rest| rest.split("devShells.").next())
        .expect("flake.nix defines devShells.ci-template-wasm");
    assert!(
        shell.contains("pkgs.wasm-tools"),
        "ci-template-wasm must provide wasm-tools"
    );
    assert!(
        shell.contains("export CC_wasm32_wasip2="),
        "ci-template-wasm must export CC_wasm32_wasip2"
    );
}
