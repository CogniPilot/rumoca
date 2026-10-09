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
    let xml = Command::new("xmllint").arg("--version").output().is_ok();
    super::template_runtime_policy::prerequisites_are_available(
        check,
        &[
            ("wasm-tools", wasm_tools),
            ("CC_wasm32_wasip2", cc),
            ("xmllint", xml),
        ],
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

/// Metadata comes from the same prepared session and extracted final archive.
fn instantiation_token(package_root: &Path) -> String {
    let xml = fs::read_to_string(package_root.join("modelDescription.xml")).unwrap();
    attribute(&xml, "instantiationToken")
}

fn attribute(xml: &str, name: &str) -> String {
    xml.split_once(&format!("{name}=\""))
        .and_then(|(_, rest)| rest.split_once('"'))
        .map(|(value, _)| value.to_owned())
        .expect("required XML attribute")
}

fn value_reference_map(package_root: &Path) -> HashMap<String, u32> {
    let xml = fs::read_to_string(package_root.join("modelDescription.xml")).unwrap();
    let mut map = HashMap::new();
    for chunk in xml.split(" name=\"").skip(1) {
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

fn prepare_build_publish(
    work: &Path,
    result: &rumoca::CompilationResult,
    model: &str,
) -> (PathBuf, PathBuf, PathBuf) {
    let prepared = rumoca::prepare_packaged_target(result, model, "fmi-ls-wasm")
        .expect("prepare checked component metadata and build inputs");
    let crate_root = work.join("generated").join(model);
    prepared
        .write_build_inputs(&crate_root)
        .expect("write build inputs without an FMU archive");
    let xml = fs::read_to_string(crate_root.join("modelDescription.xml")).unwrap();
    let identifier = attribute(&xml, "modelIdentifier");
    assert_eq!(
        attribute(&xml, "instantiationToken"),
        prepared.artifact().identities["fmu"]
    );
    fs::write(
        crate_root.join("src/lib.rs"),
        b"compile_error!(\"foreign mutable staging source\");",
    )
    .unwrap();
    let built = rumoca_exec_wasm::build_wasm_component(
        &prepared.build_request("component").unwrap(),
        work,
        &work.join("component-target"),
    )
    .expect("warning-clean exact-inventory component build and validation");
    let output = work.join("artifact");
    let foreign = rumoca::prepare_packaged_target(result, model, "fmi-ls-wasm").unwrap();
    let error = rumoca::publish_wasm_component(foreign, built, &output).unwrap_err();
    assert!(error.to_string().contains("foreign prepared inventory"));
    assert!(
        !output.exists(),
        "foreign result must not publish any product"
    );
    let built = rumoca_exec_wasm::build_wasm_component(
        &prepared.build_request("component").unwrap(),
        work,
        &work.join("component-target"),
    )
    .expect("rebuild the original immutable inventory after foreign publication refusal");
    rumoca::publish_wasm_component(prepared, built, &output)
        .expect("publish the exact sealed prepared build result");
    let archive = output.join(format!("{model}.fmu"));
    let package_root = work.join("extracted");
    zip::ZipArchive::new(fs::File::open(&archive).unwrap())
        .unwrap()
        .extract(&package_root)
        .expect("extract the completed FMU archive");
    assert_eq!(
        fs::read(package_root.join("modelDescription.xml")).unwrap(),
        xml.as_bytes()
    );
    validate_deployment_metadata(&package_root);
    let component = package_root
        .join("binaries/wasm32-wasip2")
        .join(format!("{identifier}.wasm"));
    checked_output(
        Command::new("wasm-tools").arg("validate").arg(&component),
        "validate extracted component",
    );
    (crate_root, package_root, component)
}

fn validate_deployment_metadata(root: &Path) {
    let schemas = workspace_root().join("crates/rumoca/tests/fixtures/fmi-ls-wasm-schema");
    for (xml, schema) in [
        ("modelDescription.xml", "fmi3ModelDescription.xsd"),
        (
            "extra/org.modelica.fmi-ls-wasm/manifest.xml",
            "fmi3LayeredStandardManifest.xsd",
        ),
    ] {
        checked_output(
            Command::new("xmllint")
                .arg("--noout")
                .arg("--schema")
                .arg(schemas.join(schema))
                .arg(root.join(xml)),
            "validate official FMI deployment schema",
        );
    }
    let xml = fs::read_to_string(root.join("modelDescription.xml")).unwrap();
    assert!(xml.contains("<CoSimulation "));
    for absent in [
        "<ModelExchange",
        "<ScheduledExecution",
        "canGetAndSetFMUState=\"true\"",
        "canSerializeFMUState=\"true\"",
        "providesDirectionalDerivatives=\"true\"",
        "hasEventMode=\"true\"",
    ] {
        assert!(
            !xml.contains(absent),
            "unsupported deployment metadata: {absent}"
        );
    }
    assert!(
        !root.join("Cargo.toml").exists(),
        "build inputs are outside the compiled FMU"
    );
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
    let target = std::env::var_os("CARGO_TARGET_DIR")
        .map(|root| PathBuf::from(root).join("rumoca-fmi-ls-host"))
        .unwrap_or_else(|| work.join("host-target"));
    let output = checked_output(
        Command::new("cargo")
            .args(["run", "--locked", "--manifest-path"])
            .arg(host.join("Cargo.toml"))
            .args(["--"])
            .args(args)
            .env("CARGO_TARGET_DIR", target),
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
    let (crate_root, package_root, component) = prepare_build_publish(work.path(), result, model);
    let token = instantiation_token(&package_root);
    let references = value_reference_map(&package_root);

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

fn run_native_assertion_order(
    work: &Path,
    crate_root: &Path,
    token: &str,
    references: &HashMap<String, u32>,
    expected_message: &str,
) {
    let binary = work.join("native-assertion-order");
    let mut sources = fs::read_dir(crate_root.join("csrc"))
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.extension().is_some_and(|extension| extension == "c"))
        .collect::<Vec<_>>();
    sources.sort();
    checked_output(
        Command::new("cc")
            .args([
                "-std=c11",
                "-O2",
                "-ffp-contract=off",
                "-Wall",
                "-Wextra",
                "-Werror",
                "-Wvla",
            ])
            .arg("-I")
            .arg(crate_root.join("csrc"))
            .args(sources)
            .arg(workspace_root().join("crates/rumoca/tests/fixtures/fmi-assertion-order-host.c"))
            .arg("-lm")
            .arg("-o")
            .arg(&binary),
        "build original source FMI C control",
    );
    let output = checked_output(
        Command::new(binary)
            .arg(token)
            .args([
                references["valid"].to_string(),
                references["index"].to_string(),
                references["y"].to_string(),
            ])
            .arg(expected_message),
        "execute original source FMI C control",
    );
    assert!(
        String::from_utf8(output.stdout)
            .unwrap()
            .contains("OK native first authored fault")
    );
}

#[test]
fn fmi_ls_wasm_preserves_authored_first_fault_before_later_bounds_failure() {
    if !wasm_prerequisites("FMI-LS-Wasm source first-fault check") {
        return;
    }
    run_assertion_order_component(
        include_str!("../fixtures/AssertionFaultOrder.mo"),
        "AssertionFaultOrder",
        "first authored assertion",
    );
}

#[test]
fn fmi_ls_wasm_preserves_nested_loop_first_fault_and_capture() {
    if !wasm_prerequisites("FMI-LS-Wasm nested loop first-fault check") {
        return;
    }
    run_assertion_order_component(
        include_str!("../fixtures/NestedLoopAssertionFault.mo"),
        "NestedLoopAssertionFault",
        "nested first 1",
    );
}

fn run_assertion_order_component(source: &str, model: &str, expected_message: &str) {
    let result = rumoca::Compiler::new()
        .model(model)
        .compile_str(source, model)
        .expect("compile unchanged authored counterexample");
    let work = tempdir().expect("first-fault component work");
    let (crate_root, package_root, component) = prepare_build_publish(work.path(), &result, model);
    let host = host_dir(work.path(), &crate_root);
    let references = value_reference_map(&package_root);
    run_native_assertion_order(
        work.path(),
        &crate_root,
        &instantiation_token(&package_root),
        &references,
        expected_message,
    );
    let output = run_host(
        &host,
        work.path(),
        &[
            "assertion-order",
            &component.to_string_lossy(),
            &instantiation_token(&package_root),
            &references["valid"].to_string(),
            &references["index"].to_string(),
            &references["y"].to_string(),
            expected_message,
        ],
    );
    assert!(output.contains("OK first authored fault"), "{output}");
}

#[test]
fn fmi_ls_wasm_standard_logger_retains_instance_lifetime() {
    if !wasm_prerequisites("FMI-LS-Wasm standard logger check") {
        return;
    }
    let model = "FmiLsLogging";
    let result = rumoca::Compiler::new()
        .model(model)
        .compile_str(
            r#"
model FmiLsLogging
  function check
    input Boolean valid;
    output Real y;
  algorithm
    assert(valid, "authored callback diagnostic");
    y := 1;
  end check;
  input Boolean valid(start=true) = true;
  Real y;
equation
  y = check(valid);
end FmiLsLogging;
"#,
            "FmiLsLogging.mo",
        )
        .expect("compile callback fixture");
    let work = tempdir().expect("callback component work");
    let (crate_root, package_root, component) = prepare_build_publish(work.path(), &result, model);
    let host = host_dir(work.path(), &crate_root);
    let refs = value_reference_map(&package_root);
    let output = run_host(
        &host,
        work.path(),
        &[
            "logging",
            &component.to_string_lossy(),
            &instantiation_token(&package_root),
            &refs["valid"].to_string(),
        ],
    );
    assert!(output.contains("OK authored diagnostic"), "{output}");
}

#[test]
fn fmi_ls_wasm_typed_arrays_use_shared_fmi3_accessors() {
    if !wasm_prerequisites("FMI-LS-Wasm typed-array check") {
        return;
    }
    let model = "FmiLsTypedArrays";
    let result = rumoca::Compiler::new()
        .model(model)
        .compile_str(
            r#"
model FmiLsTypedArrays
  function twice
    input Integer u;
    output Integer y;
  algorithm
    y := 2*u;
  end twice;
  type Mode = enumeration(Off, On, Standby);
  input Integer integers[2](start={1, 2}) = {1, 2};
  input Boolean booleans[2](start={true, false}) = {true, false};
  input Mode mode = Mode.Off;
  Real x(start=0, fixed=true);
equation
  der(x) = (if booleans[1] and mode == Mode.On then twice(integers[1]) else twice(integers[2]))/2;
end FmiLsTypedArrays;
"#,
            "FmiLsTypedArrays.mo",
        )
        .expect("compile typed-array fixture");
    let work = tempdir().expect("typed-array component work");
    let (crate_root, package_root, component) = prepare_build_publish(work.path(), &result, model);
    let host = host_dir(work.path(), &crate_root);
    let refs = value_reference_map(&package_root);
    let metadata = rumoca::render_target_files(&result, model, "fmi3", None)
        .expect("render checked FMI metadata");
    let xml = &metadata
        .iter()
        .find(|file| file.path == "modelDescription.xml")
        .expect("FMI metadata file")
        .content;
    for (kind, name) in [
        ("Int32", "integers"),
        ("Boolean", "booleans"),
        ("Enumeration", "mode"),
        ("Float64", "x"),
    ] {
        assert!(
            xml.contains(&format!("<{kind} name=\"{name}\"")),
            "wrong FMI type for {name}"
        );
    }
    let args = ["integers", "booleans", "mode", "x"].map(|name| refs[name].to_string());
    let output = run_host(
        &host,
        work.path(),
        &[
            "typed",
            &component.to_string_lossy(),
            &instantiation_token(&package_root),
            &args[0],
            &args[1],
            &args[2],
            &args[3],
        ],
    );
    assert!(output.contains("OK typed arrays"), "{output}");
}

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
    let package_root = work.path().join("extracted");
    let component = only_wasm(&package_root.join("binaries/wasm32-wasip2"));
    let token = instantiation_token(&package_root);
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

/// SPEC_0007: the adapter forwards do-step to the shared checked FMI kernel.
/// MLS 3.6 section 3.7.3 sample events stay inside that kernel, including more
/// than one tick per communication step; no public FMI Clock API is needed.
#[test]
fn fmi_ls_wasm_periodic_samples_match_native_across_communication_steps() {
    if !wasm_prerequisites("FMI-LS-Wasm periodic sample check") {
        return;
    }
    let model = "SampledOutput";
    let result = rumoca::Compiler::new()
        .model(model)
        .compile_str(
            r#"
model SampledOutput
  Real x(start = 0, fixed = true);
  output Real held(start = -1, fixed = true);
  output Real count(start = 0, fixed = true);
equation
  der(x) = 1;
  when sample(0.05, 0.1) then
    held = x;
    count = pre(count) + 1;
  end when;
end SampledOutput;
"#,
            "SampledOutput.mo",
        )
        .expect("compile sampled output fixture");
    let (_work, rows) = assert_tracks_native(
        &result,
        Track {
            model,
            channels: &[("x", "x"), ("held", "held"), ("count", "count")],
            inputs: &[],
            t_start: 0.0,
            t_end: 1.0,
            dt: 0.2,
            solver_mode: SimSolverMode::RkLike,
            scale: 10.0,
            rate: 1.0,
        },
    );
    for (index, row) in rows.iter().enumerate() {
        assert_eq!(
            row[3],
            (2 * index) as f64,
            "each step must execute two ticks"
        );
        if index > 0 {
            assert!((row[2] - (row[0] - 0.05)).abs() < 1.0e-8);
        }
    }
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

fn fourbar1() -> Option<rumoca::CompilationResult> {
    let Some(msl) = msl_root() else {
        // Fails in the strict CI lane; skips only an ordinary local run.
        super::template_runtime_policy::prerequisites_are_available(
            "Fourbar1 MSL 4.1.0 check",
            &[("MSL 4.1.0 checkout", false)],
        );
        return None;
    };
    Some(rumoca::Compiler::new()
        .model("Fourbar1Wrap")
        .source_root(msl.to_string_lossy().as_ref())
        .compile_str(
            "model Fourbar1Wrap\n  extends Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1;\nend Fourbar1Wrap;\n",
            "Fourbar1Wrap.mo",
        )
        .expect("compile MSL Fourbar1"))
}

#[test]
fn fourbar1_native_multibody_trace_matches_pinned_omc() {
    let Some(result) = fourbar1() else { return };
    let options = SimOptions {
        t_start: 0.0,
        t_end: 0.1,
        dt: Some(0.005),
        solver_mode: SimSolverMode::Bdf,
        ..SimOptions::default()
    };
    let native = simulate_dae_with_diagnostics(&result.dae, &options)
        .expect("unchanged native Fourbar BDF trajectory");
    assert_eq!(native.times.len(), 21);
    assert_eq!(native.times.first(), Some(&0.0));
    assert_eq!(native.times.last(), Some(&0.1));
    let reference = parse_trace(include_str!(
        "../fixtures/fmi-ls-wasm-fourbar/omc-reference.csv"
    ));
    // The raw OMC result retains its identical duplicate final sample.
    assert_eq!(reference.len(), 22);
    println!(
        "Fourbar native rtol={} atol={} points=21 final=0.1; OMC points=22 duplicate-final=1 tolerance=1e-8",
        options.rtol, options.atol
    );
    for (column, name) in [(1, "j1.phi"), (2, "j1.w")] {
        let series = native_series(&native, name);
        assert_eq!(series.len(), native.times.len());
        let (mut max_absolute, mut max_scaled, mut max_tolerance_ratio) = (0.0f64, 0.0f64, 0.0f64);
        for row in &reference {
            let actual = native_at(&native.times, series, row[0]);
            let expected = row[column];
            let tolerance = 10.0 * ((options.rtol + 1e-8) * expected.abs().max(1.0) + options.atol);
            let error = (actual - expected).abs();
            max_absolute = max_absolute.max(error);
            max_scaled = max_scaled.max(error / expected.abs().max(1.0));
            max_tolerance_ratio = max_tolerance_ratio.max(error / tolerance);
            assert!(actual.is_finite() && expected.is_finite());
            assert!(
                (actual - expected).abs() <= tolerance,
                "Fourbar {name} at {}: native {actual} versus OMC {expected}, tolerance {tolerance}",
                row[0]
            );
        }
        println!(
            "Fourbar {name}: max_absolute={max_absolute:e} max_scaled={max_scaled:e} max_tolerance_ratio={max_tolerance_ratio:e}"
        );
    }
    for (index, time) in native.times.iter().enumerate() {
        println!(
            "FOURBAR_NATIVE,{time:.17},{:.17},{:.17}",
            native_series(&native, "j1.phi")[index],
            native_series(&native, "j1.w")[index]
        );
    }
}

#[test]
fn fmi_ls_wasm_fourbar1_refuses_unsupported_published_strings() {
    let Some(result) = fourbar1() else { return };
    let component = rumoca_sim::lower_fmi_component(&result.dae)
        .expect("canonical complete public Fourbar inventory");
    let strings = component.variables().iter().filter(|variable| {
        variable.value_kind() == rumoca_ir_solve::SolveVariableValueKind::String
    });
    let actual = strings
        .map(|variable| {
            assert_eq!(
                variable.variability(),
                rumoca_ir_solve::fmi::FmiVariability::Tunable
            );
            let count = variable
                .storage()
                .expect("String storage owner")
                .scalar_count();
            assert_eq!(
                variable.text_start().expect("declared String values").len(),
                count
            );
            (variable.name().to_string(), count)
        })
        .collect::<std::collections::BTreeMap<_, _>>();
    let expected = include_str!("../fixtures/fmi-ls-wasm-fourbar/published-strings.csv")
        .lines()
        .skip(1)
        .map(|line| {
            let (name, count) = line.split_once(',').expect("expected name and extent");
            (
                name.to_string(),
                count.parse::<usize>().expect("expected scalar extent"),
            )
        })
        .collect::<std::collections::BTreeMap<_, _>>();
    assert_eq!(actual.len(), 27);
    assert_eq!(actual, expected);
    let preparation = rumoca::prepare_packaged_target(&result, "Fourbar1Wrap", "fmi-ls-wasm")
        .err()
        .expect("the complete public inventory requires unsupported String access");
    assert!(format!("{preparation:#}").contains("unsupported-feature:fmi-variable-access-string"));
    let work = tempdir().expect("owned refusal product parent");
    let output = work.path().join("product");
    let export =
        rumoca::compile_packaged_target(&result, "Fourbar1Wrap", "fmi-ls-wasm", output.clone())
            .expect_err("public export must refuse before creating a product");
    assert!(format!("{export:#}").contains("unsupported-feature:fmi-variable-access-string"));
    assert!(!output.exists());
    assert_eq!(fs::read_dir(work.path()).unwrap().count(), 0);
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
