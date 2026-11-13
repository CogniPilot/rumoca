//! Stateful (scalar-product) FMU construction and publication evidence; no
//! engine loads the published package. The guest is INTERPRETED until linked
//! dispatch lands: no compiled entry runs inside it.
//!
//! The model is compiled from its main document plus the listed source roots
//! (each a file or a whole package directory) and nothing else: no library is
//! discovered from the environment. The host's options document is read by
//! the same owner the browser session uses. Its parameter overrides and
//! initial inputs are applied when the model is lowered; its solver, step and
//! tolerances become the component's Co-Simulation step plan, and a choice no
//! plan or guest carries is refused, never substituted.
use rumoca::native_fmi_ls::ScalarFmiLsPackage;
use rumoca_ir_solve::fmi::FmiComponent;
use rumoca_sim::interactive_options::InteractiveOptions;
use rumoca_sim::{PreparedScalarFmiLsModel, SimExecutionPolicy, SimOptions, SimSolverMode};
use std::{collections::BTreeMap, env, error::Error, fs, path::Path, time::Instant};

fn stage<T>(
    name: &str,
    action: impl FnOnce() -> Result<T, Box<dyn Error>>,
) -> Result<(T, f64), Box<dyn Error>> {
    let started = Instant::now();
    eprintln!("SCALAR_PACKAGE stage={name} event=begin");
    let result = action();
    let seconds = started.elapsed().as_secs_f64();
    match &result {
        Ok(_) => eprintln!("SCALAR_PACKAGE stage={name} event=end seconds={seconds}"),
        Err(error) => {
            eprintln!("SCALAR_PACKAGE stage={name} event=refused seconds={seconds} error={error:?}")
        }
    }
    result.map(|value| (value, seconds))
}

/// Every source root is an existing absolute path, listed once.
fn read_source_roots(path: &str) -> Result<Vec<String>, Box<dyn Error>> {
    let roots: Vec<String> = serde_json::from_str(&fs::read_to_string(path)?)?;
    let mut seen = std::collections::BTreeSet::new();
    for root in &roots {
        let root_path = Path::new(root);
        if !root_path.is_absolute() || !root_path.exists() {
            return Err(format!("source root `{root}` is not an existing absolute path").into());
        }
        if !seen.insert(fs::canonicalize(root_path)?) {
            return Err(format!("source root `{root}` is listed twice").into());
        }
    }
    Ok(roots)
}

/// The simulation options the options document asks for. The published
/// component has no experiment window, so only the fields lowering and the
/// runtime-choice admission read are set.
fn requested_options(options: &InteractiveOptions) -> SimOptions {
    let base = SimOptions {
        solver_mode: SimSolverMode::parse_request(Some(&options.solver)).0,
        dt: (options.dt > 0.0).then_some(options.dt),
        ..SimOptions::default()
    };
    options.configure(base, options.initial_inputs.clone())
}

/// Bind the runtime choices of `opts` to the component, or refuse naming the
/// first one it would otherwise substitute. The solver, its configuration and
/// both tolerances become the component's Co-Simulation step plan, through
/// the backend that owns them. The guest is INTERPRETED, so a policy that
/// requires compiled code is refused.
fn bind_runtime_choices(
    component: FmiComponent,
    options: &InteractiveOptions,
    opts: &SimOptions,
) -> Result<FmiComponent, Box<dyn Error>> {
    if !(options.atol > 0.0 && options.rtol > 0.0) {
        return Err("the options must state a positive atol and rtol; none is defaulted".into());
    }
    if opts.execution_policy == SimExecutionPolicy::NativeRequired {
        return Err(
            "execution policy `native_required` is refused: this guest is INTERPRETED".into(),
        );
    }
    let plan = rumoca_sim::co_simulation_plan(opts)?;
    eprintln!("SCALAR_PACKAGE co_simulation={}", serde_json::json!(plan));
    Ok(component.with_co_simulation(plan))
}

/// Published variables counted by FMI type and causality.
fn census(component: &FmiComponent) -> BTreeMap<String, usize> {
    let mut counts = BTreeMap::new();
    for variable in component.variables() {
        let key = format!("{}/{:?}", variable.fmi3_type(), variable.causality());
        *counts.entry(key).or_insert(0) += variable.scalar_count();
    }
    counts
}

fn main() -> Result<(), Box<dyn Error>> {
    let arguments: Vec<_> = env::args().collect();
    let [_, source_path, model, roots_path, options_path, work, rest @ ..] = arguments.as_slice()
    else {
        return Err("usage: scalar_package SOURCE MODEL SOURCE_ROOTS_JSON OPTIONS_JSON \
                    NEW_WORK_DIR [BUILD_SCRATCH]"
            .into());
    };
    // Staged source stays under the work dir; an optional existing scratch dir
    // takes only build data (the fixed guest's cargo target and its cache,
    // and the compiler's parse cache).
    let scratch = match rest {
        [] => None,
        [scratch] => Some(Path::new(scratch)),
        _ => return Err("scalar_package takes at most one BUILD_SCRATCH".into()),
    };
    let work = Path::new(work);
    fs::create_dir(work)?;
    rumoca_compile::source_roots::set_cache_root_override(
        scratch.unwrap_or(work).join("rumoca-cache"),
    );
    let source = fs::read_to_string(source_path)?;
    let roots = read_source_roots(roots_path)?;
    let options = InteractiveOptions::parse(&fs::read_to_string(options_path)?)?;
    let opts = requested_options(&options);
    let (compilation, frontend_s) = stage("frontend", || {
        rumoca::Compiler::new()
            .model(model)
            .source_roots(&roots)
            .compile_str(&source, source_path)
            .map_err(|error| error.to_string().into())
    })?;
    let (component, lower_s) = stage("lower", || {
        Ok(rumoca_sim::lower_fmi_scalar_export_component(&compilation.dae, &opts)?)
    })?;
    let variables = census(&component);
    eprintln!("SCALAR_PACKAGE census={}", serde_json::json!(variables));
    let (component, _) = stage("runtime_choices", || {
        bind_runtime_choices(component, &options, &opts)
    })?;
    let (prepared, prepare_s) = stage("prepared_model", || {
        Ok(PreparedScalarFmiLsModel::from_component(&component)?)
    })?;
    let (core, core_s) = stage("fixed_guest_core", || {
        let staging = work.join("fixed-source");
        let target = scratch.unwrap_or(work).join("fixed-target");
        fs::create_dir(&staging)?;
        fs::create_dir(&target)?;
        let cache = match scratch {
            Some(scratch) => scratch.join("fixed-guest-cache"),
            None => work.parent().unwrap_or(work).join("fixed-guest-cache"),
        };
        Ok(rumoca_exec_wasm::build_fixed_scalar_guest_core_cached(
            &staging, &target, &cache,
        )?)
    })?;
    let (package, construct_s) = stage("construct", || {
        Ok(ScalarFmiLsPackage::construct(prepared, core, model)?)
    })?;
    let description_bytes = package.model_description().len();
    let component_bytes = package.component_bytes().len();
    let output = work.join("published");
    fs::create_dir(&output)?;
    let (_, publish_s) = stage("publish", || Ok(package.publish(&output)?))?;
    println!(
        "{}",
        serde_json::json!({
            "scope": "scalar-product FMU construction and publication; INTERPRETED guest; not loaded",
            "model": model, "model_description_bytes": description_bytes,
            "component_bytes": component_bytes,
            "source_roots": roots.len(), "variables": variables,
            "seconds": {"frontend": frontend_s, "lower": lower_s, "prepared_model": prepare_s,
                        "fixed_guest_core": core_s, "construct": construct_s,
                        "publish": publish_s},
            "peak_rss_kib": fs::read_to_string("/proc/self/status")?.lines()
                .find_map(|l| l.strip_prefix("VmHWM:")).map(str::trim).unwrap_or(""),
        })
    );
    Ok(())
}
