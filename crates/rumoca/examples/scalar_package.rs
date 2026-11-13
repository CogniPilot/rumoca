//! Stateful (scalar-product) FMU construction and publication evidence; no
//! engine loads the published package. The guest is INTERPRETED until linked
//! dispatch lands: no compiled entry runs inside it.
use rumoca::native_fmi_ls::ScalarFmiLsPackage;
use rumoca_sim::PreparedScalarFmiLsModel;
use std::{env, error::Error, fs, path::Path, time::Instant};

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

fn main() -> Result<(), Box<dyn Error>> {
    let arguments: Vec<_> = env::args().collect();
    let [_, source_path, model, work, rest @ ..] = arguments.as_slice() else {
        return Err("usage: scalar_package SOURCE MODEL NEW_WORK_DIR [BUILD_SCRATCH]".into());
    };
    // Staged source stays under the work dir; an optional existing scratch dir
    // takes only build data (the fixed guest's cargo target and its cache).
    let scratch = match rest {
        [] => None,
        [scratch] => Some(Path::new(scratch)),
        _ => return Err("scalar_package takes at most one BUILD_SCRATCH".into()),
    };
    let work = Path::new(work);
    fs::create_dir(work)?;
    let source = fs::read_to_string(source_path)?;
    let (compilation, frontend_s) = stage("frontend", || {
        rumoca::Compiler::new()
            .model(model)
            .compile_str(&source, source_path)
            .map_err(|error| error.to_string().into())
    })?;
    let (prepared, prepare_s) = stage("prepared_model", || {
        let component = rumoca_sim::lower_fmi_export_component(&compilation.dae)?;
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
            "seconds": {"frontend": frontend_s, "prepared_model": prepare_s, "fixed_guest_core": core_s,
                        "construct": construct_s, "publish": publish_s},
            "peak_rss_kib": fs::read_to_string("/proc/self/status")?.lines()
                .find_map(|l| l.strip_prefix("VmHWM:")).map(str::trim).unwrap_or(""),
        })
    );
    Ok(())
}
