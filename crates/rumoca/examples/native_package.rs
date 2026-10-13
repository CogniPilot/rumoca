//! Original-source direct-native FMU construction and publication evidence;
//! no engine loads the published package.
use rumoca::native_fmi_ls::NativeFmiLsPackage;
use rumoca_sim::PreparedNativeFmiLsModel;
use std::{env, error::Error, fs, path::Path, time::Instant};

fn stage<T>(
    name: &str,
    action: impl FnOnce() -> Result<T, Box<dyn Error>>,
) -> Result<(T, f64), Box<dyn Error>> {
    let started = Instant::now();
    eprintln!("NATIVE_PACKAGE stage={name} event=begin");
    let result = action();
    let seconds = started.elapsed().as_secs_f64();
    match &result {
        Ok(_) => eprintln!("NATIVE_PACKAGE stage={name} event=end seconds={seconds}"),
        Err(error) => {
            eprintln!("NATIVE_PACKAGE stage={name} event=refused seconds={seconds} error={error:?}")
        }
    }
    result.map(|value| (value, seconds))
}

fn main() -> Result<(), Box<dyn Error>> {
    use rumoca_exec_wasm::{CheckedNativeWasmCapacityPolicy, NativeWasmCapacityRequest};
    let arguments: Vec<_> = env::args().collect();
    let [_, source_path, model, work, private, public] = arguments.as_slice() else {
        return Err(
            "usage: native_package SOURCE MODEL NEW_WORK_DIR PRIVATE_BYTES PUBLIC_BYTES".into(),
        );
    };
    let work = Path::new(work);
    fs::create_dir(work)?;
    let policy = CheckedNativeWasmCapacityPolicy::new(NativeWasmCapacityRequest {
        private_arena_bytes: private.parse()?,
        public_scratch_bytes: public.parse()?,
    })?;
    let source = fs::read_to_string(source_path)?;
    let (compilation, _) = stage("frontend", || {
        rumoca::Compiler::new()
            .model(model)
            .compile_str(&source, source_path)
            .map_err(|error| error.to_string().into())
    })?;
    let (library, emission_s) = stage("emission", || {
        let component = rumoca_sim::lower_fmi_export_component(&compilation.dae)?;
        let root = component.runtime_view().retain_assertion_root();
        let code = rumoca_exec_wasm::compile_retained_native_preparation_wasm_with_policy(
            root.native_preparation()?,
            policy,
        )?;
        let deployment = component
            .into_codegen_view()
            .try_deployment(Default::default())?;
        Ok(rumoca_exec_wasm::NativeModelLibrary::from_preparation(
            code, deployment,
        )?)
    })?;
    let (prepared, _) = stage("prepared_model", || {
        Ok(PreparedNativeFmiLsModel::from_library(library)?)
    })?;
    let (core, core_s) = stage("fixed_guest_core", || {
        let (staging, target) = (work.join("fixed-source"), work.join("fixed-target"));
        fs::create_dir(&staging)?;
        fs::create_dir(&target)?;
        let cache = work.parent().unwrap_or(work).join("fixed-guest-cache");
        Ok(rumoca_exec_wasm::build_fixed_native_guest_core_cached(
            &staging, &target, &cache,
        )?)
    })?;
    let (package, construct_s) = stage("construct", || {
        Ok(NativeFmiLsPackage::construct(prepared, core, model, &[])?)
    })?;
    let description_bytes = package.model_description().len();
    let component_bytes = package.component_bytes().len();
    let output = work.join("published");
    fs::create_dir(&output)?;
    let (_, publish_s) = stage("publish", || Ok(package.publish(&output)?))?;
    let published: Vec<_> = fs::read_dir(&output)?
        .map(|entry| {
            entry.and_then(|e| {
                Ok((
                    e.file_name().to_string_lossy().into_owned(),
                    e.metadata()?.len(),
                ))
            })
        })
        .collect::<Result<_, _>>()?;
    println!(
        "{}",
        serde_json::json!({
            "scope": "direct-native FMU construction and publication; not loaded",
            "model": model, "model_description_bytes": description_bytes,
            "component_bytes": component_bytes, "published": published,
            "seconds": {"emission": emission_s, "fixed_guest_core": core_s, "construct": construct_s, "publish": publish_s},
            "peak_rss_kib": fs::read_to_string("/proc/self/status")?.lines()
                .find_map(|l| l.strip_prefix("VmHWM:")).map(str::trim).unwrap_or(""),
        })
    );
    Ok(())
}
