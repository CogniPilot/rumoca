//! Original-source native emission evidence; no deployment or execution authority.
use std::{env, error::Error, fs, io::Write, time::Instant};

fn stage<T>(
    name: &str,
    action: impl FnOnce() -> Result<T, Box<dyn Error>>,
) -> Result<T, Box<dyn Error>> {
    let started = Instant::now();
    eprintln!("NATIVE_EMISSION stage={name} event=begin");
    match action() {
        Ok(value) => {
            eprintln!(
                "NATIVE_EMISSION stage={name} event=end seconds={}",
                started.elapsed().as_secs_f64()
            );
            Ok(value)
        }
        Err(error) => {
            eprintln!(
                "NATIVE_EMISSION stage={name} event=refused seconds={} error={error:?}",
                started.elapsed().as_secs_f64()
            );
            Err(error)
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let arguments: Vec<_> = env::args().collect();
    let (source_path, model, output_path, policy) = arguments_and_policy(&arguments)?;
    eprintln!(
        "NATIVE_EMISSION capacity_request private_bytes={} public_bytes={}",
        policy.private_arena_bytes(),
        policy.public_scratch_bytes(),
    );
    if std::path::Path::new(output_path).exists() {
        return Err("output must not already exist".into());
    }
    let source = fs::read_to_string(source_path)?;
    let started = Instant::now();
    let compilation = stage("frontend", || {
        rumoca::Compiler::new()
            .model(model)
            .compile_str(&source, source_path)
            .map_err(|error| error.to_string().into())
    })?;
    let component = stage("checked_component", || {
        Ok(rumoca_sim::lower_fmi_component(&compilation.dae)?)
    })?;
    let root = component.runtime_view().retain_assertion_root();
    let preparation = stage("native_preparation", || Ok(root.native_preparation()?))?;
    let code = stage("native_emission", || {
        Ok(
            rumoca_exec_wasm::compile_retained_native_preparation_wasm_with_policy(
                preparation,
                policy,
            )?,
        )
    })?;
    let bytes = code.compiled().module_bytes();
    let report = code.compiled().scratch_report();
    stage("write_module", || {
        let mut output = fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(output_path)?;
        output.write_all(bytes)?;
        output.sync_all()?;
        Ok(())
    })?;
    println!(
        "{}",
        serde_json::json!({
            "scope": "original-source native WASM emission; no deployment or execution",
            "model": model, "source_bytes": source.len(),
            "module_path": output_path, "module_bytes": bytes.len(),
            "capacity_request": {
                "private_bytes": policy.private_arena_bytes(),
                "public_bytes": policy.public_scratch_bytes(),
            },
            "scratch": {
                "total_bytes": report.total_bytes,
                "work_y_bytes": report.work_y_bytes,
                "call_input_bytes": report.call_input_bytes,
                "call_output_bytes": report.call_output_bytes,
                "call_scratch_bytes": report.call_scratch_bytes,
                "memo_bytes": report.memo_bytes,
                "typed_lane_bytes": report.typed_lane_bytes,
                "p_save_bytes": report.p_save_bytes,
                "unshared_call_scratch_bytes": report.unshared_call_scratch_bytes,
                "widest_owner": report.widest_owner,
            },
            "total_seconds": started.elapsed().as_secs_f64(),
            "emitted": true, "executed": false, "published": false
        })
    );
    Ok(())
}

fn arguments_and_policy(
    arguments: &[String],
) -> Result<
    (
        &str,
        &str,
        &str,
        rumoca_exec_wasm::CheckedNativeWasmCapacityPolicy,
    ),
    Box<dyn Error>,
> {
    use rumoca_exec_wasm::{CheckedNativeWasmCapacityPolicy, NativeWasmCapacityRequest};
    match arguments {
        [_, source, model, output] => Ok((
            source,
            model,
            output,
            CheckedNativeWasmCapacityPolicy::default(),
        )),
        [_, source, model, output, private, public] => {
            let policy = CheckedNativeWasmCapacityPolicy::new(NativeWasmCapacityRequest {
                private_arena_bytes: private.parse()?,
                public_scratch_bytes: public.parse()?,
            })
            .map_err(|error| std::io::Error::new(std::io::ErrorKind::InvalidInput, error))?;
            Ok((source, model, output, policy))
        }
        _ => Err(
            "usage: native_emission SOURCE MODEL NEW_OUTPUT_WASM [PRIVATE_BYTES PUBLIC_BYTES]"
                .into(),
        ),
    }
}
