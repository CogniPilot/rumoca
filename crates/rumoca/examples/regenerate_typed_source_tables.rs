//! Author current fixture tables from unchanged source, never repair old wires.
use serde_json::json;
use sha2::{Digest, Sha256};
use std::{env, fs, path::Path, time::Instant};

fn sha256(bytes: &[u8]) -> String {
    format!("{:x}", Sha256::digest(bytes))
}

fn main() -> anyhow::Result<()> {
    let args: Vec<_> = env::args().collect();
    let [_, output, revision, inventory_sha, expected_binary_sha] = args.as_slice() else {
        anyhow::bail!(
            "expected empty output directory, source revision, inventory SHA256, executable SHA256"
        );
    };
    let output = Path::new(output);
    anyhow::ensure!(
        output.is_dir() && fs::read_dir(output)?.next().is_none(),
        "output must be an existing empty owned directory"
    );
    anyhow::ensure!(
        inventory_sha.len() == 64 && inventory_sha.bytes().all(|byte| byte.is_ascii_hexdigit()),
        "invalid source inventory SHA256"
    );
    let binary_sha = sha256(&fs::read(env::current_exe()?)?);
    anyhow::ensure!(
        &binary_sha == expected_binary_sha,
        "actual executable SHA256 differs from issuing command"
    );
    let ordered =
        include_str!("../../rumoca-exec-wasm/tests/suite_typed_calls/OrderedFrameFold.mo");
    let integer = include_str!("../../rumoca-exec-wasm/tests/suite_typed_calls/IntegerSeedFold.mo");
    let tensor =
        include_str!("../../rumoca-exec-wasm/tests/suite_typed_calls/OrderedTensorFrameFold.mo");
    let variants = [
        (
            "fixed_tables/baseline.json",
            "NativeOrderedFrameFold",
            ordered.to_owned(),
        ),
        (
            "fixed_tables/gain-two.json",
            "NativeOrderedFrameFold",
            ordered.replace("gain = 1.0", "gain = 2.0"),
        ),
        (
            "fixed_tables/subtract.json",
            "NativeOrderedFrameFold",
            ordered.replace("total + gain*values", "total - gain*values"),
        ),
        (
            "fixed_integer_tables/baseline.json",
            "NativeIntegerSeedFold",
            integer.to_owned(),
        ),
        (
            "tensor_tables/baseline.json",
            "NativeOrderedTensorFrameFold",
            tensor.to_owned(),
        ),
        (
            "tensor_tables/gain-two.json",
            "NativeOrderedTensorFrameFold",
            tensor.replace("gain = 1.0", "gain = 2.0"),
        ),
    ];
    let mut files = Vec::new();
    for (relative, model, source) in variants {
        let started = Instant::now();
        let compiled = rumoca::Compiler::new()
            .model(model)
            .compile_str(&source, &format!("{model}.mo"))?;
        let package = rumoca_phase_solve::lower_solve_package(&compiled.dae)?;
        let value = json!({
            "model": model, "source": source, "sourceSha256": sha256(source.as_bytes()),
            "compilerRevision": revision, "compilerSourceSha256": inventory_sha,
            "producerBinarySha256": binary_sha, "elapsedMs": started.elapsed().as_secs_f64() * 1000.0,
            "table": package.pure_calls,
        });
        let bytes = serde_json::to_vec(&value)?;
        let path = output.join(relative);
        fs::create_dir_all(path.parent().expect("fixture relative path has a parent"))?;
        fs::write(path, &bytes)?;
        files.push(json!({"file": relative, "bytes": bytes.len(), "sha256": sha256(&bytes)}));
    }
    fs::write(
        output.join("producer.json"),
        serde_json::to_vec_pretty(&json!({
            "compilerRevision": revision, "compilerSourceSha256": inventory_sha,
            "producerBinarySha256": binary_sha, "files": files,
        }))?,
    )?;
    Ok(())
}
