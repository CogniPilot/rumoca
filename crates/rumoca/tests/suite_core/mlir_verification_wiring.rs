use std::path::PathBuf;

fn repository_file(relative: &str) -> String {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join(relative);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("failed to read {}: {error}", path.display()))
}

#[test]
fn required_mlir_cpu_lane_is_wired_fail_closed() {
    let manifest = repository_file("crates/rumoca-exec-mlir/Cargo.toml");
    assert!(
        manifest.contains("required-mlir-cpu = []"),
        "MLIR CPU verification must use a Cargo-native feature"
    );

    let selected_tests = [
        "benchmark_matmul",
        "compile_basic",
        "implicit_euler",
        "integrate",
        "linsolve_mlir",
        "multi_fn_mlir",
        "options",
    ];
    let flake = repository_file("flake.nix");
    for required in [
        "mlir-cpu = mlirCpuTests;",
        "--features required-mlir-cpu",
        "clang-18 llc-18 mlir-opt-18 mlir-translate-18",
    ] {
        assert!(
            flake.contains(required),
            "required MLIR CPU wiring is missing `{required}`"
        );
    }
    for test in selected_tests {
        assert!(
            flake.contains(&format!("--test {test}")),
            "required MLIR CPU lane does not select `{test}`"
        );
        let source = repository_file(&format!("crates/rumoca-exec-mlir/tests/{test}.rs"));
        assert!(
            source.contains("support::missing_cpu_tool(tool)"),
            "`{test}` does not route missing CPU tools through the fail-closed helper"
        );
        assert!(
            !source.contains("eprintln!(\"SKIP:"),
            "`{test}` can still silently skip in the required CPU lane"
        );
    }
    let support = repository_file("crates/rumoca-exec-mlir/tests/support/mod.rs");
    assert!(
        support.contains("#[cfg(feature = \"required-mlir-cpu\")]")
            && support.contains("panic!(\"required MLIR CPU tool is unavailable: {tool}\")"),
        "required MLIR CPU feature must turn missing tools into a hard failure"
    );

    let workflow = repository_file(".github/workflows/ci.yml");
    assert!(
        workflow.contains(".#checks.x86_64-linux.mlir-cpu"),
        "required CI must build the fail-closed MLIR CPU check"
    );
}
