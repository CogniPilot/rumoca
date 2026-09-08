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
    assert!(
        manifest.contains("autotests = false")
            && manifest.contains("name = \"suite_exec_mlir\"")
            && manifest.contains("path = \"tests/suite_exec_mlir/main.rs\""),
        "MLIR integration tests must use one explicit umbrella crate"
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
    let mlir_cpu_args = flake
        .split_once("mlirCpuTestArgs = builtins.concatStringsSep \" \" [")
        .and_then(|(_, suffix)| suffix.split_once("];"))
        .map(|(args, _)| args)
        .expect("flake.nix must declare the mlirCpuTestArgs list");
    let selected_args = mlir_cpu_args
        .lines()
        .filter_map(|line| {
            line.trim()
                .strip_prefix('"')
                .and_then(|arg| arg.strip_suffix('"'))
        })
        .collect::<Vec<_>>();
    assert!(
        selected_args.contains(&"--lib"),
        "required MLIR CPU wiring omits the crate's library unit-test harness"
    );
    for required in ["--package rumoca-exec-mlir", "--features required-mlir-cpu"] {
        assert!(
            selected_args.contains(&required),
            "required MLIR CPU argument list is missing exact token `{required}`"
        );
    }
    for required in [
        "mlir-cpu = mlirCpuTests;",
        "clang-18 llc-18 mlir-opt-18 mlir-translate-18",
    ] {
        assert!(
            flake.contains(required),
            "required MLIR CPU wiring is missing `{required}`"
        );
    }
    assert!(
        selected_args.contains(&"--test suite_exec_mlir"),
        "required MLIR CPU lane does not select the MLIR umbrella crate"
    );
    let umbrella = repository_file("crates/rumoca-exec-mlir/tests/suite_exec_mlir/main.rs");
    for test in selected_tests {
        assert!(
            umbrella.contains(&format!("mod {test};")),
            "required MLIR CPU lane omits `{test}` from the umbrella"
        );
        let source = repository_file(&format!(
            "crates/rumoca-exec-mlir/tests/suite_exec_mlir/{test}.rs"
        ));
        assert!(
            source.contains("missing_cpu_tool::missing_cpu_tool(tool)"),
            "`{test}` does not route missing CPU tools through the fail-closed helper"
        );
        assert!(
            !source.contains("eprintln!(\"SKIP:"),
            "`{test}` can still silently skip in the required CPU lane"
        );
    }
    let support = repository_file(
        "crates/rumoca-exec-mlir/tests/suite_exec_mlir/support/missing_cpu_tool.rs",
    );
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
