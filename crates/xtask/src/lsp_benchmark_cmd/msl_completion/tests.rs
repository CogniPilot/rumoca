use super::*;
use crate::verify_cmd::tests::cargo_target_directory::in_cargo_target_directory;

#[test]
fn built_lsp_binary_selects_only_the_configured_cargo_artifact() {
    in_cargo_target_directory(
        "lsp_benchmark_cmd::msl_completion::tests::built_lsp_binary_selects_only_the_configured_cargo_artifact",
        |root, target| {
            let expected = target.join("debug").join(exe_name("rumoca-lsp"));
            let default = root.join("target/debug").join(exe_name("rumoca-lsp"));
            if default != expected {
                fs::create_dir_all(default.parent().unwrap()).unwrap();
                fs::write(&default, "wrong build root").unwrap();
            }
            let error = built_lsp_binary(root).unwrap_err();
            assert_eq!(
                error.to_string(),
                format!("missing rumoca-lsp binary at {}", expected.display())
            );
            fs::create_dir_all(expected.parent().unwrap()).unwrap();
            fs::write(&expected, "selected Cargo artifact").unwrap();
            let actual = built_lsp_binary(root).unwrap();
            assert_eq!(actual, expected);
            assert_eq!(
                fs::read_to_string(actual).unwrap(),
                "selected Cargo artifact"
            );
        },
    );
}
