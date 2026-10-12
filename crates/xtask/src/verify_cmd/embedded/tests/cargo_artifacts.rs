use crate::exe_name;
use crate::verify_cmd::embedded::built_compiler;
use crate::verify_cmd::tests::cargo_target_directory::in_cargo_target_directory;

#[test]
fn embedded_compiler_selects_only_the_configured_cargo_artifact() {
    in_cargo_target_directory(
        "verify_cmd::embedded::tests::cargo_artifacts::embedded_compiler_selects_only_the_configured_cargo_artifact",
        |root, target| {
            let selected = target.join("debug").join(exe_name("rumoca"));
            let default = root.join("target/debug").join(exe_name("rumoca"));
            if default != selected {
                std::fs::create_dir_all(default.parent().unwrap()).unwrap();
                std::fs::write(default, "wrong build root").unwrap();
            }
            let error = built_compiler(root).unwrap_err();
            assert_eq!(
                error.to_string(),
                format!("the workspace build did not produce {}", selected.display())
            );
            std::fs::create_dir_all(selected.parent().unwrap()).unwrap();
            std::fs::write(&selected, "selected Cargo artifact").unwrap();
            let actual = built_compiler(root).unwrap();
            assert_eq!(actual, selected);
            assert_eq!(
                std::fs::read_to_string(actual).unwrap(),
                "selected Cargo artifact"
            );
        },
    );
}
