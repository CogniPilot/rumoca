use super::*;
use crate::verify_cmd::tests::cargo_target_directory::in_cargo_target_directory;

#[test]
fn wasm_server_prefers_selected_cargo_artifact_and_preserves_executable_fallback() {
    in_cargo_target_directory(
        "wasm_smoke::tests::wasm_server_prefers_selected_cargo_artifact_and_preserves_executable_fallback",
        |root, target| {
            let selected = target.join("debug").join(exe_name("xtask"));
            let default = root.join("target/debug").join(exe_name("xtask"));
            if default != selected {
                std::fs::create_dir_all(default.parent().unwrap()).unwrap();
                std::fs::write(default, "wrong build root").unwrap();
            }
            assert_eq!(
                resolve_xtask_cli_executable(root).unwrap(),
                std::env::current_exe().unwrap()
            );
            std::fs::create_dir_all(selected.parent().unwrap()).unwrap();
            std::fs::write(&selected, "selected Cargo artifact").unwrap();
            let actual = resolve_xtask_cli_executable(root).unwrap();
            assert_eq!(actual, selected);
            assert_eq!(
                std::fs::read_to_string(actual).unwrap(),
                "selected Cargo artifact"
            );
        },
    );
}
