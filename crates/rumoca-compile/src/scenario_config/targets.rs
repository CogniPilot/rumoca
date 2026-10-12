use std::path::{Path, PathBuf};

use rumoca_phase_codegen::templates::builtin_targets;

/// Preserve built-in target identities and resolve custom targets beside the
/// scenario that declares them, independently of the caller's working directory.
pub fn resolve_scenario_codegen_target(scenario_path: &Path, target: &str) -> PathBuf {
    let path = PathBuf::from(target);
    if path.is_absolute()
        || builtin_targets()
            .iter()
            .any(|builtin| builtin.name == target)
    {
        return path;
    }
    scenario_path
        .parent()
        .unwrap_or_else(|| Path::new(""))
        .join(path)
}

#[cfg(test)]
mod tests {
    use super::resolve_scenario_codegen_target;
    use std::path::Path;

    #[test]
    fn scenario_targets_preserve_builtin_identity_and_custom_context() {
        let root = tempfile::tempdir().expect("temporary scenario root");
        let scenario = root.path().join("rumoca-scenario.codegen.toml");
        assert_eq!(
            resolve_scenario_codegen_target(&scenario, "jax-ode"),
            Path::new("jax-ode")
        );
        for target in ["checked_dae_report", "custom_checked_variables.jinja"] {
            assert_eq!(
                resolve_scenario_codegen_target(&scenario, target),
                root.path().join(target)
            );
        }
        let absolute = root.path().join("absolute-target");
        assert_eq!(
            resolve_scenario_codegen_target(&scenario, absolute.to_str().expect("UTF-8 path")),
            absolute
        );
    }
}
