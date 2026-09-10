//! SPEC_0029/0041, SPEC_0036/0043, and SPEC_0044 FMI absorption boundary.

use super::*;

fn manifest(crate_name: &str) -> toml::Value {
    let path = workspace_root()
        .join("crates")
        .join(crate_name)
        .join("Cargo.toml");
    let source = fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
    toml::from_str(&source).unwrap_or_else(|error| panic!("parse {}: {error}", path.display()))
}

fn feature_members(manifest: &toml::Value, feature: &str) -> Vec<String> {
    manifest["features"][feature]
        .as_array()
        .unwrap_or_else(|| panic!("feature `{feature}` must be an array"))
        .iter()
        .map(|member| {
            member
                .as_str()
                .unwrap_or_else(|| panic!("feature `{feature}` members must be strings"))
                .to_string()
        })
        .collect()
}

fn function_signature<'a>(source: &'a str, name: &str) -> &'a str {
    let marker = format!("fn {name}");
    let tail = source
        .split_once(&marker)
        .unwrap_or_else(|| panic!("missing `{marker}`"))
        .1;
    tail.split_once(") ->")
        .unwrap_or_else(|| panic!("unterminated signature for `{name}`"))
        .0
}

fn derive_prefix<'a>(source: &'a str, declaration: &str) -> &'a str {
    let before = source
        .split_once(declaration)
        .unwrap_or_else(|| panic!("missing `{declaration}`"))
        .0;
    before
        .rsplit_once("#[derive(")
        .unwrap_or_else(|| panic!("missing derive before `{declaration}`"))
        .1
}

#[test]
fn obsolete_fmi_crates_are_absorbed_without_shims() {
    let root = workspace_root();
    for removed in ["rumoca-ir-fmi", "rumoca-phase-fmi"] {
        assert!(
            !root.join("crates").join(removed).exists(),
            "{removed} must be deleted, not retained as a compatibility shim"
        );
    }

    let workspace = fs::read_to_string(root.join("Cargo.toml")).expect("read workspace manifest");
    assert!(!workspace.contains("rumoca-ir-fmi"));
    assert!(!workspace.contains("rumoca-phase-fmi"));

    let ir_root = fs::read_to_string(root.join("crates/rumoca-ir-solve/src/lib.rs"))
        .expect("read Solve IR root");
    assert!(ir_root.lines().any(|line| line == "pub mod fmi;"));

    let phase_root = fs::read_to_string(root.join("crates/rumoca-phase-solve/src/lib.rs"))
        .expect("read Solve phase root");
    assert!(phase_root.lines().any(|line| line == "pub mod fmi;"));
}

#[test]
fn fmi_runtime_projection_is_unconditional_but_export_apis_remain_feature_scoped() {
    let phase = manifest("rumoca-phase-solve");
    assert!(
        phase["features"].get("fmi").is_none(),
        "the canonical runtime projection is not an optional phase capability"
    );
    assert!(phase["dependencies"]["rumoca-eval-dae"].is_table());

    let sim = manifest("rumoca-sim");
    assert!(feature_members(&sim, "fmi").is_empty());

    let cli = manifest("rumoca");
    assert_eq!(feature_members(&cli, "fmi"), ["rumoca-sim/fmi"]);
    assert!(feature_members(&cli, "fmu-packaging").contains(&"fmi".to_string()));
    assert!(
        cli["dependencies"].get("rumoca-phase-solve").is_none(),
        "the CLI must reach FMI lowering only through rumoca-sim"
    );
}

#[test]
fn fmi_codegen_retains_one_nonconstructible_correlated_aggregate() {
    let root = workspace_root();
    let component = fs::read_to_string(root.join("crates/rumoca-ir-solve/src/fmi.rs"))
        .expect("read FMI component owner");
    assert!(component.contains("model: Arc<SolveModel>"));
    assert!(!derive_prefix(&component, "pub struct FmiComponent").contains("Clone"));
    assert!(!derive_prefix(&component, "pub struct FmiCodegenView").contains("Clone"));
    assert!(component.contains("pub fn into_codegen_view(self) -> FmiCodegenView"));
    for escape in [
        "into_solve",
        "into_retained_model",
        "-> Arc<SolveModel>",
        "pub fn new(",
    ] {
        assert!(
            !component.contains(escape),
            "FMI checked aggregate exposes banned construction/ownership escape `{escape}`"
        );
    }

    let lazy =
        fs::read_to_string(root.join("crates/rumoca-phase-codegen/src/codegen/solve_lazy.rs"))
            .expect("read Solve lazy renderer");
    assert!(lazy.contains("pub(super) enum SolveRenderHandle"));
    assert!(lazy.contains("Fmi(Arc<solve::fmi::FmiCCodegenView>)"));
    assert!(lazy.contains("Self::Fmi(component) => component.problem()"));
    assert!(lazy.contains("Self::Fmi(component) => component.artifacts()"));
    assert!(lazy.contains("Self::Fmi(component) => Value::from_serialize(component.as_ref())"));

    let renderer =
        fs::read_to_string(root.join("crates/rumoca-phase-codegen/src/codegen/solve_renderer.rs"))
            .expect("read Solve renderer");
    let fmi_constructor = function_signature(&renderer, "new_owned_with_fmi");
    assert!(!fmi_constructor.contains("SolveArtifacts"));
    assert!(!fmi_constructor.contains("artifacts"));
    assert!(renderer.contains("SolveRenderHandle::fmi(component)"));
    let context_constructor =
        function_signature(&renderer, "solve_render_context_value_with_handles");
    assert!(
        !context_constructor.contains("fmi_entry"),
        "serialized FMI metadata must be derived from the retained handle, not supplied separately"
    );
    assert!(renderer.contains("let fmi_entry = handle.fmi_value();"));
}
