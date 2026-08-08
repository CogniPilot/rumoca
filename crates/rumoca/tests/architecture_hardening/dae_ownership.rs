use std::fs;

use super::architecture_hardening_support::workspace_root;

#[test]
fn canonical_dae_is_non_cloneable_and_shared_at_session_boundaries() {
    let root = workspace_root();
    let model = read(&root, "crates/rumoca-ir-dae/src/model.rs");
    let dae_declaration = declaration_prefix(&model, "pub struct Dae");
    assert!(
        !dae_declaration.contains("Clone"),
        "canonical Dae must not derive Clone; session boundaries share Arc<Dae>"
    );
    assert!(
        !model.contains("Clone for Dae"),
        "canonical Dae must not implement Clone"
    );
    assert!(
        !declaration_prefix(&model, "struct FrozenStorage").contains("Clone"),
        "frozen DAE arenas must not regain an internal deep-copy capability"
    );

    let session = read(&root, "crates/rumoca-compile/src/session.rs");
    let result = declaration_body(&session, "pub struct CompilationResult");
    assert!(
        result.contains("pub dae: Arc<dae::Dae>"),
        "full compilation results must share their one checked DAE root"
    );

    let facade = read(&root, "crates/rumoca/src/compiler.rs");
    let facade_result = declaration_body(&facade, "pub struct CompilationResult");
    assert!(
        facade_result.contains("pub dae: Arc<Dae>"),
        "the public compiler facade must expose checked DAE sharing explicitly"
    );
    let constructor = declaration_body(&facade, "pub fn new(");
    assert!(
        constructor.contains("dae: Arc<Dae>"),
        "the compiler facade constructor must accept the existing shared root"
    );
}

#[test]
fn rendering_borrows_the_checked_dae_without_copy_adapters() {
    let root = workspace_root();
    let renderer = read(
        &root,
        "crates/rumoca-phase-codegen/src/codegen/solve_renderer.rs",
    );
    assert!(
        renderer.contains("dae_model: &dae::Dae"),
        "Solve template projection must borrow the checked DAE"
    );

    for prohibited in [
        "new_owned_with_shared_dae",
        "Arc::new(dae_model)",
        "dae_model.clone()",
    ] {
        assert!(
            !renderer.contains(prohibited),
            "Solve template renderer retains obsolete DAE copy path `{prohibited}`"
        );
    }

    let compile_support = read(
        &root,
        "crates/rumoca-compile/src/session/compile_support.rs",
    );
    assert!(
        !compile_support.contains("unwrap_or_clone(artifact.dae)"),
        "session result assembly must share its cached DAE instead of copying it"
    );
}

fn read(root: &std::path::Path, relative: &str) -> String {
    fs::read_to_string(root.join(relative))
        .unwrap_or_else(|error| panic!("read {relative}: {error}"))
        .replace("\r\n", "\n")
}

fn declaration_prefix<'a>(source: &'a str, declaration: &str) -> &'a str {
    let end = source
        .find(declaration)
        .unwrap_or_else(|| panic!("missing declaration `{declaration}`"));
    let start = source[..end].rfind("\n\n").map_or(0, |index| index + 2);
    &source[start..end]
}

fn declaration_body<'a>(source: &'a str, declaration: &str) -> &'a str {
    let start = source
        .find(declaration)
        .unwrap_or_else(|| panic!("missing declaration `{declaration}`"));
    let end = source[start..]
        .find("\n}")
        .map_or(source.len(), |offset| start + offset);
    &source[start..end]
}
