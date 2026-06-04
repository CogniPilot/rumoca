use std::path::PathBuf;

fn fixture_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/basemodelica")
        .join(name)
}

#[test]
fn parses_basemodelica_first_subset() {
    // Based on the BaseModelica flat-output First example from
    // ModelicaSpecification issue 3505. Keep this fixture compact so it tests
    // the parser-facing syntax without making routine CI output noisy.
    let path = fixture_path("first_subset.mo");
    let source = std::fs::read_to_string(&path).expect("read BaseModelica fixture");
    let file_name = path.display().to_string();
    let ast = rumoca_phase_parse::parse_to_ast(&source, &file_name)
        .expect("BaseModelica First subset should parse");

    let package = ast.classes.get("'First'").expect("top-level package");
    assert!(package.classes.contains_key("'StateSelect'"));
    let model = package.classes.get("'First'").expect("nested model");
    assert!(model.components.contains_key("'inertia1.phi'"));
    assert!(model.components.contains_key("'torque.useSupport'"));
    assert_eq!(model.equations.len(), 3);
}
