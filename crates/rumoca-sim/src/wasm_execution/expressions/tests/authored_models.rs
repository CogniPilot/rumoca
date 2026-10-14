use super::*;

fn authored_model(document: &str, name: &str) -> rumoca_compile::compile::CompilationResult {
    let source = document
        .split("```modelica,interactive")
        .nth(1)
        .unwrap()
        .split_once('\n')
        .unwrap()
        .1
        .split("```")
        .next()
        .unwrap();
    let mut session = rumoca_compile::Session::new(Default::default());
    session.update_document("authored_book.mo", source);
    session.compile_model(name).unwrap()
}

#[test]
fn portable_backend_prepares_the_authored_book_models_completely() {
    let mut failures = Vec::new();
    for (document, name) in [
        (
            include_str!("../../../../../../docs/user-guide/src/introduction.md"),
            "HotAirBalloon",
        ),
        (
            include_str!("../../../../../../docs/user-guide/src/language/arrays-pde.md"),
            "Turkey",
        ),
    ] {
        let compiled = authored_model(document, name);
        let options = rumoca_solver::SimOptions::default();
        let lowered =
            crate::lower_correlated_for_simulation_with_overrides(&compiled.dae, &options).unwrap();
        let backend = Rc::new(WasmExecutionBackend {
            layout: lowered.model().problem.layout.clone(),
            calls: lowered.model().pure_calls.clone(),
        });
        if let Err(error) =
            rumoca_solver::SolveRuntime::new_with_execution_backend(lowered.model(), Some(backend))
        {
            failures.push(format!("{name}: {error}"));
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
