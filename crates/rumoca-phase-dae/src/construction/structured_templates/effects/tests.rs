use super::*;
use rumoca_core::{
    ComponentRefPart, ComponentReference, Function, Reference, SourceId, Span, Statement,
    StatementBlock,
};

fn span() -> Span {
    Span::from_offsets(SourceId::DUMMY, 200, 210)
}

fn component(name: &str) -> ComponentReference {
    ComponentReference::construct(
        false,
        span(),
        vec![ComponentRefPart {
            ident: name.to_owned(),
            span: span(),
            subs: Vec::new(),
            def_id: rumoca_core::DefId::new(9201),
        }],
    )
    .expect("one checked local component")
}

fn variable(name: &str) -> Expression {
    Expression::VarRef {
        name: Reference::from_component_reference(component(name)),
        subscripts: Vec::new(),
        span: span(),
    }
}

fn function(name: &str, pure: bool, body: Vec<Statement>) -> Function {
    let mut function = Function::new(name, span());
    function.pure = pure;
    function.body = body;
    function
}

/// `(y) := callee(u)` as a statement of the caller's body.
fn statement_call(callee: &str) -> Statement {
    Statement::FunctionCall {
        comp: Reference::new(callee),
        args: vec![variable("u")],
        outputs: vec![Some(component("y"))],
        span: span(),
    }
}

fn permits(functions: Vec<Function>, name: &str) -> bool {
    let mut flat = flat::Model::default();
    for function in functions {
        flat.add_function(function);
    }
    CallEffects::default().permits(&flat, &VarName::new(name))
}

#[test]
fn a_statement_call_admits_its_caller_only_when_the_callee_is_admitted() {
    let caller = || function("f", true, vec![statement_call("g")]);
    assert!(permits(
        vec![caller(), function("g", true, Vec::new())],
        "f"
    ));
    // The callee of a statement call is checked like an expression call:
    // an impure or unresolved callee withdraws the caller's view.
    assert!(!permits(
        vec![caller(), function("g", false, Vec::new())],
        "f"
    ));
    assert!(!permits(vec![caller()], "f"));
}

#[test]
fn reinit_and_when_statements_withdraw_a_function_body_view() {
    let reinit = Statement::Reinit {
        variable: component("y"),
        value: variable("u"),
        span: span(),
    };
    let when = Statement::When {
        blocks: vec![StatementBlock {
            cond: variable("u"),
            stmts: Vec::new(),
        }],
        span: span(),
    };
    for statement in [reinit, when] {
        assert!(!permits(vec![function("f", true, vec![statement])], "f"));
    }
    assert!(permits(vec![function("f", true, Vec::new())], "f"));
}
