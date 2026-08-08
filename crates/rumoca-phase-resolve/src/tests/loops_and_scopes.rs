//! Loop and nested-class scoping: for/while index scopes, range expression
//! resolution, and nested class DefId assignment.

use super::*;

#[test]
fn test_for_loop_scope() {
    let source = r#"
model Test
Real x[3];
equation
for i in 1:3 loop
    x[i] = i;
end for;
end Test;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_ok(), "resolution should succeed");
}

#[test]
fn test_for_equation_range_resolves() {
    let source = r#"
model Test
parameter Integer n = 3;
Real x[n];
equation
for i in 1:n loop
    x[i] = i;
end for;
end Test;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let model = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test should exist");
    let rumoca_ir_ast::Equation::For { indices, .. } = &model.equations[0] else {
        panic!("expected for-equation");
    };
    let range_expr = &indices[0].range;
    assert!(
        find_comp_ref_def_id(range_expr).is_some(),
        "range expression should resolve component references"
    );
}

#[test]
fn test_for_statement_range_resolves() {
    let source = r#"
model Test
parameter Integer n = 3;
Integer x;
algorithm
for i in 1:n loop
    x := i;
end for;
end Test;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let model = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test should exist");
    let stmt = model.algorithms[0].first().expect("for statement");
    let rumoca_ir_ast::Statement::For { indices, .. } = stmt else {
        panic!("expected for-statement");
    };
    let range_expr = &indices[0].range;
    assert!(
        find_comp_ref_def_id(range_expr).is_some(),
        "range expression should resolve component references"
    );
}

#[test]
fn test_while_condition_resolves() {
    let source = r#"
model Test
Integer n = 3;
algorithm
while n > 0 loop
    n := n - 1;
end while;
end Test;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let model = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test should exist");
    let stmt = model.algorithms[0].first().expect("while statement");
    let rumoca_ir_ast::Statement::While(block) = stmt else {
        panic!("expected while-statement");
    };
    assert!(
        find_comp_ref_def_id(&block.cond).is_some(),
        "while condition should resolve component references"
    );
}

#[test]
fn test_nested_class_resolution() {
    let source = r#"
package TestPkg
model Inner
    Real x;
end Inner;
end TestPkg;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_ok(), "resolution should succeed");

    let tree = result.unwrap().into_inner();
    let pkg = tree
        .definitions
        .classes
        .get("TestPkg")
        .expect("TestPkg should exist");
    assert!(pkg.def_id.is_some());

    let inner = pkg.classes.get("Inner").expect("Inner should exist");
    assert!(inner.def_id.is_some());
}
