//! Source counterexamples for MLS 10.4.1 enclosing ranges and 10.4.1.2 nesting.
use super::*;

fn model(source: &str) -> ast::ClassDef {
    resolve_tree_source(source)
        .into_inner()
        .definitions
        .classes
        .shift_remove("Scopes")
        .expect("resolved source model")
}

fn constructor(expression: &ast::Expression) -> (&ast::Expression, &[ast::ForIndex]) {
    let ast::Expression::ArrayComprehension { expr, indices, .. } = expression else {
        panic!("source fixture contains an array constructor");
    };
    (expr, indices)
}

#[test]
fn comprehension_range_scope_reads_same_name_enclosing_declaration() {
    let class =
        model("model Scopes parameter Integer i=3; Real values[3]={i for i in 1:i}; end Scopes;");
    let enclosing = class.components["i"].def_id.expect("enclosing declaration");
    let (body, indices) = constructor(class.components["values"].binding.as_ref().unwrap());
    assert_eq!(find_comp_ref_def_id(&indices[0].range), Some(enclosing));
    assert_ne!(find_comp_ref_def_id(body), Some(enclosing));
    assert_eq!(
        indices[0].binder,
        find_comp_ref_def_id(body).map(rumoca_core::IndexBinderProvenance::Source)
    );
}

#[test]
fn comprehension_range_scope_last_iterator_is_outermost() {
    let class = model(
        "model Scopes parameter Integer j=9;
          Real values[3,2]={i+10*j for i in {j,j+1}, j in {1,2,3}};
        end Scopes;",
    );
    let enclosing = class.components["j"].def_id.expect("enclosing declaration");
    let (body, indices) = constructor(class.components["values"].binding.as_ref().unwrap());
    let ast::Expression::Binary { rhs, .. } = body else {
        panic!("source body is i+10*j");
    };
    let outer_iterator = find_comp_ref_def_id(rhs).expect("body reads outer iterator j");
    assert_ne!(outer_iterator, enclosing);
    assert_eq!(
        indices[1].binder,
        Some(rumoca_core::IndexBinderProvenance::Source(outer_iterator))
    );
    assert_eq!(
        find_comp_ref_def_id(&indices[0].range),
        Some(outer_iterator)
    );
}

#[test]
fn comprehension_range_scope_outer_range_cannot_read_inner_iterator() {
    let class = model(
        "model Scopes parameter Integer i=2;
          Real values[2,3]={i for i in 1:3, j in 1:i};
        end Scopes;",
    );
    let enclosing = class.components["i"].def_id.expect("enclosing declaration");
    let (body, indices) = constructor(class.components["values"].binding.as_ref().unwrap());
    assert_eq!(find_comp_ref_def_id(&indices[1].range), Some(enclosing));
    assert_ne!(find_comp_ref_def_id(body), Some(enclosing));
}

#[test]
fn comprehension_range_scope_nested_shadowing_reads_outer_iterator() {
    let class = model(
        "model Scopes parameter Integer i=9;
          Real values[:,:]={{i for i in {i,i+1}} for i in {1,2,3}};
        end Scopes;",
    );
    let enclosing = class.components["i"].def_id.expect("enclosing declaration");
    let (outer_body, _) = constructor(class.components["values"].binding.as_ref().unwrap());
    let (inner_body, indices) = constructor(outer_body);
    let outer_iterator = find_comp_ref_def_id(&indices[0].range).expect("inner range reads i");
    assert_ne!(outer_iterator, enclosing);
    assert_ne!(find_comp_ref_def_id(inner_body), Some(outer_iterator));
}

#[test]
fn comprehension_range_scope_equation_iterator_reads_enclosing_declaration() {
    let class = model(
        "model Scopes parameter Integer i=3; Real x[3]; equation for i in 1:i loop x[i]=i; end for; end Scopes;",
    );
    let enclosing = class.components["i"].def_id.unwrap();
    let ast::Equation::For { indices, equations } = &class.equations[0] else {
        panic!("source equation loop")
    };
    assert_eq!(find_comp_ref_def_id(&indices[0].range), Some(enclosing));
    let ast::Equation::Simple { rhs, .. } = &equations[0] else {
        panic!("source loop body")
    };
    assert_eq!(
        indices[0].binder,
        find_comp_ref_def_id(rhs).map(rumoca_core::IndexBinderProvenance::Source)
    );
    assert_ne!(find_comp_ref_def_id(rhs), Some(enclosing));
}
