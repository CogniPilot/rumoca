//! Regression tests for flatten-time scoped import resolution.

use rumoca_ir_ast as ast;
use rumoca_phase_flatten::flatten_ref;
use rumoca_phase_instantiate::instantiate_model;
use rumoca_phase_resolve::resolve;
use rumoca_phase_typecheck::typecheck_instanced;

fn flatten_model(source: &str, model: &str) -> rumoca_ir_flat::Model {
    let def = rumoca_phase_parse::parse_to_ast(source, "scoped_imports.mo").unwrap();
    let mut tree = ast::ClassTree::from_parsed(def);
    tree.source_map.add("scoped_imports.mo", source);
    let parsed = rumoca_ir_ast::ParsedTree::new(tree);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = &resolved.0;
    let mut overlay = instantiate_model(tree, model).expect("instantiate should succeed");
    typecheck_instanced(tree, &mut overlay, model).expect("typecheck should succeed");
    flatten_ref(tree, &overlay, model).expect("flatten should succeed")
}

fn expr_real(expr: &rumoca_core::Expression) -> f64 {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        } => *value,
        other => panic!("expected Real literal, got {other:?}"),
    }
}

fn flat_var<'a>(flat: &'a rumoca_ir_flat::Model, name: &str) -> &'a rumoca_ir_flat::Variable {
    flat.variables
        .get(&rumoca_core::VarName::new(name))
        .unwrap_or_else(|| panic!("missing flat variable {name}"))
}

#[test]
fn sibling_scoped_import_aliases_do_not_bleed_between_component_instances() {
    let source = r#"
package ScopedImport
  package A
    constant Integer n = 2;
    constant Real value = 10.0;
  end A;

  package B
    constant Integer n = 3;
    constant Real value = 20.0;
  end B;

  model Cell
    parameter Real p;
    Real x;
  end Cell;

  model BoxA
    import K = ScopedImport.A;
    parameter Real local = K.value;
    Real attr(start = K.value, min = K.value);
    Real dimmed[K.n];
    Cell cell(p = K.value, x(start = K.value));
  end BoxA;

  model BoxB
    import K = ScopedImport.B;
    parameter Real local = K.value;
    Real attr(start = K.value, min = K.value);
    Real dimmed[K.n];
    Cell cell(p = K.value, x(start = K.value));
  end BoxB;

  model Top
    BoxA a;
    BoxB b;
  end Top;
end ScopedImport;
"#;

    let flat = flatten_model(source, "ScopedImport.Top");

    let a_local = flat_var(&flat, "a.local")
        .binding
        .as_ref()
        .expect("a.local binding");
    let b_local = flat_var(&flat, "b.local")
        .binding
        .as_ref()
        .expect("b.local binding");
    assert_eq!(expr_real(a_local), 10.0);
    assert_eq!(expr_real(b_local), 20.0);

    let a_attr = flat_var(&flat, "a.attr");
    let b_attr = flat_var(&flat, "b.attr");
    assert_eq!(
        expr_real(a_attr.start.as_ref().expect("a.attr start")),
        10.0
    );
    assert_eq!(expr_real(a_attr.min.as_ref().expect("a.attr min")), 10.0);
    assert_eq!(
        expr_real(b_attr.start.as_ref().expect("b.attr start")),
        20.0
    );
    assert_eq!(expr_real(b_attr.min.as_ref().expect("b.attr min")), 20.0);

    let a_cell_p = flat_var(&flat, "a.cell.p")
        .binding
        .as_ref()
        .expect("a.cell.p binding");
    let b_cell_p = flat_var(&flat, "b.cell.p")
        .binding
        .as_ref()
        .expect("b.cell.p binding");
    assert_eq!(expr_real(a_cell_p), 10.0);
    assert_eq!(expr_real(b_cell_p), 20.0);

    assert_eq!(
        expr_real(
            flat_var(&flat, "a.cell.x")
                .start
                .as_ref()
                .expect("a.cell.x start")
        ),
        10.0
    );
    assert_eq!(
        expr_real(
            flat_var(&flat, "b.cell.x")
                .start
                .as_ref()
                .expect("b.cell.x start")
        ),
        20.0
    );

    assert_eq!(flat_var(&flat, "a.dimmed").dims, vec![2]);
    assert_eq!(flat_var(&flat, "b.dimmed").dims, vec![3]);
}
