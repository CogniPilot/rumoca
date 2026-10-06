use super::*;

struct CollectorMode(bool);

impl CollectorMode {
    fn set(enabled: bool) -> Self {
        Self(ENABLED.replace(enabled))
    }
}

impl Drop for CollectorMode {
    fn drop(&mut self) {
        ENABLED.set(self.0);
    }
}

fn flatten_source(source: &str, model: &str, prepared: bool) -> flat::Model {
    let _mode = CollectorMode::set(prepared);
    let file = "scalar_loop_projection_test.mo";
    let stored = rumoca_phase_parse::parse_to_ast(source, file).expect("fixture parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file, source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("fixture resolves");
    let instanced =
        rumoca_phase_instantiate::instantiate(resolved, model).expect("fixture instantiates");
    let ast::InstancedTree { tree, mut overlay } = instanced;
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, model)
        .expect("fixture typechecks");
    crate::flatten_ref(&tree, &overlay, model).expect("fixture flattens")
}

fn assert_scalar_views_equal(left: &flat::Model, right: &flat::Model) {
    assert_eq!(left.equations.len(), right.equations.len());
    for (left, right) in left.equations.iter().zip(&right.equations) {
        assert_eq!(left.residual, right.residual);
        assert_eq!(left.span, right.span);
        assert_eq!(format!("{:?}", left.origin), format!("{:?}", right.origin));
        assert_eq!(left.scalar_count, right.scalar_count);
    }
    // These compact catalogs include exact DefId/InstanceId, binder ids/domain,
    // regular accesses, symbolic body, scalar-view mode, spans and source origins.
    assert_eq!(
        format!("{:?}", left.variables),
        format!("{:?}", right.variables)
    );
    assert_eq!(
        format!("{:?}", left.structured_equations),
        format!("{:?}", right.structured_equations)
    );
    assert_eq!(
        format!("{:?}", left.assert_equations),
        format!("{:?}", right.assert_equations)
    );
    assert_eq!(
        format!("{:?}", left.when_chains),
        format!("{:?}", right.when_chains)
    );
}

fn differential(source: &str, model: &str) -> (flat::Model, usize) {
    PREPARED_ROWS.set(0);
    PREPARED_BODIES.set(0);
    let prepared_start = std::time::Instant::now();
    let prepared = flatten_source(source, model, true);
    let prepared_elapsed = prepared_start.elapsed();
    let prepared_rows = PREPARED_ROWS.get();
    let exhaustive_start = std::time::Instant::now();
    let exhaustive = flatten_source(source, model, false);
    let exhaustive_elapsed = exhaustive_start.elapsed();
    assert_scalar_views_equal(&prepared, &exhaustive);
    eprintln!(
        "parse_to_flatten model={model} prepared_ms={} exhaustive_ms={} prepared_rows={prepared_rows}",
        prepared_elapsed.as_millis(),
        exhaustive_elapsed.as_millis()
    );
    (prepared, prepared_rows)
}

#[test]
fn scalar_loop_projection_matches_exhaustive_order_indices_and_occurrences() {
    let source = r#"
model Cell
  input Real u[5,5];
  Real x[5,5];
equation
  for row in 5:-2:1 loop
    for column in 1:2:5 loop
      x[row,column] = ((u[row,column] + (-0.0)) * (row + column)) / 2.0;
    end for;
  end for;
end Cell;
model Projection
  Cell a;
  Cell b;
end Projection;
"#;
    let (flat, prepared_rows) = differential(source, "Projection");
    assert_eq!(flat.equations.len(), 18);
    assert_eq!(
        prepared_rows, 18,
        "the prepared path must actually emit these views"
    );
    assert_eq!(flat.structured_equations.len(), 2);
}

#[test]
fn scalar_loop_projection_retains_dependent_nested_domain_and_body_order() {
    let source = r#"
model Projection
  Real x[4,4];
  Real z[4,4];
equation
  for row in 1:4 loop
    for column in 1:row loop
      x[row,column] = row - column;
      z[row,column] = x[row,column] * 2.0;
    end for;
  end for;
end Projection;
"#;
    let (flat, rows) = differential(source, "Projection");
    assert_eq!(flat.equations.len(), 20);
    assert_eq!(rows, 20);
}

#[test]
fn scalar_loop_projection_preserves_negative_binder_literals_and_integer_folding() {
    let source = r#"
model Projection
  Real x[3];
equation
  for i in 1:-1:-1 loop
    x[(i + 2)] = i * (-0.0);
  end for;
end Projection;
"#;
    let (flat, rows) = differential(source, "Projection");
    assert_eq!(flat.equations.len(), 3);
    assert_eq!(rows, 3);
}

#[test]
fn scalar_loop_projection_declines_indexed_component_occurrence_selection() {
    let source = r#"
model Cell
  Real x;
end Cell;
model Projection
  Cell cells[3];
  Real x[3];
equation
  for i in 1:3 loop
    x[i] = cells[i].x;
  end for;
end Projection;
"#;
    let (flat, rows) = differential(source, "Projection");
    assert_eq!(flat.equations.len(), 3);
    assert_eq!(rows, 0);
}

#[test]
fn scalar_loop_projection_declines_typed_empty_reductions_and_array_slices() {
    let source = r#"
model Projection
  Real x[3];
  Real y[3,2];
equation
  for i in 1:3 loop
    x[i] = sum({j for j in 2:1});
    y[i,:] = {1.0,2.0};
  end for;
end Projection;
"#;
    let (flat, rows) = differential(source, "Projection");
    assert_eq!(flat.equations.len(), 6);
    assert_eq!(rows, 0);
}

#[test]
fn scalar_loop_projection_preserves_empty_domain_and_shadowed_binder_fallback() {
    let source = r#"
model Projection
  Real x[3];
equation
  for i in 2:1 loop
    x[i] = sum({j for j in 2:1});
  end for;
  for i in 1:3 loop
    x[i] = sum({i for i in 1:2});
  end for;
end Projection;
"#;
    let (flat, rows) = differential(source, "Projection");
    assert_eq!(flat.equations.len(), 3);
    assert_eq!(rows, 0);
}

#[test]
fn full_harris_native_frame_preserves_all_source_scalar_views() {
    let source = include_str!("fixtures/harris_native_frame.mo");
    let (flat, rows) = differential(source, "HarrisNativeFrame");
    assert_eq!(flat.equations.len(), 133_776);
    assert_eq!(rows, 133_776);
    assert_eq!(
        PREPARED_BODIES.get(),
        858,
        "qualification/lowering must be shared by the inner-loop scope"
    );
    assert_eq!(flat.structured_equations.len(), 10);
    assert!(
        flat.structured_equations
            .iter()
            .all(|family| family.interiors_materialized)
    );
}
