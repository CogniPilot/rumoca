use super::*;

#[test]
fn equal_count_ordinary_parameter_guards_defer_input_and_call_incidence_to_dae() {
    for rhs in ["u[i]", "F(u[i])"] {
        let source = format!("function F input Real x; output Real y; algorithm y:=x+1; end F;
            model M parameter Boolean enabled=true; input Real u[3]; output Real y[3];
            equation for i in 1:3 loop if enabled then y[i]={rhs}; else y[i]=0; end if; end for; end M;");
        for compact in [false, true] {
            let flat = flatten_source(&source, "M", compact).unwrap();
            assert!(flat.parameter_branch_selections.is_empty());
            assert_eq!(flat.equations.len(), 3);
        }
    }
}

#[test]
fn original_full_image_named_radius_preserves_compact_dae_family_bodies() {
    let source = include_str!("fixtures/fast_native_frame.mo");
    let flat = flatten_source(source, "FastNativeFrame", true).unwrap();
    assert_eq!(flat.equations.len(), 28_801);
    // One family per body of the row/column nest (gray, conditional scores),
    // plus the selection vector.
    assert_eq!(flat.structured_equations.len(), 3);
    let mut map = rumoca_core::SourceMap::new();
    map.add("conditional_template_test.mo", source);
    let dae = rumoca_phase_dae::to_dae(&flat, map).unwrap();
    dae.inspect(|view| {
        assert_eq!(view.continuous_family_count(), 3);
        for ordinal in 0..2 {
            let family = view.continuous_family(ordinal).unwrap();
            assert_eq!(family.scalar_rows(), 90 * 160, "family {ordinal}");
            assert_eq!(family.bodies().len(), 1, "family {ordinal}");
            assert_eq!(
                family.scalar_view(),
                rumoca_core::ComprehensionScalarView::BinderSubstitution,
                "family {ordinal}"
            );
        }
    });
}

fn checked_dae(source: &str, flat: &flat::Model) -> Result<(), rumoca_phase_dae::ToDaeError> {
    let mut map = rumoca_core::SourceMap::new();
    map.add("conditional_template_test.mo", source);
    rumoca_phase_dae::to_dae(flat, map).map(|_| ())
}

#[test]
fn conditional_template_inactive_calls_keep_original_shape_and_activation_authority() {
    let cases = [
        (
            "function Sized input Integer n; output Real value[n]; algorithm for j in 1:n loop value[j]:=j; end for; end Sized;",
            "i",
            "Sized(2)",
        ),
        (
            "function F input Real a; output Real b; algorithm b:=a+1.0; end F;",
            "i",
            "F(2.0)",
        ),
        (
            "function F input Real a; output Real b; algorithm b:=a+1.0; end F;",
            "i",
            "F({1.0,2.0})",
        ),
        (
            "function F input Real a[7,7]; output Real b; algorithm b:=a[1,1]; end F;",
            "i",
            "F(fill(1.0,2,2))",
        ),
        (
            "function F input Real a[:]; output Real b; algorithm b:=sum(a); end F;",
            "F({1.0,2.0,3.0})",
            "F({1.0,2.0})",
        ),
        (
            "function F input Real a[3]; output Real b; algorithm b:=sum(a); end F;",
            "F({1.0,2.0,3.0})",
            "F({1.0,2.0})",
        ),
    ];
    for (declarations, live, inactive) in cases {
        let source = format!(
            "{declarations} model M output Real x[3]; equation for i in 1:3 loop if i>0 then x[i]={live}; else x[i]={inactive}; end if; end for; end M;"
        );
        let compact = differential(&source, "M");
        let dense = flatten_source(&source, "M", false).unwrap();
        checked_dae(&source, &compact)
            .expect("an inactive call cannot create a new certificate obligation");
        checked_dae(&source, &dense)
            .expect("the independent original dense route remains accepted");
    }
}

#[test]
fn conditional_template_complete_result_shape_declines_vectorized_inactive_call() {
    let source = "function F input Real a; output Real b; algorithm b:=a+1.0; end F; model M Real live[2]; Real x[3]; equation live=F({1.0,2.0}); for i in 1:3 loop if i>0 then x[i]=i; else x[i]=F({1.0,2.0}); end if; end for; end M;";
    let compact = differential(source, "M");
    assert!(
        compact.structured_equations[0].template.is_some(),
        "the exact DAE result shape, not a missing call certificate, declines this candidate"
    );
    let dense = flatten_source(source, "M", false).unwrap();
    checked_dae(source, &compact)
        .expect("a present vectorized certificate does not prove a scalar residual");
    checked_dae(source, &dense).expect("original dense equations remain accepted");
}

#[test]
fn conditional_template_inactive_temporal_operators_retain_original_dense_route() {
    let cases = [
        ("input Boolean b=false;", "if edge(b) then 1.0 else 0.0"),
        ("input Boolean b=false;", "if change(b) then 1.0 else 0.0"),
        ("input Real b=0.0;", "pre(b)"),
        ("input Real b=0.0;", "delay(b,0.1)"),
        ("input Real b=0.0;", "der(b)"),
    ];
    for (declaration, inactive) in cases {
        let source = format!(
            "model M {declaration} Real x[3]; equation for i in 1:3 loop if i>0 then x[i]=i; else x[i]={inactive}; end if; end for; end M;"
        );
        let compact = differential(&source, "M");
        let dense = flatten_source(&source, "M", false).unwrap();
        checked_dae(&source, &compact)
            .expect("optional templates cannot introduce temporal owners from an inactive arm");
        checked_dae(&source, &dense).expect("the original inactive source remains accepted");
    }
}

struct CaptureMode(bool);

impl Drop for CaptureMode {
    fn drop(&mut self) {
        ENABLED.set(self.0);
    }
}

fn flatten_source(source: &str, model: &str, capture: bool) -> Result<flat::Model, FlattenError> {
    let _mode = CaptureMode(ENABLED.replace(capture));
    let file = "conditional_template_test.mo";
    let stored = rumoca_phase_parse::parse_to_ast(source, file).expect("fixture parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file, source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("fixture resolves");
    let ast::InstancedTree { tree, mut overlay } =
        rumoca_phase_instantiate::instantiate(resolved, model).expect("fixture instantiates");
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, model)
        .expect("fixture typechecks");
    crate::flatten_ref(&tree, &overlay, model)
}

fn differential(source: &str, model: &str) -> flat::Model {
    let compact = flatten_source(source, model, true).expect("compact fixture flattens");
    let exhaustive = flatten_source(source, model, false).expect("original fixture flattens");
    assert_eq!(compact.equations.len(), exhaustive.equations.len());
    for (compact, exhaustive) in compact.equations.iter().zip(&exhaustive.equations) {
        assert_eq!(compact.residual, exhaustive.residual);
        assert_eq!(compact.span, exhaustive.span);
        assert_eq!(compact.scalar_count, exhaustive.scalar_count);
        assert_eq!(
            format!("{:?}", compact.origin),
            format!("{:?}", exhaustive.origin)
        );
    }
    assert_eq!(
        format!("{:?}", compact.variables),
        format!("{:?}", exhaustive.variables)
    );
    compact
}

#[test]
fn conditional_template_retains_ordered_binders_call_and_lazy_residual() {
    let source = r#"
function Identity input Real x; output Real y; algorithm y := x; end Identity;
model Cell
  input Real u[5,3]; Real x[5,3];
equation
  for row in 5:-2:1 loop
    for column in 1:3 loop
      if row > 1 and column < 3 then
        x[row,column] = Identity(u[row,column]);
      else
        x[row,column] = -0.0;
      end if;
    end for;
  end for;
end Cell;
model M Cell a; Cell b; end M;
"#;
    let flat = differential(source, "M");
    assert_eq!(flat.equations.len(), 18);
    assert_eq!(
        flat.structured_equations.len(),
        2,
        "families={:?}",
        flat.structured_equations
    );
    for family in &flat.structured_equations {
        let template = family
            .template
            .as_ref()
            .expect("one symbolic conditional template");
        assert_eq!(template.body.len(), 1);
        assert_eq!(family.domain.binders.len(), 2);
        let rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } = &template.body[0]
        else {
            panic!("the original lazy conditional remains an explicit owner");
        };
        assert_eq!(branches.len(), 1);
        assert!(matches!(
            branches[0].1,
            rumoca_core::Expression::Binary { .. }
        ));
        assert!(matches!(
            else_branch.as_ref(),
            rumoca_core::Expression::Binary { .. }
        ));
        let text = format!("{:?}", template.body);
        assert!(text.contains("Identity"));
        assert!(text.contains("row") && text.contains("column"));
    }
}

#[test]
fn conditional_template_preserves_nested_priority_and_distinct_targets() {
    let source = r#"
model M
  input Real u[3]; Real x[3]; Real y[3];
equation
  for i in 1:3 loop
    if u[i] > 0 then
      if i > 1 then x[i] = 1/u[i]; else y[i] = u[i]; end if;
    elseif u[i] < 0 then
      x[i] = -u[i];
    else
      y[i] = 0;
    end if;
  end for;
end M;
"#;
    let flat = differential(source, "M");
    let body = &flat.structured_equations[0].template.as_ref().unwrap().body[0];
    let rumoca_core::Expression::If { branches, .. } = body else {
        panic!("conditional owner")
    };
    assert_eq!(branches.len(), 2);
    assert!(matches!(branches[0].1, rumoca_core::Expression::If { .. }));
}

#[test]
fn conditional_template_declines_nested_branch_domains_without_changing_rows() {
    let source = r#"
model M Real x[3,2]; equation
  for i in 1:3 loop
    if i > 1 then
      for j in 1:2 loop x[i,j] = i+j; end for;
    else
      for j in 1:2 loop x[i,j] = i-j; end for;
    end if;
  end for;
end M;
"#;
    let flat = differential(source, "M");
    assert!(
        flat.structured_equations
            .iter()
            .all(|family| family.template.is_none())
    );
}

#[test]
fn conditional_template_declines_omitted_else_for_static_selected_branch() {
    let source = "model M Real x[3]; equation for i in 1:3 loop if i > 0 then x[i]=i; end if; end for; end M;";
    let flat = differential(source, "M");
    assert_eq!(flat.equations.len(), 3);
    assert!(
        flat.structured_equations
            .iter()
            .all(|family| family.template.is_none())
    );
}

#[test]
fn conditional_template_keeps_original_unbalanced_branch_error() {
    let source = "model M input Boolean b; Real x[3]; Real y[3]; equation for i in 1:3 loop if b then x[i]=1; y[i]=2; else x[i]=3; end if; end for; end M;";
    let compact = flatten_source(source, "M", true).unwrap_err();
    let original = flatten_source(source, "M", false).unwrap_err();
    eprintln!("original_unbalanced_branch_error={compact:?}");
    assert_eq!(compact.to_string(), original.to_string());
    assert!(matches!(compact, FlattenError::UnsupportedEquation { .. }));
}

#[test]
fn conditional_template_retains_compact_monitored_relation_owner() {
    let source = "model M input Real u[3]; Real x[3]; equation for i in 1:3 loop if u[i] > 0 then x[i]=u[i]; else x[i]=-u[i]; end if; end for; end M;";
    let flat = differential(source, "M");
    assert!(flat.structured_equations[0].template.is_some());
    let mut map = rumoca_core::SourceMap::new();
    map.add("conditional_template_test.mo", source);
    let dae = rumoca_phase_dae::to_dae(&flat, map).expect("checked structured state relation");
    dae.inspect(|view| {
        assert_eq!(view.structured_root_count(), 1);
        let (_, root) = view
            .structured_roots()
            .next()
            .expect("one compact event owner");
        assert_eq!(view.domain(root.domain()).unwrap().extents(), [3]);
        let span = root.provenance().span();
        assert_eq!(&source[span.start.0..span.end.0], "u[i] > 0");
    });
}

#[test]
fn conditional_template_unknown_scalar_call_rhs_has_checked_dae_result() {
    let source = "function Identity input Real a; output Real b; algorithm b := a; end Identity; model M input Real u[3]; Real x[3]; equation for i in 1:3 loop if u[i] > 0 then x[i]=Identity(u[i]); else x[i]=0.0; end if; end for; end M;";
    let flat = differential(source, "M");
    assert!(flat.structured_equations[0].template.is_some());
    let mut map = rumoca_core::SourceMap::new();
    map.add("conditional_template_test.mo", source);
    let dae = rumoca_phase_dae::to_dae(&flat, map).expect("canonical call shape proof");
    dae.inspect(|view| {
        assert_eq!(view.function_count(), 1);
        assert_eq!(view.structured_root_count(), 1);
    });
}

#[test]
fn conditional_template_declines_known_incompatible_inactive_rhs_shape() {
    let source = "function Pair input Real a; output Real b[2]; algorithm b := {a,a}; end Pair; model M Real x[3]; equation for i in 1:3 loop if i > 0 then x[i]=i; else x[i]=Pair(i); end if; end for; end M;";
    let flat = differential(source, "M");
    assert_eq!(flat.equations.len(), 3);
    assert!(
        flat.structured_equations
            .iter()
            .all(|family| family.template.is_none())
    );
    let mut map = rumoca_core::SourceMap::new();
    map.add("conditional_template_test.mo", source);
    rumoca_phase_dae::to_dae(&flat, map).expect("original selected static branches remain legal");
}

#[test]
fn conditional_template_canonical_dae_rejects_incompatible_branch_payload() {
    let source = "model M parameter Real bad[2]={1.0,2.0}; input Real u[3]; Real x[3]; equation for i in 1:3 loop if u[i] > 0 then x[i]=u[i]; else x[i]=0.0; end if; end for; end M;";
    let mut flat = differential(source, "M");
    let bad = flat
        .variables
        .get(&rumoca_core::VarName::new("bad"))
        .unwrap()
        .binding
        .clone()
        .unwrap();
    let template = flat.structured_equations[0].template.as_mut().unwrap();
    let rumoca_core::Expression::If { else_branch, .. } = &mut template.body[0] else {
        panic!("conditional residual owner");
    };
    let rumoca_core::Expression::Binary { rhs, .. } = else_branch.as_mut() else {
        panic!("original subtraction residual");
    };
    **rhs = bad;
    let mut map = rumoca_core::SourceMap::new();
    map.add("conditional_template_test.mo", source);
    let error = rumoca_phase_dae::to_dae(&flat, map).unwrap_err();
    assert!(format!("{error:?}").contains("ShapeMismatch"), "{error:?}");
}

#[test]
fn conditional_template_refuses_unsupported_scheduled_event_at_occurrence() {
    let source = "model M Real x[3]; equation for i in 1:3 loop if time > i then x[i]=1; else x[i]=0; end if; end for; end M;";
    let flat = differential(source, "M");
    assert!(flat.structured_equations[0].template.is_some());
    let mut map = rumoca_core::SourceMap::new();
    map.add("conditional_template_test.mo", source);
    let error = rumoca_phase_dae::to_dae(&flat, map).unwrap_err();
    let rumoca_phase_dae::ToDaeError::Construction {
        source: fault,
        span,
    } = error
    else {
        panic!("unsupported event must have original construction provenance");
    };
    assert!(format!("{fault:?}").contains("UnsupportedStructuredEvent"));
    assert_eq!(&source[span.start.0..span.end.0], "time > i");
}
