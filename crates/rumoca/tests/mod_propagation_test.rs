//! Test modification propagation through nested components.
//!
//! This tests the pattern used in DFFREG: dFFR(n=n) should get n=2 when outer has n=2.

use rumoca_core::ComponentPath;
use rumoca_ir_ast as ast;
use rumoca_phase_instantiate::{InstantiationOutcome, instantiate_model_with_outcome};
use rumoca_phase_parse::parse_to_ast;
use rumoca_phase_resolve::resolve;
use rumoca_phase_typecheck::typecheck_instanced;

/// Helper: Find a component by qualified name in the overlay.
fn find_component<'a>(
    overlay: &'a ast::InstanceOverlay,
    name: &str,
) -> Option<&'a ast::InstanceData> {
    overlay
        .components
        .values()
        .find(|d| d.qualified_name.to_flat_string() == name)
}

fn disabled_contains(overlay: &ast::InstanceOverlay, path: &str) -> bool {
    overlay
        .disabled_components
        .contains(&ComponentPath::from_flat_path(path))
}

/// Helper: Assert that a component has the expected integer binding value.
fn assert_integer_binding(overlay: &ast::InstanceOverlay, comp_name: &str, expected: &str) {
    let data =
        find_component(overlay, comp_name).unwrap_or_else(|| panic!("{} should exist", comp_name));
    let binding = data
        .binding
        .as_ref()
        .unwrap_or_else(|| panic!("{} should have binding", comp_name));
    match binding {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
            ..
        } => {
            assert_eq!(
                token.text.as_ref(),
                expected,
                "{} should have binding {}, got {}",
                comp_name,
                expected,
                token.text.as_ref()
            );
        }
        _ => panic!(
            "{} binding should be integer literal, got {:?}",
            comp_name, binding
        ),
    }
}

/// Helper: Assert that a component has the expected boolean binding value.
fn assert_bool_binding(overlay: &ast::InstanceOverlay, comp_name: &str, expected: &str) {
    let data =
        find_component(overlay, comp_name).unwrap_or_else(|| panic!("{} should exist", comp_name));
    let binding = data
        .binding
        .as_ref()
        .unwrap_or_else(|| panic!("{} should have binding", comp_name));
    match binding {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
            ..
        } => {
            assert_eq!(
                token.text.as_ref(),
                expected,
                "{} should have binding {}, got {}",
                comp_name,
                expected,
                token.text.as_ref()
            );
        }
        _ => panic!(
            "{} binding should be bool literal, got {:?}",
            comp_name, binding
        ),
    }
}

fn assert_real_array_binding(data: &ast::InstanceData, expected: &[&str]) {
    let binding = data.binding.as_ref().unwrap_or_else(|| {
        panic!(
            "{} should have binding",
            data.qualified_name.to_flat_string()
        )
    });
    let ast::Expression::Array { elements, .. } = binding else {
        panic!(
            "{} binding should be an array row, got {:?}",
            data.qualified_name.to_flat_string(),
            binding
        );
    };
    let actual = elements
        .iter()
        .map(|expr| match expr {
            ast::Expression::Terminal {
                terminal_type: ast::TerminalType::UnsignedReal,
                token,
                ..
            } => token.text.as_ref(),
            other => panic!("expected real literal in row binding, got {other:?}"),
        })
        .collect::<Vec<_>>();
    assert_eq!(actual, expected);
}

fn assert_indexed_single_ref(expr: &ast::Expression, name: &str, index: &str) {
    let ast::Expression::ComponentReference(reference) = expr else {
        panic!("expected indexed component reference source, got {expr:?}");
    };
    assert_eq!(reference.parts.len(), 1);
    assert_eq!(reference.parts[0].ident.text.as_ref(), name);
    let Some(subscripts) = reference.parts[0].subs.as_ref() else {
        panic!("expected {name} to carry an element subscript");
    };
    assert_eq!(subscripts.len(), 1);
    let ast::Subscript::Expression(ast::Expression::Terminal { token, .. }) = &subscripts[0] else {
        panic!(
            "expected literal integer subscript, got {:?}",
            subscripts[0]
        );
    };
    assert_eq!(token.text.as_ref(), index);
}

/// Helper: Assert that a component has the expected dimensions.
fn assert_dims(overlay: &ast::InstanceOverlay, comp_name: &str, expected_dims: &[i64]) {
    let data =
        find_component(overlay, comp_name).unwrap_or_else(|| panic!("{} should exist", comp_name));
    assert_eq!(
        data.dims, expected_dims,
        "{} should have dims {:?}, got {:?}",
        comp_name, expected_dims, data.dims
    );
}

fn flat_expr_is_numeric_value(expr: &rumoca_core::Expression, expected: i64) -> bool {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => *value == expected,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        } => (*value - expected as f64).abs() <= f64::EPSILON,
        _ => false,
    }
}

fn flat_expr_mentions_name(expr: &rumoca_core::Expression, needle: &str) -> bool {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            name.as_str().contains(needle)
                || subscripts.iter().any(|subscript| match subscript {
                    rumoca_core::Subscript::Expr { expr, .. } => {
                        flat_expr_mentions_name(expr, needle)
                    }
                    _ => false,
                })
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            field.contains(needle) || flat_expr_mentions_name(base, needle)
        }
        rumoca_core::Expression::FunctionCall { name, args, .. } => {
            name.as_str().contains(needle)
                || args
                    .iter()
                    .any(|expr| flat_expr_mentions_name(expr, needle))
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::Array { elements: args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. } => args
            .iter()
            .any(|expr| flat_expr_mentions_name(expr, needle)),
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            flat_expr_mentions_name(lhs, needle) || flat_expr_mentions_name(rhs, needle)
        }
        rumoca_core::Expression::Unary { rhs, .. } => flat_expr_mentions_name(rhs, needle),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(cond, value)| {
                flat_expr_mentions_name(cond, needle) || flat_expr_mentions_name(value, needle)
            }) || flat_expr_mentions_name(else_branch, needle)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            flat_expr_mentions_name(start, needle)
                || step
                    .as_ref()
                    .is_some_and(|expr| flat_expr_mentions_name(expr, needle))
                || flat_expr_mentions_name(end, needle)
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            flat_expr_mentions_name(base, needle)
                || subscripts.iter().any(|subscript| match subscript {
                    rumoca_core::Subscript::Expr { expr, .. } => {
                        flat_expr_mentions_name(expr, needle)
                    }
                    _ => false,
                })
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            flat_expr_mentions_name(expr, needle)
                || indices
                    .iter()
                    .any(|index| flat_expr_mentions_name(&index.range, needle))
                || filter
                    .as_ref()
                    .is_some_and(|expr| flat_expr_mentions_name(expr, needle))
        }
        rumoca_core::Expression::Literal { .. } | rumoca_core::Expression::Empty { .. } => false,
    }
}

#[test]
fn test_modification_propagation_nested() {
    let source = r#"
        model SubModel
            parameter Integer n = 1;
            Real x[n];
        end SubModel;

        model Container
            parameter Integer n = 1;
            SubModel sub(n=n);
            Real y[n];
        end Container;

        model Test
            Container cont(n=2);
        end Test;
    "#;

    let stored_def = parse_to_ast(source, "<test>").expect("parse failed");
    let mut tree = ast::ClassTree::from_parsed(stored_def);
    tree.source_map.add("<test>", source);
    let parsed = ast::ParsedTree::new(tree);
    let resolved = resolve(parsed).expect("resolve failed");
    let tree = resolved.into_inner();

    let result = instantiate_model_with_outcome(&tree, "Test");
    let overlay = match result {
        InstantiationOutcome::Success(o) => o,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("Needs inner: {:?}", missing_inners);
        }
        InstantiationOutcome::Error(e) => {
            panic!("Error: {:?}", e);
        }
    };

    // Debug: print all component bindings
    println!("\n=== Component bindings ===");
    for (_def_id, data) in &overlay.components {
        let name = data.qualified_name.to_flat_string();
        if name.contains(".n") || name.ends_with(".x") || name.ends_with(".y") {
            println!(
                "  {}: binding={:?}, start={:?}, dims={:?}, dims_expr_len={}",
                name,
                data.binding,
                data.start,
                data.dims,
                data.dims_expr.len()
            );
        }
    }

    // Find cont.n and check its binding
    let outer_n = overlay
        .components
        .values()
        .find(|d| d.qualified_name.to_flat_string() == "cont.n")
        .expect("cont.n should exist");

    // cont.n should have binding = 2 (from Test's modification)
    let outer_n_binding = outer_n
        .binding
        .as_ref()
        .expect("cont.n should have binding");
    match outer_n_binding {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
            ..
        } => {
            assert_eq!(token.text.as_ref(), "2", "cont.n should have binding 2");
        }
        _ => panic!(
            "cont.n binding should be integer literal, got {:?}",
            outer_n_binding
        ),
    }

    // Find cont.sub.n
    let inner_n = overlay
        .components
        .values()
        .find(|d| d.qualified_name.to_flat_string() == "cont.sub.n")
        .expect("cont.sub.n should exist");

    // cont.sub.n should have binding = 2 (propagated from cont.n via n=n modification)
    let inner_n_binding = inner_n
        .binding
        .as_ref()
        .expect("cont.sub.n should have binding");
    match inner_n_binding {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
            ..
        } => {
            assert_eq!(
                token.text.as_ref(),
                "2",
                "cont.sub.n should have binding 2, got {}",
                token.text.as_ref()
            );
        }
        _ => panic!(
            "cont.sub.n binding should be integer literal, got {:?}",
            inner_n_binding
        ),
    }
}

#[test]
fn test_array_modifier_distribution_preserves_binding_source_scope() {
    let source = r#"
        record R
            Real p;
        end R;

        model Sub
            parameter R x[2];
        end Sub;

        model Top
            parameter Real a[2] = {1.0, 2.0};
            Sub s(x = {R(a[1]), R(a[2])});
        end Top;
    "#;

    let stored_def = parse_to_ast(source, "<test>").expect("parse failed");
    let mut tree = ast::ClassTree::from_parsed(stored_def);
    tree.source_map.add("<test>", source);
    let parsed = ast::ParsedTree::new(tree);
    let resolved = resolve(parsed).expect("resolve failed");
    let tree = resolved.into_inner();

    let overlay = match instantiate_model_with_outcome(&tree, "Top") {
        InstantiationOutcome::Success(o) => o,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("Needs inner: {:?}", missing_inners);
        }
        InstantiationOutcome::Error(e) => {
            panic!("Error: {:?}", e);
        }
    };

    let x1 = find_component(&overlay, "s.x[1]").expect("s.x[1] should exist");
    let x2 = find_component(&overlay, "s.x[2]").expect("s.x[2] should exist");

    assert!(
        x1.binding_from_modification,
        "s.x[1] should be marked as modifier-derived"
    );
    assert!(
        x2.binding_from_modification,
        "s.x[2] should be marked as modifier-derived"
    );
    assert!(
        x1.binding_source_scope.is_some(),
        "s.x[1] modifier binding should keep lexical source scope"
    );
    assert!(
        x2.binding_source_scope.is_some(),
        "s.x[2] modifier binding should keep lexical source scope"
    );
}

#[test]
fn test_array_component_modifier_reference_selects_element_row() {
    let source = r#"
        model Tower
            parameter Real v_flow_rate[:];
        end Tower;

        model TowerGroup
            parameter Integer n = 2;
            parameter Real v_flow_rate[n, 3];
            Tower ct[n](v_flow_rate = v_flow_rate);
        end TowerGroup;

        model Top
            TowerGroup group(v_flow_rate = {{1.0, 2.0, 3.0}, {4.0, 5.0, 6.0}});
        end Top;
    "#;

    let (_tree, overlay) = instantiate_test_model(source, "Top");
    let first = find_component(&overlay, "group.ct[1].v_flow_rate")
        .expect("first tower v_flow_rate should exist");
    let second = find_component(&overlay, "group.ct[2].v_flow_rate")
        .expect("second tower v_flow_rate should exist");

    assert!(
        first.binding_from_modification,
        "first tower binding should come from the array component modifier"
    );
    assert!(
        second.binding_from_modification,
        "second tower binding should come from the array component modifier"
    );
    assert_real_array_binding(first, &["1.0", "2.0", "3.0"]);
    assert_real_array_binding(second, &["4.0", "5.0", "6.0"]);

    let first_source = first
        .binding_source
        .as_ref()
        .expect("first tower should keep symbolic binding source");
    assert_indexed_single_ref(first_source, "v_flow_rate", "1");
}

#[test]
fn test_forwarded_colon_parameter_drives_record_size_dimension() {
    let source = r#"
        record Curve
            parameter Real V_flow[:];
            parameter Real dp[size(V_flow, 1)];
        end Curve;

        model Mover
            parameter Real V_flow[:];
            parameter Real dp[:];
            Curve pressure(V_flow = V_flow, dp = dp);
        end Mover;

        model Top
            parameter Real flows[2] = {1.0, 2.0};
            Mover m(V_flow = flows, dp = flows);
            Real y;
        equation
            y = m.pressure.dp[1];
        end Top;
    "#;

    let compiled = rumoca::Compiler::new()
        .model("Top")
        .compile_str(source, "test.mo")
        .expect("forwarded record size dimensions should compile");

    let dims = compiled
        .flat
        .variables
        .get(&rumoca_core::VarName::new("m.pressure.dp"))
        .map(|var| var.dims.clone())
        .expect("m.pressure.dp should be in flat variables");
    assert_eq!(dims, vec![2]);
}

#[test]
fn test_nested_record_parameter_size_dimension_resolves_through_alias() {
    let source = r#"
        record FlowParameters
            parameter Real V_flow[:];
            parameter Real dp[size(V_flow, 1)];
        end FlowParameters;

        record EfficiencyParameters
            parameter Real V_flow[:];
            parameter Real eta[size(V_flow, 1)];
        end EfficiencyParameters;

        record Generic
            parameter FlowParameters pressure;
            parameter EfficiencyParameters motorEfficiency;
            parameter EfficiencyParameters hydraulicEfficiency;
        end Generic;

        model Interface
            parameter Generic per;
            FlowParameters pressure = per.pressure;
            EfficiencyParameters motorEfficiency = per.motorEfficiency;
            final parameter Real motDer[size(per.motorEfficiency.V_flow, 1)];
            final parameter Real hydDer[size(per.hydraulicEfficiency.V_flow, 1)];
        end Interface;

        model Top
            parameter Real flows[3] = {1.0, 2.0, 3.0};
            parameter Real heads[3] = {10.0, 20.0, 30.0};
            parameter Real effs[3] = {0.1, 0.2, 0.3};
            Interface mover(
                per(
                    pressure(V_flow = flows, dp = heads),
                    motorEfficiency(V_flow = flows, eta = effs),
                    hydraulicEfficiency(V_flow = flows, eta = effs)));
            Real y;
        equation
            y = mover.pressure.dp[1] + mover.motDer[1] + mover.hydDer[1];
        end Top;
    "#;

    let compiled = rumoca::Compiler::new()
        .model("Top")
        .compile_str(source, "test.mo")
        .expect("nested record size dimensions should compile");

    for name in [
        "mover.per.pressure.dp",
        "mover.per.motorEfficiency.eta",
        "mover.pressure.dp",
        "mover.motorEfficiency.eta",
        "mover.motDer",
        "mover.hydDer",
    ] {
        let dims = compiled
            .flat
            .variables
            .get(&rumoca_core::VarName::new(name))
            .map(|var| var.dims.clone())
            .unwrap_or_else(|| panic!("{name} should be in flat variables"));
        assert_eq!(dims, vec![3], "{name} should keep the V_flow row count");
    }
}

#[test]
fn test_array_component_nested_record_defaults_drive_inner_size_dimensions() {
    let source = r#"
        record FlowParameters
            parameter Real V_flow[:];
            parameter Real dp[size(V_flow, 1)];
        end FlowParameters;

        record EfficiencyParameters
            parameter Real V_flow[:];
            parameter Real eta[size(V_flow, 1)];
        end EfficiencyParameters;

        record PowerParameters
            parameter Real V_flow[:];
            parameter Real P[size(V_flow, 1)];
        end PowerParameters;

        record Generic
            parameter FlowParameters pressure(V_flow = {0.0, 0.0}, dp = {0.0, 0.0});
            parameter EfficiencyParameters hydraulicEfficiency(V_flow = {0.0}, eta = {0.7});
            parameter EfficiencyParameters motorEfficiency(V_flow = {0.0}, eta = {0.7});
            parameter PowerParameters power(V_flow = {0.0}, P = {0.0});
            parameter Boolean motorCooledByFluid = true;
        end Generic;

        model Interface
            parameter Generic per;
            parameter Integer nOri;
            final parameter Real motDer[size(per.motorEfficiency.V_flow, 1)];
            final parameter Real hydDer[size(per.hydraulicEfficiency.V_flow, 1)];
        end Interface;

        partial model PartialMover
            parameter Generic per;
            final parameter Integer nOri = size(per.pressure.V_flow, 1);
            Interface eff(
                per(
                    final hydraulicEfficiency = per.hydraulicEfficiency,
                    final motorEfficiency = per.motorEfficiency,
                    final motorCooledByFluid = per.motorCooledByFluid,
                    final power = per.power),
                final nOri = nOri);
        end PartialMover;

        model SpeedMover
            extends PartialMover(
                eff(per(final pressure = per.pressure)));
        end SpeedMover;

        model WithoutMotor
            parameter Real HydEff[:];
            parameter Real MotEff[:];
            parameter Real VolFloCur[:];
            parameter Real PreCur[:];
            SpeedMover varSpeFloMov(
                per(
                    pressure(V_flow = VolFloCur, dp = PreCur),
                    hydraulicEfficiency(eta = HydEff, V_flow = VolFloCur),
                    motorEfficiency(eta = MotEff, V_flow = VolFloCur)));
        end WithoutMotor;

        model PumpSystem
            parameter Integer n = 2;
            parameter Real HydEff[n, 3];
            parameter Real MotEff[n, 3];
            parameter Real VolFloCur[n, 3];
            parameter Real PreCur[n, 3];
            WithoutMotor pum[n](
                HydEff = HydEff,
                MotEff = MotEff,
                VolFloCur = VolFloCur,
                PreCur = PreCur);
        end PumpSystem;

        model Top
            PumpSystem sys(
                HydEff = {{1.0, 1.0, 1.0}, {1.0, 1.0, 1.0}},
                MotEff = {{0.6, 0.7, 0.8}, {0.6, 0.7, 0.8}},
                VolFloCur = {{0.0, 1.0, 2.0}, {0.0, 1.0, 2.0}},
                PreCur = {{20.0, 10.0, 0.0}, {20.0, 10.0, 0.0}});
            Real y;
        equation
            y = sys.pum[1].varSpeFloMov.eff.motDer[1]
              + sys.pum[1].varSpeFloMov.eff.hydDer[1];
        end Top;
    "#;

    let compiled = rumoca::Compiler::new()
        .model("Top")
        .compile_str(source, "test.mo")
        .expect("array component record defaults should drive inner size dimensions");

    for name in [
        "sys.pum[1].varSpeFloMov.per.pressure.dp",
        "sys.pum[1].varSpeFloMov.per.motorEfficiency.eta",
        "sys.pum[1].varSpeFloMov.eff.per.pressure.dp",
        "sys.pum[1].varSpeFloMov.eff.per.motorEfficiency.eta",
        "sys.pum[1].varSpeFloMov.eff.motDer",
        "sys.pum[1].varSpeFloMov.eff.hydDer",
    ] {
        let dims = compiled
            .flat
            .variables
            .get(&rumoca_core::VarName::new(name))
            .map(|var| var.dims.clone())
            .unwrap_or_else(|| panic!("{name} should be in flat variables"));
        assert_eq!(
            dims,
            vec![3],
            "{name} should keep the selected pump curve row count"
        );
    }
}

#[test]
fn test_array_component_symbolic_row_modifier_keeps_source_shape_for_record_size() {
    let source = r#"
        record Curve
            parameter Real x[:];
            parameter Real y[size(x, 1)];
        end Curve;

        model Child
            parameter Real row[:];
            Curve curve(x = row, y = row);
        end Child;

        model Group
            parameter Integer n = 2;
            parameter Real rows[n, 3];
            Child child[n](row = rows);
        end Group;

        model Top
            parameter Real upstream[2, 3] = {{1.0, 2.0, 3.0}, {4.0, 5.0, 6.0}};
            Group group(rows = upstream);
            Real y;
        equation
            y = group.child[1].curve.y[1];
        end Top;
    "#;

    let compiled = rumoca::Compiler::new()
        .model("Top")
        .compile_str(source, "test.mo")
        .expect("symbolic row modifiers should preserve source array shape");

    for name in [
        "group.child[1].row",
        "group.child[1].curve.x",
        "group.child[1].curve.y",
    ] {
        let dims = compiled
            .flat
            .variables
            .get(&rumoca_core::VarName::new(name))
            .map(|var| var.dims.clone())
            .unwrap_or_else(|| panic!("{name} should be in flat variables"));
        assert_eq!(dims, vec![3], "{name} should keep the selected row width");
    }

    let row = compiled
        .flat
        .variables
        .get(&rumoca_core::VarName::new("group.child[1].row"))
        .expect("group.child[1].row should be in flat variables");
    let row_binding = row
        .binding
        .as_ref()
        .expect("group.child[1].row should keep a flat binding");
    match row_binding {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            assert_eq!(name.as_str(), "group.rows");
            assert!(
                matches!(
                    subscripts.as_slice(),
                    [rumoca_core::Subscript::Index { value: 1, .. }]
                ),
                "group.child[1].row should select the first source row, got {subscripts:?}"
            );
        }
        other => panic!("group.child[1].row should bind to selected source row, got {other:?}"),
    }
}

#[test]
fn test_nested_array_component_modifier_selects_element_row() {
    let source = r#"
        model Tower
            parameter Real v_flow_rate[:];
        end Tower;

        model TowerGroup
            parameter Integer n = 3;
            Tower ct[n];
        end TowerGroup;

        model Wrapper
            parameter Real rows[3, 3];
            TowerGroup group(ct(v_flow_rate = rows));
        end Wrapper;

        model Top
            parameter Real upstream[3, 3] = {
                {0.0, 0.5, 1.0},
                {0.0, 0.6, 1.0},
                {0.0, 0.7, 1.0}};
            Wrapper wrapper(rows = upstream);
            Real y;
        equation
            y = wrapper.group.ct[1].v_flow_rate[1];
        end Top;
    "#;

    let compiled = rumoca::Compiler::new()
        .model("Top")
        .compile_str(source, "test.mo")
        .expect("nested array component modifier should select row per element");

    let first = compiled
        .flat
        .variables
        .get(&rumoca_core::VarName::new(
            "wrapper.group.ct[1].v_flow_rate",
        ))
        .expect("first tower v_flow_rate should be in flat variables");
    assert_eq!(
        first.dims,
        vec![3],
        "nested modifier should distribute the selected row shape"
    );
    match first
        .binding
        .as_ref()
        .expect("first tower should keep a flat binding")
    {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            assert_eq!(name.as_str(), "wrapper.rows");
            assert!(
                matches!(
                    subscripts.as_slice(),
                    [rumoca_core::Subscript::Index { value: 1, .. }]
                ),
                "nested modifier should select the first source row, got {subscripts:?}"
            );
        }
        other => panic!("expected first tower binding to select source row, got {other:?}"),
    }
}

#[test]
fn test_record_field_modifier_drives_sibling_size_dimension_in_nested_array_component() {
    let source = r#"
        record Fan
            parameter Real r_V[:];
            parameter Real r_P[size(r_V, 1)];
        end Fan;

        model YorkCalc
            parameter Fan fanRelPow(r_V = {0.0, 0.5, 1.0}, r_P = {0.0, 0.2, 1.0});
            final parameter Real fanRelPowDer[size(fanRelPow.r_V, 1)];
        end YorkCalc;

        model Tower
            parameter Real v_flow_rate[:];
            parameter Real eta[:];
            YorkCalc yorkCalc(fanRelPow(r_V = v_flow_rate, r_P = eta));
        end Tower;

        model TowerGroup
            parameter Integer n = 3;
            Tower ct[n];
        end TowerGroup;

        model Wrapper
            parameter Real rows[3, 3];
            parameter Real powers[3, 3];
            TowerGroup group(ct(v_flow_rate = rows, eta = powers));
        end Wrapper;

        model Top
            parameter Real upstream[3, 3] = {
                {0.0, 0.5, 1.0},
                {0.0, 0.6, 1.0},
                {0.0, 0.7, 1.0}};
            parameter Real fanPower[3, 3] = {
                {0.0, 0.2, 1.0},
                {0.0, 0.3, 1.0},
                {0.0, 0.4, 1.0}};
            Wrapper wrapper(rows = upstream, powers = fanPower);
            Real y;
        equation
            y = wrapper.group.ct[1].yorkCalc.fanRelPow.r_P[1]
              + wrapper.group.ct[1].yorkCalc.fanRelPowDer[1];
        end Top;
    "#;

    let compiled = rumoca::Compiler::new()
        .model("Top")
        .compile_str(source, "test.mo")
        .expect("record field modifier should size sibling record fields");

    for name in [
        "wrapper.group.ct[1].yorkCalc.fanRelPow.r_V",
        "wrapper.group.ct[1].yorkCalc.fanRelPow.r_P",
        "wrapper.group.ct[1].yorkCalc.fanRelPowDer",
    ] {
        let dims = compiled
            .flat
            .variables
            .get(&rumoca_core::VarName::new(name))
            .map(|var| var.dims.clone())
            .unwrap_or_else(|| panic!("{name} should be in flat variables"));
        assert_eq!(
            dims,
            vec![3],
            "{name} should keep the selected tower fan curve row count"
        );
    }
}

#[test]
fn test_nested_attribute_modifier_on_array_component_selects_element_row() {
    let source = r#"
        record Curve
            parameter Real eta[:];
        end Curve;

        record Performance
            parameter Curve motorEfficiency(eta={1.0});
        end Performance;

        model Pump
            parameter Performance per;
            Real y;
        equation
            y = per.motorEfficiency.eta[1];
        end Pump;

        model PumpSystem
            parameter Integer n = 3;
            parameter Real Motor_eta[n, 2] = {
                {0.87, 0.88},
                {0.77, 0.78},
                {0.67, 0.68}};
            Pump pumConSpe[n](per(motorEfficiency(eta=Motor_eta)));
        end PumpSystem;

        model Top
            PumpSystem system;
            Real y;
        equation
            y = system.pumConSpe[1].y;
        end Top;
    "#;

    let compiled = rumoca::Compiler::new()
        .model("Top")
        .compile_str(source, "test.mo")
        .expect("nested attribute modifier on array component should select one row");

    let eta = compiled
        .flat
        .variables
        .get(&rumoca_core::VarName::new(
            "system.pumConSpe[1].per.motorEfficiency.eta",
        ))
        .expect("first pump motor efficiency eta should be in flat variables");
    assert_eq!(
        eta.dims,
        vec![2],
        "first pump motor efficiency eta should keep one selected row"
    );
    match eta
        .binding
        .as_ref()
        .expect("first pump motor efficiency eta should keep a flat binding")
    {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            assert_eq!(name.as_str(), "system.Motor_eta");
            assert!(
                matches!(
                    subscripts.as_slice(),
                    [rumoca_core::Subscript::Index { value: 1, .. }]
                ),
                "nested attribute modifier should select first source row, got {subscripts:?}"
            );
        }
        other => panic!("expected selected source row binding, got {other:?}"),
    }
}

#[test]
fn test_forwarded_parent_parameter_remains_visible_to_child_modifier_rhs() {
    let source = r#"
        model Damper
            parameter Real dpValve_nominal;
            Real y;
        equation
            y = dpValve_nominal;
        end Damper;

        model Terminal
            parameter Real PreDroAir;
            Damper dam(dpValve_nominal=PreDroAir);
            Real y;
        equation
            y = dam.y;
        end Terminal;

        model FiveZone
            parameter Real PreDroAir1;
            Terminal vAV1(PreDroAir=PreDroAir1);
            Real y;
        equation
            y = vAV1.y;
        end FiveZone;

        model Floor
            parameter Real PreDroAir1;
            FiveZone fivZonVAV(PreDroAir1=PreDroAir1);
            Real y;
        equation
            y = fivZonVAV.y;
        end Floor;

        model Wrapper
            parameter Real PreDroAir[5] = {200, 124, 124, 124, 124};
            Floor floor(PreDroAir1=PreDroAir[1]);
            Real y;
        equation
            y = floor.y;
        end Wrapper;
    "#;

    let compiled = rumoca::Compiler::new()
        .model("Wrapper")
        .compile_str(source, "test.mo")
        .expect("forwarded parent parameter should remain visible to child modifier RHS");

    let pre_dro_air = compiled
        .flat
        .variables
        .get(&rumoca_core::VarName::new("floor.fivZonVAV.vAV1.PreDroAir"))
        .expect("terminal PreDroAir should be in flat variables");
    assert!(
        pre_dro_air.binding.is_some(),
        "terminal PreDroAir should keep the forwarded binding"
    );

    let dp = compiled
        .flat
        .variables
        .get(&rumoca_core::VarName::new(
            "floor.fivZonVAV.vAV1.dam.dpValve_nominal",
        ))
        .expect("damper dpValve_nominal should be in flat variables");
    assert!(
        dp.binding.is_some(),
        "damper dpValve_nominal should bind through terminal PreDroAir"
    );
    match dp
        .binding
        .as_ref()
        .expect("damper dpValve_nominal should keep a flat binding")
    {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            assert_eq!(name.as_str(), "floor.fivZonVAV.vAV1.PreDroAir");
            assert!(
                subscripts.is_empty(),
                "scalar forwarded parameter must not receive an array-element subscript, got {subscripts:?}"
            );
        }
        other => {
            panic!("expected damper dpValve_nominal to reference terminal PreDroAir, got {other:?}")
        }
    }
}

#[test]
fn test_array_element_local_vector_modifier_rhs_is_not_indexed_again() {
    let source = r#"
        record PressureCurve
            parameter Real V_flow[:];
        end PressureCurve;

        record Performance
            parameter PressureCurve pressure;
        end Performance;

        model FlowMover
            parameter Performance per;
            Real y;
        equation
            y = per.pressure.V_flow[1];
        end FlowMover;

        model Pump
            parameter Real VolFloCur[:];
            FlowMover varSpeFloMov(per(pressure(V_flow=VolFloCur)));
            Real y;
        equation
            y = varSpeFloMov.y;
        end Pump;

        model PumpSystem
            parameter Integer n = 2;
            parameter Real VolFloCur[n, 3] = {
                {0.1, 0.2, 0.3},
                {1.1, 1.2, 1.3}};
            Pump pum[n](VolFloCur=VolFloCur);
        end PumpSystem;

        model Top
            PumpSystem system;
            Real y;
        equation
            y = system.pum[1].y;
        end Top;
    "#;

    let compiled = rumoca::Compiler::new()
        .model("Top")
        .compile_str(source, "test.mo")
        .expect("array element local vector modifier RHS should keep vector shape");

    let v_flow = compiled
        .flat
        .variables
        .get(&rumoca_core::VarName::new(
            "system.pum[1].varSpeFloMov.per.pressure.V_flow",
        ))
        .expect("pressure V_flow should be in flat variables");
    assert_eq!(v_flow.dims, vec![3]);
    match v_flow
        .binding
        .as_ref()
        .expect("pressure V_flow should keep a binding")
    {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            assert_eq!(name.as_str(), "system.pum[1].VolFloCur");
            assert!(
                subscripts.is_empty(),
                "local vector parameter must not receive outer array element subscript, got {subscripts:?}"
            );
        }
        other => panic!("expected pressure V_flow to reference local VolFloCur, got {other:?}"),
    }
}

#[test]
fn test_array_element_colon_parameter_uses_local_binding_shape() {
    let source = r#"
        model Base
            parameter Real stageInputs[:];
        end Base;

        model Flow
            extends Base(stageInputs=massFlowRates);
            parameter Real m_flow_nominal;
            parameter Real per_speeds[1] = {1};
            parameter Real massFlowRates[:] =
                m_flow_nominal * {per_speeds[i] / per_speeds[end] for i in 1:size(per_speeds, 1)};
        end Flow;

        model System
            parameter Integer n = 3;
            parameter Real m_flow_nominal[n] = {2, 3, 4};
            Flow pump[n](m_flow_nominal=m_flow_nominal);
        end System;

        model Top
            System system;
        end Top;
    "#;

    let compiled = rumoca::Compiler::new()
        .model("Top")
        .compile_str(source, "test.mo")
        .expect("array element colon parameter should infer shape from local binding");

    for name in ["system.pump[1].stageInputs", "system.pump[1].massFlowRates"] {
        let var = compiled
            .flat
            .variables
            .get(&rumoca_core::VarName::new(name))
            .unwrap_or_else(|| panic!("{name} should be in flat variables"));
        assert_eq!(
            var.dims,
            vec![1],
            "{name} should use the local per_speeds binding shape, not the parent pump array length"
        );
    }
}

/// Test that typecheck_instanced evaluates dimensions correctly.
#[test]
fn test_dimension_evaluation_after_typecheck() {
    let source = r#"
        model SubModel
            parameter Integer n = 1;
            Real x[n];
        end SubModel;

        model Container
            parameter Integer n = 1;
            SubModel sub(n=n);
            Real y[n];
        end Container;

        model Test
            Container cont(n=2);
        end Test;
    "#;

    let stored_def = parse_to_ast(source, "<test>").expect("parse failed");
    let mut tree = ast::ClassTree::from_parsed(stored_def);
    tree.source_map.add("<test>", source);
    let parsed = ast::ParsedTree::new(tree);
    let resolved = resolve(parsed).expect("resolve failed");
    let tree = resolved.into_inner();

    let mut overlay = match instantiate_model_with_outcome(&tree, "Test") {
        InstantiationOutcome::Success(o) => o,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("Needs inner: {:?}", missing_inners);
        }
        InstantiationOutcome::Error(e) => {
            panic!("Error: {:?}", e);
        }
    };

    // Before typecheck, dimensions may still be symbolic (dims_expr) or already
    // concretized by instantiation-time modifier propagation.
    let sub_x_before = overlay
        .components
        .values()
        .find(|d| d.qualified_name.to_flat_string() == "cont.sub.x")
        .expect("cont.sub.x should exist");
    println!("\nBefore typecheck:");
    println!(
        "  cont.sub.x dims: {:?}, dims_expr_len: {}",
        sub_x_before.dims,
        sub_x_before.dims_expr.len()
    );
    assert!(
        (sub_x_before.dims.is_empty() && !sub_x_before.dims_expr.is_empty())
            || sub_x_before.dims == vec![2],
        "expected symbolic dims_expr or pre-evaluated dims=[2] before typecheck"
    );

    // Run typecheck_instanced
    let result = typecheck_instanced(&tree, &mut overlay, "");
    if let Err(diags) = result {
        println!("\nTypecheck errors:");
        for d in diags.iter() {
            println!("  {}", d.message);
        }
    }

    // After typecheck: check dimensions
    println!("\n=== After typecheck ===");
    for (_def_id, data) in &overlay.components {
        let name = data.qualified_name.to_flat_string();
        if name.ends_with(".x") || name.ends_with(".y") {
            println!(
                "  {}: dims={:?}, dims_expr_len={}",
                name,
                data.dims,
                data.dims_expr.len()
            );
        }
    }

    // Check that dimensions are evaluated correctly
    let sub_x = overlay
        .components
        .values()
        .find(|d| d.qualified_name.to_flat_string() == "cont.sub.x")
        .expect("cont.sub.x should exist");
    assert_eq!(sub_x.dims, vec![2], "cont.sub.x should have dimension [2]");

    let cont_y = overlay
        .components
        .values()
        .find(|d| d.qualified_name.to_flat_string() == "cont.y")
        .expect("cont.y should exist");
    assert_eq!(cont_y.dims, vec![2], "cont.y should have dimension [2]");
}

#[test]
fn test_nested_colon_parameter_binding_drives_size_dimension() {
    let source = r#"
        package P
            package Conversions
                function from_deg
                    input Real degree;
                    output Real radian;
                algorithm
                    radian := degree;
                end from_deg;
            end Conversions;

            block Kinematic
                parameter Real q_begin[:] = {0};
                parameter Real q_end[:] = {1};
                parameter Real qd_max[:] = {1};
                parameter Real qdd_max[:] = {1};
                final parameter Integer nout =
                    max([size(q_begin, 1); size(q_end, 1); size(qd_max, 1); size(qdd_max, 1)]);
                Real q[nout];
            end Kinematic;

            model PathPlanning
                import Cv = P.Conversions;
                parameter Integer naxis = 6;
                parameter Real angleBegDeg[naxis] = zeros(naxis);
                final parameter Real angleBeg[:] = Cv.from_deg(angleBegDeg);
                Kinematic path(
                    q_begin = angleBeg,
                    q_end = angleBeg,
                    qd_max = angleBeg,
                    qdd_max = angleBeg);
            end PathPlanning;

            model Test
                PathPlanning pathPlanning(
                    naxis = 6,
                    angleBegDeg = {1, 2, 3, 4, 5, 6});
            end Test;
        end P;
    "#;

    let stored_def = parse_to_ast(source, "<test>").expect("parse failed");
    let mut tree = ast::ClassTree::from_parsed(stored_def);
    tree.source_map.add("<test>", source);
    let parsed = ast::ParsedTree::new(tree);
    let resolved = resolve(parsed).expect("resolve failed");
    let tree = resolved.into_inner();
    let mut overlay = match instantiate_model_with_outcome(&tree, "P.Test") {
        InstantiationOutcome::Success(o) => o,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("Needs inner: {:?}", missing_inners);
        }
        InstantiationOutcome::Error(e) => {
            panic!("Error: {:?}", e);
        }
    };

    typecheck_instanced(&tree, &mut overlay, "P.Test").expect("typecheck should succeed");

    let q = find_component(&overlay, "pathPlanning.path.q").expect("path.q should exist");
    assert_eq!(q.dims, vec![6]);
}

/// Test that mimics the DFFREG structure more closely
/// DFFREG has: parameter n, dataIn[n], and DFFR dFFR(n=n) where DFFR also has dataIn[n]
#[test]
fn test_dffreg_like_structure() {
    // Mimics: DFFR has dataIn[n], DFFREG has dataIn[n] and DFFR dFFR(n=n), Example has DFFREG(n=2)
    let source = r#"
        connector DataIn = input Real;
        model DFFR
            parameter Integer n = 1;
            DataIn dataIn[n];
        end DFFR;
        model DFFREG
            parameter Integer n = 1;
            DataIn dataIn[n];
            DFFR dFFR(n=n);
        end DFFREG;
        model Example
            DFFREG dFFREG(n=2);
        end Example;
    "#;

    let (tree, mut overlay) = instantiate_test_model(source, "Example");

    // Debug output
    print_components_matching(&overlay, |name| {
        name.contains(".n") || name.ends_with("dataIn") || name.contains("dataIn[")
    });

    // Check binding propagation
    assert!(
        find_component(&overlay, "dFFREG.n")
            .unwrap()
            .binding
            .is_some()
    );
    assert_integer_binding(&overlay, "dFFREG.dFFR.n", "2");

    // Run typecheck and verify dimensions
    let _ = typecheck_instanced(&tree, &mut overlay, "");
    assert_dims(&overlay, "dFFREG.dataIn", &[2]);
    assert_dims(&overlay, "dFFREG.dFFR.dataIn", &[2]);
}

#[test]
fn test_inherited_component_start_modifier_from_extends_is_preserved() {
    let source = r#"
        connector BooleanInput = input Boolean;
        connector BooleanOutput = output Boolean;

        partial block PartialClockedSISO
            BooleanInput u;
            BooleanOutput y;
        end PartialClockedSISO;

        block UnitDelay
            extends PartialClockedSISO(u(final start = y_start));
            parameter Boolean y_start = true;
        equation
            y = previous(u);
        end UnitDelay;

        model Top
            UnitDelay delay;
        end Top;
    "#;

    let (_tree, overlay) = instantiate_test_model(source, "Top");
    let input = find_component(&overlay, "delay.u").expect("delay.u should be instantiated");
    let start = input
        .start
        .as_ref()
        .expect("extends modifier must preserve inherited input start attribute");

    match start {
        ast::Expression::ComponentReference(reference) => {
            assert_eq!(reference.to_string(), "y_start");
        }
        other => panic!("expected start to reference y_start, got {other:?}"),
    }
}

#[test]
fn test_nested_modifier_name_collision_prefers_local_component_mod() {
    // Reproduces the BatteryDischargeCharge pattern:
    // parent has `useHeatPort=false`, while nested component declares
    // `useHeatPort=true` in its own modifier list.
    let source = r#"
        model Child
            parameter Boolean useHeatPort = false;
        end Child;

        model Parent
            parameter Boolean useHeatPort = false;
            Child c(useHeatPort=true);
        end Parent;

        model Top
            Parent p(useHeatPort=false);
        end Top;
    "#;

    let (_tree, overlay) = instantiate_test_model(source, "Top");

    assert_bool_binding(&overlay, "p.useHeatPort", "false");
    assert_bool_binding(&overlay, "p.c.useHeatPort", "true");
}

#[test]
fn test_nested_boolean_modifier_resolves_in_parent_scope_for_conditionals() {
    // Reproduces the Electrical PowerConverters pattern:
    // outer component sets `useFilter=false` and nested class forwards
    // with `final useFilter=useFilter`. The nested conditional component
    // must see `false` and be disabled.
    let source = r#"
        model Filter
            Real u;
            Real y;
        equation
            y = u;
        end Filter;

        model PassThrough
            Real u;
            Real y;
        equation
            y = u;
        end PassThrough;

        model Signal2mPulse
            parameter Boolean useFilter = true;
            Filter filter if useFilter;
            PassThrough pass if not useFilter;
        end Signal2mPulse;

        model Wrapper
            parameter Boolean useFilter = true;
            Signal2mPulse twoPulse(final useFilter=useFilter);
        end Wrapper;

        model Top
            Wrapper p(useFilter=false);
        end Top;
    "#;

    let (_tree, overlay) = instantiate_test_model(source, "Top");

    assert_bool_binding(&overlay, "p.useFilter", "false");
    assert_bool_binding(&overlay, "p.twoPulse.useFilter", "false");

    assert!(
        disabled_contains(&overlay, "p.twoPulse.filter"),
        "filter should be disabled when useFilter resolves to false"
    );
    assert!(
        !disabled_contains(&overlay, "p.twoPulse.pass"),
        "pass should stay enabled when not useFilter is true"
    );

    let mut has_filter_members = false;
    let mut has_pass_members = false;
    for data in overlay.components.values() {
        let name = data.qualified_name.to_flat_string();
        if name.starts_with("p.twoPulse.filter.") {
            has_filter_members = true;
        }
        if name.starts_with("p.twoPulse.pass.") {
            has_pass_members = true;
        }
    }

    assert!(
        !has_filter_members,
        "disabled conditional component members should not be instantiated"
    );
    assert!(
        has_pass_members,
        "enabled alternative conditional component should be instantiated"
    );
}

#[test]
fn test_fill_modifier_resolves_forwarded_boolean_for_array_components() {
    // Regression for Polyphase-style modifiers:
    // `final useHeatPort=fill(useHeatPort, m)` on arrayed components must
    // resolve the forwarded parent boolean for each scalar element.
    let source = r#"
        connector HeatPort
            Real T;
            flow Real Q_flow;
        end HeatPort;

        model Leaf
            parameter Boolean useHeatPort = false;
            HeatPort heatPort if useHeatPort;
        end Leaf;

        model Wrapper
            parameter Integer m = 3;
            parameter Boolean useHeatPort = true;
            Leaf leaf[m](final useHeatPort=fill(useHeatPort, m));
        end Wrapper;

        model Top
            Wrapper w(useHeatPort=true, m=3);
        end Top;
    "#;

    let (_tree, overlay) = instantiate_test_model(source, "Top");

    assert_bool_binding(&overlay, "w.useHeatPort", "true");
    assert_bool_binding(&overlay, "w.leaf[1].useHeatPort", "true");
    assert_bool_binding(&overlay, "w.leaf[2].useHeatPort", "true");
    assert_bool_binding(&overlay, "w.leaf[3].useHeatPort", "true");

    for i in 1..=3 {
        let heat_port = format!("w.leaf[{i}].heatPort");
        let heat_port_t = format!("{heat_port}.T");
        assert!(
            !disabled_contains(&overlay, &heat_port),
            "{heat_port} should stay enabled"
        );
        assert!(
            find_component(&overlay, &heat_port_t).is_some(),
            "{heat_port_t} should be instantiated"
        );
    }
}

#[test]
fn test_nested_string_modifier_resolves_in_parent_scope_for_conditionals() {
    // Reproduces TerminalBox-style pattern:
    // terminalConnection is forwarded from a sibling record field
    // and drives conditional components with string comparisons.
    let source = r#"
        model Star
            Real v;
        equation
            v = 1.0;
        end Star;

        model Delta
            Real v;
        equation
            v = 2.0;
        end Delta;

        record Settings
            parameter String layout = "Y3";
            parameter String terminalConnection =
                if (layout == "Y3" or layout == "Y2") then "Y" else "D";
        end Settings;

        model TerminalBoxLike
            parameter Settings settings;
            parameter String terminalConnection = settings.terminalConnection;
            Star star if terminalConnection <> "D";
            Delta delta if terminalConnection == "D";
        end TerminalBoxLike;

        model Top
            TerminalBoxLike box(settings(layout="D3"));
        end Top;
    "#;

    let (_tree, overlay) = instantiate_test_model(source, "Top");

    // For layout = "D3", terminalConnection resolves to "D":
    // star must be disabled and delta must be enabled.
    assert!(
        disabled_contains(&overlay, "box.star"),
        "star should be disabled when terminalConnection resolves to D"
    );
    assert!(
        !disabled_contains(&overlay, "box.delta"),
        "delta should stay enabled when terminalConnection resolves to D"
    );

    let mut has_star_members = false;
    let mut has_delta_members = false;
    for data in overlay.components.values() {
        let name = data.qualified_name.to_flat_string();
        if name.starts_with("box.star.") {
            has_star_members = true;
        }
        if name.starts_with("box.delta.") {
            has_delta_members = true;
        }
    }

    assert!(
        !has_star_members,
        "disabled string-conditional branch should not be instantiated"
    );
    assert!(
        has_delta_members,
        "enabled string-conditional branch should be instantiated"
    );
}

#[test]
fn test_nested_enum_modifier_resolves_forwarded_parameter_for_conditionals() {
    // Reproduces nested forwarding like resolveInFrame=resolveInFrame:
    // the child conditional must evaluate against the forwarded parent enum value.
    let source = r#"
        type Mode = enumeration(a, c);

        model PathA
            Real y;
        equation
            y = 1.0;
        end PathA;

        model PathC
            Real y;
        equation
            y = 2.0;
        end PathC;

        model Child
            parameter Mode mode = Mode.a;
            PathA path_a if mode == Mode.a;
            PathC path_c if mode == Mode.c;
        end Child;

        model Wrapper
            parameter Mode mode = Mode.a;
            Child child(final mode=mode);
        end Wrapper;

        model Top
            Wrapper w(mode=Mode.c);
        end Top;
    "#;

    let (_tree, overlay) = instantiate_test_model(source, "Top");

    assert!(
        disabled_contains(&overlay, "w.child.path_a"),
        "path_a should be disabled when forwarded mode resolves to Mode.c"
    );
    assert!(
        !disabled_contains(&overlay, "w.child.path_c"),
        "path_c should stay enabled when forwarded mode resolves to Mode.c"
    );

    let mut has_path_a_members = false;
    let mut has_path_c_members = false;
    for data in overlay.components.values() {
        let name = data.qualified_name.to_flat_string();
        if name.starts_with("w.child.path_a.") {
            has_path_a_members = true;
        }
        if name.starts_with("w.child.path_c.") {
            has_path_c_members = true;
        }
    }

    assert!(
        !has_path_a_members,
        "disabled enum-conditional branch should not be instantiated"
    );
    assert!(
        has_path_c_members,
        "enabled enum-conditional branch should be instantiated"
    );
}

#[test]
fn test_constrainedby_mod_survives_redeclare_for_replaceable_component() {
    // Mirrors Clocked logical clock pattern:
    // replaceable C c constrainedby C(n=n), then redeclare c to a derived type.
    // The constraining-clause mod (n=n) must still configure the redeclared type.
    let source = r#"
        model BaseComb
            parameter Integer n = 0;
            Real u[n];
        end BaseComb;

        model AndComb
            extends BaseComb;
        end AndComb;

        partial model PartialLogical
            parameter Integer n = 2;
            replaceable BaseComb comb constrainedby BaseComb(n=n);
        end PartialLogical;

        model Conj
            extends PartialLogical(redeclare AndComb comb);
        end Conj;

        model Top
            Conj p;
        end Top;
    "#;

    let (tree, mut overlay) = instantiate_test_model(source, "Top");
    let _ = typecheck_instanced(&tree, &mut overlay, "");

    assert_integer_binding(&overlay, "p.comb.n", "2");
    assert_dims(&overlay, "p.comb.u", &[2]);
}

#[test]
fn test_record_constructor_defaults_preserve_colon_dimension_bindings() {
    let source = r#"
        record BaseData
            parameter Real[:,:] tabris = [1, 0; 2, 1];
        end BaseData;

        block TableUse
            parameter Real table[:, :] = fill(0.0, 0, 2);
            parameter Integer columns[:] = 2:size(table, 2);
            parameter Integer n = size(columns, 1);
            input Real u[n];
            output Real y[n];
        equation
            for i in 1:n loop
                y[i] = u[i];
            end for;
        end TableUse;

        model Top
            parameter BaseData mat = BaseData();
            TableUse tab(table = mat.tabris);
        equation
            tab.u[1] = 0;
        end Top;
    "#;

    let (tree, mut overlay) = instantiate_test_model(source, "Top");
    typecheck_instanced(&tree, &mut overlay, "")
        .expect("typecheck should infer dimensions through default record constructor fields");

    assert_dims(&overlay, "mat.tabris", &[2, 2]);
    assert_dims(&overlay, "tab.table", &[2, 2]);
    assert_dims(&overlay, "tab.columns", &[1]);
    assert_dims(&overlay, "tab.u", &[1]);
    assert_dims(&overlay, "tab.y", &[1]);
}

#[test]
fn test_record_binding_modifier_keeps_reference_instead_of_collapsing_to_constructor() {
    let source = r#"
        record CoreParameters
            parameter Integer m;
            parameter Real PRef = 0;
            parameter Real VRef = 100;
            parameter Real wRef = 1;
        end CoreParameters;

        record Data
            parameter Integer m = 3;
            parameter Real fsNominal = 50;
            parameter CoreParameters statorCoreParameters(
                m = m,
                PRef = 0,
                VRef = 100,
                wRef = 2 * 3.141592653589793 * fsNominal);
        end Data;

        model Machine
            parameter Integer m = 3;
            parameter Real fsNominal = 50;
            parameter CoreParameters statorCoreParameters(
                m = m,
                PRef = 0,
                VRef = 100,
                wRef = 2 * 3.141592653589793 * fsNominal);
        end Machine;

        model Top
            parameter Data aimcData(statorCoreParameters(PRef = 410, VRef = 387.9));
            Machine aimc(statorCoreParameters = aimcData.statorCoreParameters);
        end Top;
    "#;

    let (_tree, overlay) = instantiate_test_model(source, "Top");
    let comp = find_component(&overlay, "aimc.statorCoreParameters")
        .expect("aimc.statorCoreParameters should exist");
    let binding = comp
        .binding
        .as_ref()
        .expect("aimc.statorCoreParameters should have binding");

    let ast::Expression::ComponentReference(cref) = binding else {
        panic!(
            "expected component reference binding, got {:?}",
            comp.binding.as_ref()
        );
    };
    assert_eq!(
        cref.to_string(),
        "aimcData.statorCoreParameters",
        "record binding modifier should preserve record reference"
    );
}

#[test]
fn test_nested_record_parameter_alias_preserves_child_field_bindings() {
    let source = r#"
        record R
            parameter Real a = 1;
            parameter Real b = 2;
        end R;

        model Child
            parameter R rp;
        end Child;

        model Parent
            parameter R rp(a = 3, b = 4);
            Child c(final rp = rp);
        end Parent;

        model Top
            Parent p;
        end Top;
    "#;

    let compiled = rumoca::Compiler::new()
        .model("Top")
        .compile_str(source, "test.mo")
        .expect("Top should compile");

    let unbound = compiled.flat.unbound_fixed_parameters();
    assert!(
        !unbound
            .iter()
            .any(|name| name.as_str().starts_with("p.c.rp.")),
        "child record fields should not be unbound fixed parameters; unbound={:?}",
        unbound
    );

    let child_a = compiled
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "p.c.rp.a")
        .and_then(|(_, var)| var.binding.as_ref())
        .expect("p.c.rp.a should have binding");
    let child_b = compiled
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "p.c.rp.b")
        .and_then(|(_, var)| var.binding.as_ref())
        .expect("p.c.rp.b should have binding");

    assert!(
        flat_expr_mentions_name(child_a, "p.rp.a") || flat_expr_is_numeric_value(child_a, 3),
        "unexpected binding for p.c.rp.a: {:?}",
        child_a
    );
    assert!(
        flat_expr_mentions_name(child_b, "p.rp.b") || flat_expr_is_numeric_value(child_b, 4),
        "unexpected binding for p.c.rp.b: {:?}",
        child_b
    );
}

#[test]
fn test_nested_modifier_forwarding_keeps_outer_alias_scope() {
    let source = r#"
        record FrictionParameters
            parameter Real wRef = 1;
        end FrictionParameters;

        record Data
            parameter FrictionParameters frictionParameters(wRef = 3);
        end Data;

        model Friction
            parameter FrictionParameters frictionParameters;
        end Friction;

        model Machine
            parameter FrictionParameters frictionParameters;
            Friction friction(frictionParameters = frictionParameters);
        end Machine;

        model Top
            parameter Data data;
            Machine mach(frictionParameters = data.frictionParameters);
        end Top;
    "#;

    let compiled = rumoca::Compiler::new()
        .model("Top")
        .compile_str(source, "test.mo")
        .expect("Top should compile");

    let binding = compiled
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "mach.friction.frictionParameters.wRef")
        .and_then(|(_, var)| var.binding.as_ref())
        .expect("mach.friction.frictionParameters.wRef should have binding");

    assert!(
        !flat_expr_mentions_name(binding, "mach.data.frictionParameters"),
        "nested modifier forwarding should not synthesize non-existent local alias path; binding={binding:?}"
    );
    assert!(
        flat_expr_mentions_name(binding, "data.frictionParameters")
            || flat_expr_is_numeric_value(binding, 3),
        "nested modifier forwarding should preserve outer alias source; binding={binding:?}"
    );

    let unbound = compiled.flat.unbound_fixed_parameters();
    assert!(
        !unbound
            .iter()
            .any(|name| name.as_str() == "mach.friction.frictionParameters.wRef"),
        "forwarded nested record field should not be unbound fixed parameter; unbound={unbound:?}"
    );
}

#[test]
fn test_nested_component_record_modifier_resolves_sibling_alias_scope() {
    let source = r#"
        record CoreParameters
            parameter Real p = 1;
        end CoreParameters;

        record Data
            parameter CoreParameters core;
        end Data;

        model Core
            parameter CoreParameters coreParameters;
            parameter Real use = coreParameters.p;
        end Core;

        partial model PartialMachine
            parameter CoreParameters coreParameters;
            Core core(final coreParameters = coreParameters);
        end PartialMachine;

        model Machine
            extends PartialMachine;
        end Machine;

        model Motor
            parameter Data data;
            Machine machine(coreParameters = data.core);
        end Motor;

        model Top
            parameter Data data(core(p = 3));
            Motor motor(data = data);
        end Top;
    "#;

    let compiled = rumoca::Compiler::new()
        .model("Top")
        .compile_str(source, "test.mo")
        .expect("Top should compile");

    let binding = compiled
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "motor.machine.core.coreParameters.p")
        .and_then(|(_, var)| var.binding.as_ref())
        .expect("motor.machine.core.coreParameters.p should have binding");

    assert!(
        !flat_expr_mentions_name(binding, "motor.machine.data.core"),
        "inherited nested record modifier must not be scoped under the machine component; binding={binding:?}"
    );
    assert!(
        !flat_expr_mentions_name(binding, "motor.machine.coreParameters"),
        "inherited nested record modifier must not leave an intermediate record alias for DAE lowering; binding={binding:?}"
    );
    assert!(
        flat_expr_mentions_name(binding, "motor.data.core")
            || flat_expr_mentions_name(binding, "data.core")
            || flat_expr_is_numeric_value(binding, 3),
        "inherited nested record modifier should resolve through the outer sibling alias scope; binding={binding:?}"
    );

    rumoca_phase_dae::to_dae(&compiled.flat).expect("Top should lower to DAE");
}

/// Helper: Parse source and instantiate a model.
fn instantiate_test_model(
    source: &str,
    model_name: &str,
) -> (ast::ClassTree, ast::InstanceOverlay) {
    let stored_def = parse_to_ast(source, "<test>").expect("parse failed");
    let mut tree = ast::ClassTree::from_parsed(stored_def);
    tree.source_map.add("<test>", source);
    let parsed = ast::ParsedTree::new(tree);
    let resolved = resolve(parsed).expect("resolve failed");
    let tree = resolved.into_inner();

    let overlay = match instantiate_model_with_outcome(&tree, model_name) {
        InstantiationOutcome::Success(o) => o,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("Needs inner: {:?}", missing_inners);
        }
        InstantiationOutcome::Error(e) => {
            panic!("Error: {:?}", e);
        }
    };
    (tree, overlay)
}

/// Helper: Print components matching a predicate.
fn print_components_matching<F>(overlay: &ast::InstanceOverlay, predicate: F)
where
    F: Fn(&str) -> bool,
{
    println!("\n=== Component bindings ===");
    for (_def_id, data) in &overlay.components {
        let name = data.qualified_name.to_flat_string();
        if predicate(&name) {
            println!(
                "  {}: binding={:?}, dims={:?}",
                name,
                data.binding.as_ref().map(|_| "..."),
                data.dims
            );
        }
    }
}
