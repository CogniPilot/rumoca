//! Test modification propagation through nested components.
//!
//! This tests the pattern used in DFFREG: dFFR(n=n) should get n=2 when outer has n=2.

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
    let tree = ast::ClassTree::from_parsed(stored_def);
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
    let tree = ast::ClassTree::from_parsed(stored_def);
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
        overlay.disabled_components.contains("p.twoPulse.filter"),
        "filter should be disabled when useFilter resolves to false"
    );
    assert!(
        !overlay.disabled_components.contains("p.twoPulse.pass"),
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
            !overlay.disabled_components.contains(&heat_port),
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
        overlay.disabled_components.contains("box.star"),
        "star should be disabled when terminalConnection resolves to D"
    );
    assert!(
        !overlay.disabled_components.contains("box.delta"),
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
        overlay.disabled_components.contains("w.child.path_a"),
        "path_a should be disabled when forwarded mode resolves to Mode.c"
    );
    assert!(
        !overlay.disabled_components.contains("w.child.path_c"),
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
        .map(|expr| format!("{expr:?}"))
        .expect("p.c.rp.a should have binding");
    let child_b = compiled
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "p.c.rp.b")
        .and_then(|(_, var)| var.binding.as_ref())
        .map(|expr| format!("{expr:?}"))
        .expect("p.c.rp.b should have binding");

    assert!(
        child_a.contains("p.rp.a")
            || child_a.contains("Literal(Real(3.0))")
            || child_a.contains("Literal(Integer(3))"),
        "unexpected binding for p.c.rp.a: {}",
        child_a
    );
    assert!(
        child_b.contains("p.rp.b")
            || child_b.contains("Literal(Real(4.0))")
            || child_b.contains("Literal(Integer(4))"),
        "unexpected binding for p.c.rp.b: {}",
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
        .map(|expr| format!("{expr:?}"))
        .expect("mach.friction.frictionParameters.wRef should have binding");

    assert!(
        !binding.contains("mach.data.frictionParameters"),
        "nested modifier forwarding should not synthesize non-existent local alias path; binding={binding}"
    );
    assert!(
        binding.contains("data.frictionParameters")
            || binding.contains("Literal(Real(3.0))")
            || binding.contains("Literal(Integer(3))"),
        "nested modifier forwarding should preserve outer alias source; binding={binding}"
    );

    let unbound = compiled.flat.unbound_fixed_parameters();
    assert!(
        !unbound
            .iter()
            .any(|name| name.as_str() == "mach.friction.frictionParameters.wRef"),
        "forwarded nested record field should not be unbound fixed parameter; unbound={unbound:?}"
    );
}

/// Helper: Parse source and instantiate a model.
fn instantiate_test_model(
    source: &str,
    model_name: &str,
) -> (ast::ClassTree, ast::InstanceOverlay) {
    let stored_def = parse_to_ast(source, "<test>").expect("parse failed");
    let tree = ast::ClassTree::from_parsed(stored_def);
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
