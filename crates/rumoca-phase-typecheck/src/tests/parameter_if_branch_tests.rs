//! MLS §8.3.4: a parameter if-equation contributes only the equations of the
//! branch selected at translation time, so the branches that are not selected
//! are removed before the array-bounds rule of MLS §10.5.1 applies to them.
use super::*;

fn parsed_tree(source: &str) -> ClassTree {
    let parsed = parse(source);
    resolve(parsed)
        .expect("resolve should succeed")
        .into_inner()
}

fn overlay_for(tree: &ClassTree, model_name: &str, component_names: &[&str]) -> InstanceOverlay {
    let model = tree
        .get_class_by_qualified_name(model_name)
        .expect("model class");
    let mut overlay = InstanceOverlay::new();
    for name in component_names {
        add_instanced_component(
            &mut overlay,
            &format!("{model_name}.{name}"),
            model.components.get(*name).expect("model component"),
            true,
        );
    }
    overlay
}

#[test]
fn unselected_parameter_if_branch_is_not_bounds_checked() {
    let source = r#"
        model Test
            parameter Integer n = 1;
            Real x[n];
            Real y;
        equation
            x[1] = 1.0;
            if n == 1 then
                y = x[1];
            else
                y = x[2];
            end if;
        end Test;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = overlay_for(&tree, "Test", &["n", "x", "y"]);

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("a branch removed at translation time carries no subscript obligation");
}

#[test]
fn selected_parameter_if_branch_is_still_bounds_checked() {
    let source = r#"
        model Test
            parameter Integer n = 1;
            Real x[n];
            Real y;
        equation
            x[1] = 1.0;
            if n == 1 then
                y = x[2];
            else
                y = x[1];
            end if;
        end Test;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = overlay_for(&tree, "Test", &["n", "x", "y"]);

    let diagnostics = typecheck_instanced(&tree, &mut overlay, "Test")
        .expect_err("the selected branch is part of the model and keeps its bounds obligation");
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ET009")),
        "expected ET009 for the selected branch, got: {diagnostics:?}"
    );
}

#[test]
fn simulation_time_if_condition_keeps_every_branch_checked() {
    let source = r#"
        model Test
            Real level;
            Real x[1];
            Real y;
        equation
            level = 1.0;
            x[1] = 1.0;
            if level > 0.0 then
                y = x[1];
            else
                y = x[2];
            end if;
        end Test;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = overlay_for(&tree, "Test", &["level", "x", "y"]);

    let diagnostics = typecheck_instanced(&tree, &mut overlay, "Test")
        .expect_err("a condition that varies at simulation time keeps both branches in the model");
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ET009")),
        "expected ET009 for the non-parameter if-equation, got: {diagnostics:?}"
    );
}
