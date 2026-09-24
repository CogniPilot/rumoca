//! Component-modifier diagnostics on the class tree: unknown builtin,
//! nested, and class-member modifier names, and modifier value types.

use super::*;

fn inherited_replaceable_port_source(member: &str) -> String {
    format!(
        r#"
        package P
            package Types
                type AbsolutePressure = Real;
            end Types;

            partial package BaseMedium
                extends Types;
            end BaseMedium;

            connector FluidPort
                replaceable package Medium = BaseMedium;
                Medium.AbsolutePressure p;
            end FluidPort;

            model Test
                FluidPort port;
            equation
                port.{member} = 1;
            end Test;
        end P;
    "#
    )
}

#[test]
fn inherited_replaceable_package_type_is_available_to_connector_members() {
    let resolved =
        resolve(parse(&inherited_replaceable_port_source("p"))).expect("resolve should succeed");
    let mut instanced = rumoca_phase_instantiate::instantiate(resolved, "P.Test")
        .expect("instantiation should succeed");
    typecheck_instanced(&instanced.tree, &mut instanced.overlay, "P.Test")
        .expect("inherited connector member type should resolve");
}

#[test]
fn inherited_replaceable_package_unknown_connector_member_is_rejected() {
    let diagnostics = resolve(parse(&inherited_replaceable_port_source("missing")))
        .expect_err("an unknown connector member must remain rejected");
    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ER002")
                && diagnostic.message.contains("port.missing")
        }),
        "expected unknown member diagnostic, got {diagnostics:?}"
    );
}

#[test]
fn dotted_member_without_anchor_is_rejected_by_identity_resolver() {
    let resolved = resolve(parse(&inherited_replaceable_port_source("p")))
        .expect("resolve should succeed")
        .into_inner();
    let name = rumoca_ir_ast::Name::from_string("Medium.AbsolutePressure");
    assert_eq!(
        crate::modifier_targets::resolve_dotted_member_def_id(&resolved, None, &name,),
        None
    );
}

#[test]
fn dotted_member_without_member_is_rejected_by_identity_resolver() {
    let resolved = resolve(parse(&inherited_replaceable_port_source("p")))
        .expect("resolve should succeed")
        .into_inner();
    let anchor = resolved
        .name_map
        .get("P.FluidPort.Medium")
        .copied()
        .expect("replaceable package anchor should have an identity");
    let name = rumoca_ir_ast::Name::from_string("Medium.Missing");
    assert_eq!(
        crate::modifier_targets::resolve_dotted_member_def_id(&resolved, Some(anchor), &name),
        None
    );
}

#[test]
fn ambiguous_inherited_member_is_rejected_by_identity_resolver() {
    let source = r#"
        package P
            package Left
                type Pressure = Real;
            end Left;

            package Right
                type Pressure = Integer;
            end Right;

            package AmbiguousMedium
                extends Left;
                extends Right;
            end AmbiguousMedium;

            connector FluidPort
                replaceable package Medium = AmbiguousMedium;
            end FluidPort;
        end P;
    "#;
    let resolved = resolve(parse(source))
        .expect("the unused ambiguous package should resolve")
        .into_inner();
    let anchor = resolved
        .name_map
        .get("P.FluidPort.Medium")
        .copied()
        .expect("replaceable package anchor should have an identity");
    let name = rumoca_ir_ast::Name::from_string("Medium.Pressure");
    assert_eq!(
        crate::modifier_targets::resolve_dotted_member_def_id(&resolved, Some(anchor), &name),
        None
    );
}

#[test]
fn missing_anchor_does_not_fall_through_to_qualified_type_table_entry() {
    let source = r#"
        package P
            package Types
                type AbsolutePressure = Real;
            end Types;

            partial package BaseMedium
                extends Types;
            end BaseMedium;

            connector FluidPort
                P.BaseMedium.AbsolutePressure p;
            end FluidPort;
        end P;
    "#;
    let mut tree = resolve(parse(source))
        .expect("resolve should succeed")
        .into_inner();
    let (fluid_port_def_id, display_name) = {
        let fluid_port = tree
            .definitions
            .classes
            .get_mut("P")
            .expect("package P")
            .classes
            .get_mut("FluidPort")
            .expect("FluidPort");
        let component = fluid_port.components.get_mut("p").expect("pressure member");
        let display_name = component.type_name.to_string();
        assert_eq!(display_name, "P.BaseMedium.AbsolutePressure");
        component.type_name.def_id = None;
        component.type_def_id = None;
        (fluid_port.def_id.expect("FluidPort identity"), display_name)
    };

    tree.type_table
        .add_type(rumoca_ir_ast::Type::Alias(rumoca_ir_ast::TypeAlias {
            name: display_name,
            aliased: tree.type_table.real(),
        }));
    let members = crate::modifier_targets::build_component_modifier_member_types_for_def_ids(
        &tree,
        &tree.type_table,
        &std::collections::HashMap::new(),
        &tree.source_map,
        [fluid_port_def_id],
    )
    .expect("member catalog should build");
    assert!(
        !members
            .get(&fluid_port_def_id)
            .expect("FluidPort member catalog")
            .contains_key("p"),
        "missing anchor must not use the qualified display-name table entry"
    );
}

#[test]
fn test_unknown_builtin_modifier_reports_error() {
    let source = r#"
        model Test
            Real x(startd = 1.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(result.is_err(), "typecheck should reject unknown modifiers");

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags.iter().any(|d| d.code.as_deref() == Some("ET001")
            && d.message.contains("unknown modifier `startd`")),
        "expected unknown modifier diagnostic, got: {:?}",
        diags
    );
}

#[test]
fn test_unknown_builtin_modifier_startdt_reports_error() {
    let source = r#"
        model Test
            Real x(startdt = 1.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(result.is_err(), "typecheck should reject unknown modifiers");

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags.iter().any(|d| d.code.as_deref() == Some("ET001")
            && d.message.contains("unknown modifier `startdt`")),
        "expected unknown modifier diagnostic, got: {:?}",
        diags
    );
}

#[test]
fn test_unknown_builtin_modifier_startdt_without_spaces_reports_error() {
    let source = r#"
        model Test
            Real x(startdt=1.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(result.is_err(), "typecheck should reject unknown modifiers");

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags.iter().any(|d| d.code.as_deref() == Some("ET001")
            && d.message.contains("unknown modifier `startdt`")),
        "expected unknown modifier diagnostic, got: {:?}",
        diags
    );
}

#[test]
fn test_unknown_class_component_modifier_reports_error() {
    let source = r#"
        model PID
            parameter Real kp = 1.0;
        end PID;

        model Test
            PID pid(kps = 10.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_err(),
        "typecheck should reject unknown class modifiers"
    );

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags
            .iter()
            .any(|d| d.code.as_deref() == Some("ET001")
                && d.message.contains("unknown modifier `kps`")),
        "expected unknown class modifier diagnostic, got: {:?}",
        diags
    );
}

#[test]
fn test_unknown_class_component_start_modifier_reports_error() {
    let source = r#"
        model Main
            Test t1(start=1), t2(start=2);
        end Main;

        model Test
            Real x;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_err(),
        "typecheck should reject unknown class modifiers"
    );

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags.iter().any(|d| d.code.as_deref() == Some("ET001")
            && d.message.contains("unknown modifier `start`")),
        "expected unknown class start modifier diagnostic, got: {:?}",
        diags
    );
}

#[test]
fn test_unknown_nested_builtin_modifier_reports_error() {
    let source = r#"
        model Plane
            Real x;
            Real y;
            Real theta;
        end Plane;

        model Test
            Plane p1(x.star88t = 1.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_err(),
        "typecheck should reject unknown nested builtin modifiers"
    );

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags.iter().any(|d| d.code.as_deref() == Some("ET001")
            && d.message.contains("unknown modifier `x.star88t`")),
        "expected unknown nested builtin modifier diagnostic, got: {:?}",
        diags
    );
}

#[test]
fn test_inherited_class_component_modifier_is_allowed() {
    let source = r#"
        model Base
            parameter Real kp = 1.0;
        end Base;

        model PID
            extends Base;
        end PID;

        model Test
            PID pid(kp = 10.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_ok(),
        "typecheck should allow inherited class member modifiers"
    );
}

#[test]
fn test_builtin_start_modifier_type_mismatch_reports_error() {
    let source = r#"
        model Test
            Boolean df = true;
            Real v(start = df);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_err(),
        "typecheck should reject incompatible builtin modifier types"
    );

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags.iter().any(|d| d.code.as_deref() == Some("ET002")
            && d.message.contains("modifier `start`")
            && d.message.contains("expects `Real`, found `Boolean`")),
        "expected modifier type mismatch diagnostic, got: {:?}",
        diags
    );
}

#[test]
fn test_builtin_fixed_modifier_type_mismatch_reports_error() {
    let source = r#"
        model Test
            Real v(fixed = 1);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_err(),
        "typecheck should reject incompatible builtin modifier types"
    );

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags.iter().any(|d| d.code.as_deref() == Some("ET002")
            && d.message.contains("modifier `fixed`")
            && d.message.contains("expects `Boolean`")),
        "expected modifier type mismatch diagnostic, got: {:?}",
        diags
    );
}
