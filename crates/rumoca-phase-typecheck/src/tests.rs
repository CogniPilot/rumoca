use super::*;
use rumoca_ir_ast::{
    ComponentRefPart, ComponentReference, Expression, InstanceData, InstanceId, ParsedTree,
    QualifiedName, Subscript, TerminalType, Token,
};
use rumoca_phase_parse::parse_to_ast;
use rumoca_phase_resolve::resolve;
use std::sync::Arc;

/// Helper to parse source code into a ParsedTree.
fn parse(source: &str) -> ParsedTree {
    let stored_def = parse_to_ast(source, "<test>").expect("parse should succeed");
    let tree = ClassTree::from_parsed(stored_def);
    ParsedTree::new(tree)
}

#[test]
fn test_empty_typecheck() {
    let tree = ClassTree::new();
    let parsed = ParsedTree::new(tree);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(result.is_ok());
}

#[test]
fn test_builtin_type_resolution() {
    // Parse a simple model with Real, Integer, Boolean, String types
    let source = r#"
        model Test
            Real x;
            Integer i;
            Boolean b;
            String s;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let typed = typecheck(resolved).expect("typecheck should succeed");

    // Check that types were resolved
    let tree = typed.into_inner();
    let test_class = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");

    let x = test_class.components.get("x").expect("x should exist");
    assert!(x.type_id.is_some());
    assert_ne!(x.type_id.unwrap(), TypeId::UNKNOWN);

    let i = test_class.components.get("i").expect("i should exist");
    assert!(i.type_id.is_some());
    assert_ne!(i.type_id.unwrap(), TypeId::UNKNOWN);

    let b = test_class.components.get("b").expect("b should exist");
    assert!(b.type_id.is_some());
    assert_ne!(b.type_id.unwrap(), TypeId::UNKNOWN);

    let s = test_class.components.get("s").expect("s should exist");
    assert!(s.type_id.is_some());
    assert_ne!(s.type_id.unwrap(), TypeId::UNKNOWN);
}

#[test]
fn test_builtin_clock_type_resolution() {
    let source = r#"
        model Test
            Clock c;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let typed = typecheck(resolved).expect("typecheck should succeed");

    let tree = typed.into_inner();
    let test_class = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");
    let c = test_class.components.get("c").expect("c should exist");
    assert!(c.type_id.is_some());
    assert_ne!(c.type_id.unwrap(), TypeId::UNKNOWN);
}

#[test]
fn test_predefined_stateselect_type_resolution() {
    let source = r#"
        model Test
            StateSelect sel = StateSelect.default;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let typed = typecheck(resolved).expect("typecheck should succeed");

    let tree = typed.into_inner();
    let test_class = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");
    let sel = test_class.components.get("sel").expect("sel should exist");
    assert!(sel.type_id.is_some());
    assert_ne!(sel.type_id.unwrap(), TypeId::UNKNOWN);
}

#[test]
fn test_predefined_assertion_level_type_resolution() {
    let source = r#"
        model Test
            AssertionLevel level = AssertionLevel.warning;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let typed = typecheck(resolved).expect("typecheck should succeed");

    let tree = typed.into_inner();
    let test_class = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");
    let level = test_class
        .components
        .get("level")
        .expect("level should exist");
    assert!(level.type_id.is_some());
    assert_ne!(level.type_id.unwrap(), TypeId::UNKNOWN);
}

#[test]
fn test_equation_typecheck() {
    let source = r#"
        model Test
            Real x;
            Real y;
        equation
            x = y + 1;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(result.is_ok());
}

#[test]
fn test_algorithm_typecheck() {
    let source = r#"
        model Test
            Real x;
            Real y;
        algorithm
            x := y + 1;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(result.is_ok());
}

#[test]
fn test_dimension_evaluation() {
    // Test that shape_expr is evaluated to shape during typecheck
    let source = r#"
        model Test
            parameter Integer n = 3;
            Real x[n];
            Real y[2, 3];
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let typed = typecheck(resolved).expect("typecheck should succeed");

    let tree = typed.into_inner();
    let test_class = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");

    // Check y has evaluated dimensions [2, 3]
    let y = test_class.components.get("y").expect("y should exist");
    assert_eq!(y.shape, vec![2, 3], "y should have shape [2, 3]");

    // Note: x[n] requires parameter evaluation which depends on context
    // The dimension may or may not be evaluated depending on whether n is known
}

#[test]
fn test_colon_dimension_inference() {
    // Test that colon dimensions are inferred from binding
    let source = r#"
        model Test
            Real x[:] = {1, 2, 3};
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let typed = typecheck(resolved).expect("typecheck should succeed");

    let tree = typed.into_inner();
    let test_class = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");

    // Check x has inferred dimension [3]
    let x = test_class.components.get("x").expect("x should exist");
    assert_eq!(x.shape, vec![3], "x should have inferred shape [3]");
}

#[test]
fn test_import_constant_prefixes_include_alias_and_full_path() {
    let import = ScopeImport::Renamed {
        alias: "Medium".to_string(),
        path: vec![
            "Modelica".to_string(),
            "Media".to_string(),
            "Air".to_string(),
            "ReferenceMoistAir".to_string(),
        ],
        def_id: DefId::new(7),
    };
    let mut prefixes = TypeChecker::import_constant_prefixes(&import);
    prefixes.sort_by(|a, b| a.0.cmp(&b.0));

    assert!(
        prefixes.iter().any(|(name, _)| name == "Medium"),
        "renamed import alias should be included"
    );
    assert!(
        prefixes
            .iter()
            .any(|(name, _)| name == "Modelica.Media.Air.ReferenceMoistAir"),
        "full import path should be included for strict structural lookup"
    );
    assert!(
        prefixes.iter().any(|(name, _)| name == "ReferenceMoistAir"),
        "terminal import symbol should be included for compatibility"
    );
}

#[test]
fn test_structural_parameter_marking() {
    // Test that parameters used in dimensions are marked as structural (MLS §18.3)
    let source = r#"
        model Test
            parameter Integer n = 3;
            parameter Integer m = 5;
            parameter Real unused = 1.0;
            Real x[n];
            Real y[m, 2];
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let typed = typecheck(resolved).expect("typecheck should succeed");

    let tree = typed.into_inner();
    let test_class = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");

    // Check n is marked as structural (used in x[n])
    let n = test_class.components.get("n").expect("n should exist");
    assert!(n.is_structural, "n should be marked as structural");

    // Check m is marked as structural (used in y[m, 2])
    let m = test_class.components.get("m").expect("m should exist");
    assert!(m.is_structural, "m should be marked as structural");

    // Check unused is NOT marked as structural
    let unused = test_class
        .components
        .get("unused")
        .expect("unused should exist");
    assert!(
        !unused.is_structural,
        "unused should not be marked as structural"
    );
}

#[test]
fn test_variability_validation() {
    // Test that variability constraints are validated (MLS §3.8.4)
    // A parameter binding that references a continuous variable is caught at resolve time
    let source = r#"
        model Test
            Real x;
            parameter Real p = x;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed);
    assert!(
        resolved.is_err(),
        "resolve should reject parameter depending on continuous variable"
    );
}

#[test]
fn test_variability_validation_valid() {
    // Test that valid variability bindings don't produce warnings
    let source = r#"
        model Test
            constant Real c = 1.0;
            parameter Real p = c;
            Real x = p;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(result.is_ok(), "typecheck should succeed");
}

#[test]
fn test_causality_validation_input_with_binding() {
    // Test that input with binding produces a warning (MLS §4.6)
    let source = r#"
        connector RealInput = input Real;
        model Test
            RealInput u = 1.0;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");

    // Typecheck should succeed but emit a warning
    let result = typecheck(resolved);
    assert!(result.is_ok(), "typecheck should succeed with warning");
}

#[test]
fn test_causality_validation_output_valid() {
    // Test that output with binding is valid
    let source = r#"
        connector RealOutput = output Real;
        model Test
            RealOutput y = 1.0;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(result.is_ok(), "typecheck should succeed");
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

#[test]
fn test_user_defined_type_resolution() {
    let source = r#"
        type Voltage = Real;
        type Mode = enumeration(Off, On);
        record Payload
            Real x;
        end Payload;

        model Test
            Voltage v;
            Mode m;
            Payload p;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let typed = typecheck(resolved).expect("typecheck should succeed");
    let tree = typed.into_inner();

    let test = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");

    let v_type_id = test
        .components
        .get("v")
        .and_then(|c| c.type_id)
        .expect("v type id");
    let m_type_id = test
        .components
        .get("m")
        .and_then(|c| c.type_id)
        .expect("m type id");
    let p_type_id = test
        .components
        .get("p")
        .and_then(|c| c.type_id)
        .expect("p type id");

    assert!(!v_type_id.is_unknown(), "alias type should resolve");
    assert!(!m_type_id.is_unknown(), "enum type should resolve");
    assert!(!p_type_id.is_unknown(), "record type should resolve");

    assert!(
        matches!(tree.type_table.get(v_type_id), Some(Type::Alias(_))),
        "Voltage should be represented as a Type::Alias"
    );
    assert!(
        matches!(tree.type_table.get(m_type_id), Some(Type::Enumeration(_))),
        "Mode should be represented as a Type::Enumeration"
    );
    assert!(
        matches!(
            tree.type_table.get(p_type_id),
            Some(Type::Class(cls)) if cls.kind == ClassKind::Record
        ),
        "Payload should be represented as a record class type"
    );
}

#[test]
fn test_typecheck_instanced_populates_user_defined_type_ids() {
    let source = r#"
        type Voltage = Real;
        type Mode = enumeration(Off, On);
        model Test
            Voltage v;
            Mode m;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let test = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");

    let v_decl = test.components.get("v").expect("v declaration");
    let m_decl = test.components.get("m").expect("m declaration");

    let mut overlay = InstanceOverlay::new();
    let v_id = overlay.alloc_id();
    overlay.add_component(rumoca_ir_ast::InstanceData {
        instance_id: v_id,
        qualified_name: rumoca_ir_ast::QualifiedName::from_ident("v"),
        // Seed with builtin Real id to verify instanced typecheck rewrites
        // to declared alias identity (Voltage), not just UNKNOWN placeholders.
        type_id: tree.type_table.real(),
        type_name: "Voltage".to_string(),
        type_def_id: v_decl.type_def_id,
        is_primitive: true,
        ..Default::default()
    });
    let m_id = overlay.alloc_id();
    overlay.add_component(rumoca_ir_ast::InstanceData {
        instance_id: m_id,
        qualified_name: rumoca_ir_ast::QualifiedName::from_ident("m"),
        type_id: TypeId::UNKNOWN,
        type_name: "Mode".to_string(),
        type_def_id: m_decl.type_def_id,
        is_primitive: true,
        ..Default::default()
    });

    typecheck_instanced(&tree, &mut overlay, "Test").expect("typecheck_instanced should pass");

    let v_inst = overlay
        .components
        .values()
        .find(|d| d.qualified_name.to_flat_string() == "v")
        .expect("v instance");
    let m_inst = overlay
        .components
        .values()
        .find(|d| d.qualified_name.to_flat_string() == "m")
        .expect("m instance");

    assert!(
        !v_inst.type_id.is_unknown(),
        "instanced alias type should resolve"
    );
    assert_ne!(
        v_inst.type_id,
        tree.type_table.real(),
        "alias type should preserve declared identity, not collapse to builtin Real"
    );
    assert!(
        !m_inst.type_id.is_unknown(),
        "instanced enum type should resolve"
    );
}

#[test]
fn test_typecheck_instanced_resolves_unique_suffix_type_name() {
    let source = r#"
        package A
            package Units
                type Reluctance = Real;
            end Units;
        end A;

        model Test
            A.Units.Reluctance r;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let mut overlay = InstanceOverlay::new();
    let id = overlay.alloc_id();
    overlay.add_component(InstanceData {
        instance_id: id,
        qualified_name: QualifiedName::from_dotted("Test.r"),
        type_id: TypeId::UNKNOWN,
        // Simulate an instanced relative/imported type path.
        type_name: "Units.Reluctance".to_string(),
        type_def_id: None,
        is_primitive: true,
        ..Default::default()
    });

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("instanced typecheck should resolve unique suffix type names");

    let r_inst = overlay
        .components
        .values()
        .find(|d| d.qualified_name.to_flat_string() == "Test.r")
        .expect("r instance");
    assert!(
        !r_inst.type_id.is_unknown(),
        "unique suffix type should resolve"
    );
}

#[test]
fn test_typecheck_instanced_rejects_ambiguous_suffix_type_name() {
    let source = r#"
        package A
            package Units
                type Reluctance = Real;
            end Units;
        end A;

        package B
            package Units
                type Reluctance = Real;
            end Units;
        end B;

        model Test
            A.Units.Reluctance r;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let mut overlay = InstanceOverlay::new();
    let id = overlay.alloc_id();
    overlay.add_component(InstanceData {
        instance_id: id,
        qualified_name: QualifiedName::from_dotted("Test.r"),
        type_id: TypeId::UNKNOWN,
        // Ambiguous between A.Units.Reluctance and B.Units.Reluctance.
        type_name: "Units.Reluctance".to_string(),
        type_def_id: None,
        is_primitive: true,
        ..Default::default()
    });

    let err = typecheck_instanced(&tree, &mut overlay, "Test")
        .expect_err("ambiguous suffix type names should remain unresolved");
    assert!(
        err.iter().any(|d| d.code.as_deref() == Some("ET001")
            && d.message.contains("undefined type 'Units.Reluctance'")),
        "expected unresolved-type diagnostic for ambiguous suffix, got: {:?}",
        err
    );
}

#[test]
fn test_typecheck_instanced_resolves_dotted_type_via_anchor_def_id() {
    let source = r#"
        package Outer
            package Medium
                type AbsolutePressure = Real;
            end Medium;

            model Test
                Medium.AbsolutePressure p;
            end Test;
        end Outer;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let typed = typecheck(resolved).expect("typecheck should succeed");
    let tree = typed.into_inner();

    let medium_def_id = tree
        .name_map
        .get("Outer.Medium")
        .copied()
        .expect("Outer.Medium should resolve");
    let medium_package_type = tree
        .type_table
        .lookup("Outer.Medium")
        .expect("Outer.Medium package type should exist");

    let mut overlay = InstanceOverlay::new();
    let id = overlay.alloc_id();
    overlay.add_component(InstanceData {
        instance_id: id,
        qualified_name: QualifiedName::from_dotted("Outer.Test.p"),
        type_id: TypeId::UNKNOWN,
        type_name: "Medium.AbsolutePressure".to_string(),
        // Anchor only the first segment (`Medium`) and require dotted-tail resolution.
        type_def_id: Some(medium_def_id),
        is_primitive: true,
        ..Default::default()
    });

    typecheck_instanced(&tree, &mut overlay, "Outer.Test")
        .expect("instanced typecheck should resolve anchored dotted type names");

    let p_inst = overlay
        .components
        .values()
        .find(|d| d.qualified_name.to_flat_string() == "Outer.Test.p")
        .expect("p instance");
    assert!(
        !p_inst.type_id.is_unknown(),
        "anchored dotted type should resolve"
    );
    assert_ne!(
        p_inst.type_id, medium_package_type,
        "dotted type must not collapse to anchor package type"
    );
}

#[test]
fn test_typecheck_instanced_detects_user_defined_equation_mismatch() {
    let source = r#"
        record LeftPayload
            Real x;
        end LeftPayload;
        record RightPayload
            Real x;
        end RightPayload;
        model Test
            LeftPayload lhs;
            RightPayload rhs;
        equation
            lhs = rhs;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let test = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");

    let lhs_decl = test.components.get("lhs").expect("lhs declaration");
    let rhs_decl = test.components.get("rhs").expect("rhs declaration");

    let mut overlay = InstanceOverlay::new();
    let lhs_id = overlay.alloc_id();
    overlay.add_component(rumoca_ir_ast::InstanceData {
        instance_id: lhs_id,
        qualified_name: rumoca_ir_ast::QualifiedName::from_dotted("Test.lhs"),
        type_id: TypeId::UNKNOWN,
        type_name: "LeftPayload".to_string(),
        type_def_id: lhs_decl.type_def_id,
        is_primitive: false,
        ..Default::default()
    });
    let rhs_id = overlay.alloc_id();
    overlay.add_component(rumoca_ir_ast::InstanceData {
        instance_id: rhs_id,
        qualified_name: rumoca_ir_ast::QualifiedName::from_dotted("Test.rhs"),
        type_id: TypeId::UNKNOWN,
        type_name: "RightPayload".to_string(),
        type_def_id: rhs_decl.type_def_id,
        is_primitive: false,
        ..Default::default()
    });

    let err = typecheck_instanced(&tree, &mut overlay, "Test")
        .expect_err("instanced mismatch should fail typecheck");
    assert!(
        err.iter().any(|d| d.code.as_deref() == Some("ET002")),
        "expected ET002 diagnostics for instanced user-defined type mismatch"
    );
}

#[test]
fn test_typecheck_instanced_reports_unknown_builtin_modifier() {
    let source = r#"
        model Test
            Real x(startd = 1.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let mut overlay = rumoca_ir_ast::InstanceOverlay::new();

    let err = typecheck_instanced(&tree, &mut overlay, "Test")
        .expect_err("instanced typecheck should reject unknown builtin modifiers");
    assert!(
        err.iter().any(|d| d.code.as_deref() == Some("ET001")
            && d.message.contains("unknown modifier `startd`")),
        "expected unknown modifier diagnostic in instanced pipeline, got: {:?}",
        err
    );
}

#[test]
fn test_typecheck_instanced_reports_unknown_class_component_modifier() {
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
    let tree = resolved.into_inner();
    let mut overlay = rumoca_ir_ast::InstanceOverlay::new();

    let err = typecheck_instanced(&tree, &mut overlay, "Test")
        .expect_err("instanced typecheck should reject unknown class modifiers");
    assert!(
        err.iter()
            .any(|d| d.code.as_deref() == Some("ET001")
                && d.message.contains("unknown modifier `kps`")),
        "expected unknown class modifier diagnostic in instanced pipeline, got: {:?}",
        err
    );
}

#[test]
fn test_typecheck_instanced_reports_unknown_nested_builtin_modifier() {
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
    let tree = resolved.into_inner();
    let mut overlay = rumoca_ir_ast::InstanceOverlay::new();

    let err = typecheck_instanced(&tree, &mut overlay, "Test")
        .expect_err("instanced typecheck should reject unknown nested builtin modifiers");
    assert!(
        err.iter().any(|d| d.code.as_deref() == Some("ET001")
            && d.message.contains("unknown modifier `x.star88t`")),
        "expected unknown nested builtin modifier diagnostic in instanced pipeline, got: {:?}",
        err
    );
}

#[test]
fn test_typecheck_instanced_reports_builtin_modifier_type_mismatch() {
    let source = r#"
        model Test
            Boolean df = true;
            Real v(start = df);
        equation
            der(v) = -v;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let mut overlay = rumoca_ir_ast::InstanceOverlay::new();

    let err = typecheck_instanced(&tree, &mut overlay, "Test")
        .expect_err("instanced typecheck should reject incompatible modifier types");
    assert!(
        err.iter().any(|d| d.code.as_deref() == Some("ET002")
            && d.message.contains("modifier `start`")
            && d.message.contains("expects `Real`, found `Boolean`")),
        "expected modifier type mismatch diagnostic in instanced pipeline, got: {:?}",
        err
    );
}

#[test]
fn test_user_defined_equation_compatibility() {
    let source = r#"
        type Mode = enumeration(Off, On);
        record Payload
            Real x;
        end Payload;

        model Test
            Mode m1;
            Mode m2;
            Payload p1;
            Payload p2;
        equation
            m1 = m2;
            p1 = p2;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_ok(),
        "same enum/record types should be compatible"
    );
}

#[test]
fn test_user_defined_equation_mismatch_detection() {
    let source = r#"
        type ModeA = enumeration(Off, On);
        type ModeB = enumeration(Off, On);
        record PayloadA
            Real x;
        end PayloadA;
        record PayloadB
            Real x;
        end PayloadB;

        model Test
            ModeA m1;
            ModeB m2;
            PayloadA p1;
            PayloadB p2;
        equation
            m1 = m2;
            p1 = p2;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_err(),
        "different enum/record types should mismatch"
    );

    let diags = result.expect_err("expected diagnostics");
    let et002_count = diags
        .iter()
        .filter(|d| d.code.as_deref() == Some("ET002"))
        .count();
    assert!(
        et002_count >= 2,
        "expected ET002 diagnostics for enum and record equation mismatch"
    );
}

#[test]
fn test_user_defined_record_constructor_mismatch_detection() {
    let source = r#"
        record LeftPayload
            Real x;
        end LeftPayload;
        record RightPayload
            Real x;
        end RightPayload;

        model Test
            LeftPayload lhs;
        equation
            lhs = RightPayload(x = 1.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_err(),
        "record constructor of different type should be rejected"
    );

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags
            .iter()
            .any(|d| d.code.as_deref() == Some("ET002") && d.message.contains("type mismatch")),
        "expected ET002 diagnostic for mismatched record constructor assignment"
    );
}

#[test]
fn test_user_defined_record_constructor_compatibility() {
    let source = r#"
        record Payload
            Real x;
        end Payload;

        model Test
            Payload lhs;
        equation
            lhs = Payload(x = 1.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_ok(),
        "record constructor with matching type should remain compatible"
    );
}

#[test]
fn test_record_wrapper_constructor_assignment_is_compatible() {
    let source = r#"
        record BasePayload
            Real x;
        end BasePayload;

        record WrappedPayload = BasePayload;

        model Test
            WrappedPayload lhs;
        equation
            lhs = BasePayload(x = 1.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_ok(),
        "record wrapper should be assignment-compatible with its base constructor"
    );
}

#[test]
fn test_alias_field_key_range_matches_only_target_prefix() {
    let mut sorted_keys = vec![
        "root.src.fieldA".to_string(),
        "root.src.fieldB".to_string(),
        "root.src2.fieldC".to_string(),
        "root.target.fieldD".to_string(),
    ];
    sorted_keys.sort_unstable();

    let matched = TypeChecker::alias_field_key_range(&sorted_keys, "root.src.");
    let matched: Vec<&str> = matched.iter().map(String::as_str).collect();

    assert_eq!(matched, vec!["root.src.fieldA", "root.src.fieldB"]);
}

#[test]
fn test_propagate_alias_map_copies_root_and_prefixed_fields() {
    let aliases = vec![("dst".to_string(), "src".to_string())];
    let mut values: rustc_hash::FxHashMap<String, i64> = rustc_hash::FxHashMap::default();
    values.insert("src".to_string(), 1);
    values.insert("src.nX".to_string(), 2);
    values.insert("src.nXi".to_string(), 3);
    values.insert("src2.nX".to_string(), 99);

    let progress = TypeChecker::propagate_alias_map(&aliases, &mut values);

    assert!(progress);
    assert_eq!(values.get("dst"), Some(&1));
    assert_eq!(values.get("dst.nX"), Some(&2));
    assert_eq!(values.get("dst.nXi"), Some(&3));
    assert_eq!(values.get("dst2.nX"), None);
}

#[test]
fn test_extract_simple_path_preserves_subscripted_component_refs() {
    let expr = Expression::ComponentReference(ComponentReference {
        local: false,
        parts: vec![
            ComponentRefPart {
                ident: Token {
                    text: Arc::from("stackData"),
                    location: Default::default(),
                    token_number: 0,
                    token_type: 0,
                },
                subs: None,
            },
            ComponentRefPart {
                ident: Token {
                    text: Arc::from("cellData"),
                    location: Default::default(),
                    token_number: 0,
                    token_type: 0,
                },
                subs: Some(vec![
                    Subscript::Expression(Expression::Terminal {
                        terminal_type: TerminalType::UnsignedInteger,
                        token: Token {
                            text: Arc::from("1"),
                            location: Default::default(),
                            token_number: 0,
                            token_type: 0,
                        },
                    }),
                    Subscript::Expression(Expression::Terminal {
                        terminal_type: TerminalType::UnsignedInteger,
                        token: Token {
                            text: Arc::from("2"),
                            location: Default::default(),
                            token_number: 0,
                            token_type: 0,
                        },
                    }),
                ]),
            },
        ],
        def_id: None,
    });

    assert_eq!(
        TypeChecker::extract_simple_path(&expr),
        Some("stackData.cellData[1,2]".to_string())
    );
}

#[test]
fn test_propagate_alias_map_copies_indexed_record_fields() {
    let aliases = vec![(
        "dst.cell[1].cellData".to_string(),
        "src.stackData.cellData[1,1]".to_string(),
    )];
    let mut values: rustc_hash::FxHashMap<String, Vec<usize>> = rustc_hash::FxHashMap::default();
    values.insert(
        "src.stackData.cellData[1,1].OCV_SOC".to_string(),
        vec![29, 2],
    );

    let progress = TypeChecker::propagate_alias_map(&aliases, &mut values);

    assert!(progress);
    assert_eq!(
        values.get("dst.cell[1].cellData.OCV_SOC"),
        Some(&vec![29, 2])
    );
}

#[test]
fn test_insert_instanced_aliases_ignores_dot_inside_subscript_expression() {
    let mut out = HashMap::new();
    TypeChecker::insert_instanced_aliases(
        &mut out,
        "plug[data.medium]",
        TypeId::new(7),
        Some("Top"),
    );

    assert_eq!(out.get("plug[data.medium]"), Some(&TypeId::new(7)));
    assert_eq!(out.get("Top.plug[data.medium]"), Some(&TypeId::new(7)));
}

#[test]
fn test_build_instanced_component_type_scope_keeps_subscript_dot_single_segment() {
    let mut overlay = InstanceOverlay::default();
    overlay.components.insert(
        InstanceId::new(1),
        InstanceData {
            qualified_name: QualifiedName {
                parts: vec![("plug[data.medium]".to_string(), vec![])],
            },
            type_id: TypeId::new(11),
            ..Default::default()
        },
    );

    let scope_map = TypeChecker::build_instanced_component_type_scope(&overlay, "Top.Model");
    assert_eq!(
        scope_map.get("plug[data.medium]"),
        Some(&TypeId::new(11)),
        "dot inside subscript content must not block top-level instanced scope aliases"
    );
}
