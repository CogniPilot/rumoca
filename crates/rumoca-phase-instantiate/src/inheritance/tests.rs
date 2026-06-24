use super::*;
use rumoca_core::DefId;
use std::sync::Arc;

const TEST_FILE: &str = "inheritance.mo";

fn test_location() -> rumoca_core::Location {
    rumoca_core::Location {
        start_line: 1,
        start_column: 1,
        end_line: 1,
        end_column: 2,
        start: 0,
        end: 1,
        file_name: TEST_FILE.to_string(),
    }
}

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(TEST_FILE), 1, 2)
}

/// Create a minimal component for testing.
fn make_component(name: &str, is_replaceable: bool, is_final: bool) -> ast::Component {
    ast::Component {
        name: name.to_string(),
        is_replaceable,
        is_final,
        ..ast::Component::empty_with_span(test_span())
    }
}

/// Create a component reference for testing.
fn make_component_ref(name: &str) -> ast::ComponentReference {
    ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: rumoca_core::Token {
                text: std::sync::Arc::from(name),
                location: rumoca_core::Location::default(),
                token_number: 0,
                token_type: 0,
            },
            subs: None,
        }],
        def_id: None,
        span: rumoca_core::Span::DUMMY,
    }
}

/// Create a token for testing.
fn make_token(text: &str) -> rumoca_core::Token {
    rumoca_core::Token {
        text: std::sync::Arc::from(text),
        location: rumoca_core::Location::default(),
        token_number: 0,
        token_type: 0,
    }
}

/// Create a Name for testing.
fn make_name(text: &str) -> rumoca_ir_ast::Name {
    rumoca_ir_ast::Name {
        name: rumoca_core::split_path_with_indices(text)
            .into_iter()
            .map(make_token)
            .collect(),
        def_id: None,
    }
}

fn make_resolved_name(text: &str, def_id: DefId) -> rumoca_ir_ast::Name {
    rumoca_ir_ast::Name {
        name: rumoca_core::split_path_with_indices(text)
            .into_iter()
            .map(make_token)
            .collect(),
        def_id: Some(def_id),
    }
}

fn make_int_expr(value: &str) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: make_token(value),
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn test_apply_extends_modifications_reports_final_override_at_extends_span() {
    let mut tree = ast::ClassTree::default();
    tree.source_map.add(TEST_FILE, "extends Base(x = 2);");
    let mut target = InheritedContent::default();
    target
        .components
        .insert("x".to_string(), make_component("x", false, true));
    let base_class = ast::ClassDef {
        name: make_token("Base"),
        ..Default::default()
    };
    let extend = ast::Extend {
        base_name: make_name("Base"),
        location: test_location(),
        modifications: vec![ast::ExtendModification {
            expr: ast::Expression::Modification {
                target: make_component_ref("x"),
                value: Arc::new(make_int_expr("2")),
                span: rumoca_core::Span::DUMMY,
            },
            each: false,
            final_: false,
            redeclare: false,
        }],
        ..Default::default()
    };

    let err = apply_extends_modifications(&tree, &mut target, &base_class, &extend)
        .expect_err("extends modification must not override final inherited component");
    assert!(matches!(*err, InstantiateError::RedeclareFinal { .. }));
}

#[test]
fn test_validate_redeclaration_non_replaceable() {
    // A non-replaceable component should fail redeclaration
    let tree = ast::ClassTree::default();
    let comp = make_component("x", false, false);
    let result = validate_redeclaration(&tree, &comp, "x", None, Span::DUMMY);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("not replaceable"));
}

#[test]
fn test_validate_redeclaration_final() {
    // A final component should fail redeclaration (even if replaceable)
    let tree = ast::ClassTree::default();
    let comp = make_component("x", true, true);
    let result = validate_redeclaration(&tree, &comp, "x", None, Span::DUMMY);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("final"));
}

#[test]
fn test_validate_redeclaration_replaceable() {
    // A replaceable, non-final component should succeed
    let tree = ast::ClassTree::default();
    let comp = make_component("x", true, false);
    let result = validate_redeclaration(&tree, &comp, "x", None, Span::DUMMY);
    assert!(result.is_ok());
}

#[test]
fn test_validate_redeclaration_constant_rejected() {
    let tree = ast::ClassTree::default();
    let mut comp = make_component("x", true, false);
    comp.variability = rumoca_core::Variability::Constant(make_token("constant"));
    let result = validate_redeclaration(&tree, &comp, "x", Some("Real"), Span::DUMMY);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.to_string()
            .contains("constant elements cannot be redeclared")
    );
}

#[test]
fn test_classes_are_compatible_for_equivalent_declarations() {
    let component = ast::Component {
        name: "k".to_string(),
        name_token: make_token("k"),
        type_name: make_name("Real"),
        variability: rumoca_core::Variability::Parameter(make_token("parameter")),
        start: ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: make_token("1"),
            span: rumoca_core::Span::DUMMY,
        },
        has_explicit_binding: true,
        binding: Some(ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: make_token("1"),
            span: rumoca_core::Span::DUMMY,
        }),
        ..ast::Component::empty_with_span(test_span())
    };

    let helper_a = ast::ClassDef {
        name: make_token("Helper"),
        class_type: rumoca_core::ClassType::Model,
        components: [("k".to_string(), component.clone())].into_iter().collect(),
        ..Default::default()
    };
    let helper_b = ast::ClassDef {
        name: make_token("Helper"),
        class_type: rumoca_core::ClassType::Model,
        components: [("k".to_string(), component)].into_iter().collect(),
        ..Default::default()
    };

    assert!(classes_are_compatible(&helper_a, &helper_b));
}

// -------------------------------------------------------------------------
// Constrainedby validation tests (MLS §7.3.2)
// -------------------------------------------------------------------------

/// Create a component with constrainedby for testing.
fn make_constrained_component(
    name: &str,
    type_name: &str,
    constrainedby: Option<&str>,
) -> ast::Component {
    ast::Component {
        name: name.to_string(),
        type_name: make_name(type_name),
        is_replaceable: true,
        is_final: false,
        constrainedby: constrainedby.map(make_name),
        ..ast::Component::empty_with_span(test_span())
    }
}

#[test]
fn test_constrainedby_exact_match() {
    // Redeclaring with exact same type as constraint should succeed
    let tree = ast::ClassTree::default();
    let comp = make_constrained_component("x", "Real", Some("Real"));
    let result = validate_redeclaration(&tree, &comp, "x", Some("Real"), Span::DUMMY);
    assert!(result.is_ok());
}

#[test]
fn test_constrainedby_violation_builtin() {
    // Redeclaring Real constrained component to Integer should fail
    let tree = ast::ClassTree::default();
    let comp = make_constrained_component("x", "Real", Some("Real"));
    let result = validate_redeclaration(&tree, &comp, "x", Some("Integer"), Span::DUMMY);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("violates constrainedby"));
}

#[test]
fn test_constrainedby_default_uses_original_type() {
    // When no constrainedby is specified, the original type is the constraint
    let tree = ast::ClassTree::default();
    let comp = make_constrained_component("x", "Real", None);
    // Redeclaring to Integer should fail (Real is implicit constraint)
    let result = validate_redeclaration(&tree, &comp, "x", Some("Integer"), Span::DUMMY);
    assert!(result.is_err());
}

#[test]
fn test_constrainedby_subtype_allowed() {
    // Redeclaring to a subtype of the constraint should succeed

    let mut tree = ast::ClassTree::default();

    // Create base class
    let base = ast::ClassDef {
        name: make_token("BaseConnector"),
        ..Default::default()
    };

    // Create derived class that extends base
    let derived = ast::ClassDef {
        name: make_token("DerivedConnector"),
        extends: vec![ast::Extend {
            base_name: make_name("BaseConnector"),
            ..Default::default()
        }],
        ..Default::default()
    };

    tree.definitions
        .classes
        .insert("BaseConnector".to_string(), base);
    tree.definitions
        .classes
        .insert("DerivedConnector".to_string(), derived);

    // ast::Component constrained to BaseConnector
    let comp = make_constrained_component("c", "BaseConnector", Some("BaseConnector"));

    // Redeclaring to DerivedConnector (a subtype) should succeed
    let result = validate_redeclaration(&tree, &comp, "c", Some("DerivedConnector"), Span::DUMMY);
    assert!(result.is_ok());
}

#[test]
fn test_constrainedby_explicit_type_overrides_declared_type_def_id() {
    let mut tree = ast::ClassTree::default();

    let declared_id = DefId::new(1);
    let constraint_id = DefId::new(2);
    let replacement_id = DefId::new(3);

    let declared = ast::ClassDef {
        name: make_token("DeclaredVolume"),
        def_id: Some(declared_id),
        ..Default::default()
    };
    let constraint = ast::ClassDef {
        name: make_token("HeatPortVolume"),
        def_id: Some(constraint_id),
        ..Default::default()
    };
    let replacement = ast::ClassDef {
        name: make_token("MoistureHeatPortVolume"),
        def_id: Some(replacement_id),
        extends: vec![ast::Extend {
            base_name: make_resolved_name("HeatPortVolume", constraint_id),
            base_def_id: Some(constraint_id),
            ..Default::default()
        }],
        ..Default::default()
    };

    tree.definitions
        .classes
        .insert("DeclaredVolume".to_string(), declared);
    tree.definitions
        .classes
        .insert("HeatPortVolume".to_string(), constraint);
    tree.definitions
        .classes
        .insert("MoistureHeatPortVolume".to_string(), replacement);
    for (name, def_id) in [
        ("DeclaredVolume", declared_id),
        ("HeatPortVolume", constraint_id),
        ("MoistureHeatPortVolume", replacement_id),
    ] {
        tree.name_map.insert(name.to_string(), def_id);
        tree.def_map.insert(def_id, name.to_string());
    }

    let mut comp = make_constrained_component("vol2", "DeclaredVolume", Some("HeatPortVolume"));
    comp.type_def_id = Some(declared_id);
    comp.constrainedby = Some(make_resolved_name("HeatPortVolume", constraint_id));

    let result = validate_redeclaration(
        &tree,
        &comp,
        "vol2",
        Some("MoistureHeatPortVolume"),
        Span::DUMMY,
    );

    assert!(
        result.is_ok(),
        "explicit constrainedby must be the constraint even when declared type_def_id is present"
    );
}

#[test]
fn test_class_redeclare_constraint_resolves_relative_to_declaration_scope() {
    let (tree, flow_characteristic_id) = relative_class_redeclare_constraint_tree();

    let class = tree
        .get_class_by_def_id(flow_characteristic_id)
        .expect("flowCharacteristic class should exist");

    let result = validate_class_redeclaration(
        &tree,
        class,
        "flowCharacteristic",
        Some("Modelica.Fluid.Machines.BaseClasses.PumpCharacteristics.quadraticFlow"),
        Span::DUMMY,
    );
    assert!(result.is_ok());
}

fn relative_class_redeclare_constraint_tree() -> (ast::ClassTree, DefId) {
    let ids = RelativeConstraintIds::new();
    let modelica = relative_constraint_modelica_class(&ids);

    let mut tree = ast::ClassTree::default();
    tree.definitions
        .classes
        .insert("Modelica".to_string(), modelica);
    for (name, def_id) in ids.qualified_names() {
        tree.name_map.insert(name.to_string(), def_id);
        tree.def_map.insert(def_id, name.to_string());
    }
    (tree, ids.flow_characteristic)
}

struct RelativeConstraintIds {
    base_flow: DefId,
    quadratic_flow: DefId,
    flow_characteristic: DefId,
    pump_characteristics: DefId,
    partial_pump: DefId,
    base_classes: DefId,
    machines: DefId,
    fluid: DefId,
    modelica: DefId,
}

impl RelativeConstraintIds {
    fn new() -> Self {
        Self {
            base_flow: DefId::new(1),
            quadratic_flow: DefId::new(2),
            flow_characteristic: DefId::new(3),
            pump_characteristics: DefId::new(4),
            partial_pump: DefId::new(5),
            base_classes: DefId::new(6),
            machines: DefId::new(7),
            fluid: DefId::new(8),
            modelica: DefId::new(9),
        }
    }

    fn qualified_names(&self) -> [(&'static str, DefId); 9] {
        [
            ("Modelica", self.modelica),
            ("Modelica.Fluid", self.fluid),
            ("Modelica.Fluid.Machines", self.machines),
            ("Modelica.Fluid.Machines.BaseClasses", self.base_classes),
            (
                "Modelica.Fluid.Machines.BaseClasses.PumpCharacteristics",
                self.pump_characteristics,
            ),
            (
                "Modelica.Fluid.Machines.BaseClasses.PumpCharacteristics.baseFlow",
                self.base_flow,
            ),
            (
                "Modelica.Fluid.Machines.BaseClasses.PumpCharacteristics.quadraticFlow",
                self.quadratic_flow,
            ),
            (
                "Modelica.Fluid.Machines.BaseClasses.PartialPump",
                self.partial_pump,
            ),
            (
                "Modelica.Fluid.Machines.BaseClasses.PartialPump.flowCharacteristic",
                self.flow_characteristic,
            ),
        ]
    }
}

fn relative_constraint_modelica_class(ids: &RelativeConstraintIds) -> ast::ClassDef {
    let base_flow = ast::ClassDef {
        name: make_token("baseFlow"),
        def_id: Some(ids.base_flow),
        class_type: rumoca_core::ClassType::Function,
        ..Default::default()
    };
    let quadratic_flow = ast::ClassDef {
        name: make_token("quadraticFlow"),
        def_id: Some(ids.quadratic_flow),
        class_type: rumoca_core::ClassType::Function,
        extends: vec![ast::Extend {
            base_name: make_resolved_name("baseFlow", ids.base_flow),
            base_def_id: Some(ids.base_flow),
            ..Default::default()
        }],
        ..Default::default()
    };

    let mut pump_characteristics = ast::ClassDef {
        name: make_token("PumpCharacteristics"),
        def_id: Some(ids.pump_characteristics),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    pump_characteristics
        .classes
        .insert("baseFlow".to_string(), base_flow);
    pump_characteristics
        .classes
        .insert("quadraticFlow".to_string(), quadratic_flow);

    let flow_characteristic = ast::ClassDef {
        name: make_token("flowCharacteristic"),
        def_id: Some(ids.flow_characteristic),
        class_type: rumoca_core::ClassType::Function,
        is_replaceable: true,
        extends: vec![ast::Extend {
            base_name: make_resolved_name("PumpCharacteristics.baseFlow", ids.base_flow),
            base_def_id: Some(ids.base_flow),
            ..Default::default()
        }],
        ..Default::default()
    };
    let mut partial_pump = ast::ClassDef {
        name: make_token("PartialPump"),
        def_id: Some(ids.partial_pump),
        ..Default::default()
    };
    partial_pump
        .classes
        .insert("flowCharacteristic".to_string(), flow_characteristic);

    let mut base_classes = ast::ClassDef {
        name: make_token("BaseClasses"),
        def_id: Some(ids.base_classes),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    base_classes
        .classes
        .insert("PumpCharacteristics".to_string(), pump_characteristics);
    base_classes
        .classes
        .insert("PartialPump".to_string(), partial_pump);

    let mut machines = ast::ClassDef {
        name: make_token("Machines"),
        def_id: Some(ids.machines),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    machines
        .classes
        .insert("BaseClasses".to_string(), base_classes);

    let mut fluid = ast::ClassDef {
        name: make_token("Fluid"),
        def_id: Some(ids.fluid),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    fluid.classes.insert("Machines".to_string(), machines);

    let mut modelica = ast::ClassDef {
        name: make_token("Modelica"),
        def_id: Some(ids.modelica),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    modelica.classes.insert("Fluid".to_string(), fluid);
    modelica
}

#[test]
fn test_constrainedby_non_subtype_rejected() {
    // Redeclaring to a non-subtype should fail
    let mut tree = ast::ClassTree::default();

    // Create two unrelated classes
    let class_a = ast::ClassDef {
        name: make_token("ClassA"),
        ..Default::default()
    };
    let class_b = ast::ClassDef {
        name: make_token("ClassB"),
        ..Default::default()
    };

    tree.definitions
        .classes
        .insert("ClassA".to_string(), class_a);
    tree.definitions
        .classes
        .insert("ClassB".to_string(), class_b);

    // ast::Component constrained to ClassA
    let comp = make_constrained_component("c", "ClassA", Some("ClassA"));

    // Redeclaring to ClassB (not a subtype) should fail
    let result = validate_redeclaration(&tree, &comp, "c", Some("ClassB"), Span::DUMMY);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("violates constrainedby"));
}

#[test]
fn test_class_redeclaration_default_constraint_uses_declared_base() {
    let mut tree = ast::ClassTree::default();

    let partial = ast::ClassDef {
        name: make_token("PartialPhaseSystem"),
        def_id: Some(DefId::new(1)),
        ..Default::default()
    };
    let two_conductor = ast::ClassDef {
        name: make_token("TwoConductor"),
        def_id: Some(DefId::new(2)),
        extends: vec![ast::Extend {
            base_name: rumoca_ir_ast::Name {
                name: vec![make_token("PartialPhaseSystem")],
                def_id: Some(DefId::new(1)),
            },
            base_def_id: Some(DefId::new(1)),
            ..Default::default()
        }],
        ..Default::default()
    };
    let alias_phase_system = ast::ClassDef {
        name: make_token("PhaseSystem"),
        def_id: Some(DefId::new(3)),
        is_replaceable: true,
        extends: vec![ast::Extend {
            base_name: rumoca_ir_ast::Name {
                name: vec![make_token("PartialPhaseSystem")],
                def_id: Some(DefId::new(1)),
            },
            base_def_id: Some(DefId::new(1)),
            ..Default::default()
        }],
        ..Default::default()
    };

    tree.definitions
        .classes
        .insert("PhaseSystems.PartialPhaseSystem".to_string(), partial);
    tree.definitions
        .classes
        .insert("PhaseSystems.TwoConductor".to_string(), two_conductor);
    tree.definitions.classes.insert(
        "Interfaces.TerminalDC.PhaseSystem".to_string(),
        alias_phase_system.clone(),
    );

    tree.name_map
        .insert("PhaseSystems.PartialPhaseSystem".to_string(), DefId::new(1));
    tree.name_map
        .insert("PhaseSystems.TwoConductor".to_string(), DefId::new(2));
    tree.name_map.insert(
        "Interfaces.TerminalDC.PhaseSystem".to_string(),
        DefId::new(3),
    );

    tree.def_map
        .insert(DefId::new(1), "PhaseSystems.PartialPhaseSystem".to_string());
    tree.def_map
        .insert(DefId::new(2), "PhaseSystems.TwoConductor".to_string());
    tree.def_map.insert(
        DefId::new(3),
        "Interfaces.TerminalDC.PhaseSystem".to_string(),
    );

    let result = validate_class_redeclaration(
        &tree,
        &alias_phase_system,
        "PhaseSystem",
        Some("PhaseSystems.TwoConductor"),
        Span::DUMMY,
    );
    let err = result.expect_err("test setup should trigger subtype failure in unit fixture");
    assert!(
        err.to_string().contains("PhaseSystems.PartialPhaseSystem"),
        "default constraint should come from declared base type"
    );
    assert!(
        !err.to_string()
            .contains("Interfaces.TerminalDC.PhaseSystem"),
        "default constraint must not fall back to alias class name"
    );
}

#[test]
fn test_nested_class_redeclaration_replaces_inherited_replaceable_class() {
    let mut tree = ast::ClassTree::default();
    tree.source_map.add(TEST_FILE, "extends Base;");

    let partial_id = DefId::new(10);
    let base_properties_id = DefId::new(11);
    let derived_id = DefId::new(12);
    let redeclared_id = DefId::new(13);

    let base_properties = ast::ClassDef {
        name: make_token("BaseProperties"),
        def_id: Some(base_properties_id),
        class_type: rumoca_core::ClassType::Model,
        is_replaceable: true,
        ..Default::default()
    };
    let mut partial = ast::ClassDef {
        name: make_token("PartialPureSubstance"),
        def_id: Some(partial_id),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    partial
        .classes
        .insert("BaseProperties".to_string(), base_properties);

    let redeclared_base_properties = ast::ClassDef {
        name: make_token("BaseProperties"),
        def_id: Some(redeclared_id),
        class_type: rumoca_core::ClassType::Model,
        is_replaceable: true,
        extends: vec![ast::Extend {
            base_name: make_resolved_name(
                "PartialPureSubstance.BaseProperties",
                base_properties_id,
            ),
            base_def_id: Some(base_properties_id),
            location: test_location(),
            ..Default::default()
        }],
        ..Default::default()
    };

    let mut derived = ast::ClassDef {
        name: make_token("WaterIF97_base"),
        def_id: Some(derived_id),
        class_type: rumoca_core::ClassType::Package,
        extends: vec![ast::Extend {
            base_name: make_resolved_name("PartialPureSubstance", partial_id),
            base_def_id: Some(partial_id),
            location: test_location(),
            ..Default::default()
        }],
        ..Default::default()
    };
    derived
        .classes
        .insert("BaseProperties".to_string(), redeclared_base_properties);

    tree.definitions
        .classes
        .insert("PartialPureSubstance".to_string(), partial);
    tree.definitions
        .classes
        .insert("WaterIF97_base".to_string(), derived);
    for (name, def_id) in [
        ("PartialPureSubstance", partial_id),
        ("PartialPureSubstance.BaseProperties", base_properties_id),
        ("WaterIF97_base", derived_id),
        ("WaterIF97_base.BaseProperties", redeclared_id),
    ] {
        tree.name_map.insert(name.to_string(), def_id);
        tree.def_map.insert(def_id, name.to_string());
    }

    let class = tree
        .get_class_by_qualified_name("WaterIF97_base")
        .expect("derived class should exist");
    let result = get_effective_components(&tree, class);
    assert!(
        result.is_ok(),
        "nested replaceable class redeclare should replace the inherited declaration"
    );
}

#[test]
fn test_nested_class_redeclaration_shadows_inherited_replaceable_merged_later() {
    let mut tree = ast::ClassTree::default();
    tree.source_map.add(TEST_FILE, "extends Base;");

    let partial_medium_id = DefId::new(20);
    let partial_state_id = DefId::new(21);
    let simple_medium_id = DefId::new(22);
    let simple_state_id = DefId::new(23);
    let water_id = DefId::new(24);

    let mut partial_medium = ast::ClassDef {
        name: make_token("PartialMedium"),
        def_id: Some(partial_medium_id),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    partial_medium.classes.insert(
        "ThermodynamicState".to_string(),
        ast::ClassDef {
            name: make_token("ThermodynamicState"),
            def_id: Some(partial_state_id),
            class_type: rumoca_core::ClassType::Record,
            is_replaceable: true,
            ..Default::default()
        },
    );

    let mut simple_medium = ast::ClassDef {
        name: make_token("PartialSimpleMedium"),
        def_id: Some(simple_medium_id),
        class_type: rumoca_core::ClassType::Package,
        extends: vec![ast::Extend {
            base_name: make_resolved_name("PartialMedium", partial_medium_id),
            base_def_id: Some(partial_medium_id),
            location: test_location(),
            ..Default::default()
        }],
        ..Default::default()
    };
    simple_medium.classes.insert(
        "ThermodynamicState".to_string(),
        ast::ClassDef {
            name: make_token("ThermodynamicState"),
            def_id: Some(simple_state_id),
            class_type: rumoca_core::ClassType::Record,
            is_replaceable: true,
            ..Default::default()
        },
    );

    let water = ast::ClassDef {
        name: make_token("ConstantPropertyLiquidWater"),
        def_id: Some(water_id),
        class_type: rumoca_core::ClassType::Package,
        extends: vec![ast::Extend {
            base_name: make_resolved_name("PartialSimpleMedium", simple_medium_id),
            base_def_id: Some(simple_medium_id),
            location: test_location(),
            ..Default::default()
        }],
        ..Default::default()
    };

    tree.definitions
        .classes
        .insert("PartialMedium".to_string(), partial_medium);
    tree.definitions
        .classes
        .insert("PartialSimpleMedium".to_string(), simple_medium);
    tree.definitions
        .classes
        .insert("ConstantPropertyLiquidWater".to_string(), water);
    for (name, def_id) in [
        ("PartialMedium", partial_medium_id),
        ("PartialMedium.ThermodynamicState", partial_state_id),
        ("PartialSimpleMedium", simple_medium_id),
        ("PartialSimpleMedium.ThermodynamicState", simple_state_id),
        ("ConstantPropertyLiquidWater", water_id),
    ] {
        tree.name_map.insert(name.to_string(), def_id);
        tree.def_map.insert(def_id, name.to_string());
    }

    let class = tree
        .get_class_by_qualified_name("ConstantPropertyLiquidWater")
        .expect("derived class should exist");
    let inherited = process_extends(&tree, class)
        .expect("redeclared nested class should shadow inherited replaceable class");
    let effective_state = inherited
        .classes
        .get("ThermodynamicState")
        .expect("effective nested class should exist");
    assert_eq!(effective_state.def_id, Some(simple_state_id));
}

// -------------------------------------------------------------------------
// is_type_subtype tests
// -------------------------------------------------------------------------

#[test]
fn test_is_type_subtype_exact_match() {
    let tree = ast::ClassTree::default();
    assert!(is_type_subtype(&tree, "Real", "Real"));
    assert!(is_type_subtype(&tree, "MyClass", "MyClass"));
}

#[test]
fn test_is_type_subtype_builtin_mismatch() {
    let tree = ast::ClassTree::default();
    assert!(!is_type_subtype(&tree, "Real", "Integer"));
    assert!(!is_type_subtype(&tree, "Boolean", "String"));
}

#[test]
fn test_is_type_subtype_via_extends() {
    let mut tree = ast::ClassTree::default();

    // A extends nothing
    let class_a = ast::ClassDef {
        name: make_token("A"),
        ..Default::default()
    };

    // B extends A
    let class_b = ast::ClassDef {
        name: make_token("B"),
        extends: vec![ast::Extend {
            base_name: make_name("A"),
            ..Default::default()
        }],
        ..Default::default()
    };

    // C extends B (transitive: C -> B -> A)
    let class_c = ast::ClassDef {
        name: make_token("C"),
        extends: vec![ast::Extend {
            base_name: make_name("B"),
            ..Default::default()
        }],
        ..Default::default()
    };

    tree.definitions.classes.insert("A".to_string(), class_a);
    tree.definitions.classes.insert("B".to_string(), class_b);
    tree.definitions.classes.insert("C".to_string(), class_c);

    // B is subtype of A
    assert!(is_type_subtype(&tree, "B", "A"));
    // C is subtype of B
    assert!(is_type_subtype(&tree, "C", "B"));
    // C is subtype of A (transitive)
    assert!(is_type_subtype(&tree, "C", "A"));
    // A is NOT subtype of B
    assert!(!is_type_subtype(&tree, "A", "B"));
}

#[test]
fn test_class_extends_cached_matches_base_def_id_for_relative_extends_name() {
    use rumoca_core::DefId;

    let mut tree = ast::ClassTree::default();

    let c_id = DefId::new(1);
    let interfaces_id = DefId::new(2);
    let d_id = DefId::new(3);
    let pkg_id = DefId::new(4);
    let root_id = DefId::new(5);

    let class_c = ast::ClassDef {
        def_id: Some(c_id),
        name: make_token("C"),
        ..Default::default()
    };

    let mut class_interfaces = ast::ClassDef {
        def_id: Some(interfaces_id),
        name: make_token("Interfaces"),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    class_interfaces
        .classes
        .insert("C".to_string(), class_c.clone());

    let class_d = ast::ClassDef {
        def_id: Some(d_id),
        name: make_token("D"),
        extends: vec![ast::Extend {
            // Relative name intentionally omits top-level prefix.
            base_name: make_name("Pkg.Interfaces.C"),
            base_def_id: Some(c_id),
            ..Default::default()
        }],
        ..Default::default()
    };

    let mut class_pkg = ast::ClassDef {
        def_id: Some(pkg_id),
        name: make_token("Pkg"),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    class_pkg
        .classes
        .insert("Interfaces".to_string(), class_interfaces);
    class_pkg.classes.insert("D".to_string(), class_d);

    let mut class_root = ast::ClassDef {
        def_id: Some(root_id),
        name: make_token("Root"),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    class_root.classes.insert("Pkg".to_string(), class_pkg);
    tree.definitions
        .classes
        .insert("Root".to_string(), class_root);

    tree.def_map
        .insert(c_id, "Root.Pkg.Interfaces.C".to_string());
    tree.def_map
        .insert(interfaces_id, "Root.Pkg.Interfaces".to_string());
    tree.def_map.insert(d_id, "Root.Pkg.D".to_string());
    tree.def_map.insert(pkg_id, "Root.Pkg".to_string());
    tree.def_map.insert(root_id, "Root".to_string());

    tree.name_map
        .insert("Root.Pkg.Interfaces.C".to_string(), c_id);
    tree.name_map
        .insert("Root.Pkg.Interfaces".to_string(), interfaces_id);
    tree.name_map.insert("Root.Pkg.D".to_string(), d_id);
    tree.name_map.insert("Root.Pkg".to_string(), pkg_id);
    tree.name_map.insert("Root".to_string(), root_id);

    let d_class = tree
        .get_class_by_qualified_name("Root.Pkg.D")
        .expect("Root.Pkg.D class should exist");
    let mut cache = SubtypeCache::default();
    assert!(
        class_extends_cached(&tree, d_class, "Root.Pkg.Interfaces.C", &mut cache),
        "relative extends with base_def_id should match the resolved queried supertype"
    );
    let mut cache = SubtypeCache::default();
    assert!(
        !class_extends_cached(&tree, d_class, "Interfaces.C", &mut cache),
        "unresolved short supertype names must not match by suffix"
    );
}

#[test]
fn test_type_names_match_requires_resolved_identity() {
    use rumoca_core::DefId;

    let mut tree = ast::ClassTree::default();
    let def_id = DefId::new(1);
    tree.name_map
        .insert("Modelica.Units.SI.Resistance".to_string(), def_id);
    tree.name_map.insert("SI.Resistance".to_string(), def_id);
    tree.def_map
        .insert(def_id, "Modelica.Units.SI.Resistance".to_string());

    assert!(type_names_match(
        &tree,
        "Modelica.Units.SI.Resistance",
        "SI.Resistance"
    ));
    assert!(!type_names_match(
        &tree,
        "Modelica.Units.SI.Resistance",
        "Resistance"
    ));
    assert!(!type_names_match(
        &tree,
        "Modelica.Units.SI.Resistance",
        "stance"
    ));
}

#[test]
fn test_extract_modification_target_modification() {
    // Test extracting target from ast::Expression::Modification
    let expr = ast::Expression::Modification {
        target: make_component_ref("myVar"),
        value: Arc::new(ast::Expression::Empty {
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(
        extract_modification_target(&expr),
        Some("myVar".to_string())
    );
}

#[test]
fn test_extract_modification_target_class_modification() {
    // Test extracting target from ast::Expression::ClassModification
    let expr = ast::Expression::ClassModification {
        target: make_component_ref("myClass"),
        modifications: vec![],
        each_flags: vec![],
        final_flags: vec![],
        redeclare_flags: vec![],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(
        extract_modification_target(&expr),
        Some("myClass".to_string())
    );
}

#[test]
fn test_extract_modification_target_named_argument() {
    // Test extracting target from ast::Expression::NamedArgument
    let expr = ast::Expression::NamedArgument {
        name: make_token("param"),
        value: Arc::new(ast::Expression::Empty {
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(
        extract_modification_target(&expr),
        Some("param".to_string())
    );
}

#[test]
fn test_is_effectively_primitive_transitive_enumeration_chain() {
    // Test that type alias chains leading to enumerations are detected as primitive
    // type Logic = enumeration(...)
    // connector DigitalSignal = Logic
    // connector DigitalInput = input DigitalSignal

    // Create a tree with these classes
    let mut tree = ast::ClassTree::new();

    // Logic enumeration
    let mut logic = ast::ClassDef {
        name: make_token("Logic"),
        ..Default::default()
    };
    logic.enum_literals.push(ast::EnumLiteral {
        ident: make_token("U"),
        description: vec![],
    });
    logic.enum_literals.push(ast::EnumLiteral {
        ident: make_token("X"),
        description: vec![],
    });

    // DigitalSignal = Logic
    let digital_signal = ast::ClassDef {
        name: make_token("DigitalSignal"),
        extends: vec![ast::Extend {
            base_name: make_name("Logic"),
            ..Default::default()
        }],
        ..Default::default()
    };

    // DigitalInput = input DigitalSignal
    let digital_input = ast::ClassDef {
        name: make_token("DigitalInput"),
        extends: vec![ast::Extend {
            base_name: make_name("DigitalSignal"),
            ..Default::default()
        }],
        ..Default::default()
    };

    tree.definitions.classes.insert("Logic".to_string(), logic);
    tree.definitions
        .classes
        .insert("DigitalSignal".to_string(), digital_signal);
    tree.definitions
        .classes
        .insert("DigitalInput".to_string(), digital_input.clone());

    // DigitalInput should be effectively primitive because it chains to Logic (enumeration)
    assert!(is_effectively_primitive_transitive(&tree, &digital_input));
}
