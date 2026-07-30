//! Redeclared function members carry declaration and redeclare
//! modifier actuals to the rewritten call.

use super::*;

struct FlowCharacteristicFixture {
    tree: ClassTree,
    override_functions: OverrideFunctionMap,
    modelica: DefId,
    fluid: DefId,
    machines: DefId,
    base_classes: DefId,
    partial_pump: DefId,
    quadratic: DefId,
    pump: DefId,
    volume_flow: DefId,
}

fn flow_characteristic_fixture() -> FlowCharacteristicFixture {
    let quadratic_def = DefId::new(1);
    let partial_pump_def = DefId::new(2);
    let modelica_def = DefId::new(3);
    let fluid_def = DefId::new(4);
    let machines_def = DefId::new(5);
    let base_classes_def = DefId::new(6);
    let flow_characteristic_def = DefId::new(7);
    let pump_def = DefId::new(8);
    let volume_flow_def = DefId::new(9);
    let volume_flow_operating_point_def = DefId::new(10);
    let mut quadratic = class("quadraticFlow", ClassType::Function);
    quadratic.def_id = Some(quadratic_def);
    let mut partial_pump = class("PartialPump", ClassType::Model);
    partial_pump.def_id = Some(partial_pump_def);
    let mut flow_characteristic = class("flowCharacteristic", ClassType::Function);
    flow_characteristic.def_id = Some(flow_characteristic_def);
    flow_characteristic.extends.push(Extend {
        base_name: Name::from_string("quadraticFlow"),
        base_def_id: Some(quadratic_def),
        ..Extend::default()
    });
    partial_pump
        .classes
        .insert("flowCharacteristic".to_string(), flow_characteristic);
    let mut volume_flow_operating_point =
        component("V_flow_op", "Real", rumoca_core::DefId::new(90));
    volume_flow_operating_point.def_id = Some(volume_flow_operating_point_def);
    partial_pump
        .components
        .insert("V_flow_op".to_string(), volume_flow_operating_point);

    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("quadraticFlow".to_string(), quadratic);
    tree.definitions
        .classes
        .insert("PartialPump".to_string(), partial_pump);
    tree.def_map
        .insert(quadratic_def, "quadraticFlow".to_string());
    tree.def_map.insert(
        partial_pump_def,
        "Modelica.Fluid.Machines.BaseClasses.PartialPump".to_string(),
    );
    tree.def_map.insert(
        volume_flow_operating_point_def,
        "Modelica.Fluid.Machines.BaseClasses.PartialPump.V_flow_op".to_string(),
    );
    tree.name_map
        .insert("quadraticFlow".to_string(), quadratic_def);

    let mut override_functions = OverrideFunctionMap::default();
    override_functions.insert(
        "flowCharacteristic".to_string(),
        OverrideTarget {
            alias: "flowCharacteristic".to_string(),
            name: "quadraticFlow".to_string(),
            def_id: quadratic_def,
            class_type: ClassType::Function,
            active: true,
            modifier_args: vec![FunctionModifierArg {
                name: "V_flow_nominal".to_string(),
                value: resolved_ast_var(&[("V_flow_op", volume_flow_operating_point_def)]),
                span: test_span(),
            }],
        },
    );
    FlowCharacteristicFixture {
        tree,
        override_functions,
        modelica: modelica_def,
        fluid: fluid_def,
        machines: machines_def,
        base_classes: base_classes_def,
        partial_pump: partial_pump_def,
        quadratic: quadratic_def,
        pump: pump_def,
        volume_flow: volume_flow_def,
    }
}

#[test]
fn redeclare_function_assignment_rewrites_call_with_modifier_actuals() {
    let fixture = flow_characteristic_fixture();
    let tree = fixture.tree;
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let ctx =
        FunctionOverrideRewriteContext::new(&tree, &class_index, &[], &fixture.override_functions)
            .with_active_scope(ComponentPath::from_flat_path("pump"));
    let mut expr = Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            "Modelica.Fluid.Machines.BaseClasses.PartialPump.flowCharacteristic",
            core_comp_ref(&[
                ("Modelica", fixture.modelica),
                ("Fluid", fixture.fluid),
                ("Machines", fixture.machines),
                ("BaseClasses", fixture.base_classes),
                ("PartialPump", fixture.partial_pump),
                ("flowCharacteristic", fixture.quadratic),
            ]),
        ),
        args: vec![core_var(&[
            ("pump", fixture.pump),
            ("V_flow_single", fixture.volume_flow),
        ])],
        is_constructor: false,
        span: test_span(),
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx)
        .expect("function override rewrite");

    let Expression::FunctionCall { name, args, .. } = expr else {
        panic!("expected rewritten function call");
    };
    assert_eq!(
        name.as_str(),
        "Modelica.Fluid.Machines.BaseClasses.PartialPump.flowCharacteristic"
    );
    assert_eq!(args.len(), 2);
    let Some(("V_flow_nominal", Expression::VarRef { name, .. })) = named_arg(&args[1]) else {
        panic!("expected receiver-qualified V_flow_nominal named argument");
    };
    assert_eq!(name.as_str(), "pump.V_flow_op");
}

#[test]
fn inherited_replaceable_function_call_keeps_declaration_modifier_actuals() {
    let (tree, ids) = replaceable_efficiency_fixture();
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let override_functions = OverrideFunctionMap::default();
    let ctx = FunctionOverrideRewriteContext::new(&tree, &class_index, &[], &override_functions)
        .with_active_scope(ComponentPath::from_flat_path("pump"));
    let mut expr = Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            "Modelica.Fluid.Machines.BaseClasses.PartialPump.efficiencyCharacteristic",
            core_comp_ref(&[
                ("Modelica", ids.modelica),
                ("Fluid", ids.fluid),
                ("Machines", ids.machines),
                ("BaseClasses", ids.base_classes),
                ("PartialPump", ids.partial_pump),
                ("efficiencyCharacteristic", ids.constant_efficiency),
            ]),
        ),
        args: vec![core_var(&[
            ("pump", DefId::new(10)),
            ("V_flow_single", DefId::new(11)),
        ])],
        is_constructor: false,
        span: test_span(),
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx)
        .expect("function override rewrite");

    let Expression::FunctionCall { name, args, .. } = expr else {
        panic!("expected rewritten function call");
    };
    assert_eq!(
        name.as_str(),
        "Modelica.Fluid.Machines.BaseClasses.PartialPump.efficiencyCharacteristic"
    );
    assert_eq!(args.len(), 2);
    let Some(("eta_nominal", Expression::Literal { value, .. })) = named_arg(&args[1]) else {
        panic!("expected eta_nominal declaration modifier argument");
    };
    assert_eq!(*value, rumoca_core::Literal::Real(0.8));
}

#[derive(Clone, Copy)]
struct ReplaceableEfficiencyIds {
    base_efficiency: DefId,
    constant_efficiency: DefId,
    efficiency_characteristic: DefId,
    modelica: DefId,
    fluid: DefId,
    machines: DefId,
    base_classes: DefId,
    pump_characteristics: DefId,
    partial_pump: DefId,
}

fn replaceable_efficiency_fixture() -> (ClassTree, ReplaceableEfficiencyIds) {
    let ids = ReplaceableEfficiencyIds {
        base_efficiency: DefId::new(1),
        constant_efficiency: DefId::new(2),
        efficiency_characteristic: DefId::new(3),
        modelica: DefId::new(4),
        fluid: DefId::new(5),
        machines: DefId::new(6),
        base_classes: DefId::new(7),
        pump_characteristics: DefId::new(8),
        partial_pump: DefId::new(9),
    };

    let pump_characteristics = replaceable_efficiency_pump_characteristics(ids);
    let partial_pump = replaceable_efficiency_partial_pump(ids);
    let modelica = replaceable_efficiency_modelica_tree(ids, pump_characteristics, partial_pump);
    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("Modelica".to_string(), modelica);
    register_replaceable_efficiency_names(&mut tree, ids);
    (tree, ids)
}

fn replaceable_efficiency_pump_characteristics(ids: ReplaceableEfficiencyIds) -> ClassDef {
    let mut base_efficiency = class("baseEfficiency", ClassType::Function);
    base_efficiency.def_id = Some(ids.base_efficiency);
    let mut constant_efficiency = class("constantEfficiency", ClassType::Function);
    constant_efficiency.def_id = Some(ids.constant_efficiency);
    constant_efficiency
        .algorithms
        .push(vec![rumoca_ir_ast::Statement::Return {
            token: token("return"),
        }]);
    let mut pump_characteristics = class("PumpCharacteristics", ClassType::Package);
    pump_characteristics.def_id = Some(ids.pump_characteristics);
    pump_characteristics
        .classes
        .insert("baseEfficiency".to_string(), base_efficiency);
    pump_characteristics
        .classes
        .insert("constantEfficiency".to_string(), constant_efficiency);
    pump_characteristics
}

fn replaceable_efficiency_partial_pump(ids: ReplaceableEfficiencyIds) -> ClassDef {
    let mut efficiency_characteristic = class("efficiencyCharacteristic", ClassType::Function);
    efficiency_characteristic.def_id = Some(ids.efficiency_characteristic);
    efficiency_characteristic.extends.push(Extend {
        base_name: Name {
            def_id: Some(ids.constant_efficiency),
            ..Name::from_string("PumpCharacteristics.constantEfficiency")
        },
        base_def_id: Some(ids.constant_efficiency),
        modifications: vec![eta_nominal_modifier()],
        ..Extend::default()
    });

    let mut partial_pump = class("PartialPump", ClassType::Model);
    partial_pump.def_id = Some(ids.partial_pump);
    partial_pump.classes.insert(
        "efficiencyCharacteristic".to_string(),
        efficiency_characteristic,
    );
    partial_pump
}

fn eta_nominal_modifier() -> rumoca_ir_ast::ExtendModification {
    rumoca_ir_ast::ExtendModification {
        expr: rumoca_ir_ast::Expression::NamedArgument {
            name: token("eta_nominal"),
            value: Arc::new(rumoca_ir_ast::Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedReal,
                token: token("0.8"),
                span: test_span(),
            }),
            span: test_span(),
        },
        each: false,
        final_: false,
        redeclare: false,
    }
}

fn replaceable_efficiency_modelica_tree(
    ids: ReplaceableEfficiencyIds,
    pump_characteristics: ClassDef,
    partial_pump: ClassDef,
) -> ClassDef {
    let mut base_classes = class("BaseClasses", ClassType::Package);
    base_classes.def_id = Some(ids.base_classes);
    base_classes
        .classes
        .insert("PumpCharacteristics".to_string(), pump_characteristics);
    base_classes
        .classes
        .insert("PartialPump".to_string(), partial_pump);
    let mut machines = class("Machines", ClassType::Package);
    machines.def_id = Some(ids.machines);
    machines
        .classes
        .insert("BaseClasses".to_string(), base_classes);
    let mut fluid = class("Fluid", ClassType::Package);
    fluid.def_id = Some(ids.fluid);
    fluid.classes.insert("Machines".to_string(), machines);
    let mut modelica = class("Modelica", ClassType::Package);
    modelica.def_id = Some(ids.modelica);
    modelica.classes.insert("Fluid".to_string(), fluid);
    modelica
}

fn register_replaceable_efficiency_names(tree: &mut ClassTree, ids: ReplaceableEfficiencyIds) {
    for (def_id, name) in [
        (ids.modelica, "Modelica"),
        (ids.fluid, "Modelica.Fluid"),
        (ids.machines, "Modelica.Fluid.Machines"),
        (ids.base_classes, "Modelica.Fluid.Machines.BaseClasses"),
        (
            ids.pump_characteristics,
            "Modelica.Fluid.Machines.BaseClasses.PumpCharacteristics",
        ),
        (
            ids.partial_pump,
            "Modelica.Fluid.Machines.BaseClasses.PartialPump",
        ),
        (
            ids.base_efficiency,
            "Modelica.Fluid.Machines.BaseClasses.PumpCharacteristics.baseEfficiency",
        ),
        (
            ids.constant_efficiency,
            "Modelica.Fluid.Machines.BaseClasses.PumpCharacteristics.constantEfficiency",
        ),
        (
            ids.efficiency_characteristic,
            "Modelica.Fluid.Machines.BaseClasses.PartialPump.efficiencyCharacteristic",
        ),
    ] {
        tree.def_map.insert(def_id, name.to_string());
    }
    tree.name_map.insert(
        "Modelica.Fluid.Machines.BaseClasses.PumpCharacteristics.constantEfficiency".to_string(),
        ids.constant_efficiency,
    );
}

#[test]
fn component_scope_inherits_type_extends_redeclare_function() {
    let partial_def = DefId::new(1);
    let controlled_def = DefId::new(2);
    let quadratic_def = DefId::new(3);
    let flow_characteristic_def = DefId::new(4);

    let mut partial = class("PartialPump", ClassType::Model);
    partial.def_id = Some(partial_def);
    let mut controlled = class("ControlledPump", ClassType::Model);
    controlled.def_id = Some(controlled_def);
    let mut quadratic = class("quadraticFlow", ClassType::Function);
    quadratic.def_id = Some(quadratic_def);

    let quadratic_ref = resolved_comp_ref(&[("quadraticFlow", quadratic_def)]);
    controlled.extends.push(Extend {
        base_name: Name {
            def_id: Some(partial_def),
            ..Name::from_string("PartialPump")
        },
        base_def_id: Some(partial_def),
        modifications: vec![rumoca_ir_ast::ExtendModification {
            expr: rumoca_ir_ast::Expression::Binary {
                op: rumoca_core::OpBinary::Assign,
                lhs: Arc::new(rumoca_ir_ast::Expression::ComponentReference(
                    resolved_comp_ref(&[("flowCharacteristic", flow_characteristic_def)]),
                )),
                rhs: Arc::new(rumoca_ir_ast::Expression::FunctionCall {
                    comp: quadratic_ref,
                    args: vec![rumoca_ir_ast::Expression::NamedArgument {
                        name: token("V_flow_nominal"),
                        value: Arc::new(ast_var("V_flow_op")),
                        span: test_span(),
                    }],
                    is_partial_application: false,
                    span: test_span(),
                }),
                span: test_span(),
            },
            each: false,
            final_: false,
            redeclare: true,
        }],
        ..Extend::default()
    });

    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("PartialPump".to_string(), partial);
    tree.definitions
        .classes
        .insert("ControlledPump".to_string(), controlled);
    tree.definitions
        .classes
        .insert("quadraticFlow".to_string(), quadratic);
    tree.def_map.insert(partial_def, "PartialPump".to_string());
    tree.def_map
        .insert(controlled_def, "ControlledPump".to_string());
    tree.def_map
        .insert(quadratic_def, "quadraticFlow".to_string());
    tree.name_map
        .insert("quadraticFlow".to_string(), quadratic_def);

    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let mut overlay = InstanceOverlay::new();
    let pump_id = overlay.alloc_id();
    overlay.add_component(rumoca_ir_ast::InstanceData {
        instance_id: pump_id,
        qualified_name: QualifiedName::from_ident("pump"),
        type_def_id: Some(controlled_def),
        ..rumoca_ir_ast::InstanceData::default()
    });

    let override_map =
        build_component_override_map(&overlay, &tree, &class_index, "ControlledPump")
            .expect("component override map");
    let (_, override_functions) = override_context_for_scope("pump", &override_map);
    let target = override_functions
        .get("flowCharacteristic")
        .expect("expected ControlledPump redeclare in pump scope");
    assert_eq!(target.name, "quadraticFlow");
    assert_eq!(target.modifier_args.len(), 1);
    assert_eq!(target.modifier_args[0].name, "V_flow_nominal");
}
