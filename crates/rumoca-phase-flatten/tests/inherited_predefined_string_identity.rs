//! The predefined `String(...)` conversion keeps its Resolve declaration
//! identity when an algorithm section is inherited into a concrete instance.

use rumoca_core::{Expression, Statement};
use rumoca_ir_ast as ast;

const SOURCE_NAME: &str = "<inherited_predefined_string_identity>";

fn typed_flat_model(
    source: &str,
    model_name: &str,
) -> (rumoca_ir_flat::Model, rumoca_core::SourceMap) {
    let instanced = instanced_tree(source, model_name);
    let ast::InstancedTree { tree, mut overlay } = instanced;
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, model_name)
        .expect("instanced model typechecks");
    let source_map = tree.source_map.clone();
    let flat = rumoca_phase_flatten::flatten_ref(&tree, &overlay, model_name)
        .expect("typed model flattens");
    (flat, source_map)
}

fn instanced_tree(source: &str, model_name: &str) -> ast::InstancedTree {
    let stored = rumoca_phase_parse::parse_to_ast(source, SOURCE_NAME).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    rumoca_phase_instantiate::instantiate(resolved, model_name).expect("model instantiates")
}

fn sole_initial_assert(model: &rumoca_ir_flat::Model) -> (&Expression, &Expression) {
    let [algorithm] = model.initial_algorithms.as_slice() else {
        panic!("expected one inherited initial algorithm");
    };
    let [Statement::FunctionCall { args, .. }] = algorithm.statements.as_slice() else {
        panic!("expected one inherited initial assert call");
    };
    let [condition, message] = args.as_slice() else {
        panic!("expected assert condition and message");
    };
    (condition, message)
}

#[test]
fn inherited_initial_assert_lowers_exact_predefined_string_conversion() {
    let source = r#"
package P
  partial model Base
    parameter Integer m = 3;
  initial algorithm
    assert(m > 0, String(m) + " phases");
  end Base;

  model Derived
    extends Base;
  end Derived;
end P;
"#;
    let (flat, source_map) = typed_flat_model(source, "P.Derived");
    let (_, message) = sole_initial_assert(&flat);
    let Expression::Binary { lhs, .. } = message else {
        panic!("expected concatenated assertion message");
    };
    let Expression::StringConversion { declaration, .. } = lhs.as_ref() else {
        panic!("the predefined call must have a dedicated Flat owner");
    };
    assert_eq!(Some(*declaration), flat.predefined_string_declaration);

    rumoca_phase_dae::to_dae(&flat, source_map)
        .expect("the inherited predefined conversion lowers to DAE");
}

#[test]
fn inherited_shadowing_string_function_remains_an_exact_user_call() {
    let source = r#"
package P
  function String
    input Integer value;
    output Integer result;
  algorithm
    result := value;
  end String;

  partial model Base
    parameter Integer m = 3;
  initial algorithm
    assert(String(m) > 0, "positive");
  end Base;

  model Derived
    extends Base;
  end Derived;
end P;
"#;
    let (flat, source_map) = typed_flat_model(source, "P.Derived");
    let (condition, _) = sole_initial_assert(&flat);
    let Expression::Binary { lhs, .. } = condition else {
        panic!("expected relational assertion condition");
    };
    let Expression::FunctionCall { name, .. } = lhs.as_ref() else {
        panic!("a shadowing user declaration must remain a function call");
    };
    assert_ne!(name.target_def_id(), flat.predefined_string_declaration);

    rumoca_phase_dae::to_dae(&flat, source_map)
        .expect("the exact shadowing user call lowers through its function owner");
}

#[test]
fn inherited_initial_string_and_lexical_component_modifier_keep_identities() {
    let source = r#"
package P
  function convertAlpha
    input Real alpha1;
    input Real temperature;
    output Real alpha2;
  algorithm
    alpha2 := alpha1 / temperature;
  end convertAlpha;

  model Resistor
    parameter Real alpha;
  end Resistor;

  partial model Base
    parameter Integer m = 3;
    parameter Real alpha20 = 0.1;
    parameter Real referenceTemperature = 293.15;
    Resistor resistor(
      final alpha=convertAlpha(alpha20, referenceTemperature));
  initial algorithm
    assert(m > 0, String(m) + " phases");
  end Base;

  model Derived
    extends Base;
  end Derived;
end P;
"#;
    let (flat, source_map) = typed_flat_model(source, "P.Derived");

    let alpha = flat
        .variables
        .values()
        .find(|variable| variable.name.as_str().ends_with("resistor.alpha"))
        .expect("the modified nested parameter is flattened");
    let Expression::FunctionCall { args, .. } = alpha
        .binding
        .as_ref()
        .expect("the nested parameter keeps its modifier binding")
    else {
        panic!("expected the lexical modifier function call");
    };
    assert!(args.iter().all(|argument| {
        matches!(
            argument,
            Expression::VarRef { name, .. } if name.target_def_id().is_some()
        )
    }));

    rumoca_phase_dae::to_dae(&flat, source_map)
        .expect("the inherited algorithm and lexical modifier lower to DAE");
}

#[test]
fn nested_modified_inherited_machine_keeps_internal_modifier_identities() {
    let source = r#"
package P
  function convertAlpha
    input Real alpha1;
    input Real temperature;
    output Real alpha2;
  algorithm
    alpha2 := alpha1 / temperature;
  end convertAlpha;

  record MachineData
    parameter Real alpha20 = 0.2;
  end MachineData;

  model Resistor
    parameter Real alpha;
  end Resistor;

  model ThermalAmbient
    parameter Real temperature = 293.15;
  end ThermalAmbient;

  model SpecificThermalAmbient
    extends ThermalAmbient;
  end SpecificThermalAmbient;

  partial model BaseMachine
    parameter Integer m = 3;
    parameter Boolean useThermalPort = false;
    parameter Real alpha20 = 0.1;
    parameter Real referenceTemperature = 293.15;
    parameter Real nominalValue;
    Resistor resistor(
      final alpha=convertAlpha(alpha20, referenceTemperature));
    replaceable ThermalAmbient thermalAmbient if not useThermalPort;
  initial algorithm
    assert(m > 0, String(m) + " phases");
  end BaseMachine;

  model Machine
    extends BaseMachine(
      final nominalValue=convertAlpha(alpha20, referenceTemperature),
      redeclare final SpecificThermalAmbient thermalAmbient);
  end Machine;

  model Example
    parameter MachineData data(
      alpha20(displayUnit="1/K")=0.2);
    parameter MachineData defaultData(
      alpha20(displayUnit="1/K"));
    Machine machine(alpha20=data.alpha20, useThermalPort=true);
  end Example;
end P;
"#;
    let ast::InstancedTree { tree, mut overlay } = instanced_tree(source, "P.Example");
    let projected_field = overlay
        .components
        .values()
        .find(|instance| {
            instance
                .qualified_name
                .to_flat_string()
                .ends_with("data.alpha20")
        })
        .expect("Instantiate projects the modified record field");
    assert!(matches!(
        projected_field.binding_source.as_ref(),
        Some(ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedReal,
            token,
            ..
        }) if token.text.as_ref() == "0.2"
    ));
    let default_field = overlay
        .components
        .values()
        .find(|instance| {
            instance
                .qualified_name
                .to_flat_string()
                .ends_with("defaultData.alpha20")
        })
        .expect("Instantiate projects the attribute-only record field");
    assert!(default_field.binding_source.is_none());
    assert!(matches!(
        default_field.binding.as_ref(),
        Some(ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedReal,
            token,
            ..
        }) if token.text.as_ref() == "0.2"
    ));

    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, "P.Example")
        .expect("instanced model typechecks");
    let source_map = tree.source_map.clone();
    let flat = rumoca_phase_flatten::flatten_ref(&tree, &overlay, "P.Example")
        .expect("typed model flattens");

    let record_field = flat
        .variables
        .values()
        .find(|variable| variable.name.as_str().ends_with("data.alpha20"))
        .expect("the modified record field is flattened");
    assert!(matches!(
        record_field.binding.as_ref(),
        Some(Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        }) if *value == 0.2
    ));
    let default_record_field = flat
        .variables
        .values()
        .find(|variable| variable.name.as_str().ends_with("defaultData.alpha20"))
        .expect("the attribute-only record field is flattened");
    assert!(matches!(
        default_record_field.binding.as_ref(),
        Some(Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        }) if *value == 0.2
    ));

    let alpha = flat
        .variables
        .values()
        .find(|variable| variable.name.as_str().ends_with("machine.resistor.alpha"))
        .expect("the internally modified nested parameter is flattened");
    let Expression::FunctionCall { args, .. } = alpha
        .binding
        .as_ref()
        .expect("the internal modifier binding survives")
    else {
        panic!("expected the internal lexical modifier function call");
    };
    assert!(args.iter().all(|argument| {
        matches!(
            argument,
            Expression::VarRef { name, .. } if name.target_def_id().is_some()
        )
    }));

    rumoca_phase_dae::to_dae(&flat, source_map)
        .expect("the nested inherited algorithm and modifier lower to DAE");
}
