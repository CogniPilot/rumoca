//! MLS §9.2: an outside discrete input is a connection source.
use rumoca::Compiler;
use rumoca_ir_dae as dae;

const SOURCE: &str = r#"
package BooleanConnection
  connector BooleanInput = input Boolean;
  model Consumer
    BooleanInput u;
    Boolean y;
  equation
    y = not u;
  end Consumer;
  model Top
    BooleanInput u;
    Consumer consumer;
  equation
    connect(u, consumer.u);
  end Top;
end BooleanConnection;
"#;

fn compile(source: &str) -> std::sync::Arc<dae::Dae> {
    Compiler::new()
        .model("BooleanConnection.Top")
        .compile_str(source, "BooleanConnection.mo")
        .expect("discrete potential equality must not be lowered as arithmetic")
        .dae
}

/// Inspect RHS reads, preserving explicit array element order; never execute the model.
fn definitions(model: &dae::Dae) -> Vec<(String, Vec<String>)> {
    model.inspect(definitions_view)
}

fn definitions_view<'dae>(view: dae::DaeView<'dae>) -> Vec<(String, Vec<String>)> {
    let mut definitions = Vec::new();
    for index in 0..view.discrete_value_owner_count() {
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(index).unwrap())
            .unwrap();
        for branch in owner.branches().iter() {
            for (target, (value, _)) in owner.targets().iter().zip(branch.values().iter()) {
                let target = view.variable(target.variable().into()).unwrap();
                assert_ne!(
                    target.role(),
                    dae::VariableRole::Input,
                    "inputs are read-only"
                );
                let mut reads = Vec::new();
                if let dae::ExpressionOperation::Array(elements) =
                    view.expression(value).unwrap().operation()
                {
                    for element in elements.iter() {
                        let source = view
                            .expression(element)
                            .unwrap()
                            .variable_coordinate()
                            .expect("each array element is a direct producer read");
                        reads.push(view.variable(source).unwrap().name().to_string());
                    }
                } else {
                    dae::for_each_expression(
                        view,
                        value,
                        &mut |_, expression: dae::ExpressionView<'dae>| {
                            if let Some(source) = expression.variable_coordinate() {
                                reads.push(view.variable(source).unwrap().name().to_string());
                            }
                        },
                    );
                }
                definitions.push((target.name().to_string(), reads));
            }
        }
    }
    definitions
}

fn assert_input_copy(source: &str) {
    let model = compile(source);
    let definitions = definitions(&model);
    assert!(
        definitions.contains(&("consumer.u".into(), vec!["u".into()])),
        "connection must copy the boundary input into the consumer: {definitions:?}"
    );
}

#[test]
fn boolean_connection_preserves_typed_discrete_definition() {
    assert_input_copy(SOURCE);
}

#[test]
fn reversed_boolean_connection_still_reads_the_boundary_input() {
    assert_input_copy(&SOURCE.replace("connect(u, consumer.u)", "connect(consumer.u, u)"));
}

#[test]
fn other_discrete_input_types_are_connection_sources() {
    for scalar_type in ["Integer", "String"] {
        let source = SOURCE.replace("Boolean", scalar_type).replace("not u", "u");
        // Keep the package name used by the shared compiler helper.
        assert_input_copy(
            &source.replace(&format!("{scalar_type}Connection"), "BooleanConnection"),
        );
    }
    let source = SOURCE
        .replace(
            "connector BooleanInput",
            "type Mode = enumeration(off, on);\n  connector BooleanInput",
        )
        .replace("input Boolean", "input Mode")
        .replace("Boolean y", "Mode y")
        .replace("not u", "u");
    assert_input_copy(&source);
}

#[test]
fn element_connections_keep_producer_order_through_fanout() {
    let source = r#"
package BooleanConnection
  connector In = input Boolean;
  model Consumer
    In u[2];
    Boolean y[2];
  equation
    y = not u;
  end Consumer;
  model Top
    In a;
    In b;
    Consumer first;
    Consumer second;
  equation
    connect(a, first.u[2]);
    connect(first.u[1], b);
    connect(first.u[1], second.u[1]);
    connect(second.u[2], first.u[2]);
  end Top;
end BooleanConnection;
"#;
    let model = compile(source);
    let definitions = definitions(&model);
    for target in ["first.u", "second.u"] {
        assert!(
            definitions.contains(&(target.into(), vec!["b".into(), "a".into()])),
            "row-major input mapping for {target}: {definitions:?}"
        );
    }
}

#[test]
fn real_input_connections_remain_continuous() {
    let source = SOURCE
        .replace("Boolean", "Real")
        .replace("not u", "2 * u")
        .replace("RealConnection", "BooleanConnection");
    let model = compile(&source);
    assert!(definitions(&model).is_empty());
    model.inspect(|view| assert!(view.continuous_owners().count() > 0));
}

#[test]
fn external_array_input_keeps_the_selected_element() {
    let source = SOURCE
        .replace(
            "    BooleanInput u;\n    Consumer consumer;",
            "    BooleanInput u[2];\n    Consumer consumer;\n    Consumer other;",
        )
        .replace(
            "connect(u, consumer.u);",
            "connect(u[2], consumer.u);\n    connect(other.u, u[1]);",
        );
    let model = compile(&source);
    let mut selections = Vec::new();
    model.inspect(|view| {
        for index in 0..view.discrete_value_owner_count() {
            let owner = view
                .discrete_value_owner(view.discrete_value_owner_id(index).unwrap())
                .unwrap();
            for branch in owner.branches().iter() {
                for (target, (value, _)) in owner.targets().iter().zip(branch.values().iter()) {
                    let target = view.variable(target.variable().into()).unwrap();
                    assert_ne!(target.role(), dae::VariableRole::Input);
                    let dae::ExpressionOperation::Index { base, subscripts } =
                        view.expression(value).unwrap().operation()
                    else {
                        continue;
                    };
                    let source = view
                        .expression(base)
                        .unwrap()
                        .variable_coordinate()
                        .unwrap();
                    assert_eq!(view.variable(source).unwrap().name().as_str(), "u");
                    for subscript in subscripts.iter() {
                        let dae::SubscriptView::Index { expression, .. } = subscript else {
                            panic!("literal element selection");
                        };
                        let dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(element)) =
                            view.expression(expression).unwrap().operation()
                        else {
                            panic!("literal element index");
                        };
                        selections.push((target.name().to_string(), *element));
                    }
                }
            }
        }
    });
    selections.sort();
    assert_eq!(
        selections,
        vec![("consumer.u".into(), 2), ("other.u".into(), 1)]
    );
}
