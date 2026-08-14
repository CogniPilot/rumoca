//! Exact-identity regression coverage for `connect(..., c.port[i])` where
//! `port` is an array of a *simple* connector.
//!
//! A connector whose type is a predefined type (`connector RealInput = input
//! Real;`) has no members to expand, so an array of it stays one flat variable
//! with its dimensions intact — `gate.x`, dims `[2]`. The model owns no
//! declaration named `gate.x[1]`.
//!
//! Connection sets are keyed by rendered path, so an element endpoint reaches
//! equation generation as the string `gate.x[1]`. Emitting that string as a
//! reference *name* fabricates a coordinate the flat model does not declare,
//! and every consumer that resolves references against the declared variable
//! set rejects it (`ED008` at the Flat/DAE boundary). The element must instead
//! resolve back to the declaration it selects from and carry the index as a
//! structured subscript.

use rumoca_ir_ast as ast;

const SOURCE_NAME: &str = "<connection_array_element_identity>";
const SOURCE: &str = r#"
connector RealInput = input Real;
connector RealOutput = output Real;

model Gate
    RealInput x[2];
    RealOutput y;
equation
    y = x[1] + x[2];
end Gate;

model Wired
    Gate gate;
    RealOutput a;
    RealOutput b;
    Real probe;
equation
    connect(a, gate.x[1]);
    connect(b, gate.x[2]);
    probe = gate.y;
    a = 1.0;
    b = 2.0;
end Wired;

model NestedWired
    Wired unit;
    Real echo;
equation
    echo = unit.probe;
end NestedWired;
"#;

fn flatten_model(model_name: &str) -> rumoca_ir_flat::Model {
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, SOURCE_NAME).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let instanced =
        rumoca_phase_instantiate::instantiate(resolved, model_name).expect("model instantiates");
    rumoca_phase_flatten::flatten_ref_with_options(
        instanced.inner(),
        instanced.overlay(),
        model_name,
        rumoca_phase_flatten::FlattenOptions::default(),
    )
    .expect("model flattens")
}

/// Every `(rendered name, literal subscripts)` pair reachable from the model's
/// equations.
fn equation_references(model: &rumoca_ir_flat::Model) -> Vec<(String, Vec<i64>)> {
    struct Collector<'a> {
        refs: &'a mut Vec<(String, Vec<i64>)>,
    }
    impl rumoca_core::ExpressionRewriter for Collector<'_> {
        fn rewrite_var_ref_expression(
            &mut self,
            name: &rumoca_core::Reference,
            subscripts: &[rumoca_core::Subscript],
            span: rumoca_core::Span,
        ) -> rumoca_core::Expression {
            let indices = subscripts
                .iter()
                .filter_map(|subscript| match subscript {
                    rumoca_core::Subscript::Index { value, .. } => Some(*value),
                    _ => None,
                })
                .collect();
            self.refs.push((name.as_str().to_string(), indices));
            rumoca_core::Expression::VarRef {
                name: name.clone(),
                subscripts: self.rewrite_subscripts(subscripts),
                span,
            }
        }
    }
    use rumoca_core::ExpressionRewriter as _;

    let mut refs = Vec::new();
    for equation in model.equations.iter().chain(model.initial_equations.iter()) {
        Collector { refs: &mut refs }.rewrite_expression(&equation.residual);
    }
    refs
}

fn declared_names(model: &rumoca_ir_flat::Model) -> Vec<String> {
    model
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect()
}

#[test]
fn simple_connector_array_stays_one_declaration() {
    let model = flatten_model("Wired");
    let names = declared_names(&model);

    assert!(
        names.iter().any(|name| name == "gate.x"),
        "flat model must declare the connector array itself, got {names:?}"
    );
    assert_eq!(
        model
            .variables
            .get(&rumoca_core::VarName::new("gate.x"))
            .expect("connector array is declared")
            .dims,
        vec![2],
        "the connector array keeps its declared dimension"
    );
    assert!(
        !names.iter().any(|name| name.starts_with("gate.x[")),
        "no element of the connector array is separately declared, got {names:?}"
    );
}

#[test]
fn connection_to_an_array_connector_element_names_the_declaration() {
    let model = flatten_model("Wired");
    let refs = equation_references(&model);

    assert!(
        !refs.iter().any(|(name, _)| name.contains('[')),
        "no equation reference may name an undeclared bracketed coordinate, got {refs:?}"
    );
    for index in [1, 2] {
        assert!(
            refs.iter()
                .any(|(name, indices)| name == "gate.x" && indices == &[index]),
            "the connection to element {index} must reference `gate.x` with subscript {index}, got {refs:?}"
        );
    }
}

#[test]
fn every_equation_reference_names_a_declared_variable() {
    for model_name in ["Wired", "NestedWired"] {
        let model = flatten_model(model_name);
        let declared = declared_names(&model);
        for (name, _) in equation_references(&model) {
            if name == "time" {
                continue;
            }
            assert!(
                declared.contains(&name),
                "`{model_name}` equation reference `{name}` names no declared variable; declared: {declared:?}"
            );
        }
    }
}

#[test]
fn array_connector_element_reference_carries_the_declaration_identity() {
    let model = flatten_model("Wired");
    let declaration = model
        .variables
        .get(&rumoca_core::VarName::new("gate.x"))
        .expect("connector array is declared");

    let mut seen = 0usize;
    for equation in &model.equations {
        if !matches!(
            equation.origin,
            rumoca_ir_flat::EquationOrigin::Connection { .. }
        ) {
            continue;
        }
        let mut identities = Vec::new();
        collect_identities(&equation.residual, &mut identities);
        for (name, instance_id) in identities {
            if name != "gate.x" {
                continue;
            }
            seen += 1;
            assert_eq!(
                instance_id,
                Some(declaration.instance_id),
                "an element reference must carry the occurrence identity of its declaration"
            );
        }
    }
    assert_eq!(
        seen, 2,
        "both element connections must reach the flat equations as references to `gate.x`"
    );
}

fn collect_identities(
    expr: &rumoca_core::Expression,
    out: &mut Vec<(String, Option<rumoca_core::InstanceId>)>,
) {
    struct Collector<'a> {
        out: &'a mut Vec<(String, Option<rumoca_core::InstanceId>)>,
    }
    impl rumoca_core::ExpressionRewriter for Collector<'_> {
        fn rewrite_var_ref_expression(
            &mut self,
            name: &rumoca_core::Reference,
            subscripts: &[rumoca_core::Subscript],
            span: rumoca_core::Span,
        ) -> rumoca_core::Expression {
            self.out
                .push((name.as_str().to_string(), name.instance_id()));
            rumoca_core::Expression::VarRef {
                name: name.clone(),
                subscripts: self.rewrite_subscripts(subscripts),
                span,
            }
        }
    }
    use rumoca_core::ExpressionRewriter as _;
    Collector { out }.rewrite_expression(expr);
}
