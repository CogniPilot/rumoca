//! Fail-closed coverage for `connect(..., c.port[i])` where `port` is a
//! compact array of a *simple* connector.
//!
//! A connector whose type is a predefined type (`connector RealInput = input
//! Real;`) has no members to expand, so an array of it stays one flat variable
//! with its dimensions intact — `gate.x`, dims `[2]`. The model owns no
//! declaration named `gate.x[1]`.
//!
//! Flat's current connected-state owner is declaration-wide. Accepting only
//! `gate.x[1]` would therefore falsely mark the whole compact declaration
//! connected. Until Flat owns checked connected subdomains, flattening must
//! reject at the selected endpoint before a partially mutated Flat root can
//! escape.

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
    Real probe;
equation
    connect(a, gate.x[1]);
    probe = gate.y;
    a = 1.0;
end Wired;
"#;

fn flatten_model(
    model_name: &str,
) -> Result<rumoca_ir_flat::Model, rumoca_phase_flatten::FlattenError> {
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, SOURCE_NAME).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let mut overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(
        resolved.inner(),
        model_name,
    ) {
        rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
        rumoca_phase_instantiate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
            panic!("fixture instantiation failed: {error}")
        }
    };
    rumoca_phase_typecheck::typecheck_instanced(&resolved, &mut overlay, model_name)
        .expect("instanced model typechecks");
    let tree = resolved.into_inner();
    rumoca_phase_flatten::flatten_ref_with_options(
        &tree,
        &overlay,
        model_name,
        rumoca_phase_flatten::FlattenOptions::default(),
    )
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
    let model = flatten_model("Gate").expect("an unconnected compact connector array flattens");
    let names = declared_names(&model);

    assert!(
        names.iter().any(|name| name == "x"),
        "flat model must declare the connector array itself, got {names:?}"
    );
    assert_eq!(
        model
            .variables
            .get(&rumoca_core::VarName::new("x"))
            .expect("connector array is declared")
            .dims,
        vec![2],
        "the connector array keeps its declared dimension"
    );
    assert!(
        !names.iter().any(|name| name.starts_with("x[")),
        "no element of the connector array is separately declared, got {names:?}"
    );
}

#[test]
fn partial_compact_connector_connection_refuses_before_a_flat_product_escapes() {
    let error = flatten_model("Wired")
        .expect_err("partial compact connectivity must not expose a successful Flat product");
    let rumoca_phase_flatten::FlattenError::InvalidConnectionEvidence { description, span } = error
    else {
        panic!("expected typed connection-evidence refusal, got {error:?}");
    };
    assert!(
        description.contains("partial connectivity of a compact array is not representable"),
        "the first owner must name the unsupported connected-subdomain relation: {description}"
    );
    let connection_provenance = SOURCE
        .find("connect(a, gate.x[1])")
        .expect("fixture contains the first rejected connection")
        + "connect(".len();
    assert_eq!(
        span,
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(SOURCE_NAME),
            connection_provenance,
            connection_provenance + 1,
        ),
        "the refusal must retain the exact source connection provenance owned by Instance IR"
    );
}
