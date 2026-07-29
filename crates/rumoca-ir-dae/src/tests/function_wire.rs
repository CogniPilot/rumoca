use super::*;

#[test]
fn wire_rejects_a_noncanonical_source_map_before_dae_construction() {
    let dae = function_read_fixture(false);
    let mut wire = serde_json::to_value(dae).expect("checked DAE serializes");
    let files = wire["source_map"]["files"]
        .as_array_mut()
        .expect("source map wire contains its canonical file records");
    files[0][0] = serde_json::to_value(SourceId::from_source_name("forged.mo"))
        .expect("source id serializes");

    let error = serde_json::from_value::<Dae>(wire)
        .expect_err("a source name cannot claim another source identity");

    assert!(error.to_string().contains("not stored identity"));
}

#[test]
fn function_operations_are_canonical_and_replay_in_owner_order() {
    let source = TestSource::new("function f output Real x; x:=0; 1; old_x; x:=1; end f;");
    let function_at = source.source("function f", 0);
    let output_at = source.source("output Real x", 0);
    let zero_at = source.source("0", 0);
    let initial_at = source.source("x:=0", 0);
    let one_at = source.source("1", 0);
    let old_at = source.source("old_x", 0);
    let update_at = source.source("x:=1", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        let (_, reservation) = dae.functions(|functions| {
            functions.reserve_recursive(VarName::new("f"), [], [real], function_at)
        })?;
        let output = dae.functions(|functions| {
            functions.output(&reservation, VarName::new("x"), 0, output_at)
        })?;
        let mut body = dae.functions(|functions| functions.begin(reservation, function_at))?;
        let zero =
            dae.expressions(|expressions| expressions.at(zero_at).literal(DaeLiteral::Real(0.0)))?;
        dae.functions(|functions| functions.assign(&mut body, output, zero, initial_at))?;
        let later =
            dae.expressions(|expressions| expressions.at(one_at).literal(DaeLiteral::Real(1.0)))?;
        dae.functions(|functions| functions.read(&body, output, old_at))?;
        dae.functions(|functions| functions.assign(&mut body, output, later, update_at))?;
        dae.functions(|functions| functions.define(body, function_at))
    })
    .expect("a ready future RHS does not advance the semantic owner");

    let encoded = serde_json::to_string(&dae).unwrap();
    let _: Dae = serde_json::from_str(&encoded).expect("owner-scheduled replay round trips");
    let binary = bincode::serialize(&dae).expect("function operation log serializes");
    let decoded: Dae = bincode::deserialize(&binary).expect("function operation log reconstructs");
    assert_eq!(
        bincode::serialize(&decoded).unwrap(),
        binary,
        "binary function operations have one canonical representation"
    );

    let canonical: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    let storage = canonical["storage"].as_object().unwrap();
    assert!(
        !storage.contains_key("function_folds"),
        "constructor-derived fold facts are not wire state"
    );
    let function = canonical["storage"]["functions"][0].as_object().unwrap();
    for removed in [
        "parameter_values",
        "values",
        "output_values",
        "definitions",
        "folds",
        "definition",
        "results",
    ] {
        assert!(
            !function.contains_key(removed),
            "{removed} is constructor-derived and must not be serialized"
        );
    }
    assert_eq!(
        function["statements"].as_array().unwrap().len(),
        2,
        "assignments are the readable semantic operation log"
    );
    assert!(
        function["statements"][0].get("assignment").is_some(),
        "an assignment stores its target, RHS, and provenance inline"
    );

    let mut legacy_definition = canonical;
    legacy_definition["storage"]["functions"][0]
        .as_object_mut()
        .unwrap()
        .insert("definitions".to_owned(), serde_json::json!([]));
    assert!(
        serde_json::from_value::<Dae>(legacy_definition).is_err(),
        "wire-v11 rejects the removed definition mirror"
    );
}

#[test]
fn wire_replay_rejects_future_and_stale_function_reads() {
    let future = function_read_fixture(false);
    let mut future_wire: serde_json::Value = serde_json::to_value(&future).unwrap();
    let mut future_reads = function_value_nodes_mut(&mut future_wire);
    future_reads[0]["definition_ordinal"] = 1.into();
    assert!(
        serde_json::from_value::<Dae>(future_wire).is_err(),
        "a read cannot advance to a definition whose RHS is still in the future"
    );

    let stale = function_read_fixture(true);
    let mut stale_wire: serde_json::Value = serde_json::to_value(&stale).unwrap();
    let mut stale_reads = function_value_nodes_mut(&mut stale_wire);
    stale_reads[1]["definition_ordinal"] = 0.into();
    assert!(
        serde_json::from_value::<Dae>(stale_wire).is_err(),
        "a read cannot return to an older definition after the owner advanced"
    );
}

#[test]
fn wire_replay_routes_domain_free_reads_to_the_active_loop_owner() {
    let source = TestSource::new(
        "function f output Real x; output Real y; x:=0; y:=0; \
         for k in 1:2 loop x:=1; y:=x; end for; end f;",
    );
    let function_at = source.source("function f", 0);
    let x_at = source.source("output Real x", 0);
    let y_at = source.source("output Real y", 0);
    let zero_at = source.source("0", 0);
    let x_initial_at = source.source("x:=0", 0);
    let y_initial_at = source.source("y:=0", 0);
    let loop_at = source.source("for k in 1:2 loop", 0);
    let finish_at = source.source("end for", 0);
    let one_at = source.source("1", 0);
    let x_update_at = source.source("x:=1", 0);
    let x_read_at = source.source("x", 3);
    let y_update_at = source.source("y:=x", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        let (_, reservation) = dae.functions(|functions| {
            functions.reserve_recursive(VarName::new("f"), [], [real, real], function_at)
        })?;
        let x =
            dae.functions(|functions| functions.output(&reservation, VarName::new("x"), 0, x_at))?;
        let y =
            dae.functions(|functions| functions.output(&reservation, VarName::new("y"), 1, y_at))?;
        let mut body = dae.functions(|functions| functions.begin(reservation, function_at))?;
        let zero =
            dae.expressions(|expressions| expressions.at(zero_at).literal(DaeLiteral::Real(0.0)))?;
        dae.functions(|functions| functions.assign(&mut body, x, zero, x_initial_at))?;
        dae.functions(|functions| functions.assign(&mut body, y, zero, y_initial_at))?;
        let domain = dae.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![StructuredIndexBinder {
                        id: 0,
                        display_name: "k".to_owned(),
                        lower: 1,
                        upper: 2,
                        step: 1,
                    }],
                },
                loop_at,
            )
        })?;
        let mut loop_body =
            dae.functions(|functions| functions.begin_loop(body, domain, [x, y], loop_at))?;
        let one =
            dae.expressions(|expressions| expressions.at(one_at).literal(DaeLiteral::Real(1.0)))?;
        dae.functions(|functions| functions.assign_loop(&mut loop_body, x, one, x_update_at))?;
        let x_value = dae.functions(|functions| functions.read(loop_body.body(), x, x_read_at))?;
        dae.functions(|functions| functions.assign_loop(&mut loop_body, y, x_value, y_update_at))?;
        let body = dae.functions(|functions| functions.finish_loop(loop_body, finish_at))?;
        dae.functions(|functions| functions.define(body, function_at))
    })
    .expect("domain-free loop updates remain owned by the loop transition");

    dae.inspect(|view| {
        let read = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .find(|expression| expression.provenance() == x_read_at)
            .expect("loop read remains present");
        assert!(read.binder_domain().is_none());
    });
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae =
        serde_json::from_str(&encoded).expect("active loop replay is definition-led");
    let reencoded = serde_json::to_string(&decoded).unwrap();
    assert_eq!(
        reencoded, encoded,
        "wire replay preserves every canonical column"
    );
}

#[test]
fn wire_replay_rejects_trailing_dense_columns_and_packed_operands() {
    let dae = function_read_fixture(false);
    let mut trailing_column: serde_json::Value = serde_json::to_value(&dae).unwrap();
    let provenance = trailing_column["storage"]["expressions"]["provenance"][0].clone();
    trailing_column["storage"]["expressions"]["provenance"]
        .as_array_mut()
        .unwrap()
        .push(provenance);
    assert!(serde_json::from_value::<Dae>(trailing_column).is_err());

    let mut trailing_operand: serde_json::Value = serde_json::to_value(&dae).unwrap();
    trailing_operand["storage"]["expressions"]["operands"]
        .as_array_mut()
        .unwrap()
        .push(0.into());
    assert!(serde_json::from_value::<Dae>(trailing_operand).is_err());

    let mut trailing_subscript: serde_json::Value = serde_json::to_value(&dae).unwrap();
    let provenance = trailing_subscript["storage"]["expressions"]["provenance"][0].clone();
    trailing_subscript["storage"]["expressions"]["subscripts"]
        .as_array_mut()
        .unwrap()
        .push(serde_json::json!({
            "kind": "whole",
            "provenance": provenance,
        }));
    assert!(serde_json::from_value::<Dae>(trailing_subscript).is_err());
}

#[test]
fn wire_replay_rejects_legacy_function_read_shape() {
    let dae = function_read_fixture(false);
    let mut wire: serde_json::Value = serde_json::to_value(&dae).unwrap();
    let read = function_value_nodes_mut(&mut wire)
        .into_iter()
        .next()
        .expect("fixture contains a function-value occurrence");
    let definition = read
        .as_object_mut()
        .unwrap()
        .remove("definition_ordinal")
        .expect("current wire stores the definition ordinal explicitly");
    read.as_object_mut()
        .unwrap()
        .insert("definition".to_owned(), definition);
    assert!(
        serde_json::from_value::<Dae>(wire).is_err(),
        "wire-v11 must not accept the removed function-read representation"
    );
}

fn function_read_fixture(stale_pair: bool) -> Dae {
    let source =
        TestSource::new("function f output Real x; x:=0; old; 1; x:=1; new; again; end f;");
    let function_at = source.source("function f", 0);
    let output_at = source.source("output Real x", 0);
    let zero_at = source.source("0", 0);
    let initial_at = source.source("x:=0", 0);
    let old_at = source.source("old", 0);
    let one_at = source.source("1", 0);
    let update_at = source.source("x:=1", 0);
    let new_at = source.source("new", 0);
    let again_at = source.source("again", 0);
    Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        let (_, reservation) = dae.functions(|functions| {
            functions.reserve_recursive(VarName::new("f"), [], [real], function_at)
        })?;
        let output = dae.functions(|functions| {
            functions.output(&reservation, VarName::new("x"), 0, output_at)
        })?;
        let mut body = dae.functions(|functions| functions.begin(reservation, function_at))?;
        let zero =
            dae.expressions(|expressions| expressions.at(zero_at).literal(DaeLiteral::Real(0.0)))?;
        dae.functions(|functions| functions.assign(&mut body, output, zero, initial_at))?;
        if !stale_pair {
            dae.functions(|functions| functions.read(&body, output, old_at))?;
        }
        let one =
            dae.expressions(|expressions| expressions.at(one_at).literal(DaeLiteral::Real(1.0)))?;
        dae.functions(|functions| functions.assign(&mut body, output, one, update_at))?;
        dae.functions(|functions| functions.read(&body, output, new_at))?;
        if stale_pair {
            dae.functions(|functions| functions.read(&body, output, again_at))?;
        }
        dae.functions(|functions| functions.define(body, function_at))
    })
    .unwrap()
}

fn function_value_nodes_mut(wire: &mut serde_json::Value) -> Vec<&mut serde_json::Value> {
    wire["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .filter_map(|node| node.get_mut("function_value"))
        .collect()
}
