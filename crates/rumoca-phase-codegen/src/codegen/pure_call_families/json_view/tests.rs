use super::*;

#[test]
fn nested_catalog_views_preserve_values_and_borrow_the_same_json_nodes() {
    let entries: Arc<[Json]> = vec![serde_json::json!({
        "id": 9007199254740993u64,
        "body": {"nodes": [{"value": -0.0}, {"value": null}, {"value": true}]},
        "label": "quoted \"name\"",
        "empty_map": {}, "empty_array": []
    })]
    .into();
    let expected = serde_json::to_value(entries.as_ref()).unwrap();
    let view = array_value(Arc::clone(&entries));
    assert_eq!(serde_json::to_value(&view).unwrap(), expected);
    assert_eq!(view.len(), Some(1));
    assert!(view.get_item(&Value::from(1)).unwrap().is_undefined());
    let owner = view.get_item(&Value::from(0)).unwrap();
    let body = owner.get_attr("body").unwrap();
    let nodes = body.get_attr("nodes").unwrap();
    let node = nodes.get_item(&Value::from(0)).unwrap();
    let borrowed = node.downcast_object_ref::<JsonNode>().unwrap();
    assert!(Arc::ptr_eq(&borrowed.entries, &entries));
    assert!(std::ptr::eq(
        borrowed.json().unwrap(),
        &entries[0]["body"]["nodes"][0]
    ));
    assert_eq!(
        f64::try_from(node.get_attr("value").unwrap())
            .unwrap()
            .to_bits(),
        (-0.0f64).to_bits()
    );
    assert!(owner.get_attr("missing").unwrap().is_undefined());
    assert!(nodes.get_item(&Value::from(3)).unwrap().is_undefined());
    drop(entries);
    drop(view);
    drop(owner);
    drop(body);
    drop(nodes);
    assert_eq!(
        serde_json::to_value(&node).unwrap(),
        expected[0]["body"]["nodes"][0]
    );
}

#[test]
fn empty_catalog_and_template_iteration_match_eager_projection() {
    let empty = array_value(Arc::from(Vec::<Json>::new()));
    assert_eq!(empty.len(), Some(0));
    assert_eq!(serde_json::to_value(&empty).unwrap(), serde_json::json!([]));
    let entries: Arc<[Json]> = vec![
        serde_json::json!({"id": 4, "inputs": [3, 7], "body": {"op": "load", "live": true}, "maybe": {}}),
        serde_json::json!({"id": 9, "inputs": [], "body": {"op": "store", "live": false}, "maybe": {"present": false}}),
    ]
    .into();
    let lazy = minijinja::context! { owners => array_value(Arc::clone(&entries)) };
    let eager = Value::from_serialize(serde_json::json!({"owners": entries.as_ref()}));
    let template = concat!(
        "{% for owner in table.owners %}{{owner.id}}:{{owner.inputs|length}}:",
        "{{ (owner.inputs + [11])|join(',') }}:{{ owner.inputs[1:]|list }}:",
        "{{ owner.inputs[-1]|default('empty') }}:",
        "{% if owner.inputs %}seq{% endif %}{% if owner.maybe %}map{% endif %}",
        "{% for key, value in owner.body|items %}{{key}}={{value}};{% endfor %}{% endfor %}"
    );
    let mut environment = minijinja::Environment::new();
    environment.add_template("view", template).unwrap();
    let render = |table| {
        environment
            .get_template("view")
            .unwrap()
            .render(minijinja::context! { table })
            .unwrap()
    };
    assert_eq!(render(lazy), render(eager));
}
