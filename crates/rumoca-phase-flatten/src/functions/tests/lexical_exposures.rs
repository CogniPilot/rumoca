use super::*;

fn nested_random_tree() -> (
    ast::ClassTree,
    rumoca_core::DefId,
    rumoca_core::DefId,
    rumoca_core::DefId,
) {
    let generators_def = rumoca_core::DefId::new(90_001);
    let xorshift_def = rumoca_core::DefId::new(90_002);
    let random_def = rumoca_core::DefId::new(90_003);
    let mut xorshift = class(
        "Xorshift64star",
        rumoca_core::ClassType::Package,
        xorshift_def,
    );
    xorshift.classes.insert(
        "random".to_string(),
        class("random", rumoca_core::ClassType::Function, random_def),
    );
    let mut generators = class(
        "Generators",
        rumoca_core::ClassType::Package,
        generators_def,
    );
    generators
        .classes
        .insert("Xorshift64star".to_string(), xorshift);
    let mut random = class(
        "Random",
        rumoca_core::ClassType::Package,
        rumoca_core::DefId::new(90_004),
    );
    random.classes.insert("Generators".to_string(), generators);
    let mut math = class(
        "Math",
        rumoca_core::ClassType::Package,
        rumoca_core::DefId::new(90_005),
    );
    math.classes.insert("Random".to_string(), random);
    let mut modelica = class(
        "Modelica",
        rumoca_core::ClassType::Package,
        rumoca_core::DefId::new(90_006),
    );
    modelica.classes.insert("Math".to_string(), math);
    let mut tree = ast::ClassTree::new();
    tree.definitions
        .classes
        .insert("Modelica".to_string(), modelica);
    (tree, generators_def, xorshift_def, random_def)
}

fn add_regular_function(
    flat: &mut flat::Model,
    name: &str,
    def_id: rumoca_core::DefId,
) -> rumoca_core::FunctionInstanceId {
    let mut function = rumoca_core::Function::new(name, test_span());
    function.def_id = Some(def_id);
    function
        .body
        .push(rumoca_core::Statement::Return { span: test_span() });
    flat.add_function(function);
    flat.functions[&rumoca_core::VarName::new(name)]
        .instance_id
        .expect("Flat assigns an exact function instance")
}

fn relative_random_algorithm(
    generators_def: rumoca_core::DefId,
    xorshift_def: rumoca_core::DefId,
    random_def: rumoca_core::DefId,
) -> flat::Algorithm {
    flat::Algorithm::new(
        vec![rumoca_core::Statement::FunctionCall {
            comp: rumoca_core::Reference::from_component_reference(core_structured_comp_ref(&[
                ("Generators", generators_def),
                ("Xorshift64star", xorshift_def),
                ("random", random_def),
            ])),
            args: Vec::new(),
            outputs: Vec::new(),
            span: test_span(),
        }],
        test_span(),
        "relative random call",
    )
}

#[test]
fn canonicalize_collected_function_calls_closes_exact_lexical_algorithm_path() {
    let (tree, generators_def, xorshift_def, random_def) = nested_random_tree();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let mut flat = flat::Model::new();
    add_regular_function(&mut flat, "Xorshift64star.random", random_def);
    let expected_instance = add_regular_function(
        &mut flat,
        "Modelica.Math.Random.Generators.Xorshift64star.random",
        random_def,
    );
    flat.algorithms.push(relative_random_algorithm(
        generators_def,
        xorshift_def,
        random_def,
    ));

    canonicalize_collected_function_calls(&mut flat, &class_index)
        .expect("the exact lexical parent chain selects one collected exposure");
    materialize_flat_function_call_args(&mut flat)
        .expect("the certified statement call must materialize without DefId recovery");

    let rumoca_core::Statement::FunctionCall { comp, .. } = &flat.algorithms[0].statements[0]
    else {
        panic!("expected function-call statement");
    };
    assert_eq!(
        comp.as_str(),
        "Modelica.Math.Random.Generators.Xorshift64star.random"
    );
    assert_eq!(
        comp.resolved_function()
            .map(|resolved| resolved.instance_id),
        Some(expected_instance)
    );
}

#[test]
fn canonicalize_collected_function_calls_rejects_multiple_lexical_closures() {
    let (tree, generators_def, xorshift_def, random_def) = nested_random_tree();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let mut flat = flat::Model::new();
    add_regular_function(
        &mut flat,
        "Random.Generators.Xorshift64star.random",
        random_def,
    );
    add_regular_function(
        &mut flat,
        "Modelica.Math.Random.Generators.Xorshift64star.random",
        random_def,
    );
    flat.algorithms.push(relative_random_algorithm(
        generators_def,
        xorshift_def,
        random_def,
    ));

    canonicalize_collected_function_calls(&mut flat, &class_index)
        .expect("ambiguous lexical closures remain unresolved");

    let rumoca_core::Statement::FunctionCall { comp, .. } = &flat.algorithms[0].statements[0]
    else {
        panic!("expected function-call statement");
    };
    assert_eq!(comp.as_str(), "Generators.Xorshift64star.random");
    assert_eq!(comp.resolved_function(), None);
}

#[test]
fn canonicalize_collected_function_calls_does_not_replace_unknown_exact_instance() {
    let (tree, generators_def, xorshift_def, random_def) = nested_random_tree();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let mut flat = flat::Model::new();
    add_regular_function(
        &mut flat,
        "Modelica.Math.Random.Generators.Xorshift64star.random",
        random_def,
    );
    let unknown = rumoca_core::ResolvedFunctionReference {
        instance_id: rumoca_core::FunctionInstanceId::new(900),
        base_part_count: 3,
        transitively_non_replaceable: true,
    };
    let mut algorithm = relative_random_algorithm(generators_def, xorshift_def, random_def);
    let rumoca_core::Statement::FunctionCall { comp, .. } = &mut algorithm.statements[0] else {
        panic!("expected function-call statement");
    };
    *comp = comp.clone().with_resolved_function(unknown);
    flat.algorithms.push(algorithm);

    canonicalize_collected_function_calls(&mut flat, &class_index)
        .expect("lexical closure must not replace an existing exact identity");

    let rumoca_core::Statement::FunctionCall { comp, .. } = &flat.algorithms[0].statements[0]
    else {
        panic!("expected function-call statement");
    };
    assert_eq!(comp.as_str(), "Generators.Xorshift64star.random");
    assert_eq!(comp.resolved_function(), Some(unknown));
}
