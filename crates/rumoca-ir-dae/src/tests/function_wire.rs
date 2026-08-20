use super::*;

fn assert_iteration_local_loop_view(view: DaeView<'_>, final_scratch_at: DaeProvenance) {
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    let scratch = function
        .values()
        .find(|value| value.name().as_str() == "scratch")
        .expect("fixture retains its declared scratch value");
    let statements = function.statements().collect::<Vec<_>>();
    let FunctionStatementView::For { fold, .. } = statements[2] else {
        panic!("the source loop remains one compact fold");
    };
    let fold = view.function_fold(fold).unwrap();
    assert_eq!(
        fold.targets().collect::<Vec<_>>(),
        vec![function.values().next().unwrap().id()]
    );
    assert_eq!(
        fold.iteration_locals().collect::<Vec<_>>(),
        vec![scratch.id()]
    );

    let result = view
        .expression(function.result_values().rhs(0).unwrap())
        .expect("final output has a checked expression");
    let ExpressionOperation::Binary { rhs, .. } = result.operation() else {
        panic!("post-loop output reads the restored scratch value");
    };
    let restored = view.expression(rhs).unwrap();
    assert_eq!(restored.provenance(), final_scratch_at);
    let ExpressionOperation::FunctionValue { value, definition } = restored.operation() else {
        panic!("post-loop scratch is a reaching-definition read");
    };
    assert_eq!(value, scratch.id());
    assert_eq!(
        view.source_text(definition.provenance()),
        Some("scratch := 5.0")
    );
}

fn assert_iteration_local_loop_roundtrip(dae: &Dae, final_scratch_at: DaeProvenance) {
    dae.inspect(|view| assert_iteration_local_loop_view(view, final_scratch_at));
    let encoded = serde_json::to_string(dae).unwrap();
    let replayed: Dae = serde_json::from_str(&encoded).unwrap();
    replayed.inspect(|view| assert_iteration_local_loop_view(view, final_scratch_at));
}

struct IterationLocalLoopSpans {
    function: DaeProvenance,
    output: DaeProvenance,
    scratch: DaeProvenance,
    y_initial: DaeProvenance,
    zero: DaeProvenance,
    scratch_initial: DaeProvenance,
    five: DaeProvenance,
    loop_owner: DaeProvenance,
    scratch_loop: DaeProvenance,
    one: DaeProvenance,
    loop_update: DaeProvenance,
    loop_y: DaeProvenance,
    loop_scratch: DaeProvenance,
    loop_rhs: DaeProvenance,
    finish: DaeProvenance,
    final_update: DaeProvenance,
    final_y: DaeProvenance,
    final_scratch: DaeProvenance,
    final_rhs: DaeProvenance,
}

impl IterationLocalLoopSpans {
    fn new(source: &TestSource) -> Self {
        Self {
            function: source.source("function f", 0),
            output: source.source("output Real y", 0),
            scratch: source.source("Real scratch", 0),
            y_initial: source.source("y := 0.0", 0),
            zero: source.source("0.0", 0),
            scratch_initial: source.source("scratch := 5.0", 0),
            five: source.source("5.0", 0),
            loop_owner: source.source("for k in 1:2 loop", 0),
            scratch_loop: source.source("scratch := 1.0", 0),
            one: source.source("1.0", 0),
            loop_update: source.source("y := y + scratch", 0),
            loop_y: source.source("y", 3),
            loop_scratch: source.source("scratch", 3),
            loop_rhs: source.source("y + scratch", 0),
            finish: source.source("end for", 0),
            final_update: source.source("y := y + scratch", 1),
            final_y: source.source("y", 5),
            final_scratch: source.source("scratch", 4),
            final_rhs: source.source("y + scratch", 1),
        }
    }
}

#[test]
fn iteration_local_loop_value_round_trips_and_restores_its_enclosing_definition() {
    let source = TestSource::new(
        "function f\n output Real y;\n protected Real scratch;\nalgorithm\n y := 0.0;\n scratch := 5.0;\n for k in 1:2 loop\n  scratch := 1.0;\n  y := y + scratch;\n end for;\n y := y + scratch;\nend f;",
    );
    let spans = IterationLocalLoopSpans::new(&source);
    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), spans.function))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], spans.function),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, spans.output)
                })?;
                let scratch = dae.functions(|functions| {
                    functions.local(&reservation, VarName::new("scratch"), real, spans.scratch)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, spans.function))?;
                let zero = dae.expressions(|expressions| {
                    expressions.at(spans.zero).literal(DaeLiteral::Real(0.0))
                })?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, zero, spans.y_initial)
                })?;
                let five = dae.expressions(|expressions| {
                    expressions.at(spans.five).literal(DaeLiteral::Real(5.0))
                })?;
                dae.functions(|functions| {
                    functions.assign(&mut body, scratch, five, spans.scratch_initial)
                })?;
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
                        spans.loop_owner,
                    )
                })?;
                let mut loop_body = dae.functions(|functions| {
                    functions.begin_loop_with_iteration_locals(
                        body,
                        domain,
                        [output],
                        [scratch],
                        spans.loop_owner,
                    )
                })?;
                let one = dae.expressions(|expressions| {
                    expressions.at(spans.one).literal(DaeLiteral::Real(1.0))
                })?;
                dae.functions(|functions| {
                    functions.assign_loop(&mut loop_body, scratch, one, spans.scratch_loop)
                })?;
                let current = dae.functions(|functions| {
                    functions.read(loop_body.body(), output, spans.loop_y)
                })?;
                let scratch_value = dae.functions(|functions| {
                    functions.read(loop_body.body(), scratch, spans.loop_scratch)
                })?;
                let update = dae.expressions(|expressions| {
                    expressions.at(spans.loop_rhs).binary(
                        BinaryOperator::Add,
                        current,
                        scratch_value,
                    )
                })?;
                dae.functions(|functions| {
                    functions.assign_loop(&mut loop_body, output, update, spans.loop_update)
                })?;
                let mut body =
                    dae.functions(|functions| functions.finish_loop(loop_body, spans.finish))?;
                let current =
                    dae.functions(|functions| functions.read(&body, output, spans.final_y))?;
                let restored =
                    dae.functions(|functions| functions.read(&body, scratch, spans.final_scratch))?;
                let update = dae.expressions(|expressions| {
                    expressions
                        .at(spans.final_rhs)
                        .binary(BinaryOperator::Add, current, restored)
                })?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, update, spans.final_update)
                })?;
                dae.functions(|functions| functions.define(body, spans.function))
            },
        )?;
        Ok(())
    })
    .expect("an iteration-local loop value has a checked lexical owner");

    assert_iteration_local_loop_roundtrip(&dae, spans.final_scratch);
}

#[test]
fn assertion_only_function_loop_round_trips_without_generated_fold_values() {
    let source = TestSource::new(
        "function f output Real y; algorithm y := 0; for k in 1:2 loop assert(k > 0, \"positive\"); end for; end f;",
    );
    let function_at = source.source("function f", 0);
    let output_at = source.source("output Real y", 0);
    let assignment_at = source.source("y := 0", 0);
    let zero_at = source.source("0", 0);
    let loop_at = source.source("for k in 1:2 loop", 0);
    let assertion_at = source.source("assert(k > 0, \"positive\")", 0);
    let condition_at = source.source("k > 0", 0);
    let condition_zero_at = source.source("0", 1);
    let message_at = source.source("\"positive\"", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], function_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, output_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let zero = dae.expressions(|expressions| {
                    expressions.at(zero_at).literal(DaeLiteral::Real(0.0))
                })?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, zero, assignment_at)
                })?;
                let domain = dae.domains(|domains| {
                    domains.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                id: 0,
                                display_name: "k".to_string(),
                                lower: 1,
                                upper: 2,
                                step: 1,
                            }],
                        },
                        loop_at,
                    )
                })?;
                let mut loop_body =
                    dae.functions(|functions| functions.begin_loop(body, domain, [], loop_at))?;
                let k = dae.expressions(|expressions| {
                    expressions
                        .at(condition_at)
                        .binder(DomainBinderId::from_raw(domain.index(), 0))
                })?;
                let condition_zero = dae.expressions(|expressions| {
                    expressions
                        .at(condition_zero_at)
                        .literal(DaeLiteral::Integer(0))
                })?;
                let condition = dae.expressions(|expressions| {
                    expressions
                        .at(condition_at)
                        .binary(BinaryOperator::Greater, k, condition_zero)
                })?;
                let message = dae.expressions(|expressions| {
                    expressions
                        .at(message_at)
                        .literal(DaeLiteral::String("positive".to_owned()))
                })?;
                dae.functions(|functions| {
                    functions.assertion_loop(&mut loop_body, condition, message, assertion_at)
                })?;
                let body = dae.functions(|functions| functions.finish_loop(loop_body, loop_at))?;
                dae.functions(|functions| functions.define(body, function_at))
            },
        )?;
        Ok(())
    })
    .expect("an assertion-only compact fold constructs without carried values");

    let inspect = |view: DaeView<'_>| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let statements = function.statements().collect::<Vec<_>>();
        let FunctionStatementView::For {
            fold, statements, ..
        } = statements[1].clone()
        else {
            panic!("the assertion-only loop stays a compact fold");
        };
        assert_eq!(view.function_fold(fold).unwrap().targets().count(), 0);
        assert!(matches!(
            statements.collect::<Vec<_>>().as_slice(),
            [FunctionStatementView::Assertion { .. }]
        ));
    };
    dae.inspect(inspect);
    let encoded = serde_json::to_string(&dae).unwrap();
    let replayed: Dae = serde_json::from_str(&encoded).unwrap();
    replayed.inspect(inspect);
}

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
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], function_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("x"), 0, output_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let zero = dae.expressions(|expressions| {
                    expressions.at(zero_at).literal(DaeLiteral::Real(0.0))
                })?;
                dae.functions(|functions| functions.assign(&mut body, output, zero, initial_at))?;
                let later = dae.expressions(|expressions| {
                    expressions.at(one_at).literal(DaeLiteral::Real(1.0))
                })?;
                dae.functions(|functions| functions.read(&body, output, old_at))?;
                dae.functions(|functions| functions.assign(&mut body, output, later, update_at))?;
                dae.functions(|functions| functions.define(body, function_at))
            },
        )
        .map(|_| ())
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

    let mut removed_definition_mirror = canonical;
    removed_definition_mirror["storage"]["functions"][0]
        .as_object_mut()
        .unwrap()
        .insert("definitions".to_owned(), serde_json::json!([]));
    assert!(
        serde_json::from_value::<Dae>(removed_definition_mirror).is_err(),
        "wire-v12 rejects the removed definition mirror"
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
    let (dae, x_read_at) = active_loop_fixture();
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
    assert_eq!(
        serde_json::to_string(&decoded).unwrap(),
        encoded,
        "wire replay preserves every canonical column"
    );
}

#[test]
fn wire_omits_generated_fold_facts_and_replays_them_through_construction() {
    let (dae, _) = active_loop_fixture();
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae =
        serde_json::from_str(&encoded).expect("generated fold facts replay from their transitions");
    assert_eq!(
        serde_json::to_string(&decoded).unwrap(),
        encoded,
        "re-issued fold results reproduce the canonical arena"
    );
    let binary = bincode::serialize(&dae).expect("fold arena serializes");
    let decoded: Dae =
        bincode::deserialize(&binary).expect("ordinal-tagged fold nodes reconstruct");
    assert_eq!(
        bincode::serialize(&decoded).unwrap(),
        binary,
        "binary fold nodes have one canonical representation"
    );

    let canonical: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    for kind in ["function_fold_parameter", "function_fold_output"] {
        let generated = generated_fold_nodes(&canonical, kind);
        assert_eq!(generated.len(), 2, "the fixture folds two carried values");
        for node in generated {
            let fields = node.as_object().unwrap();
            assert_eq!(
                fields.keys().collect::<Vec<_>>(),
                vec!["function"],
                "a generated {kind} names only the function whose fold issued it"
            );
        }
    }
}

#[test]
fn wire_rejects_restated_generated_fold_facts() {
    let (dae, _) = active_loop_fixture();
    let canonical = serde_json::to_value(dae).unwrap();

    for (kind, field, value) in [
        ("function_fold_parameter", "fold", serde_json::json!(0)),
        ("function_fold_parameter", "carried", serde_json::json!(0)),
        (
            "function_fold_parameter",
            "definition_ordinal",
            serde_json::json!(2),
        ),
        ("function_fold_output", "fold", serde_json::json!(0)),
        ("function_fold_output", "carried", serde_json::json!(0)),
        (
            "function_fold_output",
            "definition_ordinal",
            serde_json::json!(4),
        ),
    ] {
        let mut restated = canonical.clone();
        generated_fold_nodes_mut(&mut restated, kind)
            .remove(0)
            .as_object_mut()
            .unwrap()
            .insert(field.to_owned(), value);
        assert!(
            serde_json::from_value::<Dae>(restated).is_err(),
            "a generated {kind} must not restate the {field} its fold transition re-issues"
        );
    }

    let mut foreign_owner = canonical;
    generated_fold_nodes_mut(&mut foreign_owner, "function_fold_output").remove(0)["function"] =
        u32::MAX.into();
    assert!(
        serde_json::from_value::<Dae>(foreign_owner).is_err(),
        "a generated node cannot claim a function that did not issue it"
    );
}

fn generated_fold_nodes<'value>(
    wire: &'value serde_json::Value,
    kind: &str,
) -> Vec<&'value serde_json::Value> {
    wire["storage"]["expressions"]["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|node| node.get(kind))
        .collect()
}

fn generated_fold_nodes_mut<'value>(
    wire: &'value mut serde_json::Value,
    kind: &str,
) -> Vec<&'value mut serde_json::Value> {
    wire["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .filter_map(|node| node.get_mut(kind))
        .collect()
}

#[test]
fn wire_replay_rejects_invalid_fold_transitions() {
    let (dae, _) = active_loop_fixture();
    let canonical = serde_json::to_value(dae).unwrap();

    let mut end_before_begin = canonical.clone();
    let nodes = end_before_begin["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap();
    let parameter = nodes
        .iter_mut()
        .find(|node| node.get("function_fold_parameter").is_some())
        .expect("fixture contains a generated fold parameter")
        .as_object_mut()
        .unwrap();
    let payload = parameter.remove("function_fold_parameter").unwrap();
    parameter.insert("function_fold_output".to_owned(), payload);
    assert!(
        serde_json::from_value::<Dae>(end_before_begin).is_err(),
        "a fold cannot end before its begin operation"
    );

    let mut nested_begin = canonical.clone();
    let statements = nested_begin["storage"]["functions"][0]["statements"]
        .as_array_mut()
        .unwrap();
    let nested = statements
        .iter()
        .find(|statement| statement.get("for").is_some())
        .cloned()
        .expect("fixture contains a fold statement");
    statements
        .iter_mut()
        .find_map(|statement| statement.get_mut("for"))
        .unwrap()["statements"]
        .as_array_mut()
        .unwrap()
        .push(nested);
    assert!(
        serde_json::from_value::<Dae>(nested_begin).is_err(),
        "a fold cannot begin while another fold capability is active"
    );

    let mut trailing_assignment = canonical;
    let output_index = trailing_assignment["storage"]["expressions"]["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .position(|node| node.get("function_fold_output").is_some())
        .expect("fixture contains a generated fold output");
    let statements = trailing_assignment["storage"]["functions"][0]["statements"]
        .as_array_mut()
        .unwrap();
    let assignments = statements
        .iter_mut()
        .find_map(|statement| statement.get_mut("for"))
        .unwrap()["statements"]
        .as_array_mut()
        .unwrap();
    assignments.last_mut().unwrap()["assignment"]["rhs"] = output_index.into();
    assert!(
        serde_json::from_value::<Dae>(trailing_assignment).is_err(),
        "a fold cannot end while an assignment still waits on its RHS"
    );
}

fn active_loop_fixture() -> (Dae, DaeProvenance) {
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
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real, real], function_at),
            |dae, reservation| {
                let x = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("x"), 0, x_at)
                })?;
                let y = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 1, y_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let zero = dae.expressions(|expressions| {
                    expressions.at(zero_at).literal(DaeLiteral::Real(0.0))
                })?;
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
                let one = dae.expressions(|expressions| {
                    expressions.at(one_at).literal(DaeLiteral::Real(1.0))
                })?;
                dae.functions(|functions| {
                    functions.assign_loop(&mut loop_body, x, one, x_update_at)
                })?;
                let x_value =
                    dae.functions(|functions| functions.read(loop_body.body(), x, x_read_at))?;
                dae.functions(|functions| {
                    functions.assign_loop(&mut loop_body, y, x_value, y_update_at)
                })?;
                let body =
                    dae.functions(|functions| functions.finish_loop(loop_body, finish_at))?;
                dae.functions(|functions| functions.define(body, function_at))
            },
        )
        .map(|_| ())
    })
    .expect("domain-free loop updates remain owned by the loop transition");
    (dae, x_read_at)
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
fn wire_replay_rejects_removed_function_read_shape() {
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
        "wire-v12 must not accept the removed function-read representation"
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
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], function_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("x"), 0, output_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let zero = dae.expressions(|expressions| {
                    expressions.at(zero_at).literal(DaeLiteral::Real(0.0))
                })?;
                dae.functions(|functions| functions.assign(&mut body, output, zero, initial_at))?;
                if !stale_pair {
                    dae.functions(|functions| functions.read(&body, output, old_at))?;
                }
                let one = dae.expressions(|expressions| {
                    expressions.at(one_at).literal(DaeLiteral::Real(1.0))
                })?;
                dae.functions(|functions| functions.assign(&mut body, output, one, update_at))?;
                dae.functions(|functions| functions.read(&body, output, new_at))?;
                if stale_pair {
                    dae.functions(|functions| functions.read(&body, output, again_at))?;
                }
                dae.functions(|functions| functions.define(body, function_at))
            },
        )
        .map(|_| ())
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
