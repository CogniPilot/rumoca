use super::*;

const CONDITIONAL_SOURCE: &str = "function choose input Boolean c1; input Boolean c2; \
    input Real x; output Real y; algorithm y := if c1 then x elseif c2 then x else x; end choose;";

#[test]
fn callable_conditionals_issue_an_ordered_lazy_region_chain() {
    let (dae, conditional_at) = conditional_fixture();
    dae.inspect(|view| assert_conditional_inventory(view, conditional_at));
    serde_json::from_value::<Dae>(serde_json::to_value(&dae).unwrap())
        .unwrap()
        .inspect(|view| assert_conditional_inventory(view, conditional_at));
    bincode::deserialize::<Dae>(&bincode::serialize(&dae).unwrap())
        .unwrap()
        .inspect(|view| assert_conditional_inventory(view, conditional_at));
}

fn assert_conditional_inventory(view: DaeView<'_>, conditional_at: DaeProvenance) {
    view.with_callable_source_inventory(|inventory| {
        assert_lazy_conditional_regions(&inventory, conditional_at);
    });
}

fn conditional_fixture() -> (Dae, DaeProvenance) {
    let source = TestSource::new(CONDITIONAL_SOURCE);
    let function_at = source.source("function choose", 0);
    let conditional_at = source.source("if c1 then x elseif c2 then x else x", 0);
    let dae = Dae::construct(source.map, |dae| {
        let boolean =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Boolean), function_at))?;
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(
                VarName::new("choose"),
                [boolean, boolean, real],
                [real],
                function_at,
            ),
            |dae, reservation| {
                define_conditional_body(dae, reservation, function_at, conditional_at)
            },
        )?;
        Ok(())
    })
    .expect("the ordered conditional fixture constructs");
    (dae, conditional_at)
}

fn define_conditional_body<'dae>(
    dae: &mut DaeConstruction<'dae>,
    reservation: FunctionReservation<'_, 'dae>,
    function_at: DaeProvenance,
    conditional_at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let c1 = dae.functions(|functions| {
        functions.parameter(&reservation, VarName::new("c1"), 0, function_at)
    })?;
    let c2 = dae.functions(|functions| {
        functions.parameter(&reservation, VarName::new("c2"), 1, function_at)
    })?;
    let x = dae.functions(|functions| {
        functions.parameter(&reservation, VarName::new("x"), 2, function_at)
    })?;
    let y = dae
        .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, function_at))?;
    let [c1, c2, x] = dae.expressions(|expressions| {
        Ok([
            expressions.at(conditional_at).function_parameter(c1)?,
            expressions.at(conditional_at).function_parameter(c2)?,
            expressions.at(conditional_at).function_parameter(x)?,
        ])
    })?;
    let mut body = dae.functions(|functions| functions.begin(reservation, function_at))?;
    dae.functions(|functions| {
        functions.assign_conditional_all(
            &mut body,
            &[y],
            &[c1, c2],
            &[vec![x], vec![x]],
            &[x],
            conditional_at,
        )?;
        functions.define(body, function_at)
    })
}

fn assert_lazy_conditional_regions<'inventory, 'dae>(
    inventory: &CallableSourceInventoryView<'inventory, 'dae>,
    conditional_at: DaeProvenance,
) {
    let family = inventory
        .conditional_regions()
        .next()
        .expect("one assignment conditional owns source regions");
    assert_eq!(family.provenance(), conditional_at);
    let guards = family.guards().collect::<Vec<_>>();
    let results = family.results().collect::<Vec<_>>();
    assert_eq!(guards.len(), 2);
    assert_eq!(results.len(), 2);
    let condition_uses = guards
        .iter()
        .map(|guard| {
            inventory
                .expression_uses()
                .find(|occurrence| occurrence.region() == *guard)
                .expect("every guard owns its condition use")
                .id()
        })
        .collect::<Vec<_>>();
    assert_region(
        inventory,
        guards[0],
        Some(family.parent()),
        CallableSourceRegionActivation::Enter,
    );
    assert_region(
        inventory,
        results[0],
        Some(guards[0]),
        CallableSourceRegionActivation::GuardTrue(condition_uses[0]),
    );
    assert_region(
        inventory,
        guards[1],
        Some(guards[0]),
        CallableSourceRegionActivation::GuardFalse(condition_uses[0]),
    );
    assert_region(
        inventory,
        results[1],
        Some(guards[1]),
        CallableSourceRegionActivation::GuardTrue(condition_uses[1]),
    );
    assert_region(
        inventory,
        family.fallback(),
        Some(guards[1]),
        CallableSourceRegionActivation::GuardFalse(condition_uses[1]),
    );
    assert_distinct_branch_uses_and_dominating_captures(
        inventory,
        family.parent(),
        &results,
        family.fallback(),
    );
}

fn assert_region<'inventory, 'dae>(
    inventory: &CallableSourceInventoryView<'inventory, 'dae>,
    id: CallableSourceRegionId<'inventory, 'dae>,
    parent: Option<CallableSourceRegionId<'inventory, 'dae>>,
    activation: CallableSourceRegionActivation<'inventory, 'dae>,
) {
    let region = inventory
        .source_regions()
        .find(|occurrence| occurrence.id() == id)
        .expect("issued source region resolves in its own inventory");
    assert_eq!(region.parent(), parent);
    assert_eq!(region.activation(), activation);
}

fn assert_distinct_branch_uses_and_dominating_captures<'inventory, 'dae>(
    inventory: &CallableSourceInventoryView<'inventory, 'dae>,
    root: CallableSourceRegionId<'inventory, 'dae>,
    results: &[CallableSourceRegionId<'inventory, 'dae>],
    fallback: CallableSourceRegionId<'inventory, 'dae>,
) {
    let mut x_uses = inventory
        .expression_uses()
        .filter(|occurrence| [results[0], results[1], fallback].contains(&occurrence.region()))
        .collect::<Vec<_>>();
    x_uses.sort_unstable_by_key(|occurrence| occurrence.region());
    assert_eq!(x_uses.len(), 3, "one ExprId has one use per lazy region");
    assert!(x_uses.windows(2).all(|pair| pair[0].id() != pair[1].id()));
    for expression_use in x_uses {
        let capture = inventory
            .captures()
            .find(|capture| capture.expression_use() == expression_use.id())
            .expect("each branch parameter use owns a capture edge");
        assert_eq!(capture.source_region(), root);
        assert_eq!(capture.region(), expression_use.region());
        assert!(matches!(
            capture.source(),
            CallableCaptureSource::FunctionParameter(_)
        ));
    }
}

#[test]
fn conditional_wire_cannot_restate_order_or_branch_operands() {
    let (dae, _) = conditional_fixture();
    let canonical = serde_json::to_value(&dae).unwrap();

    let mut reordered = canonical.clone();
    reordered["storage"]["functions"][0]["body"]["modelica"]["statements"][0]
        ["assignment_group"]["conditional"]["conditions"]
        .as_array_mut()
        .unwrap()
        .swap(0, 1);
    assert!(
        serde_json::from_value::<Dae>(reordered).is_err(),
        "wire replay cannot restate the issued guard order"
    );

    let mut wrong_branch = canonical;
    let condition = wrong_branch["storage"]["functions"][0]["body"]["modelica"]
        ["statements"][0]["assignment_group"]["conditional"]["conditions"][0]
        .clone();
    wrong_branch["storage"]["functions"][0]["body"]["modelica"]["statements"][0]["assignment_group"]
        ["conditional"]["branches"][0][0] = condition;
    assert!(
        serde_json::from_value::<Dae>(wrong_branch).is_err(),
        "wire replay cannot attach a foreign branch operand to the joined expression"
    );
}

#[test]
fn output_only_external_body_remains_a_mandatory_occurrence() {
    let (dae, external_at) = output_only_external_fixture();
    dae.inspect(|view| assert_output_only_external_inventory(view, external_at));
    serde_json::from_value::<Dae>(serde_json::to_value(&dae).unwrap())
        .unwrap()
        .inspect(|view| assert_output_only_external_inventory(view, external_at));
    bincode::deserialize::<Dae>(&bincode::serialize(&dae).unwrap())
        .unwrap()
        .inspect(|view| assert_output_only_external_inventory(view, external_at));

    let mut omitted = serde_json::to_value(&dae).unwrap();
    omitted["storage"]["functions"][0]
        .as_object_mut()
        .unwrap()
        .remove("body");
    assert!(
        serde_json::from_value::<Dae>(omitted).is_err(),
        "an output-only external body cannot become an empty Modelica body"
    );

    let mut invalid_provenance = serde_json::to_value(&dae).unwrap();
    invalid_provenance["storage"]["functions"][0]["body"]["external"]["body"]["provenance"]["span"]
        ["start"] = serde_json::Value::from(u64::MAX);
    assert!(
        serde_json::from_value::<Dae>(invalid_provenance).is_err(),
        "external-body provenance must re-enter checked construction"
    );
}

fn assert_output_only_external_inventory(view: DaeView<'_>, external_at: DaeProvenance) {
    view.with_callable_source_inventory(|inventory| {
        let body = inventory
            .external_bodies()
            .next()
            .expect("an output-only external body cannot disappear");
        assert_eq!(inventory.external_bodies().len(), 1);
        assert_eq!(inventory.expression_uses().len(), 0);
        let root = inventory.source_regions().next().unwrap();
        assert_eq!(body.function(), root.function());
        assert_eq!(body.region(), root.id());
        assert_eq!(body.provenance(), external_at);
        let function = view.function(body.function()).unwrap();
        assert_eq!(function.external().unwrap().provenance(), external_at);
    });
}

fn output_only_external_fixture() -> (Dae, DaeProvenance) {
    let source = TestSource::new(
        "pure function source output Real y; external \"C\" y = source_value(); end source;",
    );
    let function_at = source.source("pure function source", 0);
    let external_at = source.source("external \"C\" y = source_value()", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("source"), [], [real], function_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, function_at)
                })?;
                let external = ExternalFunctionBody::new(
                    FunctionPurity::Pure,
                    ExternalLanguage::C,
                    VarName::new("source_value"),
                    [],
                    Some(output),
                    ExternalLinkage::new([], None, None, None),
                );
                dae.functions(|functions| {
                    functions.define_external(reservation, external, external_at)
                })
            },
        )?;
        Ok(())
    })
    .expect("an output-only external body constructs");
    (dae, external_at)
}

#[test]
fn repeated_call_result_ordinals_remain_distinct_region_projections() {
    let dae = repeated_projection_fixture();
    dae.inspect(|view| {
        view.with_callable_source_inventory(|inventory| {
            let caller = inventory
                .functions()
                .nth(1)
                .expect("caller occurrence exists");
            assert_eq!(caller.calls().len(), 1);
            assert_eq!(
                caller.call(0).unwrap().call(),
                caller.calls().next().unwrap().call()
            );
            assert!(caller.call(1).is_none());
            let call = inventory
                .call_uses()
                .next()
                .expect("the caller owns one invocation");
            assert_eq!(inventory.call_uses().len(), 1);
            let projections = call.projections().collect::<Vec<_>>();
            assert_eq!(projections.len(), 2);
            assert_eq!([projections[0].output(), projections[1].output()], [1, 1]);
            assert_ne!(
                projections[0].expression_use(),
                projections[1].expression_use()
            );
            assert_eq!(
                projections
                    .iter()
                    .filter(|projection| projection.expression_use() == call.owner())
                    .count(),
                1
            );
        });
    });
}

fn repeated_projection_fixture() -> Dae {
    let source = TestSource::new(
        "function pair input Real u; output Real a; output Real b; end pair; \
         function duplicate output Real x; output Real y; algorithm (x,y) := pair(); end duplicate;",
    );
    let pair_at = source.source("function pair", 0);
    let duplicate_at = source.source("function duplicate", 0);
    let call_at = source.source("pair()", 0);
    Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), pair_at))?;
        let pair = define_pair(dae, real, pair_at)?;
        dae.function(
            FunctionSignature::new(VarName::new("duplicate"), [], [real, real], duplicate_at),
            |dae, reservation| {
                let x = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("x"), 0, duplicate_at)
                })?;
                let y = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 1, duplicate_at)
                })?;
                let values = dae.expressions(|expressions| {
                    expressions.at(call_at).call_results(pair, [1, 1], [])
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, duplicate_at))?;
                dae.functions(|functions| {
                    functions.assign_all(
                        &mut body,
                        &[(x, values[0]), (y, values[1])],
                        duplicate_at,
                    )?;
                    functions.define(body, duplicate_at)
                })
            },
        )?;
        Ok(())
    })
    .expect("repeated result ordinals remain legal occurrences")
}

fn define_pair<'dae>(
    dae: &mut DaeConstruction<'dae>,
    real: ValueTypeId<'dae>,
    at: DaeProvenance,
) -> Result<FunctionId<'dae>, DaeConstructionError> {
    dae.function(
        FunctionSignature::new(VarName::new("pair"), [], [real, real], at),
        |dae, reservation| {
            let a = dae
                .functions(|functions| functions.output(&reservation, VarName::new("a"), 0, at))?;
            let b = dae
                .functions(|functions| functions.output(&reservation, VarName::new("b"), 1, at))?;
            let values = dae.expressions(|expressions| {
                Ok([
                    expressions.at(at).literal(DaeLiteral::Real(1.0))?,
                    expressions.at(at).literal(DaeLiteral::Real(2.0))?,
                ])
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
            dae.functions(|functions| {
                functions.assign_all(&mut body, &[(a, values[0]), (b, values[1])], at)?;
                functions.define(body, at)
            })
        },
    )
    .map(|(function, ())| function)
}
