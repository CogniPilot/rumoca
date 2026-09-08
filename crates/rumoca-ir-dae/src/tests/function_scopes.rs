//! Construction-issued fold/scope ownership of function SSA definitions.

use super::*;

/// Build `alpha := 0; beta := 0; for i loop for j loop alpha := alpha + 1;
/// end for; for k loop beta := beta + alpha; end for; end for`.
///
/// The shape matters: two sibling loops inside one enclosing loop is the only
/// arrangement in which a definition can be issued by a region that neither
/// contains nor is contained by the region asking about it.
fn sibling_loops_in_one_enclosing_loop() -> Dae {
    let source = TestSource::new(
        "for i in 1:2 loop for j in 1:3 loop alpha := alpha + 1; end for; \
         for k in 1:2 loop beta := beta + alpha; end for; end for",
    );
    let at = source.source("for i in 1:2 loop", 0);
    Dae::construct(source.map, |dae| {
        let integer =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Integer), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("siblings"), [], [integer, integer], at),
            |dae, reservation| {
                let alpha = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("alpha"), 0, at)
                })?;
                let beta = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("beta"), 1, at)
                })?;
                let (zero, one) = dae.expressions(|expressions| {
                    Ok((
                        expressions.at(at).literal(DaeLiteral::Integer(0))?,
                        expressions.at(at).literal(DaeLiteral::Integer(1))?,
                    ))
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                dae.functions(|functions| {
                    functions.assign(&mut body, alpha, zero, at)?;
                    functions.assign(&mut body, beta, zero, at)
                })?;
                let outer_domain =
                    dae.domains(|domains| domains.structured(scope_range("i", 2), at))?;
                let (first_domain, second_domain) = dae.domains(|domains| {
                    Ok((
                        domains.nested(outer_domain, scope_range("j", 3), at)?,
                        domains.nested(outer_domain, scope_range("k", 2), at)?,
                    ))
                })?;
                let outer = dae.functions(|functions| {
                    functions.begin_loop(body, outer_domain, [alpha, beta], at)
                })?;
                let mut first = dae.functions(|functions| {
                    functions.begin_nested_loop(outer, first_domain, [alpha], at)
                })?;
                let carried_alpha =
                    dae.functions(|functions| functions.read(first.body(), alpha, at))?;
                let incremented = dae.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(BinaryOperator::Add, carried_alpha, one)
                })?;
                dae.functions(|functions| {
                    functions.assign_loop(&mut first, alpha, incremented, at)
                })?;
                let outer = dae.functions(|functions| functions.finish_nested_loop(first, at))?;
                let mut second = dae.functions(|functions| {
                    functions.begin_nested_loop(outer, second_domain, [beta], at)
                })?;
                let completed_alpha =
                    dae.functions(|functions| functions.read(second.body(), alpha, at))?;
                let carried_beta =
                    dae.functions(|functions| functions.read(second.body(), beta, at))?;
                let accumulated = dae.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(BinaryOperator::Add, carried_beta, completed_alpha)
                })?;
                dae.functions(|functions| {
                    functions.assign_loop(&mut second, beta, accumulated, at)
                })?;
                let outer = dae.functions(|functions| functions.finish_nested_loop(second, at))?;
                let body = dae.functions(|functions| functions.finish_loop(outer, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .expect("sibling loops construct as a lexical fold stack")
}

fn scope_range(name: &str, upper: i64) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::new(0),
            display_name: name.to_string(),
            lower: 1,
            upper,
            step: 1,
        }],
    }
}

/// The three regions and the definitions each of them issued.
struct ScopeFacts<'dae> {
    outer: FunctionFoldId<'dae>,
    first: FunctionFoldId<'dae>,
    second: FunctionFoldId<'dae>,
    function: FunctionId<'dae>,
    definition_count: usize,
}

fn scope_facts(view: DaeView<'_>) -> ScopeFacts<'_> {
    let function = view.function_id(0).expect("one function was constructed");
    let entry = view.function(function).expect("function resolves");
    assert_eq!(entry.fold_count(), 3);
    ScopeFacts {
        outer: FunctionFoldId::from_raw(function.index(), 0),
        first: FunctionFoldId::from_raw(function.index(), 1),
        second: FunctionFoldId::from_raw(function.index(), 2),
        function,
        definition_count: entry.definition_count(),
    }
}

/// Definitions the DAE issued while `region` - or a loop inside it - was open.
fn owned_definitions<'dae>(
    view: DaeView<'dae>,
    facts: &ScopeFacts<'dae>,
    region: FunctionFoldId<'dae>,
) -> Vec<u32> {
    let scope = view
        .function_scope(facts.function, Some(region))
        .expect("the region belongs to this function");
    (0..facts.definition_count as u32)
        .filter(|ordinal| {
            let definition = FunctionDefinitionId::from_raw(facts.function.index(), *ordinal);
            scope.relation(definition) == Some(FunctionScopeRelation::Region)
        })
        .collect()
}

#[test]
fn nested_regions_own_exactly_the_definitions_issued_while_they_were_open() {
    let dae = sibling_loops_in_one_enclosing_loop();
    dae.inspect(|view| {
        let facts = scope_facts(view);
        // Definitions in issue order: the two pre-loop assignments, the outer
        // loop's two entry parameters, the first sibling's entry parameter and
        // its update, the first sibling's result, the second sibling's entry
        // parameter and its update, the second sibling's result, and finally
        // the outer loop's two results.
        assert_eq!(facts.definition_count, 12);
        assert_eq!(
            owned_definitions(view, &facts, facts.outer),
            [2, 3, 4, 5, 6, 7, 8, 9]
        );
        assert_eq!(owned_definitions(view, &facts, facts.first), [4, 5]);
        assert_eq!(owned_definitions(view, &facts, facts.second), [7, 8]);
    });
}

#[test]
fn a_loop_result_is_a_value_of_the_scope_that_encloses_the_loop() {
    let dae = sibling_loops_in_one_enclosing_loop();
    dae.inspect(|view| {
        let facts = scope_facts(view);
        let first = view.function_fold(facts.first).expect("fold resolves");
        let [result] = first.output_values().iter().collect::<Vec<_>>()[..] else {
            panic!("the first sibling carries exactly one target")
        };
        // The loop's own body must not claim its result: statements after the
        // loop read it, and a body that owned it would recompute it per
        // iteration instead of capturing it.
        assert_eq!(
            first.body_scope().relation(result.id()),
            Some(FunctionScopeRelation::Enclosing)
        );
        let outer = view
            .function_scope(facts.function, Some(facts.outer))
            .unwrap();
        assert_eq!(
            outer.relation(result.id()),
            Some(FunctionScopeRelation::Region)
        );
        // The second sibling reads that completed value, so its own body sees
        // it as an enclosing-scope capture rather than as work to redo.
        let second = view.function_fold(facts.second).expect("fold resolves");
        assert_eq!(
            second.body_scope().relation(result.id()),
            Some(FunctionScopeRelation::Enclosing)
        );
    });
}

#[test]
fn a_sibling_regions_per_iteration_definition_is_neither_owned_nor_enclosing() {
    let dae = sibling_loops_in_one_enclosing_loop();
    dae.inspect(|view| {
        let facts = scope_facts(view);
        let first = view.function_fold(facts.first).expect("fold resolves");
        let [update] = first.update_values().iter().collect::<Vec<_>>()[..] else {
            panic!("the first sibling carries exactly one target")
        };
        let second = view.function_fold(facts.second).expect("fold resolves");
        assert_eq!(
            second.body_scope().relation(update.id()),
            Some(FunctionScopeRelation::Disjoint)
        );
        assert_eq!(
            first.body_scope().relation(update.id()),
            Some(FunctionScopeRelation::Region)
        );
    });
}

#[test]
fn the_top_level_body_owns_every_definition_and_encloses_none() {
    let dae = sibling_loops_in_one_enclosing_loop();
    dae.inspect(|view| {
        let facts = scope_facts(view);
        let body = view
            .function_scope(facts.function, None)
            .expect("the top-level body is a scope");
        for ordinal in 0..facts.definition_count as u32 {
            let definition = FunctionDefinitionId::from_raw(facts.function.index(), ordinal);
            assert_eq!(
                body.relation(definition),
                Some(FunctionScopeRelation::Region),
                "definition {ordinal} is computed by the function body"
            );
        }
    });
}

#[test]
fn nesting_is_recorded_as_a_parent_relation_and_survives_the_wire() {
    let dae = sibling_loops_in_one_enclosing_loop();
    let expected = dae.inspect(|view| {
        let facts = scope_facts(view);
        assert_eq!(
            view.function_fold(facts.outer).unwrap().enclosing_fold(),
            None
        );
        assert_eq!(
            view.function_fold(facts.first).unwrap().enclosing_fold(),
            Some(facts.outer)
        );
        assert_eq!(
            view.function_fold(facts.second).unwrap().enclosing_fold(),
            Some(facts.outer)
        );
        (
            owned_definitions(view, &facts, facts.outer),
            owned_definitions(view, &facts, facts.first),
            owned_definitions(view, &facts, facts.second),
        )
    });
    // Replay reissues every definition through the same constructors, so the
    // scope relation is rebuilt rather than transported: a wire cannot claim a
    // region ownership its own statement order does not produce.
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    let replayed = decoded.inspect(|view| {
        let facts = scope_facts(view);
        (
            owned_definitions(view, &facts, facts.outer),
            owned_definitions(view, &facts, facts.first),
            owned_definitions(view, &facts, facts.second),
        )
    });
    assert_eq!(replayed, expected);
}

#[test]
fn a_definition_of_another_function_is_rejected_rather_than_classified() {
    let dae = sibling_loops_in_one_enclosing_loop();
    dae.inspect(|view| {
        let facts = scope_facts(view);
        let scope = view
            .function_scope(facts.function, Some(facts.first))
            .expect("the region belongs to this function");
        let foreign = FunctionDefinitionId::from_raw(facts.function.index() + 1, 0);
        assert_eq!(scope.relation(foreign), None);
    });
}

/// `FunctionLoop::domain` names the transition that is open right now.
///
/// The capability is one value that descends into a nested loop and comes back
/// out of it, so its fold and its domain have to move together. A domain left
/// behind on the way out hands every statement after the inner loop the inner
/// iteration space, which is a wrong answer rather than a refused one.
#[test]
fn a_loop_capability_names_the_domain_of_the_transition_that_is_open() {
    let source =
        TestSource::new("for i in 1:2 loop for j in 1:3 loop alpha := alpha + 1; end for; end for");
    let at = source.source("for i in 1:2 loop", 0);
    Dae::construct(source.map, |dae| {
        let integer =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Integer), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("nested"), [], [integer], at),
            |dae, reservation| {
                let alpha = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("alpha"), 0, at)
                })?;
                let (zero, one) = dae.expressions(|expressions| {
                    Ok((
                        expressions.at(at).literal(DaeLiteral::Integer(0))?,
                        expressions.at(at).literal(DaeLiteral::Integer(1))?,
                    ))
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                dae.functions(|functions| functions.assign(&mut body, alpha, zero, at))?;
                let outer_domain =
                    dae.domains(|domains| domains.structured(scope_range("i", 2), at))?;
                let inner_domain =
                    dae.domains(|domains| domains.nested(outer_domain, scope_range("j", 3), at))?;
                let outer = dae
                    .functions(|functions| functions.begin_loop(body, outer_domain, [alpha], at))?;
                assert_eq!(outer.domain(), outer_domain);
                let mut inner = dae.functions(|functions| {
                    functions.begin_nested_loop(outer, inner_domain, [alpha], at)
                })?;
                assert_eq!(inner.domain(), inner_domain);
                let carried = dae.functions(|functions| functions.read(inner.body(), alpha, at))?;
                let incremented = dae.expressions(|expressions| {
                    expressions.at(at).binary(BinaryOperator::Add, carried, one)
                })?;
                dae.functions(|functions| {
                    functions.assign_loop(&mut inner, alpha, incremented, at)
                })?;
                let outer = dae.functions(|functions| functions.finish_nested_loop(inner, at))?;
                assert_eq!(outer.domain(), outer_domain);
                let body = dae.functions(|functions| functions.finish_loop(outer, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .expect("one nested loop constructs");
}
