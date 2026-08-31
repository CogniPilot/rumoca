use rumoca_core::{
    OperationContractKey, SourceMap, Span, StructuredIndexBinder, StructuredIndexBinderId,
    StructuredIndexDomain, VarName,
};
use rumoca_ir_dae::{Dae, DaeLiteral, FunctionSignature, ScalarType, ValueType};

use super::{
    CallableExpressionSource, CallableInterface, CallablePlan, PendingOwner, PlanConstructionError,
    topological_owner_order,
};
use crate::model::OwnerCallOccurrence;

mod calls;
mod update_element;

fn one_constant_function() -> Dae {
    let mut source_map = SourceMap::new();
    let source = source_map.add(
        "callable-test.mo",
        "function f output Real y; algorithm y := 1.0; end f;",
    );
    let span = Span::from_offsets(source, 0, 1);
    let at = rumoca_ir_dae::DaeProvenance::source(span).expect("test span is source-backed");
    Dae::construct(source_map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                let literal = dae
                    .expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(1.0)))?;
                dae.functions(|functions| functions.assign(&mut body, output, literal, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .expect("fixture DAE is construction-valid")
}
/// `if c then 1.0 else 2.0` inside one function, with the conditional
/// expression carrying its own span so a refusal that cites the region can be
/// distinguished from one that cites the enclosing declaration.
///
/// Returns the DAE and the exact span of that conditional expression.
fn one_conditional_function() -> (Dae, Span) {
    const TEXT: &str = "function choose input Boolean c; output Real y; algorithm y := if c then 1.0 else 2.0; end choose;";
    let mut source_map = SourceMap::new();
    let source = source_map.add("callable-conditional.mo", TEXT);
    let span = Span::from_offsets(source, 0, 1);
    let region_offset = TEXT.find("if c then").expect("the conditional expression");
    let region_span = Span::from_offsets(source, region_offset, region_offset + 24);
    let at = rumoca_ir_dae::DaeProvenance::source(span).expect("test span is source-backed");
    let region_at =
        rumoca_ir_dae::DaeProvenance::source(region_span).expect("region span is source-backed");
    let dae = Dae::construct(source_map, |dae| {
        let boolean =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Boolean), at))?;
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("choose"), [boolean], [real], at),
            |dae, reservation| define_conditional_choice(dae, reservation, at, region_at),
        )?;
        Ok(())
    })
    .expect("conditional fixture DAE is construction-valid");
    (dae, region_span)
}

/// The body of the `choose` fixture: one conditional assignment whose
/// conditional expression carries `region_at`.
fn define_conditional_choice<'function, 'dae>(
    dae: &mut rumoca_ir_dae::DaeConstruction<'dae>,
    reservation: rumoca_ir_dae::FunctionReservation<'function, 'dae>,
    at: rumoca_ir_dae::DaeProvenance,
    region_at: rumoca_ir_dae::DaeProvenance,
) -> Result<(), rumoca_ir_dae::DaeConstructionError> {
    let parameter =
        dae.functions(|functions| functions.parameter(&reservation, VarName::new("c"), 0, at))?;
    let output =
        dae.functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
    let condition =
        dae.expressions(|expressions| expressions.at(at).function_parameter(parameter))?;
    let one = dae.expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(1.0)))?;
    let two = dae.expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(2.0)))?;
    let conditional = dae.expressions(|expressions| {
        expressions
            .at(region_at)
            .conditional([(condition, one)], two)
    })?;
    let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
    dae.functions(|functions| functions.assign(&mut body, output, conditional, at))?;
    dae.functions(|functions| functions.define(body, at))?;
    Ok(())
}

fn nested_conditional_reusing_condition_function() -> Dae {
    let mut source_map = SourceMap::new();
    let source = source_map.add(
        "callable-nested-conditional.mo",
        "function choose input Boolean c; output Real y; algorithm y := if c then (if c then 1.0 else 2.0) else 3.0; end choose;",
    );
    let span = Span::from_offsets(source, 0, 1);
    let at = rumoca_ir_dae::DaeProvenance::source(span).expect("test span is source-backed");
    Dae::construct(source_map, |dae| {
        let boolean =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Boolean), at))?;
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("choose"), [boolean], [real], at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("c"), 0, at)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let condition = dae
                    .expressions(|expressions| expressions.at(at).function_parameter(parameter))?;
                let one = dae
                    .expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(1.0)))?;
                let two = dae
                    .expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(2.0)))?;
                let three = dae
                    .expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(3.0)))?;
                let inner = dae.expressions(|expressions| {
                    expressions.at(at).conditional([(condition, one)], two)
                })?;
                let outer = dae.expressions(|expressions| {
                    expressions.at(at).conditional([(condition, inner)], three)
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                dae.functions(|functions| functions.assign(&mut body, output, outer, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .expect("nested conditional fixture DAE is construction-valid")
}

fn one_atomic_conditional_group_function() -> Dae {
    let mut source_map = SourceMap::new();
    let source = source_map.add(
        "callable-conditional-group.mo",
        "function choose2 input Boolean c; output Real y; output Real z; algorithm if c then y := 1.0; z := 2.0; else y := 3.0; z := 4.0; end if; end choose2;",
    );
    let span = Span::from_offsets(source, 0, 1);
    let at = rumoca_ir_dae::DaeProvenance::source(span).expect("test span is source-backed");
    Dae::construct(source_map, |dae| {
        let boolean =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Boolean), at))?;
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("choose2"), [boolean], [real, real], at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("c"), 0, at)
                })?;
                let y = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let z = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("z"), 1, at)
                })?;
                let condition = dae
                    .expressions(|expressions| expressions.at(at).function_parameter(parameter))?;
                let one = dae
                    .expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(1.0)))?;
                let two = dae
                    .expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(2.0)))?;
                let three = dae
                    .expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(3.0)))?;
                let four = dae
                    .expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(4.0)))?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                dae.functions(|functions| {
                    functions.assign_conditional_all(
                        &mut body,
                        &[y, z],
                        &[condition],
                        &[vec![one, two]],
                        &[three, four],
                        at,
                    )
                })?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .expect("atomic conditional group DAE is construction-valid")
}

fn one_mixed_add_function() -> Dae {
    let mut source_map = SourceMap::new();
    let source = source_map.add(
        "callable-conversion.mo",
        "function mixed output Real y; algorithm y := 1 + 2.0; end mixed;",
    );
    let span = Span::from_offsets(source, 0, 1);
    let at = rumoca_ir_dae::DaeProvenance::source(span).expect("test span is source-backed");
    Dae::construct(source_map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("mixed"), [], [real], at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let integer = dae.expressions(|expressions| {
                    expressions.at(at).literal(DaeLiteral::Integer(1))
                })?;
                let real = dae
                    .expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(2.0)))?;
                let sum = dae.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(rumoca_ir_dae::BinaryOperator::Add, integer, real)
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                dae.functions(|functions| functions.assign(&mut body, output, sum, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .expect("mixed-add fixture DAE is construction-valid")
}

fn one_dynamic_index_function() -> Dae {
    let mut source_map = SourceMap::new();
    let source = source_map.add(
        "callable-index.mo",
        "function pick input Integer i; output Real y; algorithm y := {1.0,2.0}[i]; end pick;",
    );
    let span = Span::from_offsets(source, 0, 1);
    let at = rumoca_ir_dae::DaeProvenance::source(span).expect("test span is source-backed");
    Dae::construct(source_map, |dae| {
        let integer =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Integer), at))?;
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("pick"), [integer], [real], at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("i"), 0, at)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let index = dae
                    .expressions(|expressions| expressions.at(at).function_parameter(parameter))?;
                let one = dae
                    .expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(1.0)))?;
                let two = dae
                    .expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(2.0)))?;
                let array = dae.expressions(|expressions| expressions.at(at).array([one, two]))?;
                let selected = dae.expressions(|expressions| {
                    expressions.at(at).index(
                        array,
                        [rumoca_ir_dae::Subscript::Index {
                            expression: index,
                            provenance: at,
                        }],
                    )
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                dae.functions(|functions| functions.assign(&mut body, output, selected, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .expect("dynamic-index fixture DAE is structurally valid")
}

fn one_map_function() -> Dae {
    let mut source_map = SourceMap::new();
    let source = source_map.add(
        "callable-map.mo",
        "function indices output Integer y[2]; algorithm y := {k for k in 1:2}; end indices;",
    );
    let span = Span::from_offsets(source, 0, 1);
    let at = rumoca_ir_dae::DaeProvenance::source(span).expect("test span is source-backed");
    Dae::construct(source_map, |dae| {
        let integers =
            dae.types(|types| types.derived(ValueType::array(ScalarType::Integer, [2]), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("indices"), [], [integers], at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let domain = dae.domains(|domains| {
                    domains.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                id: StructuredIndexBinderId::new(0),
                                display_name: "k".to_owned(),
                                lower: 1,
                                upper: 2,
                                step: 1,
                            }],
                        },
                        at,
                    )
                })?;
                let binder = dae.domains(|domains| domains.binder(domain, 0, at))?;
                let body = dae.expressions(|expressions| expressions.at(at).binder(binder))?;
                let map =
                    dae.expressions(|expressions| expressions.at(at).comprehension(domain, body))?;
                let mut function = dae.functions(|functions| functions.begin(reservation, at))?;
                dae.functions(|functions| functions.assign(&mut function, output, map, at))?;
                dae.functions(|functions| functions.define(function, at))
            },
        )?;
        Ok(())
    })
    .expect("map fixture DAE is construction-valid")
}

/// Two functions that call each other, with the two call occurrences carrying
/// their own spans so a refusal citing a declaration cannot pass as a refusal
/// citing the recursive call.
///
/// The returned spans are ordered by owner: index 0 is the call `f` makes to
/// `g`, index 1 is the call `g` makes back to `f`.
fn mutually_recursive_functions() -> (Dae, [Span; 2], Span) {
    const TEXT: &str = "function f output Real y; algorithm y := g(); end f; \
function g output Real y; algorithm y := f(); end g;";
    let mut source_map = SourceMap::new();
    let source = source_map.add("callable-cycle.mo", TEXT);
    let declaration_span = Span::from_offsets(source, 0, 10);
    let at =
        rumoca_ir_dae::DaeProvenance::source(declaration_span).expect("test span is source-backed");
    let call_spans = [
        TEXT.find("g();").expect("f calls g"),
        TEXT.find("f();").expect("g calls f"),
    ]
    .map(|offset| Span::from_offsets(source, offset, offset + 3));
    let dae = Dae::construct(source_map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        let first = FunctionSignature::new(VarName::new("f"), [], [real], at);
        let second = FunctionSignature::new(VarName::new("g"), [], [real], at);
        dae.recursive_functions(first, [second], |dae, reservations| {
            define_recursive_group(dae, reservations, call_spans, at)
        })?;
        Ok(())
    })
    .expect("recursive DAE group is source-valid");
    (dae, call_spans, declaration_span)
}

#[derive(Clone, Copy)]
enum ConditionalGroupCase {
    Exact,
    Partial,
    Permuted,
    Split,
    ParentCountUnderflow,
}

fn construct_conditional_group(
    case: ConditionalGroupCase,
) -> Result<CallablePlan, PlanConstructionError> {
    CallablePlan::construct::<PlanConstructionError, _>(
        one_atomic_conditional_group_function(),
        |dae, construction| {
            let function = construction.functions().next().expect("one owner");
            let sources = construction.expressions().collect::<Vec<_>>();
            let condition = condition_source(dae, &sources);
            let one = real_literal_source(dae, &sources, 1.0);
            let two = real_literal_source(dae, &sources, 2.0);
            let three = real_literal_source(dae, &sources, 3.0);
            let four = real_literal_source(dae, &sources, 4.0);
            let joined = conditional_sources(dae, &sources);
            let group = construction
                .conditionals()
                .next()
                .expect("one atomic group");
            let definitions = construction.definitions().collect::<Vec<_>>();
            let owner = construction.add_owner(function)?;
            let root = construction.root_scope(owner);
            let condition_value = construction.add_value_operation(
                root,
                condition,
                OperationContractKey::Load,
                &[],
            )?;
            if matches!(case, ConditionalGroupCase::Split) {
                // Planted mutation: reopen a joined conditional as its own
                // region. `open_conditional` refuses immediately because the
                // expression already belongs to an atomic group, so the plan
                // never reaches `finish`.
                let _refused = construction.open_conditional(root, joined[0])?;
                return Ok(());
            }
            let region = construction.open_conditional_group(root, group)?;
            if matches!(case, ConditionalGroupCase::ParentCountUnderflow) {
                construction.scopes[root.raw as usize].open_children = 0;
            }
            let branch = region.branch_scopes().next().expect("one branch");
            let fallback = region.fallback_scope();
            let one_value = construction.add_value_operation(
                branch,
                one,
                OperationContractKey::Constant,
                &[],
            )?;
            let two_value = construction.add_value_operation(
                branch,
                two,
                OperationContractKey::Constant,
                &[],
            )?;
            let three_value = construction.add_value_operation(
                fallback,
                three,
                OperationContractKey::Constant,
                &[],
            )?;
            let four_value = construction.add_value_operation(
                fallback,
                four,
                OperationContractKey::Constant,
                &[],
            )?;
            let branches = match case {
                ConditionalGroupCase::Exact => vec![vec![one_value, two_value]],
                ConditionalGroupCase::Partial => vec![vec![one_value]],
                ConditionalGroupCase::Permuted => vec![vec![two_value, one_value]],
                ConditionalGroupCase::Split => return Ok(()),
                ConditionalGroupCase::ParentCountUnderflow => vec![vec![one_value, two_value]],
            };
            let destinations = construction.close_conditional_group(
                region,
                &[condition_value],
                &branches,
                &[three_value, four_value],
            )?;
            for (definition, destination) in definitions.iter().copied().zip(destinations) {
                construction.add_store(root, definition, destination)?;
            }
            Ok(())
        },
    )
}

#[test]
fn complete_constant_plan_has_total_correlated_views_and_derived_counts() {
    let plan = CallablePlan::construct::<PlanConstructionError, _>(
        one_constant_function(),
        |_dae, construction| {
            let function = construction
                .functions()
                .next()
                .expect("fixture has one callable owner");
            let expression = construction
                .expressions()
                .next()
                .expect("fixture has one expression occurrence");
            let definition = construction
                .definitions()
                .next()
                .expect("fixture has one definition occurrence");
            let owner = construction.add_owner(function)?;
            let scope = construction.root_scope(owner);
            let value = construction.add_value_operation(
                scope,
                expression,
                OperationContractKey::Constant,
                &[],
            )?;
            construction.add_store(scope, definition, value)
        },
    )
    .expect("complete source obligations mint one plan");

    let counts = plan.counters();
    assert_eq!(counts.source_functions(), 1);
    assert_eq!(counts.source_expressions(), 1);
    assert_eq!(counts.source_definitions(), 1);
    assert_eq!(counts.evidence_discharges(), 0);
    assert_eq!(counts.owners(), 1);
    assert_eq!(counts.scopes(), 1);
    assert_eq!(counts.operations(), 2);
    plan.inspect(|view| {
        let owner = view.owners().next().expect("one owner view");
        assert_eq!(owner.source_function().name().as_str(), "f");
        assert_eq!(owner.operations().len(), 2);
        assert_eq!(
            view.scopes().next().expect("root scope").operations().len(),
            2
        );
        assert_eq!(view.operations().len(), 2);
        assert_eq!(
            view.values()
                .next()
                .expect("literal value")
                .source_expression()
                .provenance(),
            owner.declaration()
        );
    });
}

#[test]
fn first_contract_error_poison_is_stable_after_caller_catches_it() {
    let result = CallablePlan::construct::<PlanConstructionError, _>(
        one_constant_function(),
        |_dae, construction| {
            let function = construction.functions().next().expect("one owner");
            let expression = construction.expressions().next().expect("one expression");
            let owner = construction.add_owner(function)?;
            let scope = construction.root_scope(owner);
            let first = construction
                .add_value_operation(scope, expression, OperationContractKey::UnarySinReal, &[])
                .expect_err("wrong contract key poisons construction");
            let repeated = construction
                .add_value_operation(scope, expression, OperationContractKey::Constant, &[])
                .expect_err("later mutation cites original poison");
            assert_eq!(repeated, first);
            Ok(())
        },
    );
    assert!(matches!(
        result,
        Err(PlanConstructionError::InvalidOperation { .. })
    ));
}

#[test]
fn incomplete_coverage_refuses_instead_of_minting_a_partial_plan() {
    let result = CallablePlan::construct::<PlanConstructionError, _>(
        one_constant_function(),
        |_dae, construction| {
            let function = construction.functions().next().expect("one owner");
            construction.add_owner(function)?;
            Ok(())
        },
    );
    assert!(matches!(
        result,
        Err(PlanConstructionError::IncompleteCoverage { .. })
    ));
}

#[test]
fn conditional_close_publishes_one_parent_value_and_closes_children() {
    let plan = CallablePlan::construct::<PlanConstructionError, _>(
        one_conditional_function().0,
        |dae, construction| {
            let function = construction.functions().next().expect("one owner");
            let sources = construction.expressions().collect::<Vec<_>>();
            let condition = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Coordinate(_)
                    )
                })
                .expect("condition source");
            let mut literals = sources.iter().copied().filter(|source| {
                matches!(
                    dae.exact_expression(source.expression()).operation(),
                    rumoca_ir_dae::ExpressionOperation::Literal(DaeLiteral::Real(_))
                )
            });
            let branch_source = literals.next().expect("branch literal");
            let fallback_source = literals.next().expect("fallback literal");
            let conditional_source = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Conditional(_)
                    )
                })
                .expect("conditional source");
            let definition = construction.definitions().next().expect("one store");
            let owner = construction.add_owner(function)?;
            let root = construction.root_scope(owner);
            let condition_value = construction.add_value_operation(
                root,
                condition,
                OperationContractKey::Load,
                &[],
            )?;
            let region = construction.open_conditional(root, conditional_source)?;
            let branch = region.branch_scopes().next().expect("one branch scope");
            let fallback = region.fallback_scope();
            let branch_value = construction.add_value_operation(
                branch,
                branch_source,
                OperationContractKey::Constant,
                &[],
            )?;
            let fallback_value = construction.add_value_operation(
                fallback,
                fallback_source,
                OperationContractKey::Constant,
                &[],
            )?;
            let result = construction.close_conditional(
                region,
                &[condition_value],
                &[branch_value],
                fallback_value,
            )?;
            construction.add_store(root, definition, result)
        },
    )
    .expect("closed conditional constructs");

    plan.inspect(|view| {
        assert_eq!(view.regions().len(), 1);
        assert_eq!(view.scopes().len(), 3);
        let region = view.regions().next().expect("conditional region");
        let crate::CallableRegionDetailView::Conditional(detail) = region.detail() else {
            panic!("fixture region remains conditional")
        };
        assert_eq!(detail.branch_scopes().len(), 1);
        assert_eq!(detail.conditions().len(), 1);
        assert_eq!(detail.branch_count(), 1);
        assert_eq!(
            detail.branch_results(0).expect("first branch tuple").len(),
            1
        );
        assert_eq!(detail.fallback_results().len(), 1);
    });
}

#[test]
fn a_condition_reused_in_a_branch_is_not_duplicated_as_a_capture() {
    let plan = CallablePlan::construct::<PlanConstructionError, _>(
        nested_conditional_reusing_condition_function(),
        |dae, construction| {
            let function = construction.functions().next().expect("one owner");
            let sources = construction.expressions().collect::<Vec<_>>();
            let condition = condition_source(dae, &sources);
            let (outer, inner) = nested_conditional_sources(dae, &sources);
            let definition = construction.definitions().next().expect("one store");
            let owner = construction.add_owner(function)?;
            let root = construction.root_scope(owner);
            let condition_value = construction.add_value_operation(
                root,
                condition,
                OperationContractKey::Load,
                &[],
            )?;
            let outer_region = construction.open_conditional(root, outer)?;
            let outer_branch = outer_region
                .branch_scopes()
                .next()
                .expect("outer branch scope");
            let outer_fallback = outer_region.fallback_scope();
            let inner_region = construction.open_conditional(outer_branch, inner)?;
            let inner_branch = inner_region
                .branch_scopes()
                .next()
                .expect("inner branch scope");
            let inner_fallback = inner_region.fallback_scope();
            let one = construction.add_value_operation(
                inner_branch,
                real_literal_source(dae, &sources, 1.0),
                OperationContractKey::Constant,
                &[],
            )?;
            let two = construction.add_value_operation(
                inner_fallback,
                real_literal_source(dae, &sources, 2.0),
                OperationContractKey::Constant,
                &[],
            )?;
            let inner_value =
                construction.close_conditional(inner_region, &[condition_value], &[one], two)?;
            let three = construction.add_value_operation(
                outer_fallback,
                real_literal_source(dae, &sources, 3.0),
                OperationContractKey::Constant,
                &[],
            )?;
            let outer_value = construction.close_conditional(
                outer_region,
                &[condition_value],
                &[inner_value],
                three,
            )?;
            construction.add_store(root, definition, outer_value)
        },
    )
    .expect("nested conditionals construct without duplicating condition inputs");

    plan.inspect(|view| {
        for operation in view
            .operations()
            .filter(|operation| operation.contract() == OperationContractKey::Conditional)
        {
            assert_eq!(operation.operands().len(), 1);
        }
        for region in view.regions() {
            assert_eq!(region.captures().len(), 0);
        }
    });
}

#[test]
fn atomic_conditional_group_publishes_its_exact_result_tuple_once() {
    let plan = construct_conditional_group(ConditionalGroupCase::Exact)
        .expect("exact atomic conditional group constructs");
    assert_eq!(plan.counters().source_conditionals(), 1);
    plan.inspect(|view| {
        let operation = view
            .operations()
            .find(|operation| operation.contract() == OperationContractKey::Conditional)
            .expect("one atomic Conditional operation");
        assert_eq!(operation.results().len(), 2);
        let crate::CallableOperationSourceView::ConditionalGroup(source) = operation.source()
        else {
            panic!("atomic group retains its exact correlated source")
        };
        assert_eq!(source.expressions().len(), 2);
        assert_eq!(source.definitions().len(), 2);
        let region = view.regions().next().expect("one conditional region");
        assert_eq!(region.results().len(), 2);
    });
}

#[test]
fn atomic_conditional_group_refuses_split_partial_permuted_and_corrupt_parent_ownership() {
    for case in [
        ConditionalGroupCase::Split,
        ConditionalGroupCase::Partial,
        ConditionalGroupCase::Permuted,
        ConditionalGroupCase::ParentCountUnderflow,
    ] {
        assert!(matches!(
            construct_conditional_group(case),
            Err(PlanConstructionError::InvalidOperation { .. })
        ));
    }
}

#[test]
fn conditional_rejects_a_sibling_value_as_another_branch_result() {
    let result = CallablePlan::construct::<PlanConstructionError, _>(
        one_conditional_function().0,
        |dae, construction| {
            let function = construction.functions().next().expect("one owner");
            let sources = construction.expressions().collect::<Vec<_>>();
            let condition = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Coordinate(_)
                    )
                })
                .expect("condition source");
            let mut literals = sources.iter().copied().filter(|source| {
                matches!(
                    dae.exact_expression(source.expression()).operation(),
                    rumoca_ir_dae::ExpressionOperation::Literal(DaeLiteral::Real(_))
                )
            });
            let branch_source = literals.next().expect("branch literal");
            let fallback_source = literals.next().expect("fallback literal");
            let conditional_source = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Conditional(_)
                    )
                })
                .expect("conditional source");
            let owner = construction.add_owner(function)?;
            let root = construction.root_scope(owner);
            let condition_value = construction.add_value_operation(
                root,
                condition,
                OperationContractKey::Load,
                &[],
            )?;
            let region = construction.open_conditional(root, conditional_source)?;
            let branch = region.branch_scopes().next().expect("one branch scope");
            let fallback = region.fallback_scope();
            let branch_value = construction.add_value_operation(
                branch,
                branch_source,
                OperationContractKey::Constant,
                &[],
            )?;
            let fallback_value = construction.add_value_operation(
                fallback,
                fallback_source,
                OperationContractKey::Constant,
                &[],
            )?;
            construction
                .close_conditional(region, &[condition_value], &[fallback_value], branch_value)
                .expect_err("sibling values cannot cross branch scopes");
            Ok(())
        },
    );
    assert!(matches!(
        result,
        Err(PlanConstructionError::InvalidOperation { .. })
    ));
}

#[test]
fn integer_to_real_is_owned_once_by_the_exact_consumer_edge() {
    let plan = CallablePlan::construct::<PlanConstructionError, _>(
        one_mixed_add_function(),
        |dae, construction| {
            let function = construction.functions().next().expect("one owner");
            let sources = construction.expressions().collect::<Vec<_>>();
            let integer = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Literal(DaeLiteral::Integer(_))
                    )
                })
                .expect("integer literal");
            let real = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Literal(DaeLiteral::Real(_))
                    )
                })
                .expect("real literal");
            let sum = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Binary { .. }
                    )
                })
                .expect("sum consumer");
            let definition = construction.definitions().next().expect("one store");
            let edge = construction
                .operand_edges(sum)?
                .find(|edge| edge.operand() == integer.expression())
                .expect("integer consumer edge");
            let owner = construction.add_owner(function)?;
            let root = construction.root_scope(owner);
            let integer_value = construction.add_value_operation(
                root,
                integer,
                OperationContractKey::Constant,
                &[],
            )?;
            let promoted = construction.promote_integer_to_real(root, edge, integer_value)?;
            let real_value = construction.add_value_operation(
                root,
                real,
                OperationContractKey::Constant,
                &[],
            )?;
            let sum_value = construction.add_value_operation(
                root,
                sum,
                OperationContractKey::BinaryAddReal,
                &[promoted, real_value],
            )?;
            construction.add_store(root, definition, sum_value)
        },
    )
    .expect("exact conversion edge completes the mixed add");

    plan.inspect(|view| {
        let conversion = view
            .operations()
            .find(|operation| operation.contract() == OperationContractKey::ConvertIntegerToReal)
            .expect("one conversion operation");
        assert!(matches!(
            conversion.detail(),
            crate::CallableOperationDetailView::IntegerToReal {
                source_fact: crate::CallableIntegerSourceFact::ExactLiteral(1)
            }
        ));
    });
}

#[test]
fn mixed_add_refuses_a_missing_conversion_edge() {
    let result = CallablePlan::construct::<PlanConstructionError, _>(
        one_mixed_add_function(),
        |dae, construction| {
            let function = construction.functions().next().expect("one owner");
            let sources = construction.expressions().collect::<Vec<_>>();
            let integer = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Literal(DaeLiteral::Integer(_))
                    )
                })
                .expect("integer literal");
            let real = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Literal(DaeLiteral::Real(_))
                    )
                })
                .expect("real literal");
            let sum = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Binary { .. }
                    )
                })
                .expect("sum consumer");
            let owner = construction.add_owner(function)?;
            let root = construction.root_scope(owner);
            let integer_value = construction.add_value_operation(
                root,
                integer,
                OperationContractKey::Constant,
                &[],
            )?;
            let real_value = construction.add_value_operation(
                root,
                real,
                OperationContractKey::Constant,
                &[],
            )?;
            construction
                .add_value_operation(
                    root,
                    sum,
                    OperationContractKey::BinaryAddReal,
                    &[integer_value, real_value],
                )
                .expect_err("raw Integer cannot occupy the Real consumer edge");
            Ok(())
        },
    );
    assert!(matches!(
        result,
        Err(PlanConstructionError::InvalidOperation { .. })
    ));
}

#[test]
fn duplicate_conversion_edge_sets_an_immutable_poison() {
    let result = CallablePlan::construct::<PlanConstructionError, _>(
        one_mixed_add_function(),
        |dae, construction| {
            let function = construction.functions().next().expect("one owner");
            let sources = construction.expressions().collect::<Vec<_>>();
            let integer = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Literal(DaeLiteral::Integer(_))
                    )
                })
                .expect("integer literal");
            let sum = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Binary { .. }
                    )
                })
                .expect("sum consumer");
            let edge = construction
                .operand_edges(sum)?
                .find(|edge| edge.operand() == integer.expression())
                .expect("integer consumer edge");
            let owner = construction.add_owner(function)?;
            let root = construction.root_scope(owner);
            let integer_value = construction.add_value_operation(
                root,
                integer,
                OperationContractKey::Constant,
                &[],
            )?;
            construction.promote_integer_to_real(root, edge, integer_value)?;
            let first = construction
                .promote_integer_to_real(root, edge, integer_value)
                .expect_err("one consumer edge cannot be converted twice");
            let repeated = construction
                .add_value_operation(root, sum, OperationContractKey::BinaryAddReal, &[])
                .expect_err("post-poison mutation cites the first error");
            assert_eq!(repeated, first);
            Ok(())
        },
    );
    assert!(matches!(
        result,
        Err(PlanConstructionError::InvalidOperation { .. })
    ));
}

#[test]
fn dynamic_index_missing_construction_evidence_refuses_and_poison_is_stable() {
    let result = CallablePlan::construct::<PlanConstructionError, _>(
        one_dynamic_index_function(),
        |dae, construction| {
            let function = construction.functions().next().expect("one owner");
            let sources = construction.expressions().collect::<Vec<_>>();
            let literals = sources
                .iter()
                .copied()
                .filter(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Literal(DaeLiteral::Real(_))
                    )
                })
                .collect::<Vec<_>>();
            let array = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Array(_)
                    )
                })
                .expect("array source");
            let index = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Index { .. }
                    )
                })
                .expect("index source");
            let owner = construction.add_owner(function)?;
            let root = construction.root_scope(owner);
            let one = construction.add_value_operation(
                root,
                literals[0],
                OperationContractKey::Constant,
                &[],
            )?;
            let two = construction.add_value_operation(
                root,
                literals[1],
                OperationContractKey::Constant,
                &[],
            )?;
            let array_value = construction.add_value_operation(
                root,
                array,
                OperationContractKey::ConstructAggregate,
                &[one, two],
            )?;
            let first = construction
                .add_index(root, index, array_value)
                .expect_err("dynamic coordinate has no static construction evidence");
            let repeated = construction
                .add_index(root, index, array_value)
                .expect_err("post-poison call cites the original evidence error");
            assert_eq!(repeated, first);
            Ok(())
        },
    );
    assert!(matches!(
        result,
        Err(PlanConstructionError::InvalidOperation { .. })
    ));
}

#[test]
fn conditional_close_refuses_an_unclosed_nested_child_and_keeps_first_error() {
    let result = CallablePlan::construct::<PlanConstructionError, _>(
        one_conditional_function().0,
        |dae, construction| {
            let function = construction.functions().next().expect("one owner");
            let sources = construction.expressions().collect::<Vec<_>>();
            let condition = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Coordinate(_)
                    )
                })
                .expect("condition source");
            let mut literals = sources.iter().copied().filter(|source| {
                matches!(
                    dae.exact_expression(source.expression()).operation(),
                    rumoca_ir_dae::ExpressionOperation::Literal(DaeLiteral::Real(_))
                )
            });
            let branch_source = literals.next().expect("branch literal");
            let fallback_source = literals.next().expect("fallback literal");
            let conditional_source = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Conditional(_)
                    )
                })
                .expect("conditional source");
            let owner = construction.add_owner(function)?;
            let root = construction.root_scope(owner);
            let condition_value = construction.add_value_operation(
                root,
                condition,
                OperationContractKey::Load,
                &[],
            )?;
            let region = construction.open_conditional(root, conditional_source)?;
            let branch = region.branch_scopes().next().expect("one branch scope");
            let fallback = region.fallback_scope();
            let branch_value = construction.add_value_operation(
                branch,
                branch_source,
                OperationContractKey::Constant,
                &[],
            )?;
            let fallback_value = construction.add_value_operation(
                fallback,
                fallback_source,
                OperationContractKey::Constant,
                &[],
            )?;

            // Planted mutation: model a nested child capability that was
            // opened under the branch and then dropped without closing.
            construction.scopes[branch.raw as usize].open_children = 1;
            let first = construction
                .close_conditional(region, &[condition_value], &[branch_value], fallback_value)
                .expect_err("parent close refuses an unclosed nested child");
            let repeated = construction
                .add_value_operation(root, branch_source, OperationContractKey::Constant, &[])
                .expect_err("later call returns the original child-close poison");
            assert_eq!(repeated, first);
            Ok(())
        },
    );
    assert!(matches!(
        result,
        Err(PlanConstructionError::InvalidOperation { .. })
    ));
}

#[test]
fn map_close_retains_compact_domain_and_closes_its_body_scope() {
    let plan = CallablePlan::construct::<PlanConstructionError, _>(
        one_map_function(),
        |dae, construction| {
            let function = construction.functions().next().expect("one owner");
            let sources = construction.expressions().collect::<Vec<_>>();
            let binder = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Coordinate(
                            rumoca_ir_dae::CoordinateView::Binder(_)
                        )
                    )
                })
                .expect("binder source");
            let map = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Comprehension { .. }
                    )
                })
                .expect("map source");
            let definition = construction.definitions().next().expect("one store");
            let owner = construction.add_owner(function)?;
            let root = construction.root_scope(owner);
            let region = construction.open_map(root, map)?;
            let body_scope = region.body_scope();
            let body = construction.add_value_operation(
                body_scope,
                binder,
                OperationContractKey::Load,
                &[],
            )?;
            let result = construction.close_map(region, body)?;
            construction.add_store(root, definition, result)
        },
    )
    .expect("closed map constructs");

    plan.inspect(|view| {
        let region = view.regions().next().expect("one map region");
        assert_eq!(
            region.domain().expect("compact map domain").scalar_count(),
            2
        );
        let crate::CallableRegionDetailView::Map(detail) = region.detail() else {
            panic!("fixture region remains a map")
        };
        let destination = region.results().next().expect("one map destination");
        assert_ne!(detail.body_result(), destination);
        let operation = view
            .operations()
            .find(|operation| operation.id() == region.operation())
            .expect("region operation");
        assert_eq!(operation.results().next(), Some(destination));
    });
}

#[test]
fn closed_map_body_scope_cannot_be_mutated() {
    let result = CallablePlan::construct::<PlanConstructionError, _>(
        one_map_function(),
        |dae, construction| {
            let function = construction.functions().next().expect("one owner");
            let sources = construction.expressions().collect::<Vec<_>>();
            let binder = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Coordinate(
                            rumoca_ir_dae::CoordinateView::Binder(_)
                        )
                    )
                })
                .expect("binder source");
            let map = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Comprehension { .. }
                    )
                })
                .expect("map source");
            let owner = construction.add_owner(function)?;
            let root = construction.root_scope(owner);
            let region = construction.open_map(root, map)?;
            let body_scope = region.body_scope();
            let body = construction.add_value_operation(
                body_scope,
                binder,
                OperationContractKey::Load,
                &[],
            )?;
            construction.close_map(region, body)?;
            construction
                .add_value_operation(body_scope, binder, OperationContractKey::Load, &[])
                .expect_err("closed map body capability cannot accept mutations");
            Ok(())
        },
    );
    assert!(matches!(
        result,
        Err(PlanConstructionError::InvalidOperation { .. })
    ));
}

#[test]
fn map_close_refuses_an_unclosed_nested_child_and_keeps_first_error() {
    let result = CallablePlan::construct::<PlanConstructionError, _>(
        one_map_function(),
        |dae, construction| {
            let function = construction.functions().next().expect("one owner");
            let sources = construction.expressions().collect::<Vec<_>>();
            let binder = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Coordinate(
                            rumoca_ir_dae::CoordinateView::Binder(_)
                        )
                    )
                })
                .expect("binder source");
            let map = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Comprehension { .. }
                    )
                })
                .expect("map source");
            let owner = construction.add_owner(function)?;
            let root = construction.root_scope(owner);
            let region = construction.open_map(root, map)?;
            let body_scope = region.body_scope();
            let body = construction.add_value_operation(
                body_scope,
                binder,
                OperationContractKey::Load,
                &[],
            )?;

            // Planted mutation: a nested child capability was opened under
            // the map body and then dropped without closing.
            construction.scopes[body_scope.raw as usize].open_children = 1;
            let first = construction
                .close_map(region, body)
                .expect_err("map close refuses an unclosed nested child");
            let repeated = construction
                .add_value_operation(root, binder, OperationContractKey::Load, &[])
                .expect_err("later call returns the original child-close poison");
            assert_eq!(repeated, first);
            Ok(())
        },
    );
    assert!(matches!(
        result,
        Err(PlanConstructionError::InvalidOperation { .. })
    ));
}

#[test]
fn mutually_recursive_call_graph_is_refused_at_the_exact_recursive_call() {
    let (dae, call_spans, declaration_span) = mutually_recursive_functions();
    let result = CallablePlan::construct::<PlanConstructionError, _>(dae, |_dae, construction| {
        let functions = construction.functions().collect::<Vec<_>>();
        let calls = construction.calls().collect::<Vec<_>>();
        let definitions = construction.definitions().collect::<Vec<_>>();
        let owners = functions
            .iter()
            .copied()
            .map(|source| {
                construction
                    .add_owner(source)
                    .map(|owner| (source.function(), owner))
            })
            .collect::<Result<Vec<_>, _>>()?;
        for source in calls.iter().copied() {
            let definition = definitions
                .iter()
                .copied()
                .find(|definition| definition.function() == source.function())
                .expect("each callable expression has its exact function definition");
            let owner = owners
                .iter()
                .copied()
                .find_map(|(function, owner)| (function == source.function()).then_some(owner))
                .expect("each callable expression has its exact owner");
            let root = construction.root_scope(owner);
            let value = construction
                .add_call(root, source, &[])?
                .into_iter()
                .next()
                .expect("one requested call projection");
            construction.add_store(root, definition, value)?;
        }
        Ok(())
    });
    // The cycle closes when `g` calls `f` back, so that call occurrence is the
    // unsupported one. Neither caller declaration is reported.
    assert_eq!(
        result.expect_err("a recursive call graph cannot mint an acyclic receipt"),
        PlanConstructionError::UnsupportedRecursion {
            span: call_spans[1]
        }
    );
    assert_ne!(call_spans[1], declaration_span);
    assert_ne!(call_spans[1], call_spans[0]);
}

#[test]
fn self_call_is_refused_at_the_exact_recursive_call() {
    let mut source_map = SourceMap::new();
    let text = "function f algorithm f(); end f;";
    let source = source_map.add("self-call.mo", text);
    let declaration_span = Span::from_offsets(source, 0, 10);
    let call_offset = text.rfind("f();").expect("the self call");
    let call_span = Span::from_offsets(source, call_offset, call_offset + 3);
    let at =
        rumoca_ir_dae::DaeProvenance::source(declaration_span).expect("test span is source-backed");
    let owner = PendingOwner {
        source_function: 0,
        root_scope: 0,
        interface: CallableInterface {
            parameters: Box::new([]),
            results: Box::new([]),
        },
        declaration: at,
        callees: vec![0],
        operations: Vec::new(),
    };
    let occurrence = OwnerCallOccurrence {
        caller: 0,
        callee: 0,
        span: call_span,
    };
    assert_eq!(
        topological_owner_order(&[owner], &[occurrence])
            .expect_err("a self call cannot mint an acyclic receipt"),
        PlanConstructionError::UnsupportedRecursion { span: call_span }
    );
    assert_ne!(call_span, declaration_span);
}

#[test]
fn an_owner_call_edge_naming_an_absent_owner_is_refused_at_that_call() {
    let mut source_map = SourceMap::new();
    let text = "function f algorithm g(); end f;";
    let source = source_map.add("absent-callee.mo", text);
    let declaration_span = Span::from_offsets(source, 0, 10);
    let call_offset = text.rfind("g();").expect("the call");
    let call_span = Span::from_offsets(source, call_offset, call_offset + 3);
    let at =
        rumoca_ir_dae::DaeProvenance::source(declaration_span).expect("test span is source-backed");
    let owner = PendingOwner {
        source_function: 0,
        root_scope: 0,
        interface: CallableInterface {
            parameters: Box::new([]),
            results: Box::new([]),
        },
        declaration: at,
        callees: vec![1],
        operations: Vec::new(),
    };
    let occurrence = OwnerCallOccurrence {
        caller: 0,
        callee: 1,
        span: call_span,
    };
    assert_eq!(
        topological_owner_order(&[owner], &[occurrence])
            .expect_err("an edge to an absent owner cannot mint a receipt"),
        PlanConstructionError::InvalidOwner { span: call_span }
    );
}

#[test]
fn closed_branch_scope_cannot_be_mutated() {
    let result = CallablePlan::construct::<PlanConstructionError, _>(
        one_conditional_function().0,
        |dae, construction| {
            let function = construction.functions().next().expect("one owner");
            let sources = construction.expressions().collect::<Vec<_>>();
            let condition = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Coordinate(_)
                    )
                })
                .expect("condition source");
            let mut literals = sources.iter().copied().filter(|source| {
                matches!(
                    dae.exact_expression(source.expression()).operation(),
                    rumoca_ir_dae::ExpressionOperation::Literal(DaeLiteral::Real(_))
                )
            });
            let branch_source = literals.next().expect("branch literal");
            let fallback_source = literals.next().expect("fallback literal");
            let conditional_source = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        rumoca_ir_dae::ExpressionOperation::Conditional(_)
                    )
                })
                .expect("conditional source");
            let owner = construction.add_owner(function)?;
            let root = construction.root_scope(owner);
            let condition_value = construction.add_value_operation(
                root,
                condition,
                OperationContractKey::Load,
                &[],
            )?;
            let region = construction.open_conditional(root, conditional_source)?;
            let branch = region.branch_scopes().next().expect("one branch scope");
            let fallback = region.fallback_scope();
            let branch_value = construction.add_value_operation(
                branch,
                branch_source,
                OperationContractKey::Constant,
                &[],
            )?;
            let fallback_value = construction.add_value_operation(
                fallback,
                fallback_source,
                OperationContractKey::Constant,
                &[],
            )?;
            construction.close_conditional(
                region,
                &[condition_value],
                &[branch_value],
                fallback_value,
            )?;
            construction
                .add_value_operation(branch, branch_source, OperationContractKey::Constant, &[])
                .expect_err("closed branch capabilities cannot accept mutations");
            Ok(())
        },
    );
    assert!(matches!(
        result,
        Err(PlanConstructionError::InvalidOperation { .. })
    ));
}

/// The exact expression source of the fixture's Boolean condition coordinate.
fn condition_source<'plan, 'dae>(
    dae: rumoca_ir_dae::DaeView<'dae>,
    sources: &[CallableExpressionSource<'plan, 'dae>],
) -> CallableExpressionSource<'plan, 'dae> {
    sources
        .iter()
        .copied()
        .find(|source| {
            matches!(
                dae.exact_expression(source.expression()).operation(),
                rumoca_ir_dae::ExpressionOperation::Coordinate(_)
            )
        })
        .expect("condition source")
}

/// The exact expression source of one Real literal the fixture writes.
fn real_literal_source<'plan, 'dae>(
    dae: rumoca_ir_dae::DaeView<'dae>,
    sources: &[CallableExpressionSource<'plan, 'dae>],
    wanted: f64,
) -> CallableExpressionSource<'plan, 'dae> {
    sources
        .iter()
        .copied()
        .find(|source| {
            matches!(
                dae.exact_expression(source.expression()).operation(),
                rumoca_ir_dae::ExpressionOperation::Literal(DaeLiteral::Real(value))
                    if *value == wanted
            )
        })
        .expect("exact literal source")
}

/// Every conditional expression source the fixture publishes, in inventory
/// order.
fn conditional_sources<'plan, 'dae>(
    dae: rumoca_ir_dae::DaeView<'dae>,
    sources: &[CallableExpressionSource<'plan, 'dae>],
) -> Vec<CallableExpressionSource<'plan, 'dae>> {
    sources
        .iter()
        .copied()
        .filter(|source| {
            matches!(
                dae.exact_expression(source.expression()).operation(),
                rumoca_ir_dae::ExpressionOperation::Conditional(_)
            )
        })
        .collect()
}

/// The outer and inner conditional of a nested fixture: the outer is the one
/// that has another conditional among its own operands.
fn nested_conditional_sources<'plan, 'dae>(
    dae: rumoca_ir_dae::DaeView<'dae>,
    sources: &[CallableExpressionSource<'plan, 'dae>],
) -> (
    CallableExpressionSource<'plan, 'dae>,
    CallableExpressionSource<'plan, 'dae>,
) {
    let conditionals = conditional_sources(dae, sources);
    let outer = conditionals
        .iter()
        .copied()
        .find(|source| {
            let rumoca_ir_dae::ExpressionOperation::Conditional(operands) =
                dae.exact_expression(source.expression()).operation()
            else {
                return false;
            };
            operands.iter().any(|operand| {
                matches!(
                    dae.exact_expression(operand).operation(),
                    rumoca_ir_dae::ExpressionOperation::Conditional(_)
                )
            })
        })
        .expect("outer conditional source");
    let inner = conditionals
        .iter()
        .copied()
        .find(|source| source.expression() != outer.expression())
        .expect("inner conditional source");
    (outer, inner)
}

/// Each member of a recursive group stores the result of calling the next
/// member, at that call's own span.
fn define_recursive_group<'group, 'dae>(
    dae: &mut rumoca_ir_dae::DaeConstruction<'dae>,
    reservations: Vec<rumoca_ir_dae::FunctionReservation<'group, 'dae>>,
    call_spans: [Span; 2],
    at: rumoca_ir_dae::DaeProvenance,
) -> Result<(), rumoca_ir_dae::DaeConstructionError> {
    let targets = reservations
        .iter()
        .map(rumoca_ir_dae::FunctionReservation::function)
        .collect::<Vec<_>>();
    for (ordinal, reservation) in reservations.into_iter().enumerate() {
        let output =
            dae.functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
        let target = targets[(ordinal + 1) % targets.len()];
        let call_at = rumoca_ir_dae::DaeProvenance::source(call_spans[ordinal])
            .expect("test call span is source-backed");
        let call = dae.expressions(|expressions| expressions.at(call_at).call(target, 0, []))?;
        let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
        dae.functions(|functions| functions.assign(&mut body, output, call, at))?;
        dae.functions(|functions| functions.define(body, at))?;
    }
    Ok(())
}

#[test]
fn an_abandoned_region_capability_refuses_at_its_exact_retained_span() {
    let (dae, region_span) = one_conditional_function();
    // The fixture declares its function at offsets 0..1 of the same source.
    let declaration_span = Span::from_offsets(region_span.source, 0, 1);
    let result = CallablePlan::construct::<PlanConstructionError, _>(dae, |dae, construction| {
        let function = construction.functions().next().expect("one owner");
        let sources = construction.expressions().collect::<Vec<_>>();
        let conditional = conditional_sources(dae, &sources)
            .into_iter()
            .next()
            .expect("one conditional source");
        let owner = construction.add_owner(function)?;
        let root = construction.root_scope(owner);
        // The caller opens the region and never closes it. `finish` must still
        // name a real retained source location, not a sentinel span.
        let _abandoned = construction.open_conditional(root, conditional)?;
        Ok(())
    });

    assert_eq!(
        result.expect_err("an abandoned region capability cannot mint a plan"),
        PlanConstructionError::InvalidOperation { span: region_span }
    );
    assert!(!region_span.is_dummy());
    assert_ne!(region_span, declaration_span);
}
