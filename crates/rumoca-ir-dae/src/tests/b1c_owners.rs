use super::*;

#[test]
fn every_local_b1c_target_requires_exactly_one_definition() {
    let source = TestSource::new("discrete Boolean m;");
    let declaration = source.source("discrete Boolean m", 0);
    let error = Dae::construct(source.map, |dae| {
        let boolean = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Boolean),
                declaration,
            )
        })?;
        dae.variables(|variables| {
            variables.discrete_value(
                VarName::new("m"),
                boolean,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        Ok(())
    })
    .unwrap_err();
    assert!(matches!(
        error,
        DaeConstructionError::IncompleteDefinition {
            kind: "B.1c topology",
            ..
        }
    ));
}

#[derive(Clone, Copy)]
enum PairTarget {
    A,
    B,
}

#[derive(Clone, Copy)]
enum PairValue {
    CurrentA,
    CurrentB,
    PreA,
    PreB,
}

#[derive(Clone, Copy)]
struct PairBranch {
    a: PairValue,
    b: PairValue,
}

struct PairOwnerResult {
    result: Result<Dae, DaeConstructionError>,
    first_a: Span,
    first_b: Span,
    second_b: Span,
}

fn define_pair_variables<'dae>(
    dae: &mut DaeConstruction<'dae>,
    declarations: [DaeProvenance; 2],
) -> Result<[DiscreteValueId<'dae>; 2], DaeConstructionError> {
    let boolean = dae.types(|types| {
        types.intern(
            TypeId::new(0),
            ValueType::scalar(ScalarType::Boolean),
            declarations[0],
        )
    })?;
    dae.variables(|variables| {
        Ok([
            variables.discrete_value(
                VarName::new("a"),
                boolean,
                declarations[0],
                VariableAttributes::default(),
            )?,
            variables.discrete_value(
                VarName::new("b"),
                boolean,
                declarations[1],
                VariableAttributes::default(),
            )?,
        ])
    })
}

fn define_pair_conditions<'dae>(
    dae: &mut DaeConstruction<'dae>,
    branch_count: usize,
    provenance: [DaeProvenance; 2],
) -> Result<Option<[ConditionId<'dae>; 2]>, DaeConstructionError> {
    if branch_count == 1 {
        return Ok(None);
    }
    let values = dae.expressions(|expressions| {
        Ok([
            expressions
                .at(provenance[0])
                .literal(DaeLiteral::Boolean(true))?,
            expressions
                .at(provenance[1])
                .literal(DaeLiteral::Boolean(false))?,
        ])
    })?;
    dae.conditions(|conditions| {
        let first = conditions.reserve(provenance[0])?;
        conditions.define(first, ConditionInput::Discrete(values[0]), provenance[0])?;
        let second = conditions.reserve(provenance[1])?;
        conditions.define(second, ConditionInput::Discrete(values[1]), provenance[1])?;
        Ok(Some([first, second]))
    })
}

fn pair_value_expression<'dae>(
    dae: &mut DaeConstruction<'dae>,
    value: PairValue,
    variables: [DiscreteValueId<'dae>; 2],
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    dae.expressions(|expressions| match value {
        PairValue::CurrentA => expressions
            .at(provenance)
            .coordinate(CoordinateInput::DiscreteValue(variables[0])),
        PairValue::CurrentB => expressions
            .at(provenance)
            .coordinate(CoordinateInput::DiscreteValue(variables[1])),
        PairValue::PreA => expressions
            .at(provenance)
            .coordinate(CoordinateInput::PreDiscreteValue(variables[0])),
        PairValue::PreB => expressions
            .at(provenance)
            .coordinate(CoordinateInput::PreDiscreteValue(variables[1])),
    })
}

fn lower_pair_branches<'dae>(
    dae: &mut DaeConstruction<'dae>,
    order: [PairTarget; 2],
    branches: &[PairBranch],
    variables: [DiscreteValueId<'dae>; 2],
    action_provenance: [[DaeProvenance; 2]; 2],
) -> Result<Vec<Vec<(ExprId<'dae>, DaeProvenance)>>, DaeConstructionError> {
    let mut lowered = Vec::with_capacity(branches.len());
    for (branch_index, branch) in branches.iter().copied().enumerate() {
        let mut values = Vec::with_capacity(order.len());
        for target in order {
            let (value, provenance) = match target {
                PairTarget::A => (branch.a, action_provenance[branch_index][0]),
                PairTarget::B => (branch.b, action_provenance[branch_index][1]),
            };
            values.push((
                pair_value_expression(dae, value, variables, provenance)?,
                provenance,
            ));
        }
        lowered.push(values);
    }
    Ok(lowered)
}

fn define_pair_owner<'dae>(
    topology: &mut DiscreteValueTopology<'_, 'dae>,
    targets: [DiscreteValueId<'dae>; 2],
    conditions: Option<[ConditionId<'dae>; 2]>,
    condition_provenance: [DaeProvenance; 2],
    owner_provenance: DaeProvenance,
    branches: Vec<Vec<(ExprId<'dae>, DaeProvenance)>>,
) -> Result<(), DaeConstructionError> {
    topology.owner(owner_provenance, targets, |owner| {
        match conditions {
            Some(conditions) => {
                for (branch_index, values) in branches.into_iter().enumerate() {
                    owner.when(
                        conditions[branch_index],
                        conditions[branch_index],
                        condition_provenance[branch_index],
                        values,
                    )?;
                }
            }
            None => {
                for values in branches {
                    owner.always(owner_provenance, values)?;
                }
            }
        }
        Ok(())
    })?;
    Ok(())
}

fn construct_pair_owner(order: [PairTarget; 2], branches: &[PairBranch]) -> PairOwnerResult {
    assert!((1..=2).contains(&branches.len()));
    let source = TestSource::new(
        "discrete Boolean a; discrete Boolean b; \
         when first then a = firstA; b = firstB; \
         elsewhen second then a = secondA; b = secondB; end when;",
    );
    let a_declaration = source.source("discrete Boolean a", 0);
    let b_declaration = source.source("discrete Boolean b", 0);
    let owner_at = source.source(
        "when first then a = firstA; b = firstB; \
         elsewhen second then a = secondA; b = secondB; end when",
        0,
    );
    let first_condition = source.source("first", 1);
    let second_condition = source.source("second", 1);
    let first_a = source.source("a = firstA", 0);
    let first_b = source.source("b = firstB", 0);
    let second_a = source.source("a = secondA", 0);
    let second_b = source.source("b = secondB", 0);
    let action_provenance = [[first_a, first_b], [second_a, second_b]];
    let branches = branches.to_vec();
    let result = Dae::construct(source.map, |dae| {
        let variables = define_pair_variables(dae, [a_declaration, b_declaration])?;
        let conditions =
            define_pair_conditions(dae, branches.len(), [first_condition, second_condition])?;
        let ordered_targets = order.map(|target| match target {
            PairTarget::A => variables[0],
            PairTarget::B => variables[1],
        });
        let lowered_branches =
            lower_pair_branches(dae, order, &branches, variables, action_provenance)?;
        dae.b1c(ordered_targets, |topology| {
            define_pair_owner(
                topology,
                ordered_targets,
                conditions,
                [first_condition, second_condition],
                owner_at,
                lowered_branches,
            )
        })
    });
    PairOwnerResult {
        result,
        first_a: first_a.span(),
        first_b: first_b.span(),
        second_b: second_b.span(),
    }
}

#[test]
fn b1c_atomic_owner_accepts_current_dependency_on_issued_target_prefix() {
    let outcome = construct_pair_owner(
        [PairTarget::A, PairTarget::B],
        &[PairBranch {
            a: PairValue::PreA,
            b: PairValue::CurrentA,
        }],
    );
    let dae = outcome
        .result
        .expect("the second target may read the first target in one atomic owner");
    dae.inspect(|view| {
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        assert_eq!(
            owner
                .targets()
                .iter()
                .map(DiscreteValueId::index)
                .collect::<Vec<_>>(),
            [0, 1]
        );
    });
}

#[test]
fn b1c_atomic_owner_rejects_reverse_order_until_the_producer_corrects_the_plan() {
    let reversed = construct_pair_owner(
        [PairTarget::B, PairTarget::A],
        &[PairBranch {
            a: PairValue::PreA,
            b: PairValue::CurrentA,
        }],
    );
    assert!(matches!(
        reversed.result,
        Err(DaeConstructionError::UnissuedDiscreteDependency {
            target: 1,
            dependency: 0,
            span,
        }) if span == reversed.first_b
    ));

    let corrected = construct_pair_owner(
        [PairTarget::A, PairTarget::B],
        &[PairBranch {
            a: PairValue::PreA,
            b: PairValue::CurrentA,
        }],
    );
    corrected
        .result
        .expect("the producer-supplied topological target order is accepted");
}

#[test]
fn b1c_atomic_owner_rejects_a_direct_current_value_self_cycle() {
    let outcome = construct_pair_owner(
        [PairTarget::A, PairTarget::B],
        &[PairBranch {
            a: PairValue::CurrentA,
            b: PairValue::PreB,
        }],
    );
    assert!(matches!(
        outcome.result,
        Err(DaeConstructionError::UnissuedDiscreteDependency {
            target: 0,
            dependency: 0,
            span,
        }) if span == outcome.first_a
    ));
}

#[test]
fn b1c_atomic_owner_rejects_a_two_target_current_value_cycle() {
    let outcome = construct_pair_owner(
        [PairTarget::A, PairTarget::B],
        &[PairBranch {
            a: PairValue::CurrentB,
            b: PairValue::CurrentA,
        }],
    );
    assert!(matches!(
        outcome.result,
        Err(DaeConstructionError::UnissuedDiscreteDependency {
            target: 0,
            dependency: 1,
            span,
        }) if span == outcome.first_a
    ));
}

#[test]
fn b1c_atomic_owner_checks_dependencies_present_in_only_one_conditional_branch() {
    let outcome = construct_pair_owner(
        [PairTarget::B, PairTarget::A],
        &[
            PairBranch {
                a: PairValue::PreA,
                b: PairValue::PreB,
            },
            PairBranch {
                a: PairValue::PreA,
                b: PairValue::CurrentA,
            },
        ],
    );
    assert!(matches!(
        outcome.result,
        Err(DaeConstructionError::UnissuedDiscreteDependency {
            target: 1,
            dependency: 0,
            span,
        }) if span == outcome.second_b
    ));
}

#[test]
fn b1c_current_value_dependencies_must_be_acyclic() {
    let source = TestSource::new("discrete Boolean a; discrete Boolean b; equation a = b; b = a;");
    let a_declaration = source.source("discrete Boolean a", 0);
    let b_declaration = source.source("discrete Boolean b", 0);
    let a_assignment = source.source("a = b", 0);
    let b_assignment = source.source("b = a", 0);
    let error = Dae::construct(source.map, |dae| {
        let boolean = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Boolean),
                a_declaration,
            )
        })?;
        let (a, b) = dae.variables(|variables| {
            Ok((
                variables.discrete_value(
                    VarName::new("a"),
                    boolean,
                    a_declaration,
                    VariableAttributes::default(),
                )?,
                variables.discrete_value(
                    VarName::new("b"),
                    boolean,
                    b_declaration,
                    VariableAttributes::default(),
                )?,
            ))
        })?;
        let a_value = dae.expressions(|expressions| {
            expressions
                .at(a_assignment)
                .coordinate(CoordinateInput::DiscreteValue(a))
        })?;
        let b_value = dae.expressions(|expressions| {
            expressions
                .at(b_assignment)
                .coordinate(CoordinateInput::DiscreteValue(b))
        })?;
        dae.b1c([a, b], |topology| {
            topology.owner(a_assignment, [a], |owner| {
                owner.always(a_assignment, [(b_value, a_assignment)])
            })?;
            topology.owner(b_assignment, [b], |owner| {
                owner.always(b_assignment, [(a_value, b_assignment)])
            })?;
            Ok(())
        })
    })
    .unwrap_err();

    assert!(matches!(
        error,
        DaeConstructionError::UnissuedDiscreteDependency {
            target,
            dependency,
            span,
        } if target == 0 && dependency == 1 && span == a_assignment.span()
    ));
}

#[test]
fn b1c_when_owner_preserves_source_priority_and_action_provenance() {
    let source = TestSource::new(
        "discrete Boolean m; when a then m = true; elsewhen b then m = false; end when;",
    );
    let declaration = source.source("discrete Boolean m", 0);
    let owner_at = source.source(
        "when a then m = true; elsewhen b then m = false; end when",
        0,
    );
    let a_at = source.source("a", 1);
    let b_at = source.source("b", 0);
    let true_at = source.source("m = true", 0);
    let false_at = source.source("m = false", 0);
    let dae = Dae::construct(source.map, |dae| {
        let boolean = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Boolean),
                declaration,
            )
        })?;
        let m = dae.variables(|variables| {
            variables.discrete_value(
                VarName::new("m"),
                boolean,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let (a_value, b_value, true_value, false_value) = dae.expressions(|expressions| {
            Ok((
                expressions.at(a_at).literal(DaeLiteral::Boolean(true))?,
                expressions.at(b_at).literal(DaeLiteral::Boolean(false))?,
                expressions.at(true_at).literal(DaeLiteral::Boolean(true))?,
                expressions
                    .at(false_at)
                    .literal(DaeLiteral::Boolean(false))?,
            ))
        })?;
        let (a, b) = dae.conditions(|conditions| {
            let a = conditions.reserve(a_at)?;
            conditions.define(a, ConditionInput::Discrete(a_value), a_at)?;
            let b = conditions.reserve(b_at)?;
            conditions.define(b, ConditionInput::Discrete(b_value), b_at)?;
            Ok((a, b))
        })?;
        dae.b1c([m], |topology| {
            topology.owner(owner_at, [m], |owner| {
                owner.when(a, a, a_at, [(true_value, true_at)])?;
                owner.when(b, b, b_at, [(false_value, false_at)])
            })?;
            Ok(())
        })
    })
    .unwrap();

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        let branches = owner.branches();
        assert_eq!(owner.provenance(), owner_at);
        assert_eq!(branches.get(0).unwrap().provenance(), a_at);
        assert_eq!(branches.get(0).unwrap().values().get(0).unwrap().1, true_at);
        assert_eq!(branches.get(1).unwrap().provenance(), b_at);
        assert_eq!(
            branches.get(1).unwrap().values().get(0).unwrap().1,
            false_at
        );
    });
}

fn construct_structured_b1c_dae() -> (Dae, DaeProvenance) {
    let source = TestSource::new("discrete Boolean m[2]; equation m = {true, false};");
    let declaration = source.source("discrete Boolean m[2]", 0);
    let assignment = source.source("m = {true, false}", 0);
    let dae = Dae::construct(source.map, |dae| {
        let boolean_array = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::array(ScalarType::Boolean, [2]),
                declaration,
            )
        })?;
        let target = dae.variables(|variables| {
            variables.discrete_value(
                VarName::new("m"),
                boolean_array,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let domain = dae.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![StructuredIndexBinder {
                        id: 0,
                        display_name: "i".to_string(),
                        lower: 1,
                        upper: 2,
                        step: 1,
                    }],
                },
                assignment,
            )
        })?;
        let value = dae.expressions(|expressions| {
            let yes = expressions
                .at(assignment)
                .literal(DaeLiteral::Boolean(true))?;
            let no = expressions
                .at(assignment)
                .literal(DaeLiteral::Boolean(false))?;
            let yes = expressions.at(assignment).array([yes])?;
            let no = expressions.at(assignment).array([no])?;
            expressions.at(assignment).array([yes, no])
        })?;
        dae.b1c([target], |topology| {
            let rejected = topology.structured_owner(
                assignment,
                domain,
                rumoca_core::ComprehensionScalarView::BinderSubstitution,
                [target],
                |owner| owner.always(assignment, [(value, assignment)]),
            );
            assert!(matches!(
                rejected,
                Err(DaeConstructionError::ShapeMismatch { .. })
            ));
            topology.structured_owner(
                assignment,
                domain,
                rumoca_core::ComprehensionScalarView::RowMajorProjection,
                [target],
                |owner| owner.always(assignment, [(value, assignment)]),
            )?;
            Ok(())
        })
    })
    .expect("the checked structured B.1c owner constructs after an atomic rejection");
    (dae, assignment)
}

#[test]
fn structured_b1c_owner_derives_domain_view_rows_and_rejects_forged_wire() {
    let (dae, assignment) = construct_structured_b1c_dae();
    let assert_owner = |dae: &Dae| {
        dae.inspect(|view| {
            let owner = view
                .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
                .unwrap();
            let structure = owner.structure().expect("the B.1c family stays structured");
            assert_eq!(structure.domain().index(), 0);
            assert_eq!(
                structure.scalar_view(),
                rumoca_core::ComprehensionScalarView::RowMajorProjection
            );
            assert_eq!(structure.scalar_rows(), 2);
            assert_eq!(owner.targets().len(), 1);
            assert_eq!(owner.branches().get(0).unwrap().provenance(), assignment);
        });
    };
    assert_owner(&dae);

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    assert_owner(&decoded);

    let forged_domain = encoded.replacen("\"upper\":2", "\"upper\":3", 1);
    assert_ne!(forged_domain, encoded);
    assert!(serde_json::from_str::<Dae>(&forged_domain).is_err());

    let forged_view = encoded.replacen(
        "\"scalar_view\":\"RowMajorProjection\"",
        "\"scalar_view\":\"BinderSubstitution\"",
        1,
    );
    assert_ne!(forged_view, encoded);
    assert!(serde_json::from_str::<Dae>(&forged_view).is_err());
}

#[test]
fn b1c_topology_error_rolls_back_earlier_owners_before_retry() {
    let source = TestSource::new(
        "discrete Boolean a; discrete Boolean b; pre(a); pre(b); first owner; second owner;",
    );
    let a_declaration = source.source("discrete Boolean a", 0);
    let b_declaration = source.source("discrete Boolean b", 0);
    let a_value_at = source.source("pre(a)", 0);
    let b_value_at = source.source("pre(b)", 0);
    let first_owner = source.source("first owner", 0);
    let second_owner = source.source("second owner", 0);
    let dae = Dae::construct(source.map, |dae| {
        let [a, b] = define_pair_variables(dae, [a_declaration, b_declaration])?;
        let [a_value, b_value] = [
            pair_value_expression(dae, PairValue::PreA, [a, b], a_value_at)?,
            pair_value_expression(dae, PairValue::PreB, [a, b], b_value_at)?,
        ];

        let rejected = dae.b1c([a, b], |topology| {
            topology.owner(first_owner, [a], |owner| {
                owner.always(first_owner, [(a_value, a_value_at)])
            })?;
            topology.owner(second_owner, [a], |_| Ok(()))?;
            Ok(())
        });
        assert!(matches!(
            rejected,
            Err(DaeConstructionError::InvalidDiscreteTargetOrder {
                expected: Some(1),
                found: Some(0),
                ..
            })
        ));

        dae.b1c([a, b], |topology| {
            topology.owner(first_owner, [a], |owner| {
                owner.always(first_owner, [(a_value, a_value_at)])
            })?;
            topology.owner(second_owner, [b], |owner| {
                owner.always(second_owner, [(b_value, b_value_at)])
            })?;
            Ok(())
        })
    })
    .unwrap();

    dae.inspect(|view| {
        assert_eq!(view.discrete_value_owner_count(), 2);
        let first = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        let second = view
            .discrete_value_owner(view.discrete_value_owner_id(1).unwrap())
            .unwrap();
        assert_eq!(first.targets().len(), 1);
        assert_eq!(second.targets().len(), 1);
        assert_eq!(first.provenance(), first_owner);
        assert_eq!(second.provenance(), second_owner);
        assert_eq!(
            first.branches().get(0).unwrap().values().get(0).unwrap().1,
            a_value_at
        );
        assert_eq!(
            second.branches().get(0).unwrap().values().get(0).unwrap().1,
            b_value_at
        );
    });
}

#[test]
fn b1c_owner_error_rolls_back_direct_aggregate_insertion_before_retry() {
    let source = TestSource::new("discrete Boolean a; pre(a); owner;");
    let declaration = source.source("discrete Boolean a", 0);
    let value_at = source.source("pre(a)", 0);
    let owner_at = source.source("owner", 0);
    let dae = Dae::construct(source.map, |dae| {
        let boolean = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Boolean),
                declaration,
            )
        })?;
        let a = dae.variables(|variables| {
            variables.discrete_value(
                VarName::new("a"),
                boolean,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let value = dae.expressions(|expressions| {
            expressions
                .at(value_at)
                .coordinate(CoordinateInput::PreDiscreteValue(a))
        })?;
        dae.b1c([a], |topology| {
            let rejected = topology.owner(owner_at, [a], |owner| {
                owner.always(owner_at, [(value, value_at)])?;
                owner.always(owner_at, [(value, value_at)])
            });
            assert!(matches!(
                rejected,
                Err(DaeConstructionError::InvalidDiscreteBranchSet { .. })
            ));
            topology.owner(owner_at, [a], |owner| {
                owner.always(owner_at, [(value, value_at)])
            })?;
            Ok(())
        })
    })
    .unwrap();

    dae.inspect(|view| {
        assert_eq!(view.discrete_value_owner_count(), 1);
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        assert_eq!(owner.branches().len(), 1);
    });
}

#[test]
fn empty_b1c_topology_seals_an_empty_discrete_role_view_and_round_trips() {
    let source = TestSource::new("model Empty end Empty;");
    let dae = Dae::construct(source.map, |dae| dae.b1c([], |_| Ok(()))).unwrap();
    let assert_empty = |dae: &Dae| {
        dae.inspect(|view| {
            assert_eq!(view.discrete_value_owner_count(), 0);
            assert_eq!(view.discrete_value_definition_count(), 0);
            assert_eq!(
                view.variables()
                    .filter(|(_, variable)| variable.role() == VariableRole::DiscreteValue)
                    .count(),
                0
            );
        });
    };
    assert_empty(&dae);

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    assert_empty(&decoded);
}

#[test]
fn empty_b1c_topology_capability_cannot_be_consumed_twice() {
    let source = TestSource::new("model Empty end Empty;");
    let error = Dae::construct(source.map, |dae| {
        dae.b1c([], |_| Ok(()))?;
        dae.b1c([], |_| {
            panic!("a consumed empty B.1c topology must reject before rebuilding")
        })
    })
    .unwrap_err();
    assert_eq!(
        error,
        DaeConstructionError::DuplicateTopology {
            kind: "B.1c topology",
            span: None,
        }
    );
    assert_eq!(error.source_span(), None);
}

#[test]
fn empty_b1c_topology_cannot_be_reopened_by_a_late_discrete_target() {
    let source = TestSource::new("model Empty discrete Boolean late; end Empty;");
    let declaration = source.source("discrete Boolean late", 0);
    let dae = Dae::construct(source.map, |dae| {
        dae.b1c([], |_| Ok(()))?;
        let boolean = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Boolean),
                declaration,
            )
        })?;
        let rejected = dae.variables(|variables| {
            variables.discrete_value(
                VarName::new("late"),
                boolean,
                declaration,
                VariableAttributes::default(),
            )
        });
        assert_eq!(
            rejected,
            Err(DaeConstructionError::DuplicateTopology {
                kind: "B.1c topology",
                span: Some(declaration.span()),
            })
        );
        Ok(())
    })
    .unwrap();
    dae.inspect(|view| assert_eq!(view.variable_count(), 0));
}
