//! Guard dependencies belong to the same issued relation as its operands.
use super::*;

pub(super) fn arm_relation<'dae>(
    dae: &mut DaeConstruction<'dae>,
    guard: ExprId<'dae>,
    at: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    arm_relation_with(dae, guard, at, |_, _| Ok(()))
}

pub(super) fn arm_relation_with<'dae>(
    dae: &mut DaeConstruction<'dae>,
    guard: ExprId<'dae>,
    at: DaeProvenance,
    publish: impl FnOnce(&mut DaeConstruction<'dae>, ExprId<'dae>) -> Result<(), DaeConstructionError>,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    let owner = dae.begin_model_conditional(guard, at)?;
    let first = dae.model_conditional_arm(&owner, true, |dae| {
        let relation = dae.expressions(|e| {
            let time = e.at(at).coordinate(CoordinateInput::Time)?;
            let zero = e.at(at).literal(DaeLiteral::Real(0.0))?;
            e.at(at).binary(BinaryOperator::Greater, time, zero)
        })?;
        publish(dae, relation)?;
        Ok(relation)
    })?;
    let second = dae.model_conditional_arm(&owner, false, |dae| {
        dae.expressions(|e| e.at(at).literal(DaeLiteral::Boolean(false)))
    })?;
    dae.finish_model_conditional(owner, [first, second])?;
    Ok(first)
}

#[test]
fn scalar_arm_relation_refuses_a_domain_capture_in_its_guard() {
    let source = TestSource::new("if i>0 then time>0 else false");
    let at = source.source("if i>0 then time>0 else false", 0);
    Dae::construct(source.map, |dae| {
        let domain = dae.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![StructuredIndexBinder {
                        provenance: rumoca_core::IndexBinderProvenance::Generated,
                        id: 0,
                        display_name: "i".into(),
                        lower: 1,
                        upper: 2,
                        step: 1,
                    }],
                },
                at,
            )
        })?;
        let guard = dae.expressions(|e| {
            let index = e
                .at(at)
                .binder(DomainBinderId::from_raw(domain.index(), 0))?;
            let zero = e.at(at).literal(DaeLiteral::Integer(0))?;
            e.at(at).binary(BinaryOperator::Greater, index, zero)
        })?;
        let expression = arm_relation(dae, guard, at)?;
        assert!(matches!(
            dae.conditions(|c| c.relation(expression, at)),
            Err(DaeConstructionError::InvalidBinderScope {
                expected_domain: None,
                found_domain,
                span,
            }) if found_domain == domain.index() && span == at.span()
        ));
        Ok(())
    })
    .expect("a closed conditional does not erase its guard's domain identity");
}

fn discrete_guard_owners<'dae>(
    topology: &mut DiscreteValueTopology<'_, 'dae>,
    order: [DiscreteValueId<'dae>; 2],
    a: DiscreteValueId<'dae>,
    condition: ConditionId<'dae>,
    value: ExprId<'dae>,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    for target in order {
        topology.owner(at, [target], |owner| {
            if target == a {
                owner.always(at, [(value, at)])
            } else {
                owner.when(condition, condition, at, [(value, at)])
            }
        })?;
    }
    Ok(())
}

fn discrete_guard_fixture(producer_first: bool) -> Result<Dae, DaeConstructionError> {
    let source = TestSource::new(
        "discrete Boolean a; discrete Boolean b; if a then time>0 else false; a=true; b=true;",
    );
    let at = source.source("if a then time>0 else false", 0);
    Dae::construct(source.map, |dae| {
        let ty = dae.types(|t| t.derived(ValueType::scalar(ScalarType::Boolean), at))?;
        let [a, b] = dae.variables(|v| {
            Ok([
                v.discrete_value(VarName::new("a"), ty, at, VariableAttributes::default())?,
                v.discrete_value(VarName::new("b"), ty, at, VariableAttributes::default())?,
            ])
        })?;
        let guard = dae.expressions(|e| e.at(at).coordinate(CoordinateInput::DiscreteValue(a)))?;
        let expression = arm_relation(dae, guard, at)?;
        let condition = dae.conditions(|c| {
            let relation = c.relation(expression, at)?;
            let condition = c.reserve(at)?;
            c.define(condition, ConditionInput::Relation(relation), at)?;
            c.root(relation, condition, at)?;
            Ok(condition)
        })?;
        let value = dae.expressions(|e| e.at(at).literal(DaeLiteral::Boolean(true)))?;
        let order = if producer_first { [a, b] } else { [b, a] };
        dae.b1c(order, |topology| {
            discrete_guard_owners(topology, order, a, condition, value, at)
        })?;
        Ok(())
    })
}

#[test]
fn arm_relation_guard_requires_the_discrete_producer_prefix() {
    assert!(matches!(
        discrete_guard_fixture(false),
        Err(DaeConstructionError::UnissuedDiscreteDependency {
            target: 1,
            dependency: 0,
            ..
        })
    ));
    let model = discrete_guard_fixture(true)
        .expect("the exact same guard is valid after its current-value producer");
    let bytes = bincode::serialize(&model).unwrap();
    let decoded: Dae = bincode::deserialize(&bytes).unwrap();
    assert_eq!(bincode::serialize(&decoded).unwrap(), bytes);
}
