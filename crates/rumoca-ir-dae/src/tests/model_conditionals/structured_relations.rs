//! SPEC0032 §1: ordinary arm relations retain one issued domain and guard.
use super::guard_dependencies::arm_relation;
use super::*;

fn domain<'dae>(
    dae: &mut DaeConstruction<'dae>,
    at: DaeProvenance,
    extent: i64,
) -> Result<DomainId<'dae>, DaeConstructionError> {
    dae.domains(|domains| {
        domains.structured(
            StructuredIndexDomain {
                binders: vec![StructuredIndexBinder {
                    provenance: rumoca_core::IndexBinderProvenance::Generated,
                    id: 0,
                    display_name: "i".into(),
                    lower: 1,
                    upper: extent,
                    step: 1,
                }],
            },
            at,
        )
    })
}

fn guarded_relation<'dae>(
    dae: &mut DaeConstruction<'dae>,
    domain: DomainId<'dae>,
    at: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    let guard = dae.expressions(|e| {
        let index = e
            .at(at)
            .binder(DomainBinderId::from_raw(domain.index(), 0))?;
        let zero = e.at(at).literal(DaeLiteral::Integer(0))?;
        e.at(at).binary(BinaryOperator::Greater, index, zero)
    })?;
    arm_relation(dae, guard, at)
}

fn fixture(extent: i64) -> Dae {
    let source = TestSource::new("for i in 1:2 loop if i>0 then time>0 else false; end for;");
    let at = source.source("if i>0 then time>0 else false", 0);
    Dae::construct(source.map, |dae| {
        let owned = domain(dae, at, extent)?;
        domain(dae, at, extent)?;
        let expression = guarded_relation(dae, owned, at)?;
        dae.conditions(|c| c.structured_root(owned, expression, at))?;
        Ok(())
    })
    .expect("one ordinary structured relation retains its closed arm guard")
}

fn assert_owner(model: &Dae) -> usize {
    model.inspect(|view| {
        assert_eq!(view.structured_root_count(), 1);
        assert_eq!(view.root_count(), 0);
        assert_eq!(view.runtime_quotient_owner_count(), 0);
        let (_, root) = view.structured_roots().next().unwrap();
        assert_eq!(root.domain().index(), 0);
        let issued_guard = view.model_activation_guard(root.expression());
        assert!(issued_guard.is_some());
        assert_eq!(root.activation_guard(), issued_guard);
        assert_eq!(
            view.expression(root.expression()).unwrap().binder_domain(),
            Some(root.domain())
        );
        view.expression_count()
    })
}

#[test]
fn ordinary_structured_arm_relation_replays_one_compact_guarded_owner() {
    let mut counts = Vec::new();
    for extent in [2, 1_000_000] {
        let model = fixture(extent);
        counts.push(assert_owner(&model));
        let json = serde_json::to_vec(&model).unwrap();
        let replayed: Dae = serde_json::from_slice(&json).unwrap();
        assert_owner(&replayed);
        assert_eq!(serde_json::to_vec(&replayed).unwrap(), json);
        let bytes = bincode::serialize(&model).unwrap();
        let mut input = std::io::Cursor::new(bytes.as_slice());
        let replayed: Dae = bincode::deserialize_from(&mut input).unwrap();
        assert_eq!(input.position(), bytes.len() as u64);
        assert_owner(&replayed);
        assert_eq!(bincode::serialize(&replayed).unwrap(), bytes);
    }
    assert_eq!(
        counts[0], counts[1],
        "extent cannot create per-point owners"
    );
}

#[test]
fn ordinary_structured_arm_relation_refuses_foreign_domain_arm_and_function() {
    let source = TestSource::new("for i in 1:2 loop if i>0 then time>0 else false; end for;");
    let at = source.source("if i>0 then time>0 else false", 0);
    Dae::construct(source.map, |dae| {
        let owned = domain(dae, at, 2)?;
        let foreign = domain(dae, at, 2)?;
        let expression = guarded_relation(dae, owned, at)?;
        assert!(matches!(dae.conditions(|c| c.structured_root(foreign, expression, at)),
            Err(DaeConstructionError::InvalidBinderScope { expected_domain: Some(expected), found_domain, span })
                if expected == foreign.index() && found_domain == owned.index() && span == at.span()));
        let guard = dae.expressions(|e| e.at(at).literal(DaeLiteral::Boolean(true)))?;
        let owner = dae.begin_model_conditional(guard, at)?;
        let first = dae.model_conditional_arm(&owner, true, |dae| {
            assert!(matches!(dae.conditions(|c| c.structured_root(owned, expression, at)),
                Err(DaeConstructionError::InvalidModelActivationScope { span }) if span == at.span()));
            dae.expressions(|e| e.at(at).literal(DaeLiteral::Real(1.0)))
        })?;
        let second = dae.model_conditional_arm(&owner, false, |dae| {
            dae.expressions(|e| e.at(at).literal(DaeLiteral::Real(2.0)))
        })?;
        dae.finish_model_conditional(owner, [first, second])?;
        dae.function(FunctionSignature::new(VarName::new("f"), [], [], at), |dae, reservation| {
            assert!(matches!(dae.conditions(|c| c.structured_root(owned, expression, at)),
                Err(DaeConstructionError::InvalidModelActivationScope { span }) if span == at.span()));
            let body = dae.functions(|functions| functions.begin(reservation, at))?;
            dae.functions(|functions| functions.define(body, at))
        })?;
        dae.conditions(|c| c.structured_root(owned, expression, at))?;
        Ok(())
    }).expect("failed foreign authority checks do not prevent the owned root");
}

#[test]
fn ordinary_structured_arm_relation_wire_refuses_foreign_domain() {
    let mut wire = serde_json::to_value(fixture(2)).unwrap();
    wire["storage"]["structured_roots"][0]["root"]["domain"] = 1.into();
    let error = serde_json::from_value::<Dae>(wire)
        .expect_err("identical domain geometry is not the issued binder identity");
    assert!(error.to_string().contains("binder"), "{error}");
}
