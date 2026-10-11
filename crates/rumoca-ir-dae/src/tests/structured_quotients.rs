//! MLS §3.7.2/§8.5, SPEC_0032 §§1–3: quotient boundaries retain their domain.
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

fn operands<'dae>(
    dae: &mut DaeConstruction<'dae>,
    domain: DomainId<'dae>,
    scalar: ScalarType,
    at: DaeProvenance,
) -> Result<[ExprId<'dae>; 2], DaeConstructionError> {
    let ty = dae.types(|types| types.derived(ValueType::scalar(scalar), at))?;
    let variability = if scalar == ScalarType::Real {
        InputVariability::Continuous
    } else {
        InputVariability::Discrete
    };
    let input = dae.variables(|variables| {
        variables.input(
            VarName::new("n"),
            ty,
            variability,
            at,
            VariableAttributes::default(),
        )
    })?;
    dae.expressions(|expressions| {
        let input = expressions
            .at(at)
            .coordinate(CoordinateInput::Input(input))?;
        let index = expressions
            .at(at)
            .binder(DomainBinderId::from_raw(domain.index(), 0))?;
        let lhs = expressions
            .at(at)
            .binary(BinaryOperator::Add, input, index)?;
        let rhs = expressions.at(at).literal(DaeLiteral::Integer(7))?;
        Ok([lhs, rhs])
    })
}

fn fixture(builtin: PureBuiltin, scalar: ScalarType, extent: i64) -> Dae {
    let source = TestSource::new("input Real n; for i in 1:2 loop mod(n+i,7); end for;");
    let at = source.source("mod(n+i,7)", 0);
    Dae::construct(source.map, |dae| {
        let owned = domain(dae, at, extent)?;
        domain(dae, at, extent)?; // Same geometry, independently issued foreign identity.
        let operands = operands(dae, owned, scalar, at)?;
        dae.runtime_quotient(builtin, operands, at)?;
        Ok(())
    })
    .expect("the boundary owns its exact compact domain")
}

fn assert_structured_boundary(dae: &Dae, builtin: PureBuiltin) -> usize {
    dae.inspect(|view| {
        assert_eq!(view.root_count(), 0);
        assert_eq!(view.relation_count(), 0);
        assert_eq!(view.condition_count(), 0);
        assert_eq!(view.structured_root_count(), 1);
        assert_eq!(view.runtime_quotient_owner_count(), 1);
        let owner = view.runtime_quotient_owner_at(0).unwrap();
        let RuntimeQuotientOwnerKind::StructuredModelEvent {
            generated,
            domain,
            root,
        } = owner.kind()
        else {
            panic!("one structured boundary")
        };
        assert_eq!(owner.builtin(), builtin);
        assert_eq!(domain.index(), 0);
        let root = view.structured_root(root).unwrap();
        assert_eq!(root.domain(), domain);
        assert_eq!(root.expression(), generated[5]);
        assert_eq!(
            view.expression(owner.quotient()).unwrap().binder_domain(),
            Some(domain)
        );
        view.expression_count()
    })
}

fn assert_exact_wire_roundtrips(dae: &Dae) {
    let encoded = serde_json::to_vec(dae).unwrap();
    let replayed: Dae = serde_json::from_slice(&encoded).unwrap();
    assert_eq!(serde_json::to_vec(&replayed).unwrap(), encoded);
    let encoded = bincode::serialize(dae).unwrap();
    let mut input = std::io::Cursor::new(encoded.as_slice());
    let replayed: Dae = bincode::deserialize_from(&mut input).unwrap();
    assert_eq!(input.position(), encoded.len() as u64);
    assert_eq!(bincode::serialize(&replayed).unwrap(), encoded);
}

#[test]
fn structured_quotient_roundtrips_retain_one_compact_owner_for_real_and_integer() {
    for builtin in [PureBuiltin::Div, PureBuiltin::Mod, PureBuiltin::Rem] {
        for scalar in [ScalarType::Real, ScalarType::Integer] {
            let mut counts = Vec::new();
            for extent in [0, 2, 1000] {
                let dae = fixture(builtin, scalar, extent);
                counts.push(assert_structured_boundary(&dae, builtin));
                assert_exact_wire_roundtrips(&dae);
            }
            assert!(
                counts.iter().all(|count| *count == counts[0]),
                "extent does not expand the canonical batch"
            );
        }
    }
}

#[test]
fn structured_quotient_wire_rejects_foreign_domains_and_invalid_boundary_markers() {
    let wire = serde_json::to_value(fixture(PureBuiltin::Mod, ScalarType::Real, 2)).unwrap();
    for fault in 0..4 {
        let mut forged = wire.clone();
        let storage = &mut forged["storage"];
        match fault {
            0 => {
                let owner = storage["expressions"]["nodes"]
                    .as_array_mut()
                    .unwrap()
                    .iter_mut()
                    .find(|node| node.get("runtime_quotient_owner").is_some())
                    .unwrap();
                owner["runtime_quotient_owner"]["kind"]["structured_model"]["domain"] = 1.into();
            }
            1 => {
                let roots = storage["structured_roots"].as_array_mut().unwrap();
                roots.push(roots[0].clone());
            }
            2 => storage["structured_roots"].as_array_mut().unwrap().clear(),
            3 => storage["structured_roots"][0]["quotient_owner"]["owner"] = 1.into(),
            _ => unreachable!(),
        }
        assert!(
            serde_json::from_value::<Dae>(forged).is_err(),
            "fault {fault} must not produce a checked DAE"
        );
    }
}

#[test]
fn structured_quotient_replay_refuses_foreign_domain_and_scalar_stages_before_valid_consumption() {
    let source = TestSource::new("mod(n+i,7)");
    let at = source.source("mod(n+i,7)", 0);
    Dae::construct(source.map, |dae| {
        let owned = domain(dae, at, 2)?;
        let foreign = domain(dae, at, 2)?;
        let args = operands(dae, owned, ScalarType::Real, at)?;
        let mut token = dae.begin_quotient_replay(PureBuiltin::Mod, args, at)?;
        assert!(matches!(
            dae.replay_quotient_structured_root(&mut token, foreign),
            Err(DaeConstructionError::InvalidQuotientReplayStage {
                stage: "structured root",
                ..
            })
        ));
        assert!(matches!(
            dae.replay_quotient_relation(&mut token),
            Err(DaeConstructionError::InvalidQuotientReplayStage {
                stage: "relation",
                ..
            })
        ));
        dae.replay_quotient_structured_root(&mut token, owned)?;
        assert!(matches!(
            dae.replay_quotient_structured_root(&mut token, owned),
            Err(DaeConstructionError::InvalidQuotientReplayStage {
                stage: "structured root",
                ..
            })
        ));
        dae.finish_quotient_replay(token)
    })
    .expect("failed stages do not prevent exact valid owner consumption");
}

#[test]
fn structured_quotient_model_owner_refuses_function_scope_before_emitting_boundary() {
    let source =
        TestSource::new("function f input Real n; output Real y; algorithm y:=mod(n+i,7); end f;");
    let at = source.source("mod(n+i,7)", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [real], [real], at),
            |dae, reservation| {
                let n = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("n"), 0, at)
                })?;
                let y = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                let owned = domain(dae, at, 2)?;
                let (n, lhs, seven) = dae.expressions(|expressions| {
                    let n = expressions.at(at).function_parameter(n)?;
                    let i = expressions
                        .at(at)
                        .binder(DomainBinderId::from_raw(owned.index(), 0))?;
                    let lhs = expressions.at(at).binary(BinaryOperator::Add, n, i)?;
                    let seven = expressions.at(at).literal(DaeLiteral::Integer(7))?;
                    Ok((n, lhs, seven))
                })?;
                assert!(matches!(
                    dae.runtime_quotient(PureBuiltin::Mod, [lhs, seven], at),
                    Err(DaeConstructionError::InvalidFunctionScope {
                        expected_function: None,
                        found_function: 0,
                        ..
                    })
                ));
                let quotient =
                    dae.function_runtime_quotient(&body, PureBuiltin::Mod, [n, seven], at)?;
                dae.functions(|functions| {
                    functions.assign(&mut body, y, quotient, at)?;
                    functions.define(body, at)
                })
            },
        )?;
        Ok(())
    })
    .expect("a rejected model owner leaves the exact function owner usable");
    dae.inspect(|view| {
        assert_eq!(view.root_count(), 0);
        assert_eq!(view.structured_root_count(), 0);
        assert_eq!(view.runtime_quotient_owner_count(), 1);
        assert!(matches!(
            view.runtime_quotient_owner_at(0).unwrap().kind(),
            RuntimeQuotientOwnerKind::FunctionBody { .. }
        ));
        assert!((0..view.expression_count()).all(|index| {
            view.expression(view.expression_id(index).unwrap())
                .unwrap()
                .provenance()
                .origin()
                != DaeProvenanceOrigin::Generated(DaeGeneration::RuntimeDiscontinuity)
        }));
    });
}
