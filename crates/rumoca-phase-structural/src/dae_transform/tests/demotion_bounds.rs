use super::*;

#[test]
fn independent_demotions_do_not_rebuild_candidates_that_cannot_sort() {
    for dimensions in [vec![], vec![1], vec![3]] {
        let model = pinned_blocks(5, dimensions);
        let (prepared, report) = inspect_prepare_for_solve(&model);
        assert!(prepared.is_ok());
        let attempts = report
            .records
            .iter()
            .filter(|record| {
                matches!(
                    record,
                    ReductionRecord::Attempt {
                        lane: ReductionLane::Direct,
                        ..
                    }
                )
            })
            .count();
        let selected = report
            .records
            .iter()
            .filter(|record| {
                matches!(
                    record,
                    ReductionRecord::Selected {
                        lane: ReductionLane::Direct,
                        ..
                    }
                )
            })
            .count();
        assert_eq!(selected, 5);
        assert!(
            attempts < 10,
            "independent candidates need no quadratic trials: {attempts}"
        );
    }
}

#[test]
fn later_sorted_tensor_candidate_still_beats_a_reduced_scalar_candidate() {
    let (model, candidates) = mixed_width_constraint();
    let residue = unmatched_residue(&structural_analysis(&model).err().unwrap()).unwrap();
    let source = ReductionSource::new(&model);
    let first =
        attempt_direct_candidate(&source, residue, &[], &candidates[0], &[], &mut ()).unwrap();
    assert!(matches!(first, DirectAttempt::Accepted { residue: next, .. } if next < residue));
    let DirectAttempt::Sorted {
        rebuilt: expected, ..
    } = attempt_direct_candidate(&source, residue, &[], &candidates[1], &[], &mut ()).unwrap()
    else {
        panic!("the whole tensor demotion sorts both constraint rows");
    };
    let round = demotion_pass_with_observer(
        &source,
        residue,
        &[],
        &candidates,
        &[],
        DemotionPassPolicy {
            group: CandidateGroup::DirectAdmissible,
            allow_held: true,
        },
        &mut (),
    )
    .unwrap();
    let Some(DemotionStep::Sorted { dae: actual, .. }) = round.step else {
        panic!("a checked Reduced candidate must not hide a later Sorted candidate");
    };
    assert_eq!(
        serde_json::to_vec(&actual).unwrap(),
        serde_json::to_vec(&expected).unwrap()
    );
}

fn mixed_width_constraint() -> (dae::Dae, [DirectStateConstraint; 2]) {
    let text =
        "parameter Real p; Real x; Real y[2]; Real v[2]; equation y={x,p}; der(x)=1; der(y)=v;";
    let mut sources = SourceMap::new();
    let source = sources.add("mixed_width.mo", text);
    let at = source_provenance(source, text, text);
    let mut candidates = None;
    let model = dae::Dae::construct(sources, |model| {
        let (scalar, vector) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [2]), at)?,
            ))
        })?;
        let (x, y, p, v) = model.variables(|variables| {
            Ok((
                variables.state(VarName::new("x"), scalar, at, Default::default())?,
                variables.state(VarName::new("y"), vector, at, Default::default())?,
                variables.parameter(VarName::new("p"), scalar, at, Default::default())?,
                variables.algebraic(VarName::new("v"), vector, at, Default::default())?,
            ))
        })?;
        let residuals =
            model.expressions(|e| {
                let xv = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
                let yv = e.at(at).coordinate(dae::CoordinateInput::State(y))?;
                let pv = e.at(at).coordinate(dae::CoordinateInput::Parameter(p))?;
                let vv = e.at(at).coordinate(dae::CoordinateInput::Algebraic(v))?;
                let dx = e.at(at).coordinate(dae::CoordinateInput::Derivative(x))?;
                let dy = e.at(at).coordinate(dae::CoordinateInput::Derivative(y))?;
                let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
                let index = e.at(at).literal(dae::DaeLiteral::Integer(1))?;
                let projected = e.at(at).index(
                    yv,
                    [dae::Subscript::Index {
                        expression: index,
                        provenance: at,
                    }],
                )?;
                let array = e.at(at).array([xv, pv])?;
                // Both exact definitions follow from the same whole-tensor equation.
                candidates = Some([(x.index(), projected), (y.index(), array)].map(
                    |(state, rhs)| DirectStateConstraint {
                        state,
                        rhs: StateDefinition::Expression(rhs.index()),
                        rhs_sign: super::super::equalities::EqualitySign::Same,
                        owner: at,
                    },
                ));
                Ok([
                    e.at(at).binary(dae::BinaryOperator::Subtract, yv, array)?,
                    e.at(at).binary(dae::BinaryOperator::Subtract, dx, one)?,
                    e.at(at).binary(dae::BinaryOperator::Subtract, dy, vv)?,
                ])
            })?;
        model.continuous(|continuous| {
            for residual in residuals {
                continuous.value_equation(at, residual)?;
            }
            Ok(())
        })
    })
    .unwrap();
    (model, candidates.unwrap())
}

fn pinned_blocks(count: usize, dimensions: Vec<u32>) -> dae::Dae {
    let text = "parameter Real p; Real x; Real v; equation x=p; der(x)=v;";
    let mut sources = SourceMap::new();
    let source = sources.add("pinned_blocks.mo", text);
    let at = source_provenance(source, text, text);
    dae::Dae::construct(sources, |model| {
        let ty = model.types(|types| {
            types.derived(dae::ValueType::array(dae::ScalarType::Real, dimensions), at)
        })?;
        let blocks = model.variables(|variables| {
            (0..count)
                .map(|i| {
                    Ok((
                        variables.parameter(
                            VarName::new(format!("p{i}")),
                            ty,
                            at,
                            Default::default(),
                        )?,
                        variables.state(
                            VarName::new(format!("x{i}")),
                            ty,
                            at,
                            Default::default(),
                        )?,
                        variables.algebraic(
                            VarName::new(format!("v{i}")),
                            ty,
                            at,
                            Default::default(),
                        )?,
                    ))
                })
                .collect::<Result<Vec<_>, dae::DaeConstructionError>>()
        })?;
        let residuals = model.expressions(|expressions| {
            blocks
                .into_iter()
                .map(|(p, x, v)| {
                    let p = expressions
                        .at(at)
                        .coordinate(dae::CoordinateInput::Parameter(p))?;
                    let value = expressions
                        .at(at)
                        .coordinate(dae::CoordinateInput::State(x))?;
                    let derivative = expressions
                        .at(at)
                        .coordinate(dae::CoordinateInput::Derivative(x))?;
                    let v = expressions
                        .at(at)
                        .coordinate(dae::CoordinateInput::Algebraic(v))?;
                    Ok([
                        expressions
                            .at(at)
                            .binary(dae::BinaryOperator::Subtract, value, p)?,
                        expressions
                            .at(at)
                            .binary(dae::BinaryOperator::Subtract, derivative, v)?,
                    ])
                })
                .collect::<Result<Vec<_>, dae::DaeConstructionError>>()
        })?;
        model.continuous(|continuous| {
            for residual in residuals.into_iter().flatten() {
                continuous.value_equation(at, residual)?;
            }
            Ok(())
        })
    })
    .unwrap()
}
