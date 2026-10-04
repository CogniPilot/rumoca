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
fn the_holonomic_lane_threads_each_direct_round_incidence_to_the_next() {
    let model = pinned_blocks(5, vec![]);
    let (outcome, reusable) = structural_analysis_capturing(&model, None, None);
    let Err(error) = outcome else {
        panic!("every pinned block starts singular");
    };
    let residue = unmatched_residue(&error).expect("a singular system has a residue");
    let mut threaded = HolonomicReductionState::new(residue, error.clone());
    threaded.reusable = reusable;
    let mut fresh = HolonomicReductionState::new(residue, error);
    for _ in 0..3 {
        let Some(step @ DemotionStep::Reduced { .. }) =
            threaded.direct_step(&model, true, &mut ()).unwrap()
        else {
            panic!("an independent block reduces");
        };
        threaded.accept_demotion(step);
        assert!(
            threaded.reusable.is_some(),
            "a demotion without a manifold hands its incidence to the next round"
        );
        let Some(step @ DemotionStep::Reduced { .. }) =
            fresh.direct_step(&model, true, &mut ()).unwrap()
        else {
            panic!("an independent block reduces");
        };
        fresh.accept_demotion(step);
        fresh.reusable = None;
        assert_eq!(threaded.residue, fresh.residue);
        assert_eq!(
            serde_json::to_vec(threaded.current(&model)).unwrap(),
            serde_json::to_vec(fresh.current(&model)).unwrap(),
            "the threaded incidence decides exactly as a rebuilt one"
        );
    }
}

#[test]
fn later_sorted_tensor_candidate_still_beats_a_reduced_scalar_candidate() {
    let (model, candidates) = mixed_width_constraint();
    let residue = unmatched_residue(&structural_analysis(&model).err().unwrap()).unwrap();
    let source = ReductionSource::new(&model);
    let first = attempt_direct_candidate(&source, residue, &[], &candidates[0], &[], None, &mut ())
        .unwrap();
    assert!(matches!(first, DirectAttempt::Accepted { residue: next, .. } if next < residue));
    let DirectAttempt::Sorted {
        rebuilt: expected, ..
    } = attempt_direct_candidate(&source, residue, &[], &candidates[1], &[], None, &mut ())
        .unwrap()
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
            reuse: None,
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

/// The structural residue bound never exceeds the residue of the system a
/// demotion actually rebuilds, so a candidate it excludes cannot reduce.
#[test]
fn demotion_residue_bound_is_a_lower_bound_on_the_rebuilt_residue() {
    let mut bounded = 0;
    let models = [
        pinned_blocks(3, vec![]),
        pinned_blocks(3, vec![3]),
        mixed_width_constraint().0,
    ];
    for model in &models {
        let (analysis, reusable) = structural_analysis_capturing(model, None, None);
        let residue = unmatched_residue(&analysis.err().expect("the fixture is singular"))
            .expect("a singular fixture has a residue");
        let reusable = reusable.expect("the fixture builds its incidence");
        let source = ReductionSource::new(model);
        let candidates = source.inspect(direct_state_constraints);
        let screen =
            demotion_screen::DemotionScreen::new(&reusable, &source.demotion_rows, residue);
        for candidate in candidates.admissible.iter().chain(&candidates.conditional) {
            let Some(bound) =
                source.inspect(|view, facts| screen.residue_bound(view, facts, candidate))
            else {
                continue;
            };
            let (rebuilt, _) =
                rebuild_with_state_demotion_and_manifold(&source, *candidate, &[]).unwrap();
            let actual = match structural_analysis(&rebuilt) {
                Ok(_) => 0,
                Err(error) => unmatched_residue(&error).expect("an ordinary singularity"),
            };
            assert!(
                bound <= actual,
                "bound {bound} exceeds rebuilt residue {actual}"
            );
            bounded += 1;
        }
    }
    assert!(bounded > 0, "the fixtures exercise the bound");
}

/// `x = w + u; w = s; der(s) = -s; der(x) = 1`, demoting `x` by `x = w`:
/// `der(x)` becomes the derivative of `w`, which the differentiator reads
/// through the equality `w = s` as `der(s)`, a column `w` itself does not
/// show. The closure follows `w` to its state anchor, so the bound includes
/// the `der(s)` column and never exceeds the rebuilt residue of 2.
#[test]
fn a_definition_reading_an_anchored_algebraic_is_bounded_soundly() {
    let text = "Real x; Real s; Real w; Real u; equation x=w+u; w=s; der(s)=-s; der(x)=1;";
    let mut sources = SourceMap::new();
    let source = sources.add("algebraic_definition.mo", text);
    let at = source_provenance(source, text, text);
    let mut defined = None;
    let model = dae::Dae::construct(sources, |model| {
        let scalar = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let (x, s, w, u) = model.variables(|variables| {
            Ok((
                variables.state(VarName::new("x"), scalar, at, Default::default())?,
                variables.state(VarName::new("s"), scalar, at, Default::default())?,
                variables.algebraic(VarName::new("w"), scalar, at, Default::default())?,
                variables.algebraic(VarName::new("u"), scalar, at, Default::default())?,
            ))
        })?;
        let residuals = model.expressions(|e| {
            let xv = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
            let sv = e.at(at).coordinate(dae::CoordinateInput::State(s))?;
            let wv = e.at(at).coordinate(dae::CoordinateInput::Algebraic(w))?;
            let uv = e.at(at).coordinate(dae::CoordinateInput::Algebraic(u))?;
            let dx = e.at(at).coordinate(dae::CoordinateInput::Derivative(x))?;
            let ds = e.at(at).coordinate(dae::CoordinateInput::Derivative(s))?;
            let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            defined = Some(DirectStateConstraint {
                state: x.index(),
                rhs: StateDefinition::Expression(wv.index()),
                rhs_sign: super::super::equalities::EqualitySign::Same,
                owner: at,
            });
            let sum = e.at(at).binary(dae::BinaryOperator::Add, wv, uv)?;
            Ok([
                e.at(at).binary(dae::BinaryOperator::Subtract, xv, sum)?,
                e.at(at).binary(dae::BinaryOperator::Subtract, wv, sv)?,
                e.at(at).binary(dae::BinaryOperator::Add, ds, sv)?,
                e.at(at).binary(dae::BinaryOperator::Subtract, dx, one)?,
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
    let (_, reusable) = structural_analysis_capturing(&model, None, None);
    let reusable = reusable.expect("the fixture builds its incidence");
    let source = ReductionSource::new(&model);
    let candidate = defined.expect("the fixture defines x by w");
    let (rebuilt, _) = rebuild_with_state_demotion_and_manifold(&source, candidate, &[]).unwrap();
    let rebuilt_residue = structural_analysis(&rebuilt)
        .err()
        .and_then(|error| unmatched_residue(&error));
    assert_eq!(
        rebuilt_residue,
        Some(2),
        "the demotion leaves der(x) and der(s) competing"
    );
    let screen = demotion_screen::DemotionScreen::new(&reusable, &source.demotion_rows, 1);
    let bound = source
        .inspect(|view, facts| screen.residue_bound(view, facts, &candidate))
        .expect("the closure follows w through its state anchor");
    assert!(bound <= 2, "bound {bound} exceeds the rebuilt residue 2");
}

/// `x = w + u; w = s*s; u = s; der(s) = -s; der(x) = 1`, demoting `x` by
/// `x = w + u`: `w` has no state anchor, so the differentiator reads its
/// causal definition `s*s`, and `u` its anchor `s`. The closure follows both
/// to `der(s)`, and the bound never exceeds the rebuilt residue.
#[test]
fn a_definition_reading_a_causally_defined_algebraic_is_bounded_soundly() {
    causal_definition_bound_is_sound(dae::BinaryOperator::Add);
}

/// `x = w * u` with the same definitions: the product rule keeps the values
/// of `w` and `u` as coefficients, so the closure adds their columns, and the
/// bound never exceeds the rebuilt residue.
#[test]
fn a_definition_reading_algebraics_as_coefficients_is_bounded_soundly() {
    causal_definition_bound_is_sound(dae::BinaryOperator::Multiply);
}

fn causal_definition_bound_is_sound(operator: dae::BinaryOperator) {
    let text = "Real x; Real s; Real w; Real u; equation x=w+u; w=s*s; u=s; der(s)=-s; der(x)=1;";
    let mut sources = SourceMap::new();
    let source = sources.add("causal_definition.mo", text);
    let at = source_provenance(source, text, text);
    let mut defined = None;
    let model = dae::Dae::construct(sources, |model| {
        let scalar = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let (x, s, w, u) = model.variables(|variables| {
            Ok((
                variables.state(VarName::new("x"), scalar, at, Default::default())?,
                variables.state(VarName::new("s"), scalar, at, Default::default())?,
                variables.algebraic(VarName::new("w"), scalar, at, Default::default())?,
                variables.algebraic(VarName::new("u"), scalar, at, Default::default())?,
            ))
        })?;
        let residuals = model.expressions(|e| {
            let xv = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
            let sv = e.at(at).coordinate(dae::CoordinateInput::State(s))?;
            let wv = e.at(at).coordinate(dae::CoordinateInput::Algebraic(w))?;
            let uv = e.at(at).coordinate(dae::CoordinateInput::Algebraic(u))?;
            let dx = e.at(at).coordinate(dae::CoordinateInput::Derivative(x))?;
            let ds = e.at(at).coordinate(dae::CoordinateInput::Derivative(s))?;
            let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            let sum = e.at(at).binary(operator, wv, uv)?;
            defined = Some(DirectStateConstraint {
                state: x.index(),
                rhs: StateDefinition::Expression(sum.index()),
                rhs_sign: super::super::equalities::EqualitySign::Same,
                owner: at,
            });
            let square = e.at(at).binary(dae::BinaryOperator::Multiply, sv, sv)?;
            Ok([
                e.at(at).binary(dae::BinaryOperator::Subtract, xv, sum)?,
                e.at(at).binary(dae::BinaryOperator::Subtract, wv, square)?,
                e.at(at).binary(dae::BinaryOperator::Subtract, uv, sv)?,
                e.at(at).binary(dae::BinaryOperator::Add, ds, sv)?,
                e.at(at).binary(dae::BinaryOperator::Subtract, dx, one)?,
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
    let (_, reusable) = structural_analysis_capturing(&model, None, None);
    let reusable = reusable.expect("the fixture builds its incidence");
    let source = ReductionSource::new(&model);
    let candidate = defined.expect("the fixture defines x by w + u");
    let (rebuilt, _) = rebuild_with_state_demotion_and_manifold(&source, candidate, &[]).unwrap();
    let actual = structural_analysis(&rebuilt)
        .err()
        .and_then(|error| unmatched_residue(&error))
        .unwrap_or(0);
    let screen = demotion_screen::DemotionScreen::new(&reusable, &source.demotion_rows, 1);
    let bound = source
        .inspect(|view, facts| screen.residue_bound(view, facts, &candidate))
        .expect("the closure follows w through its causal definition");
    assert!(
        bound <= actual,
        "bound {bound} exceeds rebuilt residue {actual}"
    );
}

/// `x = s; der(s) = w; w = p*s; der(x) = 1`, demoting `x` by `x = s`: the
/// derivative of `s` is its explicit definition `w`, whose column `s` does
/// not show. The closure reads the definition's own columns, so the bound
/// includes `w` and never exceeds the rebuilt residue.
#[test]
fn a_definition_reading_a_defined_state_is_bounded_soundly() {
    let text = "parameter Real p; Real x; Real s; Real w; equation x=s; der(s)=w; w=p*s; der(x)=1;";
    let mut sources = SourceMap::new();
    let source = sources.add("defined_state.mo", text);
    let at = source_provenance(source, text, text);
    let mut defined = None;
    let model = dae::Dae::construct(sources, |model| {
        let scalar = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let (p, x, s, w) = model.variables(|variables| {
            Ok((
                variables.parameter(VarName::new("p"), scalar, at, Default::default())?,
                variables.state(VarName::new("x"), scalar, at, Default::default())?,
                variables.state(VarName::new("s"), scalar, at, Default::default())?,
                variables.algebraic(VarName::new("w"), scalar, at, Default::default())?,
            ))
        })?;
        let residuals = model.expressions(|e| {
            let pv = e.at(at).coordinate(dae::CoordinateInput::Parameter(p))?;
            let xv = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
            let sv = e.at(at).coordinate(dae::CoordinateInput::State(s))?;
            let wv = e.at(at).coordinate(dae::CoordinateInput::Algebraic(w))?;
            let dx = e.at(at).coordinate(dae::CoordinateInput::Derivative(x))?;
            let ds = e.at(at).coordinate(dae::CoordinateInput::Derivative(s))?;
            let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            defined = Some(DirectStateConstraint {
                state: x.index(),
                rhs: StateDefinition::Expression(sv.index()),
                rhs_sign: super::super::equalities::EqualitySign::Same,
                owner: at,
            });
            let product = e.at(at).binary(dae::BinaryOperator::Multiply, pv, sv)?;
            Ok([
                e.at(at).binary(dae::BinaryOperator::Subtract, xv, sv)?,
                e.at(at).binary(dae::BinaryOperator::Subtract, ds, wv)?,
                e.at(at)
                    .binary(dae::BinaryOperator::Subtract, wv, product)?,
                e.at(at).binary(dae::BinaryOperator::Subtract, dx, one)?,
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
    let (_, reusable) = structural_analysis_capturing(&model, None, None);
    let reusable = reusable.expect("the fixture builds its incidence");
    let source = ReductionSource::new(&model);
    let candidate = defined.expect("the fixture defines x by s");
    assert!(
        source.inspect(|_, facts| facts.derivative_definitions.iter().any(Option::is_some)),
        "der(s) = w is an explicit derivative definition"
    );
    let screen = demotion_screen::DemotionScreen::new(&reusable, &source.demotion_rows, 1);
    let bound = source
        .inspect(|view, facts| screen.residue_bound(view, facts, &candidate))
        .expect("the closure reads the definition's own columns");
    let (rebuilt, _) = rebuild_with_state_demotion_and_manifold(&source, candidate, &[]).unwrap();
    let actual = structural_analysis(&rebuilt)
        .err()
        .and_then(|error| unmatched_residue(&error))
        .unwrap_or(0);
    assert!(
        bound <= actual,
        "bound {bound} exceeds rebuilt residue {actual}"
    );
}

/// `x = s; der(s) = q; der(q) = -s; der(x) = 1`, demoting `x` by `x = s`:
/// `der(s)` is the state `q`, a known value, so the bound applies and never
/// exceeds the rebuilt residue.
#[test]
fn a_definition_reading_a_state_defined_by_a_state_is_bounded_soundly() {
    let text = "Real x; Real s; Real q; equation x=s; der(s)=q; der(q)=-s; der(x)=1;";
    let mut sources = SourceMap::new();
    let source = sources.add("state_defined_state.mo", text);
    let at = source_provenance(source, text, text);
    let mut defined = None;
    let model = dae::Dae::construct(sources, |model| {
        let scalar = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let (x, s, q) = model.variables(|variables| {
            Ok((
                variables.state(VarName::new("x"), scalar, at, Default::default())?,
                variables.state(VarName::new("s"), scalar, at, Default::default())?,
                variables.state(VarName::new("q"), scalar, at, Default::default())?,
            ))
        })?;
        let residuals = model.expressions(|e| {
            let xv = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
            let sv = e.at(at).coordinate(dae::CoordinateInput::State(s))?;
            let qv = e.at(at).coordinate(dae::CoordinateInput::State(q))?;
            let dx = e.at(at).coordinate(dae::CoordinateInput::Derivative(x))?;
            let ds = e.at(at).coordinate(dae::CoordinateInput::Derivative(s))?;
            let dq = e.at(at).coordinate(dae::CoordinateInput::Derivative(q))?;
            let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            defined = Some(DirectStateConstraint {
                state: x.index(),
                rhs: StateDefinition::Expression(sv.index()),
                rhs_sign: super::super::equalities::EqualitySign::Same,
                owner: at,
            });
            Ok([
                e.at(at).binary(dae::BinaryOperator::Subtract, xv, sv)?,
                e.at(at).binary(dae::BinaryOperator::Subtract, ds, qv)?,
                e.at(at).binary(dae::BinaryOperator::Add, dq, sv)?,
                e.at(at).binary(dae::BinaryOperator::Subtract, dx, one)?,
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
    let (_, reusable) = structural_analysis_capturing(&model, None, None);
    let reusable = reusable.expect("the fixture builds its incidence");
    let source = ReductionSource::new(&model);
    let candidate = defined.expect("the fixture defines x by s");
    let screen = demotion_screen::DemotionScreen::new(&reusable, &source.demotion_rows, 1);
    let bound = source
        .inspect(|view, facts| screen.residue_bound(view, facts, &candidate))
        .expect("a definition read through a state-valued rate is bounded");
    let (rebuilt, _) = rebuild_with_state_demotion_and_manifold(&source, candidate, &[]).unwrap();
    let actual = structural_analysis(&rebuilt)
        .err()
        .and_then(|error| unmatched_residue(&error))
        .unwrap_or(0);
    assert!(
        bound <= actual,
        "bound {bound} exceeds rebuilt residue {actual}"
    );
}
