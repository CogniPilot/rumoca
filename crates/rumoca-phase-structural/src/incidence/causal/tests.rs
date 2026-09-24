use super::*;
use rumoca_core::{SourceMap, Span, TypeId, VarName};

#[derive(Clone, Copy)]
enum Case {
    Loop,
    Cancellation,
    Nonlinear,
    Parameter,
    Branch,
    Unit,
    Weighted,
    TunableSelf,
}

fn model(case: Case, parameter: f64) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("causal-coefficient.mo", "x - p*y; y-z; z-x;");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 17)).unwrap();
    dae::Dae::construct(sources, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                at,
            )
        })?;
        let binding = model.expressions(|e| e.at(at).literal(dae::DaeLiteral::Real(parameter)))?;
        let (x, y, z, p) = model.variables(|variables| {
            Ok((
                variables.algebraic(VarName::new("x"), real, at, Default::default())?,
                variables.algebraic(VarName::new("y"), real, at, Default::default())?,
                variables.algebraic(VarName::new("z"), real, at, Default::default())?,
                variables.parameter(
                    VarName::new("p"),
                    real,
                    at,
                    dae::VariableAttributes {
                        binding: (!matches!(case, Case::Loop)).then_some(binding),
                        start: Some(binding),
                        is_tunable: matches!(case, Case::TunableSelf),
                        ..Default::default()
                    },
                )?,
            ))
        })?;
        let rows = model.expressions(|e| {
            let x = e.at(at).coordinate(dae::CoordinateInput::Algebraic(x))?;
            let y = e.at(at).coordinate(dae::CoordinateInput::Algebraic(y))?;
            let z = e.at(at).coordinate(dae::CoordinateInput::Algebraic(z))?;
            let p = e.at(at).coordinate(dae::CoordinateInput::Parameter(p))?;
            let weighted = e.at(at).binary(dae::BinaryOperator::Multiply, p, y)?;
            let first = match case {
                Case::Unit => e.at(at).binary(dae::BinaryOperator::Add, x, y)?,
                Case::TunableSelf => {
                    let weighted = e.at(at).binary(dae::BinaryOperator::Multiply, p, x)?;
                    let difference = e
                        .at(at)
                        .binary(dae::BinaryOperator::Subtract, x, weighted)?;
                    e.at(at).binary(dae::BinaryOperator::Add, difference, y)?
                }
                Case::Weighted => {
                    let weighted = e.at(at).binary(dae::BinaryOperator::Multiply, p, x)?;
                    e.at(at).binary(dae::BinaryOperator::Add, weighted, y)?
                }
                Case::Loop => {
                    let weighted = e.at(at).binary(dae::BinaryOperator::Multiply, p, x)?;
                    e.at(at)
                        .binary(dae::BinaryOperator::Subtract, weighted, y)?
                }
                Case::Parameter => e
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, x, weighted)?,
                Case::Cancellation => {
                    let cancel = e.at(at).binary(dae::BinaryOperator::Subtract, x, x)?;
                    e.at(at).binary(dae::BinaryOperator::Add, cancel, y)?
                }
                Case::Nonlinear => {
                    let square = e.at(at).binary(dae::BinaryOperator::Multiply, x, x)?;
                    let residual = e.at(at).binary(dae::BinaryOperator::Subtract, x, square)?;
                    e.at(at).binary(dae::BinaryOperator::Add, residual, y)?
                }
                Case::Branch => {
                    let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
                    let condition = e.at(at).binary(dae::BinaryOperator::Greater, p, zero)?;
                    e.at(at).conditional([(condition, x)], y)?
                }
            };
            let second_lhs = if matches!(case, Case::Loop) {
                e.at(at).binary(dae::BinaryOperator::Multiply, p, y)?
            } else {
                y
            };
            let second = e
                .at(at)
                .binary(dae::BinaryOperator::Subtract, second_lhs, z)?;
            let third = e.at(at).binary(dae::BinaryOperator::Subtract, z, x)?;
            Ok(vec![first, second, third])
        })?;
        model.continuous(|continuous| {
            for residual in rows {
                continuous.equation(at, |equation| equation.residual(residual))?;
            }
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn zero_parameter_coefficient_is_not_a_causal_pivot_and_whole_plan_covers() {
    model(Case::Loop, 0.0).inspect(|view| {
        let incidence = build_incidence(view).unwrap();
        let proofs = incidence.causal_candidates.as_ref().unwrap();
        assert_eq!(incidence.eq_unknowns.row(0), &[0, 1]);
        assert_eq!(proofs.row(0), &[1]);
        let sorted = crate::sort(view).unwrap();
        let crate::BltBlock::AlgebraicLoop {
            equations,
            unknowns,
            tearing: Some(plan),
            guarded_tearing,
        } = &sorted.blocks[0]
        else {
            panic!("coupled loop should retain a reduced plan");
        };
        let graph = guarded_tearing
            .as_ref()
            .expect("retain the conditional graph candidate separately");
        assert_ne!(graph, plan);
        assert!(
            graph.causal_sequence.iter().any(|&(row, col)| {
                let global_col = incidence
                    .unknowns
                    .iter()
                    .position(|u| *u == unknowns[col])
                    .unwrap();
                !proofs.row(equations[row].0).contains(&global_col)
            }),
            "the graph candidate must not masquerade as unit-proven"
        );
        let mut known = plan
            .tear_var_local_indices
            .iter()
            .copied()
            .collect::<HashSet<_>>();
        let mut rows = plan
            .residual_eq_local_indices
            .iter()
            .copied()
            .collect::<HashSet<_>>();
        assert_eq!(known.len(), 1);
        for &(row, col) in &plan.causal_sequence {
            let global_row = equations[row].0;
            let global_col = incidence
                .unknowns
                .iter()
                .position(|u| *u == unknowns[col])
                .unwrap();
            assert!(
                proofs.row(global_row).contains(&global_col),
                "unproved causal pivot"
            );
            for &read in incidence.eq_unknowns.row(global_row) {
                let local = unknowns
                    .iter()
                    .position(|u| *u == incidence.unknowns[read])
                    .unwrap();
                assert!(read == global_col || known.contains(&local));
            }
            assert!(known.insert(col));
            assert!(rows.insert(row));
        }
        assert_eq!(known.len(), 3);
        assert_eq!(rows.len(), 3);
    });
}

#[test]
fn complete_residual_rejects_cancelled_and_nonlinear_self_dependencies() {
    for case in [Case::Cancellation, Case::Nonlinear] {
        model(case, 0.0).inspect(|view| {
            let incidence = build_incidence(view).unwrap();
            assert_eq!(incidence.eq_unknowns.row(0), &[0, 1]);
            assert_eq!(incidence.causal_candidates.as_ref().unwrap().row(0), &[1]);
        });
    }
}

#[test]
fn parameter_defaults_and_branches_do_not_issue_nonzero_proofs() {
    for parameter in [0.0, 1.0, -2.0] {
        for case in [Case::Parameter, Case::Branch] {
            model(case, parameter).inspect(|view| {
                let incidence = build_incidence(view).unwrap();
                let proof = incidence.causal_candidates.as_ref().unwrap().row(0);
                match case {
                    Case::Parameter => assert_eq!(proof, &[0]),
                    Case::Branch => assert!(proof.is_empty()),
                    _ => unreachable!(),
                }
            });
        }
    }
}

#[test]
fn changed_coefficient_rebuilds_proof_even_when_incidence_is_identical() {
    let old = model(Case::Unit, 2.0);
    let saved =
        old.inspect(|view| ReusableIncidence::from_incidence(&build_incidence(view).unwrap()));
    model(Case::Weighted, 2.0).inspect(|view| {
        let fresh = build_incidence(view).unwrap();
        let reused =
            build_incidence_reusing(view, IncidenceReuse::new(&saved, &[true, false, false]))
                .unwrap();
        assert_eq!(saved.rows, fresh.eq_unknowns);
        assert_eq!(saved.causal_candidates.row(0), &[0, 1]);
        assert_eq!(fresh.causal_candidates.as_ref().unwrap().row(0), &[1]);
        assert_eq!(reused.causal_candidates, fresh.causal_candidates);
    });
}

#[test]
fn graph_only_incidence_does_not_issue_coefficient_certified_tearing() {
    let incidence =
        solver_incidence(vec![HashSet::from([0, 1]), HashSet::from([0, 1])], 2).unwrap();
    let blocks = crate::build_blt_from_incidence(&incidence).unwrap();
    assert!(matches!(
        &blocks[0],
        crate::BltBlock::AlgebraicLoop { tearing: None, .. }
    ));
}

#[test]
fn tunable_zero_default_does_not_hide_a_repeated_target() {
    for parameter in [0.0, 1.0] {
        model(Case::TunableSelf, parameter).inspect(|view| {
            let incidence = build_incidence(view).unwrap();
            assert_eq!(incidence.eq_unknowns.row(0), &[0, 1]);
            assert_eq!(incidence.causal_candidates.as_ref().unwrap().row(0), &[1]);
        });
    }
}
