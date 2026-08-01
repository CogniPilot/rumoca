//! Structural analysis over one immutable, valid-by-construction DAE.
//!
//! Entry points accept [`rumoca_ir_dae::DaeView`], so every structural product
//! carrying DAE identities remains branded to the inspected root and cannot be
//! mixed with another model.

mod blt;
mod causal_definitions;
mod dae_transform;
pub mod diagnostic_codes;
mod diagnostics;
pub mod incidence;
mod matching;
pub mod report;
pub mod runtime_defined;
mod tarjan;
pub mod tearing;
mod types;

use std::collections::HashSet;

use rumoca_ir_dae as dae;

pub use causal_definitions::CausalDefinitions;
pub use dae_transform::{
    InitialValuePin, InitialValueRole, PinTerm, PreparedDae, PreparedSystem, prepare_for_solve,
};
pub use diagnostic_codes::STRUCTURAL_DIAGNOSTIC_CODES;
pub use diagnostics::{AlgebraicLoop, StructuralDiagnostics};
pub use incidence::{Incidence, solver_incidence};
pub use report::{BlockReport, StructuralReport, TearingReport};
pub use runtime_defined::{
    runtime_defined_continuous_unknown_names, runtime_defined_unknown_names,
};
pub use tearing::{TearingResult, tear_algebraic_loop, tear_algebraic_loop_with_causal_candidates};
pub use types::{
    BltBlock, EquationRef, SingularBlockWitness, SortedDae, StructuralError, StructuredScalarBlock,
    UnknownId,
};

/// Analyze and BLT-sort a checked DAE view.
pub fn sort<'dae>(view: dae::DaeView<'dae>) -> Result<SortedDae<'dae>, StructuralError> {
    let incidence = incidence::build_incidence(view)?;
    if incidence.n_eq == 0 && incidence.n_var == 0 {
        return Err(StructuralError::EmptySystem);
    }
    let preferences = explicit_derivative_preferences(view, &incidence);
    let (match_eq, match_var) = maximum_matching(&incidence, &preferences);
    require_perfect_matching(view, &incidence, &match_eq, &match_var)?;
    let adjacency =
        incidence::build_dependency_graph(&incidence.eq_unknowns, &match_var, incidence.n_eq);
    let diagnostics = diagnostics::collect_warnings(view, &incidence, &match_eq, &adjacency);
    let blocks = blt::build_blt_blocks(&incidence, &match_eq, &adjacency);
    let matching = match_eq
        .iter()
        .enumerate()
        .filter_map(|(equation, unknown)| {
            unknown.map(|unknown| {
                (
                    incidence.equation_refs[equation],
                    incidence.unknowns[unknown],
                )
            })
        })
        .collect();
    Ok(SortedDae {
        blocks,
        matching,
        diagnostics,
    })
}

fn explicit_derivative_preferences<'dae>(
    view: dae::DaeView<'dae>,
    incidence: &Incidence<'dae>,
) -> Vec<Option<usize>> {
    incidence
        .equation_refs
        .iter()
        .map(|equation| {
            let dae::ContinuousOwnerView::Residual { equation, .. } =
                view.continuous_owner_for_scalar_row(equation.0)?
            else {
                return None;
            };
            let residual = view.expression(equation.residual())?;
            if !residual.value_type().is_scalar() {
                return None;
            }
            let dae::ExpressionOperation::Binary {
                operator: dae::BinaryOperator::Subtract,
                lhs,
                ..
            } = residual.operation()
            else {
                return None;
            };
            let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state)) =
                view.expression(lhs)?.operation()
            else {
                return None;
            };
            incidence.unknowns.iter().position(|unknown| {
                matches!(
                    unknown,
                    UnknownId::Derivative {
                        state: candidate,
                        scalar: 0
                    } if *candidate == state
                )
            })
        })
        .collect()
}

/// Produce diagnostic-only structural results without inventing a fallback
/// matching for a singular model.
pub fn analyze(view: dae::DaeView<'_>) -> StructuralDiagnostics {
    let mut result = StructuralDiagnostics::default();
    let incidence = match incidence::build_incidence(view) {
        Ok(incidence) => incidence,
        Err(error) => {
            use rumoca_core::PhaseError;
            result.diagnostics.push(error.to_diagnostic());
            return result;
        }
    };
    result.n_equations = incidence.n_eq;
    result.n_unknowns = incidence.n_var;
    if incidence.n_eq == 0 && incidence.n_var == 0 {
        return result;
    }

    let preferences = explicit_derivative_preferences(view, &incidence);
    let (match_eq, match_var) = maximum_matching(&incidence, &preferences);
    result.matching_size = match_eq.iter().filter(|matched| matched.is_some()).count();
    result.unmatched_equations = match_eq
        .iter()
        .enumerate()
        .filter(|(_, matched)| matched.is_none())
        .map(|(index, _)| equation_label(view, &incidence.equation_refs[index]))
        .collect();
    result.unmatched_unknowns = match_var
        .iter()
        .enumerate()
        .filter(|(_, matched)| matched.is_none())
        .map(|(index, _)| unknown_label(view, incidence.unknowns[index]))
        .collect();
    if result.matching_size < incidence.n_eq || result.matching_size < incidence.n_var {
        let span = unmatched_span(&incidence, &match_eq, &match_var);
        result.diagnostics.push(diagnostics::singular_warning(
            span,
            &result.unmatched_equations,
            &result.unmatched_unknowns,
            result.matching_size,
            incidence.n_eq,
            incidence.n_var,
        ));
        return result;
    }

    let adjacency =
        incidence::build_dependency_graph(&incidence.eq_unknowns, &match_var, incidence.n_eq);
    result.diagnostics.extend(diagnostics::collect_warnings(
        view, &incidence, &match_eq, &adjacency,
    ));
    result
}

pub fn build_structural_report(
    view: dae::DaeView<'_>,
) -> Result<StructuralReport, StructuralError> {
    let sorted = sort(view)?;
    let matching = sorted
        .matching
        .iter()
        .map(|(equation, unknown)| {
            (
                equation_label(view, equation),
                unknown_label(view, *unknown),
            )
        })
        .collect();
    let blocks = sorted
        .blocks
        .iter()
        .map(|block| block_report(view, block))
        .collect();
    Ok(StructuralReport {
        n_equations: sorted.matching.len(),
        n_unknowns: sorted.matching.len(),
        matching,
        blocks,
    })
}

fn block_report<'dae>(view: dae::DaeView<'dae>, block: &BltBlock<'dae>) -> BlockReport {
    match block {
        BltBlock::Scalar { equation, unknown } => BlockReport::Scalar {
            equation: equation_label(view, equation),
            unknown: unknown_label(view, *unknown),
        },
        BltBlock::AlgebraicLoop {
            equations,
            unknowns,
        } => BlockReport::Coupled {
            equations: equations
                .iter()
                .map(|equation| equation_label(view, equation))
                .collect(),
            unknowns: unknowns
                .iter()
                .map(|unknown| unknown_label(view, *unknown))
                .collect(),
            tearing: None,
        },
        BltBlock::StructuredScalar(family) => BlockReport::StructuredScalar {
            origin: equation_label(view, &EquationRef(family.first_equation_index)),
            point_count: family.point_count,
            equations_per_point: family.equations_per_point,
        },
    }
}

pub fn build_blt_from_incidence<'dae>(
    incidence: &Incidence<'dae>,
) -> Result<Vec<BltBlock<'dae>>, StructuralError> {
    if incidence.n_eq == 0 && incidence.n_var == 0 {
        return Ok(Vec::new());
    }
    let (match_eq, match_var) = maximum_matching(incidence, &[]);
    let matched = match_eq.iter().filter(|entry| entry.is_some()).count();
    if matched < incidence.n_eq || matched < incidence.n_var {
        return Err(unlabeled_singular(incidence, &match_eq, &match_var));
    }
    let adjacency =
        incidence::build_dependency_graph(&incidence.eq_unknowns, &match_var, incidence.n_eq);
    Ok(blt::build_blt_blocks(incidence, &match_eq, &adjacency))
}

#[derive(Debug)]
pub struct RegularSubsystem<'dae> {
    pub incidence: Incidence<'dae>,
    pub blocks: Vec<BltBlock<'dae>>,
    pub dropped_equations: Vec<EquationRef>,
    pub dropped_unknowns: Vec<UnknownId<'dae>>,
}

pub fn maximum_regular_subsystem<'dae>(
    incidence: &Incidence<'dae>,
    preferred_unknowns: &[Option<usize>],
) -> Result<RegularSubsystem<'dae>, StructuralError> {
    let (match_eq, match_var) = maximum_matching(incidence, preferred_unknowns);
    let matched_equations = matched_indices(&match_eq);
    let matched_unknowns = matched_indices(&match_var);
    if matched_equations.is_empty() || matched_equations.len() != matched_unknowns.len() {
        return Err(unlabeled_singular(incidence, &match_eq, &match_var));
    }
    let mut old_to_new = vec![None; incidence.n_var];
    for (new, old) in matched_unknowns.iter().copied().enumerate() {
        old_to_new[old] = Some(new);
    }
    let rows = matched_equations
        .iter()
        .map(|equation| {
            incidence
                .eq_unknowns
                .row(*equation)
                .iter()
                .filter_map(|unknown| old_to_new[*unknown])
                .collect::<HashSet<_>>()
        })
        .collect();
    let regular = Incidence {
        n_eq: matched_equations.len(),
        n_var: matched_unknowns.len(),
        eq_unknowns: incidence::rows::IncidenceRows::from_sets(rows),
        unknowns: matched_unknowns
            .iter()
            .map(|index| incidence.unknowns[*index])
            .collect(),
        unknown_spans: matched_unknowns
            .iter()
            .filter_map(|index| incidence.unknown_spans.get(*index).copied())
            .collect(),
        equation_refs: matched_equations
            .iter()
            .map(|index| incidence.equation_refs[*index])
            .collect(),
        equation_spans: matched_equations
            .iter()
            .filter_map(|index| incidence.equation_spans.get(*index).copied())
            .collect(),
        structured_matching: Vec::new(),
    };
    let blocks = build_blt_from_incidence(&regular)?;
    let matched_equation_set = matched_equations.iter().copied().collect::<HashSet<_>>();
    let matched_unknown_set = matched_unknowns.iter().copied().collect::<HashSet<_>>();
    Ok(RegularSubsystem {
        incidence: regular,
        blocks,
        dropped_equations: incidence
            .equation_refs
            .iter()
            .enumerate()
            .filter_map(|(index, equation)| {
                (!matched_equation_set.contains(&index)).then_some(*equation)
            })
            .collect(),
        dropped_unknowns: incidence
            .unknowns
            .iter()
            .enumerate()
            .filter_map(|(index, unknown)| {
                (!matched_unknown_set.contains(&index)).then_some(*unknown)
            })
            .collect(),
    })
}

fn maximum_matching(
    incidence: &Incidence<'_>,
    preferred_unknowns: &[Option<usize>],
) -> (Vec<Option<usize>>, Vec<Option<usize>>) {
    matching::maximum_matching_with_structured(
        incidence.n_eq,
        incidence.n_var,
        &incidence.eq_unknowns,
        preferred_unknowns,
        &incidence.structured_matching,
    )
}

fn require_perfect_matching<'dae>(
    view: dae::DaeView<'dae>,
    incidence: &Incidence<'dae>,
    match_eq: &[Option<usize>],
    match_var: &[Option<usize>],
) -> Result<(), StructuralError> {
    let matched = match_eq.iter().filter(|entry| entry.is_some()).count();
    if matched == incidence.n_eq && matched == incidence.n_var {
        return Ok(());
    }
    let unmatched_equations = match_eq
        .iter()
        .enumerate()
        .filter(|(_, matched)| matched.is_none())
        .map(|(index, _)| equation_label(view, &incidence.equation_refs[index]))
        .collect();
    let unmatched_unknowns = match_var
        .iter()
        .enumerate()
        .filter(|(_, matched)| matched.is_none())
        .map(|(index, _)| unknown_label(view, incidence.unknowns[index]))
        .collect();
    Err(StructuralError::Singular {
        n_equations: incidence.n_eq,
        n_unknowns: incidence.n_var,
        n_matched: matched,
        unmatched_equations,
        unmatched_unknowns,
        unmatched_unknown_spans: match_var
            .iter()
            .enumerate()
            .filter_map(|(index, matched)| {
                matched
                    .is_none()
                    .then(|| incidence.unknown_spans.get(index).copied())
                    .flatten()
            })
            .collect(),
        over_determined_block: Box::new(over_determined_block(incidence, match_eq, match_var)),
    })
}

fn unlabeled_singular(
    incidence: &Incidence<'_>,
    match_eq: &[Option<usize>],
    match_var: &[Option<usize>],
) -> StructuralError {
    StructuralError::Singular {
        n_equations: incidence.n_eq,
        n_unknowns: incidence.n_var,
        n_matched: match_eq.iter().filter(|entry| entry.is_some()).count(),
        unmatched_equations: match_eq
            .iter()
            .enumerate()
            .filter(|(_, matched)| matched.is_none())
            .map(|(index, _)| format!("f_x[{index}]"))
            .collect(),
        unmatched_unknowns: match_var
            .iter()
            .enumerate()
            .filter(|(_, matched)| matched.is_none())
            .map(|(index, _)| format!("unknown[{index}]"))
            .collect(),
        unmatched_unknown_spans: match_var
            .iter()
            .enumerate()
            .filter_map(|(index, matched)| {
                matched
                    .is_none()
                    .then(|| incidence.unknown_spans.get(index).copied())
                    .flatten()
            })
            .collect(),
        over_determined_block: Box::new(over_determined_block(incidence, match_eq, match_var)),
    }
}

fn over_determined_block(
    incidence: &Incidence<'_>,
    match_eq: &[Option<usize>],
    match_var: &[Option<usize>],
) -> SingularBlockWitness {
    let mut seen_equations = vec![false; incidence.n_eq];
    let mut seen_unknowns = vec![false; incidence.n_var];
    let mut pending = match_eq
        .iter()
        .enumerate()
        .filter_map(|(equation, matched)| matched.is_none().then_some(equation))
        .collect::<Vec<_>>();
    for equation in pending.iter().copied() {
        seen_equations[equation] = true;
    }
    while let Some(equation) = pending.pop() {
        for unknown in incidence.eq_unknowns.row(equation).iter().copied() {
            if seen_unknowns[unknown] {
                continue;
            }
            seen_unknowns[unknown] = true;
            if let Some(owner) = match_var[unknown]
                && !seen_equations[owner]
            {
                seen_equations[owner] = true;
                pending.push(owner);
            }
        }
    }
    SingularBlockWitness {
        equations: seen_equations.iter().filter(|seen| **seen).count(),
        unknowns: seen_unknowns.iter().filter(|seen| **seen).count(),
        sample: seen_equations
            .iter()
            .enumerate()
            .filter(|(_, seen)| **seen)
            .map(|(index, _)| format!("f_x[{index}]"))
            .take(24)
            .collect(),
    }
}

fn unmatched_span(
    incidence: &Incidence<'_>,
    match_eq: &[Option<usize>],
    match_var: &[Option<usize>],
) -> Option<rumoca_core::Span> {
    match_eq
        .iter()
        .enumerate()
        .find_map(|(index, matched)| {
            matched
                .is_none()
                .then(|| incidence.equation_spans.get(index).copied())
                .flatten()
        })
        .or_else(|| {
            match_var.iter().enumerate().find_map(|(index, matched)| {
                matched
                    .is_none()
                    .then(|| incidence.unknown_spans.get(index).copied())
                    .flatten()
            })
        })
}

fn matched_indices(matching: &[Option<usize>]) -> Vec<usize> {
    matching
        .iter()
        .enumerate()
        .filter_map(|(index, matched)| matched.is_some().then_some(index))
        .collect()
}

pub(crate) fn equation_label(view: dae::DaeView<'_>, equation: &EquationRef) -> String {
    let Some(owner) = view.continuous_owner_for_scalar_row(equation.0) else {
        return equation.to_string();
    };
    let provenance = match owner {
        dae::ContinuousOwnerView::Residual { equation, .. } => equation.provenance(),
        dae::ContinuousOwnerView::Structured { family, .. } => family.provenance(),
    };
    match view.source_text(provenance) {
        Some(text) if !text.trim().is_empty() => format!("{equation} ({})", text.trim()),
        _ => equation.to_string(),
    }
}

pub(crate) fn unknown_label<'dae>(view: dae::DaeView<'dae>, unknown: UnknownId<'dae>) -> String {
    match unknown {
        UnknownId::Derivative { state, scalar } => {
            let variable = view
                .variable(state.into())
                .expect("branded state identity resolves");
            let name = variable
                .scalar_name(scalar as usize)
                .expect("checked state scalar ordinal resolves");
            format!("der({name})")
        }
        UnknownId::Algebraic { variable, scalar } => view
            .variable(variable.into())
            .expect("branded algebraic identity resolves")
            .scalar_name(scalar as usize)
            .expect("checked algebraic scalar ordinal resolves"),
        UnknownId::Solver(index) => format!("y[{index}]"),
        UnknownId::Unmatched { equation } => format!("<unmatched f_x[{equation}]>"),
    }
}

#[cfg(test)]
mod checked_tests {
    use rumoca_core::{
        ComprehensionScalarView, SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain,
        TypeId, VarName,
    };

    use super::*;

    fn at(source: rumoca_core::SourceId, start: usize, end: usize) -> dae::DaeProvenance {
        dae::DaeProvenance::source(Span::from_offsets(source, start, end)).unwrap()
    }

    #[test]
    fn scalar_structure_uses_branded_state_and_algebraic_identities() {
        let mut sources = SourceMap::new();
        let source = sources.add(
            "scalar_structure.mo",
            "Real x; Real y; equation der(x) - y = 0; y - x = 0;",
        );
        let x_at = at(source, 0, 6);
        let y_at = at(source, 8, 14);
        let first_at = at(source, 25, 39);
        let second_at = at(source, 41, 50);
        let model = dae::Dae::construct(sources, |model| {
            let real = model.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    x_at,
                )
            })?;
            let (x, y) = model.variables(|variables| {
                Ok((
                    variables.state(
                        VarName::new("x"),
                        real,
                        x_at,
                        dae::VariableAttributes::default(),
                    )?,
                    variables.algebraic(
                        VarName::new("y"),
                        real,
                        y_at,
                        dae::VariableAttributes::default(),
                    )?,
                ))
            })?;
            let (first, second) = model.expressions(|expressions| {
                let derivative = expressions
                    .at(first_at)
                    .coordinate(dae::CoordinateInput::Derivative(x))?;
                let y_first = expressions
                    .at(first_at)
                    .coordinate(dae::CoordinateInput::Algebraic(y))?;
                let first = expressions.at(first_at).binary(
                    dae::BinaryOperator::Subtract,
                    derivative,
                    y_first,
                )?;
                let y_second = expressions
                    .at(second_at)
                    .coordinate(dae::CoordinateInput::Algebraic(y))?;
                let x_current = expressions
                    .at(second_at)
                    .coordinate(dae::CoordinateInput::State(x))?;
                let second = expressions.at(second_at).binary(
                    dae::BinaryOperator::Subtract,
                    y_second,
                    x_current,
                )?;
                Ok((first, second))
            })?;
            model.continuous(|continuous| {
                continuous.equation(first_at, |equation| equation.residual(first))?;
                continuous.equation(second_at, |equation| equation.residual(second))?;
                Ok(())
            })
        })
        .unwrap();

        model.inspect(|view| {
            let sorted = sort(view).unwrap();
            assert_eq!(sorted.matching.len(), 2);
            assert!(sorted.matching.iter().any(|(_, unknown)| {
                matches!(unknown, UnknownId::Derivative { scalar: 0, .. })
            }));
            assert!(
                sorted.matching.iter().any(|(_, unknown)| {
                    matches!(unknown, UnknownId::Algebraic { scalar: 0, .. })
                })
            );
        });
    }

    #[test]
    fn row_major_array_family_stays_one_compact_structural_block() {
        let mut sources = SourceMap::new();
        let source = sources.add("array_structure.mo", "Real x[3]; equation x = {0,0,0};");
        let x_at = at(source, 0, 10);
        let equation_at = at(source, 20, 31);
        let model = dae::Dae::construct(sources, |model| {
            let array = model.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::array(dae::ScalarType::Real, [3]),
                    x_at,
                )
            })?;
            let x = model.variables(|variables| {
                variables.algebraic(
                    VarName::new("x"),
                    array,
                    x_at,
                    dae::VariableAttributes::default(),
                )
            })?;
            let domain = model.domains(|domains| {
                domains.structured(
                    StructuredIndexDomain {
                        binders: vec![StructuredIndexBinder {
                            id: 0,
                            display_name: "i".to_string(),
                            lower: 1,
                            upper: 3,
                            step: 1,
                        }],
                    },
                    equation_at,
                )
            })?;
            let x = model.expressions(|expressions| {
                expressions
                    .at(equation_at)
                    .coordinate(dae::CoordinateInput::Algebraic(x))
            })?;
            model.continuous(|continuous| {
                continuous.structured_family(
                    equation_at,
                    domain,
                    ComprehensionScalarView::RowMajorProjection,
                    |family| family.body(x),
                )?;
                Ok(())
            })
        })
        .unwrap();

        model.inspect(|view| {
            let sorted = sort(view).unwrap();
            assert_eq!(sorted.blocks.len(), 1);
            let BltBlock::StructuredScalar(block) = &sorted.blocks[0] else {
                panic!("checked affine family should remain compact");
            };
            assert_eq!(block.scalar_block_count(), 3);
        });
    }

    #[test]
    fn dynamic_structural_subscript_contributes_all_potential_incidence() {
        let mut sources = SourceMap::new();
        let source = sources.add(
            "dynamic_structure.mo",
            "Real x[3]; input Integer i; equation x[i] = 0;",
        );
        let x_at = at(source, 0, 10);
        let i_at = at(source, 11, 27);
        let equation_at = at(source, 38, 46);
        let model = dae::Dae::construct(sources, |model| {
            let array = model.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::array(dae::ScalarType::Real, [3]),
                    x_at,
                )
            })?;
            let integer = model.types(|types| {
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::scalar(dae::ScalarType::Integer),
                    i_at,
                )
            })?;
            let (x, i) = model.variables(|variables| {
                Ok((
                    variables.algebraic(
                        VarName::new("x"),
                        array,
                        x_at,
                        dae::VariableAttributes::default(),
                    )?,
                    variables.input(
                        VarName::new("i"),
                        integer,
                        dae::InputVariability::Discrete,
                        i_at,
                        dae::VariableAttributes::default(),
                    )?,
                ))
            })?;
            let indexed = model.expressions(|expressions| {
                let x = expressions
                    .at(equation_at)
                    .coordinate(dae::CoordinateInput::Algebraic(x))?;
                let i = expressions
                    .at(equation_at)
                    .coordinate(dae::CoordinateInput::Input(i))?;
                expressions.at(equation_at).index(
                    x,
                    [dae::Subscript::Index {
                        expression: i,
                        provenance: equation_at,
                    }],
                )
            })?;
            model.continuous(|continuous| {
                continuous.equation(equation_at, |equation| equation.residual(indexed))?;
                Ok(())
            })
        })
        .unwrap();

        model.inspect(|view| {
            let error = sort(view).unwrap_err();
            assert!(matches!(
                error,
                StructuralError::Singular {
                    n_equations: 1,
                    n_unknowns: 3,
                    n_matched: 1,
                    ref unmatched_unknowns,
                    ref unmatched_unknown_spans,
                    ..
                } if unmatched_unknowns == &["x[2]", "x[3]"]
                    && unmatched_unknown_spans == &[x_at.span(), x_at.span()]
            ));
        });
    }

    #[test]
    fn projection_failure_retains_the_exact_subscript_occurrence() {
        use rumoca_core::PhaseError;

        let text = "Real x[3]; equation x[4] = 0;";
        let mut sources = SourceMap::new();
        let source = sources.add("projection_occurrence.mo", text);
        let span_of = |snippet: &str| {
            let start = text.find(snippet).expect("fixture snippet exists");
            at(source, start, start + snippet.len())
        };
        let declaration_at = span_of("Real x[3]");
        let equation_at = span_of("x[4] = 0");
        let index_at = span_of("x[4]");
        let subscript_at = span_of("4");
        let model = dae::Dae::construct(sources, |model| {
            let array = model.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::array(dae::ScalarType::Real, [3]),
                    declaration_at,
                )
            })?;
            let x = model.variables(|variables| {
                variables.algebraic(
                    VarName::new("x"),
                    array,
                    declaration_at,
                    dae::VariableAttributes::default(),
                )
            })?;
            let indexed = model.expressions(|expressions| {
                let base = expressions
                    .at(index_at)
                    .coordinate(dae::CoordinateInput::Algebraic(x))?;
                let index = expressions
                    .at(subscript_at)
                    .literal(dae::DaeLiteral::Integer(4))?;
                expressions.at(index_at).index(
                    base,
                    [dae::Subscript::Index {
                        expression: index,
                        provenance: subscript_at,
                    }],
                )
            })?;
            model.continuous(|continuous| {
                continuous.equation(equation_at, |equation| equation.residual(indexed))?;
                Ok(())
            })
        })
        .expect("out-of-bounds runtime indexes remain a projection concern");

        model.inspect(|view| {
            let error = sort(view).expect_err("constant index exceeds the checked array extent");
            assert!(matches!(
                &error,
                StructuralError::Projection { span, .. } if *span == subscript_at.span()
            ));
            let diagnostic = error.to_diagnostic();
            assert_eq!(diagnostic.labels.len(), 1);
            assert_eq!(diagnostic.labels[0].span, subscript_at.span());
            assert_eq!(view.source_text(subscript_at), Some("4"));
            assert_ne!(subscript_at.span(), equation_at.span());
        });
    }
}
