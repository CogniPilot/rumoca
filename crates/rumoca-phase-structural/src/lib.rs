//! Optional structural analysis phase for DAE systems.
//!
//! This phase is **not required** for CasADi targets (CasADi handles matching
//! and implicit systems internally). It provides diagnostic information about
//! the DAE structure and is required for Rust/C simulation code generation.
//!
//! Provides two entry points:
//! - [`sort_dae`]: Transform a DAE into BLT-sorted block form (errors on singular systems).
//! - [`analyze_structure`]: Diagnostic-only analysis (for CasADi workflows).

mod blt;
pub mod dae_prepare;
pub mod diagnostic_codes;
mod diagnostics;
pub mod eliminate;
mod function_arguments;
pub mod ic_plan;
pub mod incidence;
mod matching;
pub mod projection_maps;
pub mod report;
pub mod runtime_defined;
pub mod scalarize;
mod static_eval;
mod tarjan;
pub mod tearing;
mod types;
mod variable_scope;

pub use diagnostic_codes::STRUCTURAL_DIAGNOSTIC_CODES;
pub use diagnostics::{AlgebraicLoop, StructuralDiagnostics};
pub use eliminate::{EliminationResult, Substitution};
pub use ic_plan::{CausalStep, IcBlock, IcRelaxationHint, build_ic_plan, build_ic_relaxation_hint};
pub use incidence::{Incidence, build_solver_sparsity_triplets};
pub use report::{BlockReport, StructuralReport, TearingReport};
pub use runtime_defined::{
    runtime_defined_continuous_unknown_names, runtime_defined_unknown_names,
};
pub use tearing::{TearingResult, tear_algebraic_loop, tear_algebraic_loop_with_causal_candidates};
pub use types::{
    BltBlock, EquationRef, SingularBlockWitness, SortedDae, StructuralError, StructuredScalarBlock,
    UnknownId,
};

use rumoca_ir_dae as dae;

#[cfg(feature = "tracing")]
pub(crate) fn structural_trace_enabled() -> bool {
    tracing::enabled!(tracing::Level::DEBUG)
}

#[cfg(not(feature = "tracing"))]
pub(crate) fn structural_trace_enabled() -> bool {
    false
}

#[cfg(feature = "tracing")]
macro_rules! structural_trace {
    ($($arg:tt)*) => {
        tracing::debug!(target: "rumoca_phase_structural", $($arg)*)
    };
}

#[cfg(not(feature = "tracing"))]
macro_rules! structural_trace {
    ($($arg:tt)*) => {
        let _ = format_args!($($arg)*);
    };
}

pub(crate) use structural_trace;

/// Build BLT blocks from a raw incidence matrix.
///
/// Used by the IC solver to decompose arbitrary subsystems (e.g. algebraic-only).
/// Wraps: maximum matching → dependency graph → Tarjan SCC → BLT blocks.
///
/// Returns `Err` if the incidence is structurally singular.
pub fn build_blt_from_incidence(incidence: &Incidence) -> Result<Vec<BltBlock>, StructuralError> {
    if incidence.n_eq == 0 && incidence.n_var == 0 {
        return Ok(Vec::new());
    }

    let (match_eq, match_var) = maximum_matching_for_incidence(incidence, &[]);
    let matching_size = match_eq.iter().filter(|m| m.is_some()).count();

    if matching_size < incidence.n_eq || matching_size < incidence.n_var {
        return Err(singular_from_matching(incidence, &match_eq, &match_var));
    }

    let adj = incidence::build_dependency_graph(&incidence.eq_unknowns, &match_var, incidence.n_eq);
    let blocks = blt::build_blt_blocks(incidence, &match_eq, &adj);

    Ok(blocks)
}

/// A maximum square subsystem selected from a rectangular or singular
/// incidence matrix.
#[derive(Debug)]
pub struct RegularSubsystem {
    pub incidence: Incidence,
    pub blocks: Vec<BltBlock>,
    pub dropped_equations: Vec<EquationRef>,
    pub dropped_unknowns: Vec<UnknownId>,
}

/// Select the largest structurally regular square subsystem supported by the
/// incidence matrix's maximum matching.
///
/// Initialization projection can contain redundant rows and unconstrained
/// variables (for example visualization aliases). This function exposes the
/// structurally regular core explicitly so callers can solve the meaningful
/// subsystem without relying on string sentinels or unmatched `???` BLT blocks.
pub fn maximum_regular_subsystem(
    incidence: &Incidence,
    preferred_unknowns: &[Option<usize>],
) -> Result<RegularSubsystem, StructuralError> {
    let (match_eq, match_var) = maximum_matching_for_incidence(incidence, preferred_unknowns);
    let matched_equations = match_eq
        .iter()
        .enumerate()
        .filter_map(|(idx, matched)| matched.is_some().then_some(idx))
        .collect::<Vec<_>>();
    let matched_unknowns = match_var
        .iter()
        .enumerate()
        .filter_map(|(idx, matched)| matched.is_some().then_some(idx))
        .collect::<Vec<_>>();
    if matched_equations.is_empty() || matched_equations.len() != matched_unknowns.len() {
        return Err(singular_from_matching(incidence, &match_eq, &match_var));
    }

    let mut old_to_new_var = vec![None; incidence.n_var];
    for (new_idx, old_idx) in matched_unknowns.iter().copied().enumerate() {
        old_to_new_var[old_idx] = Some(new_idx);
    }

    let eq_unknowns = matched_equations
        .iter()
        .map(|old_eq_idx| {
            incidence
                .eq_unknowns
                .row(*old_eq_idx)
                .iter()
                .filter_map(|old_var_idx| old_to_new_var[*old_var_idx])
                .collect()
        })
        .collect::<Vec<_>>();
    let equation_refs = matched_equations
        .iter()
        .map(|old_eq_idx| incidence.equation_refs[*old_eq_idx].clone())
        .collect::<Vec<_>>();
    let unknown_names = matched_unknowns
        .iter()
        .map(|old_var_idx| incidence.unknown_names[*old_var_idx].clone())
        .collect::<Vec<_>>();

    let matched_eq_set = matched_equations
        .iter()
        .copied()
        .collect::<std::collections::HashSet<_>>();
    let matched_var_set = matched_unknowns
        .iter()
        .copied()
        .collect::<std::collections::HashSet<_>>();
    let dropped_equations = incidence
        .equation_refs
        .iter()
        .enumerate()
        .filter_map(|(idx, eq)| (!matched_eq_set.contains(&idx)).then_some(eq.clone()))
        .collect::<Vec<_>>();
    let dropped_unknowns = incidence
        .unknown_names
        .iter()
        .enumerate()
        .filter_map(|(idx, unknown)| (!matched_var_set.contains(&idx)).then_some(unknown.clone()))
        .collect::<Vec<_>>();

    let regular_incidence = Incidence::new(eq_unknowns, equation_refs, unknown_names);
    let regular_match_eq = matched_equations
        .iter()
        .map(|old_eq_idx| match_eq[*old_eq_idx].and_then(|old_var_idx| old_to_new_var[old_var_idx]))
        .collect::<Vec<_>>();
    let regular_match_var = matching_inverse(&regular_match_eq, regular_incidence.n_var);
    let adjacency = incidence::build_dependency_graph(
        &regular_incidence.eq_unknowns,
        &regular_match_var,
        regular_incidence.n_eq,
    );
    let blocks = blt::build_blt_blocks(&regular_incidence, &regular_match_eq, &adjacency);

    Ok(RegularSubsystem {
        incidence: regular_incidence,
        blocks,
        dropped_equations,
        dropped_unknowns,
    })
}

fn matching_inverse(match_eq: &[Option<usize>], n_var: usize) -> Vec<Option<usize>> {
    let mut match_var = vec![None; n_var];
    for (eq_idx, var_idx) in match_eq.iter().copied().enumerate() {
        if let Some(var_idx) = var_idx {
            match_var[var_idx] = Some(eq_idx);
        }
    }
    match_var
}

fn maximum_matching_for_incidence(
    incidence: &Incidence,
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

fn singular_from_matching(
    incidence: &Incidence,
    match_eq: &[Option<usize>],
    match_var: &[Option<usize>],
) -> StructuralError {
    singular_from_matching_labeled(incidence, match_eq, match_var, EquationRef::to_string)
}

/// [`singular_from_matching`] with a caller-supplied equation label.
///
/// Callers that hold the DAE name the unmatched rows by their source origin
/// (`f_x[530] (b2.frame_a.f = ...)`) instead of by slot alone, so a structural
/// singularity is traceable to the equation that produced it without a second
/// compile. Callers that only hold an incidence matrix (IC / Solve projection
/// subsystems) keep the slot-only label, which is the only honest name they
/// have.
fn singular_from_matching_labeled(
    incidence: &Incidence,
    match_eq: &[Option<usize>],
    match_var: &[Option<usize>],
    label: impl Fn(&EquationRef) -> String,
) -> StructuralError {
    StructuralError::Singular {
        n_equations: incidence.n_eq,
        n_unknowns: incidence.n_var,
        n_matched: match_eq.iter().filter(|m| m.is_some()).count(),
        unmatched_equations: match_eq
            .iter()
            .enumerate()
            .filter(|(_, m)| m.is_none())
            .map(|(i, _)| label(&incidence.equation_refs[i]))
            .collect(),
        unmatched_unknowns: match_var
            .iter()
            .enumerate()
            .filter(|(_, m)| m.is_none())
            .map(|(i, _)| incidence.unknown_names[i].to_string())
            .collect(),
        unmatched_unknown_spans: match_var
            .iter()
            .enumerate()
            .filter(|(_, m)| m.is_none())
            .map(|(i, _)| incidence.unknown_spans.get(i).copied().flatten())
            .collect(),
        over_determined_block: Box::new(over_determined_block(
            incidence, match_eq, match_var, &label,
        )),
    }
}

/// Maximum equation labels carried in a [`SingularBlockWitness`].
///
/// The witness exists to name the redundant constraint set, not to dump it: a
/// deficient block in an MSL multibody loop can span hundreds of rows, and the
/// block's *size* already says whether the deficiency is local or global.
const SINGULAR_BLOCK_SAMPLE_LIMIT: usize = 24;

/// The Dulmage-Mendelsohn over-determined block around the unmatched rows.
///
/// Alternating-path reachability from every unmatched equation: an unmatched
/// row reaches its unknowns, each reached unknown reaches the equation it is
/// matched to, and so on. Because the matching is maximum, every unknown
/// reached this way is matched, so the reached equations outnumber the reached
/// unknowns by exactly the number of unmatched rows in the block — that
/// difference is the redundancy the modeller has to remove.
fn over_determined_block(
    incidence: &Incidence,
    match_eq: &[Option<usize>],
    match_var: &[Option<usize>],
    label: impl Fn(&EquationRef) -> String,
) -> types::SingularBlockWitness {
    let mut seen_equation = vec![false; incidence.n_eq];
    let mut seen_unknown = vec![false; incidence.n_var];
    let mut queue: Vec<usize> = match_eq
        .iter()
        .enumerate()
        .filter_map(|(equation, matched)| matched.is_none().then_some(equation))
        .collect();
    for &equation in &queue {
        if let Some(flag) = seen_equation.get_mut(equation) {
            *flag = true;
        }
    }
    let mut unknowns = 0usize;
    while let Some(equation) = queue.pop() {
        for &unknown in incidence.eq_unknowns.row(equation) {
            let Some(flag) = seen_unknown.get_mut(unknown) else {
                continue;
            };
            if *flag {
                continue;
            }
            *flag = true;
            unknowns += 1;
            let Some(Some(owner)) = match_var.get(unknown).copied() else {
                continue;
            };
            let Some(owner_flag) = seen_equation.get_mut(owner) else {
                continue;
            };
            if !*owner_flag {
                *owner_flag = true;
                queue.push(owner);
            }
        }
    }
    let equations = seen_equation.iter().filter(|seen| **seen).count();
    let sample = seen_equation
        .iter()
        .enumerate()
        .filter_map(|(equation, seen)| seen.then_some(equation))
        .take(SINGULAR_BLOCK_SAMPLE_LIMIT)
        .filter_map(|equation| incidence.equation_refs.get(equation))
        .map(&label)
        .collect();
    types::SingularBlockWitness {
        equations,
        unknowns,
        sample,
    }
}

/// Transform a DAE into BLT-sorted block form for sequential simulation.
///
/// Returns `Err` if the system is structurally singular or empty.
pub fn sort_dae(dae: &dae::Dae) -> Result<SortedDae, StructuralError> {
    let inc = incidence::build_incidence(dae)?;

    if inc.n_eq == 0 && inc.n_var == 0 {
        return Err(StructuralError::EmptySystem);
    }

    let (match_eq, match_var) = maximum_matching_for_incidence(&inc, &[]);
    let matching_size = match_eq.iter().filter(|m| m.is_some()).count();

    if matching_size < inc.n_eq || matching_size < inc.n_var {
        return Err(singular_from_matching_labeled(
            &inc,
            &match_eq,
            &match_var,
            |equation| equation_label(dae, equation),
        ));
    }

    let adj = incidence::build_dependency_graph(&inc.eq_unknowns, &match_var, inc.n_eq);

    let equations: Vec<_> = dae.continuous.equations.iter().collect();

    let diagnostics_warnings = diagnostics::collect_warnings(&inc, &match_eq, &adj, &equations);
    let blocks = blt::build_blt_blocks(&inc, &match_eq, &adj);

    let matching_pairs: Vec<(EquationRef, UnknownId)> = match_eq
        .iter()
        .enumerate()
        .filter_map(|(eq_idx, var_idx)| {
            var_idx.map(|v| {
                (
                    inc.equation_refs[eq_idx].clone(),
                    inc.unknown_names[v].clone(),
                )
            })
        })
        .collect();

    Ok(SortedDae {
        blocks,
        matching: matching_pairs,
        diagnostics: diagnostics_warnings,
    })
}

pub(crate) fn finish_dae(dae: dae::Dae) -> Result<dae::Dae, StructuralError> {
    Ok(dae)
}

/// Build a named structural report: the matching (which equation determines
/// which variable), the BLT blocks in evaluation order, the coupled SCCs
/// (algebraic loops), and the tearing of each coupled block. Backs the
/// `rumoca sim --structure` debug dump. Errors identically to [`sort_dae`] on
/// singular / empty systems.
pub fn build_structural_report(dae: &dae::Dae) -> Result<StructuralReport, StructuralError> {
    use std::collections::HashMap;

    let inc = incidence::build_incidence(dae)?;
    if inc.n_eq == 0 && inc.n_var == 0 {
        return Err(StructuralError::EmptySystem);
    }

    let (match_eq, match_var) = maximum_matching_for_incidence(&inc, &[]);
    let matching_size = match_eq.iter().filter(|m| m.is_some()).count();
    if matching_size < inc.n_eq || matching_size < inc.n_var {
        return Err(singular_from_matching_labeled(
            &inc,
            &match_eq,
            &match_var,
            |equation| equation_label(dae, equation),
        ));
    }

    let adj = incidence::build_dependency_graph(&inc.eq_unknowns, &match_var, inc.n_eq);
    let blocks_raw = blt::build_blt_blocks(&inc, &match_eq, &adj);

    let matching = match_eq
        .iter()
        .enumerate()
        .filter_map(|(eq_idx, var_idx)| {
            var_idx.map(|v| {
                (
                    equation_label(dae, &inc.equation_refs[eq_idx]),
                    inc.unknown_names[v].to_string(),
                )
            })
        })
        .collect();

    // Map each unknown to its global incidence index, for per-loop tearing.
    let unknown_index: HashMap<&UnknownId, usize> = inc
        .unknown_names
        .iter()
        .enumerate()
        .map(|(idx, name)| (name, idx))
        .collect();

    let blocks = blocks_raw
        .iter()
        .map(|block| match block {
            BltBlock::Scalar { equation, unknown } => BlockReport::Scalar {
                equation: equation_label(dae, equation),
                unknown: unknown.to_string(),
            },
            BltBlock::AlgebraicLoop {
                equations,
                unknowns,
            } => BlockReport::Coupled {
                equations: equations.iter().map(|eq| equation_label(dae, eq)).collect(),
                unknowns: unknowns.iter().map(UnknownId::to_string).collect(),
                tearing: tear_loop(dae, &inc, equations, unknowns, &unknown_index),
            },
            // Reported compactly: expanding a 2048-element family into 2048
            // report lines would bury the rest of the structural dump.
            BltBlock::StructuredScalar(structured) => BlockReport::StructuredScalar {
                origin: equation_label(dae, &EquationRef(structured.first_equation_index)),
                point_count: structured.point_count,
                equations_per_point: structured.equations_per_point,
            },
        })
        .collect();

    Ok(StructuralReport {
        n_equations: inc.n_eq,
        n_unknowns: inc.n_var,
        matching,
        blocks,
    })
}

/// Tear a single coupled block, mapping the local tearing indices back to names.
fn tear_loop(
    dae: &dae::Dae,
    inc: &Incidence,
    equations: &[EquationRef],
    unknowns: &[UnknownId],
    unknown_index: &std::collections::HashMap<&UnknownId, usize>,
) -> Option<report::TearingReport> {
    // Local var index for each block unknown (by its global incidence index).
    let var_local: std::collections::HashMap<usize, usize> = unknowns
        .iter()
        .enumerate()
        .filter_map(|(local, name)| unknown_index.get(name).map(|&global| (global, local)))
        .collect();

    // Restrict the global incidence to this block: per block-equation, the set
    // of block-local unknown indices it touches.
    let local_eq_unknowns: Vec<std::collections::HashSet<usize>> = equations
        .iter()
        .map(|eq| {
            inc.eq_unknowns
                .row(eq.0)
                .iter()
                .filter_map(|global_var| var_local.get(global_var).copied())
                .collect()
        })
        .collect();

    let result = tear_algebraic_loop(unknowns.len(), &local_eq_unknowns)?;
    Some(report::TearingReport {
        tear_vars: result
            .tear_var_local_indices
            .iter()
            .map(|&local| unknowns[local].to_string())
            .collect(),
        residual_equations: result
            .residual_eq_local_indices
            .iter()
            .map(|&local| equation_label(dae, &equations[local]))
            .collect(),
        causal_sequence: result
            .causal_sequence
            .iter()
            .map(|&(eq_local, var_local)| {
                (
                    equation_label(dae, &equations[eq_local]),
                    unknowns[var_local].to_string(),
                )
            })
            .collect(),
    })
}

/// Human label for a continuous equation: its `f_x` slot plus origin (when the
/// origin carries useful context).
fn equation_label(dae: &dae::Dae, equation: &EquationRef) -> String {
    match dae.continuous.equations.get(equation.0) {
        Some(eq) if !eq.origin.trim().is_empty() => {
            format!("{equation} ({})", eq.origin.trim())
        }
        _ => equation.to_string(),
    }
}

/// Perform diagnostic-only structural analysis on a DAE system.
///
/// Builds the incidence matrix, computes maximum matching, detects
/// structural singularity and algebraic loops. Returns diagnostics
/// as warnings (these don't prevent compilation).
pub fn analyze_structure(dae: &dae::Dae) -> StructuralDiagnostics {
    let mut result = StructuralDiagnostics::default();

    let inc = match incidence::build_incidence(dae) {
        Ok(incidence) => incidence,
        Err(error) => {
            let diagnostic = match error.source_span() {
                Some(span) => rumoca_core::Diagnostic::warning(
                    error.code(),
                    error.to_string(),
                    rumoca_core::PrimaryLabel::new(span)
                        .with_message("invalid compact structural family"),
                ),
                None => rumoca_core::Diagnostic::global_warning(error.code(), error.to_string()),
            };
            result.diagnostics.push(diagnostic);
            return result;
        }
    };

    result.n_equations = inc.n_eq;
    result.n_unknowns = inc.n_var;

    if inc.n_eq == 0 && inc.n_var == 0 {
        return result;
    }

    let (match_eq, match_var) = maximum_matching_for_incidence(&inc, &[]);
    let matching_size = match_eq.iter().filter(|m| m.is_some()).count();
    result.matching_size = matching_size;

    let equations: Vec<_> = dae.continuous.equations.iter().collect();

    let ctx = diagnostics::MatchingContext {
        equations: &equations,
        unknown_names: &inc.unknown_names,
        eq_unknowns: &inc.eq_unknowns,
        match_eq: &match_eq,
        match_var: &match_var,
    };

    if matching_size < inc.n_eq || matching_size < inc.n_var {
        ctx.check_singularity(&mut result, inc.n_eq, inc.n_var, matching_size);
    }

    if matching_size > 0 {
        ctx.detect_algebraic_loops(&mut result, inc.n_eq);
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{SourceId, Span};
    use rumoca_ir_dae as dae;

    #[test]
    fn test_analyze_empty_dae() {
        let dae = dae::Dae::new();
        let result = analyze_structure(&dae);
        assert!(result.diagnostics.is_empty(), "empty DAE has no issues");
        assert_eq!(result.n_equations, 0);
        assert_eq!(result.n_unknowns, 0);
    }

    #[test]
    fn test_analyze_simple_ode() {
        let mut dae = dae::Dae::new();

        let x_name = rumoca_core::VarName::from("x");
        dae.variables.states.insert(
            x_name.clone(),
            dae::Variable::new(
                x_name.clone(),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );

        let span = Span::from_offsets(
            SourceId::from_source_name("phase_structural_lib_source_0.mo"),
            0,
            10,
        );
        let der_x = rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args: vec![rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::from_var_name(x_name.clone()),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }],
            span: rumoca_core::Span::DUMMY,
        };
        let one = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        };
        let residual = rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(der_x),
            rhs: Box::new(one),
            span: rumoca_core::Span::DUMMY,
        };
        dae.continuous
            .equations
            .push(dae::Equation::residual(residual, span, "der(x) = 1.0"));

        let result = analyze_structure(&dae);
        assert_eq!(result.n_equations, 1);
        assert_eq!(result.n_unknowns, 1);
        assert_eq!(result.matching_size, 1, "perfect matching for simple ODE");
        assert!(
            result.diagnostics.is_empty(),
            "simple ODE should have no warnings"
        );
        assert!(result.algebraic_loops.is_empty(), "no algebraic loops");
    }

    #[test]
    fn test_analyze_algebraic_loop() {
        let mut dae = dae::Dae::new();

        let y_name = rumoca_core::VarName::from("y");
        let z_name = rumoca_core::VarName::from("z");
        dae.variables.algebraics.insert(
            y_name.clone(),
            dae::Variable::new(
                y_name.clone(),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
        dae.variables.algebraics.insert(
            z_name.clone(),
            dae::Variable::new(
                z_name.clone(),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );

        let span = Span::from_offsets(
            SourceId::from_source_name("phase_structural_lib_source_0.mo"),
            0,
            10,
        );

        let y_ref = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_var_name(y_name.clone()),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        };
        let z_ref = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_var_name(z_name.clone()),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        };
        let two_z = rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.0),
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(z_ref.clone()),
            span: rumoca_core::Span::DUMMY,
        };
        let eq1 = rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(y_ref.clone()),
            rhs: Box::new(two_z),
            span: rumoca_core::Span::DUMMY,
        };
        dae.continuous
            .equations
            .push(dae::Equation::residual(eq1, span, "y = 2*z"));

        let three_y = rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(3.0),
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(y_ref),
            span: rumoca_core::Span::DUMMY,
        };
        let eq2 = rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(z_ref),
            rhs: Box::new(three_y),
            span: rumoca_core::Span::DUMMY,
        };
        dae.continuous
            .equations
            .push(dae::Equation::residual(eq2, span, "z = 3*y"));

        let result = analyze_structure(&dae);
        assert_eq!(result.matching_size, 2, "should find perfect matching");
        assert_eq!(
            result.algebraic_loops.len(),
            1,
            "should detect one algebraic loop"
        );
        assert_eq!(
            result.algebraic_loops[0].unknown_names.len(),
            2,
            "loop involves 2 unknowns"
        );
    }

    #[test]
    fn build_structural_report_names_coupled_block_and_tearing() {
        // Coupled 2x2 algebraic loop: y = 2*z, z = 3*y.
        let mut dae = dae::Dae::new();
        let y_name = rumoca_core::VarName::from("y");
        let z_name = rumoca_core::VarName::from("z");
        dae.variables.algebraics.insert(
            y_name.clone(),
            dae::Variable::new(
                y_name.clone(),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
        dae.variables.algebraics.insert(
            z_name.clone(),
            dae::Variable::new(
                z_name.clone(),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
        let span = Span::from_offsets(
            SourceId::from_source_name("phase_structural_lib_source_0.mo"),
            0,
            10,
        );
        let y_ref = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_var_name(y_name),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        };
        let z_ref = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_var_name(z_name),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        };
        let scale = |k: f64, inner: rumoca_core::Expression| rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(k),
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(inner),
            span: rumoca_core::Span::DUMMY,
        };
        let sub = |lhs, rhs| rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: rumoca_core::Span::DUMMY,
        };
        dae.continuous.equations.push(dae::Equation::residual(
            sub(y_ref.clone(), scale(2.0, z_ref.clone())),
            span,
            "y = 2*z",
        ));
        dae.continuous.equations.push(dae::Equation::residual(
            sub(z_ref, scale(3.0, y_ref)),
            span,
            "z = 3*y",
        ));

        let report = build_structural_report(&dae).expect("report should build");
        assert_eq!(report.n_equations, 2);
        assert_eq!(report.matching.len(), 2);
        assert_eq!(
            report.coupled_block_count(),
            1,
            "y/z form one coupled block"
        );
        assert_eq!(report.largest_coupled_block(), 2);

        let BlockReport::Coupled {
            unknowns, tearing, ..
        } = &report.blocks[0]
        else {
            panic!("first block should be coupled: {:?}", report.blocks);
        };
        assert_eq!(unknowns.len(), 2);
        assert!(unknowns.iter().any(|name| name == "y"));
        assert!(unknowns.iter().any(|name| name == "z"));
        let tearing = tearing.as_ref().expect("loop should tear");
        assert_eq!(
            tearing.tear_vars.len(),
            1,
            "2x2 loop tears to 1 iteration var"
        );
        assert_eq!(tearing.residual_equations.len(), 1);
        // Rendered report should mention the coupled block.
        assert!(report.to_string().contains("coupled"));
    }

    #[test]
    fn test_analyze_singular_system() {
        let mut dae = dae::Dae::new();

        let y_name = rumoca_core::VarName::from("y");
        let z_name = rumoca_core::VarName::from("z");
        dae.variables.algebraics.insert(
            y_name.clone(),
            dae::Variable::new(
                y_name.clone(),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
        dae.variables.algebraics.insert(
            z_name.clone(),
            dae::Variable::new(
                z_name.clone(),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );

        let span = Span::from_offsets(
            SourceId::from_source_name("phase_structural_lib_source_0.mo"),
            0,
            10,
        );

        let y_ref = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_var_name(y_name.clone()),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        };
        let eq1 = rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(y_ref.clone()),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        };
        let eq2 = rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(y_ref),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.0),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        };
        dae.continuous
            .equations
            .push(dae::Equation::residual(eq1, span, "y = 1"));
        dae.continuous
            .equations
            .push(dae::Equation::residual(eq2, span, "y = 2"));

        let result = analyze_structure(&dae);
        assert_eq!(result.matching_size, 1, "only one variable can be matched");
        assert!(!result.diagnostics.is_empty(), "should report singularity");
        assert_eq!(result.unmatched_unknowns.len(), 1, "z is unmatched");
        assert!(
            result.unmatched_unknowns[0].contains('z'),
            "unmatched unknown should be z"
        );
    }

    #[test]
    fn test_sort_dae_empty() {
        let dae = dae::Dae::new();
        let result = sort_dae(&dae);
        assert!(matches!(result, Err(StructuralError::EmptySystem)));
    }

    #[test]
    fn test_sort_dae_simple_ode() {
        let mut dae = dae::Dae::new();

        let x_name = rumoca_core::VarName::from("x");
        dae.variables.states.insert(
            x_name.clone(),
            dae::Variable::new(
                x_name.clone(),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );

        let span = Span::from_offsets(
            SourceId::from_source_name("phase_structural_lib_source_0.mo"),
            0,
            10,
        );
        let der_x = rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args: vec![rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::from_var_name(x_name.clone()),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }],
            span: rumoca_core::Span::DUMMY,
        };
        let one = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        };
        let residual = rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(der_x),
            rhs: Box::new(one),
            span: rumoca_core::Span::DUMMY,
        };
        dae.continuous
            .equations
            .push(dae::Equation::residual(residual, span, "der(x) = 1.0"));

        let sorted = sort_dae(&dae).expect("should succeed");
        assert_eq!(sorted.blocks.len(), 1, "one scalar block");
        assert!(matches!(&sorted.blocks[0], BltBlock::Scalar { .. }));
        assert_eq!(sorted.matching.len(), 1);
        assert!(sorted.diagnostics.is_empty());
    }

    #[test]
    fn test_sort_dae_singular() {
        let mut dae = dae::Dae::new();

        let y_name = rumoca_core::VarName::from("y");
        let z_name = rumoca_core::VarName::from("z");
        dae.variables.algebraics.insert(
            y_name.clone(),
            dae::Variable::new(
                y_name.clone(),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
        dae.variables.algebraics.insert(
            z_name.clone(),
            dae::Variable::new(
                z_name.clone(),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );

        let span = Span::from_offsets(
            SourceId::from_source_name("phase_structural_lib_source_0.mo"),
            0,
            10,
        );
        let y_ref = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_var_name(y_name.clone()),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        };
        let eq1 = rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(y_ref.clone()),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        };
        let eq2 = rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(y_ref),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.0),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        };
        dae.continuous
            .equations
            .push(dae::Equation::residual(eq1, span, "y = 1"));
        dae.continuous
            .equations
            .push(dae::Equation::residual(eq2, span, "y = 2"));

        let result = sort_dae(&dae);
        assert!(matches!(result, Err(StructuralError::Singular { .. })));
    }

    /// A singular sort must name the redundant constraint set, not just the
    /// arbitrary row the matching happened to leave over.
    ///
    /// `y = 1` and `y = 2` over-determine `y`: two equations reach one unknown,
    /// so the Dulmage-Mendelsohn block is 2 equations over 1 unknown and both
    /// equations are named by origin. Without the block the message reports one
    /// unmatched row whose identity depends on matching order, which points at
    /// neither of the two rows that actually conflict.
    #[test]
    fn singular_sort_reports_the_over_determined_block_by_origin() {
        let mut dae = dae::Dae::new();
        let y_name = rumoca_core::VarName::from("y");
        dae.variables.algebraics.insert(
            y_name.clone(),
            dae::Variable::new(
                y_name.clone(),
                Span::from_offsets(SourceId::from_source_name(file!()), 1, 2),
            ),
        );
        let span = Span::from_offsets(
            SourceId::from_source_name("phase_structural_lib_witness.mo"),
            0,
            10,
        );
        let y_ref = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_var_name(y_name),
            subscripts: vec![],
            span,
        };
        let residual = |value: f64| rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(y_ref.clone()),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(value),
                span,
            }),
            span,
        };
        dae.continuous
            .equations
            .push(dae::Equation::residual(residual(1.0), span, "y = 1"));
        dae.continuous
            .equations
            .push(dae::Equation::residual(residual(2.0), span, "y = 2"));

        let Err(StructuralError::Singular {
            over_determined_block,
            ..
        }) = sort_dae(&dae)
        else {
            panic!("two equations for one unknown must sort as singular");
        };
        assert_eq!(over_determined_block.equations, 2);
        assert_eq!(over_determined_block.unknowns, 1);
        assert_eq!(
            over_determined_block.sample,
            vec!["f_x[0] (y = 1)".to_string(), "f_x[1] (y = 2)".to_string()],
            "the block must name both conflicting rows by origin"
        );
        let rendered = StructuralError::Singular {
            n_equations: 2,
            n_unknowns: 1,
            n_matched: 1,
            unmatched_equations: vec!["f_x[1] (y = 2)".to_string()],
            unmatched_unknowns: Vec::new(),
            unmatched_unknown_spans: Vec::new(),
            over_determined_block,
        }
        .to_string();
        assert!(
            rendered.contains("over-determined block: 2 equations over 1 unknowns"),
            "the block must reach the rendered diagnostic: {rendered}"
        );
    }
}
