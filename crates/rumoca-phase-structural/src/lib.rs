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
mod diagnostics;
pub mod eliminate;
pub mod ic_plan;
pub mod incidence;
mod matching;
pub mod projection_maps;
pub mod report;
pub mod runtime_defined;
pub mod scalarize;
mod tarjan;
pub mod tearing;
mod types;
mod variable_scope;

pub use diagnostics::{AlgebraicLoop, StructuralDiagnostics};
pub use eliminate::{EliminationResult, Substitution};
pub use ic_plan::{CausalStep, IcBlock, IcRelaxationHint, build_ic_plan, build_ic_relaxation_hint};
pub use incidence::{Incidence, build_solver_sparsity_triplets};
pub use report::{BlockReport, StructuralReport, TearingReport};
pub use runtime_defined::{
    runtime_defined_continuous_unknown_names, runtime_defined_unknown_names,
};
pub use tearing::{TearingResult, tear_algebraic_loop};
pub use types::{BltBlock, EquationRef, SortedDae, StructuralError, UnknownId};

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

    let (match_eq, match_var) =
        matching::maximum_matching(incidence.n_eq, incidence.n_var, &incidence.eq_unknowns);
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
) -> Result<RegularSubsystem, StructuralError> {
    let (match_eq, match_var) =
        matching::maximum_matching(incidence.n_eq, incidence.n_var, &incidence.eq_unknowns);
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
            incidence.eq_unknowns[*old_eq_idx]
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

    Ok(RegularSubsystem {
        incidence: Incidence::new(eq_unknowns, equation_refs, unknown_names),
        dropped_equations,
        dropped_unknowns,
    })
}

fn singular_from_matching(
    incidence: &Incidence,
    match_eq: &[Option<usize>],
    match_var: &[Option<usize>],
) -> StructuralError {
    StructuralError::Singular {
        n_equations: incidence.n_eq,
        n_unknowns: incidence.n_var,
        n_matched: match_eq.iter().filter(|m| m.is_some()).count(),
        unmatched_equations: match_eq
            .iter()
            .enumerate()
            .filter(|(_, m)| m.is_none())
            .map(|(i, _)| incidence.equation_refs[i].to_string())
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
            .map(|(i, _)| {
                incidence
                    .unknown_spans
                    .get(i)
                    .copied()
                    .unwrap_or(rumoca_core::Span::DUMMY)
            })
            .collect(),
    }
}

/// Transform a DAE into BLT-sorted block form for sequential simulation.
///
/// Returns `Err` if the system is structurally singular or empty.
pub fn sort_dae(dae: &dae::Dae) -> Result<SortedDae<'_>, StructuralError> {
    let inc = incidence::build_incidence(dae);

    if inc.n_eq == 0 && inc.n_var == 0 {
        return Err(StructuralError::EmptySystem);
    }

    let (match_eq, match_var) = matching::maximum_matching(inc.n_eq, inc.n_var, &inc.eq_unknowns);
    let matching_size = match_eq.iter().filter(|m| m.is_some()).count();

    if matching_size < inc.n_eq || matching_size < inc.n_var {
        return Err(singular_from_matching(&inc, &match_eq, &match_var));
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
        dae,
        blocks,
        matching: matching_pairs,
        diagnostics: diagnostics_warnings,
    })
}

/// Build a named structural report: the matching (which equation determines
/// which variable), the BLT blocks in evaluation order, the coupled SCCs
/// (algebraic loops), and the tearing of each coupled block. Backs the
/// `rumoca sim --structure` debug dump. Errors identically to [`sort_dae`] on
/// singular / empty systems.
pub fn build_structural_report(dae: &dae::Dae) -> Result<StructuralReport, StructuralError> {
    use std::collections::HashMap;

    let inc = incidence::build_incidence(dae);
    if inc.n_eq == 0 && inc.n_var == 0 {
        return Err(StructuralError::EmptySystem);
    }

    let (match_eq, match_var) = matching::maximum_matching(inc.n_eq, inc.n_var, &inc.eq_unknowns);
    let matching_size = match_eq.iter().filter(|m| m.is_some()).count();
    if matching_size < inc.n_eq || matching_size < inc.n_var {
        return Err(singular_from_matching(&inc, &match_eq, &match_var));
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
            inc.eq_unknowns[eq.0]
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

    let inc = incidence::build_incidence(dae);

    result.n_equations = inc.n_eq;
    result.n_unknowns = inc.n_var;

    if inc.n_eq == 0 && inc.n_var == 0 {
        return result;
    }

    let (match_eq, match_var) = matching::maximum_matching(inc.n_eq, inc.n_var, &inc.eq_unknowns);
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
        dae.variables
            .states
            .insert(x_name.clone(), dae::Variable::new(x_name.clone()));

        let span = Span::from_offsets(SourceId(0), 0, 10);
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
        dae.variables
            .algebraics
            .insert(y_name.clone(), dae::Variable::new(y_name.clone()));
        dae.variables
            .algebraics
            .insert(z_name.clone(), dae::Variable::new(z_name.clone()));

        let span = Span::from_offsets(SourceId(0), 0, 10);

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
        dae.variables
            .algebraics
            .insert(y_name.clone(), dae::Variable::new(y_name.clone()));
        dae.variables
            .algebraics
            .insert(z_name.clone(), dae::Variable::new(z_name.clone()));
        let span = Span::from_offsets(SourceId(0), 0, 10);
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
        dae.variables
            .algebraics
            .insert(y_name.clone(), dae::Variable::new(y_name.clone()));
        dae.variables
            .algebraics
            .insert(z_name.clone(), dae::Variable::new(z_name.clone()));

        let span = Span::from_offsets(SourceId(0), 0, 10);

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
        dae.variables
            .states
            .insert(x_name.clone(), dae::Variable::new(x_name.clone()));

        let span = Span::from_offsets(SourceId(0), 0, 10);
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
        dae.variables
            .algebraics
            .insert(y_name.clone(), dae::Variable::new(y_name.clone()));
        dae.variables
            .algebraics
            .insert(z_name.clone(), dae::Variable::new(z_name.clone()));

        let span = Span::from_offsets(SourceId(0), 0, 10);
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
}
