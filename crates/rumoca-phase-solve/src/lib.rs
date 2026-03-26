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
mod diagnostics;
pub mod eliminate;
mod fold_start_values;
pub mod ic_plan;
pub mod incidence;
mod matching;
mod tarjan;
pub mod tearing;
mod types;

pub use diagnostics::{AlgebraicLoop, StructuralDiagnostics};
pub use fold_start_values::{
    fold_start_values_to_literals, sort_algebraics_by_equation_deps, sort_parameters_by_start_deps,
};
pub use eliminate::{EliminationResult, Substitution};
pub use ic_plan::{CausalStep, IcBlock, IcRelaxationHint, build_ic_plan, build_ic_relaxation_hint};
pub use incidence::{Incidence, build_solver_sparsity_triplets};
pub use tearing::{TearingResult, tear_algebraic_loop};
pub use types::{BltBlock, EquationRef, SortedDae, StructuralError, UnknownId};

use rumoca_ir_dae as dae;

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

    if matching_size < incidence.n_eq.min(incidence.n_var) {
        let unmatched_equations: Vec<String> = match_eq
            .iter()
            .enumerate()
            .filter(|(_, m)| m.is_none())
            .map(|(i, _)| incidence.equation_refs[i].to_string())
            .collect();
        let unmatched_unknowns: Vec<String> = match_var
            .iter()
            .enumerate()
            .filter(|(_, m)| m.is_none())
            .map(|(i, _)| incidence.unknown_names[i].to_string())
            .collect();

        return Err(StructuralError::Singular {
            n_equations: incidence.n_eq,
            n_unknowns: incidence.n_var,
            n_matched: matching_size,
            unmatched_equations,
            unmatched_unknowns,
        });
    }

    let adj = incidence::build_dependency_graph(&incidence.eq_unknowns, &match_var, incidence.n_eq);
    let blocks = blt::build_blt_blocks(incidence, &match_eq, &adj);

    Ok(blocks)
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

    if matching_size < inc.n_eq.min(inc.n_var) {
        let unmatched_equations: Vec<String> = match_eq
            .iter()
            .enumerate()
            .filter(|(_, m)| m.is_none())
            .map(|(i, _)| inc.equation_refs[i].to_string())
            .collect();
        let unmatched_unknowns: Vec<String> = match_var
            .iter()
            .enumerate()
            .filter(|(_, m)| m.is_none())
            .map(|(i, _)| inc.unknown_names[i].to_string())
            .collect();

        return Err(StructuralError::Singular {
            n_equations: inc.n_eq,
            n_unknowns: inc.n_var,
            n_matched: matching_size,
            unmatched_equations,
            unmatched_unknowns,
        });
    }

    let adj = incidence::build_dependency_graph(&inc.eq_unknowns, &match_var, inc.n_eq);

    let equations: Vec<_> = dae.f_x.iter().collect();

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

    let equations: Vec<_> = dae.f_x.iter().collect();

    let ctx = diagnostics::MatchingContext {
        equations: &equations,
        unknown_names: &inc.unknown_names,
        eq_unknowns: &inc.eq_unknowns,
        match_eq: &match_eq,
        match_var: &match_var,
    };

    if matching_size < inc.n_eq.min(inc.n_var) {
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

        let x_name = dae::VarName::from("x");
        dae.states
            .insert(x_name.clone(), dae::Variable::new(x_name.clone()));

        let span = Span::from_offsets(SourceId(0), 0, 10);
        let der_x = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![dae::Expression::VarRef {
                name: x_name.clone(),
                subscripts: vec![],
            }],
        };
        let one = dae::Expression::Literal(dae::Literal::Real(1.0));
        let residual = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(rumoca_ir_core::Token::default()),
            lhs: Box::new(der_x),
            rhs: Box::new(one),
        };
        dae.f_x
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

        let y_name = dae::VarName::from("y");
        let z_name = dae::VarName::from("z");
        dae.algebraics
            .insert(y_name.clone(), dae::Variable::new(y_name.clone()));
        dae.algebraics
            .insert(z_name.clone(), dae::Variable::new(z_name.clone()));

        let span = Span::from_offsets(SourceId(0), 0, 10);

        let y_ref = dae::Expression::VarRef {
            name: y_name.clone(),
            subscripts: vec![],
        };
        let z_ref = dae::Expression::VarRef {
            name: z_name.clone(),
            subscripts: vec![],
        };
        let two_z = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Mul(rumoca_ir_core::Token::default()),
            lhs: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
            rhs: Box::new(z_ref.clone()),
        };
        let eq1 = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(rumoca_ir_core::Token::default()),
            lhs: Box::new(y_ref.clone()),
            rhs: Box::new(two_z),
        };
        dae.f_x.push(dae::Equation::residual(eq1, span, "y = 2*z"));

        let three_y = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Mul(rumoca_ir_core::Token::default()),
            lhs: Box::new(dae::Expression::Literal(dae::Literal::Real(3.0))),
            rhs: Box::new(y_ref),
        };
        let eq2 = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(rumoca_ir_core::Token::default()),
            lhs: Box::new(z_ref),
            rhs: Box::new(three_y),
        };
        dae.f_x.push(dae::Equation::residual(eq2, span, "z = 3*y"));

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
    fn test_analyze_singular_system() {
        let mut dae = dae::Dae::new();

        let y_name = dae::VarName::from("y");
        let z_name = dae::VarName::from("z");
        dae.algebraics
            .insert(y_name.clone(), dae::Variable::new(y_name.clone()));
        dae.algebraics
            .insert(z_name.clone(), dae::Variable::new(z_name.clone()));

        let span = Span::from_offsets(SourceId(0), 0, 10);

        let y_ref = dae::Expression::VarRef {
            name: y_name.clone(),
            subscripts: vec![],
        };
        let eq1 = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(rumoca_ir_core::Token::default()),
            lhs: Box::new(y_ref.clone()),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
        };
        let eq2 = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(rumoca_ir_core::Token::default()),
            lhs: Box::new(y_ref),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
        };
        dae.f_x.push(dae::Equation::residual(eq1, span, "y = 1"));
        dae.f_x.push(dae::Equation::residual(eq2, span, "y = 2"));

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

        let x_name = dae::VarName::from("x");
        dae.states
            .insert(x_name.clone(), dae::Variable::new(x_name.clone()));

        let span = Span::from_offsets(SourceId(0), 0, 10);
        let der_x = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![dae::Expression::VarRef {
                name: x_name.clone(),
                subscripts: vec![],
            }],
        };
        let one = dae::Expression::Literal(dae::Literal::Real(1.0));
        let residual = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(rumoca_ir_core::Token::default()),
            lhs: Box::new(der_x),
            rhs: Box::new(one),
        };
        dae.f_x
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

        let y_name = dae::VarName::from("y");
        let z_name = dae::VarName::from("z");
        dae.algebraics
            .insert(y_name.clone(), dae::Variable::new(y_name.clone()));
        dae.algebraics
            .insert(z_name.clone(), dae::Variable::new(z_name.clone()));

        let span = Span::from_offsets(SourceId(0), 0, 10);
        let y_ref = dae::Expression::VarRef {
            name: y_name.clone(),
            subscripts: vec![],
        };
        let eq1 = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(rumoca_ir_core::Token::default()),
            lhs: Box::new(y_ref.clone()),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
        };
        let eq2 = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(rumoca_ir_core::Token::default()),
            lhs: Box::new(y_ref),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
        };
        dae.f_x.push(dae::Equation::residual(eq1, span, "y = 1"));
        dae.f_x.push(dae::Equation::residual(eq2, span, "y = 2"));

        let result = sort_dae(&dae);
        assert!(matches!(result, Err(StructuralError::Singular { .. })));
    }
}
