//! Structural diagnostics: singularity checks and algebraic loop reporting.

use rumoca_core::{Diagnostic, Label, Span};
use rumoca_ir_dae as dae;

use crate::incidence::Incidence;
use crate::types::UnknownId;

/// Result of structural analysis on a DAE system.
#[derive(Debug, Default)]
pub struct StructuralDiagnostics {
    /// Diagnostic warnings/notes generated during analysis.
    pub diagnostics: Vec<Diagnostic>,
    /// Size of maximum matching (ideally equals n_eq = n_var).
    pub matching_size: usize,
    /// Total number of equations analyzed.
    pub n_equations: usize,
    /// Total number of unknowns analyzed.
    pub n_unknowns: usize,
    /// Names of unmatched unknowns (structurally underdetermined).
    pub unmatched_unknowns: Vec<String>,
    /// Origins of unmatched equations (structurally overdetermined).
    pub unmatched_equations: Vec<String>,
    /// Algebraic loops: each inner Vec is a group of equation indices forming a loop.
    pub algebraic_loops: Vec<AlgebraicLoop>,
}

/// An algebraic loop: a set of equations that must be solved simultaneously.
#[derive(Debug)]
pub struct AlgebraicLoop {
    /// Equation origins in this loop.
    pub equation_origins: Vec<String>,
    /// Unknown names involved in this loop.
    pub unknown_names: Vec<String>,
    /// Spans of equations involved (for diagnostics).
    pub spans: Vec<Span>,
}

/// Groups matching-related data for diagnostic generation.
pub(crate) struct MatchingContext<'a> {
    pub equations: &'a [&'a dae::Equation],
    pub unknown_names: &'a [UnknownId],
    pub eq_unknowns: &'a [std::collections::HashSet<usize>],
    pub match_eq: &'a [Option<usize>],
    pub match_var: &'a [Option<usize>],
}

impl MatchingContext<'_> {
    /// Check for structural singularity and generate diagnostics.
    pub(crate) fn check_singularity(
        &self,
        result: &mut StructuralDiagnostics,
        n_eq: usize,
        n_var: usize,
        matching_size: usize,
    ) {
        let unmatched_eq_indices: Vec<usize> = self
            .match_eq
            .iter()
            .enumerate()
            .filter(|(_, m)| m.is_none())
            .map(|(i, _)| i)
            .collect();

        let unmatched_var_indices: Vec<usize> = self
            .match_var
            .iter()
            .enumerate()
            .filter(|(_, m)| m.is_none())
            .map(|(i, _)| i)
            .collect();

        result.unmatched_equations = unmatched_eq_indices
            .iter()
            .map(|&i| self.equations[i].origin.clone())
            .collect();
        result.unmatched_unknowns = unmatched_var_indices
            .iter()
            .map(|&i| self.unknown_names[i].to_string())
            .collect();

        let mut diag = Diagnostic::warning(format!(
            "structurally singular system: matching size {} < {} (equations={}, unknowns={})",
            matching_size,
            n_eq.min(n_var),
            n_eq,
            n_var,
        ));

        for &eq_idx in &unmatched_eq_indices {
            diag.labels.push(Label {
                span: self.equations[eq_idx].span,
                message: Some("unmatched equation".to_string()),
                primary: true,
            });
        }

        if !result.unmatched_unknowns.is_empty() {
            diag.notes.push(format!(
                "unmatched unknowns: {}",
                result.unmatched_unknowns.join(", ")
            ));
        }
        if !result.unmatched_equations.is_empty() {
            diag.notes.push(format!(
                "unmatched equations: {}",
                result.unmatched_equations.join(", ")
            ));
        }
        diag.notes.push(
            "a structurally singular system may have redundant equations or underdetermined variables"
                .to_string(),
        );

        result.diagnostics.push(diag);
    }

    /// Detect algebraic loops using Tarjan SCC and generate diagnostics.
    pub(crate) fn detect_algebraic_loops(&self, result: &mut StructuralDiagnostics, n_eq: usize) {
        let adj = crate::incidence::build_dependency_graph(self.eq_unknowns, self.match_var, n_eq);
        let sccs = crate::tarjan::tarjan_scc(n_eq, &adj);

        for scc in &sccs {
            if scc.len() <= 1 {
                continue;
            }

            let eq_origins: Vec<String> = scc
                .iter()
                .map(|&i| self.equations[i].origin.clone())
                .collect();
            let eq_spans: Vec<Span> = scc.iter().map(|&i| self.equations[i].span).collect();
            let loop_unknowns: Vec<String> = scc
                .iter()
                .filter_map(|&i| self.match_eq[i].map(|v| self.unknown_names[v].to_string()))
                .collect();

            let mut diag = Diagnostic::warning(format!(
                "algebraic loop detected: {} equations must be solved simultaneously",
                scc.len(),
            ));
            for span in &eq_spans {
                diag.labels.push(Label {
                    span: *span,
                    message: Some("part of algebraic loop".to_string()),
                    primary: false,
                });
            }
            diag.notes
                .push(format!("unknowns: {}", loop_unknowns.join(", ")));
            diag.notes
                .push(format!("equations: {}", eq_origins.join(", ")));
            diag.notes.push(
                "algebraic loops require implicit solvers; CasADi handles this automatically"
                    .to_string(),
            );

            result.diagnostics.push(diag);
            result.algebraic_loops.push(AlgebraicLoop {
                equation_origins: eq_origins,
                unknown_names: loop_unknowns,
                spans: eq_spans,
            });
        }
    }
}

/// Collect warning diagnostics from BLT blocks (algebraic loop notifications).
pub(crate) fn collect_warnings(
    incidence: &Incidence,
    match_eq: &[Option<usize>],
    adj: &[Vec<usize>],
    equations: &[&dae::Equation],
) -> Vec<Diagnostic> {
    let sccs = crate::tarjan::tarjan_scc(incidence.n_eq, adj);
    let mut warnings = Vec::new();

    for scc in &sccs {
        if scc.len() <= 1 {
            continue;
        }

        let eq_origins: Vec<String> = scc.iter().map(|&i| equations[i].origin.clone()).collect();
        let eq_spans: Vec<Span> = scc.iter().map(|&i| equations[i].span).collect();
        let loop_unknowns: Vec<String> = scc
            .iter()
            .filter_map(|&i| match_eq[i].map(|v| incidence.unknown_names[v].to_string()))
            .collect();

        let mut diag = Diagnostic::warning(format!(
            "algebraic loop detected: {} equations must be solved simultaneously",
            scc.len(),
        ));
        for span in &eq_spans {
            diag.labels.push(Label {
                span: *span,
                message: Some("part of algebraic loop".to_string()),
                primary: false,
            });
        }
        diag.notes
            .push(format!("unknowns: {}", loop_unknowns.join(", ")));
        diag.notes
            .push(format!("equations: {}", eq_origins.join(", ")));
        diag.notes.push(
            "algebraic loops require implicit solvers; CasADi handles this automatically"
                .to_string(),
        );

        warnings.push(diag);
    }

    warnings
}
