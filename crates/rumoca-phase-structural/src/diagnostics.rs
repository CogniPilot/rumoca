use rumoca_core::{Diagnostic, Label, PrimaryLabel, Span};
use rumoca_ir_dae as dae;

use crate::diagnostic_codes::{ES001_STRUCTURAL_SINGULARITY, ES002_ALGEBRAIC_LOOP};
use crate::incidence::Incidence;

#[derive(Debug, Default)]
pub struct StructuralDiagnostics {
    pub diagnostics: Vec<Diagnostic>,
    pub matching_size: usize,
    pub n_equations: usize,
    pub n_unknowns: usize,
    pub unmatched_unknowns: Vec<String>,
    pub unmatched_equations: Vec<String>,
    pub algebraic_loops: Vec<AlgebraicLoop>,
}

#[derive(Debug)]
pub struct AlgebraicLoop {
    pub equation_origins: Vec<String>,
    pub unknown_names: Vec<String>,
    pub spans: Vec<Span>,
}

pub(crate) fn collect_warnings<'dae>(
    view: dae::DaeView<'dae>,
    incidence: &Incidence<'dae>,
    match_eq: &[Option<usize>],
    adjacency: &[Vec<usize>],
) -> Vec<Diagnostic> {
    let mut warnings = Vec::new();
    for component in crate::tarjan::tarjan_scc(incidence.n_eq, adjacency) {
        if component.len() <= 1 {
            continue;
        }
        let equations = component
            .iter()
            .map(|index| crate::equation_label(view, &incidence.equation_refs[*index]))
            .collect::<Vec<_>>();
        let unknowns = component
            .iter()
            .filter_map(|index| match_eq[*index])
            .map(|index| crate::unknown_label(view, incidence.unknowns[index]))
            .collect::<Vec<_>>();
        let spans = component
            .iter()
            .filter_map(|index| incidence.equation_spans.get(*index).copied())
            .collect::<Vec<_>>();
        let mut diagnostic = structural_warning(
            ES002_ALGEBRAIC_LOOP,
            format!(
                "algebraic loop detected: {} equations must be solved simultaneously",
                component.len()
            ),
            spans.first().copied(),
            "part of algebraic loop",
        );
        for span in spans.iter().copied().skip(1) {
            diagnostic = diagnostic
                .with_label(Label::secondary(span).with_message("part of algebraic loop"));
        }
        diagnostic
            .notes
            .push(format!("unknowns: {}", unknowns.join(", ")));
        diagnostic
            .notes
            .push(format!("equations: {}", equations.join(", ")));
        warnings.push(diagnostic);
    }
    warnings
}

pub(crate) fn singular_warning(
    span: Option<Span>,
    equations: &[String],
    unknowns: &[String],
    matched: usize,
    equation_count: usize,
    unknown_count: usize,
) -> Diagnostic {
    let mut diagnostic = structural_warning(
        ES001_STRUCTURAL_SINGULARITY,
        format!(
            "structurally singular system: matching size {matched} (equations={equation_count}, unknowns={unknown_count})"
        ),
        span,
        "unmatched structural owner",
    );
    if !equations.is_empty() {
        diagnostic
            .notes
            .push(format!("unmatched equations: {}", equations.join(", ")));
    }
    if !unknowns.is_empty() {
        diagnostic
            .notes
            .push(format!("unmatched unknowns: {}", unknowns.join(", ")));
    }
    diagnostic
}

fn structural_warning(
    code: &'static str,
    message: String,
    span: Option<Span>,
    label: &'static str,
) -> Diagnostic {
    match span.filter(|span| !span.is_dummy()) {
        Some(span) => {
            Diagnostic::warning(code, message, PrimaryLabel::new(span).with_message(label))
        }
        None => Diagnostic::global_warning(code, message),
    }
}
