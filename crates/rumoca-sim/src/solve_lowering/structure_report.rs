//! Human-readable structural reports over the immutable checked DAE.

use rumoca_ir_dae as dae;
use rumoca_solver::SimOptions;

use super::diagnostics::SimulationDiagnosticError;

pub fn structural_report_for_dae(
    model: &dae::Dae,
    _: &SimOptions,
) -> Result<rumoca_phase_structural::StructuralReport, SimulationDiagnosticError> {
    model.inspect(|view| {
        rumoca_phase_structural::build_structural_report(view).map_err(structural_error)
    })
}

#[derive(Debug, Clone)]
pub struct SingularityDiagnosis {
    pub n_equations: usize,
    pub n_unknowns: usize,
    pub n_matched: usize,
    pub unknowns: Vec<UnmatchedUnknownDiagnosis>,
    pub equations: Vec<UnmatchedEquationDiagnosis>,
}

#[derive(Debug, Clone)]
pub struct UnmatchedEquationDiagnosis {
    pub name: String,
    pub origin: String,
    pub summary: String,
}

#[derive(Debug, Clone)]
pub struct UnmatchedUnknownDiagnosis {
    pub name: String,
    pub category: String,
    pub referencing_rows: Vec<usize>,
}

pub fn diagnose_structural_singularity(
    model: &dae::Dae,
    _: &SimOptions,
) -> Result<Option<SingularityDiagnosis>, SimulationDiagnosticError> {
    model.inspect(|view| {
        let error = match rumoca_phase_structural::sort(view) {
            Ok(_) | Err(rumoca_phase_structural::StructuralError::EmptySystem) => return Ok(None),
            Err(error) => error,
        };
        let rumoca_phase_structural::StructuralError::Singular {
            n_equations,
            n_unknowns,
            n_matched,
            unmatched_equations,
            unmatched_unknowns,
            ..
        } = error
        else {
            return Err(structural_error(error));
        };
        let unknowns = unmatched_unknowns
            .into_iter()
            .map(|name| UnmatchedUnknownDiagnosis {
                category: classify_unknown(&name).to_string(),
                name,
                referencing_rows: Vec::new(),
            })
            .collect();
        let equations = unmatched_equations
            .into_iter()
            .map(|name| equation_diagnosis(model, view, name))
            .collect();
        Ok(Some(SingularityDiagnosis {
            n_equations,
            n_unknowns,
            n_matched,
            unknowns,
            equations,
        }))
    })
}

fn equation_diagnosis(
    model: &dae::Dae,
    view: dae::DaeView<'_>,
    name: String,
) -> UnmatchedEquationDiagnosis {
    let index = name
        .strip_prefix("f_x[")
        .and_then(|rest| rest.strip_suffix(']'))
        .and_then(|digits| digits.parse::<usize>().ok());
    let provenance = index
        .and_then(|index| view.continuous_owner_for_scalar_row(index))
        .map(|owner| match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => equation.provenance(),
            dae::ContinuousOwnerView::Structured { family, .. } => family.provenance(),
        });
    let source = provenance
        .and_then(|provenance| model.source_text(provenance))
        .unwrap_or("")
        .to_string();
    UnmatchedEquationDiagnosis {
        name,
        origin: source.clone(),
        summary: source,
    }
}

fn classify_unknown(name: &str) -> &'static str {
    if name.starts_with("der(") {
        "unmatched state derivative"
    } else if name.ends_with(".v") {
        "unmatched connector potential"
    } else if name.ends_with(".tau") || name.ends_with(".f") {
        "unmatched connector flow"
    } else {
        "unmatched algebraic coordinate"
    }
}

fn structural_error(error: rumoca_phase_structural::StructuralError) -> SimulationDiagnosticError {
    SimulationDiagnosticError::RuntimePreparation {
        message: error.to_string(),
        span: error.source_span(),
    }
}
