//! The `rumoca sim --inspect structure` report and its automated structural
//! singularity triage: a named matching / BLT / tearing report, and — when the
//! prepared system is singular — a per-unmatched-unknown root-cause diagnosis.

use rumoca_ir_dae as dae;
use rumoca_solver::SimOptions;

use super::diagnostics::SimulationDiagnosticError;
use super::expr_util::equation_lhs_prefix;
use super::structural_lowering::{
    prepare_dae_after_boundary_elimination, prepare_dae_for_structural_analysis,
};

fn prepare_dae_for_structure_report(
    prepared: &mut dae::Dae,
    opts: &SimOptions,
) -> Result<(), SimulationDiagnosticError> {
    prepare_dae_for_structural_analysis(prepared, opts)
        .map_err(SimulationDiagnosticError::SolveLowering)?;
    loop {
        let boundary =
            rumoca_phase_structural::eliminate::resolve_boundary_equations_to_fixpoint(prepared)
                .map_err(|error| {
                    SimulationDiagnosticError::Solver(format!(
                        "structural boundary preparation failed: {error}"
                    ))
                })?;
        let changed = prepare_dae_after_boundary_elimination(prepared, &boundary.substitutions)
            .map_err(SimulationDiagnosticError::SolveLowering)?;
        if boundary.n_eliminated == 0 && !changed {
            break;
        }
    }
    Ok(())
}

/// Structurally analyze the model and return a named report of the matching,
/// BLT blocks, coupled SCCs, and tearing. The analysis runs on the flattened
/// DAE *before* scalar-block elimination/tearing collapses the system, so the
/// coupled algebraic loops (e.g. "3 derivatives form one coupled block") and how
/// the compiler would tear them are visible — that elimination is exactly what
/// hides them in the final lowered artifact. Uses the same matching / BLT /
/// tearing algorithms the compiler uses. Backs the `rumoca sim --structure`
/// debug dump.
pub fn structural_report_for_dae(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<rumoca_phase_structural::StructuralReport, SimulationDiagnosticError> {
    // Report on the same prepared system the simulator matches, so the analysis
    // reflects reality (e.g. `der(x)` references in non-ODE rows are resolved).
    let mut prepared = dae_model.clone();
    prepare_dae_for_structure_report(&mut prepared, opts)?;
    rumoca_phase_structural::build_structural_report(&prepared).map_err(|error| {
        SimulationDiagnosticError::Solver(format!("structural analysis failed: {error}"))
    })
}

/// Structured triage of a structural singularity, so failures can be classified
/// without hand-reading the DAE: for each unmatched unknown it records a likely
/// root-cause category and the `f_x` rows that reference it.
#[derive(Debug, Clone)]
pub struct SingularityDiagnosis {
    pub n_equations: usize,
    pub n_unknowns: usize,
    pub n_matched: usize,
    pub unknowns: Vec<UnmatchedUnknownDiagnosis>,
    pub equations: Vec<UnmatchedEquationDiagnosis>,
}

/// One unmatched equation from a [`SingularityDiagnosis`].
#[derive(Debug, Clone)]
pub struct UnmatchedEquationDiagnosis {
    pub name: String,
    pub origin: String,
    pub summary: String,
}

/// One unmatched unknown from a [`SingularityDiagnosis`].
#[derive(Debug, Clone)]
pub struct UnmatchedUnknownDiagnosis {
    pub name: String,
    pub category: String,
    pub referencing_rows: Vec<usize>,
}

/// Run the shared structural prep + matching; if the prepared system is
/// singular, return a per-unmatched-unknown diagnosis (`None` when it matches).
/// This powers `--inspect structure`'s automated failure explanation so the
/// development cycle does not require hand-reading DAE dumps.
pub fn diagnose_structural_singularity(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<Option<SingularityDiagnosis>, SimulationDiagnosticError> {
    let mut prepared = dae_model.clone();
    prepare_dae_for_structure_report(&mut prepared, opts)?;

    let error = match rumoca_phase_structural::build_structural_report(&prepared) {
        Ok(_) => return Ok(None),
        Err(error) => error,
    };
    if tracing::enabled!(target: "rumoca_phase_structural", tracing::Level::DEBUG) {
        for (index, eq) in prepared.continuous.equations.iter().enumerate() {
            let mut summary = format!("{}{:?}", equation_lhs_prefix(eq), eq.rhs);
            summary.truncate(200);
            tracing::debug!(
                target: "rumoca_phase_structural",
                "[sim-trace] prepared f_x[{index}] origin='{}' {}",
                eq.origin,
                summary
            );
        }
    }
    let rumoca_phase_structural::StructuralError::Singular {
        n_equations,
        n_unknowns,
        n_matched,
        unmatched_unknowns,
        unmatched_equations,
        ..
    } = error
    else {
        return Ok(None);
    };

    let unknowns = unmatched_unknowns
        .iter()
        .map(|name| {
            let (category, der_inner) = classify_unmatched_unknown(name, &prepared);
            let referencing_rows = unknown_referencing_rows(&prepared, name, der_inner.as_deref());
            UnmatchedUnknownDiagnosis {
                name: name.clone(),
                category,
                referencing_rows,
            }
        })
        .collect();
    let equations = unmatched_equations
        .iter()
        .map(|name| unmatched_equation_diagnosis(&prepared, name))
        .collect();

    Ok(Some(SingularityDiagnosis {
        n_equations,
        n_unknowns,
        n_matched,
        unknowns,
        equations,
    }))
}

/// Resolve one `f_x[N]` unmatched-equation name to its origin and a truncated
/// expression summary from the prepared system.
fn unmatched_equation_diagnosis(dae: &dae::Dae, name: &str) -> UnmatchedEquationDiagnosis {
    let index = name
        .strip_prefix("f_x[")
        .and_then(|rest| rest.strip_suffix(']'))
        .and_then(|digits| digits.parse::<usize>().ok());
    let equation = index.and_then(|index| dae.continuous.equations.get(index));
    let (origin, summary) = equation.map_or_else(
        || ("<not an f_x row>".to_string(), String::new()),
        |eq| {
            let mut summary = format!("{}{:?}", equation_lhs_prefix(eq), eq.rhs);
            summary.truncate(220);
            (eq.origin.clone(), summary)
        },
    );
    UnmatchedEquationDiagnosis {
        name: name.to_string(),
        origin,
        summary,
    }
}

fn unmatched_base_name(name: &str) -> &str {
    rumoca_core::parse_scalar_name(name)
        .map(|scalar| scalar.base)
        .unwrap_or(name)
}

/// Classify an unmatched unknown by likely root cause. For `der(X)` it also
/// returns the inner name `X` so callers can find the rows that reference it.
fn classify_unmatched_unknown(name: &str, dae: &dae::Dae) -> (String, Option<String>) {
    if let Some(inner) = name
        .strip_prefix("der(")
        .and_then(|rest| rest.strip_suffix(')'))
    {
        let key = rumoca_core::VarName::new(unmatched_base_name(inner));
        let category = if dae.variables.states.contains_key(&key) {
            "der of state (its ODE row should match — check der-equation form)"
        } else if dae.variables.algebraics.contains_key(&key) {
            "der of an ALGEBRAIC (not a state): der-of-der / index reduction needed"
        } else {
            "der of a non-state variable"
        };
        return (category.to_string(), Some(inner.to_string()));
    }
    let category = if name.ends_with(".v") {
        "node potential (electrical/magnetic topology / missing reference)"
    } else if name.ends_with(".tau") || name.ends_with(".f") {
        "connector flow (mechanical: gear/constraint torque or force)"
    } else if name.ends_with(".y2") {
        "inverse-block output (`y2`)"
    } else {
        "algebraic unknown"
    };
    (category.to_string(), None)
}

/// `f_x` row indices that reference the unmatched unknown (the `der(X)` inner
/// name for derivatives, else the plain variable).
fn unknown_referencing_rows(dae: &dae::Dae, name: &str, der_inner: Option<&str>) -> Vec<usize> {
    let key = rumoca_core::VarName::new(der_inner.map_or(name, unmatched_base_name));
    dae.continuous
        .equations
        .iter()
        .enumerate()
        .filter(|(_, eq)| {
            // `lhs` is an optional assigned variable name; `rhs` is the expression.
            if der_inner.is_some() {
                dae::expr_contains_der_of(&eq.rhs, &key)
            } else {
                eq.lhs.as_ref().map(rumoca_core::Reference::var_name) == Some(&key)
                    || dae::expr_contains_var(&eq.rhs, &key)
            }
        })
        .map(|(index, _)| index)
        .collect()
}
