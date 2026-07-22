//! Exact block Gaussian condensation of scalar algebraic BLT loops.
//!
//! This pass keeps the DAE's variables and scalar equation count unchanged. An
//! eligible `N`-row algebraic loop is rewritten as `N-K` explicit causal
//! reconstruction assignments and `K` residual equations containing only the
//! tear variables. Algebraically, substituting the causal assignments into the
//! retained residuals is block Gaussian elimination; for an affine loop the
//! retained equations are its exact Schur complement.
//!
//! Keeping reconstruction as ordinary DAE rows is important: dependency-based
//! Solve lowering can omit them from state-derivative evaluation while still
//! pulling them in for event conditions and requested visible outputs.

use std::collections::HashSet;

use indexmap::IndexSet;

use crate::{BltBlock, StructuralError, UnknownId, sort_dae};

use super::{
    Dae, Reference, Substitution, VarName, apply_substitutions_in_order,
    drop_unreferenced_continuous_unknowns, expr_contains_var, simplify_arithmetic_identities,
    stable_solution_for_unknown, substitution_for_var,
};

/// One algebraic BLT block replaced by its exact causal/Schur form.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CondensedAlgebraicBlock {
    /// Unknown count before condensation.
    pub original_size: usize,
    /// Unknown count in the retained Schur-complement residual system.
    pub core_size: usize,
    /// Variables solved by explicit reconstruction rows, in causal order.
    pub causal_variables: Vec<VarName>,
    /// Variables retained in the condensed residual system.
    pub core_variables: Vec<VarName>,
}

/// Summary of exact algebraic-loop condensation.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ScalarBlockCondensationResult {
    /// Condensed BLT blocks in original evaluation order.
    pub blocks: Vec<CondensedAlgebraicBlock>,
}

impl ScalarBlockCondensationResult {
    /// Total number of variables moved out of coupled residual systems.
    pub fn causal_variable_count(&self) -> usize {
        self.blocks
            .iter()
            .map(|block| block.causal_variables.len())
            .sum()
    }
}

#[derive(Debug)]
struct EquationPatch {
    equation_index: usize,
    equation: rumoca_ir_dae::Equation,
}

#[derive(Debug)]
struct CondensationPlan {
    summary: CondensedAlgebraicBlock,
    patches: Vec<EquationPatch>,
}

struct CausalSubstitutions {
    compact: Vec<Substitution>,
    expanded: Vec<Substitution>,
}

/// Condense eligible scalar algebraic BLT loops by exact symbolic elimination.
///
/// The input must already be the scalar solver view. Unsupported loops are
/// preserved unchanged, so this is semantics-preserving and never required for
/// correctness.
pub fn condense_scalar_algebraic_loops(
    dae: &mut Dae,
) -> Result<ScalarBlockCondensationResult, StructuralError> {
    if dae
        .continuous
        .equations
        .iter()
        .any(|equation| equation.scalar_count != 1)
    {
        return Ok(ScalarBlockCondensationResult::default());
    }
    // Observable variables may intentionally remain in the solver-view DAE
    // after their defining aliases were eliminated. Sort the regular continuous
    // subsystem used by the solver without deleting those cold reconstruction
    // slots from the authoritative view.
    let mut sort_input = dae.clone();
    drop_unreferenced_continuous_unknowns(&mut sort_input)?;
    let sorted = match sort_dae(&sort_input) {
        Ok(sorted) => sorted,
        Err(StructuralError::EmptySystem) => return Ok(ScalarBlockCondensationResult::default()),
        Err(error) => {
            // Structural validation already ran before observation-only causal
            // rows were restored. Those cold rows can make this optional view
            // rectangular without invalidating the solver model, so decline
            // condensation rather than promoting an optimization precondition
            // into a semantic error.
            crate::structural_trace!(
                "[sim-trace] exact block condensation skipped: scalar solver view is not a regular square subsystem ({})",
                error
            );
            return Ok(ScalarBlockCondensationResult::default());
        }
    };
    let mut plans = Vec::new();
    for block in &sorted.blocks {
        let BltBlock::AlgebraicLoop {
            equations,
            unknowns,
        } = block
        else {
            continue;
        };
        if let Some(plan) = plan_block_condensation(dae, equations, unknowns)? {
            plans.push(plan);
        }
    }
    apply_condensation_plans(dae, plans)
}

fn plan_block_condensation(
    dae: &Dae,
    equations: &[crate::EquationRef],
    unknowns: &[UnknownId],
) -> Result<Option<CondensationPlan>, StructuralError> {
    let Some(var_names) = algebraic_unknown_names(unknowns) else {
        return Ok(None);
    };
    let eq_indices = equations
        .iter()
        .map(|equation| equation.0)
        .collect::<Vec<_>>();
    let incidence = local_incidence(dae, &eq_indices, &var_names);
    let candidates = exact_causal_candidates(dae, &eq_indices, &var_names, &incidence)?;
    let Some(tearing) =
        crate::tear_algebraic_loop_with_causal_candidates(var_names.len(), &incidence, &candidates)
    else {
        return Ok(None);
    };
    let Some(substitutions) =
        build_causal_substitutions(dae, &eq_indices, &var_names, &tearing.causal_sequence)?
    else {
        return Ok(None);
    };
    build_condensation_plan(dae, eq_indices, var_names, tearing, substitutions).map(Some)
}

fn algebraic_unknown_names(unknowns: &[UnknownId]) -> Option<Vec<VarName>> {
    unknowns
        .iter()
        .map(|unknown| match unknown {
            UnknownId::Variable(name) => Some(name.clone()),
            UnknownId::DerState(_) | UnknownId::SolverY(_) => None,
        })
        .collect()
}

fn local_incidence(dae: &Dae, eq_indices: &[usize], var_names: &[VarName]) -> Vec<HashSet<usize>> {
    eq_indices
        .iter()
        .map(|&equation_index| {
            let rhs = &dae.continuous.equations[equation_index].rhs;
            var_names
                .iter()
                .enumerate()
                .filter_map(|(index, name)| expr_contains_var(rhs, name).then_some(index))
                .collect()
        })
        .collect()
}

fn exact_causal_candidates(
    dae: &Dae,
    eq_indices: &[usize],
    var_names: &[VarName],
    incidence: &[HashSet<usize>],
) -> Result<Vec<HashSet<usize>>, StructuralError> {
    eq_indices
        .iter()
        .zip(incidence)
        .map(|(&equation_index, row_incidence)| {
            let rhs = &dae.continuous.equations[equation_index].rhs;
            let mut candidates = HashSet::new();
            for &variable_index in row_incidence {
                let variable = &var_names[variable_index];
                if stable_solution_for_unknown(dae, rhs, variable)?.is_some() {
                    candidates.insert(variable_index);
                }
            }
            Ok(candidates)
        })
        .collect()
}

fn build_causal_substitutions(
    dae: &Dae,
    eq_indices: &[usize],
    var_names: &[VarName],
    causal_sequence: &[(usize, usize)],
) -> Result<Option<CausalSubstitutions>, StructuralError> {
    let mut compact = Vec::with_capacity(causal_sequence.len());
    let mut expanded = Vec::with_capacity(causal_sequence.len());
    for &(local_equation, local_variable) in causal_sequence {
        let equation_index = eq_indices[local_equation];
        let variable = &var_names[local_variable];
        let rhs = &dae.continuous.equations[equation_index].rhs;
        let Some(compact_solution) = stable_solution_for_unknown(dae, rhs, variable)? else {
            return Ok(None);
        };
        let expanded_solution = apply_substitutions_in_order(dae, &compact_solution, &expanded)?;
        compact.push(solver_view_substitution(dae, variable, compact_solution)?);
        expanded.push(solver_view_substitution(dae, variable, expanded_solution)?);
    }
    Ok(Some(CausalSubstitutions { compact, expanded }))
}

fn solver_view_substitution(
    dae: &Dae,
    variable: &VarName,
    solution: rumoca_core::Expression,
) -> Result<Substitution, StructuralError> {
    let mut substitution = substitution_for_var(dae, variable.clone(), solution)?;
    // This substitution belongs to the generated scalar solver view. Its
    // canonical scalar BLT name is the identity consumed by Solve IR;
    // retaining an aggregate element's source component identity can point
    // at an embedded connector alias rather than the owning DAE slot.
    substitution.var_ref = Some(Reference::generated(variable.as_str()));
    Ok(substitution)
}

fn build_condensation_plan(
    dae: &Dae,
    eq_indices: Vec<usize>,
    var_names: Vec<VarName>,
    tearing: crate::TearingResult,
    substitutions: CausalSubstitutions,
) -> Result<CondensationPlan, StructuralError> {
    let mut patches = reconstruction_patches(dae, &tearing, &eq_indices, &substitutions.compact);
    patches.extend(condensed_residual_patches(
        dae,
        &tearing,
        &eq_indices,
        &substitutions.expanded,
    )?);
    let causal_variables = substitutions
        .compact
        .iter()
        .map(|substitution| substitution.var_name.clone())
        .collect::<Vec<_>>();
    let causal_set = causal_variables.iter().cloned().collect::<IndexSet<_>>();
    let core_variables = var_names
        .into_iter()
        .filter(|variable| !causal_set.contains(variable))
        .collect::<Vec<_>>();
    let summary = CondensedAlgebraicBlock {
        original_size: causal_variables.len() + core_variables.len(),
        core_size: core_variables.len(),
        causal_variables,
        core_variables,
    };
    Ok(CondensationPlan { summary, patches })
}

fn reconstruction_patches(
    dae: &Dae,
    tearing: &crate::TearingResult,
    eq_indices: &[usize],
    substitutions: &[Substitution],
) -> Vec<EquationPatch> {
    tearing
        .causal_sequence
        .iter()
        .zip(substitutions)
        .map(|(&(local_equation, _), substitution)| {
            let equation_index = eq_indices[local_equation];
            let source = &dae.continuous.equations[equation_index];
            let lhs = scalar_target_reference(substitution);
            EquationPatch {
                equation_index,
                equation: rumoca_ir_dae::Equation::explicit(
                    lhs,
                    substitution.expr.clone(),
                    source.span,
                    format!("exact causal reconstruction: {}", source.origin),
                ),
            }
        })
        .collect()
}

fn condensed_residual_patches(
    dae: &Dae,
    tearing: &crate::TearingResult,
    eq_indices: &[usize],
    substitutions: &[Substitution],
) -> Result<Vec<EquationPatch>, StructuralError> {
    let causal_names = substitutions
        .iter()
        .map(|substitution| &substitution.var_name)
        .collect::<Vec<_>>();
    tearing
        .residual_eq_local_indices
        .iter()
        .map(|&local_equation| {
            let equation_index = eq_indices[local_equation];
            let source = &dae.continuous.equations[equation_index];
            let rhs = apply_substitutions_in_order(dae, &source.rhs, substitutions)?;
            let rhs = simplify_arithmetic_identities(rhs);
            if causal_names
                .iter()
                .any(|name| expr_contains_var(&rhs, name))
            {
                return Err(StructuralError::ContractViolation {
                    reason: "condensed residual still references a causal variable".to_string(),
                    span: source.span,
                });
            }
            Ok(EquationPatch {
                equation_index,
                equation: rumoca_ir_dae::Equation::residual(
                    rhs,
                    source.span,
                    format!("exact Schur residual: {}", source.origin),
                ),
            })
        })
        .collect()
}

fn scalar_target_reference(substitution: &Substitution) -> Reference {
    // Scalar-view elements are backend projections of an aggregate DAE
    // variable, not new source components. Keep their canonical BLT name and
    // do not synthesize a source identity that may denote an embedded connector
    // alias rather than the aggregate variable owning the solver slot.
    Reference::generated(substitution.var_name.as_str())
}

fn apply_condensation_plans(
    dae: &mut Dae,
    plans: Vec<CondensationPlan>,
) -> Result<ScalarBlockCondensationResult, StructuralError> {
    let changed_rows = plans
        .iter()
        .flat_map(|plan| plan.patches.iter().map(|patch| patch.equation_index))
        .collect::<HashSet<_>>();
    for plan in &plans {
        for patch in &plan.patches {
            dae.continuous.equations[patch.equation_index] = patch.equation.clone();
        }
    }
    drop_rewritten_structured_families(dae, &changed_rows);
    let result = ScalarBlockCondensationResult {
        blocks: plans.into_iter().map(|plan| plan.summary).collect(),
    };
    trace_condensation(&result);
    Ok(result)
}

fn drop_rewritten_structured_families(dae: &mut Dae, changed_rows: &HashSet<usize>) {
    dae.continuous.structured_equations.retain(|family| {
        let Ok(row_count) = family.scalar_view_row_count() else {
            return true;
        };
        let end = family.first_equation_index.saturating_add(row_count);
        !(family.first_equation_index..end).any(|row| changed_rows.contains(&row))
    });
}

fn trace_condensation(result: &ScalarBlockCondensationResult) {
    for block in &result.blocks {
        let causal = block
            .causal_variables
            .iter()
            .map(VarName::as_str)
            .collect::<Vec<_>>()
            .join(",");
        let core = block
            .core_variables
            .iter()
            .map(VarName::as_str)
            .collect::<Vec<_>>()
            .join(",");
        crate::structural_trace!(
            "[sim-trace] exact block condensation original={} core={} causal=[{}] core_variables=[{}]",
            block.original_size,
            block.core_size,
            causal,
            core
        );
    }
}

#[cfg(test)]
mod tests {
    use rumoca_core::{BuiltinFunction, Expression, OpBinary, Reference, Span, VarName};
    use rumoca_ir_dae::{Dae, Equation, Variable};

    use super::*;

    fn variable(name: &str) -> Expression {
        Expression::VarRef {
            name: Reference::generated(name),
            subscripts: Vec::new(),
            span: Span::DUMMY,
        }
    }

    fn subtract(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: Span::DUMMY,
        }
    }

    fn loop_dae() -> Dae {
        let mut dae = Dae::new();
        for name in ["a", "b", "c"] {
            dae.variables.algebraics.insert(
                VarName::new(name),
                Variable::new(VarName::new(name), Span::DUMMY),
            );
        }
        dae.continuous.equations.extend([
            Equation::residual(subtract(variable("a"), variable("b")), Span::DUMMY, "a=b"),
            Equation::residual(subtract(variable("b"), variable("c")), Span::DUMMY, "b=c"),
            Equation::residual(
                subtract(
                    variable("c"),
                    Expression::BuiltinCall {
                        function: BuiltinFunction::Sin,
                        args: vec![variable("a")],
                        span: Span::DUMMY,
                    },
                ),
                Span::DUMMY,
                "c=sin(a)",
            ),
        ]);
        dae
    }

    #[test]
    fn condenses_loop_without_deleting_variables_or_equations() {
        let mut dae = loop_dae();
        let result = condense_scalar_algebraic_loops(&mut dae)
            .expect("exact scalar loop condensation should succeed");

        assert_eq!(result.blocks.len(), 1);
        assert_eq!(result.blocks[0].original_size, 3);
        assert_eq!(result.blocks[0].core_size, 1);
        assert_eq!(result.causal_variable_count(), 2);
        assert_eq!(dae.variables.algebraics.len(), 3);
        assert_eq!(dae.continuous.equations.len(), 3);

        let causal = result.blocks[0]
            .causal_variables
            .iter()
            .collect::<IndexSet<_>>();
        let schur_rows = dae
            .continuous
            .equations
            .iter()
            .filter(|equation| equation.origin.starts_with("exact Schur residual:"))
            .collect::<Vec<_>>();
        assert_eq!(schur_rows.len(), 1);
        assert!(
            causal
                .iter()
                .all(|name| !expr_contains_var(&schur_rows[0].rhs, name))
        );
        assert_eq!(
            dae.continuous
                .equations
                .iter()
                .filter(|equation| equation.origin.starts_with("exact causal reconstruction:"))
                .count(),
            2
        );
    }
}
