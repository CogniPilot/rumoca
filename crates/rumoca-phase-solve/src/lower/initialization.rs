//! Lower the complete MLS §8.6 system in one initialization context.

use super::{DerivativeRowIndex, ScalarCompiler, ScalarRowSource, ScalarRows, scalar_count};
use super::{initial_discrete, initial_parameters, initial_pins, initial_projection};
use crate::{LowerError, layout::LoweredLayout};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_phase_structural as structural;
use std::collections::HashMap;

pub(super) fn lower_initialization<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    derivatives: &DerivativeRowIndex<'dae>,
    pins: &[structural::InitialValuePin],
    manifold: &[dae::ExprId<'dae>],
    overrides: &HashMap<String, f64>,
) -> Result<solve::InitializationSolveSystem, LowerError> {
    // Every parameter coordinate the initialization determines gets exactly one
    // owner, decided before any row is lowered: the residual rows below have to
    // recompute a bound owner's binding rather than read the seed the parameter
    // set stored for it.
    let ownership =
        initial_parameters::initialization_parameter_ownership(view, layout, overrides)?;
    let context = InitializationRowContext {
        view,
        layout,
        derivatives,
        ownership: &ownership,
    };
    let mut rows = ScalarRows::default();
    let mut row_incidence: Vec<initial_projection::InitialRowIncidence<'dae>> = Vec::new();
    lower_declared_rows(context, &mut rows, &mut row_incidence)?;
    let transferred =
        initial_pins::lower_transferred_initial_values(view, layout, &ownership, pins)?;
    // MLS §8.6 solves the states and the `fixed = false` parameters together; the
    // space names which coordinate of each kind the projection may own, and which
    // a declaration has already determined.
    let space = initial_projection::initialization_unknown_space(
        initial_projection::InitializationUnknownInputs {
            view,
            layout,
            ownership: &ownership,
            derivatives,
            given_state_indices: &transferred.given_state_indices,
        },
    )?;
    rows.extend(transferred.checks);
    row_incidence.extend(transferred.check_incidence);
    let manifold_start = rows.len();
    lower_manifold_rows(context, manifold, &mut rows, &mut row_incidence)?;
    let manifold_row_count = rows.len() - manifold_start;
    let plan = initial_projection::plan_initialization_projection(&space, &row_incidence)?;
    let mut updates = initial_discrete::lower_initial_discrete_values(view, layout)?;
    // MLS §8.6 orders these after the projection that solves the `fixed = false`
    // unknowns they read; `settle_initialization_system` iterates the whole set
    // to a fixed point, so the two row groups share one update block.
    let dependents = ownership.lower_solved_parameter_reads(view, layout, derivatives)?;
    updates.rows.extend(dependents.rows);
    updates.targets.extend(dependents.targets);
    Ok(solve::InitializationSolveSystem::construct(
        solve::InitializationSystemInput {
            residual: rows.into_compute_block()?,
            manifold_row_count,
            given_state_indices: transferred.given_state_indices,
            row_roles: plan.row_roles,
            projection_plan: plan.plan,
            update_rhs: updates.rows.into_scalar_block()?,
            update_targets: updates.targets,
        },
    )?)
}

/// Everything a lowered initialization row resolves its leaves against.
#[derive(Clone, Copy)]
struct InitializationRowContext<'a, 'dae> {
    view: dae::DaeView<'dae>,
    layout: &'a LoweredLayout<'dae>,
    derivatives: &'a DerivativeRowIndex<'dae>,
    ownership: &'a initial_parameters::InitializationParameterOwnership<'dae>,
}

fn lower_declared_rows<'dae>(
    context: InitializationRowContext<'_, 'dae>,
    rows: &mut ScalarRows,
    incidence: &mut Vec<initial_projection::InitialRowIncidence<'dae>>,
) -> Result<(), LowerError> {
    for owner in context.view.initialization_owners() {
        match owner {
            dae::InitializationOwnerView::Residual { equation, .. } => {
                let count = scalar_count(context.view, equation.residual());
                for scalar in 0..count {
                    // An initial residual may constrain a state derivative;
                    // the continuous row that defines it supplies its value.
                    let program = lower_initial_residual_program(context, equation, scalar)?;
                    let output = rows.programs.len();
                    rows.push(program, equation.provenance().span(), output);
                    incidence.push(initial_projection::InitialRowIncidence::Residual(
                        ScalarRowSource {
                            expression: equation.residual(),
                            scalar,
                            domain_point: None,
                        },
                    ));
                }
            }
            dae::InitializationOwnerView::Structured { family, .. } => {
                lower_initialization_family(context, family, rows, incidence)?;
            }
        }
    }
    Ok(())
}

fn lower_manifold_rows<'dae>(
    context: InitializationRowContext<'_, 'dae>,
    manifold: &[dae::ExprId<'dae>],
    rows: &mut ScalarRows,
    incidence: &mut Vec<initial_projection::InitialRowIncidence<'dae>>,
) -> Result<(), LowerError> {
    let selector = super::scalar::ScalarSelector::new(context.view, None);
    for expression in manifold.iter().copied() {
        for scalar in 0..scalar_count(context.view, expression) {
            let node = selector.node(expression);
            let program = ScalarCompiler::new(context.view, context.layout, None)
                .with_derivative_definitions(context.derivatives)
                .with_parameter_substitutions(context.ownership.substitutions())
                .program(expression, scalar)?;
            let output = rows.len();
            rows.push(program, node.provenance().span(), output);
            incidence.push(initial_projection::InitialRowIncidence::Residual(
                ScalarRowSource {
                    expression,
                    scalar,
                    domain_point: None,
                },
            ));
        }
    }
    Ok(())
}

fn lower_initial_residual_program<'dae>(
    context: InitializationRowContext<'_, 'dae>,
    equation: dae::ResidualEquationView<'dae>,
    scalar: usize,
) -> Result<Vec<solve::LinearOp>, LowerError> {
    ScalarCompiler::new(context.view, context.layout, None)
        .with_derivative_definitions(context.derivatives)
        .with_parameter_substitutions(context.ownership.substitutions())
        .program(equation.residual(), scalar)
        .map_err(|error| {
            LowerError::non_computable(
                format!("initial residual cannot resolve its derivative reads: {error}"),
                equation.provenance().span(),
            )
        })
}

fn lower_initialization_family<'dae>(
    context: InitializationRowContext<'_, 'dae>,
    family: dae::StructuredFamilyView<'dae>,
    rows: &mut ScalarRows,
    incidence: &mut Vec<initial_projection::InitialRowIncidence<'dae>>,
) -> Result<(), LowerError> {
    let domain = context
        .view
        .domain(family.domain())
        .expect("checked family domain resolves");
    let points = domain
        .structured()
        .index_tuple_iter()
        .expect("checked domain remains valid");
    for (point, values) in points.enumerate() {
        let scalar = family
            .scalar_view()
            .body_scalar(point, domain.extents())
            .expect("checked family view projects its domain point");
        lower_initialization_family_point(context, family, scalar, &values, rows, incidence)?;
    }
    Ok(())
}

fn lower_initialization_family_point<'dae>(
    context: InitializationRowContext<'_, 'dae>,
    family: dae::StructuredFamilyView<'dae>,
    scalar: usize,
    values: &[i64],
    rows: &mut ScalarRows,
    incidence: &mut Vec<initial_projection::InitialRowIncidence<'dae>>,
) -> Result<(), LowerError> {
    for body in family.bodies().iter() {
        let program = ScalarCompiler::new(
            context.view,
            context.layout,
            Some((family.domain(), values)),
        )
        .with_derivative_definitions(context.derivatives)
        .with_parameter_substitutions(context.ownership.substitutions())
        .program(body, scalar)
        .map_err(|error| {
            LowerError::non_computable(
                format!("structured initial residual cannot resolve its derivative reads: {error}"),
                family.provenance().span(),
            )
        })?;
        let output = rows.programs.len();
        rows.push(program, family.provenance().span(), output);
        incidence.push(initial_projection::InitialRowIncidence::Residual(
            ScalarRowSource {
                expression: body,
                scalar,
                domain_point: Some((family.domain(), values.to_vec())),
            },
        ));
    }
    Ok(())
}
