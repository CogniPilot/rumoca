//! Lower the complete MLS §8.6 system in one initialization context.

use super::events::structured::derive_affine_program_certificate;
use super::{ContinuousRowIndex, ScalarCompiler, ScalarRowSource, ScalarRows, scalar_count};
use super::{initial_discrete, initial_parameters, initial_pins, initial_projection};
use crate::{LowerError, layout::LoweredLayout};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_phase_structural as structural;
use std::collections::HashMap;

pub(super) fn lower_initialization<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    derivatives: &ContinuousRowIndex<'dae>,
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
    let mut rows = InitializationRows::default();
    let mut row_incidence: Vec<initial_projection::InitialRowIncidence<'dae>> = Vec::new();
    lower_declared_rows(context, &mut rows, &mut row_incidence)?;
    let transferred =
        initial_pins::lower_transferred_initial_values(view, layout, &ownership, pins)?;
    // MLS §8.6 solves the states and the `fixed = false` parameters together; the
    // space names which coordinate of each kind the projection may own, and which
    // a declaration has already determined: a seeded start, or one assigned
    // from a settable parameter before the projection.
    let known_states = transferred
        .given_state_indices
        .iter()
        .chain(&transferred.assigned_state_indices)
        .copied()
        .collect::<Vec<_>>();
    let space = initial_projection::initialization_unknown_space(
        initial_projection::InitializationUnknownInputs {
            view,
            layout,
            ownership: &ownership,
            derivatives,
            given_state_indices: &known_states,
        },
    )?;
    rows.extend(transferred.checks);
    row_incidence.extend(transferred.check_incidence);
    let manifold_start = rows.len();
    lower_manifold_rows(context, manifold, &mut rows, &mut row_incidence)?;
    let manifold_row_count = rows.len() - manifold_start;
    let plan = initial_projection::plan_initialization_projection(&space, &row_incidence)?;
    let mut updates = initial_discrete::lower_initial_discrete_values(view, layout, &space)?;
    updates.rows.extend(transferred.start_updates);
    updates.targets.extend(transferred.start_update_targets);
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
    derivatives: &'a ContinuousRowIndex<'dae>,
    ownership: &'a initial_parameters::InitializationParameterOwnership<'dae>,
}

/// The initialization residual under construction: scalar rows, and compact
/// tensor nodes for structured families whose points share one affine program.
/// Every row writes the residual output at its own position.
#[derive(Default)]
struct InitializationRows {
    nodes: Vec<solve::ComputeNode>,
    scalars: ScalarRows,
    len: usize,
}

impl InitializationRows {
    fn len(&self) -> usize {
        self.len
    }

    fn push(&mut self, program: Vec<solve::LinearOp>, span: rumoca_core::Span) {
        self.scalars.push(program, span, self.len);
        self.len += 1;
    }

    fn extend(&mut self, other: ScalarRows) {
        for (program, span) in other.into_programs() {
            self.push(program, span);
        }
    }

    /// Append a tensor node owning the next `count` residual outputs.
    fn push_tensor(&mut self, node: solve::ComputeNode, count: usize) -> Result<(), LowerError> {
        self.flush()?;
        self.nodes.push(node);
        self.len += count;
        Ok(())
    }

    fn flush(&mut self) -> Result<(), LowerError> {
        let scalars = std::mem::take(&mut self.scalars);
        if scalars.len() > 0 {
            self.nodes.extend(scalars.into_compute_block()?.nodes);
        }
        Ok(())
    }

    fn into_compute_block(mut self) -> Result<solve::ComputeBlock, LowerError> {
        self.flush()?;
        Ok(solve::ComputeBlock { nodes: self.nodes })
    }
}

fn lower_declared_rows<'dae>(
    context: InitializationRowContext<'_, 'dae>,
    rows: &mut InitializationRows,
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
                    rows.push(program, equation.provenance().span());
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
    rows: &mut InitializationRows,
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
            rows.push(program, node.provenance().span());
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

/// Lower a structured initialization family. Its points' programs are
/// compiled in domain order; a single-body family whose programs share one
/// affine base program becomes one `Map` node writing its rows' consecutive
/// residual outputs (SPEC_0032 §4), so the residual keeps the family compact.
/// Otherwise every point keeps its scalar row. Row order and incidence are the
/// same either way.
fn lower_initialization_family<'dae>(
    context: InitializationRowContext<'_, 'dae>,
    family: dae::StructuredFamilyView<'dae>,
    rows: &mut InitializationRows,
    incidence: &mut Vec<initial_projection::InitialRowIncidence<'dae>>,
) -> Result<(), LowerError> {
    let domain = context
        .view
        .domain(family.domain())
        .expect("checked family domain resolves");
    let points = domain
        .structured()
        .index_tuples()
        .expect("checked domain remains valid");
    let span = family.provenance().span();
    let mut programs = Vec::with_capacity(points.len() * family.bodies().len());
    for (point, values) in points.iter().enumerate() {
        let scalar = family
            .scalar_view()
            .body_scalar(point, domain.extents())
            .expect("checked family view projects its domain point");
        lower_initialization_family_point(
            context,
            family,
            scalar,
            values,
            &mut programs,
            incidence,
        )?;
    }
    if family.bodies().len() == 1
        && let Some(node) =
            compact_family_node(domain.structured(), &points, &programs, rows.len(), span)?
    {
        return rows.push_tensor(node, programs.len());
    }
    for program in programs {
        rows.push(program, span);
    }
    Ok(())
}

/// The `Map` node of a single-body family, when every point's program is the
/// base program with affine load and constant strides. Its outputs are the
/// family's rows, starting at residual output `first_output`.
fn compact_family_node(
    domain: &rumoca_core::StructuredIndexDomain,
    points: &[Vec<i64>],
    programs: &[Vec<solve::LinearOp>],
    first_output: usize,
    span: rumoca_core::Span,
) -> Result<Option<solve::ComputeNode>, LowerError> {
    let Some(base_point) = points.first() else {
        return Ok(None);
    };
    let Ok((base_ops, load_strides, const_strides)) =
        derive_affine_program_certificate(domain, base_point, points, programs, span)
    else {
        return Ok(None);
    };
    let output_map =
        solve::TensorOutputMap::dense_contiguous(first_output, domain).map_err(|_| {
            LowerError::contract("structured initial residual output map overflow", span)
        })?;
    Ok(Some(solve::ComputeNode::Map {
        domain: domain.clone(),
        output_map,
        base_ops,
        load_strides,
        const_strides,
        metadata: solve::TensorNodeMetadata::default(),
        span,
    }))
}

fn lower_initialization_family_point<'dae>(
    context: InitializationRowContext<'_, 'dae>,
    family: dae::StructuredFamilyView<'dae>,
    scalar: usize,
    values: &[i64],
    programs: &mut Vec<Vec<solve::LinearOp>>,
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
        programs.push(program);
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
