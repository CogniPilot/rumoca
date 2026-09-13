//! Which owner determines each parameter coordinate at the MLS §8.6 instant.
//!
//! MLS 3.6 §8.6: "All variables declared as parameter having `fixed = false`
//! are treated as unknowns during the initialization phase, i.e., there must be
//! additional equations for them — and the start-value can be used as a
//! guess-value during initialization."
//!
//! The parameter set is evaluated once, before any trajectory exists, so the
//! only number it has for such a parameter is that guess. A parameter whose
//! binding transitively reads one is therefore *not* finished by the parameter
//! set: the number it computed is a seed, not the value.
//!
//! Every parameter the initialization system determines has exactly one owner
//! here, and the owners are disjoint by construction:
//!
//! * **an initial definition** owns a non-Real `fixed = false` parameter whose
//!   explicit initial equation defines it from initialization unknowns.
//! * **the projection** owns an otherwise unbound `fixed = false` parameter. It is
//!   the guess MLS §8.6 names, and the initialization equations solve it.
//! * **a binding** owns every parameter that has one. MLS 3.6 §8.6: "In the case
//!   a parameter has both a binding equation and `fixed = false` a diagnostic is
//!   recommended, but the parameter should be solved from the binding equation."
//!   So a bound parameter is never also a projection unknown — offering the same
//!   coordinate to both leaves the projection and the update row fighting over
//!   one storage slot.
//!
//! A bound parameter that reads a projection unknown is re-derived two ways, and
//! both are needed:
//!
//! * as an **update row**, so the coordinate itself ends the initialization
//!   holding the binding's value rather than the parameter set's seed; and
//! * as a **substitution** inside every initialization residual that reads it,
//!   so the projection solves the guessed unknowns *simultaneously* with the
//!   binding instead of iterating against a stale number. Without the
//!   substitution the settle loop is a Gauss–Seidel sweep whose fixed point only
//!   exists when the binding's loop gain is below one: `parameter Real g = 2*q`
//!   with `initial equation g + q = 30` has gain `-2` and diverges, though it is
//!   well posed at `q = 10`, `g = 20`.
//!
//! Bindings that read independent parameters also retain update owners: an FMI
//! importer may change those parameters after compilation. Literal bindings
//! remain independent values. Explicit scalar overrides retain their supplied
//! values instead of being overwritten by the original binding.
//!
//! The §8.6 sentence above also *recommends* a diagnostic when a parameter has
//! both a binding equation and `fixed = false`. This phase deliberately emits
//! none: the recommendation is not a requirement, the declaration has one
//! unambiguous reading ("solved from the binding equation") which is the reading
//! implemented here, and OMC accepts the same models with a "probably redundant"
//! warning. Refusing or warning would add noise to models every tool runs.
//!
//! Ownership disjointness is *checked*, not assumed. A parameter row that does
//! not land in parameter storage is a hard error ([`push_dependent_binding_rows`]),
//! and a carried §8.6 initial value that does not land in solver storage is a
//! hard error in the sibling pin lane (`initial_pins::lower_transferred_initial_values`),
//! so the two lanes cannot silently write one another's slots.
//!
//! This module decides *who owns* each parameter coordinate. Which row solves an
//! owned coordinate — the bipartite matching over the whole §8.6 unknown space,
//! parameters and states together — belongs to the sibling `initial_projection`.

use std::collections::{BTreeMap, BTreeSet, HashMap};

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::{DerivativeRowIndex, ScalarRows, variable_scalar_slot};
use crate::LowerError;
use crate::layout::LoweredLayout;
use crate::lower::scalar::{ParameterBindingSubstitutions, ScalarCompiler};

pub(super) struct InitialParameterUpdates {
    pub(super) rows: ScalarRows,
    pub(super) targets: Vec<solve::ScalarSlot>,
}

/// The single owner assignment for every parameter the initialization determines.
pub(super) struct InitializationParameterOwnership<'dae> {
    /// P-slot indices of each unbound `fixed = false` parameter scalar, keyed by
    /// variable index. These are the coordinates the projection solves.
    projection_unknowns: HashMap<u32, Vec<usize>>,
    /// Initialization definitions and numeric dependent bindings, ordered so
    /// each update follows the parameter definitions it reads.
    dependents: Vec<u32>,
    bound: BTreeMap<u32, BoundParameter<'dae>>,
    substitutions: ParameterBindingSubstitutions<'dae>,
    overridden_scalars: BTreeSet<(u32, usize)>,
}

impl<'dae> InitializationParameterOwnership<'dae> {
    /// The projection unknown slots a parameter occupies, when the projection is
    /// its owner.
    pub(super) fn projection_unknown_slots(&self, parameter: u32) -> Option<&[usize]> {
        self.projection_unknowns.get(&parameter).map(Vec::as_slice)
    }

    /// Every parameter scalar the initialization projection owns. The order is
    /// irrelevant to incidence construction; the caller collects into a
    /// deterministic set before issuing a plan.
    pub(super) fn all_projection_unknown_slots(&self) -> impl Iterator<Item = usize> + '_ {
        self.projection_unknowns
            .values()
            .flat_map(|indices| indices.iter().copied())
    }

    /// The binding an initialization residual must recompute for a parameter,
    /// when a binding is its owner and it reads a projection unknown.
    pub(super) fn substitution(&self, parameter: u32) -> Option<dae::ExprId<'dae>> {
        self.substitutions.binding(parameter)
    }

    pub(super) const fn substitutions(&self) -> &ParameterBindingSubstitutions<'dae> {
        &self.substitutions
    }

    /// Apply dependent parameter bindings in dependency order.
    ///
    /// The residual the projection iterates already recomputes these bindings
    /// through [`Self::substitutions`], so this row is what leaves the *stored*
    /// coordinate holding the solved value once the projection has finished.
    pub(super) fn lower_solved_parameter_reads(
        &self,
        view: dae::DaeView<'dae>,
        layout: &LoweredLayout<'dae>,
        derivatives: &DerivativeRowIndex<'dae>,
    ) -> Result<InitialParameterUpdates, LowerError> {
        let mut updates = InitialParameterUpdates {
            rows: ScalarRows::default(),
            targets: Vec::new(),
        };
        for index in self.dependents.iter().copied() {
            let Some(parameter) = self.bound.get(&index) else {
                continue;
            };
            push_dependent_binding_rows(
                view,
                layout,
                derivatives,
                index,
                parameter,
                &self.overridden_scalars,
                &mut updates,
            )?;
        }
        Ok(updates)
    }
}

/// Emit one update row per scalar of a dependent parameter's binding.
fn push_dependent_binding_rows<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    derivatives: &DerivativeRowIndex<'dae>,
    index: u32,
    parameter: &BoundParameter<'dae>,
    overridden_scalars: &BTreeSet<(u32, usize)>,
    updates: &mut InitialParameterUpdates,
) -> Result<(), LowerError> {
    let variable = parameter.variable;
    let span = variable.declaration().span();
    for scalar in 0..variable.scalar_count() {
        if overridden_scalars.contains(&(index, scalar)) {
            continue;
        }
        let program = ScalarCompiler::new(view, layout, None)
            .with_derivative_definitions(derivatives)
            .program(parameter.binding, scalar)
            .map_err(|error| dependent_binding_error(error, variable, span))?;
        let target = variable_scalar_slot(layout, index, scalar, span)?;
        let solve::ScalarSlot::P { .. } = target else {
            return Err(LowerError::contract(
                format!(
                    "dependent parameter `{}` does not occupy \
                     parameter storage",
                    variable.name()
                ),
                span,
            ));
        };
        let output = updates.rows.len();
        updates.rows.push(program, span, output);
        updates.targets.push(target);
    }
    Ok(())
}

/// Partition the parameters the MLS §8.6 initialization system determines.
pub(super) fn initialization_parameter_ownership<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    overrides: &HashMap<String, f64>,
) -> Result<InitializationParameterOwnership<'dae>, LowerError> {
    let bound = bound_parameters(view)?;
    let projection_unknowns = projection_unknown_slots(view, layout, &bound)?;
    let reads: BTreeMap<u32, BTreeSet<u32>> = bound
        .iter()
        .map(|(index, parameter)| (*index, parameter.reads.clone()))
        .collect();
    let initialized: BTreeSet<u32> = bound
        .iter()
        .filter_map(|(id, parameter)| parameter.initialization_defined.then_some(*id))
        .collect();
    let guessed: BTreeSet<u32> = projection_unknowns
        .keys()
        .copied()
        .chain(initialized.iter().copied())
        .collect();
    let mut guessed_dependents = dependent_parameters(&reads, &guessed, &bound);
    guessed_dependents.extend(&initialized);
    let substitutions = ParameterBindingSubstitutions::new(
        guessed_dependents
            .iter()
            .filter_map(|index| Some((*index, bound.get(index)?.binding)))
            .collect(),
    );
    let dependents = ordered_dependents(
        &reads,
        &bound
            .iter()
            .filter_map(|(id, parameter)| {
                (is_numeric_parameter(parameter.variable)
                    && (parameter.initialization_defined || !parameter.reads.is_empty()))
                .then_some(*id)
            })
            .collect(),
        &bound,
    )?;
    let overridden_scalars = bound
        .iter()
        .filter(|(_, parameter)| !parameter.initialization_defined)
        .flat_map(|(id, parameter)| {
            (0..parameter.variable.scalar_count()).filter_map(|scalar| {
                let name = parameter.variable.scalar_name(scalar)?;
                overrides.contains_key(&name).then_some((*id, scalar))
            })
        })
        .collect();
    Ok(InitializationParameterOwnership {
        projection_unknowns,
        dependents,
        bound,
        substitutions,
        overridden_scalars,
    })
}

/// P-slot indices of every `fixed = false` parameter scalar the projection owns.
///
/// A `fixed = false` parameter that also carries a binding is excluded: MLS 3.6
/// §8.6 solves it from that binding, so making it an unknown as well would give
/// one storage slot two owners — the projection writing the value its block
/// implies and the update row overwriting it with the binding's.
fn projection_unknown_slots<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    bound: &BTreeMap<u32, BoundParameter<'dae>>,
) -> Result<HashMap<u32, Vec<usize>>, LowerError> {
    let mut slots = HashMap::new();
    for (id, variable) in view.variables() {
        if variable.role() != dae::VariableRole::Parameter || variable.fixed() != Some(false) {
            continue;
        }
        if bound.contains_key(&id.index()) {
            continue;
        }
        let span = variable.declaration().span();
        let mut indices = Vec::with_capacity(variable.scalar_count());
        for scalar in 0..variable.scalar_count() {
            let solve::ScalarSlot::P { index, .. } =
                variable_scalar_slot(layout, id.index(), scalar, span)?
            else {
                return Err(LowerError::contract(
                    format!(
                        "`fixed = false` parameter `{}` does not occupy parameter storage",
                        variable.name()
                    ),
                    span,
                ));
            };
            indices.push(index);
        }
        if !indices.is_empty() {
            slots.insert(id.index(), indices);
        }
    }
    Ok(slots)
}

/// Name the owner that is absent rather than the consequence of it.
///
/// The binding is legal Modelica; what it lacks is a lowering that can be
/// re-applied at the initialization instant. Leaving the original error would
/// blame the expression shape for a requirement only this ordering imposes.
///
/// Only an unsupported shape is re-mapped that way. A contract violation is a
/// broken internal invariant, not a capability this ordering asks for, so it
/// keeps its own typed identity and surfaces unchanged.
fn dependent_binding_error(
    error: LowerError,
    variable: dae::VariableView<'_>,
    span: rumoca_core::Span,
) -> LowerError {
    let LowerError::Unsupported { reason, .. } = &error else {
        return error;
    };
    LowerError::unsupported(
        format!(
            "parameter `{}` depends on other parameters, so its binding must be \
             re-applied after parameter changes, and its expression \
             has no checked initialization-update lowering: {reason}",
            variable.name()
        ),
        span,
    )
}

struct BoundParameter<'dae> {
    variable: dae::VariableView<'dae>,
    binding: dae::ExprId<'dae>,
    reads: BTreeSet<u32>,
    initialization_defined: bool,
}

/// Every parameter that has a binding, with the parameter coordinates it reads.
fn bound_parameters(
    view: dae::DaeView<'_>,
) -> Result<BTreeMap<u32, BoundParameter<'_>>, LowerError> {
    let initialized: BTreeMap<_, _> = view
        .initial_parameter_values()
        .map(|definition| (definition.target().index(), definition.value()))
        .collect();
    let mut bound = BTreeMap::new();
    for (id, variable) in view.variables() {
        if variable.role() != dae::VariableRole::Parameter {
            continue;
        }
        let initialization = initialized.get(&id.index()).copied();
        if initialization.is_some() && !is_numeric_parameter(variable) {
            return Err(LowerError::unsupported(
                "initialization-determined String parameters have no checked Solve storage",
                variable.declaration().span(),
            ));
        }
        let Some(binding) = initialization.or_else(|| variable.binding()) else {
            continue;
        };
        let mut reads = BTreeSet::new();
        dae::for_each_expression(view, binding, |_, expression| {
            if let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(parameter)) =
                expression.operation()
            {
                reads.insert(parameter.index());
            }
        });
        bound.insert(
            id.index(),
            BoundParameter {
                variable,
                binding,
                reads,
                initialization_defined: initialization.is_some(),
            },
        );
    }
    Ok(bound)
}

/// A parameter whose value the Solve program can carry in a numeric coordinate.
///
/// String bindings are evaluated by the parameter set. An explicit initial
/// definition of a String needs separate storage and is rejected above.
fn is_numeric_parameter(variable: dae::VariableView<'_>) -> bool {
    !matches!(variable.value_type().scalar_type(), dae::ScalarType::String)
}

/// The bound parameters whose binding transitively reads a guessed parameter.
///
/// A guessed parameter is never its own dependent: it has no binding to
/// re-apply, and the projection owns its value.
fn dependent_parameters(
    reads: &BTreeMap<u32, BTreeSet<u32>>,
    guessed: &BTreeSet<u32>,
    bound: &BTreeMap<u32, BoundParameter<'_>>,
) -> BTreeSet<u32> {
    let mut dependents: BTreeSet<u32> = BTreeSet::new();
    loop {
        let mut grew = false;
        for (variable, read) in reads {
            if guessed.contains(variable) || dependents.contains(variable) {
                continue;
            }
            if !bound
                .get(variable)
                .is_some_and(|parameter| is_numeric_parameter(parameter.variable))
            {
                continue;
            }
            if read
                .iter()
                .any(|read| guessed.contains(read) || dependents.contains(read))
            {
                dependents.insert(*variable);
                grew = true;
            }
        }
        if !grew {
            return dependents;
        }
    }
}

/// Order the dependents so a binding is re-applied after everything it reads.
///
/// Substitution requires an acyclic definition graph. A dependency cycle needs
/// a coupled initialization owner instead of a declaration-order evaluation.
fn ordered_dependents(
    reads: &BTreeMap<u32, BTreeSet<u32>>,
    dependents: &BTreeSet<u32>,
    bound: &BTreeMap<u32, BoundParameter<'_>>,
) -> Result<Vec<u32>, LowerError> {
    let mut pending: BTreeMap<u32, BTreeSet<u32>> = dependents
        .iter()
        .map(|variable| {
            let blockers = reads
                .get(variable)
                .map(|read| read.intersection(dependents).copied().collect())
                .unwrap_or_default();
            (*variable, blockers)
        })
        .collect();
    let mut order = Vec::with_capacity(dependents.len());
    while let Some((&target, _)) = pending.first_key_value() {
        let ready: Vec<u32> = match pending
            .iter()
            .filter(|(_, blockers)| blockers.is_empty())
            .map(|(variable, _)| *variable)
            .collect::<Vec<_>>()
        {
            ready if ready.is_empty() => {
                return Err(LowerError::unsupported(
                    "cyclic initialization parameter definitions require a coupled owner",
                    bound[&target].variable.declaration().span(),
                ));
            }
            ready => ready,
        };
        for variable in ready {
            pending.remove(&variable);
            for blockers in pending.values_mut() {
                blockers.remove(&variable);
            }
            order.push(variable);
        }
    }
    Ok(order)
}
