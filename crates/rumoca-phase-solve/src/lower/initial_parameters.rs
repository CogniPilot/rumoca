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
//! here, and the two owners are disjoint by construction:
//!
//! * **the projection** owns a `fixed = false` parameter with no binding. It is
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
//! Only the dependency cases are ordered this way. A binding that reads nothing
//! the initialization system owns keeps its exact parameter-set value and emits
//! neither, so the value-equivalence the calculated-parameter owner claims still
//! holds for every shape that never touches an initialization unknown.
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

use std::collections::{BTreeMap, BTreeSet, HashMap};

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::{ScalarRows, variable_scalar_slot};
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
    /// Bound parameters whose binding transitively reads a projection unknown,
    /// ordered so a binding follows everything it reads.
    dependents: Vec<u32>,
    bound: BTreeMap<u32, BoundParameter<'dae>>,
    substitutions: ParameterBindingSubstitutions<'dae>,
}

impl<'dae> InitializationParameterOwnership<'dae> {
    /// The projection unknown slots a parameter occupies, when the projection is
    /// its owner.
    pub(super) fn projection_unknown_slots(&self, parameter: u32) -> Option<&[usize]> {
        self.projection_unknowns.get(&parameter).map(Vec::as_slice)
    }

    /// The binding an initialization residual must recompute for a parameter,
    /// when a binding is its owner and it reads a projection unknown.
    pub(super) fn substitution(&self, parameter: u32) -> Option<dae::ExprId<'dae>> {
        self.substitutions.binding(parameter)
    }

    pub(super) const fn substitutions(&self) -> &ParameterBindingSubstitutions<'dae> {
        &self.substitutions
    }

    pub(super) fn has_projection_unknowns(&self) -> bool {
        !self.projection_unknowns.is_empty()
    }

    /// Re-apply, at the initialization instant, every parameter binding whose
    /// value the parameter set could only guess.
    ///
    /// The residual the projection iterates already recomputes these bindings
    /// through [`Self::substitutions`], so this row is what leaves the *stored*
    /// coordinate holding the solved value once the projection has finished.
    pub(super) fn lower_solved_parameter_reads(
        &self,
        view: dae::DaeView<'dae>,
        layout: &LoweredLayout<'dae>,
    ) -> Result<InitialParameterUpdates, LowerError> {
        let mut updates = InitialParameterUpdates {
            rows: ScalarRows::default(),
            targets: Vec::new(),
        };
        for index in self.dependents.iter().copied() {
            let Some(parameter) = self.bound.get(&index) else {
                continue;
            };
            push_dependent_binding_rows(view, layout, index, parameter, &mut updates)?;
        }
        Ok(updates)
    }
}

/// Emit one update row per scalar of a dependent parameter's binding.
fn push_dependent_binding_rows<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    index: u32,
    parameter: &BoundParameter<'dae>,
    updates: &mut InitialParameterUpdates,
) -> Result<(), LowerError> {
    let variable = parameter.variable;
    let span = variable.declaration().span();
    for scalar in 0..variable.scalar_count() {
        let program = ScalarCompiler::new(view, layout, None)
            .program(parameter.binding, scalar)
            .map_err(|error| dependent_binding_error(error, variable, span))?;
        let target = variable_scalar_slot(layout, index, scalar, span)?;
        let solve::ScalarSlot::P { .. } = target else {
            return Err(LowerError::contract(
                format!(
                    "parameter `{}` reads an initialization unknown but does not occupy \
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
) -> Result<InitializationParameterOwnership<'dae>, LowerError> {
    let bound = bound_parameters(view);
    let projection_unknowns = projection_unknown_slots(view, layout, &bound)?;
    let reads: BTreeMap<u32, BTreeSet<u32>> = bound
        .iter()
        .map(|(index, parameter)| (*index, parameter.reads.clone()))
        .collect();
    let guessed: BTreeSet<u32> = projection_unknowns.keys().copied().collect();
    let dependents = if guessed.is_empty() {
        Vec::new()
    } else {
        ordered_dependents(&reads, &dependent_parameters(&reads, &guessed, &bound))
    };
    let substitutions = ParameterBindingSubstitutions::new(
        dependents
            .iter()
            .filter_map(|index| Some((*index, bound.get(index)?.binding)))
            .collect(),
    );
    Ok(InitializationParameterOwnership {
        projection_unknowns,
        dependents,
        bound,
        substitutions,
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
            "parameter `{}` reads a `fixed = false` parameter the initialization system \
             solves, so its binding must be re-applied after that solve, and its expression \
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
}

/// Every parameter that has a binding, with the parameter coordinates it reads.
fn bound_parameters(view: dae::DaeView<'_>) -> BTreeMap<u32, BoundParameter<'_>> {
    let mut bound = BTreeMap::new();
    for (id, variable) in view.variables() {
        if variable.role() != dae::VariableRole::Parameter {
            continue;
        }
        let Some(binding) = variable.binding() else {
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
            },
        );
    }
    bound
}

/// A parameter whose value the Solve program can carry in a numeric coordinate.
///
/// A `String` parameter has no numeric storage at all, so it can never be an
/// initialization update row or a residual substitution. That is not a missing
/// capability: MLS §8.6 gives it no equation either, so the parameter set's
/// ordinary evaluation of its binding is already its whole determination — the
/// only reason it appears here is that its binding happens to mention a solved
/// parameter through `String(...)`. Rejecting it would refuse a legal model for
/// an ordering it does not participate in.
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
/// `apply_initialization_updates` iterates to a fixed point either way, so this
/// only decides how many passes that takes — but a total order also keeps the
/// emitted row sequence reproducible across runs.
fn ordered_dependents(
    reads: &BTreeMap<u32, BTreeSet<u32>>,
    dependents: &BTreeSet<u32>,
) -> Vec<u32> {
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
    while !pending.is_empty() {
        // A binding cycle among parameters is rejected before Solve lowering.
        // Falling back to declaration order keeps this ordering total instead
        // of dropping a row if one ever reached here.
        let ready: Vec<u32> = match pending
            .iter()
            .filter(|(_, blockers)| blockers.is_empty())
            .map(|(variable, _)| *variable)
            .collect::<Vec<_>>()
        {
            ready if ready.is_empty() => pending.keys().copied().collect(),
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
    order
}

pub(super) struct InitialParameterProjection {
    pub(super) unknowns: Vec<solve::ScalarSlot>,
    pub(super) plan: solve::InitializationProjectionPlan,
}

/// Plan the initialization unknowns that only the initialization system can determine.
///
/// MLS §8.6 gives a `parameter` declared `fixed = false` no value of its own: its `start`
/// attribute is the iteration guess and the initialization equations are what actually
/// determine it (`Modelica.Electrical.Analog.Basic.SaturatingInductor.Ipar` is the
/// canonical case). Without a projection block the runtime can only *check* such a row,
/// so the guess survives into the trajectory — a numerically plausible but wrong run, or
/// the residual failure this planner removes.
///
/// Only rows whose sole unknowns are `fixed = false` parameters are planned here. A row
/// that also reads a state, algebraic, or discrete coordinate belongs to the full
/// initialization system, which has no checked plan yet; leaving it unplanned keeps its
/// typed residual failure instead of inventing a block that cannot be square.
pub(super) fn plan_initial_parameter_projection<'dae>(
    view: dae::DaeView<'dae>,
    ownership: &InitializationParameterOwnership<'dae>,
    rows: &[InitialRowIncidence<'dae>],
) -> Result<InitialParameterProjection, LowerError> {
    if !ownership.has_projection_unknowns() {
        return Ok(InitialParameterProjection {
            unknowns: Vec::new(),
            plan: solve::InitializationProjectionPlan::default(),
        });
    }
    let mut incidence: Vec<(usize, BTreeSet<usize>)> = Vec::new();
    for (row, source) in rows.iter().enumerate() {
        let Some(unknowns) = row_parameter_unknowns(view, ownership, source) else {
            continue;
        };
        if unknowns.is_empty() {
            continue;
        }
        incidence.push((row, unknowns));
    }
    let mut blocks = Vec::new();
    let mut unknowns = Vec::new();
    for component in connected_components(&incidence) {
        // A block the runtime can solve has one equation per unknown, and the
        // matching is what names those equations. A component with rows to
        // spare is legal — MLS §8.6 lets a parameter be solved from its binding
        // *and* be read by another initialization equation — so the surplus rows
        // stay consistency checks that the complete-residual test at the end of
        // the projection still has to satisfy. A component that cannot match
        // every unknown is under-determined by the rows this planner can see; it
        // keeps the residual check rather than becoming a non-square block.
        //
        // The matching is structural, so it is rank-blind: it takes the first
        // augmenting assignment, which can pick a block whose Jacobian is
        // numerically singular while a different assignment of the same rows
        // would not be. The runtime reports that as a failed solve rather than a
        // wrong answer, and a minimum-degree ordering would choose better; until
        // then the risk is accepted and named here rather than left implied.
        let Some(matched) = match_rows_to_unknowns(&component) else {
            continue;
        };
        let slots = component
            .unknowns
            .iter()
            .map(|index| solve::scalar_slot_p(*index))
            .collect::<Vec<_>>();
        unknowns.extend(slots.iter().copied());
        blocks.push(solve::InitializationProjectionBlock {
            rows: matched,
            unknowns: slots,
        });
    }
    Ok(InitialParameterProjection {
        unknowns,
        plan: solve::InitializationProjectionPlan { blocks },
    })
}

/// Give every unknown of a component a distinct row that reads it.
///
/// Returns the matched rows, positionally paired with `component.unknowns`, or
/// `None` when no such assignment exists. This is the Hungarian augmenting-path
/// search over the row/unknown bipartite graph: each round either matches the
/// next unknown to a free row or reroutes an already-matched row to make one
/// free, and an unknown that neither reaches is one no row can determine.
fn match_rows_to_unknowns(component: &ProjectionComponent) -> Option<Vec<usize>> {
    let row_count = component.rows.len();
    let unknown_count = component.unknowns.len();
    let position: HashMap<usize, usize> = component
        .unknowns
        .iter()
        .enumerate()
        .map(|(position, unknown)| (*unknown, position))
        .collect();
    let mut adjacency: Vec<Vec<usize>> = vec![Vec::new(); unknown_count];
    for (row, unknowns) in component.row_unknowns.iter().enumerate() {
        for unknown in unknowns {
            if let Some(unknown) = position.get(unknown) {
                adjacency[*unknown].push(row);
            }
        }
    }
    let mut matching = RowUnknownMatching {
        row_of_unknown: vec![None; unknown_count],
        unknown_of_row: vec![None; row_count],
    };
    for start in 0..unknown_count {
        if !matching.augment_from(start, &adjacency) {
            return None;
        }
    }
    matching
        .row_of_unknown
        .into_iter()
        .map(|row| Some(component.rows[row?]))
        .collect()
}

/// A partial assignment of component rows to the unknowns they determine.
struct RowUnknownMatching {
    row_of_unknown: Vec<Option<usize>>,
    unknown_of_row: Vec<Option<usize>>,
}

impl RowUnknownMatching {
    /// Grow the matching by one, starting the alternating search at `start`.
    ///
    /// Returns whether an augmenting path was found. `adjacency` lists, per
    /// unknown, the component-local rows that read it.
    fn augment_from(&mut self, start: usize, adjacency: &[Vec<usize>]) -> bool {
        let mut search = AlternatingSearch {
            visited_row: vec![false; self.unknown_of_row.len()],
            visited_unknown: vec![false; self.row_of_unknown.len()],
            reached_from: vec![None; self.unknown_of_row.len()],
            queue: std::collections::VecDeque::from([start]),
        };
        search.visited_unknown[start] = true;
        while let Some(unknown) = search.queue.pop_front() {
            let Some(free_row) = self.reach_rows_of(unknown, adjacency, &mut search) else {
                continue;
            };
            self.flip_path_to(free_row, &search.reached_from);
            return true;
        }
        false
    }

    /// Visit every not-yet-reached row this unknown occurs in.
    ///
    /// Returns the first row that no unknown holds yet — the end of an
    /// augmenting path. A row that is already matched instead enqueues the
    /// unknown holding it, which is how the search looks for a reroute.
    fn reach_rows_of(
        &self,
        unknown: usize,
        adjacency: &[Vec<usize>],
        search: &mut AlternatingSearch,
    ) -> Option<usize> {
        for row in adjacency[unknown].iter().copied() {
            if search.visited_row[row] {
                continue;
            }
            search.visited_row[row] = true;
            search.reached_from[row] = Some(unknown);
            let Some(matched) = self.unknown_of_row[row] else {
                return Some(row);
            };
            if !search.visited_unknown[matched] {
                search.visited_unknown[matched] = true;
                search.queue.push_back(matched);
            }
        }
        None
    }

    /// Walk the alternating path back to its free unknown, flipping each edge.
    ///
    /// Every row on the path is reassigned to the unknown that reached it, and
    /// the unknown it displaced takes the row *it* had reached — so the path
    /// ends at the search's starting unknown, which had no row, and the matching
    /// grows by exactly one.
    fn flip_path_to(&mut self, free_row: usize, reached_from: &[Option<usize>]) {
        let mut row = free_row;
        while let Some(unknown) = reached_from[row] {
            let displaced = self.row_of_unknown[unknown];
            self.row_of_unknown[unknown] = Some(row);
            self.unknown_of_row[row] = Some(unknown);
            let Some(displaced) = displaced else {
                return;
            };
            row = displaced;
        }
    }
}

/// Breadth-first state of one alternating-path search.
struct AlternatingSearch {
    visited_row: Vec<bool>,
    visited_unknown: Vec<bool>,
    reached_from: Vec<Option<usize>>,
    queue: std::collections::VecDeque<usize>,
}

/// Where one initialization row's parameter incidence is read from.
///
/// The two forms differ in what the walk may start from, not in what it proves.
pub(super) enum InitialRowIncidence<'dae> {
    /// Nothing this planner can read: a multi-scalar equation, whose per-scalar
    /// unknowns a whole-expression walk cannot separate.
    Opaque,
    /// One whole-model residual expression. Every coordinate it reaches is an
    /// unknown of the row, so a coordinate the projection cannot own disqualifies
    /// the row.
    Residual(dae::ExprId<'dae>),
    /// A stated initial value carried onto a coordinate that already holds one
    /// (`initial_pins`). The row is `coordinate - Σ terms`, and the coordinate is
    /// a state the initialization instant has already seeded, so only the terms
    /// carry unknowns — which is exactly what lets such a row determine a
    /// `fixed = false` parameter its displacement reads.
    CarriedValue(Vec<dae::ExprId<'dae>>),
}

/// The `fixed = false` parameter scalars a row reads, or `None` when it also
/// reads a coordinate the initialization projection cannot own.
///
/// A parameter a binding owns is not itself an unknown, but the binding the row
/// was lowered to recompute is: the walk follows that binding so the incidence
/// matches the program the projection actually evaluates.
fn row_parameter_unknowns<'dae>(
    view: dae::DaeView<'dae>,
    ownership: &InitializationParameterOwnership<'dae>,
    row: &InitialRowIncidence<'dae>,
) -> Option<BTreeSet<usize>> {
    let pending = match row {
        InitialRowIncidence::Opaque => return None,
        InitialRowIncidence::Residual(residual) => vec![*residual],
        InitialRowIncidence::CarriedValue(terms) => terms.clone(),
    };
    let mut incidence = ParameterIncidence {
        unknowns: BTreeSet::new(),
        owned: true,
        substituted: BTreeSet::new(),
        pending,
    };
    while let Some(expression) = incidence.pending.pop() {
        dae::for_each_expression(view, expression, |_, node| {
            let dae::ExpressionOperation::Coordinate(coordinate) = node.operation() else {
                return;
            };
            incidence.visit(ownership, coordinate);
        });
    }
    incidence.owned.then_some(incidence.unknowns)
}

/// The projection unknowns one initialization residual program reaches.
struct ParameterIncidence<'dae> {
    unknowns: BTreeSet<usize>,
    owned: bool,
    substituted: BTreeSet<u32>,
    pending: Vec<dae::ExprId<'dae>>,
}

impl<'dae> ParameterIncidence<'dae> {
    fn visit(
        &mut self,
        ownership: &InitializationParameterOwnership<'dae>,
        coordinate: dae::CoordinateView<'dae>,
    ) {
        match coordinate {
            dae::CoordinateView::Parameter(parameter) => self.visit_parameter(ownership, parameter),
            // A domain binder and a clock interval are compile-time constants, and
            // `time` is the known initialization instant.
            dae::CoordinateView::Time
            | dae::CoordinateView::ClockInterval(_)
            | dae::CoordinateView::Binder(_) => {}
            _ => self.owned = false,
        }
    }

    fn visit_parameter(
        &mut self,
        ownership: &InitializationParameterOwnership<'dae>,
        parameter: dae::ParameterId<'dae>,
    ) {
        if let Some(indices) = ownership.projection_unknown_slots(parameter.index()) {
            self.unknowns.extend(indices.iter().copied());
            return;
        }
        if let Some(binding) = ownership.substitution(parameter.index())
            && self.substituted.insert(parameter.index())
        {
            self.pending.push(binding);
        }
    }
}

struct ProjectionComponent {
    rows: Vec<usize>,
    /// The unknowns each entry of `rows` reads, positionally paired with it.
    row_unknowns: Vec<BTreeSet<usize>>,
    unknowns: Vec<usize>,
}

/// Group rows that share a `fixed = false` parameter into one solvable component.
///
/// Two rows that read the same unknown must be solved together, so the components of the
/// row/unknown bipartite graph are the coarsest blocks that stay independent.
fn connected_components(incidence: &[(usize, BTreeSet<usize>)]) -> Vec<ProjectionComponent> {
    let mut sets = DisjointSets::new(incidence.len());
    let mut owner: HashMap<usize, usize> = HashMap::new();
    for (position, (_, unknowns)) in incidence.iter().enumerate() {
        for unknown in unknowns {
            let first = *owner.entry(*unknown).or_insert(position);
            sets.union(first, position);
        }
    }
    let mut grouped: Vec<ProjectionComponent> = Vec::new();
    let mut group_of: HashMap<usize, usize> = HashMap::new();
    for (position, (row, unknowns)) in incidence.iter().enumerate() {
        let root = sets.find(position);
        let group = *group_of.entry(root).or_insert_with(|| {
            grouped.push(ProjectionComponent {
                rows: Vec::new(),
                row_unknowns: Vec::new(),
                unknowns: Vec::new(),
            });
            grouped.len() - 1
        });
        grouped[group].rows.push(*row);
        grouped[group].row_unknowns.push(unknowns.clone());
        grouped[group].unknowns.extend(unknowns.iter().copied());
    }
    for component in &mut grouped {
        component.unknowns.sort_unstable();
        component.unknowns.dedup();
    }
    grouped
}

/// Union-find over the rows of one initialization incidence.
struct DisjointSets {
    parent: Vec<usize>,
}

impl DisjointSets {
    fn new(len: usize) -> Self {
        Self {
            parent: (0..len).collect(),
        }
    }

    fn find(&mut self, mut node: usize) -> usize {
        while self.parent[node] != node {
            self.parent[node] = self.parent[self.parent[node]];
            node = self.parent[node];
        }
        node
    }

    fn union(&mut self, left: usize, right: usize) {
        let (left, right) = (self.find(left), self.find(right));
        if left != right {
            self.parent[right] = left;
        }
    }
}
