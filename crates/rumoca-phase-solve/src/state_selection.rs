//! Static independent coordinates for structurally constrained state manifolds.

mod evaluation;
mod exchange;

use std::collections::HashMap;

use rumoca_core::StateSelect;
use rumoca_eval_solve::dense_basis::ColumnChoice;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_phase_structural::{
    AliasQuotientReport, FormalDerivativeSystem, FormalDerivativeView, FormalStageCoordinate,
    FormalStateCoordinate, PreparedDae, ReducedSelectionChart, StateSelection, StructuralError,
    construct_formal_derivatives, fold_evaluable_parameters, formal_alias_quotient_report,
    inline_annotated_calls, inline_formal_calls, prepare_for_solve, quotient_aliases,
    quotient_formal_aliases,
};

use crate::lower::typed_functions::formal_stages::lower_state_selection_stages;

use evaluation::TrialPoint;

/// The prepared reduced state selection: the primary basis every model executes,
/// plus one prepared alternate DAE per alternate reduced chart, either a mirror
/// of a folding definitional first-integral group or a single exchange of a
/// reduced constraint group.
///
/// The alternates are the same coordinate transformation as the primary run with
/// another Independent set, so each lowers through the ordinary Solve machinery
/// into its own executable reconstruction and derivative kernel. They are empty
/// for every model without such a group. The alternates align with the primary's
/// reduced chart set positionally: `alternates[k]` is the prepared DAE for
/// reduced chart `k + 1` (chart zero is the primary basis). An exchange whose
/// checked construction fails is `None`; lowering withholds its chart.
pub(crate) struct PreparedSelection<'source> {
    pub primary: PreparedDae<'source>,
    pub alternates: Vec<Option<PreparedDae<'static>>>,
    /// Coverage record of a reduced constraint group's exchanges; empty for a
    /// first-integral mirror set and for every model without such a group.
    pub exchanges: Vec<solve::ChartExchange>,
    /// The formal-derivative application of the STRUCT-T02 quotient on the
    /// primary candidate, with every class it left unchanged; empty when the
    /// source basis is retained.
    pub formal_aliases: AliasQuotientReport,
}

/// The alternate Independent sets of a primary selection, each described by
/// source variable ordinal, formal derivative order, and tensor scalar so it
/// survives the branded-coordinate lifetime, plus the exchange coverage of a
/// reduced constraint group (empty for a first-integral mirror).
#[derive(Default)]
struct AlternateSelections {
    selections: Vec<Vec<(u32, usize, u32)>>,
    exchanges: Vec<solve::ChartExchange>,
}

/// Prepare the executable selection of `model` after its STRUCT-T10(a)
/// evaluable-parameter folding, STRUCT-T10(b) annotated call inlining, and
/// STRUCT-T02 alias quotient. A model no transform changes prepares unchanged; otherwise the owned reconstruction is
/// prepared, which keeps every source declaration, so later stages read the
/// same variables and names.
pub(crate) fn prepare<'source>(
    model: &'source dae::Dae,
    overrides: &HashMap<String, f64>,
) -> Result<PreparedSelection<'source>, StructuralError> {
    let folded = fold_evaluable_parameters(model)?;
    let inlined = inline_annotated_calls(folded.as_ref().unwrap_or(model))?.or(folded);
    match (
        quotient_aliases(inlined.as_ref().unwrap_or(model))?,
        inlined,
    ) {
        (None, None) => prepare_source(model, overrides),
        (Some(quotient), _) | (None, Some(quotient)) => prepare_quotient(quotient, overrides),
    }
}

/// Prepare an owned alias quotient and detach the result from its borrow.
fn prepare_quotient(
    quotient: dae::Dae,
    overrides: &HashMap<String, f64>,
) -> Result<PreparedSelection<'static>, StructuralError> {
    let PreparedSelection {
        primary,
        alternates,
        exchanges,
        formal_aliases,
    } = prepare_source(&quotient, overrides)?;
    let primary = match primary {
        PreparedDae::Borrowed {
            pins, structural, ..
        } => Err((pins, structural)),
        PreparedDae::Transformed {
            dae,
            manifold,
            manifold_redundant,
            pins,
            structural,
            charts,
        } => Ok(PreparedDae::Transformed {
            dae,
            manifold,
            manifold_redundant,
            pins,
            structural,
            charts,
        }),
    };
    let primary = primary.unwrap_or_else(|(pins, structural)| PreparedDae::Transformed {
        dae: Box::new(quotient),
        manifold: Box::new([]),
        manifold_redundant: Box::new([]),
        pins,
        structural,
        charts: Box::new([]),
    });
    Ok(PreparedSelection {
        primary,
        alternates,
        exchanges,
        formal_aliases,
    })
}

fn prepare_source<'source>(
    model: &'source dae::Dae,
    overrides: &HashMap<String, f64>,
) -> Result<PreparedSelection<'source>, StructuralError> {
    match prepare_for_solve(model) {
        Ok(prepared) => reduce_or_retain(model, prepared, overrides),
        // The ordinary reducer cannot desingularize every constrained system: a
        // buried orientation lock (a quaternion body under a loop joint) leaves
        // an unmatched acceleration residual it reports as structurally
        // singular. The formal-derivative path differentiates and selects an
        // independent basis for exactly those coordinates, so route a singular
        // system through it before surfacing the reducer's failure. Nothing that
        // the reducer already accepts changes: this branch is reached only when
        // it fails.
        Err(error) if matches!(error, StructuralError::Singular { .. }) => {
            recover_singular_via_formal(model, overrides)?.ok_or(error)
        }
        Err(error) => Err(error),
    }
}

/// Decide the prepared system for a model the ordinary reducer accepted.
///
/// The choice is per-constraint and structural, read off the manifold the
/// structural phase already classified. A holonomic manifold constraint is
/// *definitional* when it is a conserved first integral: its lower-order form is
/// implied by the ODE, so a single differentiation reconstructs a matched state
/// derivative (a unit-quaternion norm whose rate vanishes identically under the
/// norm-preserving kinematics). It is *redundant* when it is a genuine loop
/// closure: over-determining at the position level and closed only by
/// differentiating to acceleration, which introduces a Lagrange multiplier.
///
/// Reduce iff at least one manifold constraint is redundant; retain when every
/// manifold constraint is definitional. Reducing a conserved invariant to a
/// fixed independent basis folds when a selected coordinate passes through zero,
/// whereas retaining its source coordinates and enforcing the invariant through
/// the manifold projection stays regular. A loop closure removes a shared degree
/// of freedom, so retaining it leaves a redundant acceleration residual that
/// costs the solver dearly; it must reduce. A system whose manifold mixes the two
/// across separate blocks reduces as a whole: the candidate construction cannot
/// yet retain one block while reducing another, and retaining a redundant loop
/// closure is the failure this decision exists to avoid, so the presence of any
/// redundant constraint chooses reduce.
fn reduce_or_retain<'source>(
    model: &'source dae::Dae,
    prepared: PreparedDae<'source>,
    overrides: &HashMap<String, f64>,
) -> Result<PreparedSelection<'source>, StructuralError> {
    let Some(formal) = formal_below_retained_dimension(model, &prepared)? else {
        return Ok(PreparedSelection::retained(prepared));
    };
    // Construct the reduced candidate first so an infeasible request (an
    // over-constrained `StateSelect.always`, a singular stage Jacobian) still
    // surfaces its exact typed failure rather than being masked by retention.
    let mut alternate_selections = AlternateSelections::default();
    let candidate = formal.construct_state_candidate_with_charts(|formal| {
        let (selection, alternates) = select(formal, overrides)?;
        alternate_selections = alternates;
        Ok(selection)
    })?;
    // Retain the source basis when every manifold constraint is a conserved
    // first integral, reduce when any constraint is a redundant loop closure.
    if !prepared.manifold_requires_reduction() {
        return Ok(PreparedSelection::retained(prepared));
    }
    let alternates = prepare_alternate_charts(&formal, &alternate_selections)?;
    let (primary, formal_aliases) = quotient_formal_candidate(candidate.into_prepared()?)?;
    Ok(PreparedSelection {
        primary,
        alternates,
        exchanges: alternate_selections.exchanges,
        formal_aliases,
    })
}

/// Whether Solve lowering replaces the constrained state manifold `prepared`
/// retains with a reduced state selection built from the formal derivatives of
/// `model`: exactly when [`reduce_or_retain`] reduces, which is when the manifold
/// is nonempty, the formal dimension is below the retained state count, and some
/// manifold constraint is a redundant loop closure. `model` is the system
/// `prepared` was prepared from.
pub(crate) fn executes_reduced_selection(
    model: &dae::Dae,
    prepared: &PreparedDae<'_>,
) -> Result<bool, StructuralError> {
    Ok(prepared.manifold_requires_reduction()
        && formal_below_retained_dimension(model, prepared)?.is_some())
}

/// The formal derivatives of `model` when `prepared` retains a constrained
/// state manifold and the formal dimension is below the retained state count,
/// the precondition for a reduced selection; `None` retains `prepared`.
fn formal_below_retained_dimension<'model>(
    model: &'model dae::Dae,
    prepared: &PreparedDae<'_>,
) -> Result<Option<FormalDerivativeSystem<'model>>, StructuralError> {
    if prepared.inspect(|system| system.manifold.is_empty()) {
        return Ok(None);
    }
    let formal = construct_formal_derivatives(model)?;
    let dimension = formal.inspect(|formal| formal.formal_dimension());
    let retained = prepared.as_dae().inspect(|view| {
        view.variables()
            .filter(|(_, v)| v.role() == dae::VariableRole::State)
            .map(|(_, v)| v.scalar_count())
            .sum::<usize>()
    });
    Ok((dimension < retained).then_some(formal))
}

/// Attempt the formal-derivative reduction for a system the ordinary reducer
/// left structurally singular. Returns `None` when the formal path does not
/// apply or its selection fails, so the caller reports the reducer's original
/// singularity unchanged. Once a candidate is prepared, a failure of the
/// transforms applied to it is its own error, never the reducer's singularity.
fn recover_singular_via_formal(
    model: &dae::Dae,
    overrides: &HashMap<String, f64>,
) -> Result<Option<PreparedSelection<'static>>, StructuralError> {
    let Ok(formal) = construct_formal_derivatives(model) else {
        return Ok(None);
    };
    let mut alternate_selections = AlternateSelections::default();
    let Ok(candidate) = formal.construct_state_candidate_with_charts(|formal| {
        let (selection, alternates) = select(formal, overrides)?;
        alternate_selections = alternates;
        Ok(selection)
    }) else {
        return Ok(None);
    };
    let Ok(alternates) = prepare_alternate_charts(&formal, &alternate_selections) else {
        return Ok(None);
    };
    let Ok(candidate) = candidate.into_prepared() else {
        return Ok(None);
    };
    let (primary, formal_aliases) = quotient_formal_candidate(candidate)?;
    Ok(Some(PreparedSelection {
        primary,
        alternates,
        exchanges: alternate_selections.exchanges,
        formal_aliases,
    }))
}

/// Inline the after-index-reduction calls of a prepared candidate, report its
/// formal-scope alias classes, and apply the formal quotient.
fn quotient_formal_candidate(
    candidate: PreparedDae<'_>,
) -> Result<(PreparedDae<'_>, AliasQuotientReport), StructuralError> {
    let candidate = inline_formal_calls(candidate)?;
    let report = formal_alias_quotient_report(&candidate);
    Ok((quotient_formal_aliases(candidate)?, report))
}

impl<'source> PreparedSelection<'source> {
    /// The retained (or reducer-accepted) basis with no alternate charts. Every
    /// model that keeps its source basis or reduces without a folding
    /// first-integral group carries no alternates.
    fn retained(primary: PreparedDae<'source>) -> Self {
        Self {
            primary,
            alternates: Vec::new(),
            exchanges: Vec::new(),
            formal_aliases: AliasQuotientReport::default(),
        }
    }
}

/// Prepare one transformed DAE per alternate reduced chart by re-running the
/// ordinary coordinate transformation with the chart's Independent set as the
/// chosen states. Each alternate is a distinct, fully checked candidate; it
/// carries no reduced chart set of its own, so no re-enumeration recurses.
///
/// The selections name their coordinates by source variable ordinal, formal
/// derivative order, and tensor scalar, which the finalized transformed root
/// resolves back to branded coordinates. This reuses the same construction the
/// primary basis went through, so an alternate plan is as trustworthy as the
/// primary. A first-integral mirror that fails is the model's failure; an
/// exchange that fails is withheld (`None`) and its chart is dropped when the
/// plans are attached.
fn prepare_alternate_charts(
    formal: &FormalDerivativeSystem<'_>,
    alternates: &AlternateSelections,
) -> Result<Vec<Option<PreparedDae<'static>>>, StructuralError> {
    let exchange = !alternates.exchanges.is_empty();
    alternates
        .selections
        .iter()
        .map(
            |selection| match prepare_alternate_chart(formal, selection) {
                Ok(prepared) => Ok(Some(prepared)),
                Err(_) if exchange => Ok(None),
                Err(error) => Err(error),
            },
        )
        .collect()
}

/// Prepare one alternate chart's transformed DAE from its lifetime-free
/// Independent set.
fn prepare_alternate_chart(
    formal: &FormalDerivativeSystem<'_>,
    selection: &[(u32, usize, u32)],
) -> Result<PreparedDae<'static>, StructuralError> {
    formal
        .construct_state_candidate(|view| resolve_alternate_coordinates(view, selection))?
        .into_prepared()
        .and_then(inline_formal_calls)
        .and_then(quotient_formal_aliases)
}

/// Rebind an alternate chart's lifetime-free Independent set onto branded formal
/// coordinates of the inspected view.
fn resolve_alternate_coordinates<'source, 'formal>(
    view: FormalDerivativeView<'_, 'source, 'formal>,
    selection: &[(u32, usize, u32)],
) -> Result<Vec<FormalStateCoordinate<'formal>>, StructuralError> {
    selection
        .iter()
        .map(|&(source, order, scalar)| {
            let variable = view
                .source
                .variable_id(source as usize)
                .ok_or_else(|| failure("alternate chart coordinate has no source owner"))?;
            view.state_coordinate(variable, order, scalar)
        })
        .collect()
}

/// The primary reduced selection plus the alternate Independent sets.
type SelectionWithAlternates<'formal> = (StateSelection<'formal>, AlternateSelections);

/// Refuse a `StateSelect.always` request for more independent coordinates than
/// the differential dimension provides.
fn check_forced_state_count(
    formal: FormalDerivativeView<'_, '_, '_>,
) -> Result<(), StructuralError> {
    let required = formal
        .source
        .variables()
        .filter(|(_, variable)| requests_forced_state(*variable))
        .map(|(_, variable)| variable.scalar_count())
        .sum::<usize>();
    if required > formal.formal_dimension() {
        return Err(failure(format!(
            "StateSelect.always requires {required} independent coordinates, but the differential dimension is {}",
            formal.formal_dimension()
        )));
    }
    Ok(())
}

fn select<'formal>(
    formal: FormalDerivativeView<'_, '_, 'formal>,
    overrides: &HashMap<String, f64>,
) -> Result<SelectionWithAlternates<'formal>, StructuralError> {
    check_forced_state_count(formal)?;
    let programs = lower_state_selection_stages(formal).map_err(failure)?;
    let mut point = TrialPoint::new(formal, overrides)?;
    point.seed_definitions(&programs)?;
    let mut result = Vec::new();
    // Every selected independent coordinate in `result`, named by source ordinal,
    // formal order, and scalar. An alternate chart is this list with the folding
    // group's integrated scalar remapped, so it must be recorded as the primary
    // basis is built.
    let mut primary_selection: Vec<(u32, usize, u32)> = Vec::new();
    // The same coordinates grouped by stage, deepest first, with each level.
    let mut stage_integrated = Vec::new();
    let mut charts = Vec::new();
    let mut fold = None;
    let mut exchanges = None;
    let mut deepest_stage = true;
    for stage in programs.stages().iter().filter(|s| s.stage().level() < 0) {
        let coordinates = stage
            .stage()
            .coordinates()
            .flat_map(|coordinate| {
                let count = coordinate.value_variable().scalar_count();
                (0..count).map(move |scalar| (coordinate, scalar))
            })
            .collect::<Vec<_>>();
        let matrix = point.settle(&programs, stage, &coordinates)?;
        let choices = coordinates
            .iter()
            .map(|&(coordinate, scalar)| {
                choice(
                    formal,
                    coordinate,
                    point.has_stated_initial_value(coordinate, scalar),
                )
            })
            .collect::<Vec<_>>();
        let selected = matrix
            .independent_columns(&choices)
            .map_err(|error| match error {
                rumoca_eval_solve::dense_basis::DenseBasisError::Rank => failure(format!(
                    "stage {} has a singular dependent Jacobian under the required state selection",
                    stage.stage().level()
                )),
                error => failure(format!(
                    "stage {} basis selection failed: {error:?}",
                    stage.stage().level()
                )),
            })?;
        // The deepest stage carries the lowest-order (position-level) holonomic
        // constraint, where a chart folds. Enumerate its bounded alternate charts
        // here: the mirrors of a definitional first-integral group, or else the
        // ranked single exchanges of a reduced constraint group. Every higher
        // stage follows the chosen basis and is not re-enumerated.
        if deepest_stage {
            let (enumerated, group) = enumerate_reduced_charts(&matrix, &coordinates, &selected);
            charts = enumerated;
            if group.is_none() {
                exchanges = exchange::stage_exchanges(
                    stage,
                    &matrix,
                    &coordinates,
                    &choices,
                    &selected,
                    || formal.stage_slope_is_invariant(stage.stage()),
                );
            }
            fold = group;
            deepest_stage = false;
        }
        let mut integrated = Vec::with_capacity(selected.len());
        for index in selected {
            let (coordinate, scalar) = coordinates[index];
            result.push(formal.state_coordinate(
                coordinate.source(),
                coordinate.order(),
                scalar as u32,
            )?);
            integrated.push(exchange::StageColumn {
                coordinate: selection_coordinate(coordinate, scalar),
                forced: choices[index] == ColumnChoice::Independent,
            });
        }
        primary_selection.extend(integrated.iter().map(|column| column.coordinate));
        stage_integrated.push((stage.stage().level(), integrated));
    }
    let alternates = match (fold, exchanges) {
        (Some(group), _) => AlternateSelections {
            selections: group.alternate_selections(&primary_selection),
            exchanges: Vec::new(),
        },
        (None, Some(plan)) => plan.into_alternates(&stage_integrated, &mut charts),
        (None, None) => AlternateSelections::default(),
    };
    Ok((
        StateSelection {
            coordinates: result,
            charts,
        },
        alternates,
    ))
}

/// A stage coordinate named by source ordinal, formal order, and scalar, so it
/// survives the branded-coordinate lifetime.
fn selection_coordinate(
    coordinate: FormalStageCoordinate<'_, '_>,
    scalar: usize,
) -> (u32, usize, u32) {
    (
        coordinate.source().index(),
        coordinate.order(),
        scalar as u32,
    )
}

/// Enumerate the bounded regular reconstruction charts of one folding
/// definitional first-integral coordinate group at the trial point.
///
/// The primary selection reconstructs a single dependent coordinate from a
/// conserved holonomic constraint (a norm `g = x*x - 1`, whose gradient
/// `g_d = 2*x_i` vanishes as `x_i` passes through zero). Every source scalar of
/// the group is an admissible reconstructed coordinate, so this issues one chart
/// per column: the primary (chart index zero) plus its mirrors. A mirror is kept
/// even when its dependent Jacobian is singular at the trial point, because its
/// purpose is regularity at the configuration where the primary folds; its
/// trial-point conditioning is recorded, not used as a filter.
///
/// Only a single-constraint group with a genuine alternate (two or more columns,
/// one reconstructed coordinate) is enumerated; any other stage carries no
/// alternates.
fn enumerate_reduced_charts(
    matrix: &rumoca_eval_solve::dense_basis::DenseStageMatrix,
    coordinates: &[(FormalStageCoordinate<'_, '_>, usize)],
    selected: &[usize],
) -> (Vec<ReducedSelectionChart>, Option<ReducedFoldGroup>) {
    let columns = coordinates.len();
    let independent: std::collections::BTreeSet<usize> = selected.iter().copied().collect();
    let dependent: Vec<usize> = (0..columns).filter(|c| !independent.contains(c)).collect();
    if dependent.len() != 1 || columns < 2 {
        return (Vec::new(), None);
    }
    let coordinate = |column: usize| {
        let (stage_coordinate, scalar) = coordinates[column];
        (stage_coordinate.value().index(), scalar as u32)
    };
    // The primary reconstructed column leads, so the primary basis is chart zero.
    let primary = dependent[0];
    let order = std::iter::once(primary).chain((0..columns).filter(|&c| c != primary));
    let charts = order
        .clone()
        .map(|reconstructed| {
            let (trial_rcond, trial_singular_threshold) = matrix
                .dependent_conditioning(&[reconstructed])
                .map_or((0.0, 0.0), |c| (c.rcond, c.singular_threshold));
            ReducedSelectionChart {
                dependent: vec![coordinate(reconstructed)],
                independent: (0..columns)
                    .filter(|&c| c != reconstructed)
                    .map(coordinate)
                    .collect(),
                trial_rcond,
                trial_singular_threshold,
            }
        })
        .collect();
    // The alternate plans are re-lowered only when the folding group is a single
    // source tensor (a norm-constrained coordinate whose scalars alias one
    // another across every differentiation order). A mixed-source deepest stage
    // keeps its partition-only charts unchanged but issues no re-lowered plan.
    let group = single_source(coordinates).map(|source| ReducedFoldGroup {
        source,
        primary_reconstructed_scalar: coordinates[primary].1 as u32,
        alternate_reconstructed_scalars: order
            .skip(1)
            .map(|reconstructed| coordinates[reconstructed].1 as u32)
            .collect(),
    });
    (charts, group)
}

/// The common source variable ordinal of a stage's coordinates, or `None` when
/// they span more than one source.
fn single_source(coordinates: &[(FormalStageCoordinate<'_, '_>, usize)]) -> Option<u32> {
    let mut sources = coordinates
        .iter()
        .map(|(coordinate, _)| coordinate.source().index());
    let first = sources.next()?;
    sources.all(|source| source == first).then_some(first)
}

/// A folding definitional first-integral group at the deepest selection stage:
/// one source tensor whose scalars are aliased by a conserved holonomic
/// constraint across every differentiation order. Its alternate charts are the
/// primary Independent set with the integrated scalar of this source remapped.
struct ReducedFoldGroup {
    /// Source variable ordinal of the folding tensor.
    source: u32,
    /// Scalar the primary basis reconstructs (its Dependent coordinate).
    primary_reconstructed_scalar: u32,
    /// Scalar each alternate chart reconstructs instead, in chart order.
    alternate_reconstructed_scalars: Vec<u32>,
}

impl ReducedFoldGroup {
    /// The full Independent set of each alternate chart, derived from the primary
    /// selection by swapping the folding source's integration role: the scalar an
    /// alternate reconstructs leaves the integrated set and the scalar the primary
    /// reconstructed enters it. Because the conserved constraint aliases the same
    /// scalar indices at every order, the swap applies to every selected
    /// coordinate of this source across all stages.
    fn alternate_selections(
        &self,
        primary_selection: &[(u32, usize, u32)],
    ) -> Vec<Vec<(u32, usize, u32)>> {
        self.alternate_reconstructed_scalars
            .iter()
            .map(|&reconstructed| self.remap_selection(primary_selection, reconstructed))
            .collect()
    }

    /// The primary Independent set with this source's `reconstructed` scalar
    /// swapped out of the integrated role and the primary's reconstructed scalar
    /// swapped in.
    fn remap_selection(
        &self,
        primary_selection: &[(u32, usize, u32)],
        reconstructed: u32,
    ) -> Vec<(u32, usize, u32)> {
        primary_selection
            .iter()
            .map(|&coordinate| self.remap_coordinate(coordinate, reconstructed))
            .collect()
    }

    fn remap_coordinate(
        &self,
        (source, order, scalar): (u32, usize, u32),
        reconstructed: u32,
    ) -> (u32, usize, u32) {
        if source == self.source && scalar == reconstructed {
            (source, order, self.primary_reconstructed_scalar)
        } else {
            (source, order, scalar)
        }
    }
}

/// A `StateSelect.always` request forces an independent differential state only
/// for a genuine state variable, which owns an independent integration slot. An
/// algebraic or output coordinate carries no such slot: it is structurally
/// determined by the equation system (an alias of another state's derivative, an
/// acceleration-level derivative sensor, or a constraint/function output), so it
/// cannot be an independent state. MLS 3.7 §4.9.7.1 makes `always` a request, not
/// a guarantee: a coordinate that cannot be a state is demoted rather than failing.
fn requests_forced_state(variable: dae::VariableView<'_>) -> bool {
    variable.state_select() == StateSelect::Always
        && variable.variability() == dae::ExpressionVariability::Continuous
        && variable.role() == dae::VariableRole::State
}

/// Eligibility priority for a demoted `StateSelect.always` algebraic coordinate.
/// It is above every other group (`Prefer` peaks at 6, plus a stated-initial-
/// value bump), so the demoted request is honored as an independent state
/// whenever the stage's constraint structure admits it, and is released to a
/// dependent coordinate only when it cannot be an independent state.
const DEMOTED_ALWAYS_PRIORITY: u8 = 8;

fn choice<'source, 'formal>(
    formal: FormalDerivativeView<'_, 'source, 'formal>,
    coordinate: FormalStageCoordinate<'source, 'formal>,
    stated_initial_value: bool,
) -> ColumnChoice {
    let source = coordinate.source_variable();
    if coordinate.order() == 0 {
        match source.state_select() {
            StateSelect::Always if requests_forced_state(source) => {
                return ColumnChoice::Independent;
            }
            StateSelect::Always => {
                return ColumnChoice::Eligible(
                    DEMOTED_ALWAYS_PRIORITY + u8::from(stated_initial_value),
                );
            }
            StateSelect::Never => return ColumnChoice::Dependent,
            _ => {}
        }
    }
    if formal
        .coordinate(coordinate.source(), coordinate.order() + 1)
        .is_none()
    {
        return ColumnChoice::Dependent;
    }
    let priority = match source.state_select() {
        StateSelect::Prefer => 6,
        StateSelect::Avoid => 0,
        _ if source.role() == dae::VariableRole::State => 4,
        _ => 2,
    };
    ColumnChoice::Eligible(priority + u8::from(stated_initial_value))
}

fn failure(error: impl std::fmt::Display) -> StructuralError {
    StructuralError::UnspannedContractViolation {
        reason: format!("independent state selection: {error}"),
    }
}
