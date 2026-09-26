//! Single-exchange alternate charts of a reduced constraint group (SPEC_0040
//! STRUCT-T07 constraint-fold chart rows).
//!
//! The primary reduced basis splits the deepest (position-level) stage into
//! integrated and reconstructed columns. A reconstructed state-class column
//! whose constraint slope vanishes on the manifold folds the primary chart; an
//! exchange that integrates it and reconstructs one integrated column instead is
//! regular where the primary folds whenever the whole group keeps full row rank.

use rumoca_eval_solve::dense_basis::{ColumnChoice, DenseStageMatrix};
use rumoca_eval_solve::projection_policy::MAX_ALTERNATE_CHARTS_PER_GROUP;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_phase_structural::{FormalStageCoordinate, ReducedSelectionChart};

use super::{AlternateSelections, selection_coordinate};
use crate::lower::typed_functions::formal_stages::FormalStageProgram;

/// Lowest eligible priority of a state-class column: a source state, a
/// `prefer` coordinate, or a demoted `always` coordinate (see `choice`).
const STATE_CLASS_PRIORITY: u8 = 4;

/// One single exchange over stage column indices: the primary reconstructs
/// `dependent` and integrates `incoming`; the alternate reverses both roles.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct Exchange {
    pub(super) dependent: usize,
    pub(super) incoming: usize,
    /// Stage equation owners reading both columns.
    pub(super) shared: usize,
}

/// Every admissible exchange of one group, in rank order, split at the cap.
#[derive(Debug, Default, PartialEq, Eq)]
pub(super) struct RankedExchanges {
    pub(super) issued: Vec<Exchange>,
    pub(super) withheld: Vec<Exchange>,
}

/// Rank the admissible single exchanges of a stage whose primary integrates
/// `independent`.
///
/// A fold-capable dependent is a reconstructed column of an eligible state
/// class; an incoming column is an integrated column that is not a forced
/// `StateSelect.always` state. `shared(dependent, incoming)` counts the stage
/// equation owners reading both; a pair no equation couples is not an
/// exchange. Rank by that count, descending, then by column ordinal, and issue
/// at most `MAX_ALTERNATE_CHARTS_PER_GROUP`.
pub(super) fn rank_exchanges(
    choices: &[ColumnChoice],
    independent: &[usize],
    shared: impl Fn(usize, usize) -> usize,
) -> RankedExchanges {
    let integrated = |column: usize| independent.contains(&column);
    let fold_capable = |column: usize| matches!(choices[column], ColumnChoice::Eligible(priority) if priority >= STATE_CLASS_PRIORITY);
    let incoming = independent
        .iter()
        .copied()
        .filter(|&column| matches!(choices[column], ColumnChoice::Eligible(_)))
        .collect::<Vec<_>>();
    let (shared, incoming) = (&shared, &incoming);
    let mut exchanges = (0..choices.len())
        .filter(|&column| !integrated(column) && fold_capable(column))
        .flat_map(|dependent| {
            incoming.iter().filter_map(move |&incoming| {
                let shared = shared(dependent, incoming);
                (shared > 0).then_some(Exchange {
                    dependent,
                    incoming,
                    shared,
                })
            })
        })
        .collect::<Vec<_>>();
    exchanges.sort_by(|a, b| {
        b.shared
            .cmp(&a.shared)
            .then(a.dependent.cmp(&b.dependent))
            .then(a.incoming.cmp(&b.incoming))
    });
    let withheld = exchanges.split_off(exchanges.len().min(MAX_ALTERNATE_CHARTS_PER_GROUP));
    RankedExchanges {
        issued: exchanges,
        withheld,
    }
}

/// The integrated set of every selection stage for one exchange: the primary
/// deepest-stage set with `incoming` replaced by `dependent`, and at every
/// higher stage the integrated coordinate carrying `incoming`'s rate replaced
/// by the formal successor of `dependent`.
///
/// Coordinates are `(source ordinal, formal order, scalar)`. `stages` lists the
/// primary's integrated coordinates per stage, deepest first, with each stage's
/// level and whether each coordinate is a forced `StateSelect.always` state.
/// The constraint rows of a higher stage are derivatives of the deepest ones,
/// so the exchanged rate is `dependent`'s successor. At a higher stage the
/// primary integrates `incoming`'s rate either as its formal successor or
/// through an equal coordinate (a velocity `vy` with `der(y) = vy`). The first
/// is replaced directly; otherwise the one integrated coordinate that is
/// neither forced nor the successor of a deepest-stage coordinate is taken to
/// be it. When no such single coordinate exists the exchange cannot be
/// constructed and this yields `None`.
pub(super) fn exchanged_selection(
    stages: &[(i64, Vec<StageColumn>)],
    dependent: (u32, usize, u32),
    incoming: (u32, usize, u32),
) -> Option<Vec<(u32, usize, u32)>> {
    let ((deepest_level, deepest), higher) = stages.split_first()?;
    if !deepest.iter().any(|column| column.coordinate == incoming) {
        return None;
    }
    let mut selection = deepest
        .iter()
        .map(|column| {
            if column.coordinate == incoming {
                dependent
            } else {
                column.coordinate
            }
        })
        .collect::<Vec<_>>();
    for (level, integrated) in higher {
        let offset = usize::try_from(level - deepest_level).ok()?;
        let successor =
            |(source, order, scalar): (u32, usize, u32)| (source, order + offset, scalar);
        let incoming_rate = integrated
            .iter()
            .position(|column| column.coordinate == successor(incoming))
            .or_else(|| {
                let mut unaccounted = integrated.iter().enumerate().filter(|(_, column)| {
                    !column.forced
                        && !deepest
                            .iter()
                            .any(|lower| successor(lower.coordinate) == column.coordinate)
                });
                match (unaccounted.next(), unaccounted.next()) {
                    (Some((index, _)), None) => Some(index),
                    _ => None,
                }
            })?;
        selection.extend(integrated.iter().enumerate().map(|(index, column)| {
            if index == incoming_rate {
                successor(dependent)
            } else {
                column.coordinate
            }
        }));
    }
    Some(selection)
}

/// One integrated coordinate of a primary selection stage.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct StageColumn {
    pub(super) coordinate: (u32, usize, u32),
    /// A forced `StateSelect.always` state, which every chart integrates.
    pub(super) forced: bool,
}

/// A stage coordinate named by its source declaration and scalar ordinal.
fn chart_coordinate(
    coordinate: FormalStageCoordinate<'_, '_>,
    scalar: usize,
) -> solve::ChartCoordinate {
    solve::ChartCoordinate {
        variable: coordinate.source_variable().name().as_str().to_string(),
        scalar: scalar as u32,
    }
}

/// A chart coordinate in formal value-variable index space.
fn formal_chart_coordinate(coordinate: FormalStageCoordinate<'_, '_>, scalar: usize) -> (u32, u32) {
    (coordinate.value().index(), scalar as u32)
}

/// One issued single exchange of the deepest stage, resolved out of the
/// branded stage: its coordinates, its coverage names, the formal chart
/// coordinates of both columns, and the trial conditioning of its full
/// exchanged dependent set.
struct StageExchange {
    dependent: (u32, usize, u32),
    incoming: (u32, usize, u32),
    names: (solve::ChartCoordinate, solve::ChartCoordinate),
    columns: ((u32, u32), (u32, u32)),
    trial_rcond: f64,
    trial_singular_threshold: f64,
}

/// The ranked single exchanges of a reduced constraint group's deepest stage.
pub(super) struct ExchangePlan {
    /// The stage's constraint slope is proven constant, so no chart folds.
    slope_invariant: bool,
    primary_trial: (f64, f64),
    issued: Vec<StageExchange>,
    withheld: Vec<(solve::ChartCoordinate, solve::ChartCoordinate)>,
}

/// Rank the single exchanges of the deepest selection stage. Returns `None`
/// when the stage admits none, so the model keeps its existing chart set.
pub(super) fn stage_exchanges(
    stage: &FormalStageProgram<'_, '_, '_>,
    matrix: &DenseStageMatrix,
    coordinates: &[(FormalStageCoordinate<'_, '_>, usize)],
    choices: &[ColumnChoice],
    selected: &[usize],
    slope_is_invariant: impl FnOnce() -> bool,
) -> Option<ExchangePlan> {
    let readers = stage
        .equations()
        .iter()
        .map(|equation| equation_variables(equation.inputs()))
        .collect::<Vec<_>>();
    let variable = |column: usize| coordinates[column].0.value().index();
    let shared = |dependent: usize, incoming: usize| {
        readers
            .iter()
            .filter(|reads| {
                reads.contains(&variable(dependent)) && reads.contains(&variable(incoming))
            })
            .count()
    };
    let ranked = rank_exchanges(choices, selected, shared);
    if ranked.issued.is_empty() {
        return None;
    }
    let slope_invariant = slope_is_invariant();
    let dependent = (0..coordinates.len())
        .filter(|column| !selected.contains(column))
        .collect::<Vec<_>>();
    let conditioning = |columns: &[usize]| {
        matrix
            .dependent_conditioning(columns)
            .map_or((0.0, 0.0), |c| (c.rcond, c.singular_threshold))
    };
    let names = |e: &Exchange| {
        let (d, i) = (coordinates[e.dependent], coordinates[e.incoming]);
        (chart_coordinate(d.0, d.1), chart_coordinate(i.0, i.1))
    };
    let issued = ranked
        .issued
        .iter()
        .map(|e| {
            let (d, i) = (coordinates[e.dependent], coordinates[e.incoming]);
            let swapped = dependent
                .iter()
                .map(|&column| {
                    if column == e.dependent {
                        e.incoming
                    } else {
                        column
                    }
                })
                .collect::<Vec<_>>();
            let (trial_rcond, trial_singular_threshold) = conditioning(&swapped);
            StageExchange {
                dependent: selection_coordinate(d.0, d.1),
                incoming: selection_coordinate(i.0, i.1),
                names: names(e),
                columns: (
                    formal_chart_coordinate(d.0, d.1),
                    formal_chart_coordinate(i.0, i.1),
                ),
                trial_rcond,
                trial_singular_threshold,
            }
        })
        .collect();
    Some(ExchangePlan {
        slope_invariant,
        primary_trial: conditioning(&dependent),
        issued,
        withheld: ranked.withheld.iter().map(names).collect(),
    })
}

fn chart_exchange(
    (dependent, incoming): (solve::ChartCoordinate, solve::ChartCoordinate),
    status: solve::ChartExchangeStatus,
) -> solve::ChartExchange {
    solve::ChartExchange {
        dependent,
        incoming,
        status,
    }
}

/// A constructed exchange: its `(dependent, incoming)` formal chart
/// coordinates and the trial conditioning of its exchanged dependent set.
type ConstructedExchange = (((u32, u32), (u32, u32)), (f64, f64));

/// The chart set of a reduced constraint group: the primary, which
/// reconstructs every exchanged dependent, then one alternate per constructed
/// exchange. Every chart spans the same group columns, the union of both
/// roles of every exchange, so the runtime compares them over one group.
fn exchange_charts(
    primary_trial: (f64, f64),
    constructed: &[ConstructedExchange],
) -> Vec<ReducedSelectionChart> {
    let mut group = constructed
        .iter()
        .flat_map(|&((dependent, incoming), _)| [dependent, incoming])
        .collect::<Vec<_>>();
    group.sort_unstable();
    group.dedup();
    let mut primary = constructed
        .iter()
        .map(|&((dependent, _), _)| dependent)
        .collect::<Vec<_>>();
    primary.sort_unstable();
    primary.dedup();
    let chart = |dependent: Vec<(u32, u32)>, (trial_rcond, trial_singular_threshold)| {
        ReducedSelectionChart {
            independent: group
                .iter()
                .copied()
                .filter(|column| !dependent.contains(column))
                .collect(),
            dependent,
            trial_rcond,
            trial_singular_threshold,
        }
    };
    std::iter::once(chart(primary.clone(), primary_trial))
        .chain(constructed.iter().map(|&((out, into), trial)| {
            let dependent = primary
                .iter()
                .map(|&column| if column == out { into } else { column })
                .collect();
            chart(dependent, trial)
        }))
        .collect()
}

/// The formal variable indices a stage equation reads.
fn equation_variables(inputs: &[dae::CoordinateView<'_>]) -> Vec<u32> {
    inputs
        .iter()
        .filter_map(|&input| super::evaluation::variable(input))
        .collect()
}

impl ExchangePlan {
    /// Build the alternate selections of every issued exchange whose integrated
    /// set has a successor at every stage, replace `charts` with the group's
    /// primary and those alternates, and record the coverage of every ranked
    /// exchange. When no issued exchange constructs, `charts` is left unchanged
    /// and only the coverage is recorded.
    pub(super) fn into_alternates(
        self,
        stage_integrated: &[(i64, Vec<StageColumn>)],
        charts: &mut Vec<ReducedSelectionChart>,
    ) -> AlternateSelections {
        // A constant slope nonsingular at construction never vanishes: the group
        // cannot fold, so it issues no alternate and records why.
        if self.slope_invariant {
            let status = solve::ChartExchangeStatus::WithheldBySlopeInvariance;
            return AlternateSelections {
                selections: Vec::new(),
                exchanges: self
                    .issued
                    .into_iter()
                    .map(|exchange| exchange.names)
                    .chain(self.withheld)
                    .map(|names| chart_exchange(names, status))
                    .collect(),
            };
        }
        let mut selections = Vec::new();
        let mut constructed = Vec::new();
        let mut exchanges = Vec::new();
        for exchange in self.issued {
            let status = match exchanged_selection(
                stage_integrated,
                exchange.dependent,
                exchange.incoming,
            ) {
                Some(selection) => {
                    selections.push(selection);
                    constructed.push((
                        exchange.columns,
                        (exchange.trial_rcond, exchange.trial_singular_threshold),
                    ));
                    solve::ChartExchangeStatus::Issued {
                        chart: selections.len(),
                    }
                }
                None => solve::ChartExchangeStatus::WithheldByConstruction,
            };
            exchanges.push(chart_exchange(exchange.names, status));
        }
        exchanges.extend(
            self.withheld
                .into_iter()
                .map(|names| chart_exchange(names, solve::ChartExchangeStatus::WithheldByCap)),
        );
        if !selections.is_empty() {
            *charts = exchange_charts(self.primary_trial, &constructed);
        }
        AlternateSelections {
            selections,
            exchanges,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const STATE: ColumnChoice = ColumnChoice::Eligible(4);
    const STATED_STATE: ColumnChoice = ColumnChoice::Eligible(5);
    const ALGEBRAIC: ColumnChoice = ColumnChoice::Eligible(2);
    const AVOID: ColumnChoice = ColumnChoice::Eligible(0);

    fn all_shared(_: usize, _: usize) -> usize {
        1
    }

    #[test]
    fn a_state_class_dependent_exchanges_with_each_coupled_integrated_column() {
        let choices = [STATE, STATED_STATE, STATE];
        let ranked = rank_exchanges(&choices, &[1, 2], all_shared);
        assert_eq!(
            ranked.issued,
            vec![
                Exchange {
                    dependent: 0,
                    incoming: 1,
                    shared: 1
                },
                Exchange {
                    dependent: 0,
                    incoming: 2,
                    shared: 1
                },
            ]
        );
        assert!(ranked.withheld.is_empty());
    }

    #[test]
    fn state_select_bounds_both_roles() {
        // An algebraic, `avoid`, or `never` dependent is not fold-capable, and a
        // forced `always` state never becomes reconstructed.
        let choices = [
            ALGEBRAIC,
            AVOID,
            ColumnChoice::Dependent,
            ColumnChoice::Independent,
        ];
        let ranked = rank_exchanges(&choices, &[3], all_shared);
        assert_eq!(ranked, RankedExchanges::default());
        // A demoted `always` or `prefer` coordinate is a state-class dependent.
        let choices = [ColumnChoice::Eligible(8), ColumnChoice::Eligible(6), STATE];
        let ranked = rank_exchanges(&choices, &[2], all_shared);
        assert_eq!(
            ranked
                .issued
                .iter()
                .map(|e| e.dependent)
                .collect::<Vec<_>>(),
            vec![0, 1]
        );
    }

    #[test]
    fn a_prefer_or_demoted_always_column_may_be_exchanged_out_but_a_forced_state_may_not() {
        // MLS 3.7 §4.9.7.1 makes `prefer` and an `always` request on a
        // coordinate that is not a genuine state preferences: each may be
        // reconstructed by an alternate. A forced `always` state never is.
        let prefer = ColumnChoice::Eligible(6);
        let demoted_always = ColumnChoice::Eligible(8);
        for incoming in [prefer, demoted_always] {
            let ranked = rank_exchanges(&[STATE, incoming], &[1], all_shared);
            assert_eq!(
                ranked.issued,
                vec![Exchange {
                    dependent: 0,
                    incoming: 1,
                    shared: 1
                }],
                "{incoming:?}"
            );
        }
        let ranked = rank_exchanges(&[STATE, ColumnChoice::Independent], &[1], all_shared);
        assert_eq!(ranked, RankedExchanges::default());
    }

    #[test]
    fn an_uncoupled_pair_is_not_an_exchange() {
        let choices = [STATE, STATE, STATE];
        let ranked = rank_exchanges(&choices, &[1, 2], |_, incoming| usize::from(incoming == 2));
        assert_eq!(ranked.issued.len(), 1);
        assert_eq!(ranked.issued[0].incoming, 2);
    }

    #[test]
    fn exchanges_rank_by_shared_equations_then_ordinal_and_cap_at_the_policy_limit() {
        // Three dependents by two integrated columns: six candidates; the one
        // pair sharing two equations leads, the rest follow by ordinal, and the
        // two past the cap are withheld in rank order.
        let choices = [STATE, STATE, STATE, STATE, STATE];
        let shared =
            |dependent: usize, incoming: usize| 1 + usize::from((dependent, incoming) == (2, 4));
        let ranked = rank_exchanges(&choices, &[3, 4], shared);
        let pairs = |list: &[Exchange]| {
            list.iter()
                .map(|e| (e.dependent, e.incoming))
                .collect::<Vec<_>>()
        };
        assert_eq!(MAX_ALTERNATE_CHARTS_PER_GROUP, 4);
        assert_eq!(pairs(&ranked.issued), vec![(2, 4), (0, 3), (0, 4), (1, 3)]);
        assert_eq!(pairs(&ranked.withheld), vec![(1, 4), (2, 3)]);
    }

    fn free(coordinate: (u32, usize, u32)) -> StageColumn {
        StageColumn {
            coordinate,
            forced: false,
        }
    }

    fn forced(coordinate: (u32, usize, u32)) -> StageColumn {
        StageColumn {
            coordinate,
            forced: true,
        }
    }

    #[test]
    fn an_exchange_replaces_the_incoming_successor_at_each_higher_stage() {
        // Position stage integrates a[0] and a[2] of source 1; velocity stage
        // integrates their successors. Exchanging a[1] for a[0] integrates a[1]
        // and its successor in their place.
        let stages = vec![
            (-2, vec![free((1, 0, 0)), free((1, 0, 2))]),
            (-1, vec![free((1, 1, 0)), free((1, 1, 2))]),
        ];
        assert_eq!(
            exchanged_selection(&stages, (1, 0, 1), (1, 0, 0)),
            Some(vec![(1, 0, 1), (1, 0, 2), (1, 1, 1), (1, 1, 2)])
        );
    }

    #[test]
    fn an_exchange_replaces_the_one_unaccounted_rate_and_keeps_forced_states() {
        // Position stage integrates y (source 1) and a forced angle p (source 5);
        // velocity stage integrates vy (source 3, equal to der(y)) and a forced
        // rate w (source 6, equal to der(p)). Exchanging x (source 0) for y
        // integrates x and der(x) and keeps p and w.
        let stages = vec![
            (-2, vec![free((1, 0, 0)), forced((5, 0, 0))]),
            (-1, vec![free((3, 0, 0)), forced((6, 0, 0))]),
        ];
        assert_eq!(
            exchanged_selection(&stages, (0, 0, 0), (1, 0, 0)),
            Some(vec![(0, 0, 0), (5, 0, 0), (0, 1, 0), (6, 0, 0)])
        );
    }

    #[test]
    fn an_exchange_whose_rate_is_ambiguous_is_not_constructed() {
        let stages = vec![
            (-2, vec![free((1, 0, 0))]),
            (-1, vec![free((3, 0, 0)), free((4, 0, 0))]),
        ];
        assert_eq!(exchanged_selection(&stages, (0, 0, 0), (1, 0, 0)), None);
        // The incoming column must be integrated by the primary.
        let stages = vec![(-1, vec![free((1, 0, 0))])];
        assert_eq!(exchanged_selection(&stages, (0, 0, 0), (2, 0, 0)), None);
    }
}
