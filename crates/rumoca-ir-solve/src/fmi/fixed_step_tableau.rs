//! The arithmetic of one fixed integration step, issued once.
//!
//! Every executor of a fixed step reads the same table: the stages with their
//! evaluation times, the coefficient terms each stage state folds, the update
//! terms the endpoint folds, the value every fold starts from, the stage whose
//! derivative closes the step, and the order of the continuous extension. The
//! host plugin interprets it, and a printed program is to be emitted from it,
//! so the two agree bit for bit by construction rather than by convention.
//!
//! One fold is `x + h * (((start + w_1 * k_1) + w_2 * k_2) + ...)`: products
//! weight first, a left fold from [`FIXED_STEP_FOLD_START`] over the terms in
//! the table's order, then one multiply by `h` and one add. A zero coefficient
//! contributes no term, so a stage state folds only the slopes its row names.
//!
//! A table is built only by [`FixedStepTableau::issue`], through a checked
//! constructor that refuses mismatched row and weight counts, non-finite
//! entries, a fold that does not start from zero, and an end stage or
//! extension order the stages do not support.
//!
//! # References
//!
//! The tableaus of Euler, Heun and the classical method and their continuous
//! extensions: E. Hairer, S. P. Norsett and G. Wanner, "Solving Ordinary
//! Differential Equations I: Nonstiff Problems", 2nd rev. ed., Springer 1993,
//! sections II.1 and II.6. The partitioned Euler method: E. Hairer, C. Lubich
//! and G. Wanner, "Geometric Numerical Integration", 2nd ed., Springer 2006,
//! section VI.3.

/// The value every weighted fold starts from: `-0.0`, the one identity of
/// IEEE 754 addition (`-0.0 + x` is `x` for every `x`, signed zeros
/// included), so the first term passes through the fold unchanged.
pub const FIXED_STEP_FOLD_START: f64 = -0.0;

/// Which states the update terms advance.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FixedStepStateRoles {
    /// Every state advances by the update fold.
    Uniform,
    /// Partitioned over the kinematic states the Solve IR issues for the
    /// model. The remaining states advance by the update fold. The kinematic
    /// states then advance by `x + h * g`, one product and one add, where `g`
    /// is their derivative at the step start time and at the point whose
    /// remaining entries are the update fold's endpoint and whose kinematic
    /// entries are the step start. With an empty or an all-kinematic set the
    /// step is the update fold alone.
    Kinematic,
}

/// One term of a fold: the slope of `stage` times `weight`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FixedStepTerm {
    stage: usize,
    weight: f64,
}

impl FixedStepTerm {
    #[must_use]
    pub const fn stage(&self) -> usize {
        self.stage
    }

    #[must_use]
    pub const fn weight(&self) -> f64 {
        self.weight
    }
}

/// One stage: where it is evaluated and the terms its state folds.
#[derive(Clone, Debug, PartialEq)]
struct FixedStepStage {
    abscissa: f64,
    terms: Vec<FixedStepTerm>,
}

/// The arithmetic of one fixed step of one method.
#[derive(Clone, Debug, PartialEq)]
pub struct FixedStepTableau {
    roles: FixedStepStateRoles,
    stages: Vec<FixedStepStage>,
    update: Vec<FixedStepTerm>,
    fold_start: f64,
    end_stage: Option<usize>,
    extension_order: u32,
}

/// Why no table was issued or a table was refused.
#[derive(Clone, Copy, Debug, PartialEq, thiserror::Error)]
pub enum FixedStepTableauRefusal {
    #[error("no fixed-step method has order {0}")]
    Order(u32),
    #[error("the kinematic state roles require a one-stage method, not order {0}")]
    Roles(u32),
    #[error("a fixed-step method needs at least one stage")]
    NoStage,
    #[error("{abscissae} abscissae for {stages} stages")]
    AbscissaCount { stages: usize, abscissae: usize },
    #[error("stage {stage} has {found} coefficients, not {stage}")]
    RowLength { stage: usize, found: usize },
    #[error("{weights} update weights for {stages} stages")]
    WeightCount { stages: usize, weights: usize },
    #[error("the first stage is evaluated at abscissa {0}, not at the step start")]
    FirstAbscissa(f64),
    #[error("non-finite {entry} {value}")]
    NonFinite { entry: &'static str, value: f64 },
    #[error("the fold starts from {0}, not from zero")]
    FoldStart(f64),
    #[error("end stage {0} is not a stage evaluated at the step end")]
    EndStage(usize),
    #[error("continuous extension order {0} is not that of the stages' extension")]
    ExtensionOrder(u32),
}

/// The coefficients of one method in Butcher form: row `i` of `rows` holds
/// the coefficients of the stages before stage `i`, and `weights` one
/// coefficient per stage.
#[derive(Clone, Copy, Debug)]
struct Butcher {
    abscissae: &'static [f64],
    rows: &'static [&'static [f64]],
    weights: &'static [f64],
    end_stage: Option<usize>,
    extension_order: u32,
}

const EULER: Butcher = Butcher {
    abscissae: &[0.0],
    rows: &[&[]],
    weights: &[1.0],
    end_stage: None,
    extension_order: 1,
};

const HEUN: Butcher = Butcher {
    abscissae: &[0.0, 1.0],
    rows: &[&[], &[1.0]],
    weights: &[0.5, 0.5],
    end_stage: Some(1),
    extension_order: 2,
};

const CLASSICAL: Butcher = Butcher {
    abscissae: &[0.0, 0.5, 0.5, 1.0],
    rows: &[&[], &[0.5], &[0.0, 0.5], &[0.0, 0.0, 1.0]],
    weights: &[1.0 / 6.0, 1.0 / 3.0, 1.0 / 3.0, 1.0 / 6.0],
    end_stage: Some(3),
    extension_order: 3,
};

impl FixedStepTableau {
    /// The table of the method of `order` (1 explicit Euler, 2 Heun, 4 the
    /// classical method) under `roles`; the kinematic roles admit order 1
    /// only, the partitioned Euler method.
    pub fn issue(order: u32, roles: FixedStepStateRoles) -> Result<Self, FixedStepTableauRefusal> {
        let butcher = match order {
            1 => EULER,
            2 => HEUN,
            4 => CLASSICAL,
            _ => return Err(FixedStepTableauRefusal::Order(order)),
        };
        if roles == FixedStepStateRoles::Kinematic && butcher.rows.len() != 1 {
            return Err(FixedStepTableauRefusal::Roles(order));
        }
        Self::checked(roles, butcher, FIXED_STEP_FOLD_START)
    }

    /// The table of `butcher` folding from `fold_start`, or why it is
    /// inconsistent.
    fn checked(
        roles: FixedStepStateRoles,
        butcher: Butcher,
        fold_start: f64,
    ) -> Result<Self, FixedStepTableauRefusal> {
        type Refusal = FixedStepTableauRefusal;
        let Butcher {
            abscissae,
            rows,
            weights,
            end_stage,
            extension_order,
        } = butcher;
        let stages = rows.len();
        if stages == 0 {
            return Err(Refusal::NoStage);
        }
        if abscissae.len() != stages {
            return Err(Refusal::AbscissaCount {
                stages,
                abscissae: abscissae.len(),
            });
        }
        if let Some((stage, row)) = rows.iter().enumerate().find(|(i, row)| row.len() != *i) {
            return Err(Refusal::RowLength {
                stage,
                found: row.len(),
            });
        }
        if weights.len() != stages {
            return Err(Refusal::WeightCount {
                stages,
                weights: weights.len(),
            });
        }
        let entries = abscissae
            .iter()
            .map(|value| ("abscissa", *value))
            .chain(
                rows.iter()
                    .flat_map(|row| row.iter())
                    .map(|value| ("coefficient", *value)),
            )
            .chain(weights.iter().map(|value| ("weight", *value)))
            .chain(std::iter::once(("fold start", fold_start)));
        for (entry, value) in entries {
            if !value.is_finite() {
                return Err(Refusal::NonFinite { entry, value });
            }
        }
        if fold_start != 0.0 {
            return Err(Refusal::FoldStart(fold_start));
        }
        if abscissae[0] != 0.0 {
            return Err(Refusal::FirstAbscissa(abscissae[0]));
        }
        if let Some(stage) = end_stage
            && abscissae.get(stage) != Some(&1.0)
        {
            return Err(Refusal::EndStage(stage));
        }
        // The chord is the extension of a step without an end slope.
        if extension_order == 0 || (end_stage.is_none() && extension_order != 1) {
            return Err(Refusal::ExtensionOrder(extension_order));
        }
        Ok(Self {
            roles,
            stages: rows
                .iter()
                .zip(abscissae)
                .map(|(row, &abscissa)| FixedStepStage {
                    abscissa,
                    terms: terms(row),
                })
                .collect(),
            update: terms(weights),
            fold_start,
            end_stage,
            extension_order,
        })
    }

    #[must_use]
    pub const fn roles(&self) -> FixedStepStateRoles {
        self.roles
    }

    /// Number of stages, the full derivative evaluations of a uniform step.
    #[must_use]
    pub fn stage_count(&self) -> usize {
        self.stages.len()
    }

    /// The terms the state of `stage` folds, in fold order. Empty for the
    /// first stage, which is evaluated at the step start state itself.
    #[must_use]
    pub fn stage_terms(&self, stage: usize) -> &[FixedStepTerm] {
        &self.stages[stage].terms
    }

    /// The time `stage` is evaluated at in the step `[start, end]` of length
    /// `step`: the start at abscissa 0, `end` at abscissa 1 (exactly, also
    /// when a clipped step's `start + step` rounds elsewhere), and
    /// `start + c * step` otherwise.
    #[must_use]
    pub fn stage_time(&self, stage: usize, start: f64, step: f64, end: f64) -> f64 {
        let abscissa = self.stages[stage].abscissa;
        if abscissa == 0.0 {
            start
        } else if abscissa == 1.0 {
            end
        } else {
            start + abscissa * step
        }
    }

    /// The terms the endpoint folds, in fold order.
    #[must_use]
    pub fn update_terms(&self) -> &[FixedStepTerm] {
        &self.update
    }

    /// The value every fold starts from, [`FIXED_STEP_FOLD_START`].
    #[must_use]
    pub const fn fold_start(&self) -> f64 {
        self.fold_start
    }

    /// `start + w_1 * k_1 + ...` over `terms` in order, with `slope(j)` the
    /// slope of stage `j` at the entry being folded.
    pub fn fold(&self, terms: &[FixedStepTerm], mut slope: impl FnMut(usize) -> f64) -> f64 {
        terms.iter().fold(self.fold_start, |sum, term| {
            sum + term.weight * slope(term.stage)
        })
    }

    /// The stage whose derivative is the end slope of the continuous
    /// extension; `None` when the extension is the chord.
    #[must_use]
    pub const fn end_stage(&self) -> Option<usize> {
        self.end_stage
    }

    /// Local accuracy order of the method's native continuous extension.
    #[must_use]
    pub const fn extension_order(&self) -> u32 {
        self.extension_order
    }
}

/// The nonzero coefficients of `row` as terms, in stage order.
fn terms(row: &[f64]) -> Vec<FixedStepTerm> {
    row.iter()
        .enumerate()
        .filter(|(_, weight)| **weight != 0.0)
        .map(|(stage, &weight)| FixedStepTerm { stage, weight })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    type Refusal = FixedStepTableauRefusal;

    fn terms_of(terms: &[FixedStepTerm]) -> Vec<(usize, u64)> {
        terms
            .iter()
            .map(|term| (term.stage(), term.weight().to_bits()))
            .collect()
    }

    fn checked(butcher: Butcher) -> Result<FixedStepTableau, Refusal> {
        FixedStepTableau::checked(FixedStepStateRoles::Uniform, butcher, FIXED_STEP_FOLD_START)
    }

    #[test]
    fn every_issued_table_folds_the_nonzero_coefficients_in_stage_order() {
        let uniform = FixedStepStateRoles::Uniform;
        let euler = FixedStepTableau::issue(1, uniform).unwrap();
        assert_eq!(euler.stage_count(), 1);
        assert_eq!(terms_of(euler.update_terms()), [(0, 1.0_f64.to_bits())]);
        assert_eq!((euler.end_stage(), euler.extension_order()), (None, 1));

        let heun = FixedStepTableau::issue(2, uniform).unwrap();
        assert_eq!(terms_of(heun.stage_terms(1)), [(0, 1.0_f64.to_bits())]);
        let half = 0.5_f64.to_bits();
        assert_eq!(terms_of(heun.update_terms()), [(0, half), (1, half)]);
        assert_eq!((heun.end_stage(), heun.extension_order()), (Some(1), 2));

        let classical = FixedStepTableau::issue(4, uniform).unwrap();
        assert!(classical.stage_terms(0).is_empty());
        assert_eq!(terms_of(classical.stage_terms(1)), [(0, half)]);
        assert_eq!(terms_of(classical.stage_terms(2)), [(1, half)]);
        assert_eq!(terms_of(classical.stage_terms(3)), [(2, 1.0_f64.to_bits())]);
        let (sixth, third) = ((1.0_f64 / 6.0).to_bits(), (1.0_f64 / 3.0).to_bits());
        assert_eq!(
            terms_of(classical.update_terms()),
            [(0, sixth), (1, third), (2, third), (3, sixth)]
        );
        assert_eq!(
            (classical.end_stage(), classical.extension_order()),
            (Some(3), 3)
        );

        let partitioned = FixedStepTableau::issue(1, FixedStepStateRoles::Kinematic).unwrap();
        assert_eq!(partitioned.roles(), FixedStepStateRoles::Kinematic);
        assert_eq!(
            terms_of(partitioned.update_terms()),
            terms_of(euler.update_terms())
        );
        for table in [&euler, &heun, &classical, &partitioned] {
            assert_eq!(table.fold_start().to_bits(), (-0.0_f64).to_bits());
        }
    }

    #[test]
    fn only_implemented_orders_and_one_stage_partitions_are_issued() {
        for order in [0, 3, 5, u32::MAX] {
            for roles in [FixedStepStateRoles::Uniform, FixedStepStateRoles::Kinematic] {
                assert_eq!(
                    FixedStepTableau::issue(order, roles),
                    Err(Refusal::Order(order))
                );
            }
        }
        for order in [2, 4] {
            assert_eq!(
                FixedStepTableau::issue(order, FixedStepStateRoles::Kinematic),
                Err(Refusal::Roles(order))
            );
        }
    }

    /// `-0.0` is the start that leaves every first term as it is, signed zeros
    /// included; `+0.0` would turn a `-0.0` term into `+0.0`.
    #[test]
    fn the_fold_starts_from_negative_zero_and_passes_the_first_term_through() {
        let table = FixedStepTableau::issue(1, FixedStepStateRoles::Uniform).unwrap();
        assert_eq!(FIXED_STEP_FOLD_START.to_bits(), (-0.0_f64).to_bits());
        assert_eq!(
            table.fold(&[], |_| unreachable!()).to_bits(),
            (-0.0_f64).to_bits()
        );
        for slope in [-0.0, 0.0, 1.0, -f64::MIN_POSITIVE, f64::MAX] {
            let folded = table.fold(table.update_terms(), |_| slope);
            assert_eq!(folded.to_bits(), slope.to_bits(), "{slope}");
        }
        assert_eq!((0.0_f64 + -0.0).to_bits(), 0.0_f64.to_bits());
    }

    /// Abscissa 1 is the step's end coordinate itself, which a clipped step's
    /// `start + step` need not round to.
    #[test]
    fn stage_times_are_the_start_the_end_or_the_offset() {
        let table = FixedStepTableau::issue(4, FixedStepStateRoles::Uniform).unwrap();
        let (start, end) = (0.3_f64, 0.9_f64);
        let step = end - start;
        assert_ne!(start + step, end);
        assert_eq!(table.stage_time(0, start, step, end), start);
        assert_eq!(table.stage_time(1, start, step, end), start + 0.5 * step);
        assert_eq!(table.stage_time(2, start, step, end), start + 0.5 * step);
        assert_eq!(table.stage_time(3, start, step, end), end);
    }

    #[test]
    fn the_checked_constructor_refuses_mismatched_counts() {
        assert_eq!(
            checked(Butcher {
                abscissae: &[],
                rows: &[],
                weights: &[],
                ..EULER
            }),
            Err(Refusal::NoStage)
        );
        assert_eq!(
            checked(Butcher {
                abscissae: &[0.0],
                ..HEUN
            }),
            Err(Refusal::AbscissaCount {
                stages: 2,
                abscissae: 1
            })
        );
        assert_eq!(
            checked(Butcher {
                rows: &[&[], &[0.5], &[0.5], &[0.0, 0.0, 1.0]],
                ..CLASSICAL
            }),
            Err(Refusal::RowLength { stage: 2, found: 1 })
        );
        assert_eq!(
            checked(Butcher {
                rows: &[&[0.0], &[1.0]],
                ..HEUN
            }),
            Err(Refusal::RowLength { stage: 0, found: 1 })
        );
        for weights in [&[0.5][..], &[0.5, 0.25, 0.25]] {
            assert_eq!(
                checked(Butcher { weights, ..HEUN }),
                Err(Refusal::WeightCount {
                    stages: 2,
                    weights: weights.len()
                })
            );
        }
    }

    #[test]
    fn the_checked_constructor_refuses_non_finite_entries_and_a_nonzero_start() {
        let non_finite = |refusal: Result<FixedStepTableau, Refusal>| match refusal {
            Err(Refusal::NonFinite { entry, value }) => {
                assert!(!value.is_finite());
                entry
            }
            other => panic!("expected a non-finite refusal, got {other:?}"),
        };
        let abscissa = checked(Butcher {
            abscissae: &[0.0, f64::NAN],
            ..HEUN
        });
        assert_eq!(non_finite(abscissa), "abscissa");
        let coefficient = checked(Butcher {
            rows: &[&[], &[f64::INFINITY]],
            ..HEUN
        });
        assert_eq!(non_finite(coefficient), "coefficient");
        let weight = checked(Butcher {
            weights: &[0.5, f64::NEG_INFINITY],
            ..HEUN
        });
        assert_eq!(non_finite(weight), "weight");
        let uniform = FixedStepStateRoles::Uniform;
        let start = FixedStepTableau::checked(uniform, EULER, f64::NAN);
        assert_eq!(non_finite(start), "fold start");
        assert_eq!(
            FixedStepTableau::checked(uniform, EULER, 1.0),
            Err(Refusal::FoldStart(1.0))
        );
    }

    #[test]
    fn the_checked_constructor_refuses_an_unsupported_end_stage_or_extension() {
        assert_eq!(
            checked(Butcher {
                abscissae: &[0.5, 1.0],
                ..HEUN
            }),
            Err(Refusal::FirstAbscissa(0.5))
        );
        for end_stage in [0, 2] {
            assert_eq!(
                checked(Butcher {
                    end_stage: Some(end_stage),
                    ..HEUN
                }),
                Err(Refusal::EndStage(end_stage))
            );
        }
        assert_eq!(
            checked(Butcher {
                end_stage: Some(1),
                ..CLASSICAL
            }),
            Err(Refusal::EndStage(1))
        );
        assert_eq!(
            checked(Butcher {
                extension_order: 0,
                ..HEUN
            }),
            Err(Refusal::ExtensionOrder(0))
        );
        assert_eq!(
            checked(Butcher {
                extension_order: 2,
                ..EULER
            }),
            Err(Refusal::ExtensionOrder(2))
        );
    }
}
