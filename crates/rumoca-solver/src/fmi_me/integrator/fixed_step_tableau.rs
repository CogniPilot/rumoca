//! The arithmetic of one fixed step, as a numerical plugin reads it.
//!
//! The Solve IR issues the table (`rumoca_ir_solve::fmi::FixedStepTableau`).
//! A plugin interprets it through these contract types and so names no Solve
//! type (SPEC_0038 §Internal Solver Boundary). Each type holds the issued
//! value and forwards to it; none restates a coefficient or a rule.

use rumoca_ir_solve::fmi::{
    FixedStepStateRoles, FixedStepTableau, FixedStepTableauRefusal, FixedStepTerm,
};

/// Which states the update terms of a table advance.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum MeFixedStepRoles {
    /// Every state advances by the update fold.
    Uniform,
    /// The kinematic states the model issues advance from their derivative at
    /// the advanced values of the others; the others advance by the update
    /// fold.
    Kinematic,
}

impl MeFixedStepRoles {
    const fn issued(self) -> FixedStepStateRoles {
        match self {
            Self::Uniform => FixedStepStateRoles::Uniform,
            Self::Kinematic => FixedStepStateRoles::Kinematic,
        }
    }
}

/// Why no table is issued for an order and roles.
#[derive(Clone, Copy, Debug, PartialEq, thiserror::Error)]
#[error(transparent)]
pub struct MeFixedStepRefusal(FixedStepTableauRefusal);

impl MeFixedStepRefusal {
    /// Whether the order exists but does not admit the requested roles.
    #[must_use]
    pub const fn roles_not_admitted(&self) -> bool {
        matches!(self.0, FixedStepTableauRefusal::Roles(_))
    }
}

/// The terms of one fold of a table, in fold order.
#[derive(Clone, Copy, Debug)]
pub struct MeFixedStepTerms<'table>(&'table [FixedStepTerm]);

impl MeFixedStepTerms<'_> {
    /// Whether the fold has no term: its value is the fold's start.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

/// The issued arithmetic of one fixed step of one method.
#[derive(Clone, Debug, PartialEq)]
pub struct MeFixedStepTableau(FixedStepTableau);

impl MeFixedStepTableau {
    /// The table of the method of `order` with `roles`.
    pub fn issue(order: u32, roles: MeFixedStepRoles) -> Result<Self, MeFixedStepRefusal> {
        FixedStepTableau::issue(order, roles.issued())
            .map(Self)
            .map_err(MeFixedStepRefusal)
    }

    #[must_use]
    pub const fn roles(&self) -> MeFixedStepRoles {
        match self.0.roles() {
            FixedStepStateRoles::Uniform => MeFixedStepRoles::Uniform,
            FixedStepStateRoles::Kinematic => MeFixedStepRoles::Kinematic,
        }
    }

    /// Number of stages, the full derivative evaluations of a uniform step.
    #[must_use]
    pub fn stage_count(&self) -> usize {
        self.0.stage_count()
    }

    /// The terms the state of `stage` folds. Empty for the first stage.
    #[must_use]
    pub fn stage_terms(&self, stage: usize) -> MeFixedStepTerms<'_> {
        MeFixedStepTerms(self.0.stage_terms(stage))
    }

    /// The time `stage` is evaluated at in the step `[start, end]` of length
    /// `step`.
    #[must_use]
    pub fn stage_time(&self, stage: usize, start: f64, step: f64, end: f64) -> f64 {
        self.0.stage_time(stage, start, step, end)
    }

    /// The terms the endpoint folds.
    #[must_use]
    pub fn update_terms(&self) -> MeFixedStepTerms<'_> {
        MeFixedStepTerms(self.0.update_terms())
    }

    /// The value every fold starts from.
    #[must_use]
    pub const fn fold_start(&self) -> f64 {
        self.0.fold_start()
    }

    /// The issued fold over `terms`, with `slope(j)` the slope of stage `j`
    /// at the entry being folded.
    pub fn fold(&self, terms: MeFixedStepTerms<'_>, slope: impl FnMut(usize) -> f64) -> f64 {
        self.0.fold(terms.0, slope)
    }

    /// The stage whose derivative is the end slope of the continuous
    /// extension; `None` when the extension is the chord.
    #[must_use]
    pub const fn end_stage(&self) -> Option<usize> {
        self.0.end_stage()
    }

    /// Local accuracy order of the method's native continuous extension.
    #[must_use]
    pub const fn extension_order(&self) -> u32 {
        self.0.extension_order()
    }
}
