//! Which model parameters a derivative may be taken with respect to.
//!
//! The classification is computed once, where the DAE and its Solve lowering
//! are both visible (`rumoca-phase-solve`), and carried into
//! [`crate::SensitivityProblem::construct`], which refuses a request against it
//! (SOLVE-C74). No other site re-derives which parameters the Solve programs
//! read, which the initialization defines, or which state starts a parameter
//! fixed at lowering.

use std::collections::BTreeMap;

/// Why a parameter is not a differentiation variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExclusionReason {
    /// An `Integer`, `Boolean`, `String`, or enumeration parameter: discrete
    /// valued (MLS 3.8.3), so a derivative is not defined and its value may
    /// fix array dimensions.
    NotReal,
    /// Final, constant, evaluated, or structural: fixed by recompiling.
    NotTunable,
    /// Not declared in the source model.
    Generated,
    /// Its own binding reads other parameters, so it is evaluated once at
    /// lowering and would not follow a changed slot.
    DependsOnParameters,
    /// Another parameter's binding reads it.
    ReadByParameters,
    /// No Solve program reads it: translation folded it into the rows.
    FoldedAtTranslation,
    /// The initialization system defines it (`fixed = false`).
    InitializationDefined,
}

impl ExclusionReason {
    /// A phrase for a diagnostic.
    #[must_use]
    pub const fn describe(self) -> &'static str {
        match self {
            Self::NotReal => "not a Real parameter",
            Self::NotTunable => "final, constant, evaluated, or structural",
            Self::Generated => "not declared in the source model",
            Self::DependsOnParameters => "its binding reads other parameters",
            Self::ReadByParameters => "another parameter's binding reads it",
            Self::FoldedAtTranslation => "folded into the rows at translation",
            Self::InitializationDefined => "defined by the initialization (fixed = false)",
        }
    }
}

/// One excluded parameter and the reason.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExcludedParameter {
    pub name: String,
    pub reason: ExclusionReason,
}

/// The parameters of a model split into differentiation variables and the
/// excluded, each with its reason, plus the lowering-time initial states a
/// differentiation variable would silently leave at a zero sensitivity.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ParameterClassification {
    selected: Vec<String>,
    excluded: Vec<ExcludedParameter>,
    /// Selected parameter to the states whose `start` expression reads it and
    /// that the initialization neither solves nor updates: the lowered model
    /// fixed those starts as constants.
    baked_starts: BTreeMap<String, Vec<String>>,
    verdict: BTreeMap<String, Option<ExclusionReason>>,
}

impl ParameterClassification {
    /// A classification of `selected` and `excluded` parameters (declaration
    /// order) and the `baked_starts` of the selected ones.
    #[must_use]
    pub fn new(
        selected: Vec<String>,
        excluded: Vec<ExcludedParameter>,
        baked_starts: BTreeMap<String, Vec<String>>,
    ) -> Self {
        let verdict = selected
            .iter()
            .map(|name| (name.clone(), None))
            .chain(
                excluded
                    .iter()
                    .map(|entry| (entry.name.clone(), Some(entry.reason))),
            )
            .collect();
        Self {
            selected,
            excluded,
            baked_starts,
            verdict,
        }
    }

    /// The differentiation variables, in declaration order.
    #[must_use]
    pub fn selected(&self) -> &[String] {
        &self.selected
    }

    /// The excluded parameters, each with its reason.
    #[must_use]
    pub fn excluded(&self) -> &[ExcludedParameter] {
        &self.excluded
    }

    /// `None` for a name that is not a parameter; otherwise the reason it is
    /// excluded, or `Ok` for a differentiation variable.
    #[must_use]
    pub fn verdict(&self, name: &str) -> Option<Result<(), ExclusionReason>> {
        self.verdict
            .get(name)
            .map(|reason| reason.map_or(Ok(()), Err))
    }

    /// A state whose lowering-time initial value reads the selected parameter
    /// `name`, when one exists.
    #[must_use]
    pub fn baked_start(&self, name: &str) -> Option<&str> {
        self.baked_starts
            .get(name)
            .and_then(|states| states.first())
            .map(String::as_str)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn classification() -> ParameterClassification {
        ParameterClassification::new(
            vec!["a".to_string(), "b".to_string()],
            vec![ExcludedParameter {
                name: "n".to_string(),
                reason: ExclusionReason::NotReal,
            }],
            BTreeMap::from([("b".to_string(), vec!["x".to_string()])]),
        )
    }

    #[test]
    fn a_name_is_selected_excluded_with_its_reason_or_unknown() {
        let c = classification();
        assert_eq!(c.verdict("a"), Some(Ok(())));
        assert_eq!(c.verdict("n"), Some(Err(ExclusionReason::NotReal)));
        assert_eq!(c.verdict("zz"), None);
        assert_eq!(c.selected(), ["a", "b"]);
        assert_eq!(c.excluded().len(), 1);
    }

    #[test]
    fn a_baked_start_names_the_state_the_lowering_fixed() {
        let c = classification();
        assert_eq!(c.baked_start("b"), Some("x"));
        assert_eq!(c.baked_start("a"), None);
    }

    #[test]
    fn every_reason_has_a_phrase() {
        for reason in [
            ExclusionReason::NotReal,
            ExclusionReason::NotTunable,
            ExclusionReason::Generated,
            ExclusionReason::DependsOnParameters,
            ExclusionReason::ReadByParameters,
            ExclusionReason::FoldedAtTranslation,
            ExclusionReason::InitializationDefined,
        ] {
            assert!(!reason.describe().is_empty());
        }
    }
}
