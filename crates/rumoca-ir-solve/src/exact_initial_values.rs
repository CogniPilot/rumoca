//! Stored exact source runs; scalar access never passes through Float64.

use crate::{SolveInitialValues, SolveInitialValuesError, SolveVariableValueKind};
use serde::{Deserialize, Serialize};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum SolveExactInitialRun<T> {
    Repeat {
        start: usize,
        value: T,
        count: usize,
    },
    Literal {
        start: usize,
        values: Vec<T>,
    },
}

impl<T> SolveExactInitialRun<T> {
    pub fn start(&self) -> usize {
        match self {
            Self::Repeat { start, .. } | Self::Literal { start, .. } => *start,
        }
    }
    pub fn count(&self) -> usize {
        match self {
            Self::Repeat { count, .. } => *count,
            Self::Literal { values, .. } => values.len(),
        }
    }
    pub fn value(&self, ordinal: usize) -> Option<&T> {
        if ordinal >= self.count() {
            return None;
        }
        match self {
            Self::Repeat { value, .. } => Some(value),
            Self::Literal { values, .. } => values.get(ordinal),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct ExactPlan<T> {
    count: usize,
    runs: Vec<SolveExactInitialRun<T>>,
}

#[derive(Debug, Clone)]
pub struct SolveExactInitialValues<T> {
    plan: Arc<ExactPlan<T>>,
}

impl<T> SolveExactInitialValues<T> {
    pub fn construct(
        count: usize,
        runs: Vec<SolveExactInitialRun<T>>,
    ) -> Result<Self, SolveInitialValuesError> {
        crate::initial_values::check_initial_partition(
            count,
            std::mem::size_of::<T>(),
            runs.iter().map(|run| (run.start(), run.count())),
        )?;
        Ok(Self {
            plan: Arc::new(ExactPlan { count, runs }),
        })
    }
    pub fn len(&self) -> usize {
        self.plan.count
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn runs(&self) -> impl Iterator<Item = &SolveExactInitialRun<T>> {
        self.plan.runs.iter()
    }
    pub fn value(&self, ordinal: usize) -> Option<&T> {
        let index = self
            .plan
            .runs
            .partition_point(|run| run.start() <= ordinal)
            .checked_sub(1)?;
        let run = &self.plan.runs[index];
        run.value(ordinal.checked_sub(run.start())?)
    }
    pub fn is_repeat(&self) -> bool {
        matches!(
            self.plan.runs.as_slice(),
            [SolveExactInitialRun::Repeat { .. }]
        )
    }
}

impl<T: Serialize> Serialize for SolveExactInitialValues<T> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.plan.serialize(serializer)
    }
}
impl<'de, T: Deserialize<'de>> Deserialize<'de> for SolveExactInitialValues<T> {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let plan = ExactPlan::<T>::deserialize(deserializer)?;
        Self::construct(plan.count, plan.runs).map_err(serde::de::Error::custom)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(
    tag = "kind",
    content = "values",
    rename_all = "snake_case",
    deny_unknown_fields
)]
pub enum SolveDeclarationStart {
    Real(SolveInitialValues),
    Integer(SolveExactInitialValues<i64>),
    Enumeration(SolveExactInitialValues<i64>),
    Boolean(SolveExactInitialValues<bool>),
    Missing {
        value_kind: SolveVariableValueKind,
        count: usize,
    },
}

impl SolveDeclarationStart {
    pub fn len(&self) -> usize {
        match self {
            Self::Real(values) => values.len(),
            Self::Integer(values) | Self::Enumeration(values) => values.len(),
            Self::Boolean(values) => values.len(),
            Self::Missing { count, .. } => *count,
        }
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn value_kind(&self) -> SolveVariableValueKind {
        match self {
            Self::Real(_) => SolveVariableValueKind::Real,
            Self::Integer(_) => SolveVariableValueKind::Integer,
            Self::Enumeration(_) => SolveVariableValueKind::Enumeration,
            Self::Boolean(_) => SolveVariableValueKind::Boolean,
            Self::Missing { value_kind, .. } => *value_kind,
        }
    }
    pub fn real(&self) -> Option<&SolveInitialValues> {
        match self {
            Self::Real(values) => Some(values),
            _ => None,
        }
    }
    pub fn is_missing(&self) -> bool {
        matches!(self, Self::Missing { .. })
    }
    pub fn is_repeat(&self) -> bool {
        match self {
            Self::Real(values) => {
                values.run_count() == 1
                    && values
                        .run(0)
                        .is_some_and(|run| run.repeated_bits().is_some())
            }
            Self::Integer(values) | Self::Enumeration(values) => values.is_repeat(),
            Self::Boolean(values) => values.is_repeat(),
            Self::Missing { .. } => false,
        }
    }
    pub fn require_finite(&self) -> Result<(), SolveInitialValuesError> {
        self.real()
            .map_or(Ok(()), SolveInitialValues::require_finite)
    }
}
impl From<SolveInitialValues> for SolveDeclarationStart {
    fn from(values: SolveInitialValues) -> Self {
        Self::Real(values)
    }
}
impl From<Vec<f64>> for SolveDeclarationStart {
    fn from(values: Vec<f64>) -> Self {
        Self::Real(values.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exact_repeat_wire_retains_large_integer_and_original_owner() {
        let values = SolveExactInitialValues::construct(
            120_000,
            vec![SolveExactInitialRun::Repeat {
                start: 0,
                value: i64::MAX,
                count: 120_000,
            }],
        )
        .unwrap();
        let copy = values.clone();
        assert!(Arc::ptr_eq(&values.plan, &copy.plan));
        let wire = serde_json::to_string(&values).unwrap();
        assert!(wire.len() < 150);
        let replay: SolveExactInitialValues<i64> = serde_json::from_str(&wire).unwrap();
        assert_eq!(replay.value(119_999), Some(&i64::MAX));
        assert_eq!(replay.value(120_000), None);
    }

    #[test]
    fn foreign_run_extent_and_missing_partition_are_not_constructible() {
        for wire in [
            serde_json::json!({"count": 2,"runs": [{"kind":"repeat","start":0,"value":i64::MAX,"count":1}]}),
            serde_json::json!({"count": 2,"runs": [{"kind":"repeat","start":1,"value":i64::MAX,"count":2}]}),
            serde_json::json!({"count": 0,"runs": [{"kind":"literal","start":0,"values":[]}]}),
        ] {
            assert!(serde_json::from_value::<SolveExactInitialValues<i64>>(wire).is_err());
        }
    }

    #[test]
    fn emptiness_follows_the_declared_scalar_count_of_every_start_kind() {
        let one = SolveExactInitialValues::construct(
            1,
            vec![SolveExactInitialRun::Repeat {
                start: 0,
                value: 3,
                count: 1,
            }],
        )
        .unwrap();
        let none = SolveExactInitialValues::<i64>::construct(0, Vec::new()).unwrap();
        assert!(!one.is_empty() && none.is_empty());
        assert!(!SolveDeclarationStart::Integer(one).is_empty());
        assert!(SolveDeclarationStart::Integer(none).is_empty());
        let missing = |count| SolveDeclarationStart::Missing {
            value_kind: SolveVariableValueKind::Boolean,
            count,
        };
        assert!(missing(0).is_empty() && !missing(2).is_empty());
    }
}
