//! Ordered semantic occurrences with a membership-only index.
use crate::projection::HashSet;

use super::FunctionParameterDependency;

#[derive(Debug, Default)]
pub(super) struct OrderedDependencies {
    values: Vec<FunctionParameterDependency>,
    membership: HashSet<FunctionParameterDependency>,
}

impl OrderedDependencies {
    pub(super) fn insert(&mut self, dependency: &FunctionParameterDependency) {
        if self.membership.insert(dependency.clone()) {
            self.values.push(dependency.clone());
        }
    }

    #[cfg(test)]
    pub(super) fn values(&self) -> &[FunctionParameterDependency] {
        &self.values
    }

    pub(super) fn into_values(self) -> Vec<FunctionParameterDependency> {
        self.values
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn full_identity_and_first_occurrence_order_match_independent_dense_oracle() {
        let a = FunctionParameterDependency::Scalar {
            activation: crate::projection::Activation::Guaranteed,
            parameter: 0,
            scalar: 11,
        };
        let b = FunctionParameterDependency::Scalar {
            activation: crate::projection::Activation::Guaranteed,
            parameter: 1,
            scalar: 11,
        };
        let c = FunctionParameterDependency::RecordField {
            activation: crate::projection::Activation::Guaranteed,
            parameter: 0,
            field: 0,
            scalar: 11,
        };
        let d = FunctionParameterDependency::RecordField {
            activation: crate::projection::Activation::Guaranteed,
            parameter: 0,
            field: 1,
            scalar: 11,
        };
        let e = FunctionParameterDependency::Scalar {
            activation: crate::projection::Activation::Guaranteed,
            parameter: 0,
            scalar: 12,
        };
        let sequence = [
            c.clone(),
            a.clone(),
            c,
            b.clone(),
            d.clone(),
            a,
            e.clone(),
            d,
            b,
            e,
        ];
        let mut expected = Vec::new();
        let mut actual = OrderedDependencies::default();
        for key in sequence {
            if !expected.contains(&key) {
                expected.push(key.clone());
            }
            actual.insert(&key);
        }
        assert_eq!(actual.values(), expected);
        assert_eq!(actual.into_values(), expected);
    }

    #[test]
    fn full_14400_capture_retains_order_after_reversed_duplicates() {
        let mut actual = OrderedDependencies::default();
        for scalar in 0..14400 {
            actual.insert(&FunctionParameterDependency::Scalar {
                activation: crate::projection::Activation::Guaranteed,
                parameter: 0,
                scalar,
            });
        }
        for scalar in (0..14400).rev() {
            actual.insert(&FunctionParameterDependency::Scalar {
                activation: crate::projection::Activation::Guaranteed,
                parameter: 0,
                scalar,
            });
        }
        let expected = (0..14400)
            .map(|scalar| FunctionParameterDependency::Scalar {
                activation: crate::projection::Activation::Guaranteed,
                parameter: 0,
                scalar,
            })
            .collect::<Vec<_>>();
        assert_eq!(actual.into_values(), expected);
        assert!(OrderedDependencies::default().into_values().is_empty());
    }
}
