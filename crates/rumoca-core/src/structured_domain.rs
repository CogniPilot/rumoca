use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StructuredIndexBinder {
    pub id: usize,
    pub display_name: String,
    pub lower: i64,
    pub upper: i64,
    pub step: i64,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StructuredIndexDomain {
    pub binders: Vec<StructuredIndexBinder>,
}

impl StructuredIndexDomain {
    pub fn scalar_count(&self) -> Result<usize, StructuredIndexDomainError> {
        self.validate()
    }

    pub fn index_tuples(&self) -> Result<Vec<Vec<i64>>, StructuredIndexDomainError> {
        let tuple_count = self.validate()?;
        let mut tuples = Vec::new();
        reserve_tuple_capacity(&mut tuples, tuple_count)?;
        let values = self
            .binders
            .iter()
            .map(StructuredIndexBinder::values)
            .collect::<Result<Vec<_>, _>>()?;
        let mut current = Vec::new();
        reserve_current_tuple_capacity(&mut current, values.len())?;
        push_index_tuples(&values, 0, &mut current, &mut tuples)?;
        Ok(tuples)
    }

    pub fn validate(&self) -> Result<usize, StructuredIndexDomainError> {
        let mut count = 1usize;
        for binder in &self.binders {
            count = count
                .checked_mul(binder.value_count()?)
                .ok_or(StructuredIndexDomainError::ScalarCountOverflow)?;
        }
        Ok(count)
    }
}

fn push_index_tuples(
    values: &[Vec<i64>],
    depth: usize,
    current: &mut Vec<i64>,
    tuples: &mut Vec<Vec<i64>>,
) -> Result<(), StructuredIndexDomainError> {
    if depth == values.len() {
        let mut tuple = Vec::new();
        reserve_current_tuple_capacity(&mut tuple, current.len())?;
        tuple.extend(current.iter().copied());
        tuples.push(tuple);
        return Ok(());
    }
    for value in &values[depth] {
        current.push(*value);
        push_index_tuples(values, depth + 1, current, tuples)?;
        current.pop();
    }
    Ok(())
}

fn reserve_tuple_capacity(
    tuples: &mut Vec<Vec<i64>>,
    capacity: usize,
) -> Result<(), StructuredIndexDomainError> {
    tuples
        .try_reserve_exact(capacity)
        .map_err(|_| StructuredIndexDomainError::IndexTupleCapacityOverflow)
}

fn reserve_current_tuple_capacity(
    tuple: &mut Vec<i64>,
    capacity: usize,
) -> Result<(), StructuredIndexDomainError> {
    tuple
        .try_reserve_exact(capacity)
        .map_err(|_| StructuredIndexDomainError::IndexTupleCapacityOverflow)
}

impl StructuredIndexBinder {
    fn value_count(&self) -> Result<usize, StructuredIndexDomainError> {
        if self.step == 0 {
            return Err(StructuredIndexDomainError::ZeroStep {
                binder_id: self.id,
                display_name: self.display_name.clone(),
            });
        }
        let count = if self.step > 0 {
            self.positive_value_count()
        } else {
            self.negative_value_count()
        };
        usize::try_from(count).map_err(|_| StructuredIndexDomainError::ScalarCountOverflow)
    }

    fn positive_value_count(&self) -> u128 {
        if self.lower > self.upper {
            return 0;
        }
        let distance = (self.upper as i128 - self.lower as i128) as u128;
        let step = self.step as u128;
        distance / step + 1
    }

    fn negative_value_count(&self) -> u128 {
        if self.lower < self.upper {
            return 0;
        }
        let distance = (self.lower as i128 - self.upper as i128) as u128;
        let step = -(self.step as i128);
        distance / step as u128 + 1
    }

    fn values(&self) -> Result<Vec<i64>, StructuredIndexDomainError> {
        let count = self.value_count()?;
        let mut values = Vec::new();
        values
            .try_reserve_exact(count)
            .map_err(|_| StructuredIndexDomainError::IndexTupleCapacityOverflow)?;
        let mut value = self.lower;
        for _ in 0..count {
            values.push(value);
            if values.len() == count {
                break;
            }
            value = value.checked_add(self.step).ok_or_else(|| {
                StructuredIndexDomainError::BinderValueOverflow {
                    binder_id: self.id,
                    display_name: self.display_name.clone(),
                }
            })?;
        }
        Ok(values)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum StructuredIndexDomainError {
    ZeroStep {
        binder_id: usize,
        display_name: String,
    },
    ScalarCountOverflow,
    BinderValueOverflow {
        binder_id: usize,
        display_name: String,
    },
    IndexTupleCapacityOverflow,
}

impl std::fmt::Display for StructuredIndexDomainError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ZeroStep {
                binder_id,
                display_name,
            } => write!(
                f,
                "index binder `{display_name}` ({binder_id}) has zero step"
            ),
            Self::ScalarCountOverflow => {
                write!(f, "structured domain scalar count overflows usize")
            }
            Self::BinderValueOverflow {
                binder_id,
                display_name,
            } => write!(
                f,
                "index binder `{display_name}` ({binder_id}) value stepping overflows i64"
            ),
            Self::IndexTupleCapacityOverflow => {
                write!(
                    f,
                    "structured domain index tuple capacity exceeds host memory limits"
                )
            }
        }
    }
}

impl std::error::Error for StructuredIndexDomainError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn structured_index_domain_error_displays_zero_step() {
        let error = StructuredIndexDomainError::ZeroStep {
            binder_id: 3,
            display_name: "i".to_string(),
        };

        assert_eq!(error.to_string(), "index binder `i` (3) has zero step");
    }

    #[test]
    fn structured_index_domain_error_displays_scalar_count_overflow() {
        assert_eq!(
            StructuredIndexDomainError::ScalarCountOverflow.to_string(),
            "structured domain scalar count overflows usize"
        );
    }

    #[test]
    fn index_tuples_enumerates_cartesian_domain() {
        let domain = StructuredIndexDomain {
            binders: vec![
                StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 2,
                    step: 1,
                },
                StructuredIndexBinder {
                    id: 1,
                    display_name: "j".to_string(),
                    lower: 3,
                    upper: 4,
                    step: 1,
                },
            ],
        };

        assert_eq!(
            domain.index_tuples(),
            Ok(vec![vec![1, 3], vec![1, 4], vec![2, 3], vec![2, 4]])
        );
    }

    #[test]
    fn index_tuples_rejects_zero_step() {
        let domain = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: 7,
                display_name: "k".to_string(),
                lower: 1,
                upper: 3,
                step: 0,
            }],
        };

        assert_eq!(
            domain.index_tuples(),
            Err(StructuredIndexDomainError::ZeroStep {
                binder_id: 7,
                display_name: "k".to_string()
            })
        );
    }

    #[test]
    fn index_tuples_rejects_scalar_count_overflow() {
        let domain = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: i64::MIN,
                upper: i64::MAX,
                step: 1,
            }],
        };

        assert_eq!(
            domain.index_tuples(),
            Err(StructuredIndexDomainError::ScalarCountOverflow)
        );
    }
}
