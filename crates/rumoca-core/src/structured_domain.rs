use serde::{Deserialize, Serialize};

use crate::Expression;

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

/// An index expression written as `constant + Σ coeffs[b] · binder_b` over a
/// family's binder list (positional: `coeffs[b]` is the `b`-th binder's stride).
///
/// This is the compact, materialization-free description of how one array
/// subscript varies across a regular elementwise `for` family.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AffineForm {
    pub constant: i64,
    pub coeffs: Vec<i64>,
}

impl AffineForm {
    pub fn constant(value: i64, binder_count: usize) -> Self {
        Self {
            constant: value,
            coeffs: vec![0; binder_count],
        }
    }

    pub fn unit_binder(index: usize, binder_count: usize) -> Self {
        let mut coeffs = vec![0; binder_count];
        coeffs[index] = 1;
        Self {
            constant: 0,
            coeffs,
        }
    }

    /// True when the form has no binder dependence (a plain integer).
    pub fn is_binder_free(&self) -> bool {
        self.coeffs.iter().all(|c| *c == 0)
    }

    pub fn add(&self, other: &Self) -> Self {
        Self {
            constant: self.constant + other.constant,
            coeffs: self
                .coeffs
                .iter()
                .zip(&other.coeffs)
                .map(|(a, b)| a + b)
                .collect(),
        }
    }

    pub fn neg(&self) -> Self {
        Self {
            constant: -self.constant,
            coeffs: self.coeffs.iter().map(|c| -c).collect(),
        }
    }

    pub fn scale(&self, factor: i64) -> Self {
        Self {
            constant: self.constant * factor,
            coeffs: self.coeffs.iter().map(|c| c * factor).collect(),
        }
    }
}

/// One subscripted array access within a regular family body, with each
/// subscript dimension resolved to an [`AffineForm`] over the family binders.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArrayAccess {
    /// Dotted source name of the accessed variable (e.g. `u`, `body.x`).
    pub var: String,
    /// Per-dimension affine index, in subscript order.
    pub subscripts: Vec<AffineForm>,
}

impl ArrayAccess {
    /// Per-binder stride of this access's flat (scalar) element index.
    ///
    /// Given the accessed array's row-major memory strides (`memory_strides[k]`
    /// is the element-index step for a unit increment of subscript dimension
    /// `k`), the flat index is
    /// `Σ_k memory_strides[k] · (constant_k + Σ_b coeffs_k[b] · binder_b)`, so a
    /// unit increment of binder `b` moves the flat index by
    /// `Σ_k memory_strides[k] · coeffs_k[b]`. The returned vector holds that step
    /// for each binder `b` in `0..binder_count`. The affine *constants* (stencil
    /// offsets such as the `+1` in `u[i+1, j]`) do not affect the stride -- they
    /// shift only the base element index, which the Solve-IR carries in the base
    /// row's operations.
    ///
    /// `memory_strides` must have one entry per subscript dimension, i.e.
    /// `memory_strides.len() == self.subscripts.len()`.
    pub fn binder_index_strides(&self, memory_strides: &[i64], binder_count: usize) -> Vec<i64> {
        debug_assert_eq!(
            self.subscripts.len(),
            memory_strides.len(),
            "memory_strides must have one entry per subscript dimension"
        );
        (0..binder_count)
            .map(|binder| {
                self.subscripts
                    .iter()
                    .zip(memory_strides)
                    .map(|(subscript, memory_stride)| {
                        memory_stride * subscript.coeffs.get(binder).copied().unwrap_or(0)
                    })
                    .sum()
            })
            .collect()
    }
}

/// Row-major (last dimension contiguous) element-index strides for an array of
/// the given dimensions: `strides[k]` is the flat element-index step for a unit
/// increment of dimension `k`. For `[NX, NY]` this is `[NY, 1]`; for `[a, b, c]`
/// it is `[b*c, c, 1]`. An empty `dims` yields an empty stride vector.
///
/// These are the `memory_strides` consumed by [`ArrayAccess::binder_index_strides`].
pub fn row_major_strides(dims: &[usize]) -> Vec<i64> {
    let mut strides = vec![1i64; dims.len()];
    for k in (0..dims.len().saturating_sub(1)).rev() {
        strides[k] = strides[k + 1] * dims[k + 1] as i64;
    }
    strides
}

/// A regular elementwise `for` family: its (possibly nested) loop binders and
/// the affine array accesses appearing in its uniform body.
///
/// This is the compact, materialization-free description of the family that the
/// Solve-IR lowering needs to build an `AffineStencil`: one base row plus, per
/// access, the per-binder strides recorded here.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RegularForFamily {
    /// Loop binder names, outermost first (a nested `for i ... for j` is `[i, j]`).
    pub binders: Vec<String>,
    /// Every subscripted access in the body (loads and the output).
    pub accesses: Vec<ArrayAccess>,
}

/// The canonical "writable as an array comprehension" form of a structured family:
/// its loop body residual(s) written once with symbolic binder indices, e.g.
/// `{ body(i, j) for i in 1:NX, j in 1:NY }`. This is the source-of-truth shape of a
/// family — flatten captures it before expanding the loop, so downstream phases read
/// the template directly instead of reconstructing it from materialized corner cells.
///
/// [`RegularForFamily`] is a *derived* affine index over this template, present only
/// when every array access is affine in the binders. A family whose body uses
/// `sqrt`/`abs`/`if` (e.g. an immersed-boundary mask) is still a comprehension and
/// stays compact here even though it has no affine descriptor.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ComprehensionTemplate {
    /// One residual `Expression` per template equation, in the family's equation
    /// order. Binder indices appear as variable references whose names match the
    /// family `domain`'s binder `display_name`s; binder-dependent array subscripts
    /// are kept symbolic (e.g. `u[i - 1, j]`).
    pub body: Vec<Expression>,
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

    fn access(var: &str, subscripts: Vec<AffineForm>) -> ArrayAccess {
        ArrayAccess {
            var: var.to_string(),
            subscripts,
        }
    }

    #[test]
    fn row_major_strides_are_inner_dimension_contiguous() {
        assert_eq!(row_major_strides(&[]), Vec::<i64>::new());
        assert_eq!(row_major_strides(&[4]), vec![1]);
        assert_eq!(row_major_strides(&[3, 4]), vec![4, 1]);
        assert_eq!(row_major_strides(&[2, 3, 4]), vec![12, 4, 1]);
    }

    #[test]
    fn row_major_strides_compose_with_binder_index_strides() {
        // u[NX, NY] with NX=5, NY=4: a unit i-step moves the index by NY=4,
        // a unit j-step by 1 -- exactly what binder_index_strides recovers.
        let memory_strides = row_major_strides(&[5, 4]);
        let u_ij = access(
            "u",
            vec![
                AffineForm {
                    constant: 0,
                    coeffs: vec![1, 0],
                },
                AffineForm {
                    constant: 0,
                    coeffs: vec![0, 1],
                },
            ],
        );
        assert_eq!(u_ij.binder_index_strides(&memory_strides, 2), vec![4, 1]);
    }

    #[test]
    fn binder_index_strides_for_2d_stencil_offsets() {
        // A 2-D field `u[NX, NY]` is row-major, so a unit step of binder i (the
        // outer subscript) moves the flat index by NY and a unit step of binder j
        // by 1. The +1 offset in u[i+1, j] must not change the stride.
        let memory_strides = [4, 1]; // NY = 4
        let u_ij = access(
            "u",
            vec![
                AffineForm {
                    constant: 0,
                    coeffs: vec![1, 0],
                },
                AffineForm {
                    constant: 0,
                    coeffs: vec![0, 1],
                },
            ],
        );
        let u_ip1_j = access(
            "u",
            vec![
                AffineForm {
                    constant: 1,
                    coeffs: vec![1, 0],
                },
                AffineForm {
                    constant: 0,
                    coeffs: vec![0, 1],
                },
            ],
        );
        assert_eq!(u_ij.binder_index_strides(&memory_strides, 2), vec![4, 1]);
        assert_eq!(u_ip1_j.binder_index_strides(&memory_strides, 2), vec![4, 1]);
    }

    #[test]
    fn binder_index_strides_account_for_scaled_subscripts() {
        // A `2*i` outer subscript doubles the per-i stride.
        let memory_strides = [4, 1];
        let scaled = access(
            "u",
            vec![
                AffineForm {
                    constant: 0,
                    coeffs: vec![2, 0],
                },
                AffineForm {
                    constant: 0,
                    coeffs: vec![0, 1],
                },
            ],
        );
        assert_eq!(scaled.binder_index_strides(&memory_strides, 2), vec![8, 1]);
    }

    #[test]
    fn binder_index_strides_sum_across_subscript_dimensions() {
        // A coupled access u[i + j, j]: binder j appears in BOTH subscript
        // dimensions, so its stride sums both contributions (4*1 + 1*1 = 5),
        // while binder i appears only in the outer dimension (4).
        let memory_strides = [4, 1];
        let coupled = access(
            "u",
            vec![
                AffineForm {
                    constant: 0,
                    coeffs: vec![1, 1],
                }, // i + j
                AffineForm {
                    constant: 0,
                    coeffs: vec![0, 1],
                }, // j
            ],
        );
        assert_eq!(coupled.binder_index_strides(&memory_strides, 2), vec![4, 5]);
    }

    #[test]
    fn binder_index_strides_zero_for_binder_free_access() {
        // A boundary access at a fixed index (e.g. u[NX, j]) has no i-stride.
        let memory_strides = [4, 1];
        let boundary = access(
            "u",
            vec![
                AffineForm {
                    constant: 6,
                    coeffs: vec![0, 0],
                },
                AffineForm {
                    constant: 0,
                    coeffs: vec![0, 1],
                },
            ],
        );
        assert_eq!(
            boundary.binder_index_strides(&memory_strides, 2),
            vec![0, 1]
        );
    }
}
