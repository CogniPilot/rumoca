//! Complete source-bound update classification; no integrator selection or FMI operation.
use crate::{
    ComputeBlock,
    state_derivative_sources::{self, StateSource},
};

/// Scalar-first orientation and body-angular velocity state coordinates.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct QuaternionStateGroup {
    quaternion: [usize; 4],
    angular_velocity: [usize; 3],
}
impl QuaternionStateGroup {
    pub fn quaternion(&self) -> &[usize; 4] {
        &self.quaternion
    }
    pub fn angular_velocity(&self) -> &[usize; 3] {
        &self.angular_velocity
    }
}

/// Every state has exactly one update role. No names, caller-provided offsets,
/// serialized assertion or floating-point probe establishes these roles.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GeometricStepPlan {
    quaternions: Vec<QuaternionStateGroup>,
    cartesian_pairs: Vec<[usize; 2]>,
    auxiliary: Vec<usize>,
}
impl GeometricStepPlan {
    /// Derive once from source outputs and facts matching the actual call table.
    /// A source relation does not prove the unit-quaternion numerical domain.
    pub fn from_derivative(
        derivative: &ComputeBlock,
        width: usize,
        calls: &crate::SolvePureCallTable,
    ) -> Option<Self> {
        let source = state_derivative_sources::derive(derivative, width, Some(calls))?;
        let sources = &source.rows;
        let mut covered = vec![false; width];
        let quaternions = quaternion_groups(&source, &mut covered)?;
        if quaternions.is_empty() {
            return None;
        }
        let mut cartesian_pairs = Vec::new();
        for (position, source) in sources.iter().enumerate() {
            if covered[position] {
                continue;
            }
            let Some(velocity) = source.and_then(StateSource::direct) else {
                continue;
            };
            if velocity == position
                || covered[velocity]
                || sources[velocity].and_then(StateSource::direct).is_some()
            {
                return None;
            }
            covered[position] = true;
            covered[velocity] = true;
            cartesian_pairs.push([position, velocity]);
        }
        let auxiliary = covered
            .iter()
            .enumerate()
            .filter_map(|(i, used)| (!used).then_some(i))
            .collect();
        Some(Self {
            quaternions,
            cartesian_pairs,
            auxiliary,
        })
    }
    pub fn quaternions(&self) -> &[QuaternionStateGroup] {
        &self.quaternions
    }
    pub fn cartesian_pairs(&self) -> &[[usize; 2]] {
        &self.cartesian_pairs
    }
    /// Original first-order derivative updates, with no omitted auxiliary state.
    pub fn auxiliary(&self) -> &[usize] {
        &self.auxiliary
    }
}

fn quaternion_groups(
    derived: &state_derivative_sources::StateDerivativeSources,
    covered: &mut [bool],
) -> Option<Vec<QuaternionStateGroup>> {
    let mut groups = Vec::new();
    let sources = &derived.rows;
    for (output, source) in sources.iter().enumerate() {
        let Some(StateSource::Quaternion { binding, lane }) = source else {
            continue;
        };
        let state_derivative_sources::QuaternionBinding { q, w } =
            derived.quaternions.get(*binding)?;
        if q[*lane] != output {
            return None;
        }
        if *lane != 0 {
            continue;
        }
        for (lane, index) in q.iter().copied().enumerate() {
            if sources.get(index)?
                != &Some(StateSource::Quaternion {
                    binding: *binding,
                    lane,
                })
            {
                return None;
            }
        }
        for index in q.iter().chain(w).copied() {
            if std::mem::replace(covered.get_mut(index)?, true) {
                return None;
            }
        }
        groups.push(QuaternionStateGroup {
            quaternion: *q,
            angular_velocity: *w,
        });
    }
    Some(groups)
}
