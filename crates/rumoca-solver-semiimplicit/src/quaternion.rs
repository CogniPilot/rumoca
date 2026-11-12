//! Numerical-domain controls for the future geometric profile. The Cartesian
//! plugin does not select these helpers or accept quaternion states yet.

/// Scalar-first unit orientation with right-multiplied body-angular increments.
/// No zero-to-identity repair or source-observable radial scale is discarded.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct UnitBodyQuaternion {
    components: [f64; 4],
}
impl UnitBodyQuaternion {
    pub fn checked(components: [f64; 4]) -> Option<Self> {
        let norm = norm(&components);
        if !norm.is_finite() || (norm - 1.0).abs() > 1e-10 {
            return None;
        }
        Some(Self { components })
    }
    pub fn components(self) -> [f64; 4] {
        self.components
    }

    /// The caller supplies the already-updated angular velocity in the body's
    /// basis. This is exponential integration, not normalized additive Euler.
    pub fn advance(self, updated_body_omega: [f64; 3], step: f64) -> Option<Self> {
        if !step.is_finite() || step <= 0.0 {
            return None;
        }
        let speed = norm(&updated_body_omega);
        let angle = 0.5 * step * speed;
        if !speed.is_finite() || !angle.is_finite() {
            return None;
        }
        let factor = if speed == 0.0 {
            0.0
        } else {
            angle.sin() / speed
        };
        let [x, y, z] = updated_body_omega.map(|w| w * factor);
        let [a, b, c, d] = self.components;
        let w = angle.cos();
        let mut components = [
            a * w - b * x - c * y - d * z,
            a * x + b * w + c * z - d * y,
            a * y - b * z + c * w + d * x,
            a * z + b * y - c * x + d * w,
        ];
        let length = norm(&components);
        if !length.is_finite() || length == 0.0 {
            return None;
        }
        for value in &mut components {
            *value /= length;
        }
        Some(Self { components })
    }

    /// Explicit body-to-world rotation only; world linear velocities must not
    /// be passed through a rotation merely because orientation changes.
    pub fn body_vector_to_world(self, vector: [f64; 3]) -> Option<[f64; 3]> {
        if vector.iter().any(|v| !v.is_finite()) {
            return None;
        }
        let [a, b, c, d] = self.components;
        let axis = [b, c, d];
        let t = cross(axis, vector).map(|v| 2.0 * v);
        let second = cross(axis, t);
        let out = std::array::from_fn(|i| vector[i] + a * t[i] + second[i]);
        out.iter().all(|v| v.is_finite()).then_some(out)
    }
}
fn norm(values: &[f64]) -> f64 {
    values.iter().fold(0.0, |n, v| n.hypot(*v))
}
fn cross(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    ]
}
#[cfg(test)]
mod tests;
