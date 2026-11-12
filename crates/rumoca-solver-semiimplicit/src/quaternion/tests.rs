use super::*;
fn close<const N: usize>(actual: [f64; N], expected: [f64; N]) {
    for (a, e) in actual.into_iter().zip(expected) {
        assert!((a - e).abs() < 2e-14, "{a} != {e}");
    }
}
#[test]
fn nonidentity_orientation_uses_updated_body_angular_velocity_on_the_right() {
    let c = std::f64::consts::FRAC_1_SQRT_2;
    let q = UnitBodyQuaternion::checked([c, c, 0.0, 0.0]).unwrap();
    let after = q
        .advance([0.0, 0.0, std::f64::consts::FRAC_PI_2], 1.0)
        .unwrap();
    close(after.components(), [0.5, 0.5, -0.5, 0.5]);
    // The new body z is world -y after the initial 90-degree x rotation.
    close(
        q.body_vector_to_world([0.0, 0.0, 1.0]).unwrap(),
        [0.0, -1.0, 0.0],
    );
    close(
        after.body_vector_to_world([1.0, 0.0, 0.0]).unwrap(),
        [0.0, 0.0, 1.0],
    );
}
#[test]
fn half_turn_and_quaternion_sign_keep_the_same_rotations() {
    let q = UnitBodyQuaternion::checked([0.0, 1.0, 0.0, 0.0]).unwrap();
    let minus = UnitBodyQuaternion::checked([0.0, -1.0, 0.0, 0.0]).unwrap();
    close(
        q.body_vector_to_world([1.0, 2.0, 3.0]).unwrap(),
        [1.0, -2.0, -3.0],
    );
    let a = q.advance([0.3, -0.4, 0.5], 0.005).unwrap();
    let b = minus.advance([0.3, -0.4, 0.5], 0.005).unwrap();
    close(a.components(), b.components().map(|v| -v));
    close(
        a.body_vector_to_world([1.0, 2.0, 3.0]).unwrap(),
        b.body_vector_to_world([1.0, 2.0, 3.0]).unwrap(),
    );
}
#[test]
fn exponential_is_not_the_normalized_additive_euler_step() {
    let q = UnitBodyQuaternion::checked([1.0, 0.0, 0.0, 0.0]).unwrap();
    let a = q.advance([0.0, 0.0, std::f64::consts::PI], 1.0).unwrap();
    close(a.components(), [0.0, 0.0, 0.0, 1.0]);
    close(
        q.advance([0.0; 3], 0.005).unwrap().components(),
        q.components(),
    );
}
#[test]
fn invalid_domains_refuse_without_identity_or_radial_repair() {
    for q in [
        [0.0; 4],
        [2.0, 0.0, 0.0, 0.0],
        [f64::NAN, 0.0, 0.0, 0.0],
        [f64::INFINITY, 0.0, 0.0, 0.0],
        [1.0 + 1e-8, 0.0, 0.0, 0.0],
    ] {
        assert!(UnitBodyQuaternion::checked(q).is_none());
    }
    let q = UnitBodyQuaternion::checked([1.0, 0.0, 0.0, 0.0]).unwrap();
    for step in [0.0, -1.0, f64::NAN, f64::INFINITY] {
        assert!(q.advance([0.0; 3], step).is_none());
    }
    assert!(q.advance([f64::INFINITY, 0.0, 0.0], 0.005).is_none());
    assert!(q.body_vector_to_world([f64::NAN, 0.0, 0.0]).is_none());
}
