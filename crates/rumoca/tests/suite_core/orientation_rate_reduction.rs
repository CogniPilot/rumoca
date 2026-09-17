//! Index reduction of an orientation lock closed through a buried quaternion rate.
//!
//! A holonomic orientation lock whose rate enters only through the quaternion
//! kinematic relation `w = 2*G(q)*der(q)` has no bare `der(q)` definition, so
//! the ordinary matching reducer cannot differentiate the lock to acceleration
//! level and leaves the system under-reduced. The formal-derivative
//! prolongation differentiates whole equations, including the gauge row that
//! the unit-norm constraint `q*q = 1` contributes, and reduces the same system
//! to a perfect matching once the independent position-and-velocity coordinate
//! is selected. These two tests pin both halves: the ordinary reducer's
//! under-reduction (so constrained orientation systems must route to the formal
//! path) and the formal path's complete reduction (MLS 3.6 section 3.7.4 for
//! `der()`; STRUCT-T07 / SPEC_0007).

use rumoca::Compiler;
use rumoca_ir_dae as dae;
use rumoca_phase_structural::{StructuralError, construct_formal_derivatives, prepare_for_solve};

fn quaternion_lock_inline() -> std::sync::Arc<dae::Dae> {
    Compiler::new()
        .model("QuaternionLockInline")
        .compile_str(
            include_str!("../fixtures/index_reduction/QuaternionLockInline.mo"),
            "QuaternionLockInline.mo",
        )
        .unwrap()
        .dae
}

#[test]
fn ordinary_reducer_leaves_the_orientation_lock_under_reduced() {
    let dae = quaternion_lock_inline();
    let Err(error) = prepare_for_solve(&dae) else {
        panic!("the buried quaternion rate must keep the lock under-reduced");
    };
    let StructuralError::Singular {
        n_matched,
        n_equations,
        unmatched_unknowns,
        ..
    } = error
    else {
        panic!("expected a structurally singular result, got {error}");
    };
    assert_eq!((n_matched, n_equations), (6, 9));
    // The acceleration-level unknowns and the quaternion rate stay unmatched.
    for unknown in ["der(q[4])", "a1", "a2"] {
        assert!(
            unmatched_unknowns.iter().any(|name| name == unknown),
            "{unknown} must remain unmatched in the under-reduced system, got {unmatched_unknowns:?}"
        );
    }
}

#[test]
fn formal_path_reduces_the_orientation_lock_to_a_perfect_matching() {
    let dae = quaternion_lock_inline();
    let formal = construct_formal_derivatives(&dae).expect("the prolongation constructs");
    assert_eq!(formal.inspect(|system| system.formal_dimension()), 2);
    let candidate = formal
        .construct_state_candidate(|view| {
            let q = view
                .source
                .variables()
                .find(|(_, variable)| variable.name().as_str() == "q")
                .expect("the quaternion state is retained")
                .0;
            // The one independent coordinate of the single locked rotational
            // freedom: its position and its velocity successor.
            Ok(vec![
                view.state_coordinate(q, 0, 0)?,
                view.state_coordinate(q, 1, 0)?,
            ])
        })
        .expect("the position-and-velocity selection rebuilds");
    candidate
        .into_prepared()
        .expect("the formal path reduces the orientation lock to a perfect matching");
}
