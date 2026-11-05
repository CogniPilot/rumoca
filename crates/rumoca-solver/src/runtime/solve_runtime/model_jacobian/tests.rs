use super::*;
use std::cell::Cell;

struct Counted {
    calls: Cell<usize>,
}

impl CompiledSolveModelStateJacobian for Counted {
    fn call(
        &self,
        state: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.calls.set(self.calls.get() + 1);
        out.copy_from_slice(&[state[0], t, p[0], state[1]]);
        if t < 0.0 {
            return Err(refused("injected native failure"));
        }
        Ok(())
    }
}

fn cache() -> JacobianCache {
    JacobianCache {
        valid: false,
        time: 0,
        state: vec![0.0; 2],
        parameters: vec![0.0],
        matrix: vec![0.0; 4],
        output: vec![0.0; 2],
    }
}

#[test]
fn colored_seeds_share_one_jacobian_at_the_exact_coordinate() {
    let code = Counted {
        calls: Cell::new(0),
    };
    let mut cache = cache();
    let mut out = [f64::NAN; 2];
    for (seed, expected) in [
        ([1.0, 0.0], [2.0, 5.0]),
        ([0.0, 1.0], [7.0, 3.0]),
        ([1.0, 1.0], [9.0, 8.0]),
    ] {
        cache
            .apply(&code, 7.0, &[2.0, 3.0], &[5.0], &seed, &mut out)
            .unwrap();
        assert_eq!(out, expected);
    }
    assert_eq!(code.calls.get(), 1);
    cache
        .apply(&code, 8.0, &[2.0, 3.0], &[5.0], &[0.0, 1.0], &mut out)
        .unwrap();
    assert_eq!(out, [8.0, 3.0]);
    cache
        .apply(&code, 8.0, &[4.0, 3.0], &[5.0], &[1.0, 0.0], &mut out)
        .unwrap();
    assert_eq!(out, [4.0, 5.0]);
    cache
        .apply(&code, 8.0, &[4.0, 3.0], &[9.0], &[1.0, 0.0], &mut out)
        .unwrap();
    assert_eq!(out, [4.0, 9.0]);
    assert_eq!(code.calls.get(), 4);
}

#[test]
fn signed_zero_changes_invalidate_the_coordinate() {
    let code = Counted {
        calls: Cell::new(0),
    };
    let mut cache = cache();
    let mut out = [0.0; 2];
    cache
        .apply(&code, 0.0, &[0.0, 3.0], &[0.0], &[1.0, 0.0], &mut out)
        .unwrap();
    cache
        .apply(&code, -0.0, &[0.0, 3.0], &[0.0], &[1.0, 0.0], &mut out)
        .unwrap();
    cache
        .apply(&code, -0.0, &[-0.0, 3.0], &[0.0], &[1.0, 0.0], &mut out)
        .unwrap();
    cache
        .apply(&code, -0.0, &[-0.0, 3.0], &[-0.0], &[1.0, 0.0], &mut out)
        .unwrap();
    assert_eq!(code.calls.get(), 4);
}

#[test]
fn native_failure_preserves_output_and_cannot_accept_a_cached_point() {
    let code = Counted {
        calls: Cell::new(0),
    };
    let mut cache = cache();
    let mut out = [42.0, 43.0];
    for _ in 0..2 {
        assert!(
            cache
                .apply(&code, -1.0, &[2.0, 3.0], &[5.0], &[1.0, 1.0], &mut out)
                .is_err()
        );
        assert_eq!(out, [42.0, 43.0]);
        assert!(!cache.valid);
    }
    assert_eq!(code.calls.get(), 2);
    cache
        .apply(&code, 1.0, &[2.0, 3.0], &[5.0], &[1.0, 1.0], &mut out)
        .unwrap();
    assert_eq!(out, [3.0, 8.0]);
    assert_eq!(code.calls.get(), 3);
}

#[test]
fn state_seeds_keep_zero_extension_and_ignore_the_parameter_tail() {
    let code = Counted {
        calls: Cell::new(0),
    };
    let mut cache = cache();
    let mut out = [0.0; 2];
    cache
        .apply(&code, 7.0, &[2.0, 3.0], &[5.0], &[2.0], &mut out)
        .unwrap();
    assert_eq!(out, [4.0, 10.0]);
    cache
        .apply(
            &code,
            7.0,
            &[2.0, 3.0],
            &[5.0],
            &[2.0, 0.0, f64::NAN],
            &mut out,
        )
        .unwrap();
    assert_eq!(out, [4.0, 10.0]);
    assert_eq!(code.calls.get(), 1);
}

#[test]
fn invalid_extent_seed_and_overflow_never_publish_outputs() {
    let code = Counted {
        calls: Cell::new(0),
    };
    let mut cache = cache();
    let mut out = [42.0, 43.0];
    assert!(
        cache
            .apply(&code, 1.0, &[2.0], &[5.0], &[1.0, 1.0], &mut out)
            .is_err()
    );
    assert!(
        cache
            .apply(&code, 1.0, &[2.0, 3.0], &[5.0], &[f64::NAN], &mut out)
            .is_err()
    );
    assert_eq!(code.calls.get(), 0);
    assert!(
        cache
            .apply(&code, 1.0, &[f64::MAX, 3.0], &[5.0], &[2.0, 1.0], &mut out)
            .is_err()
    );
    assert_eq!(out, [42.0, 43.0]);
}

#[test]
fn unsupported_capability_retains_its_reason_and_does_not_execute() {
    let prepared = PreparedModelJacobian::Unsupported {
        reason: "algebraic projection".into(),
    };
    assert_eq!(prepared.unsupported_reason(), Some("algebraic projection"));
    assert!(prepared.jvp(0.0, &[], &[], &[], &mut []).is_none());
}
