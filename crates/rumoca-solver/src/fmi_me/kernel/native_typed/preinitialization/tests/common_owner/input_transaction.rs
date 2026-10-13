//! Count the actual same-kernel checkpoints and inject post-write failures.

use super::*;
use crate::fmi_me::kernel::native_typed::real::{InputCommitFault, fail_input_commit};
use crate::fmi_me::kernel::snapshot_count;

#[test]
fn covered_float64_write_uses_one_checkpoint_and_standalone_remains_atomic() {
    let component = fixture::reset_component();
    let mut kernel = entered(&component);
    let saved = kernel.fmu_state().unwrap();
    let before = snapshot_count();
    kernel
        .with_input_rollback(|write| write.set_native_float64(&[2], &[-0.0]))
        .unwrap();
    assert_eq!(snapshot_count() - before, 1);
    let mut value = [1.0];
    kernel.get_native_float64(&[2], &mut value).unwrap();
    assert_eq!(value[0].to_bits(), (-0.0_f64).to_bits());
    kernel.reset_to_fmu_state(&saved).unwrap();

    for fault in [InputCommitFault::BeforeSync, InputCommitFault::BeforeDelay] {
        fail_input_commit(fault);
        let before = snapshot_count();
        let (error, restored) = kernel
            .with_input_rollback(|write| {
                write.set_native_integer(&[3], &[i64::MIN])?;
                write.set_boolean(&[4], &[false])?;
                write.set_native_float64(&[2], &[-0.0])
            })
            .unwrap_err();
        assert!(
            error
                .to_string()
                .contains("injected native input commit failure")
        );
        restored.unwrap();
        assert_eq!(snapshot_count() - before, 1);
        assert!(kernel.verification_matches_snapshot(&saved));

        fail_input_commit(fault);
        let before = snapshot_count();
        assert!(kernel.set_native_float64(&[2], &[-0.0]).is_err());
        assert_eq!(snapshot_count() - before, 1);
        assert!(kernel.verification_matches_snapshot(&saved));
    }
}

#[test]
fn standalone_invalid_and_busy_float64_writes_take_no_checkpoint() {
    let component = fixture::reset_component();
    let mut kernel = entered(&component);
    let saved = kernel.fmu_state().unwrap();
    let before = snapshot_count();
    for (refs, values) in [(&[2, 1][..], &[9.0, 0.0][..]), (&[2][..], &[f64::NAN][..])] {
        assert!(kernel.set_native_float64(refs, values).is_err());
    }
    assert_eq!(snapshot_count(), before);
    let host = kernel.native_transport.as_ref().unwrap().borrow().host();
    let mut other = {
        let resource = kernel.native_transport.as_ref().unwrap().borrow();
        let (y, p) = resource.coordinates();
        host.transport_from_source(y, p).unwrap()
    };
    let gate = other.enter().unwrap();
    let before = snapshot_count();
    assert!(kernel.set_native_float64(&[2], &[-0.0]).is_err());
    assert_eq!(snapshot_count(), before);
    assert!(kernel.verification_matches_snapshot(&saved));
    drop(gate);
}
