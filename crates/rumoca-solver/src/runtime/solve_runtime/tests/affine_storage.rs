use super::*;
use crate::runtime::projection::{ImplicitProjectionModel, JacobianMatrix, JacobianStorage};
use nalgebra::DMatrix;
use prepared_projection_jacobian::{PreparedNative, mixed_coupled_model, prepare};

#[test]
fn poisoned_affine_storage_is_fresh_for_native_and_mixed_decline() {
    let mut runtime = SolveRuntime::new_fixture(&mixed_coupled_model()).unwrap();
    for native in [true, false] {
        prepare(&mut runtime, Rc::new(PreparedNative::default()));
        if !native {
            runtime.compiled_algebraic_jacobians[0] = None;
        }
        for k in [2.0, -3.0, 0.0] {
            *runtime.affine_jacobian_storage[0].borrow_mut() = Some(JacobianStorage::Dense(
                DMatrix::from_element(2, 2, f64::NAN),
            ));
            let mut y = [8.0, -5.0];
            runtime
                .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[k], 1e-10, 4)
                .unwrap();
            assert!((y[0] + k * y[1] - 2.0).abs() < 1e-10);
            assert!((k * y[0] - y[1] - 1.0).abs() < 1e-10);
            assert_eq!(
                runtime.affine_jacobian_storage[0]
                    .borrow()
                    .as_ref()
                    .unwrap()
                    .as_slice(),
                &[1.0, k, k, -1.0]
            );
        }
    }
}

#[test]
fn failed_native_affine_fill_releases_storage_without_replay_or_coordinate_writes() {
    let mut runtime = SolveRuntime::new_fixture(&mixed_coupled_model()).unwrap();
    let native = Rc::new(PreparedNative::failing());
    prepare(&mut runtime, native.clone());
    let mut y = [8.0, -5.0];
    for iteration in 0..2 {
        let error = runtime
            .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[2.0], 1e-10, 4)
            .unwrap_err();
        assert!(error.to_string().contains("prepared projection failed"));
        assert_eq!(y, [8.0, -5.0]);
        assert_eq!(native.calls.get(), iteration + 1);
        assert!(runtime.affine_jacobian_storage[0].try_borrow_mut().is_ok());
    }
    prepare(&mut runtime, Rc::new(PreparedNative::default()));
    runtime
        .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[2.0], 1e-10, 4)
        .unwrap();
    assert!((y[0] + 2.0 * y[1] - 2.0).abs() < 1e-10);
    assert!((2.0 * y[0] - y[1] - 1.0).abs() < 1e-10);
}

#[test]
fn affine_lease_rejects_reentry_wrong_owner_coordinates_and_extent() {
    let runtime = SolveRuntime::new_fixture(&mixed_coupled_model()).unwrap();
    let other = SolveRuntime::new_fixture(&mixed_coupled_model()).unwrap();
    let projection = native_projection_jvp::selected_projection(&runtime, false);
    let structure = &runtime.continuous_structural.algebraic_projection()[0];
    let application = structure.jacobian_application().unwrap();
    let coordinates = (application.rows(), application.y_indices());
    let mut lease = projection
        .lease_affine_jacobian(structure, coordinates, 2)
        .unwrap()
        .unwrap();
    lease.as_mut_slice().fill(123.0);
    let mut y = [8.0, -5.0];
    let error = runtime
        .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[2.0], 1e-10, 4)
        .unwrap_err();
    assert!(error.to_string().contains("already leased"));
    assert_eq!(y, [8.0, -5.0]);
    assert!(lease.as_slice().iter().all(|value| *value == 123.0));
    drop(lease);
    assert!(
        projection
            .lease_affine_jacobian(structure, coordinates, 3)
            .is_err()
    );
    assert!(
        projection
            .lease_affine_jacobian(structure, (&[0, 1], coordinates.1), 2)
            .is_err()
    );
    assert!(
        projection
            .lease_affine_jacobian(
                &other.continuous_structural.algebraic_projection()[0],
                coordinates,
                2
            )
            .is_err()
    );
    *runtime.affine_jacobian_storage[0].borrow_mut() =
        Some(JacobianStorage::Dense(DMatrix::zeros(1, 4)));
    assert!(
        projection
            .lease_affine_jacobian(structure, coordinates, 2)
            .is_err()
    );
    assert!(runtime.affine_jacobian_storage[0].try_borrow_mut().is_ok());
}

#[test]
fn affine_storage_rejects_mutated_coordinate_partition() {
    let mut runtime = SolveRuntime::new_fixture(&mixed_coupled_model()).unwrap();
    runtime.state_count += 1;
    let projection = native_projection_jvp::selected_projection(&runtime, false);
    let structure = &runtime.continuous_structural.algebraic_projection()[0];
    let application = structure.jacobian_application().unwrap();
    assert!(
        projection
            .lease_affine_jacobian(structure, (application.rows(), application.y_indices()), 2)
            .is_err()
    );
    assert!(runtime.affine_jacobian_storage[0].borrow().is_none());
}
