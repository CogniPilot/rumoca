use super::*;
use crate::{ComputeBlock, ComputeNode, LinearOp, TensorNodeMetadata, TensorOutputMap};
use rumoca_core::{IndexBinderProvenance, StructuredIndexBinder, StructuredIndexDomain};

fn fixture_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_derivative_operation.mo"),
        0,
        1,
    )
}

fn state_component(with_values: bool) -> FmiComponent {
    valued_component(with_values.then_some(3.0))
}

/// The one-state component whose two derivative outputs are `value`, or that
/// has no derivative vector without one.
fn valued_component(value: Option<f64>) -> FmiComponent {
    let (mut model, input) = super::max_step_duration_local::delay_bearing_model_with_one_state();
    model.problem.events.delays = crate::SolveDelayPartition::default();
    if let Some(value) = value {
        let domain = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                provenance: IndexBinderProvenance::Generated,
                id: 0,
                display_name: "state".into(),
                lower: 1,
                upper: 2,
                step: 1,
            }],
        };
        model.problem.continuous.derivative_rhs = ComputeBlock {
            nodes: vec![ComputeNode::Map {
                output_map: TensorOutputMap::dense_contiguous(0, &domain).unwrap(),
                domain,
                base_ops: vec![
                    LinearOp::Const { dst: 0, value },
                    LinearOp::StoreOutput { src: 0 },
                ],
                load_strides: Vec::new(),
                const_strides: Vec::new(),
                metadata: TensorNodeMetadata::default(),
                span: fixture_span(),
            }],
        };
    }
    FmiComponent::construct(model, vec![input]).unwrap()
}

#[test]
fn derivative_operation_borrows_original_tensor_and_complete_refresh_owner() {
    let component = state_component(true);
    let model = component.runtime_view().model();
    let operation = component.runtime_view().try_derivative_operation().unwrap();
    assert_eq!(operation.vector_extent(), 2);
    assert!(std::ptr::eq(
        operation.values(),
        &model.problem.continuous.derivative_rhs
    ));
    assert!(matches!(
        operation.values().nodes.as_slice(),
        [ComputeNode::Map { .. }]
    ));
    assert!(std::ptr::eq(
        operation.dependencies(),
        model.problem.continuous.refresh_owners.derivative()
    ));
    assert!(std::ptr::eq(operation.calls(), &model.pure_calls));
    assert!(std::ptr::eq(operation.inventory(), component.metadata()));
    assert!(operation.staged_dependencies().unwrap().is_empty());
}

#[test]
fn derivative_operation_refuses_a_missing_state_vector_without_issuing_a_source() {
    let component = state_component(false);
    let error = component
        .runtime_view()
        .try_derivative_operation()
        .unwrap_err();
    assert!(matches!(
        error,
        FmiDerivativeOperationRefusal::OutputExtent {
            states: 2,
            outputs: 0
        }
    ));
    assert!(error.to_string().contains("0 output slots for 2 states"));
}

#[test]
fn an_empty_component_has_an_empty_derivative_vector() {
    let component = FmiComponent::construct(SolveModel::default(), Vec::new()).unwrap();
    let operation = component.runtime_view().try_derivative_operation().unwrap();
    assert_eq!(operation.vector_extent(), 0);
    assert!(operation.values().is_empty());
}

#[test]
fn retained_runtime_root_lends_the_identical_complete_operation() {
    let component = state_component(true);
    let root = component.runtime_view().retain_assertion_root();
    let direct = component.runtime_view().try_derivative_operation().unwrap();
    let retained = root.try_derivative_operation().unwrap();
    assert_eq!(retained.vector_extent(), direct.vector_extent());
    assert!(std::ptr::eq(retained.problem(), direct.problem()));
    assert!(std::ptr::eq(retained.values(), direct.values()));
    assert!(std::ptr::eq(retained.dependencies(), direct.dependencies()));
    assert!(std::ptr::eq(retained.calls(), direct.calls()));
    assert!(std::ptr::eq(retained.inventory(), direct.inventory()));
    drop(component);
    // The retained root, rather than a reconstructed model, keeps the source alive.
    let after = root.try_derivative_operation().unwrap();
    assert_eq!(after.vector_extent(), 2);
    assert!(std::ptr::eq(after.problem(), &root.model().problem));
}

#[test]
fn retained_runtime_root_preserves_missing_derivative_vector_refusal() {
    let component = state_component(false);
    let root = component.runtime_view().retain_assertion_root();
    assert!(matches!(
        root.try_derivative_operation(),
        Err(FmiDerivativeOperationRefusal::OutputExtent {
            states: 2,
            outputs: 0
        })
    ));
}

#[test]
fn causal_workspace_checks_complete_private_demand_and_keeps_counted_control() {
    let component = state_component(true);
    let operation = component
        .runtime_view()
        .try_derivative_operation()
        .unwrap()
        .try_causal_operation()
        .unwrap();
    let capacity = crate::CheckedNativeWasmCapacityPolicy::default();
    let control = operation.control_plan([1024, 2048], capacity).unwrap();
    assert_eq!(control.workspace().snapshot(), 16);
    assert_eq!(
        control.workspace().helper(),
        16 + control.workspace().y_bytes()
    );
    assert_eq!(
        control.workspace().bytes(),
        control.workspace().helper() + 2048
    );
    assert!(
        control
            .instructions()
            .contains(&FmiDerivativeControlInstruction::Loop)
    );
    assert!(
        !control
            .instructions()
            .contains(&FmiDerivativeControlInstruction::Call(
                FmiDerivativeHelper::Static
            ))
    );
    assert!(
        control
            .instructions()
            .contains(&FmiDerivativeControlInstruction::Call(
                FmiDerivativeHelper::Values
            ))
    );
    assert!(operation.control_plan([u32::MAX], capacity).is_err());
    let capacity = crate::CheckedNativeWasmCapacityPolicy::new(crate::NativeWasmCapacityRequest {
        private_arena_bytes: 65536,
        public_scratch_bytes: 65536,
    })
    .unwrap();
    assert!(operation.control_plan([65536], capacity).is_err());
}

/// The persisted-product key of the complete operation is its source
/// identity: the component built again from equal source keeps it, and one
/// whose derivative values differ does not.
#[test]
fn the_complete_operation_key_follows_its_source() {
    let key = |component: &FmiComponent| {
        let operation = component
            .runtime_view()
            .try_derivative_operation()
            .unwrap()
            .try_causal_operation()
            .unwrap();
        let source = operation.source();
        crate::PreparedKernelSource::new(&source.problem().layout, source.calls())
            .unwrap()
            .fmi_derivative(&operation)
            .unwrap()
    };
    let original = key(&valued_component(Some(3.0)));
    assert_eq!(original, key(&valued_component(Some(3.0))));
    assert_ne!(original, key(&valued_component(Some(4.0))));
}

/// The complete operation returns its status as the issued discriminant, and
/// the host decodes exactly those: each status from its own code, and no
/// status from any other code.
#[test]
fn the_host_decodes_exactly_the_issued_status_codes() {
    for status in [
        FmiDerivativeStatus::Success,
        FmiDerivativeStatus::InvalidBuffers,
        FmiDerivativeStatus::StaticRefresh,
        FmiDerivativeStatus::DynamicRefresh,
        FmiDerivativeStatus::Values,
        FmiDerivativeStatus::NonFinite,
    ] {
        assert_eq!(FmiDerivativeStatus::from_code(status as u32), Some(status));
    }
    for code in [6, 7, 255, u32::MAX] {
        assert_eq!(FmiDerivativeStatus::from_code(code), None, "code {code}");
    }
}
