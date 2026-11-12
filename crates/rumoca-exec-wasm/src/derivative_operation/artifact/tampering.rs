//! Negative controls: a persisted helper that differs from its issued
//! emission in one module byte or one fault-table entry is refused at bind.
use super::*;
use rumoca_ir_solve::fmi::FmiComponent;

/// `w` is the velocity of `q`; `q` and `s` are kinematic, so both the
/// complete and the kinematic values helpers are persisted.
fn mixed() -> FmiComponent {
    let mut session = rumoca_compile::Session::default();
    session
        .add_document(
            "persisted-kinematic.mo",
            "model PersistedKinematic parameter Real k=2; Real q(start=1,fixed=true); \
             Real w(start=0.5,fixed=true); Real s(start=0,fixed=true); Real a; equation \
             a=k*q; der(q)=w; der(w)=-a; der(s)=a; end PersistedKinematic;",
        )
        .unwrap();
    let compiled = session.compile_model("PersistedKinematic").unwrap();
    rumoca_phase_solve::fmi::lower_to_fmi_component(&compiled.dae, &Default::default()).unwrap()
}

/// One forged entry in an otherwise issued fault table: the first entry's
/// status moved, or a gather fault added to an empty table.
fn change_fault_table(helper: &mut serde_json::Value) {
    for table in ["faults", "gather", "linear"] {
        if let Some(entry) = helper[table].as_array_mut().and_then(|t| t.first_mut()) {
            let status = entry["status"].as_u64().unwrap();
            entry["status"] = (status + 1000).into();
            return;
        }
    }
    let forged = crate::NativeGatherFault {
        status: 1000,
        kind: crate::TypedCallFaultKind::IndexBounds,
        kernel: 0,
        program: 0,
        operation: 0,
        region_path: Vec::new(),
        provenance: rumoca_core::Span::DUMMY,
    };
    helper["gather"]
        .as_array_mut()
        .unwrap()
        .push(serde_json::to_value(forged).unwrap());
}

#[test]
fn every_persisted_helper_byte_and_fault_table_is_re_derived_at_bind() {
    let component = mixed();
    let root = component.runtime_view().retain_assertion_root();
    let operation = root
        .try_derivative_operation()
        .unwrap()
        .try_causal_operation()
        .unwrap();
    assert!(operation.kinematic().is_some());
    let compiled = compile_fmi_derivative_operation_wasm(&operation).unwrap();
    let issued = PrecompiledFmiDerivativeOperation::from_compiled(&compiled);
    let json = serde_json::to_value(&issued).unwrap();
    assert!(issued.bind(&operation).is_ok());
    for (field, helper) in [
        ("values", FmiDerivativeHelperRole::Values),
        ("kinematic", FmiDerivativeHelperRole::Kinematic),
    ] {
        let mut byte = json.clone();
        let bytes = byte[field]["bytes"].as_array_mut().unwrap();
        let last = bytes.last_mut().unwrap();
        *last = (last.as_u64().unwrap() ^ 1).into();
        let mut faults = json.clone();
        change_fault_table(&mut faults[field]);
        for (tampered, part) in [
            (byte, FmiHelperMismatch::Module),
            (faults, FmiHelperMismatch::Faults),
        ] {
            let tampered: PrecompiledFmiDerivativeOperation =
                serde_json::from_value(tampered).unwrap();
            let expected = FmiDerivativeArtifactMismatch::Helper { helper, part };
            assert_eq!(tampered.mismatch(&issued), Some(expected), "{field}");
            let Err(error) = tampered.bind(&operation) else {
                panic!("a tampered {field} helper must not bind");
            };
            assert!(error.to_string().contains(&expected.to_string()), "{error}");
        }
    }
}
