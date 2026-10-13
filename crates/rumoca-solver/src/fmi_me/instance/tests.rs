use super::*;

impl MeKernelCell {
    pub(in crate::fmi_me) fn with_prepared_for_control<T>(
        &self,
        inspect: impl FnOnce(&MePendingNativeComponent) -> T,
    ) -> T {
        let resource = self.resource.borrow();
        let MeKernelResource::Prepared(pending) = &*resource else {
            panic!("the owner control requires actual preparation")
        };
        inspect(pending)
    }

    pub(in crate::fmi_me) fn activate_for_control(
        &self,
        config: &MeInstanceConfig,
    ) -> Result<(), MeError> {
        self.activate_prepared_owned(config)
    }
}

#[test]
fn an_interrupted_ownership_transition_leaves_the_resource_refusing_every_request() {
    use crate::fmi_me::tests::{harmonic_oscillator, refresh_owned};
    let model = refresh_owned(harmonic_oscillator());
    let kernel = SolveMeKernel::instantiate(
        crate::fmi_me::MeModelSource::fixture(&model),
        &MeInstanceConfig::new("vacated", 1.0e-10, 0.0, 1.0).unwrap(),
    )
    .unwrap();
    let cell = MeKernelCell::new(kernel);
    let interrupted = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        transition(
            &mut cell.resource.borrow_mut(),
            |_| -> (MeKernelResource, ()) { panic!("ownership move interrupted") },
        )
    }));
    assert!(interrupted.is_err());

    let refusal = "interrupted ownership transition";
    assert!(cell.terminate().unwrap_err().to_string().contains(refusal));
    assert!(
        cell.native_value_count(&[0], rumoca_ir_solve::SolveVariableValueKind::Real)
            .unwrap_err()
            .to_string()
            .contains(refusal)
    );
    assert!(cell.reset().is_err());
    assert_eq!(cell.diagnostics_since(0), (Vec::new(), 0));
    assert_eq!(cell.with_model(|_| ()), None);
}
