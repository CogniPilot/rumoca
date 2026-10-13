//! Consuming handoff receipts for the existing retained component and driver.

use super::*;

impl MeRetainedComponent {
    /// Complete only the actual native FMI Initialization Mode through the existing driver.
    #[cfg(feature = "native-wasm")]
    pub fn complete_native_initialization_retaining(
        self,
        options: MeSessionOptions,
    ) -> Result<MeSimulationSession<'static, 'static>, (Box<Self>, MeSessionError)> {
        if let Err(error) = self.require_native_initialization() {
            return Err((Box::new(self), error.into()));
        }
        self.into_session_retaining(options, None)
    }

    #[cfg(feature = "native-wasm")]
    fn require_native_initialization(&self) -> Result<(), crate::fmi_me::MeError> {
        self.kernel.borrow().and_then(|kernel| {
            if kernel.has_native_resource()
                && kernel.initialization_state() == crate::fmi_me::lifecycle::MeState::InitializationMode
            {
                Ok(())
            } else {
                Err(crate::fmi_me::MeError::Contract {
                    reason: "native initialization completion requires the actual Initialization Mode resource".into(),
                })
            }
        })
    }

    /// Complete actual native initialization using a plugin over the issued host setup.
    #[cfg(feature = "native-wasm")]
    pub fn complete_native_initialization_with_integrator_retaining(
        self,
        options: MeSessionOptions,
        integrator: fn(MeNumericalSetup) -> Box<dyn MeIntegratorBackend + 'static>,
    ) -> Result<MeSimulationSession<'static, 'static>, (Box<Self>, MeSessionError)> {
        if let Err(error) = self.require_native_initialization() {
            return Err((Box::new(self), error.into()));
        }
        self.attach_session_retaining(options, |host| {
            if host.state_count() == 0 {
                Ok(None)
            } else {
                Ok(Some(integrator(host.numerical_setup(None)?)))
            }
        })
    }

    /// Attach the original host/plugin while retaining this component on failure.
    /// Failure preserves the actual lifecycle and effects; it promises no rollback.
    pub fn into_session_retaining<'plugin>(
        self,
        options: MeSessionOptions,
        backend: Option<Box<dyn MeIntegratorBackend + 'plugin>>,
    ) -> Result<MeSimulationSession<'static, 'plugin>, (Box<Self>, MeSessionError)> {
        self.attach_session_retaining(options, |_| Ok(backend))
    }

    fn attach_session_retaining<'plugin>(
        self,
        options: MeSessionOptions,
        backend: impl FnOnce(
            &MeComponentHost<'_>,
        )
            -> Result<Option<Box<dyn MeIntegratorBackend + 'plugin>>, MeSessionError>,
    ) -> Result<MeSimulationSession<'static, 'plugin>, (Box<Self>, MeSessionError)> {
        let attached = (|| {
            self.kernel
                .prepare_host(options.start_time(), options.stop_time())?;
            let host = build_host_state(Rc::clone(&self.kernel), options)?;
            let host = MeComponentHost {
                host,
                owned_component: None,
                lease: PhantomData,
            };
            let backend = backend(&host)?;
            host.into_session(backend)
        })();
        match attached {
            Ok(mut session) => {
                session.owned_component = Some(self);
                Ok(session)
            }
            Err(error) => Err((Box::new(self), error)),
        }
    }
}

impl MeSimulationSession<'_, '_> {
    /// End this owned session and return its actual retained component for FMI Reset.
    /// A borrowed lease cannot manufacture a second retained owner. Completed host
    /// calls, including DoStep, have already closed their activation guard.
    pub fn into_retained_component(
        mut self,
    ) -> Result<MeRetainedComponent, (Box<Self>, MeSessionError)> {
        if self.owned_component.is_none() || self.host.derivatives.is_active() {
            let reason = if self.owned_component.is_none() {
                "a borrowed session has no consuming retained-component receipt"
            } else {
                "session recovery requires the current derivative call to have closed"
            };
            return Err((
                Box::new(self),
                MeSessionError::Contract {
                    reason: reason.into(),
                },
            ));
        }
        let retained = self.owned_component.take().expect("owned receipt checked");
        // No activation guard can survive a host call. Escaped handles stay inactive;
        // dropping the numerical plugin and host ends their driver ownership.
        let Self {
            host,
            backend,
            scan_workspace,
            owned_component: _,
            lease: _,
        } = self;
        drop(backend);
        drop(scan_workspace);
        drop(host);
        Ok(retained)
    }
}

#[cfg(test)]
mod tests;
