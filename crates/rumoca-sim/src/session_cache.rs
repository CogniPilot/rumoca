//! One idle prepared session, reused only through the original reset owner.

mod options_key;
#[cfg(test)]
mod tests;

use options_key::OptionsKey;
use std::{
    cell::RefCell,
    rc::{Rc, Weak},
    sync::Arc,
};

use crate::{
    SessionCommand, SessionEvent, SimOptions, SimulationDiagnosticError, SimulationSession,
};

struct SessionKey {
    dae: Arc<rumoca_ir_dae::Dae>,
    options: OptionsKey,
}

impl SessionKey {
    fn matches(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.dae, &other.dae) && self.options == other.options
    }
}

struct IdleSession {
    key: SessionKey,
    session: SimulationSession,
}

#[derive(Default)]
struct CacheState {
    idle: Option<IdleSession>,
    generation: Rc<()>,
}

/// Retains at most one idle instance in this process/WASM instance, never IR
/// bytes on disk. The caller supplies the compiler's current checked DAE owner;
/// only the compiler decides whether edits preserve that owner.
#[derive(Clone, Default)]
pub struct SimulationSessionCache(Rc<RefCell<CacheState>>);

impl SimulationSessionCache {
    /// Create an independent live session or exclusively check out the idle
    /// one. A hit executes the existing original-default Reset, including
    /// initialization and plugin-history reset. A failed reset is returned;
    /// it never triggers a second preparation attempt in this request.
    pub fn acquire(
        &self,
        dae: Arc<rumoca_ir_dae::Dae>,
        options: SimOptions,
    ) -> Result<SimulationSessionLease, SimulationDiagnosticError> {
        let key = SessionKey {
            dae,
            options: OptionsKey::new(&options)?,
        };
        let (idle, generation) = {
            let mut state = self.0.borrow_mut();
            (state.idle.take(), Rc::clone(&state.generation))
        };
        let (session, reused) = match idle.filter(|entry| entry.key.matches(&key)) {
            Some(mut entry) => {
                entry.session.reset(options.t_start)?;
                (entry.session, true)
            }
            None => (SimulationSession::new(&key.dae, options)?, false),
        };
        Ok(SimulationSessionLease {
            session: Some(session),
            key: Some(key),
            cache: Rc::downgrade(&self.0),
            generation,
            reusable: true,
            reused,
        })
    }

    /// Release idle resources and prevent older active leases from repopulating
    /// this cache. Active sessions continue to own their original model.
    pub fn clear(&self) {
        let mut state = self.0.borrow_mut();
        state.idle = None;
        state.generation = Rc::new(());
    }
}

/// Exclusive ownership of one actual session. There is no Clone or shared
/// mutable session handle. Dropping it evaluates no Modelica code; the next
/// explicit acquire performs Reset before publishing a hit. Fallible reads must
/// use `apply` so errors prevent reuse; only passive time inspection is exposed.
///
/// ```compile_fail,E0599
/// use rumoca_sim::SimulationSessionLease;
/// fn read_without_tracking(lease: &SimulationSessionLease) {
///     let _ = lease.get("x");
/// }
/// ```
///
/// ```compile_fail,E0599
/// use rumoca_sim::SimulationSessionLease;
/// fn state_without_tracking(lease: &SimulationSessionLease) {
///     let _ = lease.state();
/// }
/// ```
pub struct SimulationSessionLease {
    session: Option<SimulationSession>,
    key: Option<SessionKey>,
    cache: Weak<RefCell<CacheState>>,
    generation: Rc<()>,
    reusable: bool,
    reused: bool,
}

impl SimulationSessionLease {
    /// Current time only; all fallible session access goes through `apply`.
    #[must_use]
    pub fn time(&self) -> f64 {
        self.session.as_ref().expect("live session lease").time()
    }

    #[must_use]
    pub fn preparation_reused(&self) -> bool {
        self.reused
    }

    /// Dispatch through the existing protocol; a failed call prevents this
    /// instance from being returned as a reusable idle entry.
    pub fn apply(&mut self, command: SessionCommand) -> SessionEvent {
        let event = self
            .session
            .as_mut()
            .expect("live session lease")
            .apply(command);
        if matches!(event, SessionEvent::Error { .. }) {
            self.reusable = false;
        }
        event
    }
}

impl Drop for SimulationSessionLease {
    fn drop(&mut self) {
        if !self.reusable || std::thread::panicking() {
            return;
        }
        let Some(cache) = self.cache.upgrade() else {
            return;
        };
        let Ok(mut state) = cache.try_borrow_mut() else {
            return;
        };
        if !Rc::ptr_eq(&self.generation, &state.generation) {
            return;
        }
        if let (Some(key), Some(session)) = (self.key.take(), self.session.take()) {
            state.idle = Some(IdleSession { key, session });
        }
    }
}
