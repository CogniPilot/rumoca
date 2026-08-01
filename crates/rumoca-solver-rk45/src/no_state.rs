//! Host wrapper for a zero-continuous-state ME component.
//!
//! A model with no continuous states has no integrator, so this crate owns
//! nothing but the session surface and the host-side input overlay; the
//! component itself ([`MeNoStateSession`]) runs the FMI 3 ME event loop.

use indexmap::IndexMap;
use rumoca_solver::{
    SimOptions,
    fmi_me::{MeModelSource, MeNoStateSession},
};

use crate::{SessionState, SimError};

pub(crate) struct NoStateSession {
    component: MeNoStateSession,
    input_values: IndexMap<String, f64>,
}

impl NoStateSession {
    pub(crate) fn new(source: MeModelSource<'_>, opts: SimOptions) -> Result<Self, SimError> {
        Ok(Self {
            component: MeNoStateSession::instantiate(source, opts)?,
            input_values: IndexMap::new(),
        })
    }

    pub(crate) fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        if !self.component.set_input(name, value)? {
            return Err(SimError::SolveIr(format!("unknown input '{name}'")));
        }
        self.input_values.insert(name.to_string(), value);
        Ok(())
    }

    pub(crate) fn advance_to(&mut self, target_time: f64) -> Result<(), SimError> {
        self.component.advance_to(target_time).map_err(Into::into)
    }

    pub(crate) fn ensure_end_time(&mut self, target_time: f64) {
        self.component.extend_stop_time(target_time);
    }

    pub(crate) fn step(&mut self, dt: f64) -> Result<(), SimError> {
        if dt <= 0.0 {
            return Ok(());
        }
        self.advance_to(self.component.time() + dt)
    }

    pub(crate) fn reset(&mut self, t_start: f64) -> Result<(), SimError> {
        self.input_values.clear();
        self.component.reset(t_start).map_err(Into::into)
    }

    pub(crate) fn time(&self) -> f64 {
        self.component.time()
    }

    pub(crate) fn get(&self, name: &str) -> Result<Option<f64>, SimError> {
        if let Some(value) = self.input_values.get(name).copied() {
            return Ok(Some(value));
        }
        let values = self.session_visible_values()?;
        Ok(values.get(name).copied())
    }

    pub(crate) fn state(&self) -> Result<SessionState, SimError> {
        Ok(SessionState {
            time: self.time(),
            values: self.session_visible_values()?,
        })
    }

    pub(crate) fn values_for(&self, names: &[String]) -> Result<IndexMap<String, f64>, SimError> {
        let visible_values = self.session_visible_values()?;
        let mut values = IndexMap::with_capacity(names.len());
        for name in names {
            if let Some(value) = visible_values.get(name).copied() {
                values.insert(name.clone(), value);
            }
        }
        Ok(values)
    }

    pub(crate) fn input_names(&self) -> &[String] {
        self.component.input_names()
    }

    pub(crate) fn variable_names(&self) -> &[String] {
        self.component.output_names()
    }

    fn session_visible_values(&self) -> Result<IndexMap<String, f64>, SimError> {
        let mut values = self.component.output_values()?;
        values.extend(
            self.input_values
                .iter()
                .map(|(name, value)| (name.clone(), *value)),
        );
        Ok(values)
    }
}
