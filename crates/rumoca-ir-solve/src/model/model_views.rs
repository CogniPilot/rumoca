use super::{SolveModel, SolveVariableMeta};

impl SolveModel {
    /// Explicit complete report boundary, independent of numerical construction.
    pub fn visible_names(&self) -> Vec<String> {
        (0..self.visible_projection.scalar_count())
            .map(|point| {
                self.visible_projection()
                    .name(point)
                    .expect("checked visible declaration or authored expression has a name")
            })
            .collect()
    }

    /// Explicit report of the metadata actually authored on visible members.
    pub fn visible_metadata(&self) -> Vec<SolveVariableMeta> {
        (0..self.visible_projection.scalar_count())
            .filter_map(|point| self.visible_projection().metadata(point))
            .collect()
    }

    /// This compile-time model with every periodic schedule resolved at the
    /// FMI instance's simulation start instant. A model with no periodic
    /// schedule has nothing to resolve and is borrowed, not copied.
    pub fn resolved_periodic_schedules_at(
        &self,
        start_time: f64,
    ) -> Result<std::borrow::Cow<'_, Self>, rumoca_core::ClockLatticeErrorKind> {
        if self.problem.clocks.periodic_event_schedules.is_empty()
            && self
                .problem
                .solve_layout
                .pre_param_bindings
                .iter()
                .all(|binding| binding.clock_schedule.is_none())
        {
            return Ok(std::borrow::Cow::Borrowed(self));
        }
        let mut resolved = self.clone();
        for schedule in &mut resolved.problem.clocks.periodic_event_schedules {
            *schedule = schedule.resolved_at(start_time)?;
        }
        for binding in &mut resolved.problem.solve_layout.pre_param_bindings {
            if let Some(schedule) = &mut binding.clock_schedule {
                *schedule = schedule.resolved_at(start_time)?;
            }
        }
        Ok(std::borrow::Cow::Owned(resolved))
    }

    pub fn state_scalar_count(&self) -> usize {
        self.problem.solve_layout.state_scalar_count()
    }

    pub fn solver_scalar_count(&self) -> usize {
        self.problem.solve_layout.solver_scalar_count()
    }

    /// Return a finite positive scale for one solver variable.
    ///
    /// The declared nominal is the baseline. A larger start magnitude expands
    /// the scale so solver tolerances remain meaningful for large initial
    /// values and runtime start overrides.
    pub fn solver_variable_scale(&self, index: usize) -> f64 {
        variable_scale(
            self.solver_nominals.get(index).copied(),
            self.initial_y.value(index),
        )
    }

    /// [`Self::solver_variable_scale`] of every solver variable, in order:
    /// the dense table a runtime forms once instead of searching the start
    /// runs per lookup.
    pub fn solver_variable_scales(&self) -> Box<[f64]> {
        let starts = self.initial_y.as_slice();
        (0..self.solver_scalar_count())
            .map(|index| {
                variable_scale(
                    self.solver_nominals.get(index).copied(),
                    starts.get(index).copied(),
                )
            })
            .collect()
    }
}

/// The declared nominal, expanded by a larger finite start magnitude.
fn variable_scale(nominal: Option<f64>, start: Option<f64>) -> f64 {
    let nominal = nominal
        .filter(|value| value.is_finite() && *value > 0.0)
        .unwrap_or(1.0);
    let start_magnitude = start
        .filter(|value| value.is_finite())
        .map_or(0.0, f64::abs);
    nominal.max(start_magnitude)
}
