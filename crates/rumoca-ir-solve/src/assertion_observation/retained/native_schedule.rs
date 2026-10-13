//! Source-bound preparation and resolved inputs for the existing complete native schedule.

use super::RetainedAssertionRoot;
use crate::{NativeRefreshAssignmentSchedule, NativeScheduleRefusal};

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum RetainedNativeScheduleError {
    #[error("canonical start-time resolution must precede native execution binding")]
    UnresolvedStartTime,
    #[error("the retained root has no issued native schedule")]
    NotIssued,
    #[error("{0}")]
    Refused(#[source] NativeScheduleRefusal),
    #[error("native preparation belongs to another root, metadata or schedule owner")]
    ForeignPreparation,
}

/// Preparation-only source inputs. This grants no native execution authority.
pub struct RetainedNativePreparation {
    root: RetainedAssertionRoot,
}

/// Exact resolved inputs, without a detached schedule or layout constructor.
pub struct RetainedNativeSchedule {
    root: RetainedAssertionRoot,
}

impl RetainedAssertionRoot {
    pub fn native_preparation(
        &self,
    ) -> Result<RetainedNativePreparation, RetainedNativeScheduleError> {
        self.issued_native_schedule()?;
        Ok(RetainedNativePreparation { root: self.clone() })
    }

    pub fn native_schedule(&self) -> Result<RetainedNativeSchedule, RetainedNativeScheduleError> {
        if self.resolved_start_time.is_none() {
            return Err(RetainedNativeScheduleError::UnresolvedStartTime);
        }
        self.issued_native_schedule()?;
        Ok(RetainedNativeSchedule { root: self.clone() })
    }

    fn issued_native_schedule(
        &self,
    ) -> Result<&NativeRefreshAssignmentSchedule, RetainedNativeScheduleError> {
        let owners = &self.model.problem.continuous.refresh_owners;
        let schedule = owners.native_assignment_schedule().ok_or_else(|| {
            owners
                .native_assignment_refusal()
                .map_or(RetainedNativeScheduleError::NotIssued, |refusal| {
                    RetainedNativeScheduleError::Refused(refusal.clone())
                })
        })?;
        if !schedule.matches_call_table(&self.model.pure_calls) {
            return Err(RetainedNativeScheduleError::ForeignPreparation);
        }
        Ok(schedule)
    }
}

impl RetainedNativePreparation {
    pub fn root(&self) -> &RetainedAssertionRoot {
        &self.root
    }

    pub fn schedule(&self) -> &NativeRefreshAssignmentSchedule {
        self.root
            .issued_native_schedule()
            .expect("preparation selected this immutable root's issued schedule")
    }

    /// Activate only against the actual instance's resolved issuing owner.
    pub fn bind_resolved(
        &self,
        root: RetainedAssertionRoot,
    ) -> Result<RetainedNativeSchedule, RetainedNativeScheduleError> {
        let inputs = root.native_schedule()?;
        if !self.root.shares_root_owner(inputs.root())
            || !std::ptr::eq(self.root.metadata(), inputs.root().metadata())
            || !std::ptr::eq(self.schedule(), inputs.schedule())
        {
            return Err(RetainedNativeScheduleError::ForeignPreparation);
        }
        Ok(inputs)
    }
}

impl RetainedNativeSchedule {
    pub fn root(&self) -> &RetainedAssertionRoot {
        &self.root
    }

    pub fn schedule(&self) -> &NativeRefreshAssignmentSchedule {
        self.root
            .issued_native_schedule()
            .expect("resolved inputs selected this immutable root's issued schedule")
    }
}
