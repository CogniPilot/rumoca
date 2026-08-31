//! Closed validation and measurement sequence for one authenticated row.

use anyhow::Result;
#[cfg(test)]
use anyhow::ensure;
use serde::Serialize;
use sha2::{Digest, Sha256};

macro_rules! define_suite_catalog {
    (; $($variant:ident => $id:literal => $executor:ident),+ $(,)?) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub(in crate::verify_cmd::embedded_head_to_head) enum SuiteStep {
            $($variant),+
        }

        const REQUIRED_STEPS: &[OperationSpec] = &[
            $(OperationSpec {
                step: SuiteStep::$variant,
                id: $id,
                executor: stringify!($executor),
            }),+
        ];
    };
}

embedded_suite_operations!(define_suite_catalog);

#[derive(Clone, Copy)]
struct OperationSpec {
    step: SuiteStep,
    id: &'static str,
    executor: &'static str,
}

#[derive(Clone, Copy)]
pub(in crate::verify_cmd::embedded_head_to_head) struct BoundSuitePlan;

impl BoundSuitePlan {
    pub(super) const fn closed() -> Self {
        Self
    }

    pub(super) fn update_digest(self, digest: &mut Sha256) {
        update_digest(digest, REQUIRED_STEPS);
    }

    pub(super) const fn cursor(self) -> SuiteCursor {
        SuiteCursor { next: 0 }
    }
}

pub(in crate::verify_cmd::embedded_head_to_head) struct SuiteCursor {
    next: usize,
}

pub(in crate::verify_cmd::embedded_head_to_head) enum SuiteProgress {
    Pending(PendingSuiteStep),
    Complete(CompletedSuite),
}

pub(in crate::verify_cmd::embedded_head_to_head) struct PendingSuiteStep {
    cursor: SuiteCursor,
    operation: OperationSpec,
}

pub(in crate::verify_cmd::embedded_head_to_head) struct CompletedSuite {
    digest: String,
}

/// Proof that one catalog operation completed successfully at its exact ordinal.
#[derive(Clone, Serialize)]
pub(in crate::verify_cmd::embedded_head_to_head) struct ExecutedSuiteStep {
    ordinal: usize,
    id: &'static str,
    executor: &'static str,
}

impl SuiteCursor {
    pub(in crate::verify_cmd::embedded_head_to_head) fn next(self) -> SuiteProgress {
        match REQUIRED_STEPS.get(self.next) {
            Some(operation) => SuiteProgress::Pending(PendingSuiteStep {
                cursor: self,
                operation: *operation,
            }),
            None => SuiteProgress::Complete(CompletedSuite {
                digest: catalog_digest(REQUIRED_STEPS),
            }),
        }
    }
}

impl PendingSuiteStep {
    pub(super) fn execute<T>(
        mut self,
        operation: impl FnOnce(SuiteStep) -> Result<T>,
    ) -> Result<(SuiteCursor, ExecutedSuiteStep, T)> {
        let value = operation(self.operation.step)?;
        let executed = ExecutedSuiteStep {
            ordinal: self.cursor.next,
            id: self.operation.id,
            executor: self.operation.executor,
        };
        self.cursor.next += 1;
        Ok((self.cursor, executed, value))
    }
}

impl CompletedSuite {
    pub(in crate::verify_cmd::embedded_head_to_head) fn digest(&self) -> &str {
        &self.digest
    }
}

fn update_digest(digest: &mut Sha256, operations: &[OperationSpec]) {
    digest.update(b"ordered-validation-suite\0");
    for (index, operation) in operations.iter().enumerate() {
        digest.update(index.to_string().as_bytes());
        digest.update(b"\0");
        digest.update(operation.id.as_bytes());
        digest.update(b"\0");
        digest.update(operation.executor.as_bytes());
        digest.update(b"\0");
    }
}

fn catalog_digest(operations: &[OperationSpec]) -> String {
    let mut digest = Sha256::new();
    update_digest(&mut digest, operations);
    format!("{:x}", digest.finalize())
}

#[cfg(test)]
fn validate_catalog(operations: &[OperationSpec]) -> Result<()> {
    ensure!(
        operations.len() == REQUIRED_STEPS.len(),
        "suite omitted or duplicated a required operation"
    );
    for (index, (actual, expected)) in operations.iter().zip(REQUIRED_STEPS).enumerate() {
        ensure!(
            actual.step == expected.step
                && actual.id == expected.id
                && actual.executor == expected.executor,
            "suite operation {index} differs from the closed catalog"
        );
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{REQUIRED_STEPS, catalog_digest, validate_catalog};

    #[test]
    fn closed_catalog_rejects_omission_duplication_reorder_and_executor_swap() {
        assert!(validate_catalog(REQUIRED_STEPS).is_ok());
        let original = catalog_digest(REQUIRED_STEPS);

        let mut omitted = REQUIRED_STEPS.to_vec();
        omitted.remove(4);
        assert!(validate_catalog(&omitted).is_err());
        assert_ne!(catalog_digest(&omitted), original);

        let mut duplicated = REQUIRED_STEPS.to_vec();
        duplicated[5] = duplicated[4];
        assert!(validate_catalog(&duplicated).is_err());
        assert_ne!(catalog_digest(&duplicated), original);

        let mut reordered = REQUIRED_STEPS.to_vec();
        reordered.swap(3, 4);
        assert!(validate_catalog(&reordered).is_err());
        assert_ne!(catalog_digest(&reordered), original);

        let mut rebound = REQUIRED_STEPS.to_vec();
        rebound[2].executor = rebound[3].executor;
        assert!(validate_catalog(&rebound).is_err());
        assert_ne!(catalog_digest(&rebound), original);
    }
}
