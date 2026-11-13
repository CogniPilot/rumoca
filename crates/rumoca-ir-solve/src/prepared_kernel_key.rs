//! Identity of checked executable source retained by a precompiled product.
//! This selects no storage, lowering, omission, caching or execution policy.
use crate::{
    ComputeBlock, ContinuousRefreshOwners, ExactRefreshAssignmentSchedule, ScalarProgramBlock,
    SolvePureCallTable, VarLayout,
};
use std::io::Write;

fn serialized_digest(source: &impl serde::Serialize) -> Result<String, String> {
    let mut writer = std::io::BufWriter::new(blake3::Hasher::new());
    bincode::serialize_into(&mut writer, source).map_err(|error| error.to_string())?;
    writer.flush().map_err(|error| error.to_string())?;
    Ok(writer.get_ref().finalize().to_hex().to_string())
}

/// Issued only from immutable checked source. A digest alone grants no entry authority.
pub struct PreparedKernelSource {
    context: String,
}

impl PreparedKernelSource {
    pub fn new(layout: &VarLayout, calls: &SolvePureCallTable) -> Result<Self, String> {
        Ok(Self {
            context: serialized_digest(&(1u32, layout, calls))?,
        })
    }

    pub fn private_program(
        &self,
        source: &ScalarProgramBlock,
        seed: Option<usize>,
    ) -> Result<String, String> {
        self.key(&("private-v1", source, seed))
    }

    /// Bare source fingerprint; this does not identify an executable schedule.
    pub fn exact_assignments(&self, source: &ComputeBlock) -> Result<String, String> {
        self.key(&("exact-v1", source))
    }

    /// The complete original source and the specific constructor-issued schedule.
    /// Unlike the bare source fingerprint, this names owners, order and chunks.
    pub fn exact_assignment_schedule(
        &self,
        source: &ComputeBlock,
        owners: &ContinuousRefreshOwners,
        schedule: &ExactRefreshAssignmentSchedule,
    ) -> Result<String, String> {
        self.key(&("exact-schedule-v2", source, owners, schedule.key_identity()))
    }

    /// Issued ordered assignment rows: their programs, slot targets, function
    /// chunks, slot count and seed slots.
    pub fn issued_assignment_rows(
        &self,
        rows: &crate::IssuedAssignmentRows,
    ) -> Result<String, String> {
        self.key(&("issued-rows-v1", rows))
    }

    /// Identity of the complete original FMI operation, never a wire plan.
    /// The kinematic rows select its kinematic entry; the problem fixes the
    /// values they select.
    pub fn fmi_derivative(
        &self,
        operation: &crate::fmi::FmiDerivativeCausalOperation<'_>,
    ) -> Result<String, String> {
        let kinematic = operation
            .kinematic()
            .map(|kinematic| kinematic.rows().iter().collect::<Vec<_>>());
        self.key(&(
            "fmi-derivative-v2",
            operation.source().problem(),
            operation.source().vector_extent(),
            operation.parameter_keys(),
            kinematic,
        ))
    }

    pub fn retained_event(&self, source: &crate::RetainedEventProgram) -> Result<String, String> {
        self.key(&(
            "retained-event-v1",
            source.program(),
            source.span(),
            &source.source().model().problem.events,
        ))
    }

    fn key(&self, source: &impl serde::Serialize) -> Result<String, String> {
        serialized_digest(&(&self.context, source))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::LinearOp;
    use rumoca_core::{SourceId, Span};

    fn block(value: f64, origin: &str) -> ScalarProgramBlock {
        ScalarProgramBlock::with_program_spans(
            vec![vec![
                LinearOp::Const { dst: 0, value },
                LinearOp::StoreOutput { src: 0 },
            ]],
            vec![Span::from_offsets(SourceId::from_source_name(origin), 1, 2)],
        )
        .unwrap()
    }

    fn context() -> PreparedKernelSource {
        PreparedKernelSource::new(&VarLayout::default(), &SolvePureCallTable::default()).unwrap()
    }

    #[test]
    fn buffered_digest_preserves_large_nested_bincode_bytes() {
        let values: Vec<_> = (0..50_000u64)
            .map(|i| (i, f64::from_bits(i << 17), i % 2 == 0))
            .collect();
        let source = ("nested", vec![(-0.0f64, values), (0.0, Vec::new())]);
        let legacy = blake3::hash(&bincode::serialize(&source).unwrap());
        assert_eq!(
            serialized_digest(&source).unwrap(),
            legacy.to_hex().as_str()
        );
    }

    #[test]
    fn buffered_context_and_program_keys_preserve_legacy_identity() {
        let layout = VarLayout::default();
        let calls = SolvePureCallTable::default();
        let context = PreparedKernelSource::new(&layout, &calls).unwrap();
        let legacy_context = blake3::hash(&bincode::serialize(&(1u32, &layout, &calls)).unwrap())
            .to_hex()
            .to_string();
        assert_eq!(context.context, legacy_context);
        let source = block(-0.0, "Identity.mo");
        for seed in [None, Some(1), Some(3)] {
            let legacy = blake3::hash(
                &bincode::serialize(&(&legacy_context, ("private-v1", &source, seed))).unwrap(),
            );
            assert_eq!(
                context.private_program(&source, seed).unwrap(),
                legacy.to_hex().as_str()
            );
        }
    }

    #[test]
    fn checked_source_key_survives_fresh_owner_and_canonical_wire_replay() {
        let original = block(2.0, "Kernel.mo");
        let replay: ScalarProgramBlock =
            serde_json::from_slice(&serde_json::to_vec(&original).unwrap()).unwrap();
        assert!(!original.shares_program_owner(&replay));
        assert_eq!(
            context().private_program(&original, None).unwrap(),
            context().private_program(&replay, None).unwrap()
        );
    }

    #[test]
    fn checked_source_key_distinguishes_signed_zero_and_source_provenance() {
        let key = context();
        let positive = key.private_program(&block(0.0, "Kernel.mo"), None).unwrap();
        assert_ne!(
            positive,
            key.private_program(&block(-0.0, "Kernel.mo"), None)
                .unwrap()
        );
        assert_ne!(
            positive,
            key.private_program(&block(0.0, "Other.mo"), None).unwrap()
        );
    }

    #[test]
    fn checked_source_key_distinguishes_value_and_each_directional_extent() {
        let key = context();
        let source = block(2.0, "Kernel.mo");
        let value = key.private_program(&source, None).unwrap();
        let first = key.private_program(&source, Some(1)).unwrap();
        let second = key.private_program(&source, Some(2)).unwrap();
        assert_ne!(value, first);
        assert_ne!(first, second);
    }

    #[test]
    fn exact_assignment_key_preserves_replay_and_invalidates_changed_source() {
        let source = ComputeBlock {
            nodes: vec![crate::ComputeNode::ScalarPrograms(block(2.0, "Kernel.mo"))],
        };
        let replay: ComputeBlock =
            serde_json::from_slice(&serde_json::to_vec(&source).unwrap()).unwrap();
        let original = context().exact_assignments(&source).unwrap();
        assert_eq!(original, context().exact_assignments(&replay).unwrap());

        for changed in [block(3.0, "Kernel.mo"), block(2.0, "Other.mo")] {
            let changed = ComputeBlock {
                nodes: vec![crate::ComputeNode::ScalarPrograms(changed)],
            };
            assert_ne!(original, context().exact_assignments(&changed).unwrap());
        }
    }

    #[test]
    fn retained_event_key_preserves_replay_and_binds_the_event_inventory() {
        let mut model = crate::SolveModel::default();
        model.problem.events.root_conditions = block(1.0, "Event.mo");
        let mut replay = model.clone();
        replay.problem.events.root_conditions = serde_json::from_slice(
            &serde_json::to_vec(&model.problem.events.root_conditions).unwrap(),
        )
        .unwrap();
        let source = crate::SolveExecutionSource::new(model.clone());
        let event = source
            .event_program(crate::RetainedAssertionSource::RootConditions, 0)
            .unwrap();
        let replayed_source = crate::SolveExecutionSource::new(replay);
        let replayed = replayed_source
            .event_program(crate::RetainedAssertionSource::RootConditions, 0)
            .unwrap();
        assert!(!std::ptr::eq(event.program(), replayed.program()));
        let original = context().retained_event(&event).unwrap();
        assert_eq!(original, context().retained_event(&replayed).unwrap());

        model.problem.events.action_conditions = block(0.0, "Action.mo");
        let changed_source = crate::SolveExecutionSource::new(model);
        let changed = changed_source
            .event_program(crate::RetainedAssertionSource::RootConditions, 0)
            .unwrap();
        assert_eq!(event.program(), changed.program());
        assert_ne!(original, context().retained_event(&changed).unwrap());
    }
}
