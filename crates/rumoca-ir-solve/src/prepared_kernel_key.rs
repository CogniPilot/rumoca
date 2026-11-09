//! Identity of checked executable source retained by a precompiled product.
//! This selects no storage, lowering, omission, caching or execution policy.
use crate::{ComputeBlock, ScalarProgramBlock, SolvePureCallTable, VarLayout};

/// Issued only from immutable checked source. A digest alone grants no entry authority.
pub struct PreparedKernelSource {
    context: String,
}

impl PreparedKernelSource {
    pub fn new(layout: &VarLayout, calls: &SolvePureCallTable) -> Result<Self, String> {
        let bytes =
            bincode::serialize(&(1u32, layout, calls)).map_err(|error| error.to_string())?;
        Ok(Self {
            context: blake3::hash(&bytes).to_hex().to_string(),
        })
    }

    pub fn private_program(
        &self,
        source: &ScalarProgramBlock,
        seed: Option<usize>,
    ) -> Result<String, String> {
        self.key(&("private-v1", source, seed))
    }

    pub fn exact_assignments(&self, source: &ComputeBlock) -> Result<String, String> {
        self.key(&("exact-v1", source))
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
        let bytes =
            bincode::serialize(&(&self.context, source)).map_err(|error| error.to_string())?;
        Ok(blake3::hash(&bytes).to_hex().to_string())
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
}
