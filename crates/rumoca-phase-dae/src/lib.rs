//! Flat-to-DAE construction for the Rumoca compiler.
//!
//! Production lowering returns only the immutable schema-v11 DAE assembled by
//! [`rumoca_ir_dae::Dae::construct`]. Unsupported semantic owners fail with a
//! typed, source-bearing error before construction; there is no mutable DAE
//! draft, validator pass, superseded representation, or fallback value.

pub mod balance;
mod construction;
mod errors;

use rumoca_core::SourceMap;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

pub use balance::{BalanceBreakdown, BalanceDetail};
pub use errors::{ToDaeError, ToDaeResult};

/// A completed canonical DAE and the source analysis that issued its evidence.
///
/// Only this phase constructs the result, after every DAE construction check
/// succeeds. Consumers move its matched parts without analyzing Flat again.
pub struct DaeConstructionResult {
    dae: dae::Dae,
    balance_detail: BalanceDetail,
    structural_selections: Vec<StructuralSelection>,
}

impl DaeConstructionResult {
    pub fn into_parts(self) -> (dae::Dae, BalanceDetail, Vec<StructuralSelection>) {
        (self.dae, self.balance_detail, self.structural_selections)
    }
}

/// Construct the canonical DAE while transferring the source-map snapshot that
/// resolves every retained provenance range.
pub fn to_dae(flat: &flat::Model, source_map: SourceMap) -> Result<dae::Dae, ToDaeError> {
    construction::construct(flat, source_map)
}

/// Construct the canonical DAE and its evidence from one prepared Flat analysis.
pub fn to_dae_with_evidence(
    flat: &flat::Model,
    source_map: SourceMap,
) -> Result<DaeConstructionResult, ToDaeError> {
    construction::construct_with_evidence(flat, source_map)
}

pub use construction::StructuralSelection;
pub mod function_normalization;

/// The balance evidence together with every owner whose folded parameter
/// guard fixes parameters at translation (SPEC_0040 DAE-C22), from one
/// analysis of the Flat model.
pub fn construction_evidence(
    flat: &flat::Model,
) -> Result<(BalanceDetail, Vec<StructuralSelection>), ToDaeError> {
    construction::construction_evidence(flat)
}
