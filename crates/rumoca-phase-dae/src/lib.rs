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

/// Strictness controls applied before a canonical DAE can be returned.
#[derive(Debug, Clone, Copy)]
pub struct ToDaeOptions {
    pub error_on_unbalanced: bool,
}

impl Default for ToDaeOptions {
    fn default() -> Self {
        Self {
            error_on_unbalanced: true,
        }
    }
}

/// Construct the canonical DAE while transferring the source-map snapshot that
/// resolves every retained provenance range.
pub fn to_dae(flat: &flat::Model, source_map: SourceMap) -> Result<dae::Dae, ToDaeError> {
    to_dae_with_options(flat, source_map, ToDaeOptions::default())
}

/// Construct the canonical DAE with explicit phase strictness.
pub fn to_dae_with_options(
    flat: &flat::Model,
    source_map: SourceMap,
    options: ToDaeOptions,
) -> Result<dae::Dae, ToDaeError> {
    construction::construct(flat, source_map, options)
}

/// Compute the exact source-model balance evidence while Flat ownership is
/// still available. The canonical DAE intentionally does not duplicate it.
pub fn balance_detail(flat: &flat::Model) -> Result<BalanceDetail, ToDaeError> {
    construction::balance_detail(flat)
}
