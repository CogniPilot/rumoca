//! Flat-to-DAE construction for the Rumoca compiler.
//!
//! Production lowering returns one checked product containing the immutable
//! schema-v11 DAE assembled by [`rumoca_ir_dae::Dae::construct`] and the exact
//! balance evidence from the same Flat analysis. Unsupported semantic owners
//! fail with a typed, source-bearing error before construction; there is no
//! mutable DAE draft, validator pass, superseded representation, or fallback
//! value.

pub mod balance;
mod construction;
mod errors;

use rumoca_core::SourceMap;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

pub use balance::{BalanceBreakdown, BalanceDetail};
pub use errors::{ToDaeError, ToDaeResult};

/// One successful Flat-to-DAE construction and its correlated source balance.
///
/// Private fields prevent callers from forging this checked correlation.
/// Construction performs the complete Flat analysis once and publishes
/// neither member on failure.
#[must_use = "the checked DAE and its correlated balance evidence must be consumed"]
#[derive(Debug)]
pub struct DaeConstructionProduct {
    dae: dae::Dae,
    balance_detail: BalanceDetail,
}

impl DaeConstructionProduct {
    pub(crate) fn new(dae: dae::Dae, balance_detail: BalanceDetail) -> Self {
        Self {
            dae,
            balance_detail,
        }
    }

    /// Borrow the checked canonical DAE.
    pub fn dae(&self) -> &dae::Dae {
        &self.dae
    }

    /// Borrow the exact source-model balance from the construction analysis.
    pub fn balance_detail(&self) -> &BalanceDetail {
        &self.balance_detail
    }

    /// Consume the product into its correlated members.
    pub fn into_parts(self) -> (dae::Dae, BalanceDetail) {
        (self.dae, self.balance_detail)
    }
}

/// Construct the canonical DAE and its exact source-model balance in one Flat
/// analysis while transferring the source-map snapshot that resolves every
/// retained provenance range.
pub fn construct(
    flat: &flat::Model,
    source_map: SourceMap,
) -> Result<DaeConstructionProduct, ToDaeError> {
    construction::construct(flat, source_map)
}
