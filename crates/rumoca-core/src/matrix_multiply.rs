//! Target-neutral matrix-product arithmetic vocabulary.

use serde::{Deserialize, Serialize};

/// Closed arithmetic relation for one Real matrix-product occurrence.
///
/// This type names the relation defined by SPEC_0049. It does not issue a
/// plan, select a default, or validate an occurrence; checked IR constructors
/// retain those authorities.
///
/// ```compile_fail
/// fn require_default<T: Default>() {}
/// require_default::<rumoca_core::RealMatrixMultiplySemantics>();
/// ```
///
/// ```compile_fail
/// let _ = rumoca_core::RealMatrixMultiplySemantics::default();
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RealMatrixMultiplySemantics {
    SeparateMulAddAscendingFirstProduct,
    SeparateMulAddAscendingPositiveZero,
}
