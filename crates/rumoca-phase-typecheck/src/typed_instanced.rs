//! The Typecheck-minted proof artifact for the instanced pipeline.
//!
//! SPEC_0036 `ResolvedTree + raw InstanceOverlay -> TypedInstancedTree` mint
//! and SPEC_0029 §4 forward proof edges: [`TypedInstancedTree`] is the sole
//! successful publication of post-instantiation type checking. Its fields are private,
//! it is not `Clone`, not `Default`, and not serializable in either
//! direction, and it exposes no mutable projection, so the unique phase
//! capability it carries can only move — exactly once — into the Flatten
//! mint. Downstream phases read its payload only through the immutable query
//! views below.
//!
//! [`TypedOverlayProjection`] is a `Clone`-able read-only sharing of the same
//! immutable payload for session caching and diagnostics rendering. It
//! carries no phase capability: nothing accepts it directly in a proof input
//! position and no unchecked conversion adopts it as a proof. A caller may
//! clone its raw overlay view and rerun the sole Typecheck mint, including the
//! disclosed CE-3 cross-root pairing until the Instantiate cutover.
//!
//! ```compile_fail
//! // The proof cannot be forged by literal construction: fields are private.
//! let forged = rumoca_phase_typecheck::TypedInstancedTree {
//!     overlay: std::sync::Arc::new(rumoca_ir_ast::InstanceOverlay::new()),
//!     model_name: String::new(),
//! };
//! ```
//!
//! ```compile_fail
//! // The proof cannot be duplicated: it is not `Clone`.
//! fn duplicate(proof: rumoca_phase_typecheck::TypedInstancedTree) {
//!     let _second: rumoca_phase_typecheck::TypedInstancedTree = Clone::clone(&proof);
//! }
//! ```

use crate::InstanceOverlay;
use rumoca_phase_resolve::ResolvedTreeProjection;
use std::sync::Arc;

/// Opaque proof that one instantiated model completed post-instantiation
/// type checking with zero errors.
///
/// Minted only by [`crate::typecheck_instanced_tree`]. Consuming it by value
/// transfers the unique phase capability to the Flatten mint.
#[derive(Debug)]
pub struct TypedInstancedTree {
    resolved: ResolvedTreeProjection,
    overlay: Arc<InstanceOverlay>,
    model_name: String,
}

impl TypedInstancedTree {
    /// Sole in-crate mint, called only after the checker reported zero
    /// errors on the finalized overlay.
    pub(crate) fn mint(
        resolved: ResolvedTreeProjection,
        overlay: InstanceOverlay,
        model_name: String,
    ) -> Self {
        Self {
            resolved,
            overlay: Arc::new(overlay),
            model_name,
        }
    }

    /// Immutable view of the exact Resolve root retained at this proof's mint.
    ///
    /// Flatten receives no caller-selected tree; this sealed view is its only
    /// resolved input.
    pub fn resolved_tree(&self) -> &rumoca_ir_ast::ClassTree {
        self.resolved.inner()
    }

    /// Immutable view of the finalized, typecheck-annotated instance overlay.
    pub fn overlay(&self) -> &InstanceOverlay {
        &self.overlay
    }

    /// The qualified model name this proof was minted for.
    pub fn model_name(&self) -> &str {
        &self.model_name
    }

    /// A `Clone`-able read-only sharing of the immutable payload.
    ///
    /// The projection carries no phase capability: it cannot enter a proof
    /// input position and no unchecked conversion adopts it as a proof. A
    /// fresh proof requires cloning the raw overlay view and rerunning the
    /// sole Typecheck mint. The original proof remains affine and unaffected
    /// by how many projections exist.
    pub fn shared_projection(&self) -> TypedOverlayProjection {
        TypedOverlayProjection {
            overlay: Arc::clone(&self.overlay),
            model_name: self.model_name.clone(),
        }
    }
}

/// Read-only, `Clone`-able sharing of one proof's immutable payload.
///
/// Used by session caches and diagnostics rendering. It is deliberately not
/// accepted by any phase mint.
#[derive(Debug, Clone)]
pub struct TypedOverlayProjection {
    overlay: Arc<InstanceOverlay>,
    model_name: String,
}

impl TypedOverlayProjection {
    /// Immutable view of the shared finalized overlay.
    pub fn overlay(&self) -> &InstanceOverlay {
        &self.overlay
    }

    /// The qualified model name the originating proof was minted for.
    pub fn model_name(&self) -> &str {
        &self.model_name
    }
}
