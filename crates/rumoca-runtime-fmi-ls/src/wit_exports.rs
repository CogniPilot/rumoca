//! Pinned generated canonical WIT ABI boundary (SPEC_0044 ME-LSW-006b).
#![allow(unsafe_code)]

use crate::{adapter::RumocaFmu, bindings};

bindings::export!(RumocaFmu with_types_in crate::bindings);

#[cfg(feature = "preinit")]
impl bindings::Guest for RumocaFmu {
    fn rumoca_preinit() {
        crate::bootstrap::preinitialize();
    }
}

#[cfg(feature = "certify-linked")]
impl bindings::Guest for RumocaFmu {
    fn rumoca_certify_linked() {
        crate::bootstrap::certify_linked();
    }
}
