//! Explicit resource bounds, without model or execution admission.

mod envelope;
pub use envelope::{decode_native_source_envelope, encode_native_source_envelope};

const PAGE_BYTES: u32 = 65_536;
const DEFAULT_BYTES: u32 = 64 * 1024 * 1024;

/// Unchecked requested bounds. Replay must call the policy constructor again.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NativeWasmCapacityRequest {
    pub private_arena_bytes: u32,
    pub public_scratch_bytes: u32,
}

/// Checked memory32 deployment bounds; never a model or engine capability.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CheckedNativeWasmCapacityPolicy {
    request: NativeWasmCapacityRequest,
}

impl CheckedNativeWasmCapacityPolicy {
    pub fn new(request: NativeWasmCapacityRequest) -> Result<Self, String> {
        for bytes in [request.private_arena_bytes, request.public_scratch_bytes] {
            if bytes == 0 || bytes % PAGE_BYTES != 0 {
                return Err("native capacity must be nonzero whole memory32 pages".into());
            }
        }
        Ok(Self { request })
    }

    pub fn request(self) -> NativeWasmCapacityRequest {
        self.request
    }

    pub fn private_arena_bytes(self) -> u32 {
        self.request.private_arena_bytes
    }

    pub fn public_scratch_bytes(self) -> u32 {
        self.request.public_scratch_bytes
    }

    pub(crate) fn require_private_bytes(self, required: usize) -> Result<(), String> {
        if required > self.private_arena_bytes() as usize {
            return Err(self.refusal(
                "private register arena",
                required,
                self.private_arena_bytes(),
            ));
        }
        Ok(())
    }

    pub(crate) fn require_public_bytes(self, required: u32) -> Result<u32, String> {
        if required > self.public_scratch_bytes() {
            return Err(self.refusal(
                "whole-program scratch",
                required as usize,
                self.public_scratch_bytes(),
            ));
        }
        Ok(required)
    }

    fn refusal(self, stage: &str, required: usize, limit: u32) -> String {
        let bound = if limit == DEFAULT_BYTES {
            "64 MiB"
        } else {
            "explicit capacity"
        };
        format!("native {stage} exceeds {bound}: required_bytes={required} capacity_bytes={limit}")
    }

    pub fn private_arena_pages(self) -> u64 {
        u64::from(self.private_arena_bytes() / PAGE_BYTES)
    }
}

impl Default for CheckedNativeWasmCapacityPolicy {
    fn default() -> Self {
        Self::new(NativeWasmCapacityRequest {
            private_arena_bytes: DEFAULT_BYTES,
            public_scratch_bytes: DEFAULT_BYTES,
        })
        .expect("the canonical default is a nonzero memory32 page bound")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn explicit_capacity_checks_each_bound_and_preserves_default() {
        let default = CheckedNativeWasmCapacityPolicy::default();
        assert_eq!(default.private_arena_bytes(), DEFAULT_BYTES);
        assert_eq!(default.public_scratch_bytes(), DEFAULT_BYTES);
        assert_eq!(default.private_arena_pages(), 1024);
        let maximum = u32::MAX - (u32::MAX % PAGE_BYTES);
        let request = NativeWasmCapacityRequest {
            private_arena_bytes: maximum,
            public_scratch_bytes: PAGE_BYTES,
        };
        let policy = CheckedNativeWasmCapacityPolicy::new(request).unwrap();
        assert_eq!(policy.request(), request);
        assert_eq!(policy.private_arena_pages(), 65_535);
        for bad in [0, 1, PAGE_BYTES - 1, u32::MAX] {
            for request in [
                NativeWasmCapacityRequest {
                    private_arena_bytes: bad,
                    ..request
                },
                NativeWasmCapacityRequest {
                    public_scratch_bytes: bad,
                    ..request
                },
            ] {
                assert!(CheckedNativeWasmCapacityPolicy::new(request).is_err());
            }
        }
    }
}
