//! Unchecked wire transport; only the existing paired product binds these bytes.
use super::{CheckedNativeWasmCapacityPolicy, NativeWasmCapacityRequest};

const MAGIC: &[u8; 8] = b"RUMONAT2";
const HEADER: usize = 24;
/// Compression level of the capsule. The canonical wire repeats its field
/// names for every operation, so DEFLATE removes more than 95 percent of it.
const LEVEL: u8 = 9;

/// Encode transport inputs only; this grants no code, model or execution authority.
/// The capsule is stored DEFLATE-compressed; its exact length is recorded so the
/// receiver inflates into one allocation of the checked size.
pub fn encode_native_source_envelope(
    capsule: &[u8],
    policy: CheckedNativeWasmCapacityPolicy,
) -> Result<Vec<u8>, String> {
    let length = u32::try_from(capsule.len()).map_err(|_| "native capsule exceeds memory32")?;
    if length == 0 {
        return Err("native capsule is empty".into());
    }
    let packed = miniz_oxide::deflate::compress_to_vec(capsule, LEVEL);
    let packed_length =
        u32::try_from(packed.len()).map_err(|_| "native packed capsule exceeds memory32")?;
    let mut bytes = Vec::new();
    bytes
        .try_reserve_exact(HEADER + packed.len())
        .map_err(|_| "native envelope allocation refused")?;
    bytes.extend_from_slice(MAGIC);
    bytes.extend_from_slice(&policy.private_arena_bytes().to_le_bytes());
    bytes.extend_from_slice(&policy.public_scratch_bytes().to_le_bytes());
    bytes.extend_from_slice(&length.to_le_bytes());
    bytes.extend_from_slice(&packed_length.to_le_bytes());
    bytes.extend_from_slice(&packed);
    Ok(bytes)
}

/// Decode unchecked source bytes and reissue only the resource-bound policy.
/// The canonical component reader and sealed entry owner still admit execution.
pub fn decode_native_source_envelope(
    bytes: &[u8],
) -> Result<(Vec<u8>, CheckedNativeWasmCapacityPolicy), String> {
    if bytes.len() < HEADER || &bytes[..8] != MAGIC {
        return Err("native source envelope version is unsupported".into());
    }
    let read =
        |start| u32::from_le_bytes(bytes[start..start + 4].try_into().expect("checked header"));
    let (length, packed_length) = (read(16) as usize, read(20) as usize);
    if length == 0 || HEADER.checked_add(packed_length) != Some(bytes.len()) {
        return Err("native source envelope length differs from its capsule".into());
    }
    let policy = CheckedNativeWasmCapacityPolicy::new(NativeWasmCapacityRequest {
        private_arena_bytes: read(8),
        public_scratch_bytes: read(12),
    })?;
    let capsule = miniz_oxide::inflate::decompress_to_vec_with_limit(&bytes[HEADER..], length)
        .map_err(|error| format!("native source capsule is not valid DEFLATE: {error:?}"))?;
    if capsule.len() != length {
        return Err("native source envelope length differs from its capsule".into());
    }
    Ok((capsule, policy))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn policy() -> CheckedNativeWasmCapacityPolicy {
        CheckedNativeWasmCapacityPolicy::new(NativeWasmCapacityRequest {
            private_arena_bytes: 128 * 65_536,
            public_scratch_bytes: 256 * 65_536,
        })
        .unwrap()
    }

    #[test]
    fn source_envelope_reissues_bounds_and_refuses_version_length_and_malformed_request() {
        let policy = policy();
        let bytes = encode_native_source_envelope(b"canonical-reader-input", policy).unwrap();
        let (capsule, replayed) = decode_native_source_envelope(&bytes).unwrap();
        assert_eq!(capsule, b"canonical-reader-input");
        assert_eq!(replayed, policy);
        for offset in [0, 8, 12, 16, 20] {
            let mut changed = bytes.clone();
            changed[offset] ^= 1;
            assert!(decode_native_source_envelope(&changed).is_err());
        }
        assert!(decode_native_source_envelope(&bytes[..bytes.len() - 1]).is_err());
        let mut trailing = bytes;
        trailing.push(0);
        assert!(decode_native_source_envelope(&trailing).is_err());
    }

    #[test]
    fn source_envelope_stores_a_repetitive_capsule_compressed_and_replays_it_exactly() {
        let capsule =
            br#"{"operation":"binary","destination":1,"provenance":{"start":0}}"#.repeat(50_000);
        let bytes = encode_native_source_envelope(&capsule, policy()).unwrap();
        assert!(bytes.len() * 20 < capsule.len(), "{}", bytes.len());
        let (replayed, _) = decode_native_source_envelope(&bytes).unwrap();
        assert_eq!(replayed, capsule);
    }

    #[test]
    fn source_envelope_refuses_a_payload_longer_than_its_recorded_length() {
        let capsule = vec![7u8; 4096];
        let mut bytes = encode_native_source_envelope(&capsule, policy()).unwrap();
        bytes[16..20].copy_from_slice(&4095u32.to_le_bytes());
        assert!(decode_native_source_envelope(&bytes).is_err());
        bytes[16..20].copy_from_slice(&4097u32.to_le_bytes());
        assert!(decode_native_source_envelope(&bytes).is_err());
    }
}
