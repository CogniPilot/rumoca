//! Versioned binary framing. Payload is borrowed and hashed before checked decode.
use sha2::{Digest, Sha256};
const MAGIC: &[u8; 8] = b"RMCORE02";
pub(super) fn encode(key: &[u8], payload: &[u8]) -> Result<Vec<u8>, String> {
    let length = u32::try_from(key.len()).map_err(|_| "runtime key exceeds binary extent")?;
    let mut bytes = Vec::new();
    bytes.extend_from_slice(MAGIC);
    bytes.extend_from_slice(&length.to_le_bytes());
    bytes.extend_from_slice(key);
    bytes.extend_from_slice(&Sha256::digest(payload));
    bytes.extend_from_slice(payload);
    Ok(bytes)
}
pub(super) fn decode(bytes: &[u8]) -> Result<(&[u8], &[u8]), String> {
    if bytes.get(..8) != Some(MAGIC.as_slice()) {
        return Err("unsupported binary runtime container".into());
    }
    let length = u32::from_le_bytes(
        bytes
            .get(8..12)
            .ok_or("truncated runtime header")?
            .try_into()
            .map_err(|_| "invalid runtime header")?,
    ) as usize;
    let end = 12usize
        .checked_add(length)
        .ok_or("runtime key extent overflow")?;
    let payload_start = end.checked_add(32).ok_or("runtime hash extent overflow")?;
    let key = bytes.get(12..end).ok_or("truncated runtime key")?;
    let hash = bytes
        .get(end..payload_start)
        .ok_or("truncated runtime hash")?;
    let payload = bytes
        .get(payload_start..)
        .ok_or("truncated runtime payload")?;
    if Sha256::digest(payload).as_slice() != hash {
        return Err("precompiled runtime payload hash mismatch".into());
    }
    Ok((key, payload))
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn exact_bytes_and_refusals() {
        let key = b"{\"schema\":2}";
        let payload = [0, 255, 128, 0, 1];
        let bytes = encode(key, &payload).unwrap();
        assert_eq!(
            decode(&bytes).unwrap(),
            (key.as_slice(), payload.as_slice())
        );
        for n in 0..bytes.len() {
            assert!(decode(&bytes[..n]).is_err(), "truncated {n}");
        }
        let mut wrong = bytes.clone();
        wrong[0] ^= 1;
        assert!(decode(&wrong).is_err());
        let mut wrong = bytes.clone();
        *wrong.last_mut().unwrap() ^= 1;
        assert!(decode(&wrong).is_err());
        let mut wrong = bytes.clone();
        wrong[8..12].copy_from_slice(&u32::MAX.to_le_bytes());
        assert!(decode(&wrong).is_err());
        let mut wrong = bytes;
        wrong.push(0);
        assert!(decode(&wrong).is_err());
    }
}
