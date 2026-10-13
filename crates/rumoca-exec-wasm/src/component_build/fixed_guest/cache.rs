//! Content-addressed store for the source-built fixed guest core.
//!
//! The key covers every staged byte (path and content), the issuing compiler
//! identity and the exact build recipe, so a hit is the output the same build
//! would produce. No timestamps are consulted.

use std::path::{Path, PathBuf};

const CORE_FILE: &str = "core.wasm";
const MESSAGES_FILE: &str = "messages.txt";
const DIGEST_FILE: &str = "core.blake3";

/// Hex digest naming one build. Fields are length-prefixed so no two distinct
/// inventories can frame to the same byte stream.
pub(super) fn key(
    sources: &[(&str, &[u8])],
    compiler_path: &str,
    compiler_version: &str,
    recipe: &str,
) -> String {
    let mut hasher = blake3::Hasher::new();
    let mut field = |bytes: &[u8]| {
        hasher.update(&(bytes.len() as u64).to_le_bytes());
        hasher.update(bytes);
    };
    field(compiler_path.as_bytes());
    field(compiler_version.as_bytes());
    field(recipe.as_bytes());
    field(&(sources.len() as u64).to_le_bytes());
    for (path, bytes) in sources {
        field(path.as_bytes());
        field(bytes);
    }
    hasher.finalize().to_hex().to_string()
}

/// A previously stored core and the Cargo output that issued it.
pub(super) struct Stored {
    pub(super) bytes: Vec<u8>,
    pub(super) messages: String,
}

fn entry(root: &Path, key: &str) -> PathBuf {
    root.join(key)
}

/// Read an entry whose stored bytes still match their recorded digest; any
/// other state is a miss.
pub(super) fn load(root: &Path, key: &str) -> Option<Stored> {
    let directory = entry(root, key);
    let bytes = std::fs::read(directory.join(CORE_FILE)).ok()?;
    let digest = std::fs::read_to_string(directory.join(DIGEST_FILE)).ok()?;
    if digest.trim() != blake3::hash(&bytes).to_hex().as_str() {
        return None;
    }
    let messages = std::fs::read_to_string(directory.join(MESSAGES_FILE)).ok()?;
    Some(Stored { bytes, messages })
}

/// Publish an entry atomically; a concurrent identical entry is kept.
pub(super) fn store(root: &Path, key: &str, bytes: &[u8], messages: &str) -> Result<(), String> {
    let staging = tempfile::Builder::new()
        .prefix(".fixed-guest-entry-")
        .tempdir_in(root)
        .map_err(|error| format!("fixed guest cache staging: {error}"))?;
    let write = |name: &str, content: &[u8]| {
        std::fs::write(staging.path().join(name), content)
            .map_err(|error| format!("fixed guest cache write {name}: {error}"))
    };
    write(CORE_FILE, bytes)?;
    write(MESSAGES_FILE, messages.as_bytes())?;
    write(DIGEST_FILE, blake3::hash(bytes).to_hex().as_bytes())?;
    match std::fs::rename(staging.keep(), entry(root, key)) {
        Ok(()) => Ok(()),
        Err(_) if load(root, key).is_some() => Ok(()),
        Err(error) => Err(format!("fixed guest cache publish: {error}")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SOURCES: &[(&str, &[u8])] = &[("a/lib.rs", b"fn a() {}"), ("b.toml", b"x = 1")];

    fn base() -> String {
        key(SOURCES, "/nix/rustc", "rustc 1.0\nhost: x", "recipe")
    }

    #[test]
    fn key_changes_with_every_staged_byte_path_and_compiler_identity() {
        let reference = base();
        assert_eq!(reference, base());
        let edited: &[(&str, &[u8])] = &[("a/lib.rs", b"fn a() { }"), SOURCES[1]];
        let renamed: &[(&str, &[u8])] = &[("a/lib2.rs", SOURCES[0].1), SOURCES[1]];
        let dropped = &SOURCES[..1];
        let reordered: &[(&str, &[u8])] = &[SOURCES[1], SOURCES[0]];
        for other in [edited, renamed, dropped, reordered] {
            assert_ne!(
                reference,
                key(other, "/nix/rustc", "rustc 1.0\nhost: x", "recipe")
            );
        }
        assert_ne!(
            reference,
            key(SOURCES, "/nix/rustc2", "rustc 1.0\nhost: x", "recipe")
        );
        assert_ne!(
            reference,
            key(SOURCES, "/nix/rustc", "rustc 1.1\nhost: x", "recipe")
        );
        assert_ne!(
            reference,
            key(SOURCES, "/nix/rustc", "rustc 1.0\nhost: x", "recipe2")
        );
    }

    #[test]
    fn key_framing_separates_adjacent_fields() {
        let joined: &[(&str, &[u8])] = &[("ab", b"c")];
        let shifted: &[(&str, &[u8])] = &[("a", b"bc")];
        assert_ne!(key(joined, "", "", ""), key(shifted, "", "", ""));
    }

    #[test]
    fn stored_core_loads_only_while_its_digest_matches() {
        let root = tempfile::tempdir().unwrap();
        assert!(load(root.path(), "k").is_none());
        store(root.path(), "k", b"\0asm", "messages").unwrap();
        let hit = load(root.path(), "k").unwrap();
        assert_eq!(
            (hit.bytes.as_slice(), hit.messages.as_str()),
            (&b"\0asm"[..], "messages")
        );
        store(root.path(), "k", b"\0asm", "messages").unwrap();
        std::fs::write(root.path().join("k").join(CORE_FILE), b"tampered").unwrap();
        assert!(load(root.path(), "k").is_none());
    }
}
