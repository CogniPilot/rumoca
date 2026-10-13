//! File installation support shared by the parser build script and its tests.

use std::fs;
use std::io;
use std::path::Path;

/// Exact `parol` release whose output the checked-in parsers record. It is the
/// workspace pin; the architecture gate keeps the two equal.
pub(crate) const PAROL_VERSION: &str = "4.2.2";

/// Content stamp of everything that determines the generated parser: the
/// grammar, the generator settings (the build script itself) and the pinned
/// generator release. FNV-1a over length-prefixed parts, so it is stable across
/// platforms and toolchains and independent of file modification times.
pub(crate) fn generation_stamp(grammar: &[u8], build_script: &[u8]) -> String {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for part in [PAROL_VERSION.as_bytes(), grammar, build_script] {
        for byte in (part.len() as u64).to_le_bytes().iter().chain(part) {
            hash ^= u64::from(*byte);
            hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
        }
    }
    format!("{hash:016x}\n")
}

/// Whether the checked-in parser was generated from exactly `stamp`'s inputs.
///
/// The decision reads file content only. Generation also depends on the
/// formatter installed on the build machine, so an unchanged grammar must never
/// trigger regeneration: that would rewrite tracked files with whatever the
/// local formatter produces.
pub(crate) fn generation_is_current(stamp_file: &Path, stamp: &str, outputs: &[&Path]) -> bool {
    outputs.iter().all(|output| output.is_file())
        && fs::read_to_string(stamp_file).is_ok_and(|recorded| recorded == stamp)
}

/// Install one staged generated file only when its bytes changed.
///
/// Parol always writes its output. Keeping that write under `OUT_DIR` and
/// guarding the checked-in copy prevents unrelated build-script instances from
/// changing source mtimes and invalidating warm Cargo artifacts.
pub(crate) fn install_generated_if_changed(staged: &Path, checked_in: &Path) -> io::Result<bool> {
    let generated = fs::read(staged)?;
    match fs::read(checked_in) {
        Ok(existing) if existing == generated => return Ok(false),
        Ok(_) => {}
        Err(err) if err.kind() == io::ErrorKind::NotFound => {}
        Err(err) => return Err(err),
    }
    fs::write(checked_in, generated)?;
    Ok(true)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unchanged_inputs_keep_generation_current_regardless_of_mtime() {
        let temp = tempfile::tempdir().expect("temporary directory");
        let output = temp.path().join("parser.rs");
        let stamp_file = temp.path().join("grammar.stamp");
        let stamp = generation_stamp(b"grammar", b"settings");
        fs::write(&output, b"fn parse() {}\n").expect("seed output");
        fs::write(&stamp_file, &stamp).expect("seed stamp");
        // Grammar touched after the output: content is unchanged, so no regeneration.
        fs::write(temp.path().join("grammar.par"), b"grammar").expect("touch grammar");

        assert!(generation_is_current(&stamp_file, &stamp, &[&output]));
    }

    #[test]
    fn changed_grammar_settings_or_missing_output_require_generation() {
        let temp = tempfile::tempdir().expect("temporary directory");
        let output = temp.path().join("parser.rs");
        let stamp_file = temp.path().join("grammar.stamp");
        let stamp = generation_stamp(b"grammar", b"settings");
        fs::write(&stamp_file, &stamp).expect("seed stamp");

        assert!(!generation_is_current(&stamp_file, &stamp, &[&output]));
        fs::write(&output, b"fn parse() {}\n").expect("seed output");
        assert!(generation_is_current(&stamp_file, &stamp, &[&output]));
        for changed in [
            generation_stamp(b"grammar2", b"settings"),
            generation_stamp(b"grammar", b"settings2"),
        ] {
            assert_ne!(changed, stamp);
            assert!(!generation_is_current(&stamp_file, &changed, &[&output]));
        }
        assert!(!generation_is_current(
            &temp.path().join("absent.stamp"),
            &stamp,
            &[&output]
        ));
    }

    #[test]
    fn equal_generated_content_preserves_checked_in_mtime() {
        let temp = tempfile::tempdir().expect("temporary directory");
        let staged = temp.path().join("staged.rs");
        let checked_in = temp.path().join("checked_in.rs");
        let content = b"fn generated() {}\n";
        fs::write(&staged, content).expect("seed staged output");
        fs::write(&checked_in, content).expect("seed checked-in output");
        let modified_before = fs::metadata(&checked_in)
            .expect("checked-in metadata")
            .modified()
            .expect("checked-in mtime");

        let changed = install_generated_if_changed(&staged, &checked_in).expect("install output");

        let modified_after = fs::metadata(&checked_in)
            .expect("checked-in metadata")
            .modified()
            .expect("checked-in mtime");
        assert!(!changed);
        assert_eq!(modified_after, modified_before);
        assert_eq!(fs::read(&checked_in).expect("checked-in bytes"), content);
    }

    #[test]
    fn changed_generated_content_updates_checked_in_file() {
        let temp = tempfile::tempdir().expect("temporary directory");
        let staged = temp.path().join("staged.rs");
        let checked_in = temp.path().join("checked_in.rs");
        fs::write(&staged, b"fn generated() { revised(); }\n").expect("seed staged output");
        fs::write(&checked_in, b"fn generated() {}\n").expect("seed checked-in output");

        let changed = install_generated_if_changed(&staged, &checked_in).expect("install output");

        assert!(changed);
        assert_eq!(
            fs::read(&checked_in).expect("checked-in bytes"),
            b"fn generated() { revised(); }\n"
        );
    }
}
