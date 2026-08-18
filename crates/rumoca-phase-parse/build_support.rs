//! File installation support shared by the parser build script and its tests.

use std::fs;
use std::io;
use std::path::Path;

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
