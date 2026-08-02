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
