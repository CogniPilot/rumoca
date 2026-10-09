//! Serialize adapter builds until their shared-cache output has been captured.

use std::fs::File;
use std::path::Path;

pub(super) fn acquire(target: &Path) -> Result<File, String> {
    std::fs::create_dir_all(target).map_err(|error| format!("create component cache: {error}"))?;
    let lease = File::options()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(target.join(".rumoca-component-build.lock"))
        .map_err(|error| format!("open component cache lease: {error}"))?;
    lease
        .lock()
        .map_err(|error| format!("lock component cache: {error}"))?;
    Ok(lease)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cache_lease_excludes_a_concurrent_adapter_until_byte_capture_finishes() {
        let work = tempfile::tempdir().unwrap();
        let first = acquire(work.path()).unwrap();
        let second = File::options()
            .read(true)
            .write(true)
            .open(work.path().join(".rumoca-component-build.lock"))
            .unwrap();
        assert!(matches!(
            second.try_lock(),
            Err(std::fs::TryLockError::WouldBlock)
        ));
        drop(first);
        second.try_lock().unwrap();
    }
}
