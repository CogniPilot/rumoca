use super::*;

static PERF_AVAILABLE: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
static PERF_WARNED: AtomicBool = AtomicBool::new(false);

pub(super) enum PerfRecordTarget {
    Process(u32),
    Thread(u32),
}

pub(super) struct PerfSession {
    child: std::process::Child,
}

impl PerfSession {
    pub(super) fn finish(mut self) {
        if matches!(self.child.try_wait(), Ok(Some(_))) {
            return;
        }
        request_perf_stop(self.child.id());
        for _ in 0..50 {
            match self.child.try_wait() {
                Ok(Some(_)) => return,
                Ok(None) => std::thread::sleep(Duration::from_millis(20)),
                Err(_) => return,
            }
        }
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

fn request_perf_stop(pid: u32) {
    #[cfg(unix)]
    {
        let _ = Command::new("kill")
            .arg("-INT")
            .arg(pid.to_string())
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status();
    }
}

pub(super) fn env_flag_enabled(key: &str) -> bool {
    std::env::var(key).ok().is_some_and(|raw| {
        matches!(
            raw.trim(),
            "1" | "true" | "TRUE" | "yes" | "YES" | "on" | "ON"
        )
    })
}

pub(super) fn env_nonnegative_f64(key: &str, default: f64) -> f64 {
    std::env::var(key)
        .ok()
        .and_then(|raw| raw.trim().parse::<f64>().ok())
        .filter(|secs| secs.is_finite() && *secs >= 0.0)
        .unwrap_or(default)
}

pub(super) fn env_positive_usize_or(key: &str, default: usize) -> usize {
    std::env::var(key)
        .ok()
        .and_then(|raw| raw.trim().parse::<usize>().ok())
        .filter(|value| *value > 0)
        .unwrap_or(default)
}

pub(super) fn current_thread_perf_id() -> Option<u32> {
    let path = std::fs::read_link("/proc/thread-self").ok()?;
    path.file_name()?.to_str()?.parse::<u32>().ok()
}

pub(super) fn start_current_thread_perf_record_session(
    profile_path: &Path,
    frequency: usize,
    warning_context: &str,
) -> Option<PerfSession> {
    let Some(thread_id) = current_thread_perf_id() else {
        warn_perf_once(format!(
            "{warning_context} perf profiling requested but /proc/thread-self is unavailable; running without profiles"
        ));
        return None;
    };
    start_perf_record_session(
        PerfRecordTarget::Thread(thread_id),
        profile_path,
        frequency,
        warning_context,
    )
}

pub(super) fn start_perf_record_session(
    target: PerfRecordTarget,
    profile_path: &Path,
    frequency: usize,
    warning_context: &str,
) -> Option<PerfSession> {
    if !perf_available() {
        warn_perf_once(format!(
            "{warning_context} perf profiling requested but `perf` is unavailable; running without profiles"
        ));
        return None;
    }

    let mut cmd = Command::new("perf");
    cmd.arg("record")
        .arg("--call-graph")
        .arg("dwarf")
        .arg("-F")
        .arg(frequency.to_string())
        .arg("-o")
        .arg(profile_path);
    match target {
        PerfRecordTarget::Process(pid) => {
            cmd.arg("-p").arg(pid.to_string());
        }
        PerfRecordTarget::Thread(tid) => {
            cmd.arg("-t").arg(tid.to_string());
        }
    }
    cmd.stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null());

    match cmd.spawn() {
        Ok(child) => Some(PerfSession { child }),
        Err(error) => {
            warn_perf_once(format!(
                "failed to start {warning_context} perf profiling ({error}); running without profiles"
            ));
            None
        }
    }
}

fn perf_available() -> bool {
    *PERF_AVAILABLE.get_or_init(|| {
        Command::new("perf")
            .arg("--version")
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .is_ok_and(|status| status.success())
    })
}

fn warn_perf_once(message: String) {
    if !PERF_WARNED.swap(true, Ordering::Relaxed) {
        eprintln!("WARNING: {message}");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nonnegative_f64_env_parser_uses_default_for_missing_values() {
        assert_eq!(env_nonnegative_f64("__RUMOCA_MISSING_ENV__", 3.5), 3.5);
    }

    #[test]
    fn current_thread_perf_id_is_available_on_linux_procfs() {
        if Path::new("/proc/thread-self").exists() {
            assert!(current_thread_perf_id().is_some());
        }
    }
}
