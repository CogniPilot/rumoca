use std::fmt;
#[cfg(target_os = "linux")]
use std::io;
#[cfg(target_os = "linux")]
use std::thread;
#[cfg(target_os = "linux")]
use std::time::Duration;

#[cfg(target_os = "linux")]
use crate::{
    MODEL_WORKER_MEMORY_LIMIT_EXCEEDED_CLASSIFICATION, MODEL_WORKER_MEMORY_LIMIT_EXIT_CODE,
};
use crate::{
    MODEL_WORKER_MEMORY_LIMIT_UNAVAILABLE_CLASSIFICATION,
    MODEL_WORKER_MEMORY_LIMIT_UNAVAILABLE_EXIT_CODE,
};

#[cfg(target_os = "linux")]
const MEMORY_WATCHDOG_POLL_INTERVAL: Duration = Duration::from_millis(250);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WorkerMemoryLimitEnforcement {
    Unlimited,
    Enforced,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WorkerMemoryLimitStartError {
    kind: WorkerMemoryLimitStartErrorKind,
    detail: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WorkerMemoryLimitStartErrorKind {
    #[cfg(target_os = "linux")]
    Exceeded,
    Unavailable,
}

impl WorkerMemoryLimitStartError {
    #[cfg(target_os = "linux")]
    fn exceeded(memory_limit_mb: usize, observed_kb: u64) -> Self {
        Self {
            kind: WorkerMemoryLimitStartErrorKind::Exceeded,
            detail: format!(
                "rumoca-worker already uses {observed_kb} kB, exceeding its \
                 {memory_limit_mb} MB resident-plus-swap limit"
            ),
        }
    }

    fn unavailable(detail: impl Into<String>) -> Self {
        Self {
            kind: WorkerMemoryLimitStartErrorKind::Unavailable,
            detail: detail.into(),
        }
    }

    pub fn exit_code(&self) -> i32 {
        match self.kind {
            #[cfg(target_os = "linux")]
            WorkerMemoryLimitStartErrorKind::Exceeded => MODEL_WORKER_MEMORY_LIMIT_EXIT_CODE,
            WorkerMemoryLimitStartErrorKind::Unavailable => {
                MODEL_WORKER_MEMORY_LIMIT_UNAVAILABLE_EXIT_CODE
            }
        }
    }
}

impl fmt::Display for WorkerMemoryLimitStartError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let classification = match self.kind {
            #[cfg(target_os = "linux")]
            WorkerMemoryLimitStartErrorKind::Exceeded => {
                MODEL_WORKER_MEMORY_LIMIT_EXCEEDED_CLASSIFICATION
            }
            WorkerMemoryLimitStartErrorKind::Unavailable => {
                MODEL_WORKER_MEMORY_LIMIT_UNAVAILABLE_CLASSIFICATION
            }
        };
        write!(formatter, "[{classification}] {}", self.detail)
    }
}

impl std::error::Error for WorkerMemoryLimitStartError {}

pub fn start_worker_memory_limit(
    memory_limit_mb: usize,
) -> Result<WorkerMemoryLimitEnforcement, WorkerMemoryLimitStartError> {
    if memory_limit_mb == 0 {
        return Ok(WorkerMemoryLimitEnforcement::Unlimited);
    }
    start_platform_memory_limit(memory_limit_mb)
}

#[cfg(target_os = "linux")]
fn start_platform_memory_limit(
    memory_limit_mb: usize,
) -> Result<WorkerMemoryLimitEnforcement, WorkerMemoryLimitStartError> {
    start_linux_memory_limit_with(
        memory_limit_mb,
        current_linux_resident_and_swap_kb,
        |watchdog| {
            thread::Builder::new()
                .name("rumoca-worker-memory-watchdog".to_string())
                .spawn(watchdog)
                .map(|_| ())
        },
    )
}

#[cfg(not(target_os = "linux"))]
fn start_platform_memory_limit(
    _memory_limit_mb: usize,
) -> Result<WorkerMemoryLimitEnforcement, WorkerMemoryLimitStartError> {
    Err(WorkerMemoryLimitStartError::unavailable(format!(
        "resident-plus-swap enforcement is unsupported on {}",
        std::env::consts::OS
    )))
}

#[cfg(target_os = "linux")]
fn start_linux_memory_limit_with<ReadMemory, SpawnWatchdog>(
    memory_limit_mb: usize,
    read_memory: ReadMemory,
    spawn_watchdog: SpawnWatchdog,
) -> Result<WorkerMemoryLimitEnforcement, WorkerMemoryLimitStartError>
where
    ReadMemory: Fn() -> Result<u64, String> + Send + 'static,
    SpawnWatchdog: FnOnce(Box<dyn FnOnce() + Send>) -> io::Result<()>,
{
    let memory_limit_kb = u64::try_from(memory_limit_mb)
        .ok()
        .and_then(|limit| limit.checked_mul(1024))
        .ok_or_else(|| {
            WorkerMemoryLimitStartError::unavailable(format!(
                "{memory_limit_mb} MB memory limit cannot be represented in kB"
            ))
        })?;
    let observed_kb = read_memory().map_err(|error| {
        WorkerMemoryLimitStartError::unavailable(format!(
            "cannot read Linux resident-plus-swap accounting: {error}"
        ))
    })?;
    if observed_kb > memory_limit_kb {
        return Err(WorkerMemoryLimitStartError::exceeded(
            memory_limit_mb,
            observed_kb,
        ));
    }

    let watchdog = Box::new(move || {
        loop {
            thread::sleep(MEMORY_WATCHDOG_POLL_INTERVAL);
            match read_memory() {
                Ok(memory_kb) if memory_kb > memory_limit_kb => {
                    eprintln!(
                        "[{MODEL_WORKER_MEMORY_LIMIT_EXCEEDED_CLASSIFICATION}] \
                         rumoca-worker exceeded its \
                         {memory_limit_mb} MB resident-plus-swap limit"
                    );
                    std::process::exit(MODEL_WORKER_MEMORY_LIMIT_EXIT_CODE);
                }
                Ok(_) => {}
                Err(error) => {
                    eprintln!(
                        "[{MODEL_WORKER_MEMORY_LIMIT_UNAVAILABLE_CLASSIFICATION}] \
                         lost Linux resident-plus-swap accounting: {error}"
                    );
                    std::process::exit(MODEL_WORKER_MEMORY_LIMIT_UNAVAILABLE_EXIT_CODE);
                }
            }
        }
    });
    spawn_watchdog(watchdog).map_err(|error| {
        WorkerMemoryLimitStartError::unavailable(format!(
            "cannot start memory watchdog thread: {error}"
        ))
    })?;
    Ok(WorkerMemoryLimitEnforcement::Enforced)
}

#[cfg(target_os = "linux")]
fn current_linux_resident_and_swap_kb() -> Result<u64, String> {
    let raw = std::fs::read_to_string("/proc/self/status")
        .map_err(|error| format!("failed to read /proc/self/status: {error}"))?;
    resident_and_swap_kb_from_linux_status(&raw)
}

#[cfg(target_os = "linux")]
fn resident_and_swap_kb_from_linux_status(raw: &str) -> Result<u64, String> {
    let rss_kb = linux_status_kb_field(raw, "VmRSS:")?;
    let swap_kb = linux_status_kb_field(raw, "VmSwap:")?;
    rss_kb
        .checked_add(swap_kb)
        .ok_or_else(|| "VmRSS plus VmSwap overflows u64".to_string())
}

#[cfg(target_os = "linux")]
fn linux_status_kb_field(raw: &str, field: &str) -> Result<u64, String> {
    let line = raw
        .lines()
        .find(|line| line.starts_with(field))
        .ok_or_else(|| format!("/proc/self/status is missing {field}"))?;
    let mut values = line[field.len()..].split_whitespace();
    let value = values
        .next()
        .ok_or_else(|| format!("{field} has no value"))?
        .parse::<u64>()
        .map_err(|error| format!("{field} has an invalid value: {error}"))?;
    match (values.next(), values.next()) {
        (Some("kB"), None) => Ok(value),
        _ => Err(format!("{field} is not a single kB value")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(target_os = "linux")]
    fn fail_memory_read_after_first(reads: &std::sync::atomic::AtomicUsize) -> Result<u64, String> {
        if reads.fetch_add(1, std::sync::atomic::Ordering::SeqCst) == 0 {
            Ok(1024)
        } else {
            Err("injected runtime read failure".to_string())
        }
    }

    #[cfg(target_os = "linux")]
    fn run_runtime_accounting_failure_child() -> ! {
        let reads = std::sync::Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let watchdog_reads = std::sync::Arc::clone(&reads);
        start_linux_memory_limit_with(
            8,
            move || fail_memory_read_after_first(&watchdog_reads),
            |watchdog| {
                thread::Builder::new()
                    .name("rumoca-test-memory-watchdog".to_string())
                    .spawn(watchdog)
                    .map(|_| ())
            },
        )
        .expect("initial accounting should establish enforcement");
        thread::sleep(Duration::from_secs(2));
        panic!("watchdog did not terminate the child after accounting was lost");
    }

    #[test]
    fn zero_limit_explicitly_disables_enforcement() {
        assert_eq!(
            start_worker_memory_limit(0).expect("zero means unlimited"),
            WorkerMemoryLimitEnforcement::Unlimited
        );
    }

    #[cfg(not(target_os = "linux"))]
    #[test]
    fn positive_limit_fails_closed_on_unsupported_platform() {
        let error = start_worker_memory_limit(1).expect_err("platform must refuse bounded mode");
        assert_eq!(
            error.exit_code(),
            MODEL_WORKER_MEMORY_LIMIT_UNAVAILABLE_EXIT_CODE
        );
        assert!(
            error
                .to_string()
                .contains(MODEL_WORKER_MEMORY_LIMIT_UNAVAILABLE_CLASSIFICATION)
        );
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn linux_accounting_counts_resident_and_swapped_pages() {
        let status = "Name:\trumoca-worker\nVmRSS:\t4096 kB\nVmSwap:\t2048 kB\n";
        assert_eq!(resident_and_swap_kb_from_linux_status(status), Ok(6144));
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn linux_accounting_rejects_missing_or_malformed_fields() {
        assert!(
            resident_and_swap_kb_from_linux_status("Name:\ttest\n")
                .expect_err("missing accounting must fail")
                .contains("VmRSS")
        );
        assert!(
            resident_and_swap_kb_from_linux_status("VmRSS:\t4096 bytes\nVmSwap:\t0 kB\n")
                .expect_err("wrong units must fail")
                .contains("single kB value")
        );
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn linux_initial_accounting_failure_refuses_startup() {
        let result = start_linux_memory_limit_with(
            8,
            || Err("injected read failure".to_string()),
            |_| Ok(()),
        );
        let error = result.expect_err("accounting failure must refuse startup");
        assert_eq!(
            error.exit_code(),
            MODEL_WORKER_MEMORY_LIMIT_UNAVAILABLE_EXIT_CODE
        );
        assert!(error.to_string().contains("injected read failure"));
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn linux_watchdog_spawn_failure_refuses_startup() {
        let result = start_linux_memory_limit_with(
            8,
            || Ok(1024),
            |_| Err(io::Error::other("injected spawn failure")),
        );
        let error = result.expect_err("watchdog spawn failure must refuse startup");
        assert_eq!(
            error.exit_code(),
            MODEL_WORKER_MEMORY_LIMIT_UNAVAILABLE_EXIT_CODE
        );
        assert!(error.to_string().contains("injected spawn failure"));
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn linux_runtime_accounting_failure_exits_only_worker_process() {
        let executable = std::env::current_exe().expect("resolve current worker test executable");
        let marker = executable.with_extension("memory-accounting-loss-child");
        let marker_parent = std::fs::read_to_string(&marker)
            .ok()
            .and_then(|contents| contents.parse::<u32>().ok());
        if marker_parent == linux_parent_process_id() {
            let _ = std::fs::remove_file(&marker);
            run_runtime_accounting_failure_child();
        }

        std::fs::write(&marker, std::process::id().to_string())
            .expect("write isolated watchdog child marker");
        let status = std::process::Command::new(&executable)
            .args([
                "--exact",
                "memory_limit::tests::linux_runtime_accounting_failure_exits_only_worker_process",
                "--nocapture",
            ])
            .status()
            .expect("spawn isolated watchdog child");
        let _ = std::fs::remove_file(marker);
        assert_eq!(
            status.code(),
            Some(MODEL_WORKER_MEMORY_LIMIT_UNAVAILABLE_EXIT_CODE)
        );
    }

    #[cfg(target_os = "linux")]
    fn linux_parent_process_id() -> Option<u32> {
        let stat = std::fs::read_to_string("/proc/self/stat").ok()?;
        let mut fields = stat.rsplit_once(')')?.1.split_whitespace();
        fields.next()?;
        fields.next()?.parse().ok()
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn linux_limit_is_verified_before_watchdog_starts() {
        let result = start_linux_memory_limit_with(
            1,
            || Ok(1025),
            |_| {
                panic!("watchdog must not start after an exceeded initial reading");
            },
        );
        let error = result.expect_err("already-exceeded limit must fail");
        assert_eq!(error.exit_code(), MODEL_WORKER_MEMORY_LIMIT_EXIT_CODE);
        assert!(
            error
                .to_string()
                .contains(MODEL_WORKER_MEMORY_LIMIT_EXCEEDED_CLASSIFICATION)
        );
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn linux_verified_limit_starts_enforcement() {
        let mut spawned = false;
        let state = start_linux_memory_limit_with(
            8,
            || Ok(1024),
            |_| {
                spawned = true;
                Ok(())
            },
        )
        .expect("valid Linux accounting starts enforcement");
        assert_eq!(state, WorkerMemoryLimitEnforcement::Enforced);
        assert!(spawned);
    }
}
