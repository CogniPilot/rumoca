//! Host-aware resource budgets inherited by repository-tool subprocesses.
//!
//! Two things bound how much work the Rust toolchain may run in parallel: CPU
//! topology and host memory. The memory bound exists because the development
//! hosts run `earlyoom -m10`, which SIGKILLs the largest process as soon as
//! available memory falls below 10% of total RAM (~6.3 GiB on a 62 GiB box).
//! A job count derived from CPU count alone starts 28 concurrent jobs here, and
//! a single link peaks around 1 GiB resident, so the link-heavy tail of a
//! workspace build walks straight into that threshold. Deriving the job count
//! from `/proc/meminfo` `MemAvailable` as well keeps the peak under the floor
//! instead of relying on an out-of-memory-killer exemption.

use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

const CARGO_BUILD_JOBS: &str = "CARGO_BUILD_JOBS";
const RUST_TEST_THREADS: &str = "RUST_TEST_THREADS";
const RAYON_NUM_THREADS: &str = "RAYON_NUM_THREADS";
const MAX_RESERVED_PHYSICAL_CORES: usize = 2;

/// Measured peak resident set of one Cargo job slot while linking, in MiB.
///
/// Calibration (32-logical-CPU / 16-core host, `profile.dev` carrying
/// `debug = "line-tables-only"` and `split-debuginfo = "unpacked"`):
/// `CARGO_BUILD_JOBS=1 cargo test -p rumoca --no-run`, run after touching
/// `crates/rumoca/src/lib.rs`, relinks the package's ~60 integration-test
/// binaries one at a time. Sampling `VmHWM` from `/proc/<pid>/status` across
/// that run at 20 Hz:
///
/// * `rust-lld` peaked at 1043 MiB (851-1000 MiB was the common band),
/// * the `rustc` driving each link peaked at 502 MiB,
/// * the whole build process tree peaked at 1346 MiB resident, of which 179 MiB
///   is the one `cargo` parent that does not repeat per job.
///
/// One job slot therefore costs ~1.17 GiB at its peak, rounded up here. Re-run
/// the same measurement whenever the debuginfo or codegen profile changes: the
/// pre-`line-tables-only` binaries were 806 MiB instead of 116-168 MiB, and the
/// link peak scaled with them.
const RUST_JOB_LINK_PEAK_MIB: usize = 1_250;

/// Safety factor applied to [`RUST_JOB_LINK_PEAK_MIB`].
///
/// Peaks do not all land at once - an 8-job build of the same package peaked at
/// 5.2 GiB rather than 8 GiB - but the calibration covers one package's links on
/// one host, codegen-bound jobs in the heavier compiler crates cost more than
/// link-bound ones, and the failure modes are asymmetric: overshooting the
/// estimate costs a few percent of build throughput, undershooting it costs a
/// SIGKILL from earlyoom part-way through a gate.
const RUST_JOB_MEMORY_SAFETY_FACTOR: usize = 2;

/// Memory charged against the host budget per Cargo job slot, in MiB.
const RUST_JOB_MEMORY_MIB: usize = RUST_JOB_LINK_PEAK_MIB * RUST_JOB_MEMORY_SAFETY_FACTOR;

/// Memory charged against the host budget per `libtest` thread, in MiB.
///
/// Sampled the same way while running `cargo test -p rumoca` at 32 test
/// threads: the heaviest test process in the package peaked at 81 MiB resident.
/// 256 MiB keeps a comparable margin to the build estimate, so the derived cap
/// only binds when the host is genuinely short of memory rather than trimming
/// test parallelism on a healthy host.
const RUST_TEST_THREAD_MEMORY_MIB: usize = 256;

/// Available-memory percentage at which `earlyoom -m10` starts killing the
/// largest process. Budgets must stay clear of this line by construction.
const EARLYOOM_KILL_PERCENT_OF_TOTAL: usize = 10;

/// Share of total RAM held back from the build budget: the earlyoom kill line
/// plus five points of slack.
///
/// The slack means a job whose peak overshoots the per-job estimate still has
/// 5% of RAM (~3 GiB here) before the kill threshold, and expressing the reserve
/// as a percentage keeps it scaled to the host rather than a fixed number that
/// shrinks into insignificance on large machines.
const HOST_MEMORY_RESERVE_PERCENT: usize = EARLYOOM_KILL_PERCENT_OF_TOTAL + 5;

/// Lower bound on the host reserve, for hosts where 15% is a small absolute
/// number. Also covers the editor, language server, and page cache that share
/// the box with a gate run.
const MIN_HOST_MEMORY_RESERVE_MIB: usize = 4_096;

const MIB_PER_KIB: usize = 1_024;

/// Total and available memory as reported by `/proc/meminfo`, in MiB.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct HostMemory {
    total_mib: usize,
    available_mib: usize,
}

impl HostMemory {
    fn detect() -> Option<Self> {
        std::fs::read_to_string("/proc/meminfo")
            .ok()
            .as_deref()
            .and_then(Self::parse_meminfo)
    }

    /// Parse `MemTotal` and `MemAvailable` (both reported in kB) into MiB.
    fn parse_meminfo(meminfo: &str) -> Option<Self> {
        let field_mib = |key: &str| {
            meminfo.lines().find_map(|line| {
                let rest = line.strip_prefix(key)?;
                let rest = rest.strip_prefix(':')?;
                let kib = rest.split_whitespace().next()?.parse::<usize>().ok()?;
                Some(kib / MIB_PER_KIB)
            })
        };
        let total_mib = field_mib("MemTotal")?;
        let available_mib = field_mib("MemAvailable")?;
        Some(Self {
            total_mib,
            available_mib,
        })
    }

    /// Memory withheld from every derived budget so the host stays above the
    /// earlyoom kill threshold.
    fn reserve_mib(self) -> usize {
        let proportional = self
            .total_mib
            .saturating_mul(HOST_MEMORY_RESERVE_PERCENT)
            .div_ceil(100);
        proportional.max(MIN_HOST_MEMORY_RESERVE_MIB)
    }

    /// Memory a budget may actually spend.
    fn usable_mib(self) -> usize {
        self.available_mib.saturating_sub(self.reserve_mib())
    }

    /// Worker count this host can afford at `per_worker_mib` each, at least one.
    fn workers_for(self, per_worker_mib: usize) -> usize {
        (self.usable_mib() / per_worker_mib.max(1)).max(1)
    }
}

/// Which bound decided the job count, for the budget notice.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum JobBound {
    /// CPU topology was the tighter bound (or memory could not be read).
    CpuTopology,
    /// Host memory was the tighter bound.
    HostMemory,
}

impl JobBound {
    fn as_str(self) -> &'static str {
        match self {
            Self::CpuTopology => "cpu-bound",
            Self::HostMemory => "memory-bound",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RustJobBudget {
    logical_cpus: usize,
    physical_cores: usize,
    reserved_physical_cores: usize,
    /// Jobs the CPU topology alone would allow.
    cpu_jobs: usize,
    /// Host memory, when `/proc/meminfo` could be read.
    memory: Option<HostMemory>,
    /// Jobs host memory alone would allow.
    memory_jobs: Option<usize>,
    /// Effective budget: the tighter of the two bounds.
    jobs: usize,
    bound: JobBound,
}

impl RustJobBudget {
    fn detect() -> Self {
        let logical_cpus = std::thread::available_parallelism()
            .map(usize::from)
            .unwrap_or(1);
        let physical_cores = num_cpus::get_physical().clamp(1, logical_cpus);
        Self::for_host(logical_cpus, physical_cores, HostMemory::detect())
    }

    fn for_host(logical_cpus: usize, physical_cores: usize, memory: Option<HostMemory>) -> Self {
        let logical_cpus = logical_cpus.max(1);
        let physical_cores = physical_cores.clamp(1, logical_cpus);
        let requested_reserve = match logical_cpus {
            1..=3 => 0,
            4..=7 => 1,
            _ => MAX_RESERVED_PHYSICAL_CORES,
        };
        let reserved_physical_cores = requested_reserve.min(physical_cores.saturating_sub(1));
        let reserved_logical_cpus = logical_cpus
            .saturating_mul(reserved_physical_cores)
            .div_ceil(physical_cores);
        let cpu_jobs = logical_cpus.saturating_sub(reserved_logical_cpus).max(1);
        let memory_jobs = memory.map(|memory| memory.workers_for(RUST_JOB_MEMORY_MIB));
        let jobs = memory_jobs.map_or(cpu_jobs, |memory_jobs| cpu_jobs.min(memory_jobs));
        let bound = if memory_jobs.is_some_and(|memory_jobs| memory_jobs < cpu_jobs) {
            JobBound::HostMemory
        } else {
            JobBound::CpuTopology
        };
        Self {
            logical_cpus,
            physical_cores,
            reserved_physical_cores,
            cpu_jobs,
            memory,
            memory_jobs,
            jobs,
            bound,
        }
    }

    /// Test-thread cap, or `None` when memory allows the libtest default of one
    /// thread per logical CPU. The budget only ever lowers test parallelism.
    fn test_threads(self) -> Option<usize> {
        let threads = self
            .memory?
            .workers_for(RUST_TEST_THREAD_MEMORY_MIB)
            .min(self.logical_cpus);
        (threads < self.logical_cpus).then_some(threads)
    }

    fn describe_jobs(self) -> String {
        let topology = format!(
            "{} logical CPUs / {} physical cores, reserving {} physical cores",
            self.logical_cpus, self.physical_cores, self.reserved_physical_cores
        );
        let reason = match (self.memory, self.memory_jobs) {
            (Some(memory), Some(memory_jobs)) => format!(
                "{}; cpu allows {} ({topology}); memory allows {memory_jobs} = ({} MiB MemAvailable - {} MiB host reserve) / {RUST_JOB_MEMORY_MIB} MiB per job",
                self.bound.as_str(),
                self.cpu_jobs,
                memory.available_mib,
                memory.reserve_mib(),
            ),
            _ => format!(
                "{}; cpu allows {} ({topology}); /proc/meminfo unavailable, no memory bound applied",
                self.bound.as_str(),
                self.cpu_jobs,
            ),
        };
        format!(
            "Rust build budget: {} jobs ({reason}); override with {CARGO_BUILD_JOBS}",
            self.jobs
        )
    }

    fn describe_test_threads(self, threads: usize) -> String {
        let detail = match self.memory {
            Some(memory) => format!(
                "memory-bound; ({} MiB MemAvailable - {} MiB host reserve) / {RUST_TEST_THREAD_MEMORY_MIB} MiB per test thread",
                memory.available_mib,
                memory.reserve_mib(),
            ),
            None => "memory-bound".to_string(),
        };
        format!(
            "Rust test budget: {threads} test threads ({detail}, below {} logical CPUs); override with {RUST_TEST_THREADS}",
            self.logical_cpus
        )
    }
}

/// Last job count announced by [`apply_to_child`]; 0 before the first notice.
static ANNOUNCED_JOBS: AtomicUsize = AtomicUsize::new(0);
/// Last test-thread cap announced by [`apply_to_child`]; 0 means "uncapped".
static ANNOUNCED_TEST_THREADS: AtomicUsize = AtomicUsize::new(0);

/// Apply the automatic Cargo and libtest budgets to a child process and
/// everything it launches. Explicit overrides remain authoritative per variable.
///
/// The budget is re-derived for every child rather than sampled once: a gate
/// spawns children over tens of minutes, and the memory available when it starts
/// says nothing about the memory available when its last child links. The notice
/// is reprinted whenever the derived budget changes, so the logged value always
/// describes the child that is about to run.
pub(crate) fn apply_to_child(command: &mut Command) {
    let jobs_overridden = std::env::var_os(CARGO_BUILD_JOBS).is_some();
    let test_threads_overridden = std::env::var_os(RUST_TEST_THREADS).is_some();
    let rayon_overridden = std::env::var_os(RAYON_NUM_THREADS).is_some();
    if jobs_overridden && test_threads_overridden && rayon_overridden {
        return;
    }
    let budget = RustJobBudget::detect();
    // Rayon pools default to every logical CPU; the harness's parallel MSL
    // parse fans out in one burst, which reads as a runaway spawn to
    // growth-based process killers and oversubscribes the host regardless.
    // Cap the pool at the same derived job budget unless the caller chose.
    if !rayon_overridden {
        command.env(RAYON_NUM_THREADS, budget.jobs.to_string());
    }
    let test_threads = budget.test_threads();
    if !jobs_overridden {
        command.env(CARGO_BUILD_JOBS, budget.jobs.to_string());
        if ANNOUNCED_JOBS.swap(budget.jobs, Ordering::Relaxed) != budget.jobs {
            eprintln!("{}", budget.describe_jobs());
        }
    }
    if !test_threads_overridden {
        let announced = ANNOUNCED_TEST_THREADS.swap(test_threads.unwrap_or(0), Ordering::Relaxed);
        match test_threads {
            Some(threads) => {
                command.env(RUST_TEST_THREADS, threads.to_string());
                if announced != threads {
                    eprintln!("{}", budget.describe_test_threads(threads));
                }
            }
            // Memory recovered: say so rather than leaving the last cap notice
            // as the standing description of a run that is no longer capped.
            None if announced != 0 => eprintln!(
                "Rust test budget: test-thread cap lifted, back to {} logical CPUs",
                budget.logical_cpus
            ),
            None => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A host with memory to spare, so topology tests exercise the CPU bound.
    fn unconstrained_memory() -> Option<HostMemory> {
        Some(HostMemory {
            total_mib: 1_024 * 1_024,
            available_mib: 1_024 * 1_024,
        })
    }

    fn for_topology(logical_cpus: usize, physical_cores: usize) -> RustJobBudget {
        RustJobBudget::for_host(logical_cpus, physical_cores, unconstrained_memory())
    }

    #[test]
    fn small_hosts_keep_all_logical_cpus() {
        assert_eq!(for_topology(1, 1).jobs, 1);
        assert_eq!(for_topology(3, 2).jobs, 3);
    }

    #[test]
    fn medium_hosts_reserve_one_physical_core() {
        let budget = for_topology(4, 2);
        assert_eq!(budget.reserved_physical_cores, 1);
        assert_eq!(budget.jobs, 2);

        let budget = for_topology(7, 4);
        assert_eq!(budget.reserved_physical_cores, 1);
        assert_eq!(budget.jobs, 5);
    }

    #[test]
    fn large_hosts_reserve_at_most_two_physical_cores() {
        let budget = for_topology(8, 4);
        assert_eq!(budget.reserved_physical_cores, 2);
        assert_eq!(budget.jobs, 4);

        let budget = for_topology(32, 16);
        assert_eq!(budget.reserved_physical_cores, 2);
        assert_eq!(budget.cpu_jobs, 28);

        let budget = for_topology(128, 64);
        assert_eq!(budget.reserved_physical_cores, 2);
        assert_eq!(budget.cpu_jobs, 124);
    }

    #[test]
    fn budget_always_leaves_one_physical_core_for_building() {
        let budget = for_topology(8, 1);
        assert_eq!(budget.reserved_physical_cores, 0);
        assert_eq!(budget.jobs, 8);
    }

    #[test]
    fn meminfo_parser_reads_total_and_available() {
        let meminfo =
            "MemTotal:       65744636 kB\nMemFree:         1234 kB\nMemAvailable:   59825652 kB\n";
        let memory = HostMemory::parse_meminfo(meminfo).expect("parsed");
        assert_eq!(memory.total_mib, 64_203);
        assert_eq!(memory.available_mib, 58_423);
    }

    #[test]
    fn meminfo_parser_rejects_incomplete_or_unparsable_input() {
        assert_eq!(HostMemory::parse_meminfo("MemTotal:  65744636 kB\n"), None);
        assert_eq!(
            HostMemory::parse_meminfo("MemAvailable:   59825652 kB\n"),
            None
        );
        assert_eq!(
            HostMemory::parse_meminfo("MemTotal: lots kB\nMemAvailable: 100 kB\n"),
            None
        );
        // A prefix match must not be mistaken for the field itself.
        assert_eq!(
            HostMemory::parse_meminfo("MemTotalHuge:  1 kB\nMemAvailable: 1024 kB\n"),
            None
        );
    }

    #[test]
    fn host_reserve_scales_with_total_memory_above_a_floor() {
        let large = HostMemory {
            total_mib: 64_203,
            available_mib: 58_423,
        };
        assert_eq!(large.reserve_mib(), 9_631);

        let small = HostMemory {
            total_mib: 8_192,
            available_mib: 6_000,
        };
        assert_eq!(small.reserve_mib(), MIN_HOST_MEMORY_RESERVE_MIB);
    }

    #[test]
    fn host_reserve_stays_clear_of_the_earlyoom_kill_threshold() {
        for total_gib in [4usize, 8, 16, 32, 62, 128, 512] {
            let total_mib = total_gib * 1_024;
            let memory = HostMemory {
                total_mib,
                available_mib: total_mib,
            };
            let kill_threshold_mib = total_mib * EARLYOOM_KILL_PERCENT_OF_TOTAL / 100;
            assert!(
                memory.reserve_mib() > kill_threshold_mib,
                "reserve {} MiB must exceed the earlyoom floor {kill_threshold_mib} MiB on a {total_gib} GiB host",
                memory.reserve_mib(),
            );
        }
    }

    #[test]
    fn memory_bound_divides_usable_memory_by_the_per_job_estimate() {
        let memory = HostMemory {
            total_mib: 64_203,
            available_mib: 58_423,
        };
        // (58_423 - 9_631) / 2_500 = 19
        assert_eq!(memory.usable_mib(), 48_792);
        assert_eq!(memory.workers_for(RUST_JOB_MEMORY_MIB), 19);
    }

    #[test]
    fn memory_bound_caps_the_cpu_derived_job_count() {
        let memory = HostMemory {
            total_mib: 64_203,
            available_mib: 30_000,
        };
        let budget = RustJobBudget::for_host(32, 16, Some(memory));
        assert_eq!(budget.cpu_jobs, 28);
        // (30_000 - 9_631) / 2_500 = 8
        assert_eq!(budget.memory_jobs, Some(8));
        assert_eq!(budget.jobs, 8);
        assert_eq!(budget.bound, JobBound::HostMemory);
    }

    #[test]
    fn cpu_bound_wins_when_memory_is_plentiful() {
        let memory = HostMemory {
            total_mib: 64_203,
            available_mib: 58_423,
        };
        let budget = RustJobBudget::for_host(8, 4, Some(memory));
        assert_eq!(budget.memory_jobs, Some(19));
        assert_eq!(budget.jobs, 4);
        assert_eq!(budget.bound, JobBound::CpuTopology);
    }

    #[test]
    fn exhausted_memory_still_yields_one_job() {
        let memory = HostMemory {
            total_mib: 64_203,
            available_mib: 1_000,
        };
        let budget = RustJobBudget::for_host(32, 16, Some(memory));
        assert_eq!(budget.jobs, 1);
        assert_eq!(budget.bound, JobBound::HostMemory);
    }

    #[test]
    fn missing_meminfo_falls_back_to_the_cpu_bound() {
        let budget = RustJobBudget::for_host(32, 16, None);
        assert_eq!(budget.memory_jobs, None);
        assert_eq!(budget.jobs, 28);
        assert_eq!(budget.bound, JobBound::CpuTopology);
        assert!(budget.describe_jobs().contains("/proc/meminfo unavailable"));
    }

    #[test]
    fn test_threads_are_uncapped_until_memory_is_short() {
        let healthy = HostMemory {
            total_mib: 64_203,
            available_mib: 58_423,
        };
        assert_eq!(
            RustJobBudget::for_host(32, 16, Some(healthy)).test_threads(),
            None
        );

        let tight = HostMemory {
            total_mib: 64_203,
            available_mib: 12_000,
        };
        // (12_000 - 9_631) / 256 = 9
        assert_eq!(
            RustJobBudget::for_host(32, 16, Some(tight)).test_threads(),
            Some(9)
        );
    }

    #[test]
    fn test_threads_never_exceed_logical_cpus_and_never_reach_zero() {
        let tight = HostMemory {
            total_mib: 64_203,
            available_mib: 9_700,
        };
        let budget = RustJobBudget::for_host(4, 2, Some(tight));
        assert_eq!(budget.test_threads(), Some(1));
    }

    #[test]
    fn notice_names_the_binding_reason_and_the_arithmetic() {
        let memory = HostMemory {
            total_mib: 64_203,
            available_mib: 30_000,
        };
        let notice = RustJobBudget::for_host(32, 16, Some(memory)).describe_jobs();
        assert!(
            notice.starts_with("Rust build budget: 8 jobs (memory-bound;"),
            "{notice}"
        );
        assert!(notice.contains("cpu allows 28"), "{notice}");
        assert!(notice.contains("30000 MiB MemAvailable"), "{notice}");
        assert!(notice.contains("9631 MiB host reserve"), "{notice}");
        assert!(notice.contains("2500 MiB per job"), "{notice}");
    }
}
