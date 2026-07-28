//! Host-aware resource budgets inherited by repository-tool subprocesses.

use std::process::Command;
use std::sync::Once;

const CARGO_BUILD_JOBS: &str = "CARGO_BUILD_JOBS";
const MAX_RESERVED_PHYSICAL_CORES: usize = 2;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RustJobBudget {
    logical_cpus: usize,
    physical_cores: usize,
    reserved_physical_cores: usize,
    jobs: usize,
}

impl RustJobBudget {
    fn detect() -> Self {
        let logical_cpus = std::thread::available_parallelism()
            .map(usize::from)
            .unwrap_or(1);
        let physical_cores = num_cpus::get_physical().clamp(1, logical_cpus);
        Self::for_topology(logical_cpus, physical_cores)
    }

    fn for_topology(logical_cpus: usize, physical_cores: usize) -> Self {
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
        let jobs = logical_cpus.saturating_sub(reserved_logical_cpus).max(1);
        Self {
            logical_cpus,
            physical_cores,
            reserved_physical_cores,
            jobs,
        }
    }
}

/// Apply the automatic Cargo budget to a child process and everything it
/// launches. An explicit Cargo override remains authoritative.
pub(crate) fn apply_to_child(command: &mut Command) {
    if std::env::var_os(CARGO_BUILD_JOBS).is_some() {
        return;
    }
    let budget = RustJobBudget::detect();
    command.env(CARGO_BUILD_JOBS, budget.jobs.to_string());
    static NOTICE: Once = Once::new();
    NOTICE.call_once(|| {
        eprintln!(
            "Rust build budget: {} jobs on {} logical CPUs / {} physical cores (reserving {} physical cores; override with {CARGO_BUILD_JOBS})",
            budget.jobs,
            budget.logical_cpus,
            budget.physical_cores,
            budget.reserved_physical_cores,
        );
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn small_hosts_keep_all_logical_cpus() {
        assert_eq!(RustJobBudget::for_topology(1, 1).jobs, 1);
        assert_eq!(RustJobBudget::for_topology(3, 2).jobs, 3);
    }

    #[test]
    fn medium_hosts_reserve_one_physical_core() {
        let budget = RustJobBudget::for_topology(4, 2);
        assert_eq!(budget.reserved_physical_cores, 1);
        assert_eq!(budget.jobs, 2);

        let budget = RustJobBudget::for_topology(7, 4);
        assert_eq!(budget.reserved_physical_cores, 1);
        assert_eq!(budget.jobs, 5);
    }

    #[test]
    fn large_hosts_reserve_at_most_two_physical_cores() {
        let budget = RustJobBudget::for_topology(8, 4);
        assert_eq!(budget.reserved_physical_cores, 2);
        assert_eq!(budget.jobs, 4);

        let budget = RustJobBudget::for_topology(32, 16);
        assert_eq!(budget.reserved_physical_cores, 2);
        assert_eq!(budget.jobs, 28);

        let budget = RustJobBudget::for_topology(128, 64);
        assert_eq!(budget.reserved_physical_cores, 2);
        assert_eq!(budget.jobs, 124);
    }

    #[test]
    fn budget_always_leaves_one_physical_core_for_building() {
        let budget = RustJobBudget::for_topology(8, 1);
        assert_eq!(budget.reserved_physical_cores, 0);
        assert_eq!(budget.jobs, 8);
    }
}
