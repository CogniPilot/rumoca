use serde::{Deserialize, Serialize};

/// Host load captured at one point in a wall-time measurement interval.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct HostLoadSnapshot {
    pub one_minute: f64,
    pub logical_cpus: usize,
}

impl HostLoadSnapshot {
    #[must_use]
    pub fn normalized(self) -> f64 {
        self.one_minute / self.logical_cpus as f64
    }
}

/// Metadata needed to judge whether a wall-time comparison is trustworthy.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct WallTimeMeasurementProvenance {
    pub omc_fresh_sample_count: usize,
    pub omc_cached_sample_count: usize,
    pub affinity_requested_worker_count: usize,
    pub affinity_applied_worker_count: usize,
    pub affinity_failed_worker_count: usize,
    pub normalized_load_before: Option<f64>,
    pub normalized_load_after: Option<f64>,
    pub rumoca_workers_used: usize,
    pub workers_used: usize,
    pub omc_threads: usize,
}

fn parse_load_text(text: &str, logical_cpus: usize) -> Option<HostLoadSnapshot> {
    if logical_cpus == 0 {
        return None;
    }
    let one_minute = text
        .trim()
        .trim_start_matches('{')
        .split_whitespace()
        .next()?
        .parse::<f64>()
        .ok()?;
    if !one_minute.is_finite() {
        return None;
    }
    Some(HostLoadSnapshot {
        one_minute,
        logical_cpus,
    })
}

/// Capture the platform one-minute load average without unsafe system calls.
#[must_use]
pub fn sample_host_load() -> Option<HostLoadSnapshot> {
    let logical_cpus = std::thread::available_parallelism().ok()?.get();
    #[cfg(target_os = "linux")]
    let text = std::fs::read_to_string("/proc/loadavg").ok()?;
    #[cfg(target_os = "macos")]
    let text = {
        let output = std::process::Command::new("sysctl")
            .args(["-n", "vm.loadavg"])
            .output()
            .ok()?;
        if !output.status.success() {
            return None;
        }
        String::from_utf8(output.stdout).ok()?
    };
    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    return None;
    parse_load_text(&text, logical_cpus)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_linux_loadavg() {
        let snapshot = parse_load_text("15.0 8.0 4.0 2/100 123", 10).unwrap();
        assert_eq!(snapshot.one_minute, 15.0);
        assert_eq!(snapshot.normalized(), 1.5);
    }

    #[test]
    fn parses_macos_vm_loadavg() {
        let snapshot = parse_load_text("{ 2.50 3.00 4.00 }", 10).unwrap();
        assert_eq!(snapshot.one_minute, 2.5);
        assert_eq!(snapshot.normalized(), 0.25);
    }

    #[test]
    fn rejects_missing_or_non_finite_load() {
        assert!(parse_load_text("unavailable", 10).is_none());
        assert!(parse_load_text("NaN 1 1", 10).is_none());
        assert!(parse_load_text("1 1 1", 0).is_none());
    }
}
