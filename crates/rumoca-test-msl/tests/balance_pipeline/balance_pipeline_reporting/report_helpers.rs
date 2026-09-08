use std::collections::BTreeMap;

pub(super) fn rounded_percent(passed: usize, total: usize) -> f64 {
    if total == 0 {
        0.0
    } else {
        ((passed as f64 / total as f64) * 100.0).round()
    }
}

pub(super) fn percent_cell(passed: usize, total: usize) -> String {
    format!("{:.0}%", rounded_percent(passed, total))
}

pub(super) fn count_map_cell(counts: &BTreeMap<String, usize>) -> String {
    if counts.is_empty() {
        return "-".to_string();
    }
    counts
        .iter()
        .map(|(key, count)| format!("{key}:{count}"))
        .collect::<Vec<_>>()
        .join(", ")
}
