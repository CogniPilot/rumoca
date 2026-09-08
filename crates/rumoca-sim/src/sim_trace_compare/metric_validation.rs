use super::{InitialConditionStats, ModelDeviationMetricWire, THRESHOLD_COMPARE_EPS};
use std::collections::HashSet;

pub(super) fn validate_model_metric(metric: &ModelDeviationMetricWire) -> Result<(), String> {
    if metric.model_name.is_empty() {
        return Err("trace metric model name is empty".to_string());
    }
    let total = validate_metric_population(metric)?;
    validate_metric_scores(metric)?;
    validate_metric_ratios(metric, total)?;
    validate_initial_condition(&metric.initial_condition, total)?;
    validate_worst_channels(metric)?;
    Ok(())
}

fn validate_metric_population(metric: &ModelDeviationMetricWire) -> Result<usize, String> {
    let total = metric.channel_partition.compared().len();
    if total == 0 {
        return Err("successful trace metric compared no channels".to_string());
    }
    let classified = metric
        .channel_high_count
        .checked_add(metric.channel_minor_count)
        .and_then(|count| count.checked_add(metric.channel_deviation_count))
        .ok_or_else(|| "trace metric channel counts overflow".to_string())?;
    if classified != total {
        return Err("trace metric channel counts do not cover the compared partition".to_string());
    }
    if metric.channel_severe_count > metric.channel_deviation_count {
        return Err("trace metric severe count exceeds deviation count".to_string());
    }
    let minimum_samples = total
        .checked_mul(2)
        .ok_or_else(|| "trace metric minimum sample count overflows".to_string())?;
    if metric.samples_compared < minimum_samples {
        return Err("trace metric has fewer than two samples per compared channel".to_string());
    }
    Ok(total)
}

fn validate_metric_scores(metric: &ModelDeviationMetricWire) -> Result<(), String> {
    for (name, value) in [
        (
            "bounded_normalized_l1_score",
            metric.bounded_normalized_l1_score,
        ),
        (
            "mean_channel_bounded_normalized_l1",
            metric.mean_channel_bounded_normalized_l1,
        ),
        (
            "max_channel_bounded_normalized_l1",
            metric.max_channel_bounded_normalized_l1,
        ),
        ("channel_high_percent", metric.channel_high_percent),
        ("channel_minor_percent", metric.channel_minor_percent),
        (
            "channel_deviation_percent",
            metric.channel_deviation_percent,
        ),
        ("channel_severe_percent", metric.channel_severe_percent),
    ] {
        require_unit_interval(name, value)?;
    }
    if metric.mean_channel_bounded_normalized_l1
        > metric.max_channel_bounded_normalized_l1 + THRESHOLD_COMPARE_EPS
        || metric.bounded_normalized_l1_score
            > metric.max_channel_bounded_normalized_l1 + THRESHOLD_COMPARE_EPS
    {
        return Err("trace metric mean or median exceeds its maximum".to_string());
    }
    if !metric.channel_violation_mass.is_finite() || metric.channel_violation_mass < 0.0 {
        return Err("trace metric violation mass is not finite and nonnegative".to_string());
    }
    Ok(())
}

fn validate_metric_ratios(metric: &ModelDeviationMetricWire, total: usize) -> Result<(), String> {
    require_ratio(
        "channel_high_percent",
        metric.channel_high_percent,
        metric.channel_high_count,
        total,
    )?;
    require_ratio(
        "channel_minor_percent",
        metric.channel_minor_percent,
        metric.channel_minor_count,
        total,
    )?;
    require_ratio(
        "channel_deviation_percent",
        metric.channel_deviation_percent,
        metric.channel_deviation_count,
        total,
    )?;
    require_ratio(
        "channel_severe_percent",
        metric.channel_severe_percent,
        metric.channel_severe_count,
        total,
    )
}

fn validate_worst_channels(metric: &ModelDeviationMetricWire) -> Result<(), String> {
    let compared = metric.channel_partition.compared();
    let total = compared.len();
    if metric.worst_variables.len() > total.min(10) {
        return Err("trace metric records too many worst channels".to_string());
    }
    let compared = compared.iter().map(String::as_str).collect::<HashSet<_>>();
    let mut worst_names = HashSet::with_capacity(metric.worst_variables.len());
    for channel in &metric.worst_variables {
        if !compared.contains(channel.name.as_str()) {
            return Err(format!(
                "worst trace channel `{}` is outside the compared partition",
                channel.name
            ));
        }
        if !worst_names.insert(channel.name.as_str()) {
            return Err(format!(
                "worst trace channel `{}` is repeated",
                channel.name
            ));
        }
    }
    Ok(())
}

fn validate_initial_condition(
    stats: &InitialConditionStats,
    compared: usize,
) -> Result<(), String> {
    validate_initial_population(stats, compared)?;
    validate_initial_scalars(stats)?;
    if stats.channels_compared == 0 {
        return validate_empty_initial_metrics(stats);
    }
    validate_initial_rollup(stats)
}

fn validate_initial_population(
    stats: &InitialConditionStats,
    compared: usize,
) -> Result<(), String> {
    if stats
        .channels_compared
        .checked_add(stats.channels_unmeasured)
        != Some(compared)
    {
        return Err(
            "initial-condition measured and unmeasured channels do not cover the compared set"
                .to_string(),
        );
    }
    let classified = stats
        .high_count
        .checked_add(stats.minor_count)
        .and_then(|count| count.checked_add(stats.deviation_count))
        .ok_or_else(|| "initial-condition channel counts overflow".to_string())?;
    if classified != stats.channels_compared || stats.severe_count > stats.deviation_count {
        return Err("initial-condition channel counts are contradictory".to_string());
    }
    Ok(())
}

fn validate_initial_scalars(stats: &InitialConditionStats) -> Result<(), String> {
    for (name, value) in [
        ("initial high percent", stats.high_percent),
        ("initial minor percent", stats.minor_percent),
        ("initial deviation percent", stats.deviation_percent),
        ("initial severe percent", stats.severe_percent),
        (
            "initial mean bounded error",
            stats.mean_channel_bounded_normalized_error,
        ),
        (
            "initial max bounded error",
            stats.max_channel_bounded_normalized_error,
        ),
    ] {
        require_unit_interval(name, value)?;
    }
    for (name, value) in [
        ("initial violation mass", stats.violation_mass_total),
        (
            "initial mean violation mass",
            stats.violation_mass_mean_per_channel,
        ),
    ] {
        if !value.is_finite() || value < 0.0 {
            return Err(format!("{name} is not finite and nonnegative"));
        }
    }
    Ok(())
}

fn validate_empty_initial_metrics(stats: &InitialConditionStats) -> Result<(), String> {
    let values = [
        stats.high_percent,
        stats.minor_percent,
        stats.deviation_percent,
        stats.severe_percent,
        stats.violation_mass_total,
        stats.violation_mass_mean_per_channel,
        stats.mean_channel_bounded_normalized_error,
        stats.max_channel_bounded_normalized_error,
    ];
    if values.into_iter().any(|value| value != 0.0) {
        Err("empty initial-condition evidence carries nonzero metrics".to_string())
    } else {
        Ok(())
    }
}

fn validate_initial_rollup(stats: &InitialConditionStats) -> Result<(), String> {
    let total = stats.channels_compared;
    require_ratio(
        "initial high percent",
        stats.high_percent,
        stats.high_count,
        total,
    )?;
    require_ratio(
        "initial minor percent",
        stats.minor_percent,
        stats.minor_count,
        total,
    )?;
    require_ratio(
        "initial deviation percent",
        stats.deviation_percent,
        stats.deviation_count,
        total,
    )?;
    require_ratio(
        "initial severe percent",
        stats.severe_percent,
        stats.severe_count,
        total,
    )?;
    let expected_mean_mass = stats.violation_mass_total / total as f64;
    if (stats.violation_mass_mean_per_channel - expected_mean_mass).abs() > THRESHOLD_COMPARE_EPS
        || stats.mean_channel_bounded_normalized_error
            > stats.max_channel_bounded_normalized_error + THRESHOLD_COMPARE_EPS
    {
        return Err("initial-condition rollup is internally inconsistent".to_string());
    }
    Ok(())
}

fn require_unit_interval(name: &str, value: f64) -> Result<(), String> {
    if value.is_finite() && (0.0..=1.0).contains(&value) {
        Ok(())
    } else {
        Err(format!("{name} is outside [0, 1]"))
    }
}

fn require_ratio(name: &str, value: f64, count: usize, total: usize) -> Result<(), String> {
    let expected = count as f64 / total as f64;
    if (value - expected).abs() <= THRESHOLD_COMPARE_EPS {
        Ok(())
    } else {
        Err(format!("{name} does not match its channel counts"))
    }
}
