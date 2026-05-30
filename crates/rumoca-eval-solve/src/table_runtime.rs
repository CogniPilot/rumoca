use rumoca_core::ExternalTableData;

const NO_NEXT_TIME_EVENT: f64 = f64::MAX;
const TIME_EVENT_EPS: f64 = 1.0e-12;

#[derive(Debug, Clone, Copy)]
struct TableLookupResult {
    value: f64,
    slope: f64,
}

pub fn try_eval_table_bound_value_in(
    table_id: f64,
    max: bool,
    tables: &[ExternalTableData],
) -> Option<f64> {
    table_x_bounds(lookup_external_table(table_id, tables)?)
        .map(|(min, upper)| if max { upper } else { min })
}

pub fn try_eval_table_lookup_value_in(
    table_id: f64,
    col_arg: f64,
    x: f64,
    tables: &[ExternalTableData],
) -> Option<f64> {
    let table = lookup_external_table(table_id, tables)?;
    let col_idx = checked_table_col_index(table, col_arg)?;
    Some(eval_table_1d_lookup(table, col_idx, x)?.value)
}

pub fn try_eval_table_lookup_slope_value_in(
    table_id: f64,
    col_arg: f64,
    x: f64,
    tables: &[ExternalTableData],
) -> Option<f64> {
    let table = lookup_external_table(table_id, tables)?;
    let col_idx = checked_table_col_index(table, col_arg)?;
    Some(eval_table_1d_lookup(table, col_idx, x)?.slope)
}

pub fn try_eval_time_table_next_event_value_in(
    table_id: f64,
    time_in: f64,
    tables: &[ExternalTableData],
) -> Option<f64> {
    let table = lookup_external_table(table_id, tables)?;
    Some(eval_time_table_next_event(table, time_in))
}

fn lookup_external_table(
    table_id: f64,
    tables: &[ExternalTableData],
) -> Option<&ExternalTableData> {
    let id = external_table_id(table_id)?;
    tables.iter().find(|table| table.id == id)
}

fn external_table_id(table_id: f64) -> Option<u64> {
    if !table_id.is_finite() {
        return None;
    }
    let rounded = table_id.round();
    if (rounded - table_id).abs() > 1.0e-6 || rounded <= 0.0 {
        return None;
    }
    Some(rounded as u64)
}

fn checked_table_col_index(table: &ExternalTableData, col_arg: f64) -> Option<usize> {
    let rounded = col_arg.round();
    let output_count = table_output_count(table);
    if !rounded.is_finite()
        || (rounded - col_arg).abs() > 1.0e-9
        || rounded < 1.0
        || rounded > output_count as f64
    {
        return None;
    }
    Some(rounded as usize - 1)
}

fn table_output_count(table: &ExternalTableData) -> usize {
    if table.columns.is_empty() {
        table
            .data
            .first()
            .map(|row| row.len().saturating_sub(1))
            .unwrap_or(0)
    } else {
        table.columns.len()
    }
}

fn table_x_bounds(table: &ExternalTableData) -> Option<(f64, f64)> {
    let first = table.data.first()?.first().copied()?;
    let last = table.data.last()?.first().copied()?;
    Some((first, last))
}

fn selected_table_column(
    columns: &[usize],
    requested_output_col: usize,
    data_col_count: usize,
) -> usize {
    if data_col_count == 0 {
        return 0;
    }
    if columns.is_empty() {
        return requested_output_col
            .saturating_add(1)
            .min(data_col_count.saturating_sub(1));
    }
    columns
        .get(requested_output_col)
        .copied()
        .or_else(|| columns.last().copied())
        .unwrap_or(1)
        .saturating_sub(1)
        .min(data_col_count.saturating_sub(1))
}

fn extrapolated_x(mut x: f64, x_min: f64, x_max: f64, extrapolation: i64) -> (f64, bool, bool) {
    if !x_min.is_finite() || !x_max.is_finite() || x_min > x_max {
        return (x, false, true);
    }
    if x_min == x_max {
        return (x_min, false, false);
    }
    if x >= x_min && x <= x_max {
        return (x, false, true);
    }
    match extrapolation {
        1 | 4 => (x.clamp(x_min, x_max), true, false),
        2 => (x, true, true),
        3 => {
            let span = x_max - x_min;
            if span > 0.0 {
                let mut wrapped = (x - x_min) % span;
                if wrapped < 0.0 {
                    wrapped += span;
                }
                x = x_min + wrapped;
            } else {
                x = x_min;
            }
            (x, false, true)
        }
        _ => (x.clamp(x_min, x_max), true, false),
    }
}

fn eval_table_1d_lookup(
    table: &ExternalTableData,
    requested_output_col: usize,
    x: f64,
) -> Option<TableLookupResult> {
    let data_col_count = table.data.first().map(Vec::len)?;
    if data_col_count < 2 {
        return None;
    }
    let output_col = selected_table_column(&table.columns, requested_output_col, data_col_count);
    let (x_min, x_max) = table_x_bounds(table)?;
    let (x_real, out_of_range, preserve_slope) =
        extrapolated_x(x, x_min, x_max, table.extrapolation);
    if table.data.len() == 1 {
        return Some(TableLookupResult {
            value: *table.data[0].get(output_col)?,
            slope: 0.0,
        });
    }

    let last_idx = table.data.len() - 1;
    let k = lookup_segment_index(&table.data, x_real)?;
    let next_idx = (k + 1).min(last_idx);
    let x0 = *table.data.get(k)?.first()?;
    let x1 = *table.data.get(next_idx)?.first()?;
    let y0 = *table.data.get(k)?.get(output_col)?;
    let y1 = *table.data.get(next_idx)?.get(output_col)?;

    if table.smoothness == 3 && !out_of_range {
        return Some(TableLookupResult {
            value: if x_real >= x_max {
                *table.data.get(last_idx)?.get(output_col)?
            } else {
                y0
            },
            slope: 0.0,
        });
    }
    if (x1 - x0).abs() <= f64::EPSILON {
        return Some(TableLookupResult {
            value: y0,
            slope: 0.0,
        });
    }

    let slope = (y1 - y0) / (x1 - x0);
    Some(TableLookupResult {
        value: y0 + (x_real - x0) * slope,
        slope: if preserve_slope { slope } else { 0.0 },
    })
}

fn lookup_segment_index(data: &[Vec<f64>], x_real: f64) -> Option<usize> {
    let last_idx = data.len().saturating_sub(1);
    if x_real <= *data.first()?.first()? {
        return Some(0);
    }
    if x_real >= *data.get(last_idx)?.first()? {
        return Some(last_idx.saturating_sub(1));
    }
    let mut idx = 0usize;
    while idx + 1 < data.len() && x_real >= *data.get(idx + 1)?.first()? {
        idx += 1;
    }
    Some(idx.min(last_idx.saturating_sub(1)))
}

fn eval_time_table_next_event(table: &ExternalTableData, time_in: f64) -> f64 {
    let knots: Vec<f64> = table
        .data
        .iter()
        .filter_map(|row| row.first().copied())
        .filter(|x| x.is_finite())
        .collect();
    if knots.is_empty() {
        return NO_NEXT_TIME_EVENT;
    }

    if table.extrapolation == 3
        && let Some(next) = next_periodic_time_event(table, &knots, time_in)
    {
        return next;
    }

    knots
        .into_iter()
        .find(|x| *x > time_in + TIME_EVENT_EPS)
        .unwrap_or(NO_NEXT_TIME_EVENT)
}

fn next_periodic_time_event(table: &ExternalTableData, knots: &[f64], time_in: f64) -> Option<f64> {
    let (x_min, x_max) = table_x_bounds(table)?;
    let span = x_max - x_min;
    if span <= TIME_EVENT_EPS {
        return None;
    }
    let cycle = ((time_in - x_min) / span).floor() as i64;
    ((cycle - 1)..=(cycle + 2))
        .flat_map(|n| {
            let shift = (n as f64) * span;
            knots.iter().copied().map(move |x| x + shift)
        })
        .filter(|candidate| *candidate > time_in + TIME_EVENT_EPS)
        .min_by(|a, b| a.total_cmp(b))
}
