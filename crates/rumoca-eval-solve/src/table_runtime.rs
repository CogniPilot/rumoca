use rumoca_core::ExternalTableData;

const NO_NEXT_TIME_EVENT: f64 = f64::MAX;
const TIME_EVENT_EPS: f64 = 1.0e-12;
const U64_EXCLUSIVE_MAX_AS_F64: f64 = 18_446_744_073_709_551_616.0;
const I64_MIN_AS_F64: f64 = -9_223_372_036_854_775_808.0;
const I64_EXCLUSIVE_MAX_AS_F64: f64 = 9_223_372_036_854_775_808.0;

#[derive(Debug, Clone, PartialEq)]
pub enum TableRuntimeError {
    InvalidTableId {
        value: f64,
    },
    TableNotFound {
        id: u64,
    },
    InvalidColumn {
        value: f64,
        output_count: usize,
    },
    MissingBounds {
        table_id: u64,
    },
    InvalidColumnMetadata {
        table_id: u64,
        requested_output_col: usize,
        data_col_count: usize,
    },
    InvalidDataShape {
        table_id: u64,
        reason: &'static str,
    },
    PeriodicEventCycleOutOfRange {
        table_id: u64,
        time: f64,
    },
}

impl std::fmt::Display for TableRuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidTableId { value } => write!(
                f,
                "invalid external table id {value}; expected a finite positive integer id"
            ),
            Self::TableNotFound { id } => {
                write!(f, "external table id {id} was not provided")
            }
            Self::InvalidColumn {
                value,
                output_count,
            } => write!(
                f,
                "invalid external table column {value}; expected an integer in 1..={output_count}"
            ),
            Self::MissingBounds { table_id } => {
                write!(f, "external table id {table_id} has no x bounds")
            }
            Self::InvalidColumnMetadata {
                table_id,
                requested_output_col,
                data_col_count,
            } => {
                if let Some(column) = requested_output_col.checked_add(1) {
                    write!(
                        f,
                        "external table id {table_id} maps output column {column} outside {data_col_count} data columns"
                    )
                } else {
                    write!(
                        f,
                        "external table id {table_id} maps output column index {requested_output_col} outside {data_col_count} data columns"
                    )
                }
            }
            Self::InvalidDataShape { table_id, reason } => {
                write!(
                    f,
                    "external table id {table_id} has invalid data shape: {reason}"
                )
            }
            Self::PeriodicEventCycleOutOfRange { table_id, time } => write!(
                f,
                "external table id {table_id} periodic event cycle is out of range at time {time}"
            ),
        }
    }
}

impl std::error::Error for TableRuntimeError {}

#[derive(Debug, Clone, Copy)]
struct TableLookupResult {
    value: f64,
    slope: f64,
}

pub fn eval_table_bound_value_in(
    table_id: f64,
    max: bool,
    tables: &[ExternalTableData],
) -> Result<f64, TableRuntimeError> {
    table_x_bounds(lookup_external_table(table_id, tables)?)
        .map(|(min, upper)| if max { upper } else { min })
}

pub fn eval_table_lookup_value_in(
    table_id: f64,
    col_arg: f64,
    x: f64,
    tables: &[ExternalTableData],
) -> Result<f64, TableRuntimeError> {
    let table = lookup_external_table(table_id, tables)?;
    let col_idx = table_col_index(table, col_arg)?;
    Ok(eval_table_1d_lookup(table, col_idx, x)?.value)
}

pub fn eval_table_lookup_slope_value_in(
    table_id: f64,
    col_arg: f64,
    x: f64,
    tables: &[ExternalTableData],
) -> Result<f64, TableRuntimeError> {
    let table = lookup_external_table(table_id, tables)?;
    let col_idx = table_col_index(table, col_arg)?;
    Ok(eval_table_1d_lookup(table, col_idx, x)?.slope)
}

pub fn eval_time_table_next_event_value_in(
    table_id: f64,
    time_in: f64,
    tables: &[ExternalTableData],
) -> Result<f64, TableRuntimeError> {
    let table = lookup_external_table(table_id, tables)?;
    eval_time_table_next_event(table, time_in)
}

fn lookup_external_table(
    table_id: f64,
    tables: &[ExternalTableData],
) -> Result<&ExternalTableData, TableRuntimeError> {
    let id = external_table_id(table_id)?;
    tables
        .iter()
        .find(|table| table.id == id)
        .ok_or(TableRuntimeError::TableNotFound { id })
}

fn external_table_id(table_id: f64) -> Result<u64, TableRuntimeError> {
    if !table_id.is_finite() {
        return Err(TableRuntimeError::InvalidTableId { value: table_id });
    }
    let rounded = table_id.round();
    if (rounded - table_id).abs() > 1.0e-6 || rounded <= 0.0 || rounded >= U64_EXCLUSIVE_MAX_AS_F64
    {
        return Err(TableRuntimeError::InvalidTableId { value: table_id });
    }
    Ok(rounded as u64)
}

fn table_col_index(table: &ExternalTableData, col_arg: f64) -> Result<usize, TableRuntimeError> {
    let rounded = col_arg.round();
    let output_count = table_output_count(table);
    if !rounded.is_finite()
        || (rounded - col_arg).abs() > 1.0e-9
        || rounded < 1.0
        || rounded > output_count as f64
    {
        return Err(TableRuntimeError::InvalidColumn {
            value: col_arg,
            output_count,
        });
    }
    Ok(rounded as usize - 1)
}

fn table_output_count(table: &ExternalTableData) -> usize {
    if table.columns.is_empty() {
        match table.data.first() {
            Some(row) => match row.len() {
                0 => 0,
                len => len - 1,
            },
            None => 0,
        }
    } else {
        table.columns.len()
    }
}

fn table_x_bounds(table: &ExternalTableData) -> Result<(f64, f64), TableRuntimeError> {
    let first = table
        .data
        .first()
        .and_then(|row| row.first())
        .copied()
        .ok_or(TableRuntimeError::MissingBounds { table_id: table.id })?;
    let last = table
        .data
        .last()
        .and_then(|row| row.first())
        .copied()
        .ok_or(TableRuntimeError::MissingBounds { table_id: table.id })?;
    Ok((first, last))
}

fn selected_table_column(
    columns: &[usize],
    table_id: u64,
    requested_output_col: usize,
    data_col_count: usize,
) -> Result<usize, TableRuntimeError> {
    if data_col_count == 0 {
        return Err(TableRuntimeError::InvalidColumnMetadata {
            table_id,
            requested_output_col,
            data_col_count,
        });
    }
    if columns.is_empty() {
        let Some(data_col) = requested_output_col.checked_add(1) else {
            return Err(TableRuntimeError::InvalidColumnMetadata {
                table_id,
                requested_output_col,
                data_col_count,
            });
        };
        if data_col < data_col_count {
            return Ok(data_col);
        }
        return Err(TableRuntimeError::InvalidColumnMetadata {
            table_id,
            requested_output_col,
            data_col_count,
        });
    }
    let data_col = columns
        .get(requested_output_col)
        .copied()
        .and_then(|column| column.checked_sub(1))
        .filter(|data_col| *data_col < data_col_count);
    data_col.ok_or(TableRuntimeError::InvalidColumnMetadata {
        table_id,
        requested_output_col,
        data_col_count,
    })
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
) -> Result<TableLookupResult, TableRuntimeError> {
    let data_col_count =
        table
            .data
            .first()
            .map(Vec::len)
            .ok_or(TableRuntimeError::InvalidDataShape {
                table_id: table.id,
                reason: "table data is empty",
            })?;
    if data_col_count < 2 {
        return Err(TableRuntimeError::InvalidDataShape {
            table_id: table.id,
            reason: "first row must contain x and at least one output column",
        });
    }
    let output_col = selected_table_column(
        &table.columns,
        table.id,
        requested_output_col,
        data_col_count,
    )?;
    let (x_min, x_max) = table_x_bounds(table)?;
    let (x_real, out_of_range, preserve_slope) =
        extrapolated_x(x, x_min, x_max, table.extrapolation);
    if table.data.len() == 1 {
        return Ok(TableLookupResult {
            value: table_row_value(table, 0, output_col)?,
            slope: 0.0,
        });
    }

    let last_idx = table.data.len() - 1;
    let k = if out_of_range && x < x_min {
        0
    } else {
        lookup_segment_index(table, x_real)?
    };
    let next_idx = (k + 1).min(last_idx);
    let x0 = table_row_x(table, k)?;
    let x1 = table_row_x(table, next_idx)?;
    let y0 = table_row_value(table, k, output_col)?;
    let y1 = table_row_value(table, next_idx, output_col)?;

    if table.smoothness == 3 && !out_of_range {
        return Ok(TableLookupResult {
            value: if x_real >= x_max {
                table_row_value(table, last_idx, output_col)?
            } else {
                y0
            },
            slope: 0.0,
        });
    }
    if (x1 - x0).abs() <= f64::EPSILON {
        return Ok(TableLookupResult {
            value: y0,
            slope: 0.0,
        });
    }

    let slope = (y1 - y0) / (x1 - x0);
    Ok(TableLookupResult {
        value: y0 + (x_real - x0) * slope,
        slope: if preserve_slope { slope } else { 0.0 },
    })
}

fn lookup_segment_index(
    table: &ExternalTableData,
    x_real: f64,
) -> Result<usize, TableRuntimeError> {
    let last_idx = table
        .data
        .len()
        .checked_sub(1)
        .ok_or(TableRuntimeError::InvalidDataShape {
            table_id: table.id,
            reason: "table data is empty",
        })?;
    let last_segment_idx = last_idx
        .checked_sub(1)
        .ok_or(TableRuntimeError::InvalidDataShape {
            table_id: table.id,
            reason: "table requires at least two rows for segment lookup",
        })?;
    if x_real < table_row_x(table, 0)? {
        return Ok(0);
    }
    if x_real >= table_row_x(table, last_idx)? {
        return Ok(last_segment_idx);
    }
    let mut idx = 0usize;
    while idx + 1 < table.data.len() && x_real >= table_row_x(table, idx + 1)? {
        idx += 1;
    }
    Ok(idx.min(last_segment_idx))
}

fn eval_time_table_next_event(
    table: &ExternalTableData,
    time_in: f64,
) -> Result<f64, TableRuntimeError> {
    let knots: Vec<f64> = table
        .data
        .iter()
        .filter_map(|row| row.first().copied())
        .filter(|x| x.is_finite())
        .collect();
    if knots.is_empty() {
        return Ok(NO_NEXT_TIME_EVENT);
    }

    if table.extrapolation == 3
        && let Some(next) = next_periodic_time_event(table, &knots, time_in)?
    {
        return Ok(next);
    }

    Ok(knots
        .into_iter()
        .find(|x| *x > time_in + TIME_EVENT_EPS)
        .unwrap_or(NO_NEXT_TIME_EVENT))
}

fn next_periodic_time_event(
    table: &ExternalTableData,
    knots: &[f64],
    time_in: f64,
) -> Result<Option<f64>, TableRuntimeError> {
    let (x_min, x_max) = table_x_bounds(table)?;
    let span = x_max - x_min;
    if span <= TIME_EVENT_EPS {
        return Ok(None);
    }
    let cycle = finite_floor_to_i64((time_in - x_min) / span, table.id, time_in)?;
    let start = cycle
        .checked_sub(1)
        .ok_or(TableRuntimeError::PeriodicEventCycleOutOfRange {
            table_id: table.id,
            time: time_in,
        })?;
    let end = cycle
        .checked_add(2)
        .ok_or(TableRuntimeError::PeriodicEventCycleOutOfRange {
            table_id: table.id,
            time: time_in,
        })?;
    Ok((start..=end)
        .flat_map(|n| {
            let shift = (n as f64) * span;
            knots.iter().copied().map(move |x| x + shift)
        })
        .filter(|candidate| *candidate > time_in + TIME_EVENT_EPS)
        .min_by(|a, b| a.total_cmp(b)))
}

fn table_row_x(table: &ExternalTableData, row_idx: usize) -> Result<f64, TableRuntimeError> {
    table
        .data
        .get(row_idx)
        .and_then(|row| row.first())
        .copied()
        .ok_or(TableRuntimeError::InvalidDataShape {
            table_id: table.id,
            reason: "row is missing the x column",
        })
}

fn table_row_value(
    table: &ExternalTableData,
    row_idx: usize,
    col_idx: usize,
) -> Result<f64, TableRuntimeError> {
    table
        .data
        .get(row_idx)
        .and_then(|row| row.get(col_idx))
        .copied()
        .ok_or(TableRuntimeError::InvalidDataShape {
            table_id: table.id,
            reason: "row is missing the selected output column",
        })
}

fn finite_floor_to_i64(value: f64, table_id: u64, time: f64) -> Result<i64, TableRuntimeError> {
    let floored = value.floor();
    if !floored.is_finite() || !(I64_MIN_AS_F64..I64_EXCLUSIVE_MAX_AS_F64).contains(&floored) {
        return Err(TableRuntimeError::PeriodicEventCycleOutOfRange { table_id, time });
    }
    Ok(floored as i64)
}
