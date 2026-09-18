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
    let k = lookup_segment_index(table, x_real)?;
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

    // ContinuousDerivative smoothness (Akima cubic Hermite spline). The Akima
    // construction needs at least three knots to fabricate its boundary
    // secants; ModelicaStandardTables.c falls back to linear segments for a
    // two-row table, so a shorter table keeps the linear path below.
    if table.smoothness == 2 && table.data.len() >= 3 {
        return eval_akima_1d(table, output_col, x_real, k, out_of_range, preserve_slope);
    }

    let slope = (y1 - y0) / (x1 - x0);
    Ok(TableLookupResult {
        value: y0 + (x_real - x0) * slope,
        slope: if preserve_slope { slope } else { 0.0 },
    })
}

/// Evaluate a 1D table with `ContinuousDerivative` smoothness (the Akima cubic
/// Hermite spline), bit-matching ModelicaStandardTables.c `akimaSpline1DInit`
/// together with `CombiTable1D_getValue`/`getDerValue`.
///
/// The knot derivatives are Akima slopes computed from the surrounding secant
/// slopes. Two synthetic secants are fabricated beyond each end (extrapolating
/// the boundary secants with `3*d[2]-2*d[3]`, `2*d[2]-d[3]` on the left and the
/// mirror pair on the right) so the first and last knot derivatives are
/// defined. When the two secant differences around a knot both vanish, the
/// slope is their unweighted average, matching the exact divide-by-zero guard
/// in the C runtime. Inside an interval the value is the cubic Hermite
/// polynomial of the two knot values and the two Akima knot slopes, and the
/// returned slope is that cubic's analytic derivative so the solver Jacobian
/// stays consistent with the value.
///
/// Out-of-range abscissae follow the same rules as the C runtime: under
/// `LastTwoPoints` the boundary tangent is continued (a line through the
/// boundary knot with the Akima boundary slope), and under a holding mode the
/// boundary knot value is held with a zero slope.
fn eval_akima_1d(
    table: &ExternalTableData,
    output_col: usize,
    x: f64,
    segment: usize,
    out_of_range: bool,
    preserve_slope: bool,
) -> Result<TableLookupResult, TableRuntimeError> {
    let n = table.data.len();
    let mut xs = Vec::with_capacity(n);
    let mut ys = Vec::with_capacity(n);
    for row in 0..n {
        xs.push(table_row_x(table, row)?);
        ys.push(table_row_value(table, row, output_col)?);
    }
    let slopes = akima_knot_slopes(&xs, &ys);
    let x_min = xs[0];
    let x_max = xs[n - 1];

    // LastTwoPoints extrapolation continues the boundary tangent: a line through
    // the boundary knot whose gradient is the Akima boundary slope, exactly as
    // `CombiTable1D_getValue` does for AKIMA_C1 under LAST_TWO_POINTS. `x` here
    // is the untransformed abscissa, so a strictly outside value is either below
    // the first knot (LEFT) or above the last knot (RIGHT).
    if out_of_range && preserve_slope {
        if x < x_min {
            let (_, _, c2) = akima_interval_coeffs(&xs, &ys, &slopes, 0);
            return Ok(TableLookupResult {
                value: ys[0] + c2 * (x - x_min),
                slope: c2,
            });
        }
        let last = n - 2;
        let (c0, c1, c2) = akima_interval_coeffs(&xs, &ys, &slopes, last);
        let v = x_max - xs[last];
        let slope = (3.0 * c0 * v + 2.0 * c1) * v + c2;
        return Ok(TableLookupResult {
            value: ys[n - 1] + slope * (x - x_max),
            slope,
        });
    }

    // In-table, or an abscissa a holding mode has already clamped to a boundary
    // knot: evaluate the interval cubic in local coordinates. A held abscissa
    // reports a zero slope (`preserve_slope` is false) while its clamped value
    // still resolves to the boundary knot through the cubic.
    let (c0, c1, c2) = akima_interval_coeffs(&xs, &ys, &slopes, segment);
    let v = x - xs[segment];
    let value = ys[segment] + ((c0 * v + c1) * v + c2) * v;
    let slope = if preserve_slope {
        (3.0 * c0 * v + 2.0 * c1) * v + c2
    } else {
        0.0
    };
    Ok(TableLookupResult { value, slope })
}

/// The Akima first-order derivative at every knot of one table column.
///
/// Mirrors the divided-difference array of `akimaSpline1DInit`: the interior
/// entries hold the interval secant slopes and four fabricated entries
/// extrapolate them past each boundary, so the weighted Akima average is
/// defined at the first and last knots.
fn akima_knot_slopes(xs: &[f64], ys: &[f64]) -> Vec<f64> {
    let n = xs.len();
    // The caller guarantees at least three knots, so every fabricated boundary
    // secant references a genuine interior secant.
    let mut d = vec![0.0_f64; n + 3];
    for i in 0..n - 1 {
        d[i + 2] = (ys[i + 1] - ys[i]) / (xs[i + 1] - xs[i]);
    }
    d[0] = 3.0 * d[2] - 2.0 * d[3];
    d[1] = 2.0 * d[2] - d[3];
    d[n + 1] = 2.0 * d[n] - d[n - 1];
    d[n + 2] = 3.0 * d[n] - 2.0 * d[n - 1];

    let mut slopes = Vec::with_capacity(n);
    for j in 0..n {
        let right = (d[j + 3] - d[j + 2]).abs();
        let left = (d[j + 1] - d[j]).abs();
        let denom = right + left;
        let slope = if denom > 0.0 {
            let a = left / denom;
            (1.0 - a) * d[j + 1] + a * d[j + 2]
        } else {
            0.5 * d[j + 1] + 0.5 * d[j + 2]
        };
        slopes.push(slope);
    }
    slopes
}

/// The cubic Hermite coefficients `(c0, c1, c2)` of the interval starting at
/// knot `i`, expressed in the local coordinate `v = x - xs[i]` so that
/// `value = ys[i] + ((c0*v + c1)*v + c2)*v`. This reproduces the coefficient
/// arithmetic of `akimaSpline1DInit` exactly.
fn akima_interval_coeffs(xs: &[f64], ys: &[f64], slopes: &[f64], i: usize) -> (f64, f64, f64) {
    let dx = xs[i + 1] - xs[i];
    let secant = (ys[i + 1] - ys[i]) / dx;
    let c2 = slopes[i];
    let c2_next = slopes[i + 1];
    let c1 = (3.0 * secant - 2.0 * c2 - c2_next) / dx;
    let c0 = (c2 + c2_next - 2.0 * secant) / (dx * dx);
    (c0, c1, c2)
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
    if x_real <= table_row_x(table, 0)? {
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

#[cfg(test)]
mod tests {
    use super::{ExternalTableData, eval_table_lookup_slope_value_in, eval_table_lookup_value_in};

    /// A single-column 1D table with the given knots and the given smoothness
    /// and extrapolation runtime codes.
    fn table_1d(
        knots: &[(f64, f64)],
        smoothness: i64,
        extrapolation: i64,
    ) -> Vec<ExternalTableData> {
        vec![ExternalTableData {
            id: 1,
            data: knots.iter().map(|&(x, y)| vec![x, y]).collect(),
            columns: vec![2],
            smoothness,
            extrapolation,
        }]
    }

    fn value_at(tables: &[ExternalTableData], x: f64) -> f64 {
        eval_table_lookup_value_in(1.0, 1.0, x, tables).expect("table lookup")
    }

    fn slope_at(tables: &[ExternalTableData], x: f64) -> f64 {
        eval_table_lookup_slope_value_in(1.0, 1.0, x, tables).expect("table slope")
    }

    const AKIMA: i64 = 2;
    const LAST_TWO_POINTS: i64 = 2;
    const HOLD_LAST_POINT: i64 = 1;

    #[test]
    fn akima_reproduces_a_quadratic_exactly() {
        // On an evenly spaced sample of y = x^2, the fabricated boundary
        // secants make every Akima knot slope equal 2*x, so the interpolant is
        // the exact quadratic (its cubic term vanishes).
        let tables = table_1d(
            &[(0.0, 0.0), (1.0, 1.0), (2.0, 4.0), (3.0, 9.0), (4.0, 16.0)],
            AKIMA,
            LAST_TWO_POINTS,
        );
        assert!((value_at(&tables, 0.5) - 0.25).abs() < 1e-12);
        assert!((slope_at(&tables, 0.5) - 1.0).abs() < 1e-12);
        assert!((value_at(&tables, 2.5) - 6.25).abs() < 1e-12);
        assert!((slope_at(&tables, 2.5) - 5.0).abs() < 1e-12);
        // The knots are reproduced with a slope matching the exact derivative.
        assert!((value_at(&tables, 3.0) - 9.0).abs() < 1e-12);
        assert!((slope_at(&tables, 3.0) - 6.0).abs() < 1e-12);
    }

    #[test]
    fn akima_evaluates_a_genuine_cubic_interval() {
        // An oscillating table forces non-zero cubic terms. The hand-computed
        // Akima knot slopes are [2, 0, 0, 2]; interval 1 has coefficients
        // c0 = 2, c1 = -3, c2 = 0 in the local coordinate, so the midpoint
        // value is 0.5 and its slope is -1.5.
        let tables = table_1d(
            &[(0.0, 0.0), (1.0, 1.0), (2.0, 0.0), (3.0, 1.0)],
            AKIMA,
            LAST_TWO_POINTS,
        );
        assert!((value_at(&tables, 1.5) - 0.5).abs() < 1e-12);
        assert!((slope_at(&tables, 1.5) - (-1.5)).abs() < 1e-12);
        // The first interval overshoots the linear chord (0.75 > 0.5),
        // demonstrating the continuous-derivative shape.
        assert!((value_at(&tables, 0.5) - 0.75).abs() < 1e-12);
    }

    #[test]
    fn akima_last_two_points_continues_the_boundary_tangent() {
        // Below the first knot the boundary tangent (slope = Akima slope at the
        // first knot = 2) is continued; above the last knot the tangent uses
        // the right-boundary cubic derivative (also 2 here).
        let tables = table_1d(
            &[(0.0, 0.0), (1.0, 1.0), (2.0, 0.0), (3.0, 1.0)],
            AKIMA,
            LAST_TWO_POINTS,
        );
        assert!((value_at(&tables, -0.5) - (-1.0)).abs() < 1e-12);
        assert!((slope_at(&tables, -0.5) - 2.0).abs() < 1e-12);
        assert!((value_at(&tables, 3.5) - 2.0).abs() < 1e-12);
        assert!((slope_at(&tables, 3.5) - 2.0).abs() < 1e-12);
    }

    #[test]
    fn akima_hold_last_point_holds_the_boundary_knot() {
        // A holding extrapolation clamps to the boundary knot value with a zero
        // reported slope, independent of the Akima boundary tangent.
        let tables = table_1d(
            &[(0.0, 0.0), (1.0, 1.0), (2.0, 0.0), (3.0, 1.0)],
            AKIMA,
            HOLD_LAST_POINT,
        );
        assert!((value_at(&tables, -0.5) - 0.0).abs() < 1e-12);
        assert!(slope_at(&tables, -0.5).abs() < 1e-12);
        assert!((value_at(&tables, 3.5) - 1.0).abs() < 1e-12);
        assert!(slope_at(&tables, 3.5).abs() < 1e-12);
    }

    #[test]
    fn akima_falls_back_to_linear_for_two_row_tables() {
        // With only two knots the Akima construction cannot fabricate its
        // boundary secants, so a two-row table interpolates linearly, matching
        // the ModelicaStandardTables.c fallback.
        let tables = table_1d(&[(0.0, 0.0), (2.0, 4.0)], AKIMA, LAST_TWO_POINTS);
        assert!((value_at(&tables, 1.0) - 2.0).abs() < 1e-12);
        assert!((slope_at(&tables, 1.0) - 2.0).abs() < 1e-12);
    }
}
