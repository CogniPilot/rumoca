use crate::{OdeModel, SimError};

const ROOT_BISECTION_ITERS: usize = 64;

pub(crate) fn first_root_crossing_time(
    model: &OdeModel,
    y: &[f64],
    p: &[f64],
    t_start: f64,
    t_end: f64,
    tol: f64,
) -> Result<Option<f64>, SimError> {
    let mut start = vec![0.0; model.root_conditions.len()];
    let mut end = vec![0.0; model.root_conditions.len()];
    model.eval_roots(y, p, t_start, &mut start)?;
    model.eval_roots(y, p, t_end, &mut end)?;

    let mut crossing = None;
    for (a, b) in start.iter().zip(end.iter()) {
        if root_surface_crossed_or_near(*a, *b, tol) {
            let root = bisect_first_root(model, y, p, t_start, t_end, tol)?;
            crossing = Some(crossing.map_or(root, |current| f64::min(current, root)));
        }
    }
    Ok(crossing)
}

fn root_surface_crossed_or_near(a: f64, b: f64, tol: f64) -> bool {
    root_surface_near_zero(a, tol) || root_surface_near_zero(b, tol) || a.signum() != b.signum()
}

fn root_surface_near_zero(value: f64, tol: f64) -> bool {
    value.abs() <= tol
}

fn bisect_first_root(
    model: &OdeModel,
    y: &[f64],
    p: &[f64],
    mut lo: f64,
    mut hi: f64,
    tol: f64,
) -> Result<f64, SimError> {
    let mut lo_roots = vec![0.0; model.root_conditions.len()];
    model.eval_roots(y, p, lo, &mut lo_roots)?;
    for _ in 0..ROOT_BISECTION_ITERS {
        let mid = lo + 0.5 * (hi - lo);
        let mut mid_roots = vec![0.0; model.root_conditions.len()];
        model.eval_roots(y, p, mid, &mut mid_roots)?;
        if lo_roots
            .iter()
            .zip(mid_roots.iter())
            .any(|(a, b)| a.signum() != b.signum() || root_surface_near_zero(*b, tol))
        {
            hi = mid;
        } else {
            lo = mid;
            lo_roots = mid_roots;
        }
    }
    Ok(hi)
}
