//! Reference-scale model used to normalize per-channel trace deviations.
//!
//! A channel deviation is only meaningful relative to the scale of the quantity
//! being compared. The primary term is the robust reference range (p95 - p05 of
//! the OpenModelica samples): a channel that moves is normalized against how far
//! it moves, exactly as it always has been.
//!
//! The historical implementation floored that range at `1e-12`, which made every
//! *degenerate* reference channel (a grounded current, a disabled actuator, a
//! zero torque) divide a tiny absolute residual by `1e-12` and report a *severe*
//! deviation. Only that degenerate case gets fallbacks here:
//!
//! * the channel's own magnitude (`0.05 * max(|p05|, |p95|)`), so a constant but
//!   non-zero signal is normalized against its own size;
//! * an absolute floor (`1e-6`), so a genuinely all-zero reference channel
//!   normalizes against an absolute tolerance instead of machine epsilon.
//!
//! The fallbacks are deliberately *not* applied to a channel with a usable
//! range. A DC-offset channel (level far above its spread — a Kelvin
//! temperature, an absolute pressure) would otherwise be normalized by 5% of its
//! absolute level instead of its real dynamic range, which silently downgrades
//! genuine deviations. Hiding real errors is a worse failure than the
//! false-severe scores this module exists to remove, so the magnitude term stays
//! confined to channels that have no range to speak of.
//!
//! That confinement was measured against the MSL corpus and holds. 47 compared
//! channels have a usable range that is nonetheless under `1e-4` of their own
//! level, and 35 of them score as deviations; every one is a direction cosine or
//! a unit-vector component sitting at 1.0 with a `1e-5`-scale spread. They are
//! *not* false deviations. In `Elementary.SpringDamperSystem`,
//! `spring1.frame_b.R.T[1,1]` (the cosine of a small rotation: level 1.0, range
//! `6.398e-5`, mean error `2.254e-5`) scores `0.2605`, and its own sibling
//! `R.T[1,2]` (the sine of the *same* rotation: range `1.951e-2`, mean error
//! `5.944e-3`) scores `0.2335`. One physical error, expressed once quadratically
//! and once linearly, and the two verdicts agree to 12% — which is exactly what
//! normalizing by each channel's own range is for. Declaring a
//! small-relative-to-level range degenerate would hand the cosine `0.05 * 1.0`
//! as a scale, score it `4.5e-4` (high agreement), and leave the sine reporting
//! a deviation for the same error. That is not a repair, so it is not done.
//!
//! ## Array-valued quantities
//!
//! A reference channel that is *identically zero* carries neither a range nor a
//! level, so the two fallbacks above degrade to the bare `1e-6` constant — a
//! unit-free number applied to newtons, metres and newton-metres alike. That is
//! the one place in this module where the scale is not measured from anything.
//! It made the metric report the same absolute residual on one vector as high
//! agreement or as near-total disagreement depending only on which component it
//! landed in: for `world.frame_b.f` in `DoublePendulumInitTip`, `f[1]` and
//! `f[2]` are off by 33 N against an 855 N reference range and score `0.037`,
//! while `f[3]` is off by `2.9e-5` N against an identically-zero reference and
//! scores `0.967`.
//!
//! Modelica guarantees that the elements of one array declaration share a type
//! and a unit, so the other components of the same array are the closest
//! measured estimate of the missing scale. An information-free channel therefore
//! normalizes against [`ReferenceScale::array_group_floor`]: the *smallest*
//! usable reference range among the sibling elements of its array. Taking the
//! smallest keeps the substitution conservative — a degenerate component is
//! never granted more tolerance than the least tolerant informative sibling of
//! the same quantity — and the resulting invariant is the one the metric should
//! have had all along: the same absolute error scores the same wherever in an
//! array it lands.
//!
//! Across the corpus this substitutes a measured scale on 976 of the 5379
//! information-free channels, and it is not a near thing: the largest score any
//! of those 976 reaches is `3.6e-5`, and the 39 channels it moves out of the
//! deviation band carry residuals between `1.8e-8` and `7.2e-6` of the floor
//! that replaced their `1e-6`. The verdicts do not depend on the floor being
//! accurate; they would be unchanged if it were four orders of magnitude wrong.
//!
//! Substituting a sibling range can only widen the scale (a contributing
//! sibling is by definition non-degenerate, so the floor always exceeds the
//! `1e-6` it replaces), so no channel scores *worse* under it. That asymmetry is
//! bounded rather than unconditional: the floor is a scale, not an amnesty, and
//! a residual that is large against the least informative sibling of the same
//! quantity still scores as a deviation — `real_error_on_information_free_array_element_is_still_reported`
//! and `array_group_floor_takes_the_smallest_usable_sibling_range` in `tests`
//! pin that down. What the rule cannot do is credit a component whose residual
//! is comparable to the motion its own array shows.
//!
//! The floor is *not* an absolute-residual guard, and no such guard is added.
//! "Physically negligible" is not a property of a number: `2.9e-5` is noise for
//! a force whose siblings carry 855 N and a total disagreement for a direction
//! cosine or a normalized probability. A constant residual threshold has no
//! access to the unit and would have to be tuned; the sibling range carries the
//! unit with it and is measured from the reference trace alone, never from our
//! own output.
//!
//! The corpus makes that concrete. Of the 4403 channels that still fall back to
//! the bare constant, 36 score as deviations, and they split into two groups
//! that no unit-free threshold can separate on principle: 12 with mean errors
//! from `4.7e-3` up to `3.142` (`FixedShapes.CuboidSections.cuboidBottom.arg_H`
//! reports a phase angle inverted by exactly pi; `Rectifier.iQS.phi` is out by
//! `0.314` rad) and 24 with mean errors at or below `3.3e-6`. A guard dropped
//! into the gap between them would be a number read off this corpus, and it
//! would have to rule that `4.7e-3` rad of phase is a real disagreement while
//! `3.3e-6` N.m of torque is not — a judgement that needs the unit, which the
//! trace does not carry. The sibling range is preferred precisely because it
//! does not need to make that judgement.
//!
//! Discrete channels keep their historical `max(range, 1.0)` behaviour: a
//! discrete level difference of one is a full disagreement by construction, and
//! they neither contribute to nor consume an array group floor.

use super::{RANGE_HIGH_QUANTILE, RANGE_LOW_QUANTILE};

/// Fraction of a channel's own magnitude treated as a meaningful deviation.
///
/// Only consulted for degenerate-range channels; see the module docs.
pub(super) const MAGNITUDE_SCALE_FRACTION: f64 = 0.05;

/// Absolute normalization floor for continuous channels.
///
/// This is the module's one unit-free constant, and it is a last resort rather
/// than a physical statement: it applies only where the reference trace offers
/// no measurable scale at all, and it exists because the alternative is
/// dividing by the `1e-12` machine-noise range of an exactly-zero channel and
/// calling every rounding difference a severe deviation. It is *not* evidence
/// that `1e-6` is negligible in any particular unit — nothing in a trace can
/// establish that — which is why the array group floor replaces it wherever a
/// sibling of the same quantity has a range to lend, and why no second
/// unit-free constant is introduced as a residual guard (see the module docs).
///
/// The same value is the threshold below which a reference range counts as
/// degenerate, so a range that survives it is always a range this constant
/// would not have improved on.
pub(super) const CONTINUOUS_ABSOLUTE_SCALE_FLOOR: f64 = 1.0e-6;

/// Minimum normalization scale for discrete channels (one full level).
pub(super) const DISCRETE_SCALE_FLOOR: f64 = 1.0;

/// The scale a single channel's absolute error is normalized against, together
/// with the raw statistics that produced it (kept so triage can explain *why* a
/// channel was normalized the way it was).
#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) struct ReferenceScale {
    pub p05: f64,
    pub p95: f64,
    pub range: f64,
    pub magnitude: f64,
    pub normalization_scale: f64,
    pub use_step_hold: bool,
    /// Smallest usable reference range among the sibling elements of this
    /// channel's array, when the channel is an array element and at least one
    /// sibling has one. Only consulted for information-free channels; see the
    /// module docs.
    pub array_group_floor: Option<f64>,
}

impl ReferenceScale {
    /// True when the reference channel carries no usable signal at all: a flat
    /// line, at zero or at any other level. Used by shape classification to flag
    /// a probable channel mapping error rather than a numeric deviation.
    ///
    /// This is deliberately keyed on the *range* alone. A reference that never
    /// moves cannot explain a large residual by dynamics, whatever level it sits
    /// at, so a flat non-zero reference belongs in the same bucket as a flat
    /// zero one — which is where the pre-normalization implementation put it
    /// (it tested `normalization_scale <= 1e-11`, and the unfloored scale of a
    /// flat channel was its `1e-12` floor regardless of level).
    pub(super) fn reference_is_degenerate(&self) -> bool {
        range_is_degenerate(self.range)
    }
}

/// True when a reference range is too small to normalize against.
///
/// `range` is built from finite percentiles, so it is never NaN here.
pub(super) fn range_is_degenerate(range: f64) -> bool {
    range <= CONTINUOUS_ABSOLUTE_SCALE_FLOOR
}

/// Build the normalization scale for one channel from its reference samples.
///
/// `array_group_floor` is the sibling-derived scale of the array this channel
/// belongs to, if any; see [`ReferenceScale::array_group_floor`].
pub(super) fn reference_scale(
    ref_samples: &[f64],
    use_step_hold: bool,
    array_group_floor: Option<f64>,
) -> ReferenceScale {
    let (p05, p95) = robust_reference_percentiles(ref_samples).unwrap_or((0.0, 0.0));
    let range = (p95 - p05).abs();
    let magnitude = p05.abs().max(p95.abs());
    let normalization_scale = if use_step_hold {
        range.max(DISCRETE_SCALE_FLOOR)
    } else {
        continuous_scale(range, magnitude, array_group_floor)
    };
    ReferenceScale {
        p05,
        p95,
        range,
        magnitude,
        normalization_scale,
        use_step_hold,
        array_group_floor,
    }
}

/// True when neither the reference range nor the reference level says anything
/// about the channel's scale, so the scale would otherwise be the bare
/// unit-free [`CONTINUOUS_ABSOLUTE_SCALE_FLOOR`].
fn reference_is_information_free(range: f64, magnitude: f64) -> bool {
    range_is_degenerate(range)
        && MAGNITUDE_SCALE_FRACTION * magnitude <= CONTINUOUS_ABSOLUTE_SCALE_FLOOR
}

/// Normalization scale of a continuous channel.
///
/// A channel with a usable range is normalized by that range and nothing else —
/// widening the scale of a well-scaled channel can only hide real deviations.
/// Only a degenerate range falls back to the magnitude term, and only a
/// reference that carries no scale at all falls back to the array group floor
/// or, failing that, to the absolute floor.
fn continuous_scale(range: f64, magnitude: f64, array_group_floor: Option<f64>) -> f64 {
    if !range_is_degenerate(range) {
        return range;
    }
    match array_group_floor {
        Some(floor) if reference_is_information_free(range, magnitude) => floor,
        _ => (MAGNITUDE_SCALE_FRACTION * magnitude).max(CONTINUOUS_ABSOLUTE_SCALE_FLOOR),
    }
}

/// The array a channel name is an element of, if the name ends in a literal
/// subscript list (`body.r_0[3]`, `R.T[1,2]`).
///
/// Modelica gives every element of one array declaration the same type and the
/// same unit, which is what makes the sibling elements a usable estimate of an
/// information-free element's scale.
pub(super) fn array_element_base(name: &str) -> Option<&str> {
    let without_close = name.strip_suffix(']')?;
    let open = without_close.rfind('[')?;
    if open == 0 {
        return None;
    }
    let subscripts = &without_close[open + 1..];
    if subscripts.is_empty() {
        return None;
    }
    let all_literal = subscripts.split(',').all(|subscript| {
        let subscript = subscript.trim();
        !subscript.is_empty() && subscript.bytes().all(|byte| byte.is_ascii_digit())
    });
    all_literal.then(|| &name[..open])
}

/// Robust p05/p95 pair of the finite reference samples.
pub(super) fn robust_reference_percentiles(values: &[f64]) -> Option<(f64, f64)> {
    let mut sorted = values
        .iter()
        .copied()
        .filter(|value| value.is_finite())
        .collect::<Vec<_>>();
    if sorted.is_empty() {
        return None;
    }
    sorted.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    Some((
        percentile_sorted(&sorted, RANGE_LOW_QUANTILE),
        percentile_sorted(&sorted, RANGE_HIGH_QUANTILE),
    ))
}

pub(super) fn percentile_sorted(sorted: &[f64], quantile: f64) -> f64 {
    if sorted.is_empty() {
        return 0.0;
    }
    if sorted.len() == 1 {
        return sorted[0];
    }
    let clamped = quantile.clamp(0.0, 1.0);
    let position = clamped * (sorted.len() - 1) as f64;
    let lower_idx = position.floor() as usize;
    let upper_idx = position.ceil() as usize;
    if lower_idx == upper_idx {
        return sorted[lower_idx];
    }
    let weight = position - lower_idx as f64;
    sorted[lower_idx] * (1.0 - weight) + sorted[upper_idx] * weight
}

pub(super) fn sample_range(values: impl Iterator<Item = f64>) -> f64 {
    let (min, max) = values.fold((f64::INFINITY, f64::NEG_INFINITY), |(min, max), value| {
        (min.min(value), max.max(value))
    });
    if min.is_finite() && max.is_finite() {
        max - min
    } else {
        0.0
    }
}
