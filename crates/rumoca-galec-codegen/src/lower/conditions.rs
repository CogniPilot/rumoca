//! The canonical condition surface (`f_c`) as the lowering slice sees it.
//!
//! MLS B.1d gives every event-generating relation a slot in one generated
//! Boolean condition vector (e.g. `c[1]..c[4]`), and the sample tick of the
//! single block clock is itself a condition: its `f_c` right-hand side is
//! the internal `__rumoca_sample` call (see `rumoca_core::
//! INTERNAL_SAMPLE_FUNCTION_NAME`). This table indexes the `f_c` equations
//! so that:
//!
//! - the guard unwrapper can recognize the sample-tick when-edge
//!   `c[i] and not __pre__.c[i]` structurally (GAL-016: metadata, never
//!   value/name heuristics);
//! - condition references in update bodies (`c[3]`, `c[4]` for limiter
//!   relations) inline back to their defining Boolean expressions;
//! - the clock wiring can read the sample call's period argument.

use rumoca_core::{
    Expression, Subscript, component_path_trailing_index, expressions_semantically_equal,
    pre_slot_name,
};
use rumoca_ir_dae::Dae;

use crate::admissibility::AdmittedClock;
use crate::classify::Classification;
use crate::diagnostic::GalecTargetError;
use crate::manifest_vars::const_eval::ConstEnv;

/// One `f_c` slot: `c[index] := rhs`.
pub(crate) struct ConditionEntry<'a> {
    /// 1-based slot index within the condition vector.
    pub index: usize,
    /// Defining Boolean expression (a relation, or the sample call).
    pub rhs: &'a Expression,
    /// True when `rhs` is the clock sample-tick call.
    pub is_sample: bool,
}

/// Indexed view of the canonical `f_c` partition.
#[derive(Default)]
pub(crate) struct ConditionTable<'a> {
    /// Base name of the condition vector (`c` unless renamed), when any
    /// condition equations exist.
    pub base_name: Option<String>,
    pub entries: Vec<ConditionEntry<'a>>,
}

impl<'a> ConditionTable<'a> {
    /// Index the untouched DAE's `f_c` partition.
    pub(crate) fn build(dae: &'a Dae) -> Result<Self, GalecTargetError> {
        let mut table = Self::default();
        for equation in &dae.conditions.equations {
            let Some(lhs) = &equation.lhs else {
                return Err(GalecTargetError::LoweringInternal {
                    detail: "canonical f_c equation without an lhs target".to_owned(),
                });
            };
            let Some((base, index)) = component_path_trailing_index(lhs.as_str()) else {
                return Err(GalecTargetError::LoweringInternal {
                    detail: format!(
                        "canonical f_c target `{}` is not of the `c[i]` form",
                        lhs.as_str()
                    ),
                });
            };
            match &table.base_name {
                Some(existing) if existing != &base => {
                    return Err(GalecTargetError::LoweringInternal {
                        detail: format!(
                            "f_c targets use multiple condition vectors (`{existing}`, `{base}`)"
                        ),
                    });
                }
                Some(_) => {}
                None => table.base_name = Some(base),
            }
            table.entries.push(ConditionEntry {
                index,
                rhs: &equation.rhs,
                is_sample: is_sample_call(&equation.rhs),
            });
        }
        Ok(table)
    }

    /// The entry defining `c[index]`.
    pub(crate) fn entry(&self, index: usize) -> Option<&ConditionEntry<'a>> {
        self.entries.iter().find(|entry| entry.index == index)
    }

    /// Indices of all condition slots that define the admitted sample tick.
    /// Multiple slots are allowed only when their sample timing matches the
    /// single admitted clock (e.g. a parent uses `dt` and an inlined component
    /// uses a bound `pid.samplePeriod`). Distinct timings mean multiple rates.
    pub(crate) fn sample_indices(
        &self,
        classification: &Classification<'a>,
        clock: &AdmittedClock,
    ) -> Result<Vec<usize>, GalecTargetError> {
        let samples = self
            .entries
            .iter()
            .filter(|entry| entry.is_sample)
            .collect::<Vec<_>>();
        let env = ConstEnv::from_classification(classification);
        if !same_sample_clock(&samples, &env, clock) {
            return Err(GalecTargetError::UnsupportedFeature {
                feature: "multi-rate".to_owned(),
                detail: "model has more than one sample-tick condition \
                         (multi-rate discrete models)"
                    .to_owned(),
                span: None,
            });
        }
        Ok(samples.into_iter().map(|entry| entry.index).collect())
    }

    /// The period argument expression of the sample-tick condition
    /// (`sample(start, period)` — argument 2).
    pub(crate) fn sample_period_expr(&self) -> Option<&'a Expression> {
        let entry = self.entries.iter().find(|entry| entry.is_sample)?;
        sample_timing_args(entry.rhs).map(|(_, interval)| interval)
    }
}

fn same_sample_clock(
    samples: &[&ConditionEntry<'_>],
    env: &ConstEnv<'_>,
    clock: &AdmittedClock,
) -> bool {
    if samples
        .iter()
        .all(|entry| sample_matches_admitted_clock(entry.rhs, env, clock))
    {
        return true;
    }
    let Some(first) = samples.first() else {
        return true;
    };
    samples
        .iter()
        .skip(1)
        .all(|entry| expressions_semantically_equal(first.rhs, entry.rhs))
}

fn sample_matches_admitted_clock(
    expression: &Expression,
    env: &ConstEnv<'_>,
    clock: &AdmittedClock,
) -> bool {
    let Some((start, interval)) = sample_timing_args(expression) else {
        return false;
    };
    let Ok(start) = env.evaluate_scalar(start) else {
        return false;
    };
    let Ok(start) = start.as_real() else {
        return false;
    };
    let Ok(interval) = env.evaluate_scalar(interval) else {
        return false;
    };
    let Ok(interval) = interval.as_real() else {
        return false;
    };
    same_time(start, clock.phase_seconds) && same_time(interval, clock.period_seconds)
}

fn sample_timing_args(expression: &Expression) -> Option<(&Expression, &Expression)> {
    let args = match expression {
        Expression::FunctionCall { args, .. } | Expression::BuiltinCall { args, .. } => args,
        _ => return None,
    };
    match args.as_slice() {
        [start, interval] => Some((start, interval)),
        [_id, start, interval] => Some((start, interval)),
        _ => None,
    }
}

fn same_time(lhs: f64, rhs: f64) -> bool {
    lhs.is_finite() && rhs.is_finite() && (lhs - rhs).abs() <= 1.0e-12
}

/// True when `expr` is the lowered clock sample-tick call (brief fact 6:
/// `sample(0.0, period)` becomes `FunctionCall{__rumoca_sample}`; the
/// pre-lowering builtin form is accepted defensively).
fn is_sample_call(expr: &Expression) -> bool {
    match expr {
        Expression::FunctionCall { name, .. } => {
            name.as_str() == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME
        }
        Expression::BuiltinCall { function, .. } => {
            matches!(function, rumoca_core::BuiltinFunction::Sample)
        }
        _ => false,
    }
}

/// The condition-vector element a reference denotes, when `expr` is a
/// reference to `base_name` with exactly one literal index (either as a
/// structured subscript or embedded in the rendered name).
pub(crate) fn condition_ref_index(expr: &Expression, base_name: &str) -> Option<usize> {
    indexed_ref(expr, base_name)
}

/// Same as [`condition_ref_index`] for the generated `__pre__.` slot of the
/// condition vector.
pub(crate) fn pre_condition_ref_index(expr: &Expression, base_name: &str) -> Option<usize> {
    indexed_ref(expr, pre_slot_name(base_name).as_str())
}

fn indexed_ref(expr: &Expression, wanted_base: &str) -> Option<usize> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if name.as_str() == wanted_base {
        let [Subscript::Index { value, .. }] = subscripts.as_slice() else {
            return None;
        };
        return usize::try_from(*value).ok().filter(|index| *index >= 1);
    }
    if !subscripts.is_empty() {
        return None;
    }
    let (base, index) = component_path_trailing_index(name.as_str())?;
    (base == wanted_base).then_some(index)
}
