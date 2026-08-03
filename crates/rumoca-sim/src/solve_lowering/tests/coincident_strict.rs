use super::{column, compile};
use crate::{SimOptions, simulate_dae};

/// A sampled zero-order hold and a reset carrier meet exactly at `t = 1`:
/// `ySample = 0`, `saw = 0`. The source relation is strict, so equality belongs
/// to the false side even though the carrier reset reaches the root surface.
///
/// This is the analytic event shape used by MSL's single-phase two-level PWM
/// examples at each exact duty-cycle minimum. Keeping the fixture independent
/// of MSL proves the ownership contract rather than a model-specific outcome.
#[test]
fn sampled_zero_hold_strict_equality_keeps_the_typed_false_side() {
    let dae = compile(
        concat!(
            "model SampledStrictCoincidence\n",
            "  discrete Real ySample(start=0.25, fixed=true);\n",
            "  discrete Real carrierStart(start=0, fixed=true);\n",
            "  output Integer ticks(start=0, fixed=true);\n",
            "  Real held;\n",
            "  Real saw(start=0, fixed=true);\n",
            "  output Boolean fire;\n",
            "equation\n",
            "  der(saw) = 1;\n",
            "  when sample(1, 1) then\n",
            "    ySample = 0;\n",
            "    ticks = pre(ticks) + 1;\n",
            "  end when;\n",
            "  when sample(0, 1) then\n",
            "    carrierStart = time;\n",
            "    reinit(saw, 0);\n",
            "  end when;\n",
            "  held = pre(ySample);\n",
            "  fire = held > saw;\n",
            "end SampledStrictCoincidence;\n",
        ),
        "SampledStrictCoincidence",
    );
    let result = simulate_dae(
        &dae,
        &SimOptions {
            t_end: 1.1,
            dt: Some(0.1),
            max_wall_seconds: Some(10.0),
            ..SimOptions::default()
        },
    )
    .expect("the analytic sampled relation should simulate within its bounded gate");

    let fire = column(&result, "fire");
    let held = column(&result, "held");
    let saw = column(&result, "saw");
    let ticks = column(&result, "ticks");
    let at_or_after_tick = result
        .times
        .iter()
        .zip(fire)
        .filter(|(time, _)| **time >= 1.0)
        .map(|(time, value)| (*time, *value))
        .collect::<Vec<_>>();
    assert!(
        at_or_after_tick.iter().all(|(_, value)| *value == 0.0),
        "strict equality owns the false side at and after the coincident tick: {at_or_after_tick:?}; held={held:?}; saw={saw:?}"
    );
    assert!(
        result
            .times
            .iter()
            .zip(ticks)
            .filter(|(time, _)| **time >= 1.0)
            .all(|(_, value)| *value == 1.0),
        "post-commit canonicalization must not replay the sampled event row: {ticks:?}"
    );
}
