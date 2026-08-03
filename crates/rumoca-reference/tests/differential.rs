//! Differential harness: the reference against the real pipeline.
//!
//! Each hand-written case in [`models`] is run twice — once through
//! [`rumoca_reference`], once through compile + simulate — and the discrete
//! values are required to agree at a set of probe instants.
//!
//! A disagreement here is a finding, not a test to be relaxed. One of the two
//! sides is wrong about what the model means, and the harness deliberately does
//! not encode which: it reports both values and lets a reviewer adjudicate
//! against the specification. Widening a tolerance to make a case pass would
//! destroy the only thing this harness is for.
//!
//! Probes never land on an event instant. rumoca stamps a post-event sample at
//! `instant + 2 * atol` where the reference stamps it at `instant` exactly, so
//! probing an instant compares event *time-stamping* — a solver-reporting
//! question — instead of event *semantics*.

mod differential {
    pub(crate) mod generated;
    pub(crate) mod models;
}

use differential::generated::{Spec, WhenSpec, probes};
use differential::models::{Case, cases};
use proptest::prelude::*;

use rumoca_compile::{Session, SessionConfig};
use rumoca_reference::expand::expand;
use rumoca_reference::schedule::static_instants;
use rumoca_reference::simulate::{Options, Trace, simulate};
use rumoca_reference::trajectory::{NoContinuousState, Ramps};
use rumoca_reference::value::Value;
use rumoca_sim::{SimOptions, SimResult, SimSolverMode, simulate_with_diagnostics};

const T_START: f64 = 0.0;
const T_STOP: f64 = 1.0;

/// Discrete values are exact on both sides, so the only slack needed is for the
/// pipeline having carried them through `f64` slots.
const VALUE_TOLERANCE: f64 = 1.0e-9;

/// Both solver sessions, so a case cannot pass by naming the lucky one.
const SESSIONS: [SimSolverMode; 2] = [SimSolverMode::RkLike, SimSolverMode::Bdf];

fn reference_options() -> Options {
    Options {
        t_start: T_START,
        t_stop: T_STOP,
        ..Options::default()
    }
}

/// Runs the reference side.
fn reference_trace(case: &Case) -> Trace {
    let mut ramps = Ramps::new(T_START);
    for (name, slope) in case.slopes {
        let start = case
            .model
            .variable(name)
            .and_then(|variable| match &variable.start {
                rumoca_reference::model::Expr::Literal(value) => value.as_real(),
                _ => None,
            })
            .unwrap_or_else(|| panic!("{}: `{name}` needs a literal start", case.name));
        ramps = ramps.with(name, start, *slope);
    }
    simulate(&case.model, &ramps, reference_options())
        .unwrap_or_else(|error| panic!("{}: reference run failed: {error}", case.name))
}

/// Runs the pipeline side on one solver session.
fn pipeline_trace(case: &Case, mode: SimSolverMode) -> SimResult {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("differential.mo", case.source)
        .unwrap_or_else(|error| panic!("{}: should parse: {error}", case.name));
    let compiled = session
        .compile_model(case.name)
        .unwrap_or_else(|error| panic!("{}: should compile: {error}", case.name));
    simulate_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_start: T_START,
            t_end: T_STOP,
            dt: Some(0.05),
            solver_mode: mode,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{}: should simulate on {mode:?}: {error}", case.name))
}

/// The last pipeline sample at or before `time` (right-continuous hold).
fn pipeline_value_at(sim: &SimResult, name: &str, time: f64) -> f64 {
    let index = sim
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("trace should carry `{name}`; names={:?}", sim.names));
    let values = &sim.data[index];
    let mut result = values[0];
    for (sample, &stamp) in sim.times.iter().enumerate() {
        if stamp <= time + 1.0e-9 {
            result = values[sample];
        } else {
            break;
        }
    }
    result
}

/// The reference's value as an `f64`.
fn reference_value_at(trace: &Trace, case: &Case, name: &str, time: f64) -> f64 {
    let value = trace
        .value_at(name, time)
        .unwrap_or_else(|| panic!("{}: reference should carry `{name}`", case.name));
    match value {
        Value::Boolean(inner) => f64::from(u8::from(inner)),
        other => other
            .as_real()
            .unwrap_or_else(|| panic!("{}: `{name}` is not numeric", case.name)),
    }
}

/// Every probe where one case disagrees with the reference on one session.
fn disagreements(case: &Case, reference: &Trace, mode: SimSolverMode) -> Vec<String> {
    let pipeline = pipeline_trace(case, mode);
    let mut found = Vec::new();
    for &probe in case.probes {
        for name in case.observed {
            let expected = reference_value_at(reference, case, name, probe);
            let actual = pipeline_value_at(&pipeline, name, probe);
            if (expected - actual).abs() > VALUE_TOLERANCE {
                found.push(format!(
                    "  {model} / {mode:?}: `{name}` at t={probe} is {actual} in \
                     the pipeline and {expected} in the reference (rows {registry:?})",
                    model = case.name,
                    registry = case.registry,
                ));
            }
        }
    }
    found
}

#[test]
fn hand_written_models_agree_between_reference_and_pipeline() {
    let mut found = Vec::new();
    for case in cases() {
        let reference = reference_trace(&case);
        for mode in SESSIONS {
            if EXPECTED_DIVERGENCES
                .iter()
                .any(|(model, session)| *model == case.name && *session == mode)
            {
                continue;
            }
            found.extend(disagreements(&case, &reference, mode));
        }
    }
    assert!(
        found.is_empty(),
        "the reference and the pipeline disagree about what these models \
         mean:\n{}\n\nOne side is wrong. Do not widen this assertion and do not \
         add a row to EXPECTED_DIVERGENCES without a registry entry: adjudicate \
         against the specification first.",
        found.join("\n")
    );
}

/// Comparisons this harness skips, and why each one is skipped.
///
/// Both entries are the diffsol session. The rk-like session agrees with the
/// reference on every case, which is what makes these session divergences
/// rather than semantics disagreements.
///
/// * `LiteralWhenNeverFires` — registry row **FS-SIM-010**
///   (`RecordedDivergence`), whose latitude note states it directly: "omc
///   leaves `when true then y = pre(y) + 1` at 0; the diffsol session applies
///   its initial-event update two or three times."
/// * `SelfReschedulingCounter` — registry row **FS-SIM-016**
///   (`RecordedDivergence`), filed by this wave. The diffsol session never
///   fires the dynamic time event at all: `count` stays 0 and `nextTime` stays
///   at its start value for the whole run, where the rk-like session and the
///   reference both count 1, 2, 3, 4. This is not FS-SIM-009, which covers a
///   guard *already met at the start* and whose own note says a guard not met
///   at the start "counts normally"; here the first threshold (0.2) is not met
///   at t = 0. It is not FS-SIM-011 either, which is about the accuracy of a
///   located state crossing, not about a dynamic time event failing to occur.
///   The row is filed `SpecSourced` against §8.3.5.1 rather than
///   `OracleImplied`, because no omc measurement was taken through the parity
///   tooling and naming an oracle that was never run would be worse than
///   naming none.
const EXPECTED_DIVERGENCES: &[(&str, SimSolverMode)] = &[
    ("LiteralWhenNeverFires", SimSolverMode::Bdf),
    ("SelfReschedulingCounter", SimSolverMode::Bdf),
];

/// Pins the unregistered dynamic-time-event divergence described above.
///
/// Asserts what the diffsol session *does*, so it fails when the divergence is
/// fixed. Until then it keeps the skip above from reading as a claim that the
/// two sessions agree.
#[test]
fn the_diffsol_session_never_fires_a_self_rescheduling_time_event() {
    let case = cases()
        .into_iter()
        .find(|entry| entry.name == "SelfReschedulingCounter")
        .expect("the self-rescheduling case is in the case list");
    let reference = reference_trace(&case);
    assert_eq!(
        reference.final_value("count"),
        Some(Value::Real(5.0)),
        "the reference fires at 0.2, 0.4, 0.6, 0.8 and 1.0; the last one lands \
         exactly on the stop time, which the reference admits and the probe \
         list deliberately does not compare"
    );
    let rk_like = pipeline_trace(&case, SimSolverMode::RkLike);
    assert!(
        (pipeline_value_at(&rk_like, "count", 0.9) - 4.0).abs() <= VALUE_TOLERANCE,
        "the rk-like session agrees with the reference"
    );
    let bdf = pipeline_trace(&case, SimSolverMode::Bdf);
    assert!(
        pipeline_value_at(&bdf, "count", 0.9).abs() <= VALUE_TOLERANCE,
        "the diffsol session never fires the guard. When this assertion fails \
         the divergence has been fixed — remove it and the EXPECTED_DIVERGENCES \
         row together."
    );
}

/// Pins the FS-SIM-010 divergence rather than hiding it behind the skip above.
///
/// This test asserts what the diffsol session *does*, so it fails when the
/// divergence is fixed — that is the `PinsDivergence` polarity the registry
/// uses, and it is what stops a skipped comparison from quietly becoming a
/// claim of compliance.
#[test]
fn the_diffsol_session_still_activates_a_literal_true_when() {
    let case = cases()
        .into_iter()
        .find(|entry| entry.name == "LiteralWhenNeverFires")
        .expect("the literal-true case is in the case list");
    let reference = reference_trace(&case);
    assert_eq!(
        reference.final_value("y"),
        Some(Value::Real(0.0)),
        "§8.3.5.1 gives the buffer of `when true then` a true start value, so \
         it has no rising edge and the body never runs"
    );
    let rk_like = pipeline_trace(&case, SimSolverMode::RkLike);
    assert!(
        pipeline_value_at(&rk_like, "y", 1.0).abs() <= VALUE_TOLERANCE,
        "the rk-like session agrees with the reference and with omc"
    );
    let bdf = pipeline_trace(&case, SimSolverMode::Bdf);
    assert!(
        (pipeline_value_at(&bdf, "y", 1.0) - 1.0).abs() <= VALUE_TOLERANCE,
        "FS-SIM-010: the diffsol session applies the initial event to a \
         literal-true activation. When this assertion fails the divergence has \
         been fixed — remove it and the EXPECTED_DIVERGENCES row together."
    );
}

/// The reference's static time-event instants against the compiler's own.
///
/// This compares two independent *readings* of one model rather than two runs,
/// so it catches a scheduling disagreement even where the values happen to
/// coincide.
#[test]
fn static_time_event_instants_agree_with_the_compiler() {
    for case in cases() {
        let expected = static_instants(&expand(&case.model).model);
        let mut session = Session::new(SessionConfig::default());
        session
            .add_document("differential.mo", case.source)
            .unwrap_or_else(|error| panic!("{}: should parse: {error}", case.name));
        let compiled = session
            .compile_model(case.name)
            .unwrap_or_else(|error| panic!("{}: should compile: {error}", case.name));
        let actual: Vec<f64> = compiled.dae.inspect(|view| {
            (0..view.time_event_count())
                .filter_map(|index| view.time_event(view.time_event_id(index)?))
                .filter_map(|event| event.instant())
                .map(|instant| {
                    #[expect(
                        clippy::cast_precision_loss,
                        reason = "slice-1 instants are small dyadic rationals; the comparison below is exact for them"
                    )]
                    let value = instant.numerator() as f64 / instant.denominator() as f64;
                    value
                })
                .collect()
        });
        let mut actual = actual;
        actual.sort_by(f64::total_cmp);
        actual.dedup();
        assert_eq!(
            expected, actual,
            "{}: the reference schedules {expected:?} and the compiler \
             schedules {actual:?}. {}",
            case.name, case.note
        );
    }
}

/// Compiles and simulates a generated model on the rk-like session.
///
/// Only the rk-like session: the two divergences pinned above are both diffsol
/// ones, and a generator that rediscovered them on every case would bury a new
/// finding under known noise. Widening the generator to the diffsol session is
/// worth doing once those two rows are closed.
fn generated_pipeline(spec: &Spec) -> SimResult {
    let source = spec.source();
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("generated.mo", &source)
        .unwrap_or_else(|error| panic!("generated model should parse: {error}\n{source}"));
    let compiled = session
        .compile_model("Generated")
        .unwrap_or_else(|error| panic!("generated model should compile: {error}\n{source}"));
    simulate_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_start: T_START,
            t_end: T_STOP,
            dt: Some(0.05),
            solver_mode: SimSolverMode::RkLike,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("generated model should simulate: {error}\n{source}"))
}

fn when_spec() -> impl Strategy<Value = WhenSpec> {
    (1_u32..8, any::<bool>()).prop_map(|(instant_eighths, accumulates)| WhenSpec {
        instant_eighths,
        accumulates,
    })
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(24))]

    /// The reference and the compiler agree on every mini model in the slice-1
    /// fragment, not only on the hand-written ones.
    #[test]
    fn generated_models_agree_between_reference_and_pipeline(
        whens in proptest::collection::vec(when_spec(), 1..=2),
    ) {
        let spec = Spec { whens };
        let model = spec.model();
        let reference = simulate(&model, &NoContinuousState, reference_options())
            .expect("a generated model is inside slice 1");
        let pipeline = generated_pipeline(&spec);
        for probe in probes() {
            for name in spec.observed() {
                let expected = reference
                    .value_at(name, probe)
                    .and_then(Value::as_real)
                    .expect("the reference carries every generated target");
                let actual = pipeline_value_at(&pipeline, name, probe);
                prop_assert!(
                    (expected - actual).abs() <= VALUE_TOLERANCE,
                    "`{name}` at t={probe} is {actual} in the pipeline and \
                     {expected} in the reference, for:\n{}",
                    spec.source()
                );
            }
        }
    }
}
