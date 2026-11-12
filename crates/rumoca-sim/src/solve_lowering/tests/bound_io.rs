//! Bound input writes and output reads leave the session bit for bit where
//! the named calls of the same names leave it, under the adaptive and the
//! fixed-step plugin, whether a script uses names, bindings or both.

use rumoca_solver::fmi_me::{
    MeInstanceConfig, MeIntegratorBackend, MeNumericalSetup,
    driver::{advance_live_session, live_session_options},
    session::{
        MeBindingRefusal, MeRetainedComponent, MeSessionError, MeSimulationSession,
        input_write_audit::{InputWriteAudit, input_write_counts, unguarded_input_write_count},
    },
};

use super::{SimOptions, compile};

#[derive(Clone, Copy, Debug)]
enum Step {
    Set(&'static [(&'static str, f64)]),
    /// A frame the component refuses; a bound run also sends it short.
    Refused(&'static [(&'static str, f64)]),
    Advance(f64),
    /// Every visible output and declared input, with a repeat.
    Read,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Calls {
    Named,
    Bound,
    /// Writes and reads alternate between names and bindings.
    Alternating,
}

#[derive(Clone, Copy, Debug)]
enum Plugin {
    Adaptive,
    FixedStep,
}

/// What one run of a script leaves behind.
#[derive(Debug, PartialEq)]
struct Run {
    audits: Vec<InputWriteAudit>,
    /// Each read's bits; `None` where the named read has no value.
    reads: Vec<Vec<Option<u64>>>,
    trace: String,
    /// Changed frames as `[continuous, input event]`.
    writes: [usize; 2],
    /// Frames written while the session stood on a deferred yield.
    deferred: usize,
    /// Continuous frames written without a restoration point.
    unguarded: usize,
}

fn plugin(plugin: Plugin, setup: MeNumericalSetup) -> Box<dyn MeIntegratorBackend> {
    match plugin {
        Plugin::Adaptive => rumoca_solver_rk45::model_exchange_integrator(setup),
        Plugin::FixedStep => rumoca_solver_rk45::fixed_step_integrator(
            setup,
            rumoca_solver_rk45::FixedStepConfig::default(),
        )
        .expect("the default fixed-step configuration is valid"),
    }
}

fn session(dae: &rumoca_ir_dae::Dae, kind: Plugin) -> MeSimulationSession<'static, 'static> {
    let opts = SimOptions {
        t_end: 1.0,
        dt: Some(0.1),
        atol: 1e-9,
        rtol: 1e-9,
        ..SimOptions::default()
    };
    let (artifact, backend) = super::super::lower_runtime_fmi_artifact(dae, &opts).unwrap();
    let config = MeInstanceConfig::open_ended("bound-io", opts.rtol, opts.t_start).unwrap();
    let retained = MeRetainedComponent::instantiate(artifact.source(), &config, backend).unwrap();
    let options = live_session_options(
        &artifact.root_location(),
        opts.t_start,
        opts.rtol,
        opts.atol,
        opts.t_end - opts.t_start,
        opts.max_wall_seconds,
    )
    .unwrap();
    let host = retained.into_lease(options).unwrap();
    let plugin = (host.state_count() != 0).then(|| {
        let setup = host
            .numerical_setup(Some(crate::me_backend::default_step_size(&opts)))
            .unwrap();
        plugin(kind, setup)
    });
    host.into_session(plugin).unwrap()
}

/// The binding of `frame`'s names, issued on first use.
fn input_binding(
    session: &mut MeSimulationSession<'_, '_>,
    bindings: &mut Vec<(&'static [(&'static str, f64)], u32)>,
    frame: &'static [(&'static str, f64)],
) -> u32 {
    if let Some((_, id)) = bindings
        .iter()
        .find(|(bound, _)| std::ptr::eq(*bound, frame))
    {
        return *id;
    }
    let names: Vec<&str> = frame.iter().map(|(name, _)| *name).collect();
    let id = session.bind_inputs(&names).unwrap();
    bindings.push((frame, id));
    id
}

fn values(frame: &[(&str, f64)]) -> Vec<f64> {
    frame.iter().map(|(_, value)| *value).collect()
}

/// The bits of a bound read, checked against the named read `named` where
/// the bound read has no value: only an input not yet applied has none.
fn bound_read(
    session: &MeSimulationSession<'_, '_>,
    outputs: u32,
    named: &[Option<f64>],
) -> Vec<Option<u64>> {
    let mut read = Vec::new();
    match session.read_bound(outputs, &mut read) {
        Ok(()) => read
            .into_iter()
            .map(|value| Some(value.to_bits()))
            .collect(),
        Err(MeSessionError::Binding {
            refusal: MeBindingRefusal::Unapplied { position },
            ..
        }) => {
            assert_eq!(named.iter().position(Option::is_none), Some(position));
            named.iter().map(|value| value.map(f64::to_bits)).collect()
        }
        Err(error) => panic!("bound read: {error}"),
    }
}

fn run(source: &str, model: &str, kind: Plugin, calls: Calls, script: &[Step]) -> Run {
    let dae = compile(source, model);
    let mut session = session(&dae, kind);
    let mut names = session.output_names().unwrap();
    names.extend(session.input_names());
    names.push(names[0].clone());
    let outputs = session.bind_outputs(&names).unwrap();
    let mut bindings = Vec::new();
    let mut audits = vec![session.input_write_audit().unwrap()];
    let mut reads = Vec::new();
    let mut deferred = 0;
    let before = input_write_counts();
    let unguarded = unguarded_input_write_count();
    for (index, step) in script.iter().enumerate() {
        let bound = match calls {
            Calls::Named => false,
            Calls::Bound => true,
            Calls::Alternating => index % 2 == 1,
        };
        match *step {
            Step::Set(frame) => {
                deferred += usize::from(audits.last().is_some_and(|audit| audit.deferred_yield));
                if bound {
                    let id = input_binding(&mut session, &mut bindings, frame);
                    session.write_bound(id, &values(frame)).unwrap();
                } else {
                    session.set_inputs(frame).unwrap();
                }
            }
            Step::Refused(frame) => {
                if bound {
                    let id = input_binding(&mut session, &mut bindings, frame);
                    session.write_bound(id, &values(frame)).unwrap_err();
                    let short = session.write_bound(id, &values(&frame[1..])).unwrap_err();
                    assert!(matches!(
                        short,
                        MeSessionError::Binding {
                            refusal: MeBindingRefusal::Arity { .. },
                            ..
                        }
                    ));
                } else {
                    session.set_inputs(frame).unwrap_err();
                }
            }
            Step::Advance(time) => {
                advance_live_session(&mut session, time).unwrap();
            }
            Step::Read => {
                let named = session.selected_values(&names).unwrap();
                reads.push(if bound {
                    bound_read(&session, outputs, &named)
                } else {
                    named.iter().map(|value| value.map(f64::to_bits)).collect()
                });
            }
        }
        audits.push(
            session.input_write_audit().unwrap_or_else(|error| {
                panic!("{model} {kind:?} {calls:?} after {step:?}: {error}")
            }),
        );
    }
    let after = input_write_counts();
    Run {
        audits,
        reads,
        trace: format!("{:?}", session.finish()),
        writes: [after[0] - before[0], after[1] - before[1]],
        deferred,
        unguarded: unguarded_input_write_count() - unguarded,
    }
}

/// Run `script` through names, through bindings and alternating, under both
/// plugins; every run must leave the named run's bits after every step.
/// Returns each plugin's named run and how many frames the bound run wrote
/// without a restoration point.
fn assert_calls_agree(source: &str, model: &str, script: &[Step]) -> Vec<(Plugin, Run, usize)> {
    [Plugin::Adaptive, Plugin::FixedStep]
        .into_iter()
        .map(|kind| {
            let named = run(source, model, kind, Calls::Named, script);
            assert_eq!(named.unguarded, 0, "{model} {kind:?}: the named write");
            let mut unguarded = 0;
            for calls in [Calls::Bound, Calls::Alternating] {
                let other = run(source, model, kind, calls, script);
                if calls == Calls::Bound {
                    unguarded = other.unguarded;
                }
                for (index, (expected, actual)) in
                    named.audits.iter().zip(&other.audits).enumerate()
                {
                    assert_eq!(
                        expected,
                        actual,
                        "{model} {kind:?} {calls:?}: first difference after step {index} ({:?})",
                        index.checked_sub(1).map(|step| script[step])
                    );
                }
                assert_eq!(
                    named.reads, other.reads,
                    "{model} {kind:?} {calls:?}: reads"
                );
                assert_eq!(
                    named.trace, other.trace,
                    "{model} {kind:?} {calls:?}: trace"
                );
                assert_eq!(
                    named.writes, other.writes,
                    "{model} {kind:?} {calls:?}: paths"
                );
                assert_eq!(named.deferred, other.deferred, "{model} {kind:?} {calls:?}");
            }
            (kind, named, unguarded)
        })
        .collect()
}

const NO_EVENTS: &str = "model NoEvents input Real u = 1; input Real w = 0; \
    Real x(start=1, fixed=true); Real z; output Real y; \
    equation z = 2*u + w; der(x) = -x + z; y = x + z; end NoEvents;";

#[test]
fn bound_calls_match_named_calls_on_continuous_writes_and_deferred_yields() {
    let script = [
        // Neither input is applied yet.
        Step::Read,
        Step::Advance(0.1),
        Step::Set(&[("u", 2.0)]),
        Step::Read,
        Step::Advance(0.2),
        Step::Set(&[("u", 2.0), ("w", -1.0)]),
        // Unchanged.
        Step::Set(&[("u", 2.0), ("w", -1.0)]),
        Step::Refused(&[("u", 3.0), ("w", f64::NAN)]),
        Step::Set(&[("w", 0.5)]),
        // Off the output grid: the yield may be deferred inside a step.
        Step::Advance(0.233),
        Step::Read,
        Step::Set(&[("u", -3.0)]),
        Step::Read,
        Step::Advance(0.31),
        Step::Set(&[("u", 0.0), ("u", 4.0)]),
        Step::Advance(0.5),
        Step::Read,
    ];
    for (kind, named, unguarded) in assert_calls_agree(NO_EVENTS, "NoEvents", &script) {
        assert_eq!(named.writes, [5, 0], "{kind:?}");
        // No indicator, scheduled root or delay: no continuous write can fail
        // after its first mutation.
        assert_eq!(unguarded, 5, "{kind:?}");
        if matches!(kind, Plugin::Adaptive) {
            assert!(named.deferred > 0, "no write met a deferred yield");
        }
    }
}

const MIXED: &str = "model Mixed input Real u = 0; input Real g = 1; \
    Real x(start=0, fixed=true); output Real y; \
    equation der(x) = u; y = if g > 0.5 then x else -x; end Mixed;";

#[test]
fn bound_calls_match_named_calls_on_writes_that_reach_an_event_owner() {
    let script = [
        Step::Advance(0.1),
        Step::Set(&[("u", 2.0), ("g", 1.0)]),
        Step::Read,
        Step::Advance(0.2),
        Step::Set(&[("g", 0.0)]),
        Step::Read,
        Step::Advance(0.3),
        Step::Set(&[("g", 0.0), ("u", -1.0)]),
        Step::Refused(&[("g", f64::INFINITY), ("u", 1.0)]),
        Step::Advance(0.4),
        Step::Read,
    ];
    for (kind, named, unguarded) in assert_calls_agree(MIXED, "Mixed", &script) {
        assert_eq!(named.writes, [2, 1], "{kind:?}");
        // The relation's indicator is evaluated after positioning.
        assert_eq!(unguarded, 0, "{kind:?}");
    }
}

const TICKS: &str = "model Ticks input Real u = 0; Real x(start=0, fixed=true); \
    discrete Real s(start=0, fixed=true); output Real y; \
    equation der(x) = u; y = s; when sample(0, 0.1) then s = pre(s) + x; end when; end Ticks;";

#[test]
fn bound_calls_match_named_calls_on_writes_at_a_time_event() {
    let script = [
        Step::Advance(0.1),
        Step::Set(&[("u", 1.0)]),
        Step::Read,
        Step::Advance(0.15),
        Step::Set(&[("u", 2.0)]),
        Step::Advance(0.2),
        Step::Set(&[("u", -1.0)]),
        Step::Read,
        Step::Advance(0.35),
        Step::Read,
    ];
    for (kind, named, _) in assert_calls_agree(TICKS, "Ticks", &script) {
        if matches!(kind, Plugin::Adaptive) {
            // The ticks at 0.1 and 0.2 are still due when those writes arrive.
            assert_eq!(named.writes, [1, 2]);
        }
    }
}

const STATELESS: &str = "model Stateless input Real a = 0; input Real h = 1; input Real o = 0; \
    output Real y; output Real z; output Real w; \
    equation y = if abs(a) > h then 1 else 0; w = 3*o; z = 2*w + 1; end Stateless;";

#[test]
fn bound_calls_match_named_calls_on_a_model_without_states() {
    let script = [
        Step::Set(&[("o", 1.0)]),
        Step::Read,
        Step::Set(&[("a", 2.0)]),
        Step::Set(&[("o", 2.0), ("h", 1.0)]),
        Step::Advance(0.1),
        Step::Read,
        Step::Set(&[("o", 3.0)]),
        Step::Set(&[("a", 0.5), ("o", 4.0)]),
        Step::Refused(&[("a", f64::NAN), ("o", 4.0)]),
        Step::Advance(0.2),
        Step::Read,
    ];
    for (kind, named, _) in assert_calls_agree(STATELESS, "Stateless", &script) {
        assert_eq!(named.writes, [3, 2], "{kind:?}");
    }
}
