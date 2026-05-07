use std::collections::BTreeMap;
use std::net::{SocketAddr, UdpSocket};
use std::thread;
use std::time::{Duration, Instant};

use serde::{Deserialize, Serialize};

use super::stepper::StepperOptions;
use super::{Dae, SimError, SimOptions, SimResult, build_variable_meta};
use crate::runtime::time::stop_time_reached_with_tol;
use crate::{SimClockMode, SimLockStepBinding, SimLockStepOptions, SimStepper};

#[derive(Debug, Serialize)]
struct LockStepPacket<'a> {
    seq: u64,
    time: f64,
    values: &'a BTreeMap<String, f64>,
}

#[derive(Debug, Deserialize)]
struct LockStepReply {
    seq: Option<u64>,
    values: Option<BTreeMap<String, f64>>,
}

pub(crate) fn simulate_lock_step(dae: &Dae, opts: &SimOptions) -> Result<SimResult, SimError> {
    let mut warn = |message: String| eprintln!("{message}");
    simulate_lock_step_with_warning_sink(dae, opts, &mut warn)
}

fn simulate_lock_step_with_warning_sink<F>(
    dae: &Dae,
    opts: &SimOptions,
    warn: &mut F,
) -> Result<SimResult, SimError>
where
    F: FnMut(String),
{
    let config = validate_lock_step_options(opts)?;
    let mut stepper = build_lock_stepper(dae, opts)?;
    validate_binding_names(&stepper, config)?;
    let socket = bind_lock_step_socket(config)?;
    let expected_peer = parse_socket_addr_option(config.udp.expected_peer.as_deref())?;
    let send_target = parse_socket_addr(&config.udp.send, "lock_step.udp.send")?;
    let mut recv_buffer = vec![0u8; config.udp.max_packet_bytes];
    let names = stepper.variable_names().to_vec();
    let n_states = stepper.n_x;
    let mut times = Vec::new();
    let mut data = (0..names.len()).map(|_| Vec::new()).collect::<Vec<_>>();
    let wall_start = Instant::now();
    let mut seq = 0u64;

    record_stepper_sample(&stepper, &names, &mut times, &mut data);
    while !stop_time_reached_with_tol(stepper.time(), opts.t_end) {
        let packet_values = build_packet_values(&stepper, &config.outputs)?;
        let packet = LockStepPacket {
            seq,
            time: stepper.time(),
            values: &packet_values,
        };
        let payload = serde_json::to_vec(&packet)
            .map_err(|error| SimError::RuntimeProtocol(format!("encode packet: {error}")))?;
        socket
            .send_to(&payload, send_target)
            .map_err(|error| SimError::RuntimeIo(format!("send UDP packet: {error}")))?;

        let reply = recv_reply(&socket, &mut recv_buffer, expected_peer)?;
        apply_reply_inputs(&mut stepper, &config.inputs, seq, reply)?;

        let step_dt = remaining_step_dt(stepper.time(), opts.t_end, opts.dt.unwrap_or_default())?;
        stepper.step(step_dt)?;
        record_stepper_sample(&stepper, &names, &mut times, &mut data);
        pace_lock_step_clock(&stepper, config, wall_start, warn);
        seq = seq.wrapping_add(1);
    }

    Ok(SimResult {
        times,
        names: names.clone(),
        data,
        n_states,
        variable_meta: build_variable_meta(&stepper.dae, &names, n_states),
    })
}

fn validate_lock_step_options(opts: &SimOptions) -> Result<&SimLockStepOptions, SimError> {
    let config = opts
        .lock_step
        .as_ref()
        .ok_or_else(|| SimError::RuntimeConfig("missing lock-step config".to_string()))?;
    if opts.t_start.abs() > 1.0e-12 {
        return Err(SimError::RuntimeConfig(
            "lock-step simulation currently requires t_start == 0.0".to_string(),
        ));
    }
    if !opts.t_end.is_finite() || opts.t_end < 0.0 {
        return Err(SimError::RuntimeConfig(format!(
            "lock-step simulation requires a finite non-negative t_end, got {}",
            opts.t_end
        )));
    }
    if !opts.dt.is_some_and(|dt| dt.is_finite() && dt > 0.0) {
        return Err(SimError::RuntimeConfig(
            "lock-step simulation requires a positive dt".to_string(),
        ));
    }
    if config.udp.max_packet_bytes == 0 {
        return Err(SimError::RuntimeConfig(
            "lock_step.udp.max_packet_bytes must be greater than zero".to_string(),
        ));
    }
    if config.udp.bind.trim().is_empty() {
        return Err(SimError::RuntimeConfig(
            "lock_step.udp.bind must not be empty".to_string(),
        ));
    }
    if config.udp.send.trim().is_empty() {
        return Err(SimError::RuntimeConfig(
            "lock_step.udp.send must not be empty".to_string(),
        ));
    }
    Ok(config)
}

fn build_lock_stepper(dae: &Dae, opts: &SimOptions) -> Result<SimStepper, SimError> {
    SimStepper::new(
        dae,
        StepperOptions {
            rtol: opts.rtol,
            atol: opts.atol,
            scalarize: opts.scalarize,
            nominal_dt: opts.dt,
            max_wall_seconds_per_step: opts.max_wall_seconds,
        },
    )
}

fn validate_binding_names(
    stepper: &SimStepper,
    config: &SimLockStepOptions,
) -> Result<(), SimError> {
    for binding in &config.inputs {
        let variable = binding.variable.trim();
        if variable.is_empty() || binding.field.trim().is_empty() {
            return Err(SimError::RuntimeConfig(
                "lock-step input bindings require non-empty variable and field names".to_string(),
            ));
        }
        if !stepper.input_names().iter().any(|name| name == variable) {
            return Err(SimError::RuntimeConfig(format!(
                "unknown lock-step input variable '{variable}', available inputs: {:?}",
                stepper.input_names()
            )));
        }
    }

    for binding in &config.outputs {
        let variable = binding.variable.trim();
        if variable.is_empty() || binding.field.trim().is_empty() {
            return Err(SimError::RuntimeConfig(
                "lock-step output bindings require non-empty variable and field names".to_string(),
            ));
        }
        if !lock_step_output_available(stepper, variable) {
            return Err(SimError::RuntimeConfig(format!(
                "unknown lock-step output variable '{variable}', available outputs: {:?}",
                stepper.variable_names()
            )));
        }
    }

    Ok(())
}

fn lock_step_output_available(stepper: &SimStepper, variable: &str) -> bool {
    stepper.variable_names().iter().any(|name| name == variable)
        || stepper.input_names().iter().any(|name| name == variable)
}

fn bind_lock_step_socket(config: &SimLockStepOptions) -> Result<UdpSocket, SimError> {
    let socket = UdpSocket::bind(config.udp.bind.trim())
        .map_err(|error| SimError::RuntimeIo(format!("bind UDP socket: {error}")))?;
    if let Some(timeout) = duration_from_seconds(config.udp.recv_timeout_seconds)? {
        socket
            .set_read_timeout(Some(timeout))
            .map_err(|error| SimError::RuntimeIo(format!("set UDP read timeout: {error}")))?;
    }
    Ok(socket)
}

fn duration_from_seconds(raw: Option<f64>) -> Result<Option<Duration>, SimError> {
    let Some(seconds) = raw else {
        return Ok(None);
    };
    if !seconds.is_finite() || seconds <= 0.0 {
        return Err(SimError::RuntimeConfig(format!(
            "recv timeout must be a positive finite number, got {seconds}"
        )));
    }
    Ok(Some(Duration::from_secs_f64(seconds)))
}

fn parse_socket_addr(raw: &str, name: &str) -> Result<SocketAddr, SimError> {
    raw.trim().parse::<SocketAddr>().map_err(|error| {
        SimError::RuntimeConfig(format!("invalid {name} address '{raw}': {error}"))
    })
}

fn parse_socket_addr_option(raw: Option<&str>) -> Result<Option<SocketAddr>, SimError> {
    raw.filter(|value| !value.trim().is_empty())
        .map(|value| parse_socket_addr(value, "lock_step.udp.expected_peer"))
        .transpose()
}

fn build_packet_values(
    stepper: &SimStepper,
    bindings: &[SimLockStepBinding],
) -> Result<BTreeMap<String, f64>, SimError> {
    let state = stepper.state();
    bindings
        .iter()
        .map(|binding| {
            let variable = binding.variable.trim();
            let raw_value = state.values.get(variable).copied().ok_or_else(|| {
                SimError::RuntimeProtocol(format!(
                    "missing lock-step output variable '{variable}' in current state"
                ))
            })?;
            Ok((
                binding.field.trim().to_string(),
                apply_affine(raw_value, binding.scale, binding.offset),
            ))
        })
        .collect()
}

fn recv_reply(
    socket: &UdpSocket,
    buffer: &mut [u8],
    expected_peer: Option<SocketAddr>,
) -> Result<LockStepReply, SimError> {
    loop {
        let (size, peer) = socket
            .recv_from(buffer)
            .map_err(|error| SimError::RuntimeIo(format!("receive UDP packet: {error}")))?;
        if expected_peer.is_some() && Some(peer) != expected_peer {
            continue;
        }
        return serde_json::from_slice::<LockStepReply>(&buffer[..size]).map_err(|error| {
            SimError::RuntimeProtocol(format!("decode reply from {peer}: {error}"))
        });
    }
}

fn apply_reply_inputs(
    stepper: &mut SimStepper,
    bindings: &[SimLockStepBinding],
    expected_seq: u64,
    reply: LockStepReply,
) -> Result<(), SimError> {
    if let Some(reply_seq) = reply.seq
        && reply_seq != expected_seq
    {
        return Err(SimError::RuntimeProtocol(format!(
            "reply sequence mismatch: expected {expected_seq}, got {reply_seq}"
        )));
    }
    let values = reply.values.unwrap_or_default();
    for binding in bindings {
        let field = binding.field.trim();
        let raw_value = values.get(field).copied().ok_or_else(|| {
            SimError::RuntimeProtocol(format!("reply missing required input field '{field}'"))
        })?;
        let value = apply_affine(raw_value, binding.scale, binding.offset);
        stepper
            .set_input(binding.variable.trim(), value)
            .map_err(|error| SimError::RuntimeProtocol(error.to_string()))?;
    }
    Ok(())
}

fn apply_affine(value: f64, scale: Option<f64>, offset: Option<f64>) -> f64 {
    (value * scale.unwrap_or(1.0)) + offset.unwrap_or(0.0)
}

fn remaining_step_dt(current_time: f64, t_end: f64, base_dt: f64) -> Result<f64, SimError> {
    let remaining = (t_end - current_time).max(0.0);
    let step_dt = base_dt.min(remaining);
    if step_dt <= 0.0 {
        return Err(SimError::RuntimeConfig(
            "lock-step simulation failed to advance due to a non-positive step size".to_string(),
        ));
    }
    Ok(step_dt)
}

fn record_stepper_sample(
    stepper: &SimStepper,
    names: &[String],
    times: &mut Vec<f64>,
    data: &mut [Vec<f64>],
) {
    let state = stepper.state();
    times.push(state.time);
    for (series, name) in data.iter_mut().zip(names.iter()) {
        series.push(state.values.get(name).copied().unwrap_or_default());
    }
}

fn pace_lock_step_clock<F>(
    stepper: &SimStepper,
    config: &SimLockStepOptions,
    wall_start: Instant,
    warn: &mut F,
) where
    F: FnMut(String),
{
    if config.clock_mode != SimClockMode::RealTime {
        return;
    }
    let target = wall_start + Duration::from_secs_f64(stepper.time().max(0.0));
    let now = Instant::now();
    if now < target {
        thread::sleep(target - now);
        return;
    }
    if config.warn_if_slower_than_realtime {
        let lag_ms = (now - target).as_secs_f64() * 1_000.0;
        warn(format!(
            "warning: lock-step simulation is {:.3} ms behind real time at t={:.6}s",
            lag_ms,
            stepper.time()
        ));
    }
}

#[cfg(test)]
mod tests {
    use std::thread;
    use std::time::Duration;

    use rumoca_core::Span;
    use rumoca_ir_core::OpBinary;

    use super::*;
    use crate::with_diffsol::test_support::{sub, var_ref};
    use rumoca_ir_dae as dae;

    #[derive(Debug, Deserialize)]
    struct ControllerPacket {
        seq: u64,
        time: f64,
        values: BTreeMap<String, f64>,
    }

    type ObservedPacket = (u64, f64, f64);
    type ControllerHandle = thread::JoinHandle<Vec<ObservedPacket>>;

    fn integrator_dae() -> Dae {
        let mut dae = Dae::new();
        dae.states.insert(
            dae::VarName::new("x"),
            dae::Variable::new(dae::VarName::new("x")),
        );
        dae.inputs.insert(
            dae::VarName::new("u"),
            dae::Variable::new(dae::VarName::new("u")),
        );
        dae.outputs.insert(
            dae::VarName::new("y"),
            dae::Variable::new(dae::VarName::new("y")),
        );
        dae.f_x.push(dae::Equation {
            lhs: None,
            rhs: sub(
                dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Der,
                    args: vec![var_ref("x")],
                },
                var_ref("u"),
            ),
            span: Span::DUMMY,
            origin: "x_dynamics".to_string(),
            scalar_count: 1,
        });
        dae.f_x.push(dae::Equation {
            lhs: None,
            rhs: dae::Expression::Binary {
                op: OpBinary::Sub(Default::default()),
                lhs: Box::new(var_ref("y")),
                rhs: Box::new(var_ref("x")),
            },
            span: Span::DUMMY,
            origin: "y_alias".to_string(),
            scalar_count: 1,
        });
        dae
    }

    fn maybe_sleep(delay: Duration) {
        if delay.is_zero() {
            return;
        }
        thread::sleep(delay);
    }

    fn spawn_controller(response_delay: Duration) -> (SocketAddr, ControllerHandle) {
        let socket = UdpSocket::bind("127.0.0.1:0").expect("bind controller socket");
        let addr = socket.local_addr().expect("controller addr");
        let handle = thread::spawn(move || {
            let mut buf = [0u8; 4096];
            let mut seen = Vec::new();
            for _ in 0..3 {
                let (size, peer) = socket.recv_from(&mut buf).expect("recv packet");
                let packet: ControllerPacket =
                    serde_json::from_slice(&buf[..size]).expect("decode packet");
                seen.push((
                    packet.seq,
                    packet.time,
                    *packet.values.get("y").unwrap_or(&f64::NAN),
                ));
                maybe_sleep(response_delay);
                let reply = serde_json::to_vec(&serde_json::json!({
                    "seq": packet.seq,
                    "values": { "u": 1.0 }
                }))
                .expect("encode reply");
                socket.send_to(&reply, peer).expect("send reply");
            }
            seen
        });
        (addr, handle)
    }

    fn lock_step_options(
        send: SocketAddr,
        clock_mode: SimClockMode,
        dt: f64,
        t_end: f64,
    ) -> SimOptions {
        SimOptions {
            t_end,
            dt: Some(dt),
            lock_step: Some(SimLockStepOptions {
                clock_mode,
                udp: crate::SimLockStepUdpOptions {
                    bind: "127.0.0.1:0".to_string(),
                    send: send.to_string(),
                    ..crate::SimLockStepUdpOptions::default()
                },
                inputs: vec![crate::SimLockStepBinding {
                    variable: "u".to_string(),
                    field: "u".to_string(),
                    ..crate::SimLockStepBinding::default()
                }],
                outputs: vec![crate::SimLockStepBinding {
                    variable: "x".to_string(),
                    field: "y".to_string(),
                    ..crate::SimLockStepBinding::default()
                }],
                ..SimLockStepOptions::default()
            }),
            ..SimOptions::default()
        }
    }

    #[test]
    fn lock_step_simulation_exchanges_udp_packets_and_advances_model() {
        let (controller_addr, handle) = spawn_controller(Duration::ZERO);
        let dae = integrator_dae();
        let result = simulate_lock_step(
            &dae,
            &lock_step_options(controller_addr, SimClockMode::AsFastAsPossible, 0.1, 0.3),
        )
        .expect("lock-step simulation should succeed");

        let x_idx = result
            .names
            .iter()
            .position(|name| name == "x")
            .expect("x state");
        assert_eq!(result.times, vec![0.0, 0.1, 0.2, 0.3]);
        assert!((result.data[x_idx][3] - 0.3).abs() < 1.0e-6);

        let packets = handle.join().expect("controller thread");
        assert_eq!(packets.len(), 3);
        assert_eq!(packets[0].0, 0);
        assert_eq!(packets[1].0, 1);
        assert!((packets[0].2 - 0.0).abs() < 1.0e-9);
        assert!((packets[2].2 - 0.2).abs() < 1.0e-6);
    }

    #[test]
    fn realtime_lock_step_warns_when_controller_lags() {
        let (controller_addr, handle) = spawn_controller(Duration::from_millis(25));
        let dae = integrator_dae();
        let opts = lock_step_options(controller_addr, SimClockMode::RealTime, 0.01, 0.03);
        let mut warnings = Vec::new();
        let result = simulate_lock_step_with_warning_sink(&dae, &opts, &mut |message| {
            warnings.push(message)
        });

        assert!(
            result.is_ok(),
            "expected simulation success, got {result:?}"
        );
        assert!(
            !warnings.is_empty(),
            "expected at least one realtime lag warning"
        );
        let _ = handle.join().expect("controller thread");
    }
}
