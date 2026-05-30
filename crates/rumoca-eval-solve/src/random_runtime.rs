use std::collections::HashMap;
use std::sync::Mutex;

use rumoca_ir_solve::RandomGenerator;

use crate::{EvalSolveError, RowEvalContext, get};

#[derive(Default)]
pub(super) struct ImpureRandomState {
    streams: HashMap<i64, ImpureRandomStream>,
}

impl ImpureRandomState {
    pub(super) fn clear(&mut self) {
        self.streams.clear();
    }
}

struct ImpureRandomStream {
    state: u64,
    cached_by_event_call: HashMap<(u64, u64), f64>,
}

pub(super) fn impure_random_mutex<'a>(context: RowEvalContext<'a>) -> &'a Mutex<ImpureRandomState> {
    context
        .runtime_state
        .expect("row evaluation context must have runtime state")
        .impure_random
        .as_ref()
}

pub(super) fn impure_random_stream_id(seed: i64) -> i64 {
    clamp_i64_to_positive_u31(scramble_seed((seed as u64) ^ 0x7158_2788_3000_0001))
}

pub(super) fn impure_random_sample(
    id: i64,
    call_site: u64,
    t: f64,
    random_state: &Mutex<ImpureRandomState>,
) -> f64 {
    let mut state = random_state
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    let stream = state
        .streams
        .entry(id)
        .or_insert_with(|| ImpureRandomStream {
            state: scramble_seed((id as u64).wrapping_mul(0xD1B54A32D192ED03)).max(1),
            cached_by_event_call: HashMap::new(),
        });
    let key = (t.to_bits(), call_site);
    if let Some(value) = stream.cached_by_event_call.get(&key).copied() {
        return value;
    }
    let value = unit_from_u64(xorshift64star_next(&mut stream.state));
    stream.cached_by_event_call.insert(key, value);
    value
}

pub(super) fn read_reg_range(
    regs: &[f64],
    initialized: &[bool],
    start: u32,
    len: usize,
) -> Result<Vec<f64>, EvalSolveError> {
    if len == 0 {
        return Ok(vec![1.0]);
    }
    (0..len)
        .map(|idx| get(regs, initialized, start + idx as u32))
        .collect()
}

pub(super) fn projected_random_value(values: &[f64], index: usize) -> f64 {
    values
        .get(index)
        .copied()
        .unwrap_or_else(|| values.first().copied().unwrap_or(1.0))
}

pub(super) fn initial_state_values(
    generator: RandomGenerator,
    local_seed: i64,
    global_seed: i64,
    state_len: usize,
) -> Vec<f64> {
    let len = state_len.max(random_generator_state_len(generator));
    let mut state = scramble_seed(
        (local_seed as u64)
            ^ ((global_seed as u64) << 1)
            ^ (len as u64).wrapping_mul(0x9E3779B97F4A7C15),
    )
    .max(1);
    let mut out = Vec::with_capacity(len);
    for idx in 0..len {
        state = scramble_seed(state ^ (idx as u64 + 1).wrapping_mul(0xD1B54A32D192ED03)).max(1);
        out.push(clamp_i64_to_positive_u31(state) as f64);
    }
    out
}

pub(super) fn random_result_and_state(
    generator: RandomGenerator,
    seed_values: &[f64],
) -> (f64, Vec<f64>) {
    let len = random_generator_state_len(generator)
        .max(seed_values.len())
        .max(1);
    let mut state = seed_values
        .iter()
        .fold(0u64, |acc, value| acc ^ scramble_seed(value.to_bits()))
        ^ scramble_seed(
            random_generator_name(generator)
                .bytes()
                .fold(0u64, |acc, b| acc.wrapping_mul(16777619) ^ b as u64),
        );
    if state == 0 {
        state = 0x9E3779B97F4A7C15;
    }

    let mut out = Vec::with_capacity(len);
    for idx in 0..len {
        state = scramble_seed(state ^ (idx as u64 + 1).wrapping_mul(0x94D049BB133111EB)).max(1);
        out.push(clamp_i64_to_positive_u31(state) as f64);
    }
    let mut sample_state = state.max(1);
    (unit_from_u64(xorshift64star_next(&mut sample_state)), out)
}

fn random_generator_state_len(generator: RandomGenerator) -> usize {
    match generator {
        RandomGenerator::Xorshift64Star => 2,
        RandomGenerator::Xorshift128Plus => 4,
        RandomGenerator::Xorshift1024Star => 33,
    }
}

fn random_generator_name(generator: RandomGenerator) -> &'static str {
    match generator {
        RandomGenerator::Xorshift64Star => "Modelica.Math.Random.Generators.Xorshift64star.random",
        RandomGenerator::Xorshift128Plus => {
            "Modelica.Math.Random.Generators.Xorshift128plus.random"
        }
        RandomGenerator::Xorshift1024Star => {
            "Modelica.Math.Random.Generators.Xorshift1024star.random"
        }
    }
}

fn scramble_seed(mut x: u64) -> u64 {
    x ^= x >> 30;
    x = x.wrapping_mul(0xbf58476d1ce4e5b9);
    x ^= x >> 27;
    x = x.wrapping_mul(0x94d049bb133111eb);
    x ^ (x >> 31)
}

fn clamp_i64_to_positive_u31(v: u64) -> i64 {
    let raw = (v & 0x7FFF_FFFF) as i64;
    if raw == 0 { 1 } else { raw }
}

fn xorshift64star_next(state: &mut u64) -> u64 {
    let mut x = *state;
    if x == 0 {
        x = 0x9E3779B97F4A7C15;
    }
    x ^= x >> 12;
    x ^= x << 25;
    x ^= x >> 27;
    *state = x;
    x.wrapping_mul(0x2545F4914F6CDD1D)
}

fn unit_from_u64(sample: u64) -> f64 {
    (((sample >> 11) as f64) * (1.0 / ((1u64 << 53) as f64))).clamp(f64::EPSILON, 1.0)
}
