//! The MSL 4.1 `ModelicaRandom.c` xorshift generators over C `int` state.

/// The 64-bit word whose low and high halves are two consecutive C `int`
/// state elements (the MSL union of `int32_t[2]` and `uint64_t`).
fn word(state: &[i32], index: usize) -> u64 {
    u64::from(state[2 * index] as u32) | (u64::from(state[2 * index + 1] as u32) << 32)
}

fn store_word(state: &mut [i32], index: usize, value: u64) {
    state[2 * index] = value as u32 as i32;
    state[2 * index + 1] = (value >> 32) as u32 as i32;
}

/// `ModelicaRandom_RAND`: the word read as a signed 64-bit integer, scaled by
/// 2^-64 and shifted by 0.5, in binary64.
fn unit_interval(value: u64) -> f64 {
    // The C source spells 2^-64 as 5.42101086242752217004e-20, which rounds
    // to exactly this binary64 value.
    const INVERSE_2_POW_64: f64 = 1.0 / 18_446_744_073_709_551_616.0;
    (value as i64) as f64 * INVERSE_2_POW_64 + 0.5
}

/// `ModelicaRandom_xorshift64star` over a 2-element state.
pub(super) fn xorshift64star(state: &mut [i32]) -> f64 {
    let mut x = word(state, 0);
    x ^= x >> 12;
    x ^= x << 25;
    x ^= x >> 27;
    x = x.wrapping_mul(2_685_821_657_736_338_717);
    store_word(state, 0, x);
    unit_interval(x)
}

/// `ModelicaRandom_xorshift128plus` over a 4-element state.
pub(super) fn xorshift128plus(state: &mut [i32]) -> f64 {
    let mut s1 = word(state, 0);
    let s0 = word(state, 1);
    store_word(state, 0, s0);
    s1 ^= s1 << 23;
    let next = (s1 ^ s0 ^ (s1 >> 17) ^ (s0 >> 26)).wrapping_add(s0);
    store_word(state, 1, next);
    unit_interval(next)
}

/// `ModelicaRandom_xorshift1024star_internal` over 16 words followed by the
/// word index, which it reads modulo 16 and advances.
pub(super) fn xorshift1024star(state: &mut [i32]) -> f64 {
    let p = (state[32] & 15) as usize;
    let mut s0 = word(state, p);
    let p = (p + 1) & 15;
    let mut s1 = word(state, p);
    s1 ^= s1 << 31;
    s1 ^= s1 >> 11;
    s0 ^= s0 >> 30;
    let next = s0 ^ s1;
    store_word(state, p, next);
    state[32] = p as i32;
    unit_interval(next.wrapping_mul(1_181_783_497_276_652_981))
}
