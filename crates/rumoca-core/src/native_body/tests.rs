use super::*;

fn integers(values: &[i64]) -> Vec<NativeScalar> {
    values.iter().copied().map(NativeScalar::Integer).collect()
}

fn step(body: NativeBody, state: &[i64]) -> (f64, Vec<i64>) {
    let input = integers(state);
    let outputs = body
        .evaluate(&[&input])
        .expect("operands match the interface");
    let [state_out, result] = outputs.as_slice() else {
        panic!("two outputs");
    };
    let state_out = state_out
        .iter()
        .map(|value| match value {
            NativeScalar::Integer(value) => *value,
            NativeScalar::Real(_) => panic!("Integer state"),
        })
        .collect();
    let [NativeScalar::Real(result)] = result.as_slice() else {
        panic!("one Real result");
    };
    (*result, state_out)
}

/// `Modelica.Math.Random.Generators.Xorshift64star.initialState`.
fn initial_state_64(local_seed: i64, global_seed: i64) -> Vec<i64> {
    let mut state = if local_seed == 0 && global_seed == 0 {
        vec![126_247_697, global_seed]
    } else {
        vec![local_seed, global_seed]
    };
    for _ in 0..10 {
        state = step(NativeBody::Xorshift64Star, &state).1;
    }
    state
}

/// `Modelica.Math.Random.Utilities.initialStateWithXorshift64star`.
fn initial_state(local_seed: i64, global_seed: i64, n: usize) -> Vec<i64> {
    let mut state = vec![0; n];
    let aux = initial_state_64(local_seed, global_seed);
    state[..2].copy_from_slice(&aux);
    let even = 2 * (n / 2);
    for i in (3..=even).step_by(2) {
        let aux = step(NativeBody::Xorshift64Star, &state[i - 3..i - 1]).1;
        state[i - 1..=i].copy_from_slice(&aux);
    }
    if n >= 3 && n != even {
        let aux = step(NativeBody::Xorshift64Star, &state[n - 3..n - 1]).1;
        state[n - 1] = aux[0];
    }
    state
}

fn draws(body: NativeBody, mut state: Vec<i64>, count: usize) -> Vec<f64> {
    (0..count)
        .map(|_| {
            let (result, next) = step(body, &state);
            state = next;
            result
        })
        .collect()
}

/// The first draws of `Modelica.Math.Random.Examples.GenerateRandomNumbers`
/// (localSeed 614657, globalSeed 30020) as OpenModelica's run of the MSL 4.1
/// C sources reports them. The reference prints 16 significant digits, so the
/// draws are compared at that precision.
#[test]
fn xorshift_rows_reproduce_the_msl_c_sources() {
    let cases = [
        (
            NativeBody::Xorshift64Star,
            initial_state_64(614_657, 30_020),
            [
                0.3135925178776172_f64,
                0.5110345034226015,
                0.04244575388960031,
            ],
        ),
        (
            NativeBody::Xorshift128Plus,
            initial_state(614_657, 30_020, 4),
            [0.4611540572500742, 0.03170147695200792, 0.9192264762816789],
        ),
        (
            NativeBody::Xorshift1024Star,
            initial_state(614_657, 30_020, 33),
            [0.3115592868634632, 0.05995209958305187, 0.9468004399728742],
        ),
    ];
    for (body, state, expected) in cases {
        let actual = draws(body, state, expected.len());
        for (actual, expected) in actual.iter().zip(expected) {
            assert_eq!(
                format!("{actual:.15e}"),
                format!("{expected:.15e}"),
                "{body:?}"
            );
        }
    }
}

/// An Integer outside the C `int` range is narrowed modulo 2^32 before the
/// body reads it, as the C conversion at the foreign boundary does.
#[test]
fn wide_integer_operands_are_narrowed_to_c_int() {
    let narrow = step(NativeBody::Xorshift64Star, &[5, -7]);
    let wide = step(
        NativeBody::Xorshift64Star,
        &[5 + (1_i64 << 32), -7 - (3_i64 << 32)],
    );
    assert_eq!(narrow.0.to_bits(), wide.0.to_bits());
    assert_eq!(narrow.1, wide.1);
    assert!(narrow.1.iter().all(|value| i32::try_from(*value).is_ok()));
}

/// The 1024* word index is `state[33]` modulo 16 on entry and the advanced
/// index on exit.
#[test]
fn xorshift1024star_masks_and_advances_its_index() {
    let mut state = initial_state(1, 2, 33);
    state[32] = 16 * 5 + 15;
    let (masked, next) = step(NativeBody::Xorshift1024Star, &state);
    state[32] = 15;
    let (direct, _) = step(NativeBody::Xorshift1024Star, &state);
    assert_eq!(masked.to_bits(), direct.to_bits());
    assert_eq!(next[32], 0);
}

#[test]
fn entry_points_identify_their_rows_exactly() {
    for body in NativeBody::ALL {
        assert_eq!(
            NativeBody::from_c_entry_point(body.entry_point()),
            Some(body)
        );
        assert_eq!(body.inputs().count(), 1);
        assert_eq!(body.outputs().count(), 2);
    }
    assert_eq!(
        NativeBody::from_c_entry_point("ModelicaRandom_xorshift64"),
        None
    );
    assert_eq!(NativeBody::from_c_entry_point("dgesv"), None);
}

#[test]
fn operands_outside_the_interface_are_refused() {
    let body = NativeBody::Xorshift128Plus;
    let short = integers(&[1, 2]);
    let real = [NativeScalar::Real(1.0); 4];
    let error = NativeOperandMismatch { body };
    assert_eq!(body.evaluate(&[&short]), Err(error));
    assert_eq!(body.evaluate(&[&real]), Err(error));
    assert_eq!(body.evaluate(&[]), Err(error));
    assert!(error.to_string().contains("ModelicaRandom_xorshift128plus"));
}
