//! Bit-exact references for every configured method: each run is reduced to
//! a digest of every evaluation time, stage state, derivative, accepted
//! endpoint and continuous-extension sample, in the order they occur, and its
//! final endpoint is kept in full. The references were recorded from the
//! plugin before its arithmetic was read from the issued tableau.

use std::cell::RefCell;
use std::rc::Rc;

use rumoca_solver::fmi_me::ode_driver::{ContinuousOde, OdeRun, integrate_ode};

use super::*;

/// FNV-1a over the little-endian bytes of each recorded word, with the word
/// count, so two runs agree only when every recorded value agrees bit for
/// bit and in order.
#[derive(Debug)]
struct Digest {
    hash: u64,
    words: usize,
}

impl Default for Digest {
    fn default() -> Self {
        Self {
            hash: 0xcbf2_9ce4_8422_2325,
            words: 0,
        }
    }
}

impl Digest {
    fn word(&mut self, word: u64) {
        for byte in word.to_le_bytes() {
            self.hash ^= u64::from(byte);
            self.hash = self.hash.wrapping_mul(0x0000_0100_0000_01b3);
        }
        self.words += 1;
    }

    fn values(&mut self, values: &[f64]) {
        for value in values {
            self.word(value.to_bits());
        }
    }
}

/// Tags that keep the record of one kind of value from aliasing another.
const FULL: u64 = 1;
const KINEMATIC: u64 = 2;
const INTERVAL: u64 = 3;

struct Recorded {
    width: usize,
    rhs: fn(f64, &[f64], &mut [f64]),
    kinematic: Vec<usize>,
    digest: Rc<RefCell<Digest>>,
}

impl Recorded {
    fn record(&self, tag: u64, time: f64, state: &[f64], out: &[f64]) {
        let mut digest = self.digest.borrow_mut();
        digest.word(tag);
        digest.word(time.to_bits());
        digest.values(state);
        digest.values(out);
    }
}

impl ContinuousOde for Recorded {
    fn width(&self) -> usize {
        self.width
    }

    fn derivatives_into(&self, time: f64, state: &[f64], out: &mut [f64]) -> Result<(), String> {
        (self.rhs)(time, state, out);
        self.record(FULL, time, state, out);
        Ok(())
    }

    fn kinematic_states(&self) -> &[usize] {
        &self.kinematic
    }

    fn kinematic_derivatives_into(
        &self,
        time: f64,
        state: &[f64],
        out: &mut [f64],
    ) -> Result<(), String> {
        let mut full = vec![0.0; self.width];
        (self.rhs)(time, state, &mut full);
        for &index in &self.kinematic {
            out[index] = full[index];
        }
        let selected: Vec<f64> = self.kinematic.iter().map(|&index| out[index]).collect();
        self.record(KINEMATIC, time, state, &selected);
        Ok(())
    }
}

/// `x' = v`, `v' = -x`.
fn oscillator(_time: f64, state: &[f64], out: &mut [f64]) {
    out[0] = state[1];
    out[1] = -state[0];
}

/// Exact and signed zeros, held and produced: constant `-0.0` and `+0.0`
/// derivatives of signed-zero states, `-x` of `+0.0`, a zero product with a
/// moving state, and one state driven by time so the stage times matter. A
/// fold that started from `+0.0` would turn the first state into `+0.0` under
/// every method; it stays a remaining state of the partition so the
/// semi-implicit scheme folds it too.
fn signed_zeros(time: f64, state: &[f64], out: &mut [f64]) {
    out[0] = -0.0;
    out[1] = 0.0;
    out[2] = -0.0;
    out[3] = -state[3];
    out[4] = -0.0 * state[4];
    out[5] = time;
    out[6] = -0.0 * state[5];
}

/// `x' = -1500 x`: `h lambda = -1.5` at the reference step, inside every
/// method's stability interval, with explicit Euler alternating in sign and
/// the classical method decaying through the subnormals to zero.
fn decay(_time: f64, state: &[f64], out: &mut [f64]) {
    out[0] = -1500.0 * state[0];
}

struct System {
    name: &'static str,
    rhs: fn(f64, &[f64], &mut [f64]),
    initial: &'static [f64],
    kinematic: &'static [usize],
    step: f64,
    t_end: f64,
}

const SYSTEMS: [System; 3] = [
    System {
        name: "oscillator",
        rhs: oscillator,
        initial: &[1.0, 0.0],
        kinematic: &[0],
        step: 0.01,
        t_end: 12.0,
    },
    System {
        name: "signed-zeros",
        rhs: signed_zeros,
        initial: &[-0.0, -0.0, 0.0, 0.0, 1.0, 0.0, -0.0],
        kinematic: &[2, 6],
        step: 0.01,
        t_end: 12.005,
    },
    System {
        name: "decay",
        rhs: decay,
        initial: &[1.0],
        kinematic: &[],
        step: 1.0e-3,
        t_end: 1.2,
    },
];

const METHODS: [(&str, u32, FixedStepScheme); 4] = [
    ("euler", 1, FixedStepScheme::Explicit),
    ("heun", 2, FixedStepScheme::Explicit),
    ("classical", 4, FixedStepScheme::Explicit),
    ("semi-implicit-euler", 1, FixedStepScheme::SemiImplicit),
];

/// One run's digest, its step count and its final endpoint.
fn record(system: &System, order: u32, scheme: FixedStepScheme) -> (Digest, usize, Vec<u64>) {
    let digest = Rc::new(RefCell::new(Digest::default()));
    let source = Rc::new(Recorded {
        width: system.initial.len(),
        rhs: system.rhs,
        kinematic: system.kinematic.to_vec(),
        digest: Rc::clone(&digest),
    });
    let run = OdeRun {
        relative_tolerance: 1.0e-6,
        absolute_tolerance: 1.0e-9,
        nominals: vec![1.0; system.initial.len()],
        initial_step: None,
        t_start: 0.0,
        t_end: system.t_end,
        initial_state: system.initial.to_vec(),
        breakpoints: Vec::new(),
        extension_order_limit: None,
    };
    let config = FixedStepConfig {
        step: system.step,
        order,
        scheme,
    };
    let mut steps = 0;
    let mut last = Vec::new();
    let mut sample = vec![0.0; system.initial.len()];
    integrate_ode(
        source,
        &run,
        &|setup| fixed_step_integrator(setup, config).expect("a valid configuration"),
        |interval| {
            steps += 1;
            let (t0, t1) = (interval.start_time(), interval.end_time());
            let mut digest = digest.borrow_mut();
            digest.word(INTERVAL);
            digest.word(t0.to_bits());
            digest.word(t1.to_bits());
            digest.word(u64::from(interval.extension_order()));
            digest.values(interval.end_state());
            for fraction in [0.25, 0.5] {
                interval
                    .sample(t0 + fraction * (t1 - t0), &mut sample)
                    .map_err(|error| error.to_string())?;
                digest.values(&sample);
            }
            last = interval.end_state().iter().map(|v| v.to_bits()).collect();
            Ok(())
        },
    )
    .expect("the reference system integrates");
    let digest = Rc::try_unwrap(digest)
        .expect("the driver released the system")
        .into_inner();
    (digest, steps, last)
}

/// One recorded run.
struct Reference {
    system: &'static str,
    method: &'static str,
    hash: u64,
    words: usize,
    steps: usize,
    last: &'static [u64],
}

/// Recorded from the plugin before its arithmetic was read from the issued
/// tableau, in `SYSTEMS` by `METHODS` order.
const REFERENCES: [Reference; 12] = [
    Reference {
        system: "oscillator",
        method: "euler",
        hash: 0x387b_4259_d917_346c,
        words: 19_200,
        steps: 1_200,
        last: &[0x3fec_aa6d_d602_ebaa, 0x3fe2_3e56_01e5_ae90],
    },
    Reference {
        system: "oscillator",
        method: "heun",
        hash: 0x9c20_09e5_bfc9_1d13,
        words: 26_400,
        steps: 1_200,
        last: &[0x3feb_01bd_af5b_4c79, 0x3fe1_2a3a_b4a3_f084],
    },
    Reference {
        system: "oscillator",
        method: "classical",
        hash: 0x6074_4181_d21e_52c1,
        words: 40_800,
        steps: 1_200,
        last: &[0x3feb_00da_0420_b00e, 0x3fe1_2b9a_f84a_c1cb],
    },
    Reference {
        system: "oscillator",
        method: "semi-implicit-euler",
        hash: 0x5d62_4b65_8955_e63b,
        words: 25_200,
        steps: 1_200,
        last: &[0x3feb_170c_483b_eff6, 0x3fe1_2b50_8bdb_c6af],
    },
    Reference {
        system: "signed-zeros",
        method: "euler",
        hash: 0xa7c0_7578_0000_97f7,
        words: 49_241,
        steps: 1_201,
        last: &[
            0x8000_0000_0000_0000,
            0x0000_0000_0000_0000,
            0x0000_0000_0000_0000,
            0x0000_0000_0000_0000,
            0x3ff0_0000_0000_0000,
            0x4052_0000_0000_0069,
            0x8000_0000_0000_0000,
        ],
    },
    Reference {
        system: "signed-zeros",
        method: "heun",
        hash: 0xf090_a888_8797_817e,
        words: 68_457,
        steps: 1_201,
        last: &[
            0x8000_0000_0000_0000,
            0x0000_0000_0000_0000,
            0x0000_0000_0000_0000,
            0x0000_0000_0000_0000,
            0x3ff0_0000_0000_0000,
            0x4052_03d7_3eab_36e4,
            0x8000_0000_0000_0000,
        ],
    },
    Reference {
        system: "signed-zeros",
        method: "classical",
        hash: 0x71e9_7162_7542_2fe4,
        words: 106_889,
        steps: 1_201,
        last: &[
            0x8000_0000_0000_0000,
            0x0000_0000_0000_0000,
            0x0000_0000_0000_0000,
            0x0000_0000_0000_0000,
            0x3ff0_0000_0000_0000,
            0x4052_03d7_3eab_36e4,
            0x8000_0000_0000_0000,
        ],
    },
    Reference {
        system: "signed-zeros",
        method: "semi-implicit-euler",
        hash: 0x0941_f73f_b091_beed,
        words: 62_452,
        steps: 1_201,
        last: &[
            0x8000_0000_0000_0000,
            0x0000_0000_0000_0000,
            0x0000_0000_0000_0000,
            0x0000_0000_0000_0000,
            0x3ff0_0000_0000_0000,
            0x4052_0000_0000_0069,
            0x8000_0000_0000_0000,
        ],
    },
    Reference {
        system: "decay",
        method: "euler",
        hash: 0x43b2_34fb_da8d_c049,
        words: 13_200,
        steps: 1_200,
        last: &[0x0000_0000_0000_0001],
    },
    Reference {
        system: "decay",
        method: "heun",
        hash: 0xab43_b10f_2b5e_1065,
        words: 18_000,
        steps: 1_200,
        last: &[0x0d13_e2ee_65bf_599c],
    },
    Reference {
        system: "decay",
        method: "classical",
        hash: 0x410a_5bb5_7059_a8e9,
        words: 27_600,
        steps: 1_200,
        last: &[0x0000_0000_0000_0000],
    },
    Reference {
        system: "decay",
        method: "semi-implicit-euler",
        hash: 0x43b2_34fb_da8d_c049,
        words: 13_200,
        steps: 1_200,
        last: &[0x0000_0000_0000_0001],
    },
];

#[test]
fn every_method_reproduces_its_recorded_run_bit_for_bit() {
    let mut references = REFERENCES.iter();
    for system in &SYSTEMS {
        for (method, order, scheme) in METHODS {
            let reference = references.next().expect("one reference per run");
            assert_eq!((reference.system, reference.method), (system.name, method));
            let (digest, steps, last) = record(system, order, scheme);
            let run = format!("{}/{method}", system.name);
            assert!(steps >= 1_000, "{run}: {steps} steps");
            assert_eq!(steps, reference.steps, "{run}: step count");
            assert_eq!(last, reference.last, "{run}: final endpoint");
            assert_eq!(digest.words, reference.words, "{run}: recorded values");
            assert_eq!(digest.hash, reference.hash, "{run}: recorded value bits");
        }
    }
    assert!(references.next().is_none());
}

/// The references were recorded while every fold was `Iterator::sum`, whose
/// starting value is the toolchain's choice. On this toolchain an empty sum
/// is `-0.0`, and a sum of one `-0.0` stays `-0.0`, so that start is the one
/// the tableau issues and the plugin now folds from.
#[test]
fn the_sum_the_references_were_recorded_with_started_where_the_tableau_folds_from() {
    let start = rumoca_solver::fmi_me::FIXED_STEP_FOLD_START.to_bits();
    assert_eq!(std::iter::empty::<f64>().sum::<f64>().to_bits(), start);
    assert_eq!([-0.0_f64].into_iter().sum::<f64>().to_bits(), start);
    let tableau = FixedStepTableau::issue(1, FixedStepStateRoles::Uniform).unwrap();
    assert_eq!(tableau.fold_start().to_bits(), start);
}
