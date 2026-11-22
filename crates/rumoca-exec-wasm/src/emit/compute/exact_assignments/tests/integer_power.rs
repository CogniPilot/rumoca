//! A native `TensorIntegerPower` keeps the bits of the `TensorBinary` chain
//! of its plan, in one loop whose body is the plan's, whatever the extent.
use super::tensors::artifact_with;
use super::*;
use rumoca_ir_solve::{IntegerPower, TensorInputKind};

const Y: usize = 0;
const P: usize = 1024;
const OUT: usize = 4096;

const BASES: [f64; 10] = [
    0.0,
    -0.0,
    0.5,
    -1.75,
    1.000_000_000_000_000_2,
    3.0e-5,
    -7.25e3,
    1.0e300,
    f64::INFINITY,
    f64::NEG_INFINITY,
];
const COUNT: usize = BASES.len();

fn reg(index: usize) -> Reg {
    Reg::try_from(index).expect("register fits")
}

fn program(power: IntegerPower) -> Vec<LinearOp> {
    vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: COUNT,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::TensorIntegerPower {
            dst_start: reg(COUNT),
            src_start: 0,
            count: COUNT,
            power,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: reg(COUNT),
            count: COUNT,
            stride: 1,
        },
    ]
}

#[test]
fn native_power_matches_the_plan_bit_for_bit() {
    for exponent in [-17, -3, -2, 2, 3, 4, 7, 16, 31, 63, i64::MAX, i64::MIN] {
        let power = IntegerPower::new(exponent).expect("exponent has a product");
        let artifact = artifact_with(program(power), COUNT, 0);
        let mut runner = Runner::new();
        let call = runner.instance(&artifact);
        runner.write(Y, &BASES);
        runner.write(OUT, &[77.; COUNT]);
        assert_eq!(runner.call_at(call, Y, P, OUT), 0);
        let expected: Vec<u64> = BASES
            .iter()
            .map(|&x| rumoca_core::modelica_integer_power(x, power.exponent()).to_bits())
            .collect();
        let actual = runner.read(OUT, COUNT);
        for (index, (a, b)) in actual.iter().zip(&expected).enumerate() {
            let (a, b) = (f64::from_bits(*a), f64::from_bits(*b));
            assert!(
                a.to_bits() == b.to_bits() || (a.is_nan() && b.is_nan()),
                "x ^ {exponent} at {index}: {a:e} != {b:e}"
            );
        }
    }
}
