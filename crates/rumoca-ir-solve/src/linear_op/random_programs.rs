//! Deterministic random straight-line programs and the reference run the
//! emitted C performs, shared by the tests of the program passes.

use super::*;

/// Deterministic xorshift stream, so failures replay.
pub(super) struct Stream(pub(super) u64);

impl Stream {
    pub(super) fn next(&mut self, bound: usize) -> usize {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        (self.0 % bound as u64) as usize
    }
}

const REGISTERS: usize = 24;

fn reg(stream: &mut Stream) -> Reg {
    stream.next(REGISTERS) as Reg
}

/// One random operation of a kind the slicer models; tensor ranges may
/// overlap their sources so the aliasing guard is exercised.
fn random_op(stream: &mut Stream) -> LinearOp {
    let input = |k| {
        if k == 0 {
            TensorInputKind::Y
        } else {
            TensorInputKind::P
        }
    };
    match stream.next(12) {
        0 => LinearOp::Const {
            dst: reg(stream),
            value: stream.next(97) as f64 - 48.0,
        },
        1 => LinearOp::LoadY {
            dst: reg(stream),
            index: stream.next(8),
        },
        2 => LinearOp::LoadP {
            dst: reg(stream),
            index: stream.next(8),
        },
        3 => LinearOp::Move {
            dst: reg(stream),
            src: reg(stream),
        },
        4 => LinearOp::Unary {
            dst: reg(stream),
            op: [UnaryOp::Neg, UnaryOp::Abs][stream.next(2)],
            arg: reg(stream),
        },
        5 => LinearOp::Binary {
            dst: reg(stream),
            op: [BinaryOp::Add, BinaryOp::Sub, BinaryOp::Mul, BinaryOp::Max][stream.next(4)],
            lhs: reg(stream),
            rhs: reg(stream),
        },
        6 => LinearOp::Select {
            dst: reg(stream),
            cond: reg(stream),
            if_true: reg(stream),
            if_false: reg(stream),
        },
        9 => {
            let count = 1 + stream.next(4);
            LinearOp::TensorFill {
                dst_start: stream.next(REGISTERS - count) as Reg,
                value_start: reg(stream),
                count,
                lanes: 1,
            }
        }
        11 => {
            let count = 1 + stream.next(4);
            LinearOp::TensorIntegerPower {
                dst_start: stream.next(REGISTERS - count) as Reg,
                src_start: stream.next(REGISTERS - count) as Reg,
                count,
                power: random_power(stream),
                lanes: 1,
            }
        }
        10 => {
            let (rows, columns, element_width) =
                (1 + stream.next(3), 1 + stream.next(3), 1 + stream.next(2));
            let count = rows * columns * element_width;
            LinearOp::TensorTranspose {
                dst_start: stream.next(REGISTERS - count) as Reg,
                src_start: stream.next(REGISTERS - count) as Reg,
                rows,
                columns,
                element_width,
                lanes: 1,
            }
        }
        7 => {
            let count = 1 + stream.next(4);
            LinearOp::TensorLoad {
                dst_start: stream.next(REGISTERS - count) as Reg,
                input: input(stream.next(2)),
                input_start: stream.next(4),
                count,
                seed_start: None,
                lanes: 1,
            }
        }
        _ => {
            let count = 1 + stream.next(4);
            let (lhs_stride, rhs_stride) = (stream.next(2), stream.next(2));
            LinearOp::TensorBinary {
                dst_start: stream.next(REGISTERS - count) as Reg,
                op: [BinaryOp::Add, BinaryOp::Mul][stream.next(2)],
                lhs_start: stream.next(REGISTERS - count) as Reg,
                rhs_start: stream.next(REGISTERS - count) as Reg,
                count,
                lhs_stride,
                rhs_stride,
                lanes: 1,
            }
        }
    }
}

/// A literal exponent of magnitude 2 to 9, either sign.
fn random_power(stream: &mut Stream) -> IntegerPower {
    let magnitude = 2 + stream.next(8) as i64;
    let exponent = if stream.next(2) == 0 {
        magnitude
    } else {
        -magnitude
    };
    IntegerPower::new(exponent).expect("a magnitude of at least 2")
}

/// The emitted plan of `x ^ power`: the reciprocal first for a negative
/// exponent, then the steps in order.
fn integer_power(power: IntegerPower, x: f64) -> f64 {
    let mut base = if power.reciprocal() { 1.0 / x } else { x };
    let mut result = base;
    for step in power.steps() {
        match step {
            IntegerPowerStep::Seed => result = base,
            IntegerPowerStep::Multiply => result *= base,
            IntegerPowerStep::Square => base *= base,
        }
    }
    result
}

const Y: [f64; 12] = [0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5];
const P: [f64; 12] = [
    0.0, -1.25, -2.5, -3.75, -5.0, -6.25, -7.5, -8.75, -10.0, -11.25, -12.5, -13.75,
];

fn binary(op: BinaryOp, a: f64, b: f64) -> f64 {
    match op {
        BinaryOp::Add => a + b,
        BinaryOp::Sub => a - b,
        BinaryOp::Mul => a * b,
        BinaryOp::Max => a.max(b),
        _ => unreachable!("the generator draws no other operator"),
    }
}

fn unary(op: UnaryOp, a: f64) -> f64 {
    match op {
        UnaryOp::Neg => -a,
        UnaryOp::Abs => a.abs(),
        _ => unreachable!("the generator draws no other operator"),
    }
}

fn input(kind: TensorInputKind) -> &'static [f64; 12] {
    match kind {
        TensorInputKind::Y => &Y,
        TensorInputKind::P => &P,
    }
}

/// The emitted transpose loop, element by element in its order.
fn transpose(
    r: &mut [f64],
    (dst, src): (usize, usize),
    (rows, columns, width): (usize, usize, usize),
) {
    for i in 0..rows {
        for j in 0..columns {
            for e in 0..width {
                r[dst + (i * columns + j) * width + e] = r[src + (j * rows + i) * width + e];
            }
        }
    }
}

/// One operation as the emitted C runs it; stored outputs go to `out`.
fn execute(op: &LinearOp, r: &mut [f64], out: &mut Vec<u64>) {
    match *op {
        LinearOp::Const { dst, value } => r[dst as usize] = value,
        LinearOp::LoadY { dst, index } => r[dst as usize] = Y[index],
        LinearOp::LoadP { dst, index } => r[dst as usize] = P[index],
        LinearOp::Move { dst, src } => r[dst as usize] = r[src as usize],
        LinearOp::Unary { dst, op, arg } => r[dst as usize] = unary(op, r[arg as usize]),
        LinearOp::Binary { dst, op, lhs, rhs } => {
            r[dst as usize] = binary(op, r[lhs as usize], r[rhs as usize]);
        }
        LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        } => {
            let pick = [if_false, if_true][usize::from(r[cond as usize] != 0.0)];
            r[dst as usize] = r[pick as usize];
        }
        LinearOp::TensorLoad {
            dst_start,
            input: kind,
            input_start,
            count,
            ..
        } => {
            let source = input(kind);
            for k in 0..count {
                r[dst_start as usize + k] = source[input_start + k];
            }
        }
        LinearOp::TensorBinary {
            dst_start,
            op,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            ..
        } => {
            for k in 0..count {
                let lhs = r[lhs_start as usize + k * lhs_stride];
                let rhs = r[rhs_start as usize + k * rhs_stride];
                r[dst_start as usize + k] = binary(op, lhs, rhs);
            }
        }
        LinearOp::TensorIntegerPower {
            dst_start,
            src_start,
            count,
            power,
            ..
        } => {
            for k in 0..count {
                r[dst_start as usize + k] = integer_power(power, r[src_start as usize + k]);
            }
        }
        LinearOp::TensorFill {
            dst_start,
            value_start,
            count,
            ..
        } => {
            let value = r[value_start as usize];
            r[dst_start as usize..dst_start as usize + count].fill(value);
        }
        LinearOp::TensorTranspose {
            dst_start,
            src_start,
            rows,
            columns,
            element_width,
            ..
        } => {
            transpose(
                r,
                (dst_start as usize, src_start as usize),
                (rows, columns, element_width),
            );
        }
        LinearOp::StoreOutput { src } => out.push(r[src as usize].to_bits()),
        LinearOp::StoreOutputRange {
            start,
            count,
            stride,
        } => {
            out.extend((0..count).map(|k| r[start as usize + k * stride].to_bits()));
        }
        _ => unreachable!("the generator draws no other operation"),
    }
}

/// Run a program and return its stored outputs. Registers start at
/// distinct values, so a pass that drops or misplaces a write it needs reads
/// a different value.
pub(super) fn run(program: &[LinearOp]) -> Vec<u64> {
    let mut r: Vec<f64> = (0..WIDTH).map(|k| 1000.0 + k as f64).collect();
    let mut out = Vec::new();
    for op in program {
        execute(op, &mut r, &mut out);
    }
    out
}

/// The register file `run` provides, wider than any generated program.
const WIDTH: usize = 4096;

fn random_store(stream: &mut Stream) -> LinearOp {
    if stream.next(2) == 0 {
        return LinearOp::StoreOutput { src: reg(stream) };
    }
    let count = 1 + stream.next(4);
    let stride = 1 + stream.next(2);
    LinearOp::StoreOutputRange {
        start: stream.next(REGISTERS - (count - 1) * stride) as Reg,
        count,
        stride,
    }
}

/// A random program; a store may come in the middle, so the operations after
/// it can rewrite the registers it read.
pub(super) fn random_program(stream: &mut Stream) -> Vec<LinearOp> {
    let mut program = Vec::new();
    for _ in 0..1 + stream.next(16) {
        program.push(random_op(stream));
        if stream.next(6) == 0 {
            program.push(random_store(stream));
        }
    }
    let stores = 1 + stream.next(3);
    program.extend((0..stores).map(|_| random_store(stream)));
    program
}

/// A random program whose every read is of a register written before it, as
/// lowering emits: each value takes fresh registers, a few operations
/// overwrite an earlier scalar, and range operands read earlier ranges.
pub(super) fn random_defined_program(stream: &mut Stream) -> Vec<LinearOp> {
    let mut program = Defined::default();
    for _ in 0..1 + stream.next(40) {
        program.push_random(stream);
    }
    for _ in 0..1 + stream.next(4) {
        program.push_store(stream);
    }
    program.ops
}

#[derive(Default)]
struct Defined {
    ops: Vec<LinearOp>,
    /// Written ranges `(start, len)`, in program order.
    ranges: Vec<(Reg, usize)>,
    next: Reg,
}

impl Defined {
    fn fresh(&mut self, len: usize) -> Reg {
        let start = self.next;
        self.next += len as Reg;
        self.ranges.push((start, len));
        start
    }

    /// One written scalar register, at random.
    fn scalar(&self, stream: &mut Stream) -> Reg {
        let (start, len) = self.ranges[stream.next(self.ranges.len())];
        start + stream.next(len) as Reg
    }

    /// A written range of at least `count` registers, or `None`.
    fn range(&self, stream: &mut Stream, count: usize) -> Option<Reg> {
        let wide: Vec<_> = self
            .ranges
            .iter()
            .filter(|(_, len)| *len >= count)
            .collect();
        (!wide.is_empty()).then(|| wide[stream.next(wide.len())].0)
    }

    fn scalar_dst(&mut self, stream: &mut Stream) -> Reg {
        if self.ranges.is_empty() || stream.next(5) != 0 {
            self.fresh(1)
        } else {
            self.scalar(stream)
        }
    }

    fn push_random(&mut self, stream: &mut Stream) {
        let kind = if self.ranges.is_empty() {
            stream.next(3)
        } else {
            stream.next(9)
        };
        let op = match kind {
            0 => LinearOp::Const {
                dst: self.fresh(1),
                value: stream.next(97) as f64 - 48.0,
            },
            1 => LinearOp::LoadY {
                dst: self.fresh(1),
                index: stream.next(8),
            },
            2 => {
                let count = 1 + stream.next(4);
                let input = [TensorInputKind::Y, TensorInputKind::P][stream.next(2)];
                LinearOp::TensorLoad {
                    dst_start: self.fresh(count),
                    input,
                    input_start: stream.next(4),
                    count,
                    seed_start: None,
                    lanes: 1,
                }
            }
            3 => {
                let src = self.scalar(stream);
                LinearOp::Move {
                    dst: self.scalar_dst(stream),
                    src,
                }
            }
            4 => {
                let arg = self.scalar(stream);
                LinearOp::Unary {
                    dst: self.scalar_dst(stream),
                    op: UnaryOp::Neg,
                    arg,
                }
            }
            5 | 6 => {
                let (lhs, rhs) = (self.scalar(stream), self.scalar(stream));
                let op =
                    [BinaryOp::Add, BinaryOp::Sub, BinaryOp::Mul, BinaryOp::Max][stream.next(4)];
                LinearOp::Binary {
                    dst: self.scalar_dst(stream),
                    op,
                    lhs,
                    rhs,
                }
            }
            8 => {
                let count = 1 + stream.next(4);
                match self.range(stream, count) {
                    Some(src_start) => LinearOp::TensorIntegerPower {
                        dst_start: self.fresh(count),
                        src_start,
                        count,
                        power: random_power(stream),
                        lanes: 1,
                    },
                    None => self.tensor_binary(stream),
                }
            }
            _ => self.tensor_binary(stream),
        };
        self.ops.push(op);
    }

    /// An elementwise operation over earlier ranges, broadcasting a scalar
    /// operand with stride 0.
    fn tensor_binary(&mut self, stream: &mut Stream) -> LinearOp {
        let count = 1 + stream.next(4);
        let operand = |stream: &mut Stream| match self.range(stream, count) {
            Some(start) if stream.next(3) != 0 => (start, 1),
            _ => (self.scalar(stream), 0),
        };
        let ((lhs_start, lhs_stride), (rhs_start, rhs_stride)) = (operand(stream), operand(stream));
        LinearOp::TensorBinary {
            dst_start: self.fresh(count),
            op: [BinaryOp::Add, BinaryOp::Mul][stream.next(2)],
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes: 1,
        }
    }

    fn push_store(&mut self, stream: &mut Stream) {
        let (start, len) = self.ranges[stream.next(self.ranges.len())];
        self.ops.push(if len > 1 && stream.next(2) == 0 {
            LinearOp::StoreOutputRange {
                start,
                count: len,
                stride: 1,
            }
        } else {
            LinearOp::StoreOutput {
                src: start + stream.next(len) as Reg,
            }
        });
    }
}
