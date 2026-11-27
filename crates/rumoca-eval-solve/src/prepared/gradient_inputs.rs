//! What each column of a row's solver-Y gradient reads.
//!
//! The parameter-static certificate (`dependency::parameter_static_y_gradient`)
//! says a row's whole gradient reads parameters only. A block that solves a
//! subset of Y needs less: only the columns of its own unknowns, and those may
//! read solver coordinates the block does not solve. `v - R*i` with `R` a
//! coordinate has the columns `dv: {}`, `di: {R}`, `dR: {i}`; a block solving
//! `v` and `i` reads `R` alone, which is fixed while the block is solved.
use std::collections::{BTreeMap, BTreeSet};

use rumoca_ir_solve::{BinaryOp, LinearOp, UnaryOp};

/// The largest input set tracked for one value or one column; a row beyond
/// it is declined rather than carried.
const INPUT_BOUND: usize = 64;

/// One nonzero column of a row's solver-Y gradient: the coordinate it
/// differentiates by, and what the entry's value reads besides parameters.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct YGradientColumn {
    /// The solver coordinate the row is differentiated by.
    pub column: usize,
    /// The solver coordinates the entry's value reads, ascending.
    pub y_inputs: Box<[usize]>,
    /// Whether the entry's value reads time.
    pub reads_time: bool,
}

#[derive(Clone, Default, PartialEq, Eq)]
struct Inputs {
    y: BTreeSet<usize>,
    time: bool,
}

impl Inputs {
    fn union(&self, other: &Self) -> Self {
        Self {
            y: self.y.union(&other.y).copied().collect(),
            time: self.time || other.time,
        }
    }
}

/// A register: what its value reads, and per differentiated coordinate what
/// that derivative reads. A coordinate absent from `columns` has derivative 0.
#[derive(Clone, Default)]
struct Register {
    value: Inputs,
    columns: BTreeMap<usize, Inputs>,
}

impl Register {
    /// Every column of either operand, each widened by `extra`.
    fn columns_with(lhs: &Self, lhs_extra: &Inputs, rhs: &Self, rhs_extra: &Inputs) -> Self {
        let mut columns: BTreeMap<usize, Inputs> = BTreeMap::new();
        for (source, extra) in [(lhs, lhs_extra), (rhs, rhs_extra)] {
            for (&column, inputs) in &source.columns {
                let widened = inputs.union(extra);
                columns
                    .entry(column)
                    .and_modify(|held| *held = held.union(&widened))
                    .or_insert(widened);
            }
        }
        Self {
            value: lhs.value.union(&rhs.value),
            columns,
        }
    }

    /// Time: a value with no solver-Y derivative.
    fn time() -> Self {
        Self {
            value: Inputs {
                y: BTreeSet::new(),
                time: true,
            },
            columns: BTreeMap::new(),
        }
    }

    /// The solver coordinate `index`: its one column is the constant 1.
    fn coordinate(index: usize) -> Self {
        Self {
            value: Inputs {
                y: BTreeSet::from([index]),
                time: false,
            },
            columns: BTreeMap::from([(index, Inputs::default())]),
        }
    }

    fn within_bound(&self) -> bool {
        self.value.y.len() <= INPUT_BOUND
            && self
                .columns
                .values()
                .all(|inputs| inputs.y.len() <= INPUT_BOUND)
    }
}

/// The nonzero columns of `program`'s solver-Y gradient with what each reads,
/// ascending by column; `None` when the program has an operation outside the
/// plain scalar arithmetic modelled here, more than one output, or an input
/// set past the bound.
#[must_use]
pub fn y_gradient_inputs(program: &[LinearOp]) -> Option<Vec<YGradientColumn>> {
    let mut registers: BTreeMap<u32, Register> = BTreeMap::new();
    let mut output: Option<Register> = None;
    let none = Inputs::default();
    for op in program {
        let (dst, register) = match *op {
            LinearOp::Const { dst, .. } | LinearOp::LoadP { dst, .. } => (dst, Register::default()),
            LinearOp::LoadTime { dst } => (dst, Register::time()),
            LinearOp::LoadY { dst, index } => (dst, Register::coordinate(index)),
            LinearOp::Move { dst, src } => (dst, registers.get(&src)?.clone()),
            LinearOp::Unary { dst, op, arg } => {
                let arg = registers.get(&arg)?;
                // `-a` keeps `a`'s columns; any other function scales each
                // column by a factor that reads `a`'s value.
                let extra = if matches!(op, UnaryOp::Neg) {
                    &none
                } else {
                    &arg.value
                };
                (
                    dst,
                    Register::columns_with(arg, extra, &Register::default(), &none),
                )
            }
            LinearOp::Binary { dst, op, lhs, rhs } => {
                let (lhs, rhs) = (registers.get(&lhs)?, registers.get(&rhs)?);
                let both = lhs.value.union(&rhs.value);
                let register = match op {
                    BinaryOp::Add | BinaryOp::Sub => Register::columns_with(lhs, &none, rhs, &none),
                    // d(a*b) = da*b + a*db
                    BinaryOp::Mul => Register::columns_with(lhs, &rhs.value, rhs, &lhs.value),
                    // d(a/b) = da/b - a*db/b^2
                    BinaryOp::Div => Register::columns_with(lhs, &rhs.value, rhs, &both),
                    BinaryOp::Pow
                    | BinaryOp::And
                    | BinaryOp::Or
                    | BinaryOp::Atan2
                    | BinaryOp::Min
                    | BinaryOp::Max => Register::columns_with(lhs, &both, rhs, &both),
                };
                (dst, register)
            }
            // A relation has no gradient; its value reads both sides.
            LinearOp::Compare { dst, lhs, rhs, .. } => (
                dst,
                Register {
                    value: registers
                        .get(&lhs)?
                        .value
                        .union(&registers.get(&rhs)?.value),
                    columns: BTreeMap::new(),
                },
            ),
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                let cond = &registers.get(&cond)?.value;
                let (if_true, if_false) = (registers.get(&if_true)?, registers.get(&if_false)?);
                // which branch's column is taken reads the condition
                let mut register = Register::columns_with(if_true, cond, if_false, cond);
                register.value = register.value.union(cond);
                (dst, register)
            }
            LinearOp::StoreOutput { src } => {
                if output.is_some() {
                    return None;
                }
                output = Some(registers.get(&src)?.clone());
                continue;
            }
            _ => return None,
        };
        if !register.within_bound() {
            return None;
        }
        registers.insert(dst, register);
    }
    Some(
        output?
            .columns
            .into_iter()
            .map(|(column, inputs)| YGradientColumn {
                column,
                y_inputs: inputs.y.into_iter().collect(),
                reads_time: inputs.time,
            })
            .collect(),
    )
}

/// Whether a column reads nothing but parameters.
impl YGradientColumn {
    #[must_use]
    pub fn is_parameter_only(&self) -> bool {
        self.y_inputs.is_empty() && !self.reads_time
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn load_y(dst: u32, index: usize) -> LinearOp {
        LinearOp::LoadY { dst, index }
    }

    fn binary(dst: u32, op: BinaryOp, lhs: u32, rhs: u32) -> LinearOp {
        LinearOp::Binary { dst, op, lhs, rhs }
    }

    fn column(column: usize, y_inputs: &[usize], reads_time: bool) -> YGradientColumn {
        YGradientColumn {
            column,
            y_inputs: y_inputs.into(),
            reads_time,
        }
    }

    /// `v - R*i` with `v`, `i`, `R` the coordinates 0, 1, 2.
    fn resistor() -> Vec<LinearOp> {
        vec![
            load_y(0, 0),
            load_y(1, 1),
            load_y(2, 2),
            binary(3, BinaryOp::Mul, 2, 1),
            binary(4, BinaryOp::Sub, 0, 3),
            LinearOp::StoreOutput { src: 4 },
        ]
    }

    #[test]
    fn a_coordinate_coefficient_is_read_by_the_column_it_scales() {
        assert_eq!(
            y_gradient_inputs(&resistor()).unwrap(),
            vec![
                column(0, &[], false),
                column(1, &[2], false),
                column(2, &[1], false)
            ]
        );
    }

    #[test]
    fn a_parameter_coefficient_leaves_every_column_parameter_only() {
        let program = vec![
            load_y(0, 0),
            load_y(1, 1),
            LinearOp::LoadP { dst: 2, index: 5 },
            binary(3, BinaryOp::Mul, 2, 1),
            binary(4, BinaryOp::Sub, 0, 3),
            LinearOp::StoreOutput { src: 4 },
        ];
        let columns = y_gradient_inputs(&program).unwrap();
        assert_eq!(columns, vec![column(0, &[], false), column(1, &[], false)]);
        assert!(columns.iter().all(YGradientColumn::is_parameter_only));
    }

    #[test]
    fn a_time_dependent_coefficient_marks_its_column() {
        let program = vec![
            load_y(0, 0),
            LinearOp::LoadTime { dst: 1 },
            binary(2, BinaryOp::Mul, 1, 0),
            LinearOp::StoreOutput { src: 2 },
        ];
        assert_eq!(
            y_gradient_inputs(&program).unwrap(),
            vec![column(0, &[], true)]
        );
    }

    #[test]
    fn a_quotient_s_columns_read_the_divisor_and_the_divisor_s_column_both() {
        // y0 / y1
        let program = vec![
            load_y(0, 0),
            load_y(1, 1),
            binary(2, BinaryOp::Div, 0, 1),
            LinearOp::StoreOutput { src: 2 },
        ];
        assert_eq!(
            y_gradient_inputs(&program).unwrap(),
            vec![column(0, &[1], false), column(1, &[0, 1], false)]
        );
    }

    #[test]
    fn a_function_of_a_coordinate_reads_that_coordinate_in_its_column() {
        let program = vec![
            load_y(0, 0),
            LinearOp::Unary {
                dst: 1,
                op: UnaryOp::Sin,
                arg: 0,
            },
            LinearOp::Unary {
                dst: 2,
                op: UnaryOp::Neg,
                arg: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ];
        assert_eq!(
            y_gradient_inputs(&program).unwrap(),
            vec![column(0, &[0], false)]
        );
    }

    #[test]
    fn a_selected_column_reads_the_condition() {
        // if y2 > 0 then y0 else y1
        let program = vec![
            load_y(0, 0),
            load_y(1, 1),
            load_y(2, 2),
            LinearOp::Const { dst: 3, value: 0.0 },
            LinearOp::Compare {
                dst: 4,
                op: rumoca_ir_solve::CompareOp::Gt,
                lhs: 2,
                rhs: 3,
            },
            LinearOp::Select {
                dst: 5,
                cond: 4,
                if_true: 0,
                if_false: 1,
            },
            LinearOp::StoreOutput { src: 5 },
        ];
        assert_eq!(
            y_gradient_inputs(&program).unwrap(),
            vec![column(0, &[2], false), column(1, &[2], false)]
        );
    }

    #[test]
    fn an_unmodelled_operation_or_a_second_output_declines_the_row() {
        let mut two_outputs = resistor();
        two_outputs.push(LinearOp::StoreOutput { src: 4 });
        assert_eq!(y_gradient_inputs(&two_outputs), None);
        let seeded = vec![
            LinearOp::LoadSeed { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ];
        assert_eq!(y_gradient_inputs(&seeded), None);
    }
}
