//! An elementwise literal power over the private register arena.
//!
//! One loop over the elements runs the plan of the operation's
//! `IntegerPower` on two live values per lane: the running base in a scratch
//! local and the running result in the element's destination cells. Each
//! step is the instruction sequence the arena emits for the `TensorBinary`
//! division and product of that step, so every element is the chain's value
//! bit for bit and the code size is the plan's, independent of the extent.

use super::*;
use rumoca_ir_solve::IntegerPowerStep;

/// The running base primal and, for two lanes, its tangent.
const BASE_RE: u32 = LOCAL_BASE + 1;
const BASE_DU: u32 = LOCAL_BASE;

impl BodyEmitter<'_> {
    pub(super) fn arena_integer_power(&mut self, operation: LinearOp) -> Result<(), String> {
        let LinearOp::TensorIntegerPower {
            dst_start,
            src_start,
            count,
            power,
            lanes,
        } = operation
        else {
            return Err("expected TensorIntegerPower".into());
        };
        if !(1..=2).contains(&lanes) {
            return Err("native arena integer power requires one or two lanes".into());
        }
        let counter = self.arena_plan()?.counter;
        let element = Element {
            dst: dst_start,
            src: src_start,
            lanes,
            counter,
        };
        self.loop_start(counter, count)?;
        if power.reciprocal() {
            self.power_reciprocal(&element)?;
        } else {
            self.power_read(element.src, 0, &element)?;
            self.push(Instruction::LocalSet(BASE_RE));
            if lanes == 2 {
                self.power_read(element.src, 1, &element)?;
                self.push(Instruction::LocalSet(BASE_DU));
            }
        }
        for step in power.steps() {
            self.power_step(step, &element)?;
        }
        self.loop_end(counter);
        Ok(())
    }

    fn power_read(&mut self, start: Reg, lane: u32, element: &Element) -> Result<(), String> {
        self.push_arena_address(start + lane, Some(element.counter), element.lanes)?;
        self.push(Instruction::F64Load(memarg()));
        Ok(())
    }

    fn power_address(&mut self, lane: u32, element: &Element) -> Result<(), String> {
        self.push_arena_address(element.dst + lane, Some(element.counter), element.lanes)
    }

    /// The base `1 / x`, as the arena divides a broadcast constant `1` with a
    /// zero tangent by the element.
    fn power_reciprocal(&mut self, element: &Element) -> Result<(), String> {
        if element.lanes == 2 {
            // `dual_division_tangent` with the numerator lanes `(1, 0)`.
            self.push(Instruction::F64Const(0.0.into()));
            self.push(Instruction::F64Const(1.0.into()));
            self.power_read(element.src, 0, element)?;
            self.push(Instruction::F64Div);
            self.dual_finite_or_zero();
            self.push(Instruction::F64Mul);
            self.power_read(element.src, 1, element)?;
            self.push(Instruction::F64Const(1.0.into()));
            self.push(Instruction::F64Neg);
            self.power_read(element.src, 0, element)?;
            self.power_read(element.src, 0, element)?;
            self.push(Instruction::F64Mul);
            self.push(Instruction::F64Div);
            self.dual_finite_or_zero();
            self.push(Instruction::F64Mul);
            self.push(Instruction::F64Add);
            self.push(Instruction::LocalSet(BASE_DU));
        }
        self.push(Instruction::F64Const(1.0.into()));
        self.power_read(element.src, 0, element)?;
        self.push(Instruction::F64Div);
        if element.lanes == 2 {
            // The two-lane quotient's `0 / 0` guard, as `dual_binary_lane`.
            self.push(Instruction::F64Const(0.0.into()));
            self.push(Instruction::F64Const(1.0.into()));
            self.push(Instruction::F64Const(0.0.into()));
            self.push(Instruction::F64Ne);
            self.power_read(element.src, 0, element)?;
            self.push(Instruction::F64Const(0.0.into()));
            self.push(Instruction::F64Ne);
            self.push(Instruction::I32Or);
            self.push(Instruction::Select);
        }
        self.push(Instruction::LocalSet(BASE_RE));
        Ok(())
    }

    fn power_step(&mut self, step: IntegerPowerStep, element: &Element) -> Result<(), String> {
        let dual = element.lanes == 2;
        match step {
            IntegerPowerStep::Seed => {
                self.power_address(0, element)?;
                self.push(Instruction::LocalGet(BASE_RE));
                self.push(Instruction::F64Store(memarg()));
                if dual {
                    self.power_address(1, element)?;
                    self.push(Instruction::LocalGet(BASE_DU));
                    self.push(Instruction::F64Store(memarg()));
                }
            }
            IntegerPowerStep::Multiply => {
                // The tangent reads the result primal before it is replaced.
                if dual {
                    self.power_address(1, element)?;
                    self.power_read(element.dst, 1, element)?;
                    self.push(Instruction::LocalGet(BASE_RE));
                    self.push(Instruction::F64Mul);
                    self.power_read(element.dst, 0, element)?;
                    self.push(Instruction::LocalGet(BASE_DU));
                    self.push(Instruction::F64Mul);
                    self.push(Instruction::F64Add);
                    self.push(Instruction::F64Store(memarg()));
                }
                self.power_address(0, element)?;
                self.power_read(element.dst, 0, element)?;
                self.push(Instruction::LocalGet(BASE_RE));
                self.push(Instruction::F64Mul);
                self.push(Instruction::F64Store(memarg()));
            }
            IntegerPowerStep::Square => {
                if dual {
                    self.push(Instruction::LocalGet(BASE_DU));
                    self.push(Instruction::LocalGet(BASE_RE));
                    self.push(Instruction::F64Mul);
                    self.push(Instruction::LocalGet(BASE_RE));
                    self.push(Instruction::LocalGet(BASE_DU));
                    self.push(Instruction::F64Mul);
                    self.push(Instruction::F64Add);
                    self.push(Instruction::LocalSet(BASE_DU));
                }
                self.push(Instruction::LocalGet(BASE_RE));
                self.push(Instruction::LocalGet(BASE_RE));
                self.push(Instruction::F64Mul);
                self.push(Instruction::LocalSet(BASE_RE));
            }
        }
        Ok(())
    }
}

/// The arena cells of the element the loop counter selects.
struct Element {
    dst: Reg,
    src: Reg,
    lanes: usize,
    counter: u32,
}
