//! AD lowering from primal linear ops to forward-mode J·v ops.

use crate::compiled::layout::VarLayout;
use crate::compiled::linear_op::{BinaryOp, CompareOp, LinearOp, Reg, UnaryOp};
use crate::compiled::lower::{LowerError, lower_initial_residual, lower_residual};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
struct DualReg {
    re: Reg,
    du: Reg,
}

pub fn lower_residual_ad(
    dae_model: &rumoca_ir_dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let primal_rows = lower_residual(dae_model, layout)?;
    primal_rows.iter().map(|row| lower_row_ad(row)).collect()
}

pub fn lower_initial_residual_ad(
    dae_model: &rumoca_ir_dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let primal_rows = lower_initial_residual(dae_model, layout)?;
    primal_rows.iter().map(|row| lower_row_ad(row)).collect()
}

fn lower_row_ad(primal_ops: &[LinearOp]) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder = AdBuilder::new();
    for op in primal_ops {
        builder.lower_op(*op)?;
    }
    Ok(builder.ops)
}

#[derive(Default)]
struct AdBuilder {
    ops: Vec<LinearOp>,
    next_reg: Reg,
    map: HashMap<Reg, DualReg>,
    cached_zero: Option<Reg>,
    cached_one: Option<Reg>,
    cached_ln10: Option<Reg>,
    cached_inf: Option<Reg>,
    cached_two: Option<Reg>,
}

impl AdBuilder {
    fn new() -> Self {
        Self::default()
    }

    fn lower_op(&mut self, op: LinearOp) -> Result<(), LowerError> {
        match op {
            LinearOp::Const { dst, value } => self.lower_const(dst, value),
            LinearOp::LoadTime { dst } => self.lower_load_time(dst),
            LinearOp::LoadY { dst, index } => self.lower_load_y(dst, index),
            LinearOp::LoadP { dst, index } => self.lower_load_p(dst, index),
            LinearOp::LoadSeed { .. } => Err(unsupported("unexpected LoadSeed in primal row")),
            LinearOp::TableBounds { dst, table_id, max } => {
                self.lower_table_bounds(dst, table_id, max)
            }
            LinearOp::TableLookup {
                dst,
                table_id,
                column,
                input,
            } => self.lower_table_lookup(dst, table_id, column, input),
            LinearOp::TableLookupSlope { .. } => {
                Err(unsupported("unexpected TableLookupSlope in primal row"))
            }
            LinearOp::TableNextEvent {
                dst,
                table_id,
                time,
            } => self.lower_table_next_event(dst, table_id, time),
            LinearOp::Unary { dst, op, arg } => self.lower_unary(dst, op, arg),
            LinearOp::Binary { dst, op, lhs, rhs } => self.lower_binary(dst, op, lhs, rhs),
            LinearOp::Compare { dst, op, lhs, rhs } => self.lower_compare(dst, op, lhs, rhs),
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => self.lower_select(dst, cond, if_true, if_false),
            LinearOp::StoreOutput { src } => self.lower_store(src),
        }
    }

    fn lower_const(&mut self, dst: Reg, value: f64) -> Result<(), LowerError> {
        let re = self.emit_const(value);
        let du = self.zero_reg();
        self.bind(dst, DualReg { re, du })
    }

    fn lower_load_time(&mut self, dst: Reg) -> Result<(), LowerError> {
        let re = self.emit_load_time();
        let du = self.zero_reg();
        self.bind(dst, DualReg { re, du })
    }

    fn lower_load_y(&mut self, dst: Reg, index: usize) -> Result<(), LowerError> {
        let re = self.emit_load_y(index);
        let du = self.emit_load_seed(index);
        self.bind(dst, DualReg { re, du })
    }

    fn lower_load_p(&mut self, dst: Reg, index: usize) -> Result<(), LowerError> {
        let re = self.emit_load_p(index);
        let du = self.zero_reg();
        self.bind(dst, DualReg { re, du })
    }

    fn lower_table_bounds(&mut self, dst: Reg, table_id: Reg, max: bool) -> Result<(), LowerError> {
        let table = self.lookup(table_id)?;
        let re = self.emit_table_bounds(table.re, max);
        let du = self.zero_reg();
        self.bind(dst, DualReg { re, du })
    }

    fn lower_table_lookup(
        &mut self,
        dst: Reg,
        table_id: Reg,
        column: Reg,
        input: Reg,
    ) -> Result<(), LowerError> {
        let table = self.lookup(table_id)?;
        let column = self.lookup(column)?;
        let input = self.lookup(input)?;
        let re = self.emit_table_lookup(table.re, column.re, input.re);
        let slope = self.emit_table_lookup_slope(table.re, column.re, input.re);
        let du = self.emit_binary(BinaryOp::Mul, slope, input.du);
        self.bind(dst, DualReg { re, du })
    }

    fn lower_table_next_event(
        &mut self,
        dst: Reg,
        table_id: Reg,
        time: Reg,
    ) -> Result<(), LowerError> {
        let table = self.lookup(table_id)?;
        let time = self.lookup(time)?;
        let re = self.emit_table_next_event(table.re, time.re);
        let du = self.zero_reg();
        self.bind(dst, DualReg { re, du })
    }

    fn lower_unary(&mut self, dst: Reg, op: UnaryOp, arg: Reg) -> Result<(), LowerError> {
        let x = self.lookup(arg)?;
        let out = self.unary_dual(op, x)?;
        self.bind(dst, out)
    }

    fn lower_binary(
        &mut self,
        dst: Reg,
        op: BinaryOp,
        lhs: Reg,
        rhs: Reg,
    ) -> Result<(), LowerError> {
        let l = self.lookup(lhs)?;
        let r = self.lookup(rhs)?;
        let out = self.binary_dual(op, l, r)?;
        self.bind(dst, out)
    }

    fn lower_compare(
        &mut self,
        dst: Reg,
        op: CompareOp,
        lhs: Reg,
        rhs: Reg,
    ) -> Result<(), LowerError> {
        let l = self.lookup(lhs)?;
        let r = self.lookup(rhs)?;
        let re = self.emit_compare(op, l.re, r.re);
        let du = self.zero_reg();
        self.bind(dst, DualReg { re, du })
    }

    fn lower_select(
        &mut self,
        dst: Reg,
        cond: Reg,
        if_true: Reg,
        if_false: Reg,
    ) -> Result<(), LowerError> {
        let c = self.lookup(cond)?;
        let t = self.lookup(if_true)?;
        let f = self.lookup(if_false)?;
        let re = self.emit_select(c.re, t.re, f.re);
        let du = self.emit_select(c.re, t.du, f.du);
        self.bind(dst, DualReg { re, du })
    }

    fn lower_store(&mut self, src: Reg) -> Result<(), LowerError> {
        let d = self.lookup(src)?;
        self.ops.push(LinearOp::StoreOutput { src: d.du });
        Ok(())
    }

    fn unary_dual(&mut self, op: UnaryOp, x: DualReg) -> Result<DualReg, LowerError> {
        let zero = self.zero_reg();
        let out = match op {
            UnaryOp::Neg => {
                let re = self.emit_unary(UnaryOp::Neg, x.re);
                let du = self.emit_unary(UnaryOp::Neg, x.du);
                DualReg { re, du }
            }
            UnaryOp::Not => {
                let re = self.emit_unary(UnaryOp::Not, x.re);
                DualReg { re, du: zero }
            }
            UnaryOp::Abs => {
                let re = self.emit_unary(UnaryOp::Abs, x.re);
                let neg_du = self.emit_unary(UnaryOp::Neg, x.du);
                let cond = self.emit_compare(CompareOp::Ge, x.re, zero);
                let du = self.emit_select(cond, x.du, neg_du);
                DualReg { re, du }
            }
            UnaryOp::Sign | UnaryOp::Floor | UnaryOp::Ceil | UnaryOp::Trunc => {
                let re = self.emit_unary(op, x.re);
                DualReg { re, du: zero }
            }
            UnaryOp::Sin => self.unary_mul_chain(UnaryOp::Sin, UnaryOp::Cos, x),
            UnaryOp::Cos => {
                let re = self.emit_unary(UnaryOp::Cos, x.re);
                let sinx = self.emit_unary(UnaryOp::Sin, x.re);
                let neg_sinx = self.emit_unary(UnaryOp::Neg, sinx);
                let du = self.emit_binary(BinaryOp::Mul, x.du, neg_sinx);
                DualReg { re, du }
            }
            UnaryOp::Tan => {
                let re = self.emit_unary(UnaryOp::Tan, x.re);
                let cosx = self.emit_unary(UnaryOp::Cos, x.re);
                let cos_sq = self.emit_binary(BinaryOp::Mul, cosx, cosx);
                let du = self.emit_binary(BinaryOp::Div, x.du, cos_sq);
                DualReg { re, du }
            }
            UnaryOp::Asin => self.lower_asin_or_acos(x, false),
            UnaryOp::Acos => self.lower_asin_or_acos(x, true),
            UnaryOp::Atan => {
                let re = self.emit_unary(UnaryOp::Atan, x.re);
                let one = self.one_reg();
                let x_sq = self.emit_binary(BinaryOp::Mul, x.re, x.re);
                let denom = self.emit_binary(BinaryOp::Add, one, x_sq);
                let du = self.emit_binary(BinaryOp::Div, x.du, denom);
                DualReg { re, du }
            }
            UnaryOp::Sinh => self.unary_mul_chain(UnaryOp::Sinh, UnaryOp::Cosh, x),
            UnaryOp::Cosh => self.unary_mul_chain(UnaryOp::Cosh, UnaryOp::Sinh, x),
            UnaryOp::Tanh => {
                let re = self.emit_unary(UnaryOp::Tanh, x.re);
                let cosh = self.emit_unary(UnaryOp::Cosh, x.re);
                let cosh_sq = self.emit_binary(BinaryOp::Mul, cosh, cosh);
                let du = self.emit_binary(BinaryOp::Div, x.du, cosh_sq);
                DualReg { re, du }
            }
            UnaryOp::Exp => {
                let re = self.emit_unary(UnaryOp::Exp, x.re);
                let du = self.emit_binary(BinaryOp::Mul, x.du, re);
                DualReg { re, du }
            }
            UnaryOp::Log => self.lower_log_like(x, false),
            UnaryOp::Log10 => self.lower_log_like(x, true),
            UnaryOp::Sqrt => self.lower_sqrt(x),
        };
        Ok(out)
    }

    fn unary_mul_chain(&mut self, re_op: UnaryOp, deriv_op: UnaryOp, x: DualReg) -> DualReg {
        let re = self.emit_unary(re_op, x.re);
        let deriv_term = self.emit_unary(deriv_op, x.re);
        let du = self.emit_binary(BinaryOp::Mul, x.du, deriv_term);
        DualReg { re, du }
    }

    fn lower_asin_or_acos(&mut self, x: DualReg, is_acos: bool) -> DualReg {
        let re = self.emit_unary(
            if is_acos {
                UnaryOp::Acos
            } else {
                UnaryOp::Asin
            },
            x.re,
        );
        let one = self.one_reg();
        let x_sq = self.emit_binary(BinaryOp::Mul, x.re, x.re);
        let denom_sq = self.emit_binary(BinaryOp::Sub, one, x_sq);
        let denom = self.emit_unary(UnaryOp::Sqrt, denom_sq);
        let safe = self.emit_binary(BinaryOp::Div, x.du, denom);
        let signed = if is_acos {
            self.emit_unary(UnaryOp::Neg, safe)
        } else {
            safe
        };
        let zero = self.zero_reg();
        let du_zero = self.emit_compare(CompareOp::Eq, x.du, zero);
        let du = self.emit_select(du_zero, zero, signed);
        DualReg { re, du }
    }

    fn lower_log_like(&mut self, x: DualReg, is_log10: bool) -> DualReg {
        let op = if is_log10 {
            UnaryOp::Log10
        } else {
            UnaryOp::Log
        };
        let re = self.emit_unary(op, x.re);
        let zero = self.zero_reg();
        let nonzero = self.emit_compare(CompareOp::Ne, x.re, zero);
        let denom = if is_log10 {
            let ln10 = self.ln10_reg();
            self.emit_binary(BinaryOp::Mul, x.re, ln10)
        } else {
            x.re
        };
        let safe = self.emit_binary(BinaryOp::Div, x.du, denom);
        let du = self.emit_select(nonzero, safe, zero);
        DualReg { re, du }
    }

    fn lower_sqrt(&mut self, x: DualReg) -> DualReg {
        let re = self.emit_unary(UnaryOp::Sqrt, x.re);
        let zero = self.zero_reg();
        let nonzero = self.emit_compare(CompareOp::Ne, x.re, zero);
        let two = self.two_reg();
        let denom = self.emit_binary(BinaryOp::Mul, two, re);
        let safe = self.emit_binary(BinaryOp::Div, x.du, denom);
        let du = self.emit_select(nonzero, safe, zero);
        DualReg { re, du }
    }

    fn binary_dual(
        &mut self,
        op: BinaryOp,
        lhs: DualReg,
        rhs: DualReg,
    ) -> Result<DualReg, LowerError> {
        let out = match op {
            BinaryOp::Add => self.binary_add(lhs, rhs),
            BinaryOp::Sub => self.binary_sub(lhs, rhs),
            BinaryOp::Mul => self.binary_mul(lhs, rhs),
            BinaryOp::Div => self.binary_div(lhs, rhs),
            BinaryOp::Pow => self.binary_pow(lhs, rhs),
            BinaryOp::And | BinaryOp::Or => self.binary_bool(op, lhs, rhs),
            BinaryOp::Atan2 => self.binary_atan2(lhs, rhs),
            BinaryOp::Min => self.binary_minmax(lhs, rhs, false),
            BinaryOp::Max => self.binary_minmax(lhs, rhs, true),
        };
        Ok(out)
    }

    fn binary_add(&mut self, lhs: DualReg, rhs: DualReg) -> DualReg {
        let re = self.emit_binary(BinaryOp::Add, lhs.re, rhs.re);
        let du = self.emit_binary(BinaryOp::Add, lhs.du, rhs.du);
        DualReg { re, du }
    }

    fn binary_sub(&mut self, lhs: DualReg, rhs: DualReg) -> DualReg {
        let re = self.emit_binary(BinaryOp::Sub, lhs.re, rhs.re);
        let du = self.emit_binary(BinaryOp::Sub, lhs.du, rhs.du);
        DualReg { re, du }
    }

    fn binary_mul(&mut self, lhs: DualReg, rhs: DualReg) -> DualReg {
        let re = self.emit_binary(BinaryOp::Mul, lhs.re, rhs.re);
        let term1 = self.emit_binary(BinaryOp::Mul, lhs.du, rhs.re);
        let term2 = self.emit_binary(BinaryOp::Mul, lhs.re, rhs.du);
        let du = self.emit_binary(BinaryOp::Add, term1, term2);
        DualReg { re, du }
    }

    fn binary_div(&mut self, lhs: DualReg, rhs: DualReg) -> DualReg {
        let zero = self.zero_reg();
        let inf = self.inf_reg();
        let denom_zero = self.emit_compare(CompareOp::Eq, rhs.re, zero);
        let numer_zero = self.emit_compare(CompareOp::Eq, lhs.re, zero);

        let safe_re = self.emit_binary(BinaryOp::Div, lhs.re, rhs.re);
        let denom_zero_re = self.emit_select(numer_zero, zero, inf);
        let re = self.emit_select(denom_zero, denom_zero_re, safe_re);

        let term1 = self.emit_binary(BinaryOp::Mul, lhs.du, rhs.re);
        let term2 = self.emit_binary(BinaryOp::Mul, lhs.re, rhs.du);
        let numer_du = self.emit_binary(BinaryOp::Sub, term1, term2);
        let rhs_sq = self.emit_binary(BinaryOp::Mul, rhs.re, rhs.re);
        let safe_du = self.emit_binary(BinaryOp::Div, numer_du, rhs_sq);
        let du = self.emit_select(denom_zero, zero, safe_du);

        DualReg { re, du }
    }

    fn binary_pow(&mut self, lhs: DualReg, rhs: DualReg) -> DualReg {
        let re = self.emit_binary(BinaryOp::Pow, lhs.re, rhs.re);
        let du = self.lower_pow_du(lhs, rhs, re);
        DualReg { re, du }
    }

    fn lower_pow_du(&mut self, lhs: DualReg, rhs: DualReg, re: Reg) -> Reg {
        let zero = self.zero_reg();
        let one = self.one_reg();
        let rhs_du_zero = self.emit_compare(CompareOp::Eq, rhs.du, zero);

        let lhs_re_zero = self.emit_compare(CompareOp::Eq, lhs.re, zero);
        let rhs_re_one = self.emit_compare(CompareOp::Eq, rhs.re, one);
        let rhs_minus_one = self.emit_binary(BinaryOp::Sub, rhs.re, one);
        let x_pow_n_minus_1 = self.emit_binary(BinaryOp::Pow, lhs.re, rhs_minus_one);
        let n_times = self.emit_binary(BinaryOp::Mul, rhs.re, x_pow_n_minus_1);
        let const_exp_safe = self.emit_binary(BinaryOp::Mul, n_times, lhs.du);
        let lhs_zero_branch = self.emit_select(rhs_re_one, lhs.du, zero);
        let const_exp_du = self.emit_select(lhs_re_zero, lhs_zero_branch, const_exp_safe);

        let lhs_positive = self.emit_compare(CompareOp::Gt, lhs.re, zero);
        let ln_x = self.emit_unary(UnaryOp::Log, lhs.re);
        let term1 = self.emit_binary(BinaryOp::Mul, rhs.du, ln_x);
        let xprime_over_x = self.emit_binary(BinaryOp::Div, lhs.du, lhs.re);
        let term2 = self.emit_binary(BinaryOp::Mul, rhs.re, xprime_over_x);
        let sum = self.emit_binary(BinaryOp::Add, term1, term2);
        let var_exp_safe = self.emit_binary(BinaryOp::Mul, re, sum);
        let var_exp_du = self.emit_select(lhs_positive, var_exp_safe, zero);

        self.emit_select(rhs_du_zero, const_exp_du, var_exp_du)
    }

    fn binary_bool(&mut self, op: BinaryOp, lhs: DualReg, rhs: DualReg) -> DualReg {
        let re = self.emit_binary(op, lhs.re, rhs.re);
        let du = self.zero_reg();
        DualReg { re, du }
    }

    fn binary_atan2(&mut self, lhs: DualReg, rhs: DualReg) -> DualReg {
        let re = self.emit_binary(BinaryOp::Atan2, lhs.re, rhs.re);
        let term1 = self.emit_binary(BinaryOp::Mul, lhs.du, rhs.re);
        let term2 = self.emit_binary(BinaryOp::Mul, lhs.re, rhs.du);
        let numer = self.emit_binary(BinaryOp::Sub, term1, term2);
        let lhs_sq = self.emit_binary(BinaryOp::Mul, lhs.re, lhs.re);
        let rhs_sq = self.emit_binary(BinaryOp::Mul, rhs.re, rhs.re);
        let denom = self.emit_binary(BinaryOp::Add, lhs_sq, rhs_sq);
        let du = self.emit_binary(BinaryOp::Div, numer, denom);
        DualReg { re, du }
    }

    fn binary_minmax(&mut self, lhs: DualReg, rhs: DualReg, is_max: bool) -> DualReg {
        let cmp = if is_max { CompareOp::Ge } else { CompareOp::Le };
        let cond = self.emit_compare(cmp, lhs.re, rhs.re);
        let re = self.emit_select(cond, lhs.re, rhs.re);
        let du = self.emit_select(cond, lhs.du, rhs.du);
        DualReg { re, du }
    }

    fn bind(&mut self, src: Reg, dual: DualReg) -> Result<(), LowerError> {
        if self.map.insert(src, dual).is_some() {
            return Err(unsupported("duplicate destination register in primal row"));
        }
        Ok(())
    }

    fn lookup(&self, reg: Reg) -> Result<DualReg, LowerError> {
        self.map
            .get(&reg)
            .copied()
            .ok_or_else(|| unsupported("missing source register in primal row"))
    }

    fn alloc_reg(&mut self) -> Reg {
        let reg = self.next_reg;
        self.next_reg = self.next_reg.saturating_add(1);
        reg
    }

    fn emit_const(&mut self, value: f64) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::Const { dst, value });
        dst
    }

    fn emit_load_time(&mut self) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::LoadTime { dst });
        dst
    }

    fn emit_load_y(&mut self, index: usize) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::LoadY { dst, index });
        dst
    }

    fn emit_load_p(&mut self, index: usize) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::LoadP { dst, index });
        dst
    }

    fn emit_load_seed(&mut self, index: usize) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::LoadSeed { dst, index });
        dst
    }

    fn emit_table_bounds(&mut self, table_id: Reg, max: bool) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::TableBounds { dst, table_id, max });
        dst
    }

    fn emit_table_lookup(&mut self, table_id: Reg, column: Reg, input: Reg) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::TableLookup {
            dst,
            table_id,
            column,
            input,
        });
        dst
    }

    fn emit_table_lookup_slope(&mut self, table_id: Reg, column: Reg, input: Reg) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::TableLookupSlope {
            dst,
            table_id,
            column,
            input,
        });
        dst
    }

    fn emit_table_next_event(&mut self, table_id: Reg, time: Reg) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::TableNextEvent {
            dst,
            table_id,
            time,
        });
        dst
    }

    fn emit_unary(&mut self, op: UnaryOp, arg: Reg) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::Unary { dst, op, arg });
        dst
    }

    fn emit_binary(&mut self, op: BinaryOp, lhs: Reg, rhs: Reg) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::Binary { dst, op, lhs, rhs });
        dst
    }

    fn emit_compare(&mut self, op: CompareOp, lhs: Reg, rhs: Reg) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::Compare { dst, op, lhs, rhs });
        dst
    }

    fn emit_select(&mut self, cond: Reg, if_true: Reg, if_false: Reg) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        });
        dst
    }

    fn zero_reg(&mut self) -> Reg {
        if let Some(reg) = self.cached_zero {
            return reg;
        }
        let reg = self.emit_const(0.0);
        self.cached_zero = Some(reg);
        reg
    }

    fn one_reg(&mut self) -> Reg {
        if let Some(reg) = self.cached_one {
            return reg;
        }
        let reg = self.emit_const(1.0);
        self.cached_one = Some(reg);
        reg
    }

    fn ln10_reg(&mut self) -> Reg {
        if let Some(reg) = self.cached_ln10 {
            return reg;
        }
        let reg = self.emit_const(std::f64::consts::LN_10);
        self.cached_ln10 = Some(reg);
        reg
    }

    fn inf_reg(&mut self) -> Reg {
        if let Some(reg) = self.cached_inf {
            return reg;
        }
        let reg = self.emit_const(f64::INFINITY);
        self.cached_inf = Some(reg);
        reg
    }

    fn two_reg(&mut self) -> Reg {
        if let Some(reg) = self.cached_two {
            return reg;
        }
        let reg = self.emit_const(2.0);
        self.cached_two = Some(reg);
        reg
    }
}

fn unsupported(reason: &str) -> LowerError {
    LowerError::Unsupported {
        reason: reason.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::lower_residual_ad;
    use crate::compiled::layout::VarLayout;
    use crate::compiled::linear_op::{BinaryOp, CompareOp, LinearOp, Reg, UnaryOp};
    use crate::runtime::dual::Dual;
    use crate::runtime::eval::{
        VarEnv, build_env, eval_expr_dae as eval_expr, eval_table_bound_value,
        eval_table_lookup_slope_value, eval_table_lookup_value, eval_time_table_next_event_value,
        lift_env, map_var_to_env,
    };
    use rumoca_ir_dae as dae;

    fn scalar_var(name: &str) -> dae::Variable {
        dae::Variable::new(dae::VarName::new(name))
    }

    fn bool_to_real(value: bool) -> f64 {
        if value { 1.0 } else { 0.0 }
    }

    fn eq_approx(lhs: f64, rhs: f64) -> bool {
        (lhs - rhs).abs() < f64::EPSILON
    }

    fn read_reg(regs: &[f64], reg: Reg) -> f64 {
        regs.get(reg as usize).copied().unwrap_or(0.0)
    }

    fn write_reg(regs: &mut Vec<f64>, reg: Reg, value: f64) {
        let idx = reg as usize;
        if idx >= regs.len() {
            regs.resize(idx + 1, 0.0);
        }
        regs[idx] = value;
    }

    fn apply_unary(op: UnaryOp, value: f64) -> f64 {
        match op {
            UnaryOp::Neg => -value,
            UnaryOp::Not => bool_to_real(value == 0.0),
            UnaryOp::Abs => value.abs(),
            UnaryOp::Sign => match value.partial_cmp(&0.0) {
                Some(std::cmp::Ordering::Greater) => 1.0,
                Some(std::cmp::Ordering::Less) => -1.0,
                _ => 0.0,
            },
            UnaryOp::Sqrt => value.sqrt(),
            UnaryOp::Floor => value.floor(),
            UnaryOp::Ceil => value.ceil(),
            UnaryOp::Trunc => value.trunc(),
            UnaryOp::Sin => value.sin(),
            UnaryOp::Cos => value.cos(),
            UnaryOp::Tan => value.tan(),
            UnaryOp::Asin => value.asin(),
            UnaryOp::Acos => value.acos(),
            UnaryOp::Atan => value.atan(),
            UnaryOp::Sinh => value.sinh(),
            UnaryOp::Cosh => value.cosh(),
            UnaryOp::Tanh => value.tanh(),
            UnaryOp::Exp => value.exp(),
            UnaryOp::Log => value.ln(),
            UnaryOp::Log10 => value.log10(),
        }
    }

    fn apply_binary(op: BinaryOp, lhs: f64, rhs: f64) -> f64 {
        match op {
            BinaryOp::Add => lhs + rhs,
            BinaryOp::Sub => lhs - rhs,
            BinaryOp::Mul => lhs * rhs,
            BinaryOp::Div => match (rhs == 0.0, lhs == 0.0) {
                (true, true) => 0.0,
                (true, false) => f64::INFINITY,
                (false, _) => lhs / rhs,
            },
            BinaryOp::Pow => lhs.powf(rhs),
            BinaryOp::And => bool_to_real(lhs != 0.0 && rhs != 0.0),
            BinaryOp::Or => bool_to_real(lhs != 0.0 || rhs != 0.0),
            BinaryOp::Atan2 => lhs.atan2(rhs),
            BinaryOp::Min => lhs.min(rhs),
            BinaryOp::Max => lhs.max(rhs),
        }
    }

    fn apply_compare(op: CompareOp, lhs: f64, rhs: f64) -> f64 {
        let value = match op {
            CompareOp::Lt => lhs < rhs,
            CompareOp::Le => lhs <= rhs,
            CompareOp::Gt => lhs > rhs,
            CompareOp::Ge => lhs >= rhs,
            CompareOp::Eq => eq_approx(lhs, rhs),
            CompareOp::Ne => !eq_approx(lhs, rhs),
        };
        bool_to_real(value)
    }

    fn eval_row_ops(ops: &[LinearOp], y: &[f64], p: &[f64], t: f64, v: &[f64]) -> f64 {
        let mut regs = Vec::new();
        let mut out = 0.0;
        for op in ops {
            match *op {
                LinearOp::Const { dst, value } => write_reg(&mut regs, dst, value),
                LinearOp::LoadTime { dst } => write_reg(&mut regs, dst, t),
                LinearOp::LoadY { dst, index } => {
                    write_reg(&mut regs, dst, y.get(index).copied().unwrap_or(0.0))
                }
                LinearOp::LoadP { dst, index } => {
                    write_reg(&mut regs, dst, p.get(index).copied().unwrap_or(0.0))
                }
                LinearOp::LoadSeed { dst, index } => {
                    write_reg(&mut regs, dst, v.get(index).copied().unwrap_or(0.0))
                }
                LinearOp::TableBounds { dst, table_id, max } => {
                    let table_id = read_reg(&regs, table_id);
                    write_reg(&mut regs, dst, eval_table_bound_value(table_id, max));
                }
                LinearOp::TableLookup {
                    dst,
                    table_id,
                    column,
                    input,
                } => {
                    let table_id = read_reg(&regs, table_id);
                    let column = read_reg(&regs, column);
                    let input = read_reg(&regs, input);
                    write_reg(
                        &mut regs,
                        dst,
                        eval_table_lookup_value(table_id, column, input),
                    );
                }
                LinearOp::TableLookupSlope {
                    dst,
                    table_id,
                    column,
                    input,
                } => {
                    let table_id = read_reg(&regs, table_id);
                    let column = read_reg(&regs, column);
                    let input = read_reg(&regs, input);
                    write_reg(
                        &mut regs,
                        dst,
                        eval_table_lookup_slope_value(table_id, column, input),
                    );
                }
                LinearOp::TableNextEvent {
                    dst,
                    table_id,
                    time,
                } => {
                    let table_id = read_reg(&regs, table_id);
                    let time = read_reg(&regs, time);
                    write_reg(
                        &mut regs,
                        dst,
                        eval_time_table_next_event_value(table_id, time),
                    );
                }
                LinearOp::Unary { dst, op, arg } => {
                    let value = read_reg(&regs, arg);
                    write_reg(&mut regs, dst, apply_unary(op, value));
                }
                LinearOp::Binary { dst, op, lhs, rhs } => {
                    let l = read_reg(&regs, lhs);
                    let r = read_reg(&regs, rhs);
                    write_reg(&mut regs, dst, apply_binary(op, l, r));
                }
                LinearOp::Compare { dst, op, lhs, rhs } => {
                    let l = read_reg(&regs, lhs);
                    let r = read_reg(&regs, rhs);
                    write_reg(&mut regs, dst, apply_compare(op, l, r));
                }
                LinearOp::Select {
                    dst,
                    cond,
                    if_true,
                    if_false,
                } => {
                    let value = match read_reg(&regs, cond) != 0.0 {
                        true => read_reg(&regs, if_true),
                        false => read_reg(&regs, if_false),
                    };
                    write_reg(&mut regs, dst, value);
                }
                LinearOp::StoreOutput { src } => out = read_reg(&regs, src),
            }
        }
        out
    }

    fn seed_duals_from_v(dae_model: &dae::Dae, env_dual: &mut VarEnv<Dual>, v: &[f64]) {
        let mut seed_env = VarEnv::<f64>::new();
        let mut idx = 0usize;
        for (name, var) in dae_model
            .states
            .iter()
            .chain(dae_model.algebraics.iter())
            .chain(dae_model.outputs.iter())
        {
            map_var_to_env(&mut seed_env, name.as_str(), var, v, &mut idx);
        }
        for (name, du) in seed_env.vars {
            if let Some(entry) = env_dual.vars.get_mut(&name) {
                entry.du = du;
            }
        }
    }

    fn reference_row_jv(
        dae_model: &dae::Dae,
        row_index: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
    ) -> f64 {
        let env_f64 = build_env(dae_model, y, p, t);
        let mut env_dual = lift_env::<Dual>(&env_f64);
        seed_duals_from_v(dae_model, &mut env_dual, v);

        let n_x: usize = dae_model.states.values().map(|var| var.size()).sum();
        let value = eval_expr::<Dual>(&dae_model.f_x[row_index].rhs, &env_dual);
        if row_index < n_x { -value.du } else { value.du }
    }

    fn expression_var(name: &str) -> dae::Expression {
        dae::Expression::VarRef {
            name: dae::VarName::new(name),
            subscripts: vec![],
        }
    }

    fn lit(value: f64) -> dae::Expression {
        dae::Expression::Literal(dae::Literal::Real(value))
    }

    fn int_lit(value: i64) -> dae::Expression {
        dae::Expression::Literal(dae::Literal::Integer(value))
    }

    fn fn_call(name: &str, args: Vec<dae::Expression>) -> dae::Expression {
        dae::Expression::FunctionCall {
            name: dae::VarName::new(name),
            args,
            is_constructor: false,
        }
    }

    fn simple_table_expr() -> dae::Expression {
        dae::Expression::Array {
            elements: vec![
                dae::Expression::Array {
                    elements: vec![lit(0.0), lit(10.0), lit(20.0)],
                    is_matrix: false,
                },
                dae::Expression::Array {
                    elements: vec![lit(1.0), lit(12.0), lit(24.0)],
                    is_matrix: false,
                },
                dae::Expression::Array {
                    elements: vec![lit(2.0), lit(14.0), lit(28.0)],
                    is_matrix: false,
                },
            ],
            is_matrix: true,
        }
    }

    fn columns_expr() -> dae::Expression {
        dae::Expression::Array {
            elements: vec![int_lit(2), int_lit(3)],
            is_matrix: false,
        }
    }

    fn external_table1d_id() -> f64 {
        let env = VarEnv::<f64>::new();
        eval_expr::<f64>(
            &fn_call(
                "ExternalCombiTable1D",
                vec![
                    lit(0.0),
                    lit(0.0),
                    simple_table_expr(),
                    columns_expr(),
                    int_lit(1),
                    int_lit(1),
                ],
            ),
            &env,
        )
    }

    fn binary_math_expressions(
        x: &dae::Expression,
        z: &dae::Expression,
        p: &dae::Expression,
    ) -> Vec<dae::Expression> {
        vec![
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Add(Default::default()),
                lhs: Box::new(x.clone()),
                rhs: Box::new(z.clone()),
            },
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(x.clone()),
                rhs: Box::new(z.clone()),
            },
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Mul(Default::default()),
                lhs: Box::new(x.clone()),
                rhs: Box::new(z.clone()),
            },
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Div(Default::default()),
                lhs: Box::new(x.clone()),
                rhs: Box::new(z.clone()),
            },
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Exp(Default::default()),
                lhs: Box::new(x.clone()),
                rhs: Box::new(p.clone()),
            },
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Exp(Default::default()),
                lhs: Box::new(x.clone()),
                rhs: Box::new(z.clone()),
            },
        ]
    }

    fn unary_math_expressions(x: &dae::Expression, z: &dae::Expression) -> Vec<dae::Expression> {
        let unary = |function| dae::Expression::BuiltinCall {
            function,
            args: vec![x.clone()],
        };

        vec![
            unary(dae::BuiltinFunction::Abs),
            unary(dae::BuiltinFunction::Sign),
            unary(dae::BuiltinFunction::Sqrt),
            unary(dae::BuiltinFunction::Sin),
            unary(dae::BuiltinFunction::Cos),
            unary(dae::BuiltinFunction::Tan),
            unary(dae::BuiltinFunction::Asin),
            unary(dae::BuiltinFunction::Acos),
            unary(dae::BuiltinFunction::Atan),
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Atan2,
                args: vec![x.clone(), z.clone()],
            },
            unary(dae::BuiltinFunction::Sinh),
            unary(dae::BuiltinFunction::Cosh),
            unary(dae::BuiltinFunction::Tanh),
            unary(dae::BuiltinFunction::Exp),
            unary(dae::BuiltinFunction::Log),
            unary(dae::BuiltinFunction::Log10),
            unary(dae::BuiltinFunction::Floor),
            unary(dae::BuiltinFunction::Ceil),
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Min,
                args: vec![x.clone(), z.clone()],
            },
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Max,
                args: vec![x.clone(), z.clone()],
            },
        ]
    }

    fn build_supported_math_dae() -> dae::Dae {
        let mut dae_model = dae::Dae::default();
        dae_model
            .algebraics
            .insert(dae::VarName::new("x"), scalar_var("x"));
        dae_model
            .algebraics
            .insert(dae::VarName::new("z"), scalar_var("z"));
        dae_model
            .parameters
            .insert(dae::VarName::new("p"), scalar_var("p"));

        let x = expression_var("x");
        let z = expression_var("z");
        let p = expression_var("p");

        let mut expressions = binary_math_expressions(&x, &z, &p);
        expressions.extend(unary_math_expressions(&x, &z));

        for (idx, rhs) in expressions.into_iter().enumerate() {
            dae_model.f_x.push(dae::Equation::residual(
                rhs,
                Default::default(),
                format!("eq{idx}"),
            ));
        }
        dae_model
    }

    fn assert_rows_match_dual_reference(
        dae_model: &dae::Dae,
        rows: &[Vec<LinearOp>],
        y: &[f64],
        p_vals: &[f64],
        t: f64,
        seed: &[f64],
    ) {
        for (i, row) in rows.iter().enumerate() {
            let compiled = eval_row_ops(row, y, p_vals, t, seed);
            let reference = reference_row_jv(dae_model, i, y, p_vals, t, seed);
            if compiled.is_nan() || reference.is_nan() {
                assert_eq!(compiled.is_nan(), reference.is_nan());
            } else {
                assert!((compiled - reference).abs() <= 1e-10, "row {i}");
            }
        }
    }

    #[test]
    fn ad_chain_rules_match_dual_reference_for_supported_math() {
        let dae_model = build_supported_math_dae();
        let layout = VarLayout::from_dae(&dae_model);
        let rows = lower_residual_ad(&dae_model, &layout).expect("AD lowering should succeed");
        let y = vec![0.4, 0.8];
        let p_vals = vec![2.0];
        let seed = vec![1.3, -0.7];
        let t = 0.0;

        assert_rows_match_dual_reference(&dae_model, &rows, &y, &p_vals, t, &seed);
    }

    #[test]
    fn ad_singularity_guards_match_dual_reference() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .algebraics
            .insert(dae::VarName::new("x"), scalar_var("x"));
        dae_model
            .algebraics
            .insert(dae::VarName::new("z"), scalar_var("z"));

        let x = expression_var("x");
        let z = expression_var("z");
        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Div(Default::default()),
                lhs: Box::new(x.clone()),
                rhs: Box::new(z.clone()),
            },
            Default::default(),
            "div_zero",
        ));
        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Log,
                args: vec![x.clone()],
            },
            Default::default(),
            "log_zero",
        ));
        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Asin,
                args: vec![x.clone()],
            },
            Default::default(),
            "asin_singularity_constant",
        ));

        let layout = VarLayout::from_dae(&dae_model);
        let rows = lower_residual_ad(&dae_model, &layout).expect("AD lowering should succeed");
        let y = vec![0.0, 0.0];
        let p_vals = vec![];
        let seed = vec![0.0, 2.0];
        let t = 0.0;

        let div_du = eval_row_ops(&rows[0], &y, &p_vals, t, &seed);
        let log_du = eval_row_ops(&rows[1], &y, &p_vals, t, &seed);
        assert_eq!(div_du, 0.0);
        assert_eq!(log_du, 0.0);

        let y_asin = vec![1.0, 0.0];
        let seed_asin = vec![0.0, 0.0];
        let asin_du = eval_row_ops(&rows[2], &y_asin, &p_vals, t, &seed_asin);
        assert_eq!(asin_du, 0.0);
    }

    #[test]
    fn lower_residual_ad_matches_reference_jacobian_vector_product() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .states
            .insert(dae::VarName::new("x"), scalar_var("x"));
        dae_model
            .algebraics
            .insert(dae::VarName::new("z"), scalar_var("z"));

        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Add(Default::default()),
                lhs: Box::new(expression_var("x")),
                rhs: Box::new(expression_var("z")),
            },
            Default::default(),
            "state_row",
        ));
        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Add(Default::default()),
                lhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Sin,
                    args: vec![expression_var("x")],
                }),
                rhs: Box::new(dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Mul(Default::default()),
                    lhs: Box::new(expression_var("z")),
                    rhs: Box::new(expression_var("z")),
                }),
            },
            Default::default(),
            "alg_row",
        ));

        let layout = VarLayout::from_dae(&dae_model);
        let rows = lower_residual_ad(&dae_model, &layout).expect("AD lowering should succeed");

        let y = vec![0.2, 0.7];
        let p_vals = vec![];
        let t = 0.5;
        let seed = vec![1.1, -0.4];

        for (i, row) in rows.iter().enumerate() {
            let compiled = eval_row_ops(row, &y, &p_vals, t, &seed);
            let reference = reference_row_jv(&dae_model, i, &y, &p_vals, t, &seed);
            assert!((compiled - reference).abs() <= 1e-10, "row {i}");
        }
    }

    #[test]
    fn lower_residual_ad_supports_host_backed_table_ops() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .algebraics
            .insert(dae::VarName::new("u"), scalar_var("u"));
        dae_model
            .parameters
            .insert(dae::VarName::new("table_id"), scalar_var("table_id"));

        let table_id = expression_var("table_id");
        let u = expression_var("u");
        let lookup = fn_call(
            "getTable1DValueNoDer",
            vec![table_id.clone(), int_lit(1), u],
        );
        let tmax = fn_call("getTable1DAbscissaUmax", vec![table_id.clone()]);
        let next_event = fn_call("getNextTimeEvent", vec![table_id, lit(0.25)]);
        let rhs = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Add(Default::default()),
                lhs: Box::new(lookup),
                rhs: Box::new(tmax),
            }),
            rhs: Box::new(next_event),
        };
        dae_model.f_x.push(dae::Equation::residual(
            rhs,
            Default::default(),
            "table_row",
        ));

        let layout = VarLayout::from_dae(&dae_model);
        let rows = lower_residual_ad(&dae_model, &layout)
            .expect("AD lowering should support host-backed table ops");

        let y = vec![1.25];
        let p_vals = vec![external_table1d_id()];
        let seed = vec![1.0];
        let t = 0.0;

        assert_rows_match_dual_reference(&dae_model, &rows, &y, &p_vals, t, &seed);
    }
}
