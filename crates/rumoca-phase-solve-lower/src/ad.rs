//! AD lowering from primal linear ops to forward-mode J·v ops.

use crate::lower::{LowerError, lower_initial_residual, lower_residual};
use rumoca_ir_solve::VarLayout;
use rumoca_ir_solve::{BinaryOp, CompareOp, LinearOp, Reg, UnaryOp};
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
