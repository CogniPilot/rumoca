//! Lower flat expressions and DAE residual rows to linear ops.

use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use rumoca_ir_solve::{BinaryOp, CompareOp, LinearOp, Reg, UnaryOp};
use rumoca_ir_solve::{ScalarSlot, VarLayout};

mod array_values;
mod expression_rows;
mod function_calls;
mod function_projection;
mod helpers;
mod root_conditions;
#[cfg(test)]
mod tests;

pub use expression_rows::{
    lower_expression_rows_from_expressions,
    lower_expression_rows_from_expressions_with_runtime_metadata,
    lower_initial_expression_rows_from_expressions,
    lower_initial_expression_rows_from_expressions_with_runtime_metadata,
};
use function_projection::format_subscript_binding_key;
use helpers::*;

const MAX_FUNCTION_INLINE_DEPTH: usize = 64;
const NAMED_FUNCTION_ARG_PREFIX: &str = "__rumoca_named_arg__.";

type Scope = IndexMap<String, Reg>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LowerError {
    Unsupported { reason: String },
    MissingBinding { name: String },
    MissingFunction { name: String },
    InvalidFunction { name: String, reason: String },
}

impl std::fmt::Display for LowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unsupported { reason } => write!(f, "unsupported expression: {reason}"),
            Self::MissingBinding { name } => write!(f, "missing variable binding: {name}"),
            Self::MissingFunction { name } => write!(f, "missing function definition: {name}"),
            Self::InvalidFunction { name, reason } => {
                write!(f, "invalid function `{name}`: {reason}")
            }
        }
    }
}

impl std::error::Error for LowerError {}

#[derive(Debug, Clone, PartialEq)]
pub struct LoweredExpression {
    pub ops: Vec<LinearOp>,
    pub result: Reg,
}

pub fn lower_expression(
    expr: &dae::Expression,
    layout: &VarLayout,
    functions: &IndexMap<dae::VarName, dae::Function>,
) -> Result<LoweredExpression, LowerError> {
    let mut builder = LowerBuilder::new(layout, functions);
    let scope = Scope::new();
    let result = builder.lower_expr(expr, &scope, 0)?;
    Ok(LoweredExpression {
        ops: builder.ops,
        result,
    })
}

pub fn lower_residual(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    expression_rows::lower_residual_rows_with_mode(dae_model, layout, false)
}

pub fn lower_initial_residual(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    expression_rows::lower_residual_rows_with_mode(dae_model, layout, true)
}

pub fn lower_discrete_rhs(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    expression_rows::lower_expression_rows_with_mode(
        dae_model.f_z.iter().chain(dae_model.f_m.iter()),
        layout,
        &dae_model.functions,
        &dae_model.clock_intervals,
        false,
    )
}

pub fn lower_root_conditions(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    root_conditions::lower_root_conditions(dae_model, layout)
}

struct LowerBuilder<'a> {
    layout: &'a VarLayout,
    functions: &'a IndexMap<dae::VarName, dae::Function>,
    clock_intervals: Option<&'a IndexMap<String, f64>>,
    indexed_bindings: IndexMap<String, Vec<IndexedBinding>>,
    is_initial_mode: bool,
    ops: Vec<LinearOp>,
    next_reg: Reg,
}

#[derive(Debug, Clone)]
struct IndexedBinding {
    slot: ScalarSlot,
    indices: Vec<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DynamicSubscriptSemantics {
    VarRef,
    Index,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SubscriptEvalMode {
    Truncate,
    Round,
}

impl<'a> LowerBuilder<'a> {
    fn new(layout: &'a VarLayout, functions: &'a IndexMap<dae::VarName, dae::Function>) -> Self {
        Self::new_with_metadata(layout, functions, None, false)
    }

    fn new_with_mode(
        layout: &'a VarLayout,
        functions: &'a IndexMap<dae::VarName, dae::Function>,
        is_initial_mode: bool,
    ) -> Self {
        Self::new_with_metadata(layout, functions, None, is_initial_mode)
    }

    fn new_with_runtime_metadata(
        layout: &'a VarLayout,
        functions: &'a IndexMap<dae::VarName, dae::Function>,
        clock_intervals: &'a IndexMap<String, f64>,
        is_initial_mode: bool,
    ) -> Self {
        Self::new_with_metadata(layout, functions, Some(clock_intervals), is_initial_mode)
    }

    fn new_with_metadata(
        layout: &'a VarLayout,
        functions: &'a IndexMap<dae::VarName, dae::Function>,
        clock_intervals: Option<&'a IndexMap<String, f64>>,
        is_initial_mode: bool,
    ) -> Self {
        Self {
            layout,
            functions,
            clock_intervals,
            indexed_bindings: build_indexed_binding_map(layout),
            is_initial_mode,
            ops: Vec::new(),
            next_reg: 0,
        }
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

    fn lower_expr(
        &mut self,
        expr: &dae::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        match expr {
            dae::Expression::Literal(lit) => Ok(self.emit_const(eval_literal(lit))),
            dae::Expression::VarRef { name, subscripts } => {
                self.lower_var_ref(name, subscripts, scope, call_depth)
            }
            dae::Expression::Binary { op, lhs, rhs } => {
                let l = self.lower_expr(lhs, scope, call_depth)?;
                let r = self.lower_expr(rhs, scope, call_depth)?;
                self.lower_binary(op.clone(), l, r)
            }
            dae::Expression::Unary { op, rhs } => {
                let r = self.lower_expr(rhs, scope, call_depth)?;
                self.lower_unary(op.clone(), r)
            }
            dae::Expression::BuiltinCall { function, args } => {
                self.lower_builtin(*function, args, scope, call_depth)
            }
            dae::Expression::If {
                branches,
                else_branch,
            } => self.lower_if(branches, else_branch, scope, call_depth),
            dae::Expression::FunctionCall {
                name,
                args,
                is_constructor,
            } => self.lower_function_call(name, args, *is_constructor, scope, call_depth),
            dae::Expression::FieldAccess { base, field } => {
                self.lower_field_access(base, field, scope, call_depth)
            }
            dae::Expression::Index { base, subscripts } => {
                self.lower_index(base, subscripts, scope, call_depth)
            }
            dae::Expression::Empty => Ok(self.emit_const(0.0)),
            dae::Expression::Array { elements, .. } => {
                if let Some(first) = elements.first() {
                    self.lower_expr(first, scope, call_depth)
                } else {
                    Ok(self.emit_const(0.0))
                }
            }
            dae::Expression::Tuple { elements } => {
                if let Some(first) = elements.first() {
                    self.lower_expr(first, scope, call_depth)
                } else {
                    Ok(self.emit_const(0.0))
                }
            }
            dae::Expression::Range { .. } | dae::Expression::ArrayComprehension { .. } => {
                Ok(self.emit_const(0.0))
            }
        }
    }

    fn lower_var_ref(
        &mut self,
        name: &dae::VarName,
        subscripts: &[dae::Subscript],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if subscripts.is_empty()
            && let Some(reg) = scope.get(name.as_str()).copied()
        {
            return Ok(reg);
        }

        let local_static_key = static_subscript_indices(subscripts)?
            .and_then(|indices| (!indices.is_empty()).then_some(indices))
            .map(|indices| format_subscript_binding_key(name.as_str(), &indices));
        if let Some(local_key) = local_static_key
            && let Some(reg) = scope.get(&local_key).copied()
        {
            return Ok(reg);
        }

        if !subscripts.is_empty() && scope.contains_key(name.as_str()) {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "subscripted local variable references are unsupported: {}[...]",
                    name.as_str()
                ),
            });
        }

        let base_name = name.as_str().to_string();
        if let Some(indices) = static_subscript_indices(subscripts)? {
            let key = if indices.is_empty() {
                base_name.clone()
            } else if indices.len() == 1 {
                format!("{base_name}[{}]", indices[0])
            } else {
                let suffix = indices
                    .iter()
                    .map(std::string::ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(",");
                format!("{base_name}[{suffix}]")
            };
            if let Some(slot) = self.layout.binding(&key) {
                return self.emit_slot_load(slot);
            }
        }

        self.lower_dynamic_subscripted_binding(
            base_name.as_str(),
            subscripts,
            scope,
            call_depth,
            DynamicSubscriptSemantics::VarRef,
        )
    }

    fn lower_index(
        &mut self,
        base: &dae::Expression,
        subscripts: &[dae::Subscript],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if let Some(reg) =
            self.lower_structural_index_expr(base, subscripts, scope, call_depth, None)?
        {
            return Ok(reg);
        }

        if let Ok(key) = indexed_binding_key(base, subscripts)
            && let Some(reg) = scope.get(&key).copied()
        {
            return Ok(reg);
        }

        if let Ok(key) = indexed_binding_key(base, subscripts)
            && let Some(slot) = self.layout.binding(&key)
        {
            return self.emit_slot_load(slot);
        }

        let base_key = dynamic_binding_base_key(base)?;
        self.lower_dynamic_subscripted_binding(
            base_key.as_str(),
            subscripts,
            scope,
            call_depth,
            DynamicSubscriptSemantics::Index,
        )
    }

    fn lower_dynamic_subscripted_binding(
        &mut self,
        base_key: &str,
        subscripts: &[dae::Subscript],
        scope: &Scope,
        call_depth: usize,
        semantics: DynamicSubscriptSemantics,
    ) -> Result<Reg, LowerError> {
        let mode = match semantics {
            DynamicSubscriptSemantics::VarRef => SubscriptEvalMode::Truncate,
            DynamicSubscriptSemantics::Index => SubscriptEvalMode::Round,
        };
        let subscript_regs = self.lower_subscript_regs(subscripts, scope, call_depth, mode)?;
        let candidates = indexed_entries_for_key(self.layout, &self.indexed_bindings, base_key)
            .into_iter()
            .filter(|entry| entry.indices.len() == subscript_regs.len())
            .collect::<Vec<_>>();

        let fallback = match semantics {
            DynamicSubscriptSemantics::VarRef => {
                if let Some(slot) = self.layout.binding(base_key) {
                    self.emit_slot_load(slot)?
                } else if let Some(first) = candidates.first() {
                    self.emit_slot_load(first.slot)?
                } else {
                    return Err(LowerError::MissingBinding {
                        name: base_key.to_string(),
                    });
                }
            }
            DynamicSubscriptSemantics::Index => self.emit_const(0.0),
        };

        if candidates.is_empty() {
            return Ok(fallback);
        }

        let mut merged = fallback;
        for candidate in candidates {
            let cond = self.emit_subscript_match(&subscript_regs, &candidate.indices);
            let candidate_value = self.emit_slot_load(candidate.slot)?;
            merged = self.emit_select(cond, candidate_value, merged);
        }
        Ok(merged)
    }

    fn lower_subscript_regs(
        &mut self,
        subscripts: &[dae::Subscript],
        scope: &Scope,
        call_depth: usize,
        mode: SubscriptEvalMode,
    ) -> Result<Vec<Reg>, LowerError> {
        let mut regs = Vec::with_capacity(subscripts.len());
        for sub in subscripts {
            let reg = match sub {
                dae::Subscript::Index(v) if *v > 0 => self.emit_const(*v as f64),
                dae::Subscript::Expr(expr) => {
                    let raw = self.lower_expr(expr, scope, call_depth)?;
                    match mode {
                        SubscriptEvalMode::Truncate => self.emit_unary(UnaryOp::Trunc, raw),
                        SubscriptEvalMode::Round => self.emit_round(raw),
                    }
                }
                dae::Subscript::Colon => {
                    return Err(LowerError::Unsupported {
                        reason: "slice subscript `:` is unsupported in PR2".to_string(),
                    });
                }
                _ => {
                    return Err(LowerError::Unsupported {
                        reason: "non-positive subscript is unsupported".to_string(),
                    });
                }
            };
            regs.push(reg);
        }
        Ok(regs)
    }

    fn emit_subscript_match(&mut self, lhs: &[Reg], rhs: &[usize]) -> Reg {
        debug_assert_eq!(lhs.len(), rhs.len());
        let mut cond = self.emit_const(1.0);
        for (reg, index) in lhs.iter().zip(rhs.iter()) {
            let rhs_const = self.emit_const(*index as f64);
            let eq = self.emit_compare(CompareOp::Eq, *reg, rhs_const);
            cond = self.emit_binary(BinaryOp::And, cond, eq);
        }
        cond
    }

    fn emit_round(&mut self, arg: Reg) -> Reg {
        let sign = self.emit_unary(UnaryOp::Sign, arg);
        let half = self.emit_const(0.5);
        let bias = self.emit_binary(BinaryOp::Mul, sign, half);
        let shifted = self.emit_binary(BinaryOp::Add, arg, bias);
        self.emit_unary(UnaryOp::Trunc, shifted)
    }

    fn lower_field_access(
        &mut self,
        base: &dae::Expression,
        field: &str,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if matches!(field, "re" | "im")
            && let dae::Expression::FunctionCall { name, args, .. } = base
        {
            let projected_name = format!("{}.{}", name.as_str(), field);
            if let Some(reg) =
                self.lower_complex_math_sum_projection(&projected_name, args, scope, call_depth)?
            {
                return Ok(reg);
            }
        }

        if matches!(field, "re" | "im")
            && let Some(reg) =
                self.lower_complex_operator_field_access(base, field, scope, call_depth)?
        {
            return Ok(reg);
        }

        if let dae::Expression::Index { base, subscripts } = base
            && let Some(reg) =
                self.lower_structural_index_expr(base, subscripts, scope, call_depth, Some(field))?
        {
            return Ok(reg);
        }

        if let Some(reg) = self.lower_constructor_field_access(base, field, scope, call_depth)? {
            return Ok(reg);
        }

        if let Some(values) = self.lower_structural_field_values(base, field, scope, call_depth)? {
            if let Some(first) = values.into_iter().next() {
                return Ok(first);
            }
            return Ok(self.emit_const(0.0));
        }

        let key = field_access_binding_key(base, field)?;
        if let Some(reg) = scope.get(&key).copied() {
            return Ok(reg);
        }
        let slot = self
            .layout
            .binding(&key)
            .ok_or(LowerError::MissingBinding { name: key })?;
        self.emit_slot_load(slot)
    }

    fn lower_complex_operator_field_access(
        &mut self,
        base: &dae::Expression,
        field: &str,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let (re, im) = match base {
            // MLS operator overloading for Complex numbers is flattened into
            // ordinary expression trees. Projected `re/im` access must recover
            // the selected component from the complex arithmetic result.
            dae::Expression::Binary { op, lhs, rhs } => {
                let (lhs_re, lhs_im) = self.lower_complex_operand_parts(lhs, scope, call_depth)?;
                let (rhs_re, rhs_im) = self.lower_complex_operand_parts(rhs, scope, call_depth)?;
                match op {
                    rumoca_ir_core::OpBinary::Add(_) => (
                        self.emit_binary(BinaryOp::Add, lhs_re, rhs_re),
                        self.emit_binary(BinaryOp::Add, lhs_im, rhs_im),
                    ),
                    rumoca_ir_core::OpBinary::Sub(_) => (
                        self.emit_binary(BinaryOp::Sub, lhs_re, rhs_re),
                        self.emit_binary(BinaryOp::Sub, lhs_im, rhs_im),
                    ),
                    rumoca_ir_core::OpBinary::Mul(_) => {
                        let ac = self.emit_binary(BinaryOp::Mul, lhs_re, rhs_re);
                        let bd = self.emit_binary(BinaryOp::Mul, lhs_im, rhs_im);
                        let ad = self.emit_binary(BinaryOp::Mul, lhs_re, rhs_im);
                        let bc = self.emit_binary(BinaryOp::Mul, lhs_im, rhs_re);
                        (
                            self.emit_binary(BinaryOp::Sub, ac, bd),
                            self.emit_binary(BinaryOp::Add, ad, bc),
                        )
                    }
                    rumoca_ir_core::OpBinary::Div(_) => {
                        let rr2 = self.emit_binary(BinaryOp::Mul, rhs_re, rhs_re);
                        let ri2 = self.emit_binary(BinaryOp::Mul, rhs_im, rhs_im);
                        let denom = self.emit_binary(BinaryOp::Add, rr2, ri2);
                        let lhs_rr = self.emit_binary(BinaryOp::Mul, lhs_re, rhs_re);
                        let lhs_ri = self.emit_binary(BinaryOp::Mul, lhs_re, rhs_im);
                        let li_rr = self.emit_binary(BinaryOp::Mul, lhs_im, rhs_re);
                        let li_ri = self.emit_binary(BinaryOp::Mul, lhs_im, rhs_im);
                        let re_num = self.emit_binary(BinaryOp::Add, lhs_rr, li_ri);
                        let im_num = self.emit_binary(BinaryOp::Sub, li_rr, lhs_ri);
                        (
                            self.emit_binary(BinaryOp::Div, re_num, denom),
                            self.emit_binary(BinaryOp::Div, im_num, denom),
                        )
                    }
                    _ => return Ok(None),
                }
            }
            dae::Expression::Unary {
                op: rumoca_ir_core::OpUnary::Minus(_),
                rhs,
            } => {
                let (rhs_re, rhs_im) = self.lower_complex_operand_parts(rhs, scope, call_depth)?;
                (
                    self.emit_unary(UnaryOp::Neg, rhs_re),
                    self.emit_unary(UnaryOp::Neg, rhs_im),
                )
            }
            _ => return Ok(None),
        };
        Ok(Some(if field == "re" { re } else { im }))
    }

    fn lower_complex_operand_parts(
        &mut self,
        expr: &dae::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<(Reg, Reg), LowerError> {
        let re = match self.lower_field_access(expr, "re", scope, call_depth) {
            Ok(value) => value,
            Err(_) => self.lower_expr(expr, scope, call_depth)?,
        };
        let im = match self.lower_field_access(expr, "im", scope, call_depth) {
            Ok(value) => value,
            Err(_) => self.emit_const(0.0),
        };
        Ok((re, im))
    }

    fn lower_constructor_field_access(
        &mut self,
        base: &dae::Expression,
        field: &str,
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let dae::Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } = base
        else {
            return Ok(None);
        };

        if !self.is_record_constructor_call(name, *is_constructor) {
            let projected_name = dae::VarName::new(format!("{}.{}", name.as_str(), field));
            return self
                .lower_function_call(&projected_name, args, false, caller_scope, call_depth)
                .map(Some);
        }

        if let Some(index) = constructor_positional_field_index(field)
            && let Some(expr) = args.get(index)
        {
            return self.lower_expr(expr, caller_scope, call_depth).map(Some);
        }

        let Some(constructor) = self.lookup_function(name).cloned() else {
            return Ok(None);
        };

        let mut local_scope = Scope::new();
        let mut input_regs = IndexMap::<String, Reg>::new();
        for (idx, input) in constructor.inputs.iter().enumerate() {
            let reg = if let Some(arg_expr) = args.get(idx) {
                self.lower_expr(arg_expr, caller_scope, call_depth + 1)?
            } else if let Some(default_expr) = input.default.as_ref() {
                self.lower_expr(default_expr, &local_scope, call_depth + 1)?
            } else {
                self.emit_const(0.0)
            };
            local_scope.insert(input.name.clone(), reg);
            input_regs.insert(input.name.clone(), reg);
        }

        if let Some(reg) = input_regs.get(field).copied() {
            return Ok(Some(reg));
        }

        if let Some(output) = constructor
            .outputs
            .iter()
            .find(|output| output.name == field)
        {
            if let Some(default_expr) = output.default.as_ref() {
                let reg = self.lower_expr(default_expr, &local_scope, call_depth + 1)?;
                return Ok(Some(reg));
            }
            if let Some(reg) = local_scope.get(&output.name).copied() {
                return Ok(Some(reg));
            }
        }

        Ok(None)
    }

    fn emit_slot_load(&mut self, slot: ScalarSlot) -> Result<Reg, LowerError> {
        let dst = self.alloc_reg();
        match slot {
            ScalarSlot::Time => self.ops.push(LinearOp::LoadTime { dst }),
            ScalarSlot::Y { index, .. } => self.ops.push(LinearOp::LoadY { dst, index }),
            ScalarSlot::P { index, .. } => self.ops.push(LinearOp::LoadP { dst, index }),
            ScalarSlot::Constant(value) => self.ops.push(LinearOp::Const { dst, value }),
        }
        Ok(dst)
    }

    fn lower_binary(
        &mut self,
        op: rumoca_ir_core::OpBinary,
        lhs: Reg,
        rhs: Reg,
    ) -> Result<Reg, LowerError> {
        let reg = match op {
            rumoca_ir_core::OpBinary::Add(_) | rumoca_ir_core::OpBinary::AddElem(_) => {
                self.emit_binary(BinaryOp::Add, lhs, rhs)
            }
            rumoca_ir_core::OpBinary::Sub(_) | rumoca_ir_core::OpBinary::SubElem(_) => {
                self.emit_binary(BinaryOp::Sub, lhs, rhs)
            }
            rumoca_ir_core::OpBinary::Mul(_) | rumoca_ir_core::OpBinary::MulElem(_) => {
                self.emit_binary(BinaryOp::Mul, lhs, rhs)
            }
            rumoca_ir_core::OpBinary::Div(_) | rumoca_ir_core::OpBinary::DivElem(_) => {
                self.emit_binary(BinaryOp::Div, lhs, rhs)
            }
            rumoca_ir_core::OpBinary::Exp(_) | rumoca_ir_core::OpBinary::ExpElem(_) => {
                self.emit_binary(BinaryOp::Pow, lhs, rhs)
            }
            rumoca_ir_core::OpBinary::And(_) => self.emit_binary(BinaryOp::And, lhs, rhs),
            rumoca_ir_core::OpBinary::Or(_) => self.emit_binary(BinaryOp::Or, lhs, rhs),
            rumoca_ir_core::OpBinary::Lt(_) => self.emit_compare(CompareOp::Lt, lhs, rhs),
            rumoca_ir_core::OpBinary::Le(_) => self.emit_compare(CompareOp::Le, lhs, rhs),
            rumoca_ir_core::OpBinary::Gt(_) => self.emit_compare(CompareOp::Gt, lhs, rhs),
            rumoca_ir_core::OpBinary::Ge(_) => self.emit_compare(CompareOp::Ge, lhs, rhs),
            rumoca_ir_core::OpBinary::Eq(_) => self.emit_compare(CompareOp::Eq, lhs, rhs),
            rumoca_ir_core::OpBinary::Neq(_) => self.emit_compare(CompareOp::Ne, lhs, rhs),
            rumoca_ir_core::OpBinary::Assign(_) | rumoca_ir_core::OpBinary::Empty => {
                return Err(LowerError::Unsupported {
                    reason: format!("binary operator {:?} is unsupported", op),
                });
            }
        };
        Ok(reg)
    }

    fn lower_unary(&mut self, op: rumoca_ir_core::OpUnary, rhs: Reg) -> Result<Reg, LowerError> {
        let reg = match op {
            rumoca_ir_core::OpUnary::Minus(_) | rumoca_ir_core::OpUnary::DotMinus(_) => {
                self.emit_unary(UnaryOp::Neg, rhs)
            }
            rumoca_ir_core::OpUnary::Not(_) => self.emit_unary(UnaryOp::Not, rhs),
            rumoca_ir_core::OpUnary::Plus(_)
            | rumoca_ir_core::OpUnary::DotPlus(_)
            | rumoca_ir_core::OpUnary::Empty => rhs,
        };
        Ok(reg)
    }

    fn lower_builtin(
        &mut self,
        function: dae::BuiltinFunction,
        args: &[dae::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let arg = |builder: &mut Self, idx: usize| -> Result<Reg, LowerError> {
            if let Some(expr) = args.get(idx) {
                builder.lower_expr(expr, scope, call_depth)
            } else {
                Ok(builder.emit_const(0.0))
            }
        };

        let unary = |builder: &mut Self, op: UnaryOp| -> Result<Reg, LowerError> {
            let x = arg(builder, 0)?;
            Ok(builder.emit_unary(op, x))
        };

        let binary = |builder: &mut Self, op: BinaryOp| -> Result<Reg, LowerError> {
            let x = arg(builder, 0)?;
            let y = arg(builder, 1)?;
            Ok(builder.emit_binary(op, x, y))
        };

        match function {
            dae::BuiltinFunction::Abs => unary(self, UnaryOp::Abs),
            dae::BuiltinFunction::Sign => unary(self, UnaryOp::Sign),
            dae::BuiltinFunction::Sqrt => unary(self, UnaryOp::Sqrt),
            dae::BuiltinFunction::Floor | dae::BuiltinFunction::Integer => {
                unary(self, UnaryOp::Floor)
            }
            dae::BuiltinFunction::Ceil => unary(self, UnaryOp::Ceil),
            dae::BuiltinFunction::Sin => unary(self, UnaryOp::Sin),
            dae::BuiltinFunction::Cos => unary(self, UnaryOp::Cos),
            dae::BuiltinFunction::Tan => unary(self, UnaryOp::Tan),
            dae::BuiltinFunction::Asin => unary(self, UnaryOp::Asin),
            dae::BuiltinFunction::Acos => unary(self, UnaryOp::Acos),
            dae::BuiltinFunction::Atan => unary(self, UnaryOp::Atan),
            dae::BuiltinFunction::Sinh => unary(self, UnaryOp::Sinh),
            dae::BuiltinFunction::Cosh => unary(self, UnaryOp::Cosh),
            dae::BuiltinFunction::Tanh => unary(self, UnaryOp::Tanh),
            dae::BuiltinFunction::Exp => unary(self, UnaryOp::Exp),
            dae::BuiltinFunction::Log => unary(self, UnaryOp::Log),
            dae::BuiltinFunction::Log10 => unary(self, UnaryOp::Log10),
            dae::BuiltinFunction::Atan2 => binary(self, BinaryOp::Atan2),
            dae::BuiltinFunction::Min => binary(self, BinaryOp::Min),
            dae::BuiltinFunction::Max => binary(self, BinaryOp::Max),
            dae::BuiltinFunction::Div => {
                let x = arg(self, 0)?;
                let y = arg(self, 1)?;
                let q = self.emit_binary(BinaryOp::Div, x, y);
                Ok(self.emit_unary(UnaryOp::Trunc, q))
            }
            // Keep the compiled PR2 numeric path aligned with Rumoca's runtime
            // evaluator, which currently uses truncation-based `%` semantics
            // for both `mod` and `rem`.
            dae::BuiltinFunction::Mod | dae::BuiltinFunction::Rem => {
                let x = arg(self, 0)?;
                let y = arg(self, 1)?;
                let q = self.emit_binary(BinaryOp::Div, x, y);
                let q_trunc = self.emit_unary(UnaryOp::Trunc, q);
                let product = self.emit_binary(BinaryOp::Mul, q_trunc, y);
                Ok(self.emit_binary(BinaryOp::Sub, x, product))
            }
            dae::BuiltinFunction::NoEvent
            | dae::BuiltinFunction::Delay
            | dae::BuiltinFunction::Homotopy => arg(self, 0),
            dae::BuiltinFunction::Smooth => arg(self, 1),
            dae::BuiltinFunction::SemiLinear => {
                let x = arg(self, 0)?;
                let k1 = arg(self, 1)?;
                let k2 = arg(self, 2)?;
                let zero = self.emit_const(0.0);
                let cond = self.emit_compare(CompareOp::Ge, x, zero);
                let pos = self.emit_binary(BinaryOp::Mul, k1, x);
                let neg = self.emit_binary(BinaryOp::Mul, k2, x);
                Ok(self.emit_select(cond, pos, neg))
            }
            dae::BuiltinFunction::Der => Ok(self.emit_const(0.0)),
            // MLS §8.6: before the start of integration, v = pre(v) holds.
            // Initial-mode expression rows can therefore lower pre(v) to the
            // current startup value instead of rejecting the row.
            dae::BuiltinFunction::Pre if self.is_initial_mode => arg(self, 0),
            dae::BuiltinFunction::Initial => {
                Ok(self.emit_const(if self.is_initial_mode { 1.0 } else { 0.0 }))
            }
            dae::BuiltinFunction::Sum => self.lower_sum_builtin(args, scope, call_depth),
            dae::BuiltinFunction::Product => self.lower_product_builtin(args, scope, call_depth),
            dae::BuiltinFunction::Size => self.lower_size_builtin(args, scope, call_depth),
            dae::BuiltinFunction::Zeros => Ok(self.emit_const(0.0)),
            dae::BuiltinFunction::Ones => Ok(self.emit_const(1.0)),
            dae::BuiltinFunction::Fill
            | dae::BuiltinFunction::Scalar
            | dae::BuiltinFunction::Vector
            | dae::BuiltinFunction::Matrix
            | dae::BuiltinFunction::Diagonal
            | dae::BuiltinFunction::Transpose => arg(self, 0),
            dae::BuiltinFunction::Linspace => arg(self, 0),
            dae::BuiltinFunction::Identity => Ok(self.emit_const(1.0)),
            dae::BuiltinFunction::Cat => self.lower_cat_builtin(args, scope, call_depth),
            dae::BuiltinFunction::Pre
            | dae::BuiltinFunction::Edge
            | dae::BuiltinFunction::Change
            | dae::BuiltinFunction::Reinit
            | dae::BuiltinFunction::Sample
            | dae::BuiltinFunction::Terminal
            | dae::BuiltinFunction::Ndims
            | dae::BuiltinFunction::OuterProduct
            | dae::BuiltinFunction::Symmetric
            | dae::BuiltinFunction::Cross
            | dae::BuiltinFunction::Skew => Err(LowerError::Unsupported {
                reason: format!("builtin `{}` is unsupported in PR2", function.name()),
            }),
        }
    }

    fn lower_cat_builtin(
        &mut self,
        args: &[dae::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if args.len() <= 1 {
            return Ok(self.emit_const(0.0));
        }
        let Some(first_value) = self
            .lower_array_like_values(&args[1], scope, call_depth)?
            .into_iter()
            .next()
        else {
            return Ok(self.emit_const(0.0));
        };
        Ok(first_value)
    }

    fn lower_sum_builtin(
        &mut self,
        args: &[dae::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if args.is_empty() {
            return Ok(self.emit_const(0.0));
        }
        if args.len() == 1 {
            if let Some(reg) = self.lower_sum_range(&args[0], scope, call_depth)? {
                return Ok(reg);
            }
            let values = self.lower_array_like_values(&args[0], scope, call_depth)?;
            if values.is_empty() {
                return Ok(self.emit_const(0.0));
            }
            let mut acc = self.emit_const(0.0);
            for value in values {
                acc = self.emit_binary(BinaryOp::Add, acc, value);
            }
            return Ok(acc);
        }

        let mut acc = self.emit_const(0.0);
        for expr in args {
            let value = self.lower_expr(expr, scope, call_depth)?;
            acc = self.emit_binary(BinaryOp::Add, acc, value);
        }
        Ok(acc)
    }

    fn lower_product_builtin(
        &mut self,
        args: &[dae::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if args.is_empty() {
            return Ok(self.emit_const(1.0));
        }
        if args.len() == 1 {
            let values = self.lower_array_like_values(&args[0], scope, call_depth)?;
            if values.is_empty() {
                return Ok(self.emit_const(1.0));
            }
            let mut acc = self.emit_const(1.0);
            for value in values {
                acc = self.emit_binary(BinaryOp::Mul, acc, value);
            }
            return Ok(acc);
        }

        let mut acc = self.emit_const(1.0);
        for expr in args {
            let value = self.lower_expr(expr, scope, call_depth)?;
            acc = self.emit_binary(BinaryOp::Mul, acc, value);
        }
        Ok(acc)
    }

    fn lower_size_builtin(
        &mut self,
        args: &[dae::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let Some(base_expr) = args.first() else {
            return Ok(self.emit_const(1.0));
        };
        let base_key = match dynamic_binding_base_key(base_expr) {
            Ok(key) => key,
            Err(_) => return Ok(self.emit_const(1.0)),
        };

        let dims = infer_indexed_dims(
            self.indexed_bindings
                .get(base_key.as_str())
                .map(Vec::as_slice)
                .unwrap_or(&[]),
        );
        if dims.is_empty() {
            return Ok(self.emit_const(1.0));
        }

        let dim_reg = if args.len() > 1 {
            let raw = self.lower_expr(&args[1], scope, call_depth)?;
            self.emit_round(raw)
        } else {
            self.emit_const(1.0)
        };

        let mut value = self.emit_const(1.0);
        for (idx, dim) in dims.iter().enumerate().rev() {
            let dim_idx = self.emit_const((idx + 1) as f64);
            let cond = self.emit_compare(CompareOp::Eq, dim_reg, dim_idx);
            let dim_val = self.emit_const(*dim as f64);
            value = self.emit_select(cond, dim_val, value);
        }
        Ok(value)
    }

    fn lower_function_call(
        &mut self,
        name: &dae::VarName,
        args: &[dae::Expression],
        is_constructor: bool,
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if self.is_record_constructor_call(name, is_constructor) {
            let (named_args, positional_args) =
                function_calls::split_named_and_positional_call_args(name.as_str(), args)?;
            if let Some(expr) = named_args
                .get("re")
                .copied()
                .or_else(|| positional_args.first().copied())
            {
                // Modelica.Complex and other scalar record constructors use
                // declared field order; numeric scalar contexts read the first
                // field unless a projection selects another component.
                return self.lower_expr(expr, caller_scope, call_depth + 1);
            }
            return Ok(self.emit_const(0.0));
        }

        if let Some(reg) = self.lower_runtime_string_special_intrinsic(name.as_str(), args)? {
            return Ok(reg);
        }

        if call_depth >= MAX_FUNCTION_INLINE_DEPTH {
            if let Some(reg) =
                self.try_lower_intrinsic_function_call(name, args, caller_scope, call_depth)?
            {
                return Ok(reg);
            }
            return Err(LowerError::InvalidFunction {
                name: name.as_str().to_string(),
                reason: format!("recursion depth exceeded ({MAX_FUNCTION_INLINE_DEPTH})"),
            });
        }

        let function = if let Some(function) = self.lookup_function(name) {
            function
        } else if let Some(projection) = self.lookup_function_output_projection(name) {
            return self.lower_projected_function_call(&projection, args, caller_scope, call_depth);
        } else if let Some(reg) =
            self.try_lower_intrinsic_function_call(name, args, caller_scope, call_depth)?
        {
            return Ok(reg);
        } else {
            return Err(LowerError::MissingFunction {
                name: name.as_str().to_string(),
            });
        };

        if function.external.is_some() {
            if let Some(reg) =
                self.try_lower_intrinsic_function_call(name, args, caller_scope, call_depth)?
            {
                return Ok(reg);
            }
            return Err(LowerError::Unsupported {
                reason: format!(
                    "external function call `{}` cannot be inlined in PR2",
                    name.as_str()
                ),
            });
        }

        let mut scope =
            self.bind_function_inputs(name, &function.inputs, args, caller_scope, call_depth)?;

        for param in function.outputs.iter().chain(function.locals.iter()) {
            let reg = if let Some(default) = param.default.as_ref() {
                self.lower_expr(default, &scope, call_depth + 1)?
            } else {
                self.emit_const(0.0)
            };
            scope.insert(param.name.clone(), reg);
        }

        let _returned = self.lower_statements(&function.body, &mut scope, call_depth + 1)?;

        if let Some(output) = function.outputs.first()
            && let Some(reg) = scope.get(&output.name).copied()
        {
            return Ok(reg);
        }

        Ok(self.emit_const(0.0))
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

    fn emit_table_next_event(&mut self, table_id: Reg, time: Reg) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::TableNextEvent {
            dst,
            table_id,
            time,
        });
        dst
    }

    fn lower_interval_intrinsic(
        &mut self,
        args: &[dae::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let Some(clock_expr) = args.first() else {
            return Ok(self.emit_const(1.0));
        };
        if let Some(reg) = self.lower_clock_interval_expr(clock_expr, scope, call_depth)? {
            return Ok(reg);
        }
        Ok(self.emit_const(1.0))
    }

    fn lower_clock_interval_expr(
        &mut self,
        expr: &dae::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        match expr {
            // MLS §16.5.1: interval(v) returns the associated clock interval for
            // the clocked variable v when that metadata is known at runtime.
            dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => Ok(self
                .clock_intervals
                .and_then(|intervals| intervals.get(name.as_str()).copied())
                .map(|value| self.emit_const(value))),
            dae::Expression::FunctionCall { name, args, .. } => {
                let short = intrinsic_short_name(name.as_str());
                match short {
                    "Clock" => self.lower_clock_interval_clock_call(args, scope, call_depth),
                    "subSample" => {
                        self.lower_scaled_clock_interval(args, scope, call_depth, BinaryOp::Mul)
                    }
                    "superSample" => {
                        self.lower_scaled_clock_interval(args, scope, call_depth, BinaryOp::Div)
                    }
                    "shiftSample" | "backSample" => {
                        self.lower_passthrough_clock_interval(args, scope, call_depth)
                    }
                    _ => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }

    fn lower_clock_interval_clock_call(
        &mut self,
        args: &[dae::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        match args {
            [] => Ok(Some(self.emit_const(1.0))),
            [interval] => self.lower_expr(interval, scope, call_depth).map(Some),
            [numerator_expr, denominator_expr, ..] => {
                let numerator = self.lower_expr(numerator_expr, scope, call_depth)?;
                let denominator = self.lower_expr(denominator_expr, scope, call_depth)?;
                Ok(Some(self.emit_binary(
                    BinaryOp::Div,
                    numerator,
                    denominator,
                )))
            }
        }
    }

    fn lower_scaled_clock_interval(
        &mut self,
        args: &[dae::Expression],
        scope: &Scope,
        call_depth: usize,
        op: BinaryOp,
    ) -> Result<Option<Reg>, LowerError> {
        let Some(base_expr) = args.first() else {
            return Ok(None);
        };
        let Some(base) = self.lower_clock_interval_expr(base_expr, scope, call_depth)? else {
            return Ok(None);
        };
        let factor = match args.get(1) {
            Some(factor_expr) => self.lower_expr(factor_expr, scope, call_depth)?,
            None => self.emit_const(1.0),
        };
        Ok(Some(self.emit_binary(op, base, factor)))
    }

    fn lower_passthrough_clock_interval(
        &mut self,
        args: &[dae::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let Some(base_expr) = args.first() else {
            return Ok(None);
        };
        self.lower_clock_interval_expr(base_expr, scope, call_depth)
    }

    /// Returns `true` when lowering should stop due to `return`.
    fn lower_statements(
        &mut self,
        statements: &[dae::Statement],
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        for statement in statements {
            if self.lower_statement(statement, scope, call_depth)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn lower_if_statement(
        &mut self,
        cond_blocks: &[dae::StatementBlock],
        else_block: &Option<Vec<dae::Statement>>,
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        if cond_blocks.is_empty() {
            if let Some(stmts) = else_block {
                return self.lower_statements(stmts, scope, call_depth);
            }
            return Ok(false);
        }

        let entry_scope = scope.clone();
        let mut cond_regs = Vec::with_capacity(cond_blocks.len());
        let mut branch_scopes = Vec::with_capacity(cond_blocks.len());

        for block in cond_blocks {
            let cond = self.lower_expr(&block.cond, &entry_scope, call_depth)?;
            cond_regs.push(cond);
            let mut branch_scope = entry_scope.clone();
            let returned = self.lower_statements(&block.stmts, &mut branch_scope, call_depth)?;
            if returned {
                return Err(unsupported_conditional_return());
            }
            branch_scopes.push(branch_scope);
        }

        let mut else_scope = entry_scope.clone();
        if let Some(stmts) = else_block {
            let returned = self.lower_statements(stmts, &mut else_scope, call_depth)?;
            if returned {
                return Err(unsupported_conditional_return());
            }
        }

        let mut merged_scope = entry_scope.clone();
        let names = collect_scope_names(&merged_scope, &branch_scopes, &else_scope);

        for name in names {
            let Some(mut merged) = else_scope
                .get(&name)
                .copied()
                .or_else(|| entry_scope.get(&name).copied())
            else {
                continue;
            };

            for (cond, branch_scope) in cond_regs.iter().zip(branch_scopes.iter()).rev() {
                merged = merge_branch_select(self, *cond, branch_scope, &name, merged);
            }
            merged_scope.insert(name, merged);
        }

        *scope = merged_scope;
        Ok(false)
    }

    fn lower_for_statement(
        &mut self,
        indices: &[dae::ForIndex],
        equations: &[dae::Statement],
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        let saved_bindings: Vec<(String, Option<Reg>)> = indices
            .iter()
            .map(|index| (index.ident.clone(), scope.get(&index.ident).copied()))
            .collect();

        let mut const_scope = IndexMap::<String, f64>::new();
        let returned =
            self.lower_for_iterations(indices, equations, scope, &mut const_scope, call_depth, 0);

        for (name, old_binding) in saved_bindings {
            if let Some(reg) = old_binding {
                scope.insert(name, reg);
            } else {
                scope.shift_remove(&name);
            }
        }

        returned
    }

    fn lower_for_iterations(
        &mut self,
        indices: &[dae::ForIndex],
        equations: &[dae::Statement],
        scope: &mut Scope,
        const_scope: &mut IndexMap<String, f64>,
        call_depth: usize,
        depth: usize,
    ) -> Result<bool, LowerError> {
        if depth >= indices.len() {
            return self.lower_statements(equations, scope, call_depth);
        }

        let iter = &indices[depth];
        let iter_values = self.eval_for_index_values(&iter.range, const_scope)?;
        if iter_values.is_empty() {
            return Ok(false);
        }

        for value in iter_values {
            let iter_reg = self.emit_const(value);
            scope.insert(iter.ident.clone(), iter_reg);
            const_scope.insert(iter.ident.clone(), value);
            if self.lower_for_iterations(
                indices,
                equations,
                scope,
                const_scope,
                call_depth,
                depth + 1,
            )? {
                return Ok(true);
            }
            const_scope.shift_remove(&iter.ident);
        }

        Ok(false)
    }

    fn eval_for_index_values(
        &self,
        range: &dae::Expression,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<Vec<f64>, LowerError> {
        match range {
            dae::Expression::Range { start, step, end } => {
                let start = self.eval_compile_time_int(start, const_scope, "for range start")?;
                let end = self.eval_compile_time_int(end, const_scope, "for range end")?;
                let step = if let Some(step_expr) = step.as_ref() {
                    self.eval_compile_time_int(step_expr, const_scope, "for range step")?
                } else {
                    1
                };
                if step == 0 {
                    return Err(LowerError::Unsupported {
                        reason: "for range step cannot be zero".to_string(),
                    });
                }

                Ok(build_range_values(start, end, step))
            }
            dae::Expression::Array { elements, .. } => {
                let mut values = Vec::with_capacity(elements.len());
                for element in elements {
                    let v = self.eval_compile_time_int(
                        element,
                        const_scope,
                        "for range array element",
                    )?;
                    values.push(v as f64);
                }
                Ok(values)
            }
            _ => {
                let value =
                    self.eval_compile_time_int(range, const_scope, "for range expression")?;
                Ok(vec![value as f64])
            }
        }
    }

    fn eval_compile_time_int(
        &self,
        expr: &dae::Expression,
        const_scope: &IndexMap<String, f64>,
        context: &str,
    ) -> Result<i64, LowerError> {
        let value = self.eval_compile_time_expr(expr, const_scope)?;
        if !value.is_finite() {
            return Err(LowerError::Unsupported {
                reason: format!("{context} is not finite"),
            });
        }
        let rounded = value.round();
        if (rounded - value).abs() > 1e-9 {
            return Err(LowerError::Unsupported {
                reason: format!("{context} must evaluate to an integer"),
            });
        }
        if rounded < i64::MIN as f64 || rounded > i64::MAX as f64 {
            return Err(LowerError::Unsupported {
                reason: format!("{context} overflows i64"),
            });
        }
        Ok(rounded as i64)
    }

    fn eval_compile_time_expr(
        &self,
        expr: &dae::Expression,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        match expr {
            dae::Expression::Literal(lit) => Ok(eval_literal(lit)),
            dae::Expression::VarRef { name, subscripts } => {
                self.eval_compile_time_var_ref(name, subscripts, const_scope)
            }
            dae::Expression::Unary { op, rhs } => {
                self.eval_compile_time_unary(op, rhs, const_scope)
            }
            dae::Expression::Binary { op, lhs, rhs } => {
                self.eval_compile_time_binary(op, lhs, rhs, const_scope)
            }
            dae::Expression::If {
                branches,
                else_branch,
            } => self.eval_compile_time_if(branches, else_branch, const_scope),
            dae::Expression::BuiltinCall { function, args } => {
                self.eval_compile_time_builtin(*function, args, const_scope)
            }
            dae::Expression::FunctionCall { .. }
            | dae::Expression::ArrayComprehension { .. }
            | dae::Expression::Tuple { .. }
            | dae::Expression::FieldAccess { .. }
            | dae::Expression::Index { .. }
            | dae::Expression::Range { .. }
            | dae::Expression::Array { .. }
            | dae::Expression::Empty => Err(LowerError::Unsupported {
                reason: "unsupported expression in for-loop range".to_string(),
            }),
        }
    }

    fn eval_compile_time_var_ref(
        &self,
        name: &dae::VarName,
        subscripts: &[dae::Subscript],
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        if subscripts.is_empty()
            && let Some(value) = const_scope.get(name.as_str())
        {
            return Ok(*value);
        }
        let key = compile_time_var_key(name, subscripts, const_scope)?;
        match self.layout.binding(key.as_str()) {
            Some(ScalarSlot::Constant(value)) => Ok(value),
            Some(_) | None => Err(LowerError::Unsupported {
                reason: format!("for-loop range expression requires compile-time constant `{key}`"),
            }),
        }
    }

    fn eval_compile_time_unary(
        &self,
        op: &rumoca_ir_core::OpUnary,
        rhs: &dae::Expression,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        let value = self.eval_compile_time_expr(rhs, const_scope)?;
        match op {
            rumoca_ir_core::OpUnary::Minus(_) | rumoca_ir_core::OpUnary::DotMinus(_) => Ok(-value),
            rumoca_ir_core::OpUnary::Plus(_)
            | rumoca_ir_core::OpUnary::DotPlus(_)
            | rumoca_ir_core::OpUnary::Empty => Ok(value),
            rumoca_ir_core::OpUnary::Not(_) => Ok(if value == 0.0 { 1.0 } else { 0.0 }),
        }
    }

    fn eval_compile_time_binary(
        &self,
        op: &rumoca_ir_core::OpBinary,
        lhs: &dae::Expression,
        rhs: &dae::Expression,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        let l = self.eval_compile_time_expr(lhs, const_scope)?;
        let r = self.eval_compile_time_expr(rhs, const_scope)?;
        match op {
            rumoca_ir_core::OpBinary::Add(_) | rumoca_ir_core::OpBinary::AddElem(_) => Ok(l + r),
            rumoca_ir_core::OpBinary::Sub(_) | rumoca_ir_core::OpBinary::SubElem(_) => Ok(l - r),
            rumoca_ir_core::OpBinary::Mul(_) | rumoca_ir_core::OpBinary::MulElem(_) => Ok(l * r),
            rumoca_ir_core::OpBinary::Div(_) | rumoca_ir_core::OpBinary::DivElem(_) => Ok(l / r),
            rumoca_ir_core::OpBinary::Exp(_) | rumoca_ir_core::OpBinary::ExpElem(_) => {
                Ok(l.powf(r))
            }
            rumoca_ir_core::OpBinary::Lt(_) => Ok(bool_to_f64(l < r)),
            rumoca_ir_core::OpBinary::Le(_) => Ok(bool_to_f64(l <= r)),
            rumoca_ir_core::OpBinary::Gt(_) => Ok(bool_to_f64(l > r)),
            rumoca_ir_core::OpBinary::Ge(_) => Ok(bool_to_f64(l >= r)),
            rumoca_ir_core::OpBinary::Eq(_) => Ok(bool_to_f64((l - r).abs() < f64::EPSILON)),
            rumoca_ir_core::OpBinary::Neq(_) => Ok(bool_to_f64((l - r).abs() >= f64::EPSILON)),
            rumoca_ir_core::OpBinary::And(_) => Ok(bool_to_f64(l != 0.0 && r != 0.0)),
            rumoca_ir_core::OpBinary::Or(_) => Ok(bool_to_f64(l != 0.0 || r != 0.0)),
            rumoca_ir_core::OpBinary::Assign(_) | rumoca_ir_core::OpBinary::Empty => {
                Err(LowerError::Unsupported {
                    reason: "unsupported operator in for-loop range expression".to_string(),
                })
            }
        }
    }

    fn eval_compile_time_if(
        &self,
        branches: &[(dae::Expression, dae::Expression)],
        else_branch: &dae::Expression,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        for (cond, value) in branches {
            let condition = self.eval_compile_time_expr(cond, const_scope)?;
            if condition != 0.0 {
                return self.eval_compile_time_expr(value, const_scope);
            }
        }
        self.eval_compile_time_expr(else_branch, const_scope)
    }

    fn eval_compile_time_builtin(
        &self,
        function: dae::BuiltinFunction,
        args: &[dae::Expression],
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        let arg0 = eval_builtin_arg(self, args, 0, const_scope)?;
        match function {
            dae::BuiltinFunction::Abs => Ok(arg0.abs()),
            dae::BuiltinFunction::Sign => Ok(arg0.signum()),
            dae::BuiltinFunction::Sqrt => Ok(arg0.sqrt()),
            dae::BuiltinFunction::Floor | dae::BuiltinFunction::Integer => Ok(arg0.floor()),
            dae::BuiltinFunction::Ceil => Ok(arg0.ceil()),
            dae::BuiltinFunction::Min => {
                let arg1 = eval_builtin_arg(self, args, 1, const_scope)?;
                Ok(arg0.min(arg1))
            }
            dae::BuiltinFunction::Max => {
                let arg1 = eval_builtin_arg(self, args, 1, const_scope)?;
                Ok(arg0.max(arg1))
            }
            _ => Err(LowerError::Unsupported {
                reason: format!(
                    "builtin `{}` is unsupported in for-loop range expression",
                    function.name()
                ),
            }),
        }
    }

    /// Returns `true` when lowering should stop due to `return`.
    fn lower_statement(
        &mut self,
        statement: &dae::Statement,
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        match statement {
            dae::Statement::Empty => Ok(false),
            dae::Statement::Return => Ok(true),
            dae::Statement::Assignment { comp, value } => {
                let target = assignment_target_name(comp)?;
                let values = self.lower_array_like_values(value, scope, call_depth)?;
                self.bind_assignment_values(scope, &target, &values);
                Ok(false)
            }
            dae::Statement::If {
                cond_blocks,
                else_block,
            } => self.lower_if_statement(cond_blocks, else_block, scope, call_depth),
            dae::Statement::For { indices, equations } => {
                self.lower_for_statement(indices, equations, scope, call_depth)
            }
            dae::Statement::Break => Err(LowerError::Unsupported {
                reason: "break statement is unsupported in PR6".to_string(),
            }),
            _ => Err(LowerError::Unsupported {
                reason: format!(
                    "function statement {:?} is unsupported in PR6",
                    statement_tag(statement)
                ),
            }),
        }
    }

    fn lookup_function(&self, name: &dae::VarName) -> Option<&'a dae::Function> {
        if let Some(function) = self.functions.get(name) {
            return Some(function);
        }
        self.functions
            .iter()
            .find(|(key, _)| key.as_str() == name.as_str())
            .map(|(_, value)| value)
    }

    fn is_record_constructor_call(&self, name: &dae::VarName, is_constructor: bool) -> bool {
        is_constructor
            || self
                .lookup_function(name)
                .is_some_and(|function| is_record_constructor_signature(name, function))
    }
}
