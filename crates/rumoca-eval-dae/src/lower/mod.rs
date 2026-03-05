//! Lower flat expressions and DAE residual rows to linear ops.

use crate::layout::{ScalarSlot, VarLayout};
use crate::linear_op::{BinaryOp, CompareOp, LinearOp, Reg, UnaryOp};
use indexmap::IndexMap;
use rumoca_ir_dae as dae;

mod array_values;
mod function_projection;
mod root_conditions;

use function_projection::format_subscript_binding_key;

const MAX_FUNCTION_INLINE_DEPTH: usize = 64;

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
    let n_x: usize = dae_model.states.values().map(|v| v.size()).sum();
    let mut rows = Vec::with_capacity(dae_model.f_x.len());
    for (row_idx, eq) in dae_model.f_x.iter().enumerate() {
        if eq.scalar_count != 1 {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "array residual row unsupported in PR2 (origin={} scalar_count={})",
                    eq.origin, eq.scalar_count
                ),
            });
        }

        let mut builder = LowerBuilder::new(layout, &dae_model.functions);
        let scope = Scope::new();
        let row = builder.lower_expr(&eq.rhs, &scope, 0)?;
        let signed = if row_idx < n_x {
            builder.emit_unary(UnaryOp::Neg, row)
        } else {
            row
        };
        builder.ops.push(LinearOp::StoreOutput { src: signed });
        rows.push(builder.ops);
    }
    Ok(rows)
}

pub fn lower_initial_residual(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    lower_expression_rows_with_mode(
        dae_model.initial_equations.iter(),
        layout,
        &dae_model.functions,
        true,
    )
}

pub fn lower_discrete_rhs(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    lower_expression_rows_with_mode(
        dae_model.f_z.iter().chain(dae_model.f_m.iter()),
        layout,
        &dae_model.functions,
        false,
    )
}

pub fn lower_root_conditions(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    root_conditions::lower_root_conditions(dae_model, layout)
}

pub fn lower_expression_rows_from_expressions(
    expressions: &[dae::Expression],
    layout: &VarLayout,
    functions: &IndexMap<dae::VarName, dae::Function>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let mut rows = Vec::with_capacity(expressions.len());
    for expression in expressions {
        rows.push(lower_expression_row(expression, layout, functions, false)?);
    }
    Ok(rows)
}

fn lower_expression_rows_with_mode<'a>(
    equations: impl IntoIterator<Item = &'a dae::Equation>,
    layout: &VarLayout,
    functions: &IndexMap<dae::VarName, dae::Function>,
    is_initial_mode: bool,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let equations: Vec<&dae::Equation> = equations.into_iter().collect();
    let mut rows = Vec::with_capacity(equations.len());
    for equation in equations {
        rows.push(lower_expression_row(
            &equation.rhs,
            layout,
            functions,
            is_initial_mode,
        )?);
    }
    Ok(rows)
}

fn lower_expression_row(
    expression: &dae::Expression,
    layout: &VarLayout,
    functions: &IndexMap<dae::VarName, dae::Function>,
    is_initial_mode: bool,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder = LowerBuilder::new_with_mode(layout, functions, is_initial_mode);
    let scope = Scope::new();
    let value = builder.lower_expr(expression, &scope, 0)?;
    builder.ops.push(LinearOp::StoreOutput { src: value });
    Ok(builder.ops)
}

struct LowerBuilder<'a> {
    layout: &'a VarLayout,
    functions: &'a IndexMap<dae::VarName, dae::Function>,
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
        Self::new_with_mode(layout, functions, false)
    }

    fn new_with_mode(
        layout: &'a VarLayout,
        functions: &'a IndexMap<dae::VarName, dae::Function>,
        is_initial_mode: bool,
    ) -> Self {
        Self {
            layout,
            functions,
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
            dae::Expression::FunctionCall { name, args, .. } => {
                self.lower_function_call(name, args, scope, call_depth)
            }
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
        let candidates = self
            .indexed_bindings
            .get(base_key)
            .map(|entries| {
                entries
                    .iter()
                    .filter(|entry| entry.indices.len() == subscript_regs.len())
                    .cloned()
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();

        let fallback = match semantics {
            DynamicSubscriptSemantics::VarRef => {
                if let Some(slot) = self.layout.binding(base_key) {
                    self.emit_slot_load(slot)?
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
        if let Some(reg) = self.lower_constructor_field_access(base, field, scope, call_depth)? {
            return Ok(reg);
        }

        let key = field_access_binding_key(base, field)?;
        let slot = self
            .layout
            .binding(&key)
            .ok_or(LowerError::MissingBinding { name: key })?;
        self.emit_slot_load(slot)
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

        if !*is_constructor {
            let projected_name = dae::VarName::new(format!("{}.{}", name.as_str(), field));
            return self
                .lower_function_call(&projected_name, args, caller_scope, call_depth)
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

    fn lower_binary(&mut self, op: dae::OpBinary, lhs: Reg, rhs: Reg) -> Result<Reg, LowerError> {
        let reg = match op {
            dae::OpBinary::Add(_) | dae::OpBinary::AddElem(_) => {
                self.emit_binary(BinaryOp::Add, lhs, rhs)
            }
            dae::OpBinary::Sub(_) | dae::OpBinary::SubElem(_) => {
                self.emit_binary(BinaryOp::Sub, lhs, rhs)
            }
            dae::OpBinary::Mul(_) | dae::OpBinary::MulElem(_) => {
                self.emit_binary(BinaryOp::Mul, lhs, rhs)
            }
            dae::OpBinary::Div(_) | dae::OpBinary::DivElem(_) => {
                self.emit_binary(BinaryOp::Div, lhs, rhs)
            }
            dae::OpBinary::Exp(_) | dae::OpBinary::ExpElem(_) => {
                self.emit_binary(BinaryOp::Pow, lhs, rhs)
            }
            dae::OpBinary::And(_) => self.emit_binary(BinaryOp::And, lhs, rhs),
            dae::OpBinary::Or(_) => self.emit_binary(BinaryOp::Or, lhs, rhs),
            dae::OpBinary::Lt(_) => self.emit_compare(CompareOp::Lt, lhs, rhs),
            dae::OpBinary::Le(_) => self.emit_compare(CompareOp::Le, lhs, rhs),
            dae::OpBinary::Gt(_) => self.emit_compare(CompareOp::Gt, lhs, rhs),
            dae::OpBinary::Ge(_) => self.emit_compare(CompareOp::Ge, lhs, rhs),
            dae::OpBinary::Eq(_) => self.emit_compare(CompareOp::Eq, lhs, rhs),
            dae::OpBinary::Neq(_) => self.emit_compare(CompareOp::Ne, lhs, rhs),
            dae::OpBinary::Assign(_) | dae::OpBinary::Empty => {
                return Err(LowerError::Unsupported {
                    reason: format!("binary operator {:?} is unsupported", op),
                });
            }
        };
        Ok(reg)
    }

    fn lower_unary(&mut self, op: dae::OpUnary, rhs: Reg) -> Result<Reg, LowerError> {
        let reg = match op {
            dae::OpUnary::Minus(_) | dae::OpUnary::DotMinus(_) => {
                self.emit_unary(UnaryOp::Neg, rhs)
            }
            dae::OpUnary::Not(_) => self.emit_unary(UnaryOp::Not, rhs),
            dae::OpUnary::Plus(_) | dae::OpUnary::DotPlus(_) | dae::OpUnary::Empty => rhs,
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
            | dae::BuiltinFunction::Rem
            | dae::BuiltinFunction::Mod
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
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
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

        let mut scope = Scope::new();

        for (idx, input) in function.inputs.iter().enumerate() {
            let reg = if let Some(arg_expr) = args.get(idx) {
                self.lower_expr(arg_expr, caller_scope, call_depth + 1)?
            } else if let Some(default) = input.default.as_ref() {
                self.lower_expr(default, &scope, call_depth + 1)?
            } else {
                self.emit_const(0.0)
            };
            scope.insert(input.name.clone(), reg);
        }

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

    fn try_lower_intrinsic_function_call(
        &mut self,
        name: &dae::VarName,
        args: &[dae::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let call_name = name.as_str();
        if intrinsic_short_name(call_name) == "interval" {
            return self
                .lower_interval_intrinsic(args, scope, call_depth)
                .map(Some);
        }
        if let Some(builtin) = resolve_intrinsic_builtin(call_name) {
            let reg = self.lower_builtin(builtin, args, scope, call_depth)?;
            return Ok(Some(reg));
        }
        Ok(None)
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
        let dae::Expression::FunctionCall { name, args, .. } = expr else {
            return Ok(None);
        };
        let short = intrinsic_short_name(name.as_str());
        match short {
            "Clock" => {
                if args.is_empty() {
                    return Ok(Some(self.emit_const(1.0)));
                }
                if args.len() == 1 {
                    return self.lower_expr(&args[0], scope, call_depth).map(Some);
                }
                let numerator = self.lower_expr(&args[0], scope, call_depth)?;
                let denominator = self.lower_expr(&args[1], scope, call_depth)?;
                Ok(Some(self.emit_binary(
                    BinaryOp::Div,
                    numerator,
                    denominator,
                )))
            }
            "subSample" => {
                let Some(base_expr) = args.first() else {
                    return Ok(None);
                };
                let Some(base) = self.lower_clock_interval_expr(base_expr, scope, call_depth)?
                else {
                    return Ok(None);
                };
                let factor = if let Some(factor_expr) = args.get(1) {
                    self.lower_expr(factor_expr, scope, call_depth)?
                } else {
                    self.emit_const(1.0)
                };
                Ok(Some(self.emit_binary(BinaryOp::Mul, base, factor)))
            }
            "superSample" => {
                let Some(base_expr) = args.first() else {
                    return Ok(None);
                };
                let Some(base) = self.lower_clock_interval_expr(base_expr, scope, call_depth)?
                else {
                    return Ok(None);
                };
                let factor = if let Some(factor_expr) = args.get(1) {
                    self.lower_expr(factor_expr, scope, call_depth)?
                } else {
                    self.emit_const(1.0)
                };
                Ok(Some(self.emit_binary(BinaryOp::Div, base, factor)))
            }
            "shiftSample" | "backSample" => {
                let Some(base_expr) = args.first() else {
                    return Ok(None);
                };
                self.lower_clock_interval_expr(base_expr, scope, call_depth)
            }
            _ => Ok(None),
        }
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
        op: &dae::OpUnary,
        rhs: &dae::Expression,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        let value = self.eval_compile_time_expr(rhs, const_scope)?;
        match op {
            dae::OpUnary::Minus(_) | dae::OpUnary::DotMinus(_) => Ok(-value),
            dae::OpUnary::Plus(_) | dae::OpUnary::DotPlus(_) | dae::OpUnary::Empty => Ok(value),
            dae::OpUnary::Not(_) => Ok(if value == 0.0 { 1.0 } else { 0.0 }),
        }
    }

    fn eval_compile_time_binary(
        &self,
        op: &dae::OpBinary,
        lhs: &dae::Expression,
        rhs: &dae::Expression,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        let l = self.eval_compile_time_expr(lhs, const_scope)?;
        let r = self.eval_compile_time_expr(rhs, const_scope)?;
        match op {
            dae::OpBinary::Add(_) | dae::OpBinary::AddElem(_) => Ok(l + r),
            dae::OpBinary::Sub(_) | dae::OpBinary::SubElem(_) => Ok(l - r),
            dae::OpBinary::Mul(_) | dae::OpBinary::MulElem(_) => Ok(l * r),
            dae::OpBinary::Div(_) | dae::OpBinary::DivElem(_) => Ok(l / r),
            dae::OpBinary::Exp(_) | dae::OpBinary::ExpElem(_) => Ok(l.powf(r)),
            dae::OpBinary::Lt(_) => Ok(bool_to_f64(l < r)),
            dae::OpBinary::Le(_) => Ok(bool_to_f64(l <= r)),
            dae::OpBinary::Gt(_) => Ok(bool_to_f64(l > r)),
            dae::OpBinary::Ge(_) => Ok(bool_to_f64(l >= r)),
            dae::OpBinary::Eq(_) => Ok(bool_to_f64((l - r).abs() < f64::EPSILON)),
            dae::OpBinary::Neq(_) => Ok(bool_to_f64((l - r).abs() >= f64::EPSILON)),
            dae::OpBinary::And(_) => Ok(bool_to_f64(l != 0.0 && r != 0.0)),
            dae::OpBinary::Or(_) => Ok(bool_to_f64(l != 0.0 || r != 0.0)),
            dae::OpBinary::Assign(_) | dae::OpBinary::Empty => Err(LowerError::Unsupported {
                reason: "unsupported operator in for-loop range expression".to_string(),
            }),
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
}

fn build_indexed_binding_map(layout: &VarLayout) -> IndexMap<String, Vec<IndexedBinding>> {
    let mut grouped: IndexMap<String, Vec<IndexedBinding>> = IndexMap::new();
    for (key, slot) in layout.bindings() {
        let Some((base, indices)) = parse_indexed_binding_key(key) else {
            continue;
        };
        grouped.entry(base).or_default().push(IndexedBinding {
            slot: *slot,
            indices,
        });
    }
    grouped
}

fn parse_indexed_binding_key(key: &str) -> Option<(String, Vec<usize>)> {
    let open = key.rfind('[')?;
    if !key.ends_with(']') || open >= key.len().saturating_sub(1) {
        return None;
    }
    let base = key[..open].to_string();
    if base.is_empty() {
        return None;
    }
    let contents = &key[open + 1..key.len() - 1];
    let mut indices = Vec::new();
    for raw in contents.split(',') {
        let parsed = raw.trim().parse::<usize>().ok()?;
        if parsed == 0 {
            return None;
        }
        indices.push(parsed);
    }
    if indices.is_empty() {
        return None;
    }
    Some((base, indices))
}

fn sorted_flat_entries(entries: &[IndexedBinding]) -> Vec<&IndexedBinding> {
    let mut flat = entries
        .iter()
        .filter(|entry| entry.indices.len() == 1)
        .collect::<Vec<_>>();
    flat.sort_by_key(|entry| entry.indices[0]);
    flat
}

fn infer_indexed_dims(entries: &[IndexedBinding]) -> Vec<usize> {
    let has_multi_dim = entries.iter().any(|entry| entry.indices.len() > 1);
    if has_multi_dim {
        let mut dims = Vec::<usize>::new();
        for entry in entries.iter().filter(|entry| entry.indices.len() > 1) {
            if entry.indices.len() > dims.len() {
                dims.resize(entry.indices.len(), 0);
            }
            for (idx, value) in entry.indices.iter().enumerate() {
                dims[idx] = dims[idx].max(*value);
            }
        }
        return dims;
    }

    let flat_count = entries
        .iter()
        .filter(|entry| entry.indices.len() == 1)
        .count();
    if flat_count > 0 {
        return vec![flat_count];
    }
    Vec::new()
}

fn static_subscript_indices(
    subscripts: &[dae::Subscript],
) -> Result<Option<Vec<usize>>, LowerError> {
    if subscripts.is_empty() {
        return Ok(Some(Vec::new()));
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        match sub {
            dae::Subscript::Index(v) if *v > 0 => indices.push(*v as usize),
            dae::Subscript::Expr(expr) => match lower_static_index_expr(expr)? {
                Some(value) => indices.push(value),
                None => return Ok(None),
            },
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
        }
    }
    Ok(Some(indices))
}

fn dynamic_binding_base_key(expr: &dae::Expression) -> Result<String, LowerError> {
    match expr {
        dae::Expression::VarRef { name, subscripts } => {
            if subscripts.is_empty() {
                return Ok(name.as_str().to_string());
            }
            append_subscripts_to_key(name.as_str().to_string(), subscripts)
        }
        dae::Expression::Index { base, subscripts } => {
            let base_key = dynamic_binding_base_key(base)?;
            append_subscripts_to_key(base_key, subscripts)
        }
        dae::Expression::FieldAccess { base, field } => {
            let base_key = dynamic_binding_base_key(base)?;
            Ok(format!("{base_key}.{field}"))
        }
        _ => Err(LowerError::Unsupported {
            reason: format!(
                "unsupported base expression for dynamic binding path: {}",
                expr_tag(expr)
            ),
        }),
    }
}

fn lower_subscript_index(subscript: &dae::Subscript) -> Result<usize, LowerError> {
    match subscript {
        dae::Subscript::Index(v) if *v > 0 => Ok(*v as usize),
        dae::Subscript::Expr(expr) => lower_index_expr(expr),
        dae::Subscript::Colon => Err(LowerError::Unsupported {
            reason: "slice subscript `:` is unsupported in PR2".to_string(),
        }),
        _ => Err(LowerError::Unsupported {
            reason: "non-positive subscript is unsupported".to_string(),
        }),
    }
}

fn indexed_binding_key(
    base: &dae::Expression,
    subscripts: &[dae::Subscript],
) -> Result<String, LowerError> {
    let base_key = binding_base_key(base)?;
    append_subscripts_to_key(base_key, subscripts)
}

fn field_access_binding_key(base: &dae::Expression, field: &str) -> Result<String, LowerError> {
    let base_key = binding_base_key(base)?;
    Ok(format!("{base_key}.{field}"))
}

fn binding_base_key(expr: &dae::Expression) -> Result<String, LowerError> {
    match expr {
        dae::Expression::VarRef { name, subscripts } => {
            if subscripts.is_empty() {
                Ok(name.as_str().to_string())
            } else {
                append_subscripts_to_key(name.as_str().to_string(), subscripts)
            }
        }
        dae::Expression::Index { base, subscripts } => indexed_binding_key(base, subscripts),
        dae::Expression::FieldAccess { base, field } => field_access_binding_key(base, field),
        _ => Err(LowerError::Unsupported {
            reason: format!(
                "unsupported base expression for binding path: {}",
                expr_tag(expr)
            ),
        }),
    }
}

fn append_subscripts_to_key(
    base: String,
    subscripts: &[dae::Subscript],
) -> Result<String, LowerError> {
    if subscripts.is_empty() {
        return Ok(base);
    }

    let mut indices = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        indices.push(lower_subscript_index(sub)?);
    }

    if indices.len() == 1 {
        return Ok(format!("{base}[{}]", indices[0]));
    }

    let suffix = indices
        .iter()
        .map(std::string::ToString::to_string)
        .collect::<Vec<_>>()
        .join(",");
    Ok(format!("{base}[{suffix}]"))
}

fn constructor_positional_field_index(field: &str) -> Option<usize> {
    match field {
        "re" => Some(0),
        "im" => Some(1),
        _ => None,
    }
}

fn lower_index_expr(expr: &dae::Expression) -> Result<usize, LowerError> {
    match lower_static_index_expr(expr)? {
        Some(index) => Ok(index),
        None => Err(LowerError::Unsupported {
            reason: "dynamic subscript expressions are unsupported in PR2".to_string(),
        }),
    }
}

fn lower_static_index_expr(expr: &dae::Expression) -> Result<Option<usize>, LowerError> {
    let Some(raw) = lower_static_index_numeric(expr)? else {
        return Ok(None);
    };

    let rounded = raw.round();
    if rounded.is_finite() && rounded > 0.0 && (rounded - raw).abs() < f64::EPSILON {
        return Ok(Some(rounded as usize));
    }

    Err(LowerError::Unsupported {
        reason: "subscript expression did not evaluate to a positive integer".to_string(),
    })
}

fn lower_static_index_numeric(expr: &dae::Expression) -> Result<Option<f64>, LowerError> {
    match expr {
        dae::Expression::Literal(dae::Literal::Integer(v)) => Ok(Some(*v as f64)),
        dae::Expression::Literal(dae::Literal::Real(v)) => Ok(Some(*v)),
        dae::Expression::Unary {
            op: dae::OpUnary::Plus(_) | dae::OpUnary::DotPlus(_) | dae::OpUnary::Empty,
            rhs,
        } => lower_static_index_numeric(rhs),
        dae::Expression::Unary {
            op: dae::OpUnary::Minus(_) | dae::OpUnary::DotMinus(_),
            rhs,
        } => Ok(lower_static_index_numeric(rhs)?.map(|value| -value)),
        dae::Expression::Binary { op, lhs, rhs } => {
            let Some(l) = lower_static_index_numeric(lhs)? else {
                return Ok(None);
            };
            let Some(r) = lower_static_index_numeric(rhs)? else {
                return Ok(None);
            };
            let value = match op {
                dae::OpBinary::Add(_) | dae::OpBinary::AddElem(_) => l + r,
                dae::OpBinary::Sub(_) | dae::OpBinary::SubElem(_) => l - r,
                dae::OpBinary::Mul(_) | dae::OpBinary::MulElem(_) => l * r,
                dae::OpBinary::Div(_) | dae::OpBinary::DivElem(_) => l / r,
                dae::OpBinary::Exp(_) | dae::OpBinary::ExpElem(_) => l.powf(r),
                _ => return Ok(None),
            };
            Ok(Some(value))
        }
        _ => Ok(None),
    }
}

fn compile_time_var_key(
    name: &dae::VarName,
    subscripts: &[dae::Subscript],
    const_scope: &IndexMap<String, f64>,
) -> Result<String, LowerError> {
    if subscripts.is_empty() {
        return Ok(name.as_str().to_string());
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        let index = compile_time_subscript_index(sub, const_scope)?;
        indices.push(index.to_string());
    }
    if indices.len() == 1 {
        Ok(format!("{}[{}]", name.as_str(), indices[0]))
    } else {
        Ok(format!("{}[{}]", name.as_str(), indices.join(",")))
    }
}

fn compile_time_subscript_index(
    subscript: &dae::Subscript,
    const_scope: &IndexMap<String, f64>,
) -> Result<usize, LowerError> {
    match subscript {
        dae::Subscript::Index(value) if *value > 0 => Ok(*value as usize),
        dae::Subscript::Expr(expr) => compile_time_index_expr(expr, const_scope),
        dae::Subscript::Colon => Err(LowerError::Unsupported {
            reason: "slice subscript `:` is unsupported in compile-time context".to_string(),
        }),
        _ => Err(LowerError::Unsupported {
            reason: "non-positive subscript is unsupported in compile-time context".to_string(),
        }),
    }
}

fn compile_time_index_expr(
    expr: &dae::Expression,
    const_scope: &IndexMap<String, f64>,
) -> Result<usize, LowerError> {
    let raw = match expr {
        dae::Expression::Literal(dae::Literal::Integer(v)) => *v as f64,
        dae::Expression::Literal(dae::Literal::Real(v)) => *v,
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => *const_scope
            .get(name.as_str())
            .ok_or_else(|| LowerError::Unsupported {
                reason: format!(
                    "subscript variable `{}` is not compile-time bound",
                    name.as_str()
                ),
            })?,
        dae::Expression::Unary {
            op: dae::OpUnary::Plus(_) | dae::OpUnary::DotPlus(_) | dae::OpUnary::Empty,
            rhs,
        } => compile_time_index_expr(rhs, const_scope)? as f64,
        dae::Expression::Unary {
            op: dae::OpUnary::Minus(_) | dae::OpUnary::DotMinus(_),
            rhs,
        } => -(compile_time_index_expr(rhs, const_scope)? as f64),
        _ => {
            return Err(LowerError::Unsupported {
                reason: "dynamic subscript expressions are unsupported in compile-time context"
                    .to_string(),
            });
        }
    };

    let rounded = raw.round();
    if rounded.is_finite() && rounded > 0.0 && (rounded - raw).abs() < f64::EPSILON {
        return Ok(rounded as usize);
    }

    Err(LowerError::Unsupported {
        reason: "subscript expression did not evaluate to a positive integer".to_string(),
    })
}

fn assignment_target_name(comp: &dae::ComponentReference) -> Result<String, LowerError> {
    if comp.parts.is_empty() {
        return Err(LowerError::InvalidFunction {
            name: "<anonymous>".to_string(),
            reason: "assignment target has no path parts".to_string(),
        });
    }
    if comp.parts.iter().any(|part| !part.subs.is_empty()) {
        return Err(LowerError::Unsupported {
            reason: format!(
                "subscripted assignment target `{}` is unsupported in PR2",
                comp.to_var_name().as_str()
            ),
        });
    }
    Ok(comp.to_var_name().as_str().to_string())
}

fn eval_literal(literal: &dae::Literal) -> f64 {
    match literal {
        dae::Literal::Real(v) => *v,
        dae::Literal::Integer(v) => *v as f64,
        dae::Literal::Boolean(v) => {
            if *v {
                1.0
            } else {
                0.0
            }
        }
        dae::Literal::String(_) => 0.0,
    }
}

fn expr_tag(expr: &dae::Expression) -> &'static str {
    match expr {
        dae::Expression::Binary { .. } => "Binary",
        dae::Expression::Unary { .. } => "Unary",
        dae::Expression::VarRef { .. } => "VarRef",
        dae::Expression::BuiltinCall { .. } => "BuiltinCall",
        dae::Expression::FunctionCall { .. } => "FunctionCall",
        dae::Expression::Literal(_) => "Literal",
        dae::Expression::If { .. } => "If",
        dae::Expression::Array { .. } => "Array",
        dae::Expression::Tuple { .. } => "Tuple",
        dae::Expression::Range { .. } => "Range",
        dae::Expression::ArrayComprehension { .. } => "ArrayComprehension",
        dae::Expression::Index { .. } => "Index",
        dae::Expression::FieldAccess { .. } => "FieldAccess",
        dae::Expression::Empty => "Empty",
    }
}

fn statement_tag(statement: &dae::Statement) -> &'static str {
    match statement {
        dae::Statement::Empty => "Empty",
        dae::Statement::Assignment { .. } => "Assignment",
        dae::Statement::Return => "Return",
        dae::Statement::Break => "Break",
        dae::Statement::For { .. } => "For",
        dae::Statement::While { .. } => "While",
        dae::Statement::If { .. } => "If",
        dae::Statement::When { .. } => "When",
        dae::Statement::FunctionCall { .. } => "FunctionCall",
        dae::Statement::Reinit { .. } => "Reinit",
        dae::Statement::Assert { .. } => "Assert",
    }
}

fn unsupported_conditional_return() -> LowerError {
    LowerError::Unsupported {
        reason: "conditional return in function if-statement is unsupported in PR6".to_string(),
    }
}

fn resolve_intrinsic_builtin(name: &str) -> Option<dae::BuiltinFunction> {
    dae::BuiltinFunction::from_name(name).or_else(|| {
        name.rsplit('.')
            .next()
            .and_then(dae::BuiltinFunction::from_name)
    })
}

fn intrinsic_short_name(name: &str) -> &str {
    name.rsplit('.').next().unwrap_or(name)
}

fn collect_scope_names(entry: &Scope, branches: &[Scope], else_scope: &Scope) -> Vec<String> {
    let mut names: Vec<String> = entry.keys().cloned().collect();
    for scoped in branches.iter().chain(std::iter::once(else_scope)) {
        names.extend(
            scoped
                .keys()
                .filter(|name| !entry.contains_key(*name))
                .cloned(),
        );
    }
    names
}

fn merge_branch_select(
    builder: &mut LowerBuilder<'_>,
    cond: Reg,
    branch_scope: &Scope,
    name: &str,
    merged: Reg,
) -> Reg {
    match branch_scope.get(name).copied() {
        Some(branch_value) => builder.emit_select(cond, branch_value, merged),
        None => merged,
    }
}

fn build_range_values(start: i64, end: i64, step: i64) -> Vec<f64> {
    let mut values = Vec::new();
    let mut current = start;
    while if step > 0 {
        current <= end
    } else {
        current >= end
    } {
        values.push(current as f64);
        let Some(next) = current.checked_add(step) else {
            break;
        };
        current = next;
    }
    values
}

fn eval_builtin_arg(
    builder: &LowerBuilder<'_>,
    args: &[dae::Expression],
    idx: usize,
    const_scope: &IndexMap<String, f64>,
) -> Result<f64, LowerError> {
    let Some(expr) = args.get(idx) else {
        return Ok(0.0);
    };
    builder.eval_compile_time_expr(expr, const_scope)
}

fn bool_to_f64(value: bool) -> f64 {
    if value { 1.0 } else { 0.0 }
}

#[cfg(test)]
mod tests;
