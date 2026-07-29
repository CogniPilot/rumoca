use std::collections::{BTreeMap, BTreeSet};

use rumoca_ir_galec::ast;
use rumoca_ir_galec::package::CheckedAlgorithmBlock;

use crate::runtime::{
    compare_ordered, declarations, initialized, not_value, size_dimension,
    uninitialized_declaration, value_in_integer_domain, value_matches_declaration,
};
use crate::{IntegerDomain, Value};

type SignalClosure = (String, BTreeSet<String>);
type ConditionResult = (bool, Option<SignalClosure>);

/// Typed failure of checked-block execution.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum EvaluationError {
    #[error("unknown GALEC name `{0}`")]
    UnknownName(String),
    #[error("GALEC value has the wrong type for {0}")]
    Type(&'static str),
    #[error("GALEC array index {index} is outside 1..={length}")]
    Bounds { index: i64, length: usize },
    #[error("GALEC integer division by zero")]
    IntegerDivisionByZero,
    #[error("GALEC integer arithmetic overflow")]
    IntegerOverflow,
    #[error("GALEC value `{0}` is uninitialized")]
    Uninitialized(String),
    #[error("cannot {operation} a GALEC block while it is {state}")]
    Lifecycle {
        operation: &'static str,
        state: &'static str,
    },
    #[error("GALEC for-loop step must not be zero")]
    ZeroLoopStep,
    #[error("checked GALEC invariant was violated: {0}")]
    MalformedCheckedBlock(String),
    #[error("external writes to GALEC `{name}` are not permitted ({kind})")]
    InvalidStateWrite { name: String, kind: &'static str },
    #[error("GALEC call `{name}` received {found} arguments; expected {expected}")]
    InputArity {
        name: String,
        expected: usize,
        found: usize,
    },
    #[error("GALEC call `{0}` has an unsupported oracle implementation")]
    UnsupportedBuiltin(String),
    #[error("GALEC builtin `{name}` received an invalid argument: {detail}")]
    InvalidBuiltinArgument {
        name: &'static str,
        detail: &'static str,
    },
    #[error("GALEC function `{name}` returned {found} values; expected {expected}")]
    OutputArity {
        name: String,
        expected: usize,
        found: usize,
    },
    #[error("signals escaped method `{method}` outside its declared set: {signals:?}")]
    EscapedSignals {
        method: &'static str,
        signals: Vec<String>,
    },
}

/// Stateful interpreter over one opaque checked block.
pub struct Evaluator<'a> {
    pub(super) block: &'a ast::Block,
    pub(super) state: BTreeMap<String, StateSlot<'a>>,
    pub(super) active_signals: BTreeSet<String>,
    pub(super) signal_closures: Vec<(String, BTreeSet<String>)>,
    pub(super) declaration_scopes: Vec<BTreeMap<String, ast::VariableDeclaration>>,
    pub(super) integer_domain: IntegerDomain,
    pub(super) lifecycle: Lifecycle,
}

#[derive(Clone)]
pub(super) struct StateSlot<'a> {
    pub(super) declaration: &'a ast::VariableDeclaration,
    pub(super) external_write: Option<&'static str>,
    pub(super) value: Value,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Lifecycle {
    Created,
    Started,
}

impl Lifecycle {
    const fn name(self) -> &'static str {
        match self {
            Self::Created => "created",
            Self::Started => "started",
        }
    }
}

impl<'a> Evaluator<'a> {
    pub fn new(
        block: &'a CheckedAlgorithmBlock,
        integer_domain: IntegerDomain,
    ) -> Result<Self, EvaluationError> {
        let block = block.block();
        let mut state = BTreeMap::new();
        for variable in &block.interface {
            state.insert(
                variable.decl.name.lexeme().to_owned(),
                StateSlot {
                    declaration: &variable.decl,
                    external_write: match variable.kind {
                        ast::InterfaceKind::Input => Some("control input"),
                        ast::InterfaceKind::TunableParameter => Some("tunable parameter"),
                        ast::InterfaceKind::Output => None,
                    },
                    value: uninitialized_declaration(block, &variable.decl)?,
                },
            );
        }
        for variable in &block.protected {
            state.insert(
                variable.decl.name.lexeme().to_owned(),
                StateSlot {
                    declaration: &variable.decl,
                    external_write: None,
                    value: uninitialized_declaration(block, &variable.decl)?,
                },
            );
        }
        let mut evaluator = Self {
            block,
            state,
            active_signals: BTreeSet::new(),
            signal_closures: Vec::new(),
            declaration_scopes: Vec::new(),
            integer_domain,
            lifecycle: Lifecycle::Created,
        };
        let starts = block
            .interface
            .iter()
            .filter(|variable| {
                matches!(
                    variable.kind,
                    ast::InterfaceKind::Input | ast::InterfaceKind::TunableParameter
                )
            })
            .filter_map(|variable| {
                variable
                    .start
                    .as_ref()
                    .map(|start| (variable.decl.name.lexeme().to_owned(), start.clone()))
            })
            .collect::<Vec<_>>();
        for (name, start) in starts {
            let value = evaluator.expression(&start, &BTreeMap::new())?;
            evaluator.set_state(&name, value)?;
        }
        Ok(evaluator)
    }

    pub fn set_state(&mut self, name: &str, value: Value) -> Result<(), EvaluationError> {
        let slot = self
            .state
            .get_mut(name)
            .ok_or_else(|| EvaluationError::UnknownName(name.to_owned()))?;
        let Some(kind) = slot.external_write else {
            return Err(EvaluationError::InvalidStateWrite {
                name: name.to_owned(),
                kind: "block-owned state",
            });
        };
        if !value_matches_declaration(self.block, slot.declaration, &value)? {
            return Err(EvaluationError::Type(kind));
        }
        if !value_in_integer_domain(&value, self.integer_domain) {
            return Err(EvaluationError::IntegerOverflow);
        }
        slot.value = value;
        Ok(())
    }

    pub fn state(&self, name: &str) -> Result<&Value, EvaluationError> {
        if self.lifecycle != Lifecycle::Started {
            return Err(self.lifecycle_error("read state"));
        }
        let value = &self
            .state
            .get(name)
            .ok_or_else(|| EvaluationError::UnknownName(name.to_owned()))?
            .value;
        initialized(value, name)?;
        Ok(value)
    }

    #[must_use]
    pub fn active_signals(&self) -> &BTreeSet<String> {
        &self.active_signals
    }

    pub fn startup(&mut self) -> Result<(), EvaluationError> {
        if self.lifecycle != Lifecycle::Created {
            return Err(self.lifecycle_error("invoke Startup"));
        }
        self.ensure_external_state_initialized()?;
        let state_before = self.state.clone();
        match self.invoke("Startup", &self.block.startup, false) {
            Ok(()) => {
                if let Err(error) = self.ensure_all_state_initialized() {
                    self.state = state_before;
                    return Err(error);
                }
                self.lifecycle = Lifecycle::Started;
                Ok(())
            }
            Err(error) => {
                self.state = state_before;
                Err(error)
            }
        }
    }

    pub fn recalibrate(&mut self) -> Result<(), EvaluationError> {
        self.require_started("invoke Recalibrate")?;
        self.invoke("Recalibrate", &self.block.recalibrate, true)
    }

    pub fn do_step(&mut self) -> Result<(), EvaluationError> {
        self.require_started("invoke DoStep")?;
        self.invoke("DoStep", &self.block.do_step, true)
    }

    fn require_started(&self, operation: &'static str) -> Result<(), EvaluationError> {
        if self.lifecycle == Lifecycle::Started {
            Ok(())
        } else {
            Err(self.lifecycle_error(operation))
        }
    }

    fn lifecycle_error(&self, operation: &'static str) -> EvaluationError {
        EvaluationError::Lifecycle {
            operation,
            state: self.lifecycle.name(),
        }
    }

    fn ensure_external_state_initialized(&self) -> Result<(), EvaluationError> {
        for (name, slot) in &self.state {
            if slot.external_write.is_some() {
                initialized(&slot.value, name)?;
            }
        }
        Ok(())
    }

    fn ensure_all_state_initialized(&self) -> Result<(), EvaluationError> {
        for (name, slot) in &self.state {
            initialized(&slot.value, name)?;
        }
        Ok(())
    }

    fn invoke(
        &mut self,
        name: &'static str,
        method: &ast::BlockMethod,
        limit_at_entry: bool,
    ) -> Result<(), EvaluationError> {
        self.active_signals.clear();
        if limit_at_entry {
            self.limit_all()?;
        }
        let mut locals = declarations(self.block, &method.locals)?;
        self.declaration_scopes.push(
            method
                .locals
                .iter()
                .map(|declaration| (declaration.name.lexeme().to_owned(), declaration.clone()))
                .collect(),
        );
        let result = self.statements(&method.statements, &mut locals);
        self.declaration_scopes.pop();
        result?;
        self.limit_all()?;
        let declared = method
            .signals
            .iter()
            .map(|signal| signal.name().to_owned())
            .collect::<BTreeSet<_>>();
        let escaped = self
            .active_signals
            .difference(&declared)
            .cloned()
            .collect::<Vec<_>>();
        if !escaped.is_empty() {
            return Err(EvaluationError::EscapedSignals {
                method: name,
                signals: escaped,
            });
        }
        Ok(())
    }

    pub(super) fn statements(
        &mut self,
        statements: &[ast::Spanned<ast::Statement>],
        locals: &mut BTreeMap<String, Value>,
    ) -> Result<(), EvaluationError> {
        for statement in statements {
            self.statement(&statement.node, locals)?;
        }
        Ok(())
    }

    fn statement(
        &mut self,
        statement: &ast::Statement,
        locals: &mut BTreeMap<String, Value>,
    ) -> Result<(), EvaluationError> {
        match statement {
            ast::Statement::Assignment { target, value } => {
                let value = self.expression(value, locals)?;
                self.assign(target, value, locals)
            }
            ast::Statement::MultiAssignment { targets, call } => {
                let values = self.call(call, locals)?;
                if values.len() != targets.len() {
                    return Err(EvaluationError::OutputArity {
                        name: call.function.lexeme().to_owned(),
                        expected: targets.len(),
                        found: values.len(),
                    });
                }
                for (target, value) in targets.iter().zip(values) {
                    self.assign(target, value, locals)?;
                }
                Ok(())
            }
            ast::Statement::Call(call) => {
                self.call(call, locals)?;
                Ok(())
            }
            ast::Statement::If(value) => self.if_statement(value, locals),
            ast::Statement::For(value) => self.for_loop(value, locals),
            ast::Statement::Limit(targets) => self.limit_targets(targets, locals),
            ast::Statement::Signal(signals) => {
                self.raise_signals(signals);
                Ok(())
            }
        }
    }

    fn if_statement(
        &mut self,
        statement: &ast::IfStatement,
        locals: &mut BTreeMap<String, Value>,
    ) -> Result<(), EvaluationError> {
        for branch in &statement.branches {
            let (matches, closure) = self.condition(&branch.condition, locals)?;
            if !matches {
                continue;
            }
            if let Some(closure) = closure {
                self.signal_closures.push(closure);
                let result = self.statements(&branch.body, locals);
                self.signal_closures.pop();
                return result;
            }
            return self.statements(&branch.body, locals);
        }
        statement
            .else_body
            .as_deref()
            .map_or(Ok(()), |body| self.statements(body, locals))
    }

    fn limit_targets(
        &mut self,
        targets: &[ast::LimitTarget],
        locals: &mut BTreeMap<String, Value>,
    ) -> Result<(), EvaluationError> {
        for target in targets {
            match target {
                ast::LimitTarget::SelfState => self.limit_all()?,
                ast::LimitTarget::Reference(reference) => {
                    self.limit_reference(reference, locals)?;
                }
            }
        }
        Ok(())
    }

    fn raise_signals(&mut self, signals: &[ast::Identifier]) {
        for signal in signals {
            let captured = self
                .signal_closures
                .iter()
                .rev()
                .find(|(name, _)| name == signal.as_str())
                .map(|(_, captured)| captured.clone());
            match captured {
                Some(captured) => self.active_signals.extend(captured),
                None => {
                    self.active_signals.insert(signal.as_str().to_owned());
                }
            }
        }
    }

    fn for_loop(
        &mut self,
        value: &ast::ForLoop,
        locals: &mut BTreeMap<String, Value>,
    ) -> Result<(), EvaluationError> {
        let start = self
            .expression(&value.start, locals)?
            .integer()
            .ok_or(EvaluationError::Type("for-loop start"))?;
        let stop = self
            .expression(&value.stop, locals)?
            .integer()
            .ok_or(EvaluationError::Type("for-loop stop"))?;
        let step = match &value.step {
            Some(step) => self
                .expression(step, locals)?
                .integer()
                .ok_or(EvaluationError::Type("for-loop step"))?,
            None => 1,
        };
        if step == 0 {
            return Err(EvaluationError::ZeroLoopStep);
        }
        let mut current = start;
        while if step > 0 {
            current <= stop
        } else {
            current >= stop
        } {
            if let Some(iterator) = &value.iterator {
                locals.insert(iterator.lexeme().to_owned(), Value::Integer(current));
            }
            self.statements(&value.body, locals)?;
            let Some(next) = current.checked_add(step) else {
                break;
            };
            current = next;
        }
        Ok(())
    }

    fn condition(
        &mut self,
        condition: &ast::Condition,
        locals: &mut BTreeMap<String, Value>,
    ) -> Result<ConditionResult, EvaluationError> {
        match condition {
            ast::Condition::Expression(expression) => Ok((
                self.expression(expression, locals)?
                    .boolean()
                    .ok_or(EvaluationError::Type("if condition"))?,
                None,
            )),
            ast::Condition::SignalCheck(check) => {
                let closure = check
                    .closure
                    .as_ref()
                    .map(|closure| (closure.as_str().to_owned(), self.active_signals.clone()));
                let listed = check
                    .test
                    .as_ref()
                    .map(|test| {
                        test.signals
                            .iter()
                            .map(|signal| signal.as_str().to_owned())
                            .collect::<BTreeSet<_>>()
                    })
                    .unwrap_or_else(|| self.active_signals.clone());
                let caught = if check.test.as_ref().is_some_and(|test| test.negated) {
                    self.active_signals
                        .difference(&listed)
                        .cloned()
                        .collect::<BTreeSet<_>>()
                } else {
                    self.active_signals
                        .intersection(&listed)
                        .cloned()
                        .collect::<BTreeSet<_>>()
                };
                let signal_match = !caught.is_empty();
                if signal_match {
                    self.active_signals
                        .retain(|signal| !caught.contains(signal));
                }
                let fallback = match &check.fallback {
                    Some(expression) => self
                        .expression(expression, locals)?
                        .boolean()
                        .ok_or(EvaluationError::Type("signal-check fallback"))?,
                    None => false,
                };
                Ok((signal_match || fallback, closure))
            }
        }
    }

    pub(super) fn expression(
        &mut self,
        expression: &ast::Expression,
        locals: &BTreeMap<String, Value>,
    ) -> Result<Value, EvaluationError> {
        match expression {
            ast::Expression::Bool(value) => Ok(Value::Boolean(*value)),
            ast::Expression::Integer(value) => self.integer(*value),
            ast::Expression::Real(value) => Ok(Value::Real(*value)),
            ast::Expression::Ref(reference) => self.reference(reference, locals),
            ast::Expression::Size { array, dimension } => {
                let array = self.reference(array, locals)?;
                let dimension = self
                    .expression(dimension, locals)?
                    .integer()
                    .ok_or(EvaluationError::Type("size dimension"))?;
                size_dimension(&array, dimension)
            }
            ast::Expression::Call(call) => {
                let values = self.call(call, &mut locals.clone())?;
                if values.len() != 1 {
                    return Err(EvaluationError::OutputArity {
                        name: call.function.lexeme().to_owned(),
                        expected: 1,
                        found: values.len(),
                    });
                }
                values.into_iter().next().ok_or_else(|| {
                    EvaluationError::MalformedCheckedBlock(format!(
                        "call `{}` lost its checked output",
                        call.function.lexeme()
                    ))
                })
            }
            ast::Expression::Paren(inner) => self.expression(inner, locals),
            ast::Expression::If(value) => self.if_expression(value, locals),
            ast::Expression::Array(values) => values
                .iter()
                .map(|value| self.expression(value, locals))
                .collect::<Result<Vec<_>, _>>()
                .map(Value::Array),
            ast::Expression::Neg(reference) => self.negate(self.reference(reference, locals)?),
            ast::Expression::Not(inner) => not_value(self.expression(inner, locals)?),
            ast::Expression::Binary { op, lhs, rhs } => {
                let lhs = self.expression(lhs, locals)?;
                let rhs = self.expression(rhs, locals)?;
                self.binary(*op, lhs, rhs)
            }
        }
    }

    fn if_expression(
        &mut self,
        expression: &ast::IfExpression,
        locals: &BTreeMap<String, Value>,
    ) -> Result<Value, EvaluationError> {
        for (condition, branch) in &expression.branches {
            let matches = self
                .expression(condition, locals)?
                .boolean()
                .ok_or(EvaluationError::Type("if-expression condition"))?;
            if matches {
                return self.expression(branch, locals);
            }
        }
        self.expression(&expression.else_value, locals)
    }

    fn binary(
        &mut self,
        op: ast::BinaryOp,
        lhs: Value,
        rhs: Value,
    ) -> Result<Value, EvaluationError> {
        match (lhs, rhs) {
            (Value::Array(lhs), Value::Array(rhs)) => {
                if lhs.len() != rhs.len() {
                    return Err(EvaluationError::Type("array binary operation shape"));
                }
                lhs.into_iter()
                    .zip(rhs)
                    .map(|(lhs, rhs)| self.binary(op, lhs, rhs))
                    .collect::<Result<Vec<_>, _>>()
                    .map(Value::Array)
            }
            (Value::Array(lhs), rhs) => lhs
                .into_iter()
                .map(|lhs| self.binary(op, lhs, rhs.clone()))
                .collect::<Result<Vec<_>, _>>()
                .map(Value::Array),
            (lhs, Value::Array(rhs)) => rhs
                .into_iter()
                .map(|rhs| self.binary(op, lhs.clone(), rhs))
                .collect::<Result<Vec<_>, _>>()
                .map(Value::Array),
            (lhs, rhs) => self.scalar_binary(op, lhs, rhs),
        }
    }

    fn scalar_binary(
        &mut self,
        op: ast::BinaryOp,
        lhs: Value,
        rhs: Value,
    ) -> Result<Value, EvaluationError> {
        use ast::BinaryOp as Op;
        match (op, lhs, rhs) {
            (Op::Add, Value::Integer(a), Value::Integer(b)) => {
                self.checked_integer(a.checked_add(b))
            }
            (Op::Sub, Value::Integer(a), Value::Integer(b)) => {
                self.checked_integer(a.checked_sub(b))
            }
            (Op::Mul, Value::Integer(a), Value::Integer(b)) => {
                self.checked_integer(a.checked_mul(b))
            }
            (Op::Add, Value::Real(a), Value::Real(b)) => Ok(Value::Real(a + b)),
            (Op::Sub, Value::Real(a), Value::Real(b)) => Ok(Value::Real(a - b)),
            (Op::Mul, Value::Real(a), Value::Real(b)) => Ok(Value::Real(a * b)),
            (Op::Div, Value::Real(a), Value::Real(b)) => Ok(Value::Real(a / b)),
            (Op::Pow, Value::Real(a), Value::Real(b)) => Ok(Value::Real(a.powf(b))),
            (Op::And, Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(a && b)),
            (Op::Or, Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(a || b)),
            (op @ (Op::Lt | Op::Gt | Op::Le | Op::Ge | Op::Eq | Op::Ne), a, b) => {
                self.compare(op, a, b)
            }
            _ => Err(EvaluationError::Type("binary operation")),
        }
    }

    fn integer(&self, value: i64) -> Result<Value, EvaluationError> {
        self.checked_integer(Some(value))
    }

    pub(super) fn checked_integer(&self, value: Option<i64>) -> Result<Value, EvaluationError> {
        value
            .filter(|value| self.integer_domain.contains(*value))
            .map(Value::Integer)
            .ok_or(EvaluationError::IntegerOverflow)
    }

    fn negate(&self, value: Value) -> Result<Value, EvaluationError> {
        match value {
            Value::Integer(value) => self.checked_integer(value.checked_neg()),
            Value::Real(value) => Ok(Value::Real(-value)),
            Value::Array(values) => values
                .into_iter()
                .map(|value| self.negate(value))
                .collect::<Result<Vec<_>, _>>()
                .map(Value::Array),
            Value::Boolean(_) | Value::Record(_) | Value::Uninitialized => {
                Err(EvaluationError::Type("unary minus"))
            }
        }
    }

    fn compare(
        &mut self,
        op: ast::BinaryOp,
        lhs: Value,
        rhs: Value,
    ) -> Result<Value, EvaluationError> {
        use ast::BinaryOp as Op;
        let result = match (lhs, rhs) {
            (Value::Real(a), Value::Real(b)) => {
                if a.is_nan() || b.is_nan() {
                    self.active_signals.insert("NAN".to_owned());
                    false
                } else {
                    compare_ordered(op, a, b)?
                }
            }
            (Value::Integer(a), Value::Integer(b)) => compare_ordered(op, a, b)?,
            (Value::Boolean(a), Value::Boolean(b)) => match op {
                Op::Eq => a == b,
                Op::Ne => a != b,
                _ => return Err(EvaluationError::Type("Boolean comparison")),
            },
            _ => return Err(EvaluationError::Type("comparison")),
        };
        Ok(Value::Boolean(result))
    }
}
