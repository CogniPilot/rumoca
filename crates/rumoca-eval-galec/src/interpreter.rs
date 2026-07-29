use std::collections::{BTreeMap, BTreeSet};

use rumoca_ir_galec::ast as ast;
use rumoca_ir_galec::package::CheckedAlgorithmBlock;

use crate::{IntegerDomain, Value};

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
    block: &'a ast::Block,
    state: BTreeMap<String, StateSlot<'a>>,
    active_signals: BTreeSet<String>,
    signal_closures: Vec<(String, BTreeSet<String>)>,
    declaration_scopes: Vec<BTreeMap<String, ast::VariableDeclaration>>,
    integer_domain: IntegerDomain,
}

#[derive(Clone)]
struct StateSlot<'a> {
    declaration: &'a ast::VariableDeclaration,
    external_write: Option<&'static str>,
    value: Value,
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
                    value: zero_declaration(block, &variable.decl)?,
                },
            );
        }
        for variable in &block.protected {
            state.insert(
                variable.decl.name.lexeme().to_owned(),
                StateSlot {
                    declaration: &variable.decl,
                    external_write: None,
                    value: zero_declaration(block, &variable.decl)?,
                },
            );
        }
        Ok(Self {
            block,
            state,
            active_signals: BTreeSet::new(),
            signal_closures: Vec::new(),
            declaration_scopes: Vec::new(),
            integer_domain,
        })
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

    #[must_use]
    pub fn state(&self, name: &str) -> Option<&Value> {
        self.state.get(name).map(|slot| &slot.value)
    }

    #[must_use]
    pub fn active_signals(&self) -> &BTreeSet<String> {
        &self.active_signals
    }

    pub fn startup(&mut self) -> Result<(), EvaluationError> {
        self.invoke("Startup", &self.block.startup, false)
    }

    pub fn recalibrate(&mut self) -> Result<(), EvaluationError> {
        self.invoke("Recalibrate", &self.block.recalibrate, true)
    }

    pub fn do_step(&mut self) -> Result<(), EvaluationError> {
        self.invoke("DoStep", &self.block.do_step, true)
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
                .map(|declaration| {
                    (
                        declaration.name.lexeme().to_owned(),
                        declaration.clone(),
                    )
                })
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

    fn statements(
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
            ast::Statement::If(value) => {
                for branch in &value.branches {
                    let (matches, closure) = self.condition(&branch.condition, locals)?;
                    if matches {
                        if let Some(closure) = closure {
                            self.signal_closures.push(closure);
                            let result = self.statements(&branch.body, locals);
                            self.signal_closures.pop();
                            return result;
                        }
                        return self.statements(&branch.body, locals);
                    }
                }
                if let Some(body) = &value.else_body {
                    self.statements(body, locals)?;
                }
                Ok(())
            }
            ast::Statement::For(value) => self.for_loop(value, locals),
            ast::Statement::Limit(targets) => {
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
            ast::Statement::Signal(signals) => {
                for signal in signals {
                    if let Some((_, captured)) = self
                        .signal_closures
                        .iter()
                        .rev()
                        .find(|(name, _)| name == signal.as_str())
                    {
                        self.active_signals.extend(captured.iter().cloned());
                    } else {
                        self.active_signals.insert(signal.as_str().to_owned());
                    }
                }
                Ok(())
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
    ) -> Result<(bool, Option<(String, BTreeSet<String>)>), EvaluationError> {
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
                    for signal in &caught {
                        self.active_signals.remove(signal);
                    }
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

    fn expression(
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
                Ok(values.into_iter().next().expect("one output"))
            }
            ast::Expression::Paren(inner) => self.expression(inner, locals),
            ast::Expression::If(value) => {
                for (condition, branch) in &value.branches {
                    if self
                        .expression(condition, locals)?
                        .boolean()
                        .ok_or(EvaluationError::Type("if-expression condition"))?
                    {
                        return self.expression(branch, locals);
                    }
                }
                self.expression(&value.else_value, locals)
            }
            ast::Expression::Array(values) => values
                .iter()
                .map(|value| self.expression(value, locals))
                .collect::<Result<Vec<_>, _>>()
                .map(Value::Array),
            ast::Expression::Neg(reference) => {
                self.negate(self.reference(reference, locals)?)
            }
            ast::Expression::Not(inner) => not_value(self.expression(inner, locals)?),
            ast::Expression::Binary { op, lhs, rhs } => {
                let lhs = self.expression(lhs, locals)?;
                let rhs = self.expression(rhs, locals)?;
                self.binary(*op, lhs, rhs)
            }
        }
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
                return lhs
                    .into_iter()
                    .zip(rhs)
                    .map(|(lhs, rhs)| self.binary(op, lhs, rhs))
                    .collect::<Result<Vec<_>, _>>()
                    .map(Value::Array);
            }
            (Value::Array(lhs), rhs) => {
                return lhs
                    .into_iter()
                    .map(|lhs| self.binary(op, lhs, rhs.clone()))
                    .collect::<Result<Vec<_>, _>>()
                    .map(Value::Array);
            }
            (lhs, Value::Array(rhs)) => {
                return rhs
                    .into_iter()
                    .map(|rhs| self.binary(op, lhs.clone(), rhs))
                    .collect::<Result<Vec<_>, _>>()
                    .map(Value::Array);
            }
            (lhs, rhs) => return self.scalar_binary(op, lhs, rhs),
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

    fn checked_integer(&self, value: Option<i64>) -> Result<Value, EvaluationError> {
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
            Value::Boolean(_) | Value::Record(_) => Err(EvaluationError::Type("unary minus")),
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

    fn call(
        &mut self,
        call: &ast::FunctionCall,
        locals: &mut BTreeMap<String, Value>,
    ) -> Result<Vec<Value>, EvaluationError> {
        let arguments = call
            .arguments
            .iter()
            .map(|argument| self.expression(argument, locals))
            .collect::<Result<Vec<_>, _>>()?;
        let name = call.function.lexeme();
        if rumoca_ir_galec::builtins::is_builtin_name(name) {
            let builtin = rumoca_ir_galec::builtins::find_builtin(name)
                .or_else(|| rumoca_ir_galec::builtins::find_lifted_base(name))
                .ok_or_else(|| {
                    EvaluationError::MalformedCheckedBlock(format!(
                        "builtin catalog cannot resolve `{name}`"
                    ))
                })?;
            require_arity(name, builtin.inputs.len(), arguments.len())?;
            return self.builtin(name, arguments);
        }
        let function = self
            .block
            .protected_functions
            .iter()
            .chain(&self.block.public_functions)
            .find(|function| function.name.lexeme() == name)
            .ok_or_else(|| EvaluationError::UnknownName(name.to_owned()))?
            .clone();
        let mut frame = BTreeMap::new();
        let inputs = function
            .parameters
            .iter()
            .filter(|parameter| parameter.direction == ast::Direction::Input)
            .collect::<Vec<_>>();
        require_arity(name, inputs.len(), arguments.len())?;
        for (parameter, argument) in inputs.into_iter().zip(arguments) {
            if !self.value_matches_runtime_declaration(&parameter.decl, &argument, &frame)? {
                return Err(EvaluationError::Type("function input argument"));
            }
            frame.insert(parameter.decl.name.lexeme().to_owned(), argument);
        }
        for parameter in function
            .parameters
            .iter()
            .filter(|parameter| parameter.direction == ast::Direction::Output)
        {
            let value = self.zero_runtime_declaration(&parameter.decl, &frame)?;
            frame.insert(parameter.decl.name.lexeme().to_owned(), value);
        }
        for declaration in &function.locals {
            let value = self.zero_runtime_declaration(declaration, &frame)?;
            frame.insert(declaration.name.lexeme().to_owned(), value);
        }
        self.declaration_scopes.push(
            function
                .parameters
                .iter()
                .map(|parameter| &parameter.decl)
                .chain(&function.locals)
                .map(|declaration| {
                    (
                        declaration.name.lexeme().to_owned(),
                        declaration.clone(),
                    )
                })
                .collect(),
        );
        let result = self.statements(&function.statements, &mut frame);
        self.declaration_scopes.pop();
        result?;
        function
            .parameters
            .iter()
            .filter(|parameter| parameter.direction == ast::Direction::Output)
            .map(|parameter| {
                frame
                    .get(parameter.decl.name.lexeme())
                    .cloned()
                    .ok_or_else(|| {
                        EvaluationError::MalformedCheckedBlock(format!(
                            "function `{name}` lost output `{}`",
                            parameter.decl.name.lexeme()
                        ))
                    })
            })
            .collect()
    }

    fn builtin(&mut self, name: &str, args: Vec<Value>) -> Result<Vec<Value>, EvaluationError> {
        if let Some(base) = name
            .strip_suffix("1D")
            .or_else(|| name.strip_suffix("2D"))
        {
            return lift_builtin(self, base, args).map(|value| vec![value]);
        }
        match name {
            "solveLinearEquations" => solve_linear_equations(self, args).map(|value| vec![value]),
            "luFactorize" => lu_factorize_builtin(self, args),
            "luSolve" => lu_solve_builtin(self, args).map(|value| vec![value]),
            "interpolation1D" => interpolation_1d(args).map(|value| vec![value]),
            "interpolation2D" => interpolation_2d(args).map(|value| vec![value]),
            "interpolation3D" => interpolation_3d(args).map(|value| vec![value]),
            _ => scalar_builtin(self, name, args).map(|value| vec![value]),
        }
    }

    fn zero_runtime_declaration(
        &mut self,
        declaration: &ast::VariableDeclaration,
        frame: &BTreeMap<String, Value>,
    ) -> Result<Value, EvaluationError> {
        let scalar_decl = ast::VariableDeclaration {
            dimensions: Vec::new(),
            ..declaration.clone()
        };
        let scalar = zero_declaration(self.block, &scalar_decl)?;
        declaration
            .dimensions
            .iter()
            .rev()
            .try_fold(scalar, |value, dimension| {
                let ast::Dimension::Expr(expression) = dimension else {
                    return Err(EvaluationError::MalformedCheckedBlock(format!(
                        "derived dimension used for non-input `{}`",
                        declaration.name.lexeme()
                    )));
                };
                let size = self
                    .expression(expression, frame)?
                    .integer()
                    .ok_or(EvaluationError::Type("runtime dimension"))?;
                let size = usize::try_from(size).map_err(|_| {
                    EvaluationError::MalformedCheckedBlock(format!(
                        "invalid runtime dimension {size} on `{}`",
                        declaration.name.lexeme()
                    ))
                })?;
                if size == 0 {
                    return Err(EvaluationError::MalformedCheckedBlock(format!(
                        "zero runtime dimension on `{}`",
                        declaration.name.lexeme()
                    )));
                }
                Ok(Value::Array(vec![value; size]))
            })
    }

    fn value_matches_runtime_declaration(
        &mut self,
        declaration: &ast::VariableDeclaration,
        value: &Value,
        frame: &BTreeMap<String, Value>,
    ) -> Result<bool, EvaluationError> {
        let mut elements = vec![value];
        for dimension in &declaration.dimensions {
            let expected = match dimension {
                ast::Dimension::Derived => None,
                ast::Dimension::Expr(expression) => {
                    let size = self
                        .expression(expression, frame)?
                        .integer()
                        .ok_or(EvaluationError::Type("runtime dimension"))?;
                    Some(usize::try_from(size).map_err(|_| {
                        EvaluationError::MalformedCheckedBlock(format!(
                            "invalid runtime dimension {size} on `{}`",
                            declaration.name.lexeme()
                        ))
                    })?)
                }
            };
            let mut nested = Vec::new();
            for element in elements {
                let Value::Array(values) = element else {
                    return Ok(false);
                };
                if expected.is_some_and(|expected| values.len() != expected) || values.is_empty() {
                    return Ok(false);
                }
                nested.extend(values);
            }
            elements = nested;
        }
        elements
            .into_iter()
            .map(|element| value_matches_type(self.block, &declaration.ty, element))
            .collect::<Result<Vec<_>, _>>()
            .map(|matches| matches.into_iter().all(|matches| matches))
    }

    fn reference(
        &self,
        reference: &ast::Reference,
        locals: &BTreeMap<String, Value>,
    ) -> Result<Value, EvaluationError> {
        match reference {
            ast::Reference::Local(part) => {
                let value = locals
                    .get(part.name.lexeme())
                    .ok_or_else(|| EvaluationError::UnknownName(part.name.lexeme().to_owned()))?;
                indexed(value, &part.subscripts, self, locals)
            }
            ast::Reference::State(parts) => {
                let first = parts.first().ok_or_else(|| {
                    EvaluationError::UnknownName("empty self reference".to_owned())
                })?;
                let mut value = self
                    .state
                    .get(first.name.lexeme())
                    .ok_or_else(|| EvaluationError::UnknownName(first.name.lexeme().to_owned()))?
                    .value
                    .clone();
                value = indexed_owned(value, &first.subscripts, self, locals)?;
                for part in &parts[1..] {
                    let Value::Record(fields) = value else {
                        return Err(EvaluationError::Type("record component reference"));
                    };
                    value = fields
                        .get(part.name.lexeme())
                        .cloned()
                        .ok_or_else(|| EvaluationError::UnknownName(part.name.lexeme().to_owned()))?;
                    value = indexed_owned(value, &part.subscripts, self, locals)?;
                }
                Ok(value)
            }
        }
    }

    fn assign(
        &mut self,
        reference: &ast::Reference,
        value: Value,
        locals: &mut BTreeMap<String, Value>,
    ) -> Result<(), EvaluationError> {
        match reference {
            ast::Reference::Local(part) => {
                let indices = indices(&part.subscripts, self, locals)?;
                let slot = locals
                    .get_mut(part.name.lexeme())
                    .ok_or_else(|| EvaluationError::UnknownName(part.name.lexeme().to_owned()))?;
                assign_indices(slot, &indices, value)
            }
            ast::Reference::State(parts) => {
                let first = parts.first().ok_or_else(|| {
                    EvaluationError::MalformedCheckedBlock(
                        "empty checked state reference".to_owned(),
                    )
                })?;
                let evaluated = parts
                    .iter()
                    .map(|part| indices(&part.subscripts, self, locals))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut slot = self
                    .state
                    .remove(first.name.lexeme())
                    .ok_or_else(|| EvaluationError::UnknownName(first.name.lexeme().to_owned()))?;
                let result = assign_state_parts(&mut slot.value, parts, &evaluated, value);
                self.state.insert(first.name.lexeme().to_owned(), slot);
                result
            }
        }
    }

    fn limit_all(&mut self) -> Result<(), EvaluationError> {
        let names = self.state.keys().cloned().collect::<Vec<_>>();
        for name in names {
            let mut slot = self
                .state
                .remove(&name)
                .ok_or_else(|| EvaluationError::UnknownName(name.clone()))?;
            self.limit_declaration_value(
                slot.declaration,
                &mut slot.value,
                &BTreeMap::new(),
            )?;
            self.state.insert(name, slot);
        }
        Ok(())
    }

    fn limit_reference(
        &mut self,
        reference: &ast::Reference,
        locals: &mut BTreeMap<String, Value>,
    ) -> Result<(), EvaluationError> {
        match reference {
            ast::Reference::Local(part) => {
                let declaration = self
                    .declaration_scopes
                    .last()
                    .and_then(|scope| scope.get(part.name.lexeme()))
                    .cloned()
                    .ok_or_else(|| {
                        EvaluationError::MalformedCheckedBlock(format!(
                            "checked local limit target `{}` has no runtime declaration",
                            part.name.lexeme()
                        ))
                    })?;
                let evaluated = indices(&part.subscripts, self, locals)?;
                let mut value = locals
                    .remove(part.name.lexeme())
                    .ok_or_else(|| EvaluationError::UnknownName(part.name.lexeme().to_owned()))?;
                let result = self.limit_declaration_value(
                    &declaration,
                    indexed_mut(&mut value, &evaluated)?,
                    locals,
                );
                locals.insert(part.name.lexeme().to_owned(), value);
                result
            }
            ast::Reference::State(parts) => {
                let first = parts.first().ok_or_else(|| {
                    EvaluationError::MalformedCheckedBlock(
                        "empty checked limit reference".to_owned(),
                    )
                })?;
                let declaration = state_path_declaration(self.block, parts)?.clone();
                let evaluated = parts
                    .iter()
                    .map(|part| indices(&part.subscripts, self, locals))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut slot = self
                    .state
                    .remove(first.name.lexeme())
                    .ok_or_else(|| EvaluationError::UnknownName(first.name.lexeme().to_owned()))?;
                let result = self.limit_declaration_value(
                    &declaration,
                    state_path_mut(&mut slot.value, parts, &evaluated)?,
                    locals,
                );
                self.state.insert(first.name.lexeme().to_owned(), slot);
                result
            }
        }
    }

    fn limit_declaration_value(
        &mut self,
        declaration: &ast::VariableDeclaration,
        value: &mut Value,
        locals: &BTreeMap<String, Value>,
    ) -> Result<(), EvaluationError> {
        let min = declaration
            .range
            .min
            .as_ref()
            .map(|bound| self.expression(bound, locals))
            .transpose()?;
        let max = declaration
            .range
            .max
            .as_ref()
            .map(|bound| self.expression(bound, locals))
            .transpose()?;
        limit_value(value, min.as_ref(), max.as_ref())?;
        if let ast::TypeRef::Compartment(name) = &declaration.ty {
            let compartment = self
                .block
                .compartments
                .iter()
                .find(|compartment| compartment.name.lexeme() == name.lexeme())
                .cloned()
                .ok_or_else(|| {
                    EvaluationError::MalformedCheckedBlock(format!(
                        "unknown compartment `{}`",
                        name.lexeme()
                    ))
                })?;
            self.limit_compartment_value(value, &compartment, locals)?;
        }
        Ok(())
    }

    fn limit_compartment_value(
        &mut self,
        value: &mut Value,
        compartment: &ast::StateCompartment,
        locals: &BTreeMap<String, Value>,
    ) -> Result<(), EvaluationError> {
        if let Value::Array(values) = value {
            for value in values {
                self.limit_compartment_value(value, compartment, locals)?;
            }
            return Ok(());
        }
        let Value::Record(fields) = value else {
            return Err(EvaluationError::Type("compartment value"));
        };
        for entity in &compartment.entities {
            let field = fields.get_mut(entity.decl.name.lexeme()).ok_or_else(|| {
                EvaluationError::MalformedCheckedBlock(format!(
                    "compartment value lost `{}`",
                    entity.decl.name.lexeme()
                ))
            })?;
            self.limit_declaration_value(&entity.decl, field, locals)?;
        }
        Ok(())
    }
}

fn declarations(
    block: &ast::Block,
    values: &[ast::VariableDeclaration],
) -> Result<BTreeMap<String, Value>, EvaluationError> {
    values
        .iter()
        .map(|declaration| {
            Ok((
                declaration.name.lexeme().to_owned(),
                zero_declaration(block, declaration)?,
            ))
        })
        .collect()
}

fn zero_declaration(
    block: &ast::Block,
    declaration: &ast::VariableDeclaration,
) -> Result<Value, EvaluationError> {
    zero_declaration_inner(block, declaration, &mut BTreeSet::new())
}

fn zero_declaration_inner(
    block: &ast::Block,
    declaration: &ast::VariableDeclaration,
    compartment_stack: &mut BTreeSet<String>,
) -> Result<Value, EvaluationError> {
    let scalar = match &declaration.ty {
        ast::TypeRef::Primitive(ast::ScalarType::Boolean) => Value::Boolean(false),
        ast::TypeRef::Primitive(ast::ScalarType::Integer) => Value::Integer(0),
        ast::TypeRef::Primitive(ast::ScalarType::Real) => Value::Real(0.0),
        ast::TypeRef::Compartment(name) => {
            if !compartment_stack.insert(name.lexeme().to_owned()) {
                return Err(EvaluationError::MalformedCheckedBlock(format!(
                    "recursive compartment `{}`",
                    name.lexeme()
                )));
            }
            let compartment = block
                .compartments
                .iter()
                .find(|compartment| compartment.name.lexeme() == name.lexeme())
                .ok_or_else(|| {
                    EvaluationError::MalformedCheckedBlock(format!(
                        "unknown compartment `{}`",
                        name.lexeme()
                    ))
                })?;
            let fields = compartment
                .entities
                .iter()
                .map(|entity| {
                    Ok((
                        entity.decl.name.lexeme().to_owned(),
                        zero_declaration_inner(block, &entity.decl, compartment_stack)?,
                    ))
                })
                .collect::<Result<BTreeMap<_, _>, EvaluationError>>()?;
            compartment_stack.remove(name.lexeme());
            Value::Record(fields)
        }
    };
    declaration
        .dimensions
        .iter()
        .rev()
        .try_fold(scalar, |value, dimension| {
            let ast::Dimension::Expr(ast::Expression::Integer(size)) = dimension else {
                return Err(EvaluationError::MalformedCheckedBlock(format!(
                    "non-literal runtime dimension on `{}`",
                    declaration.name.lexeme()
                )));
            };
            let size = usize::try_from(*size).map_err(|_| {
                EvaluationError::MalformedCheckedBlock(format!(
                    "invalid runtime dimension {size} on `{}`",
                    declaration.name.lexeme()
                ))
            })?;
            if size == 0 {
                return Err(EvaluationError::MalformedCheckedBlock(format!(
                    "zero runtime dimension on `{}`",
                    declaration.name.lexeme()
                )));
            }
            Ok(Value::Array(vec![value; size]))
        })
}

fn value_matches_declaration(
    block: &ast::Block,
    declaration: &ast::VariableDeclaration,
    value: &Value,
) -> Result<bool, EvaluationError> {
    let mut element = value;
    for dimension in &declaration.dimensions {
        let ast::Dimension::Expr(ast::Expression::Integer(size)) = dimension else {
            return Err(EvaluationError::MalformedCheckedBlock(format!(
                "non-literal block dimension on `{}`",
                declaration.name.lexeme()
            )));
        };
        let expected = usize::try_from(*size).map_err(|_| {
            EvaluationError::MalformedCheckedBlock(format!(
                "invalid block dimension {size} on `{}`",
                declaration.name.lexeme()
            ))
        })?;
        let Value::Array(values) = element else {
            return Ok(false);
        };
        if values.len() != expected {
            return Ok(false);
        }
        let Some(first) = values.first() else {
            return Ok(false);
        };
        if !values.iter().all(|candidate| same_shape(first, candidate)) {
            return Ok(false);
        }
        element = first;
    }
    value_matches_type(block, &declaration.ty, element)
}

fn value_matches_type(
    block: &ast::Block,
    ty: &ast::TypeRef,
    value: &Value,
) -> Result<bool, EvaluationError> {
    match (ty, value) {
        (ast::TypeRef::Primitive(ast::ScalarType::Boolean), Value::Boolean(_))
        | (ast::TypeRef::Primitive(ast::ScalarType::Integer), Value::Integer(_))
        | (ast::TypeRef::Primitive(ast::ScalarType::Real), Value::Real(_)) => Ok(true),
        (ast::TypeRef::Compartment(name), Value::Record(fields)) => {
            let compartment = block
                .compartments
                .iter()
                .find(|candidate| candidate.name.lexeme() == name.lexeme())
                .ok_or_else(|| {
                    EvaluationError::MalformedCheckedBlock(format!(
                        "unknown compartment `{}`",
                        name.lexeme()
                    ))
                })?;
            if fields.len() != compartment.entities.len() {
                return Ok(false);
            }
            for entity in &compartment.entities {
                let Some(field) = fields.get(entity.decl.name.lexeme()) else {
                    return Ok(false);
                };
                if !value_matches_declaration(block, &entity.decl, field)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        _ => Ok(false),
    }
}

fn same_shape(lhs: &Value, rhs: &Value) -> bool {
    match (lhs, rhs) {
        (Value::Array(lhs), Value::Array(rhs)) => {
            lhs.len() == rhs.len()
                && lhs
                    .iter()
                    .zip(rhs)
                    .all(|(lhs, rhs)| same_shape(lhs, rhs))
        }
        (Value::Record(lhs), Value::Record(rhs)) => {
            lhs.len() == rhs.len()
                && lhs
                    .iter()
                    .all(|(name, lhs)| rhs.get(name).is_some_and(|rhs| same_shape(lhs, rhs)))
        }
        (Value::Boolean(_), Value::Boolean(_))
        | (Value::Integer(_), Value::Integer(_))
        | (Value::Real(_), Value::Real(_)) => true,
        _ => false,
    }
}

fn require_arity(name: &str, expected: usize, found: usize) -> Result<(), EvaluationError> {
    if expected == found {
        return Ok(());
    }
    Err(EvaluationError::InputArity {
        name: name.to_owned(),
        expected,
        found,
    })
}

fn not_value(value: Value) -> Result<Value, EvaluationError> {
    match value {
        Value::Boolean(value) => Ok(Value::Boolean(!value)),
        Value::Array(values) => values
            .into_iter()
            .map(not_value)
            .collect::<Result<Vec<_>, _>>()
            .map(Value::Array),
        Value::Integer(_) | Value::Real(_) | Value::Record(_) => {
            Err(EvaluationError::Type("not"))
        }
    }
}

fn value_in_integer_domain(value: &Value, domain: IntegerDomain) -> bool {
    match value {
        Value::Integer(value) => domain.contains(*value),
        Value::Array(values) => values
            .iter()
            .all(|value| value_in_integer_domain(value, domain)),
        Value::Record(fields) => fields
            .values()
            .all(|value| value_in_integer_domain(value, domain)),
        Value::Boolean(_) | Value::Real(_) => true,
    }
}

fn compare_ordered<T: PartialOrd + PartialEq>(
    op: ast::BinaryOp,
    a: T,
    b: T,
) -> Result<bool, EvaluationError> {
    Ok(match op {
        ast::BinaryOp::Lt => a < b,
        ast::BinaryOp::Gt => a > b,
        ast::BinaryOp::Le => a <= b,
        ast::BinaryOp::Ge => a >= b,
        ast::BinaryOp::Eq => a == b,
        ast::BinaryOp::Ne => a != b,
        _ => {
            return Err(EvaluationError::MalformedCheckedBlock(
                "non-comparison operator reached comparison evaluator".to_owned(),
            ));
        }
    })
}

fn size_dimension(value: &Value, dimension: i64) -> Result<Value, EvaluationError> {
    if dimension < 1 {
        return Err(EvaluationError::Bounds {
            index: dimension,
            length: 0,
        });
    }
    let mut current = value;
    for _ in 1..dimension {
        let Value::Array(values) = current else {
            return Err(EvaluationError::Type("size array"));
        };
        current = values
            .first()
            .ok_or(EvaluationError::Type("empty checked array"))?;
    }
    let Value::Array(values) = current else {
        return Err(EvaluationError::Type("size array"));
    };
    Ok(Value::Integer(i64::try_from(values.len()).map_err(
        |_| EvaluationError::MalformedCheckedBlock("array length exceeds GALEC Integer".to_owned()),
    )?))
}

fn indices(
    subscripts: &[ast::Expression],
    evaluator: &Evaluator<'_>,
    locals: &BTreeMap<String, Value>,
) -> Result<Vec<usize>, EvaluationError> {
    let mut shadow = Evaluator {
        block: evaluator.block,
        state: evaluator.state.clone(),
        active_signals: evaluator.active_signals.clone(),
        signal_closures: evaluator.signal_closures.clone(),
        declaration_scopes: evaluator.declaration_scopes.clone(),
        integer_domain: evaluator.integer_domain,
    };
    subscripts
        .iter()
        .map(|subscript| {
            let index = shadow
                .expression(subscript, locals)?
                .integer()
                .ok_or(EvaluationError::Type("array subscript"))?;
            let zero_based = index.checked_sub(1).ok_or(EvaluationError::Bounds {
                index,
                length: 0,
            })?;
            usize::try_from(zero_based).map_err(|_| EvaluationError::Bounds { index, length: 0 })
        })
        .collect()
}

fn indexed(
    value: &Value,
    subscripts: &[ast::Expression],
    evaluator: &Evaluator<'_>,
    locals: &BTreeMap<String, Value>,
) -> Result<Value, EvaluationError> {
    indexed_owned(value.clone(), subscripts, evaluator, locals)
}

fn indexed_owned(
    mut value: Value,
    subscripts: &[ast::Expression],
    evaluator: &Evaluator<'_>,
    locals: &BTreeMap<String, Value>,
) -> Result<Value, EvaluationError> {
    for index in indices(subscripts, evaluator, locals)? {
        let Value::Array(values) = value else {
            return Err(EvaluationError::Type("array reference"));
        };
        let length = values.len();
        value = values
            .get(index)
            .cloned()
            .ok_or(EvaluationError::Bounds {
                index: i64::try_from(index + 1).map_err(|_| {
                    EvaluationError::MalformedCheckedBlock(
                        "array index exceeds GALEC Integer".to_owned(),
                    )
                })?,
                length,
            })?;
    }
    Ok(value)
}

fn assign_indices(
    slot: &mut Value,
    indices: &[usize],
    value: Value,
) -> Result<(), EvaluationError> {
    let mut current = slot;
    for &index in indices {
        let Value::Array(values) = current else {
            return Err(EvaluationError::Type("array assignment"));
        };
        let length = values.len();
        current = values.get_mut(index).ok_or(EvaluationError::Bounds {
            index: i64::try_from(index + 1).map_err(|_| {
                EvaluationError::MalformedCheckedBlock(
                    "array index exceeds GALEC Integer".to_owned(),
                )
            })?,
            length,
        })?;
    }
    *current = value;
    Ok(())
}

fn assign_state_parts(
    root: &mut Value,
    parts: &[ast::RefPart],
    indices_by_part: &[Vec<usize>],
    value: Value,
) -> Result<(), EvaluationError> {
    *state_path_mut(root, parts, indices_by_part)? = value;
    Ok(())
}

fn state_path_mut<'a>(
    root: &'a mut Value,
    parts: &[ast::RefPart],
    indices_by_part: &[Vec<usize>],
) -> Result<&'a mut Value, EvaluationError> {
    if parts.len() != indices_by_part.len() || parts.is_empty() {
        return Err(EvaluationError::MalformedCheckedBlock(
            "state reference path is inconsistent".to_owned(),
        ));
    }
    let mut current = indexed_mut(root, &indices_by_part[0])?;
    for (part, indices) in parts[1..].iter().zip(&indices_by_part[1..]) {
        let Value::Record(fields) = current else {
            return Err(EvaluationError::Type("record component reference"));
        };
        current = fields
            .get_mut(part.name.lexeme())
            .ok_or_else(|| EvaluationError::UnknownName(part.name.lexeme().to_owned()))?;
        current = indexed_mut(current, indices)?;
    }
    Ok(current)
}

fn state_path_declaration<'a>(
    block: &'a ast::Block,
    parts: &[ast::RefPart],
) -> Result<&'a ast::VariableDeclaration, EvaluationError> {
    let first = parts.first().ok_or_else(|| {
        EvaluationError::MalformedCheckedBlock("empty checked state reference".to_owned())
    })?;
    let mut declaration = block
        .interface
        .iter()
        .map(|variable| &variable.decl)
        .chain(block.protected.iter().map(|entity| &entity.decl))
        .find(|candidate| candidate.name.lexeme() == first.name.lexeme())
        .ok_or_else(|| EvaluationError::UnknownName(first.name.lexeme().to_owned()))?;
    for part in &parts[1..] {
        let ast::TypeRef::Compartment(name) = &declaration.ty else {
            return Err(EvaluationError::Type("record component declaration"));
        };
        let compartment = block
            .compartments
            .iter()
            .find(|candidate| candidate.name.lexeme() == name.lexeme())
            .ok_or_else(|| {
                EvaluationError::MalformedCheckedBlock(format!(
                    "unknown compartment `{}`",
                    name.lexeme()
                ))
            })?;
        declaration = compartment
            .entities
            .iter()
            .map(|entity| &entity.decl)
            .find(|candidate| candidate.name.lexeme() == part.name.lexeme())
            .ok_or_else(|| EvaluationError::UnknownName(part.name.lexeme().to_owned()))?;
    }
    Ok(declaration)
}

fn indexed_mut<'a>(
    mut value: &'a mut Value,
    indices: &[usize],
) -> Result<&'a mut Value, EvaluationError> {
    for &index in indices {
        let Value::Array(values) = value else {
            return Err(EvaluationError::Type("array assignment"));
        };
        let length = values.len();
        value = values.get_mut(index).ok_or(EvaluationError::Bounds {
            index: i64::try_from(index + 1).map_err(|_| {
                EvaluationError::MalformedCheckedBlock(
                    "array index exceeds GALEC Integer".to_owned(),
                )
            })?,
            length,
        })?;
    }
    Ok(value)
}

fn limit_value(
    value: &mut Value,
    min: Option<&Value>,
    max: Option<&Value>,
) -> Result<(), EvaluationError> {
    match value {
        Value::Array(values) => {
            for value in values {
                limit_value(value, min, max)?;
            }
        }
        Value::Real(number) => {
            if number.is_nan() {
                return Ok(());
            }
            if let Some(minimum) = min {
                let minimum = minimum
                    .real()
                    .ok_or(EvaluationError::Type("Real minimum bound"))?;
                *number = number.max(minimum);
            }
            if let Some(maximum) = max {
                let maximum = maximum
                    .real()
                    .ok_or(EvaluationError::Type("Real maximum bound"))?;
                *number = number.min(maximum);
            }
        }
        Value::Integer(number) => {
            if let Some(minimum) = min {
                let minimum = minimum
                    .integer()
                    .ok_or(EvaluationError::Type("Integer minimum bound"))?;
                *number = (*number).max(minimum);
            }
            if let Some(maximum) = max {
                let maximum = maximum
                    .integer()
                    .ok_or(EvaluationError::Type("Integer maximum bound"))?;
                *number = (*number).min(maximum);
            }
        }
        Value::Boolean(_) | Value::Record(_) if min.is_some() || max.is_some() => {
            return Err(EvaluationError::Type("range bound"));
        }
        Value::Boolean(_) | Value::Record(_) => {}
    }
    Ok(())
}

fn lift_builtin(
    evaluator: &mut Evaluator<'_>,
    base: &str,
    arguments: Vec<Value>,
) -> Result<Value, EvaluationError> {
    let lengths = arguments
        .iter()
        .filter_map(|value| match value {
            Value::Array(values) => Some(values.len()),
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    if lengths.len() != 1 {
        return Err(EvaluationError::Type("lifted builtin shape"));
    }
    let length = *lengths.first().expect("one lifted length");
    let mut result = Vec::with_capacity(length);
    for index in 0..length {
        let scalar_args = arguments
            .iter()
            .map(|argument| match argument {
                Value::Array(values) => values[index].clone(),
                scalar => scalar.clone(),
            })
            .collect();
        result.push(scalar_builtin(evaluator, base, scalar_args)?);
    }
    Ok(Value::Array(result))
}

fn scalar_builtin(
    evaluator: &mut Evaluator<'_>,
    name: &str,
    arguments: Vec<Value>,
) -> Result<Value, EvaluationError> {
    let real1 = || {
        arguments
            .first()
            .and_then(Value::real)
            .ok_or(EvaluationError::Type("Real builtin argument"))
    };
    let real2 = || {
        Ok((
            arguments
                .first()
                .and_then(Value::real)
                .ok_or(EvaluationError::Type("Real builtin argument"))?,
            arguments
                .get(1)
                .and_then(Value::real)
                .ok_or(EvaluationError::Type("Real builtin argument"))?,
        ))
    };
    Ok(match name {
        "minInteger" => Value::Integer(evaluator.integer_domain.min()),
        "maxInteger" => Value::Integer(evaluator.integer_domain.max()),
        "minReal" => Value::Real(-f64::MAX),
        "maxReal" => Value::Real(f64::MAX),
        "posMinReal" => Value::Real(f64::MIN_POSITIVE),
        "epsReal" => Value::Real(f64::EPSILON),
        "nan" => Value::Real(f64::NAN),
        "minusInfinite" => Value::Real(f64::NEG_INFINITY),
        "plusInfinite" => Value::Real(f64::INFINITY),
        "euler" => Value::Real(std::f64::consts::E),
        "pi" => Value::Real(std::f64::consts::PI),
        "isNaN" => Value::Boolean(real1()?.is_nan()),
        "isInfinite" => Value::Boolean(real1()?.is_infinite()),
        "isFinite" => Value::Boolean(real1()?.is_finite()),
        "real" => Value::Real(
            arguments
                .first()
                .and_then(Value::integer)
                .ok_or(EvaluationError::Type("real conversion"))? as f64,
        ),
        "integer" => {
            let value = real1()?;
            if value.is_nan() {
                evaluator.active_signals.insert("NAN".to_owned());
                Value::Integer(0)
            } else if value > evaluator.integer_domain.max() as f64
                || value < evaluator.integer_domain.min() as f64
            {
                evaluator.active_signals.insert("OVERFLOW".to_owned());
                Value::Integer(0)
            } else {
                Value::Integer(value.trunc() as i64)
            }
        }
        "roundDown" => Value::Real(real1()?.floor()),
        "roundUp" => Value::Real(real1()?.ceil()),
        "roundHalfToEven" => Value::Real(real1()?.round_ties_even()),
        "sign" => Value::Real(real1()?.signum()),
        "absolute" => Value::Real(real1()?.abs()),
        "fractional" => Value::Real(real1()?.fract()),
        "sqrt" => Value::Real(real1()?.sqrt()),
        "exp" => Value::Real(real1()?.exp()),
        "ln" => Value::Real(real1()?.ln()),
        "lg" => Value::Real(real1()?.log10()),
        "safe_posdiv" => {
            let numerator = real1()?;
            let denominator = arguments
                .get(1)
                .and_then(Value::real)
                .ok_or(EvaluationError::Type("Real builtin argument"))?;
            let epsilon = arguments
                .get(2)
                .and_then(Value::real)
                .ok_or(EvaluationError::Type("Real builtin argument"))?;
            Value::Real(if numerator.is_nan() || denominator.is_nan() || epsilon.is_nan() {
                f64::NAN
            } else {
                numerator / denominator.max(epsilon.max(f64::MIN_POSITIVE))
            })
        }
        "safe_sqrt" => {
            let value = real1()?;
            Value::Real(if value.is_nan() {
                f64::NAN
            } else {
                value.max(0.0).sqrt()
            })
        }
        "safe_ln" => {
            let value = real1()?;
            Value::Real(if value.is_nan() {
                f64::NAN
            } else {
                value.max(0.0).ln()
            })
        }
        "safe_lg" => {
            let value = real1()?;
            Value::Real(if value.is_nan() {
                f64::NAN
            } else {
                value.max(0.0).log10()
            })
        }
        "sin" => Value::Real(real1()?.sin()),
        "cos" => Value::Real(real1()?.cos()),
        "tan" => Value::Real(real1()?.tan()),
        "asin" => Value::Real(real1()?.asin()),
        "acos" => Value::Real(real1()?.acos()),
        "atan" => Value::Real(real1()?.atan()),
        "sinh" => Value::Real(real1()?.sinh()),
        "cosh" => Value::Real(real1()?.cosh()),
        "tanh" => Value::Real(real1()?.tanh()),
        "safe_tan" => {
            let value = real1()?;
            Value::Real(if value >= std::f64::consts::FRAC_PI_2 {
                f64::INFINITY
            } else if value <= -std::f64::consts::FRAC_PI_2 {
                f64::NEG_INFINITY
            } else {
                value.tan()
            })
        }
        "safe_asin" => {
            let value = real1()?;
            Value::Real(if value.is_nan() {
                f64::NAN
            } else {
                value.clamp(-1.0, 1.0).asin()
            })
        }
        "safe_acos" => {
            let value = real1()?;
            Value::Real(if value.is_nan() {
                f64::NAN
            } else {
                value.clamp(-1.0, 1.0).acos()
            })
        }
        "atan2" => {
            let (y, x) = real2()?;
            Value::Real(y.atan2(x))
        }
        "min" => {
            let (a, b) = real2()?;
            Value::Real(a.min(b))
        }
        "max" => {
            let (a, b) = real2()?;
            Value::Real(a.max(b))
        }
        "imin" | "imax" => {
            let a = arguments
                .first()
                .and_then(Value::integer)
                .ok_or(EvaluationError::Type("Integer builtin argument"))?;
            let b = arguments
                .get(1)
                .and_then(Value::integer)
                .ok_or(EvaluationError::Type("Integer builtin argument"))?;
            Value::Integer(if name == "imin" { a.min(b) } else { a.max(b) })
        }
        "divisionTowardsZero" | "remainderTowardsZero" => {
            let a = arguments
                .first()
                .and_then(Value::integer)
                .ok_or(EvaluationError::Type("Integer builtin argument"))?;
            let b = arguments
                .get(1)
                .and_then(Value::integer)
                .ok_or(EvaluationError::Type("Integer builtin argument"))?;
            if b == 0 {
                return Err(EvaluationError::IntegerDivisionByZero);
            }
            return evaluator.checked_integer(Some(if name == "divisionTowardsZero" {
                a.checked_div(b).ok_or(EvaluationError::IntegerOverflow)?
            } else {
                a.checked_rem(b).ok_or(EvaluationError::IntegerOverflow)?
            }));
        }
        "realRemainderTowardsZero" => {
            let (a, b) = real2()?;
            Value::Real(a % b)
        }
        "hasNaN1D" | "hasNaN2D" => Value::Boolean(has_nan(
            arguments
                .first()
                .ok_or(EvaluationError::Type("array builtin argument"))?,
        )),
        _ => return Err(EvaluationError::UnsupportedBuiltin(name.to_owned())),
    })
}

fn solve_linear_equations(
    evaluator: &mut Evaluator<'_>,
    arguments: Vec<Value>,
) -> Result<Value, EvaluationError> {
    let matrix = real_matrix(&arguments[0], "solveLinearEquations")?;
    let rhs = real_vector(&arguments[1], "solveLinearEquations")?;
    let (lu, pivots, singular) = factorize(matrix)?;
    let (solution, solve_failed) = solve_lu(&lu, &pivots, &rhs)?;
    if singular || solve_failed {
        evaluator
            .active_signals
            .insert("SOLVE_LINEAR_EQUATIONS_FAILED".to_owned());
    }
    Ok(Value::Array(
        solution.into_iter().map(Value::Real).collect(),
    ))
}

fn lu_factorize_builtin(
    evaluator: &mut Evaluator<'_>,
    arguments: Vec<Value>,
) -> Result<Vec<Value>, EvaluationError> {
    let matrix = real_matrix(&arguments[0], "luFactorize")?;
    let (lu, pivots, singular) = factorize(matrix)?;
    if singular {
        evaluator
            .active_signals
            .insert("SOLVE_LINEAR_EQUATIONS_FAILED".to_owned());
    }
    Ok(vec![
        matrix_value(lu),
        Value::Array(
            pivots
                .into_iter()
                .map(|pivot| Value::Integer(i64::try_from(pivot + 1).unwrap_or(i64::MAX)))
                .collect(),
        ),
    ])
}

fn lu_solve_builtin(
    evaluator: &mut Evaluator<'_>,
    arguments: Vec<Value>,
) -> Result<Value, EvaluationError> {
    let lu = real_matrix(&arguments[0], "luSolve")?;
    let pivots = integer_vector(&arguments[1], "luSolve")?
        .into_iter()
        .map(|pivot| {
            pivot
                .checked_sub(1)
                .and_then(|pivot| usize::try_from(pivot).ok())
                .ok_or(EvaluationError::InvalidBuiltinArgument {
                    name: "luSolve",
                    detail: "pivot is not a positive index",
                })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let rhs = real_vector(&arguments[2], "luSolve")?;
    let (solution, failed) = solve_lu(&lu, &pivots, &rhs)?;
    if failed {
        evaluator
            .active_signals
            .insert("SOLVE_LINEAR_EQUATIONS_FAILED".to_owned());
    }
    Ok(Value::Array(
        solution.into_iter().map(Value::Real).collect(),
    ))
}

fn factorize(mut matrix: Vec<Vec<f64>>) -> Result<(Vec<Vec<f64>>, Vec<usize>, bool), EvaluationError> {
    let n = matrix.len();
    if n == 0 || matrix.iter().any(|row| row.len() != n) {
        return Err(EvaluationError::InvalidBuiltinArgument {
            name: "luFactorize",
            detail: "matrix must be non-empty and square",
        });
    }
    let mut pivots = (0..n).collect::<Vec<_>>();
    let mut singular = false;
    for column in 0..n {
        let pivot = (column..n)
            .max_by(|&lhs, &rhs| {
                matrix[lhs][column]
                    .abs()
                    .total_cmp(&matrix[rhs][column].abs())
            })
            .expect("non-empty pivot range");
        matrix.swap(column, pivot);
        pivots.swap(column, pivot);
        let diagonal = matrix[column][column];
        if diagonal == 0.0 || diagonal.is_nan() {
            matrix[column][column] = f64::NAN;
            singular = true;
            continue;
        }
        for row in column + 1..n {
            matrix[row][column] /= diagonal;
            for inner in column + 1..n {
                matrix[row][inner] -= matrix[row][column] * matrix[column][inner];
            }
        }
    }
    Ok((matrix, pivots, singular))
}

fn solve_lu(
    lu: &[Vec<f64>],
    pivots: &[usize],
    rhs: &[f64],
) -> Result<(Vec<f64>, bool), EvaluationError> {
    let n = lu.len();
    if n == 0
        || lu.iter().any(|row| row.len() != n)
        || pivots.len() != n
        || rhs.len() != n
        || pivots.iter().any(|pivot| *pivot >= n)
    {
        return Err(EvaluationError::InvalidBuiltinArgument {
            name: "luSolve",
            detail: "matrix, pivots, and right-hand side have incompatible shapes",
        });
    }
    let mut solution = vec![0.0; n];
    for row in 0..n {
        solution[row] = rhs[pivots[row]];
        for column in 0..row {
            solution[row] -= lu[row][column] * solution[column];
        }
    }
    let mut failed = false;
    for row in (0..n).rev() {
        for column in row + 1..n {
            solution[row] -= lu[row][column] * solution[column];
        }
        solution[row] /= lu[row][row];
        failed |= solution[row].is_nan();
    }
    Ok((solution, failed))
}

fn interpolation_1d(arguments: Vec<Value>) -> Result<Value, EvaluationError> {
    let x = arguments[0]
        .real()
        .ok_or(EvaluationError::Type("interpolation1D x"))?;
    let axis = real_vector(&arguments[1], "interpolation1D")?;
    let count = interpolation_count(&arguments[2], axis.len(), "interpolation1D")?;
    let values = real_vector(&arguments[3], "interpolation1D")?;
    if values.len() < count {
        return invalid_interpolation("interpolation1D");
    }
    let mode = interpolation_options(&arguments[4..6], "interpolation1D")?;
    Ok(Value::Real(interpolate_axis(
        x,
        &axis[..count],
        &values[..count],
        mode,
    )?))
}

fn interpolation_2d(arguments: Vec<Value>) -> Result<Value, EvaluationError> {
    let x1 = arguments[0]
        .real()
        .ok_or(EvaluationError::Type("interpolation2D x1"))?;
    let x2 = arguments[1]
        .real()
        .ok_or(EvaluationError::Type("interpolation2D x2"))?;
    let axis1 = real_vector(&arguments[2], "interpolation2D")?;
    let n1 = interpolation_count(&arguments[3], axis1.len(), "interpolation2D")?;
    let axis2 = real_vector(&arguments[4], "interpolation2D")?;
    let n2 = interpolation_count(&arguments[5], axis2.len(), "interpolation2D")?;
    let values = real_matrix(&arguments[6], "interpolation2D")?;
    if values.len() < n1 || values.iter().take(n1).any(|row| row.len() < n2) {
        return invalid_interpolation("interpolation2D");
    }
    let mode = interpolation_options(&arguments[7..9], "interpolation2D")?;
    let along_second = values
        .iter()
        .take(n1)
        .map(|row| interpolate_axis(x2, &axis2[..n2], &row[..n2], mode))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Value::Real(interpolate_axis(
        x1,
        &axis1[..n1],
        &along_second,
        mode,
    )?))
}

fn interpolation_3d(arguments: Vec<Value>) -> Result<Value, EvaluationError> {
    let x1 = real_argument(&arguments[0], "interpolation3D")?;
    let x2 = real_argument(&arguments[1], "interpolation3D")?;
    let x3 = real_argument(&arguments[2], "interpolation3D")?;
    let axis1 = real_vector(&arguments[3], "interpolation3D")?;
    let n1 = interpolation_count(&arguments[4], axis1.len(), "interpolation3D")?;
    let axis2 = real_vector(&arguments[5], "interpolation3D")?;
    let n2 = interpolation_count(&arguments[6], axis2.len(), "interpolation3D")?;
    let axis3 = real_vector(&arguments[7], "interpolation3D")?;
    let n3 = interpolation_count(&arguments[8], axis3.len(), "interpolation3D")?;
    let values = real_array3(&arguments[9], "interpolation3D")?;
    let mode = interpolation_options(&arguments[10..12], "interpolation3D")?;
    if values.len() < n1
        || values
            .iter()
            .take(n1)
            .any(|plane| plane.len() < n2 || plane.iter().take(n2).any(|row| row.len() < n3))
    {
        return invalid_interpolation("interpolation3D");
    }
    let mut along_first = Vec::with_capacity(n1);
    for plane in values.iter().take(n1) {
        let along_second = plane
            .iter()
            .take(n2)
            .map(|row| interpolate_axis(x3, &axis3[..n3], &row[..n3], mode))
            .collect::<Result<Vec<_>, _>>()?;
        along_first.push(interpolate_axis(
            x2,
            &axis2[..n2],
            &along_second,
            mode,
        )?);
    }
    Ok(Value::Real(interpolate_axis(
        x1,
        &axis1[..n1],
        &along_first,
        mode,
    )?))
}

#[derive(Clone, Copy)]
struct InterpolationMode {
    linear: bool,
    linear_extrapolation: bool,
}

fn interpolation_options(
    values: &[Value],
    name: &'static str,
) -> Result<InterpolationMode, EvaluationError> {
    let interpolation = values[0]
        .integer()
        .ok_or(EvaluationError::Type("interpolation mode"))?;
    let extrapolation = values[1]
        .integer()
        .ok_or(EvaluationError::Type("extrapolation mode"))?;
    if !matches!(interpolation, 1 | 2) || !matches!(extrapolation, 1 | 2) {
        return invalid_interpolation(name);
    }
    Ok(InterpolationMode {
        linear: interpolation == 2,
        linear_extrapolation: extrapolation == 2,
    })
}

fn interpolate_axis(
    x: f64,
    axis: &[f64],
    values: &[f64],
    mode: InterpolationMode,
) -> Result<f64, EvaluationError> {
    if axis.len() < 2
        || values.len() != axis.len()
        || axis.windows(2).any(|pair| pair[0] >= pair[1])
    {
        return invalid_interpolation("interpolation");
    }
    let last = axis.len() - 1;
    if x <= axis[0] && !mode.linear_extrapolation {
        return Ok(values[0]);
    }
    if x >= axis[last] && !mode.linear_extrapolation {
        return Ok(values[last]);
    }
    let lower = if x <= axis[0] {
        0
    } else if x >= axis[last] {
        last - 1
    } else {
        axis.partition_point(|point| *point <= x) - 1
    };
    if !mode.linear && x < axis[last] {
        return Ok(values[lower]);
    }
    let weight = (x - axis[lower]) / (axis[lower + 1] - axis[lower]);
    Ok(values[lower] + weight * (values[lower + 1] - values[lower]))
}

fn interpolation_count(
    value: &Value,
    available: usize,
    name: &'static str,
) -> Result<usize, EvaluationError> {
    let count = value
        .integer()
        .and_then(|value| usize::try_from(value).ok())
        .ok_or(EvaluationError::Type("interpolation count"))?;
    if count < 2 || count > available {
        return invalid_interpolation(name);
    }
    Ok(count)
}

fn invalid_interpolation<T>(name: &'static str) -> Result<T, EvaluationError> {
    Err(EvaluationError::InvalidBuiltinArgument {
        name,
        detail: "inconsistent table, grid, count, or option",
    })
}

fn real_argument(value: &Value, name: &'static str) -> Result<f64, EvaluationError> {
    value
        .real()
        .ok_or(EvaluationError::InvalidBuiltinArgument {
            name,
            detail: "expected Real scalar",
        })
}

fn real_vector(value: &Value, name: &'static str) -> Result<Vec<f64>, EvaluationError> {
    let Value::Array(values) = value else {
        return invalid_interpolation(name);
    };
    values
        .iter()
        .map(|value| real_argument(value, name))
        .collect()
}

fn integer_vector(value: &Value, name: &'static str) -> Result<Vec<i64>, EvaluationError> {
    let Value::Array(values) = value else {
        return invalid_interpolation(name);
    };
    values
        .iter()
        .map(|value| {
            value
                .integer()
                .ok_or(EvaluationError::InvalidBuiltinArgument {
                    name,
                    detail: "expected Integer vector",
                })
        })
        .collect()
}

fn real_matrix(value: &Value, name: &'static str) -> Result<Vec<Vec<f64>>, EvaluationError> {
    let Value::Array(rows) = value else {
        return invalid_interpolation(name);
    };
    rows.iter().map(|row| real_vector(row, name)).collect()
}

fn real_array3(
    value: &Value,
    name: &'static str,
) -> Result<Vec<Vec<Vec<f64>>>, EvaluationError> {
    let Value::Array(planes) = value else {
        return invalid_interpolation(name);
    };
    planes
        .iter()
        .map(|plane| real_matrix(plane, name))
        .collect()
}

fn matrix_value(matrix: Vec<Vec<f64>>) -> Value {
    Value::Array(
        matrix
            .into_iter()
            .map(|row| Value::Array(row.into_iter().map(Value::Real).collect()))
            .collect(),
    )
}

fn has_nan(value: &Value) -> bool {
    match value {
        Value::Real(value) => value.is_nan(),
        Value::Array(values) => values.iter().any(has_nan),
        _ => false,
    }
}
