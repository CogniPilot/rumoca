//! User functions (#483): each checked function becomes one pure-call
//! owner of the Solve block. An owner returns its declared outputs followed by
//! one Boolean per predefined signal (in `SIGNALS` order), true when the body
//! raised it; the caller ORs those bits into its own status, so escape sets
//! stay per method. Functions see no block state, run no saturation and are
//! lowered callee first (GALEC has no recursion; a cycle is refused).

use std::num::NonZeroU64;
use std::rc::Rc;

use rumoca_ir_galec::ast::{Direction, FunctionKind, UserFunction};
use rumoca_ir_solve::{
    SolvePureCallIdentity, SolvePureCallOutput, SolvePureCallOwnerId, SolvePureCallTable,
    SolveSlot, SolveStatusBinding,
};

use super::*;

/// A function's call interface: Solve types of its inputs and outputs in
/// parameter order.
pub(super) struct Signature {
    inputs: Vec<SolveValueType>,
    outputs: Vec<SolveValueType>,
}

/// The block's functions as callers see them: signatures from the start,
/// owners and raised signals once each function is lowered.
#[derive(Default)]
pub(super) struct Functions {
    signatures: BTreeMap<String, Signature>,
    lowered: RefCell<BTreeMap<String, (SolvePureCallOwnerId, BTreeSet<usize>)>>,
    /// What a call can do (faults, effects), callee first.
    summaries: RefCell<BTreeMap<String, Summary>>,
}

impl Functions {
    pub(super) fn declare(
        block: &Block,
        arithmetic: SolveArithmeticProfile,
    ) -> Result<Self, AlgorithmBlockLowerError> {
        let mut signatures = BTreeMap::new();
        for function in user_functions(block) {
            let name = function.name.lexeme();
            if function.kind == FunctionKind::Stateful {
                return Err(unsupported("stateful method", function.name.span()));
            }
            if Builtin::named(name).is_some() {
                return Err(unsupported(
                    "function named like a built-in",
                    function.name.span(),
                ));
            }
            let mut signature = Signature {
                inputs: Vec::new(),
                outputs: Vec::new(),
            };
            for parameter in &function.parameters {
                let (storage, types) = match parameter.direction {
                    Direction::Input => (SolveStorageClass::Input, &mut signature.inputs),
                    Direction::Output => (SolveStorageClass::Output, &mut signature.outputs),
                };
                types.push(
                    variable(&parameter.decl, storage, Lifecycle::Any, arithmetic)?.value_type,
                );
            }
            signatures.insert(name.to_owned(), signature);
        }
        Ok(Self {
            signatures,
            lowered: RefCell::default(),
            summaries: RefCell::default(),
        })
    }

    pub(super) fn output_type(&self, call: &FunctionCall) -> Option<SolveValueType> {
        self.signatures
            .get(call.function.lexeme())?
            .outputs
            .first()
            .cloned()
    }

    pub(super) fn is_user(&self, call: &FunctionCall) -> bool {
        self.signatures.contains_key(call.function.lexeme())
    }

    /// What calling the function can do; unknown callees are assumed to do
    /// everything.
    pub(super) fn summary(&self, call: &FunctionCall) -> Summary {
        self.summaries
            .borrow()
            .get(call.function.lexeme())
            .copied()
            .unwrap_or_else(Summary::anything)
    }
}

fn user_functions(block: &Block) -> impl Iterator<Item = &UserFunction> {
    block
        .protected_functions
        .iter()
        .chain(&block.public_functions)
}

/// Check a call to a user function whose results bind `results` targets.
pub(super) fn check_user_call(
    scope: &MethodScope<'_>,
    call: &FunctionCall,
    results: usize,
    span: Span,
) -> Result<(), AlgorithmBlockLowerError> {
    let signature = scope
        .scope
        .functions
        .signatures
        .get(call.function.lexeme())
        .ok_or_else(|| unsupported("call of an unknown function", span))?;
    if call.arguments.len() != signature.inputs.len() {
        return Err(unsupported("function call arity", span));
    }
    if results != signature.outputs.len() {
        return Err(unsupported(
            "function call binding a partial result list",
            span,
        ));
    }
    for argument in &call.arguments {
        check_expression(scope, argument, span)?;
    }
    Ok(())
}

/// `(t1, ..., tn) := f(...)` and the bare call `f(...)`.
pub(super) fn check_call_statement(
    scope: &MethodScope<'_>,
    targets: &[Reference],
    call: &FunctionCall,
    span: Span,
) -> Result<(), AlgorithmBlockLowerError> {
    if !scope.scope.functions.is_user(call) {
        return Err(unsupported("built-in call statement", span));
    }
    for target in targets {
        let (id, subscripts) = scope.resolve_place(target)?;
        scope.check_store(id, span)?;
        check_subscripts(scope, id, subscripts, span)?;
    }
    // A bare call discards its outputs.
    let results = if targets.is_empty() {
        scope
            .scope
            .functions
            .signatures
            .get(call.function.lexeme())
            .map_or(0, |signature| signature.outputs.len())
    } else {
        targets.len()
    };
    check_user_call(scope, call, results, span)
}

/// Emit a call of an already lowered function and OR the signals it raised
/// into the caller's status; returns the declared outputs.
pub(super) fn lower_user_call<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    call: &FunctionCall,
    span: Span,
) -> Result<Vec<ProgramRegister<'p>>, SolveProgramConstructionError> {
    let (owner, raised) = scope
        .scope
        .functions
        .lowered
        .borrow()
        .get(call.function.lexeme())
        .cloned()
        .ok_or(SolveProgramConstructionError::UnknownCallOwner { provenance: span })?;
    let mut arguments = Vec::with_capacity(call.arguments.len());
    for argument in &call.arguments {
        arguments.push(lower_expression(builder, scope, env, argument, span)?);
    }
    let mut results = builder.call(owner, &arguments, span)?;
    let status = results.split_off(results.len().saturating_sub(SIGNALS.len()));
    if status.len() != SIGNALS.len() {
        return Err(SolveProgramConstructionError::InvalidCallInterface { provenance: span });
    }
    for signal in raised {
        raise(builder, scope, env, signal, status[signal], span)?;
    }
    Ok(results)
}

/// Lower every function into one owner of the block's call table, callee
/// first, and return the table with each owner's source.
pub(super) fn lower_functions(
    block: &Block,
    scope: &BlockScope,
) -> Result<
    (
        SolvePureCallTable,
        Vec<SolveAlgorithmCodeRef>,
        Vec<SolveStatusBinding>,
    ),
    AlgorithmBlockLowerError,
> {
    let order = callee_first(block, &scope.functions)?;
    let status_scope = function_block_scope(scope);
    let mut scopes = Vec::with_capacity(order.len());
    for function in &order {
        let function_scope = function_scope(&status_scope, function)?;
        check_statements(&function_scope, &function.statements)?;
        require_defined_function(&function_scope, function)?;
        let summary = function_summary(&function_scope, &function.statements);
        scope
            .functions
            .summaries
            .borrow_mut()
            .insert(function.name.lexeme().to_owned(), summary);
        scopes.push(function_scope);
    }
    let mut failure = None;
    let table = SolvePureCallTable::construct(scope.arithmetic, |table| {
        for (index, (function, function_scope)) in order.iter().zip(&scopes).enumerate() {
            let name = function.name.lexeme();
            let signature = &scope.functions.signatures[name];
            let outputs = signature
                .outputs
                .iter()
                .cloned()
                .chain(std::iter::repeat_n(
                    SolveValueType::scalar(SolveScalarType::Boolean),
                    SIGNALS.len(),
                ))
                .map(SolvePureCallOutput::result)
                .collect();
            let identity = SolvePureCallIdentity::issued(
                NonZeroU64::new(index as u64 + 1).unwrap_or(NonZeroU64::MIN),
            );
            let owner = table
                .add_owner(
                    identity,
                    signature.inputs.clone(),
                    outputs,
                    function_provenance(function),
                    |builder, inputs, outputs| {
                        lower_function(builder, inputs, outputs, function_scope, function)
                    },
                )
                .inspect_err(|error| failure = Some((name.to_owned(), error.clone())))?;
            let raised = function_scope.raised.borrow().clone();
            scope
                .functions
                .lowered
                .borrow_mut()
                .insert(name.to_owned(), (owner, raised));
        }
        Ok(())
    })
    .map_err(|error| {
        let (name, error) = failure.take().unwrap_or_else(|| (String::new(), error));
        AlgorithmBlockLowerError::Function { name, error }
    })?;
    for (function, function_scope) in order.iter().zip(&scopes) {
        require_declared_signals(function_scope, function)?;
    }
    let sources = order
        .iter()
        .map(|function| {
            SolveAlgorithmCodeRef::construct(
                SolveAlgorithmCodeEntityKind::Function,
                function.name.lexeme(),
                function_provenance(function),
            )
        })
        .collect();
    let bindings = status_bindings(&table);
    Ok((table, sources, bindings))
}

/// Each owner's status channels: its body declares its six status bits
/// first among its own locals, after the input and output slots (see
/// `lower_function`); the block re-validates the binding when it is built.
fn status_bindings(table: &SolvePureCallTable) -> Vec<SolveStatusBinding> {
    table
        .owners()
        .iter()
        .map(|owner| {
            let skip = owner.inputs().len() + owner.outputs().len();
            let slots = owner
                .body()
                .slots()
                .iter()
                .skip(skip)
                .take(SIGNALS.len())
                .map(SolveSlot::id)
                .collect();
            SolveStatusBinding::new(owner.id(), slots)
        })
        .collect()
}

/// The `signals` rule for functions: every signal the body may raise is in its clause, and
/// the clause names only predefined signals.
fn require_declared_signals(
    scope: &MethodScope<'_>,
    function: &UserFunction,
) -> Result<(), AlgorithmBlockLowerError> {
    let mut declared = BTreeSet::new();
    for signal in &function.signals {
        declared.insert(
            predefined_signal(signal.as_str())
                .ok_or_else(|| unsupported("user error signal", function.span))?,
        );
    }
    if scope.raised.borrow().is_subset(&declared) {
        Ok(())
    } else {
        Err(unsupported(
            "raised signal missing from the function's signals clause",
            function.span,
        ))
    }
}

/// The scope every function body shares: only the six status bits, which
/// lower to the owner's trailing outputs.
fn function_block_scope(scope: &BlockScope) -> BlockScope {
    let block = SIGNALS
        .iter()
        .map(|(_, name)| Variable {
            name: format!("ErrorSignalStatus.{name}"),
            value_type: SolveValueType::scalar(SolveScalarType::Boolean),
            dims: Vec::new(),
            storage: SolveStorageClass::Output,
            lifecycle: Lifecycle::Any,
            low: None,
            high: None,
            span: Span::DUMMY,
        })
        .collect();
    BlockScope {
        arithmetic: scope.arithmetic,
        block,
        by_name: BTreeMap::new(),
        status: [0, 1, 2, 3, 4, 5],
        functions: Rc::clone(&scope.functions),
    }
}

/// Parameters in declaration order, then the function's locals.
fn function_scope<'a>(
    scope: &'a BlockScope,
    function: &UserFunction,
) -> Result<MethodScope<'a>, AlgorithmBlockLowerError> {
    let mut locals = Vec::with_capacity(function.parameters.len() + function.locals.len());
    for parameter in &function.parameters {
        let storage = match parameter.direction {
            Direction::Input => SolveStorageClass::Input,
            Direction::Output => SolveStorageClass::Output,
        };
        locals.push(variable(
            &parameter.decl,
            storage,
            Lifecycle::Any,
            scope.arithmetic,
        )?);
    }
    for local in &function.locals {
        locals.push(variable(
            local,
            SolveStorageClass::MethodLocal,
            Lifecycle::Any,
            scope.arithmetic,
        )?);
    }
    let local_names = locals
        .iter()
        .enumerate()
        .map(|(index, local)| (local.name.clone(), index))
        .collect();
    Ok(MethodScope {
        scope,
        method: Method::Function,
        locals,
        local_names,
        loops: RefCell::new(Vec::new()),
        raised: RefCell::new(BTreeSet::new()),
    })
}

/// The body works on method-local slots (an owner stores each output
/// exactly once and never loads one); the outputs, then the status bits, are
/// stored at the end.
fn lower_function<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    inputs: &[ProgramSlot<'p>],
    outputs: &[ProgramSlot<'p>],
    scope: &MethodScope<'_>,
    function: &UserFunction,
) -> Result<(), SolveProgramConstructionError> {
    let span = function_provenance(function);
    let local = |builder: &mut TypedProgramBuilder<'p>, variable: &Variable, at: Span| {
        builder.declare_slot(
            variable.value_type.clone(),
            SolveStorageClass::MethodLocal,
            SolveSlotAccess::ReadWrite,
            at,
        )
    };
    let mut slots = Vec::with_capacity(scope.scope.block.len() + scope.locals.len());
    // The status bits are derived, so they carry the function's span.
    for status in &scope.scope.block {
        slots.push(local(builder, status, span)?);
    }
    let mut inputs = inputs.iter();
    let mut results = Vec::new();
    for (index, parameter) in function.parameters.iter().enumerate() {
        match parameter.direction {
            Direction::Input => {
                slots.push(*inputs.next().ok_or(
                    SolveProgramConstructionError::InvalidCallInterface { provenance: span },
                )?)
            }
            Direction::Output => {
                results.push(VarId::Local(index));
                slots.push(local(
                    builder,
                    &scope.locals[index],
                    scope.locals[index].span,
                )?);
            }
        }
    }
    for variable in scope.locals.iter().skip(function.parameters.len()) {
        slots.push(local(builder, variable, variable.span)?);
    }
    let mut env = Env {
        values: BTreeMap::new(),
        slots: Some(slots),
        definite: BTreeSet::new(),
    };
    let clear = builder.constant(SolveValue::boolean(false), span)?;
    for &status in &scope.scope.status {
        env.write(builder, scope, VarId::Block(status), clear, span)?;
    }
    let written = function
        .parameters
        .iter()
        .enumerate()
        .filter(|(_, parameter)| parameter.direction == Direction::Output)
        .map(|(index, _)| VarId::Local(index))
        .chain((function.parameters.len()..scope.locals.len()).map(VarId::Local))
        .collect::<Vec<_>>();
    zero_initialize(builder, scope, &mut env, written, span)?;
    lower_statements(builder, scope, &mut env, &function.statements)?;
    results.extend(
        scope
            .scope
            .status
            .iter()
            .map(|&status| VarId::Block(status)),
    );
    if results.len() != outputs.len() {
        return Err(SolveProgramConstructionError::InvalidCallInterface { provenance: span });
    }
    for (id, &output) in results.into_iter().zip(outputs) {
        let value = env.read(builder, scope, id, span)?;
        builder.store(output, value, span)?;
    }
    Ok(())
}

/// The function's own span, else the first exact span inside it: generated
/// functions may leave the declaration span empty.
fn function_provenance(function: &UserFunction) -> Span {
    std::iter::once(function.span)
        .chain(std::iter::once(function.name.span()))
        .chain(
            function
                .parameters
                .iter()
                .map(|parameter| parameter.decl.span),
        )
        .chain(function.statements.iter().map(|statement| statement.span))
        .find(|span| *span != Span::DUMMY)
        .unwrap_or(Span::DUMMY)
}

/// The functions in an order where every callee precedes its callers.
fn callee_first<'b>(
    block: &'b Block,
    functions: &Functions,
) -> Result<Vec<&'b UserFunction>, AlgorithmBlockLowerError> {
    let by_name = user_functions(block)
        .map(|function| (function.name.lexeme(), function))
        .collect::<BTreeMap<_, _>>();
    let mut order = Vec::with_capacity(by_name.len());
    let mut state = BTreeMap::new();
    for function in by_name.values() {
        visit(function, &by_name, functions, &mut state, &mut order)?;
    }
    Ok(order)
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Visit {
    Open,
    Done,
}

fn visit<'b>(
    function: &'b UserFunction,
    by_name: &BTreeMap<&str, &'b UserFunction>,
    functions: &Functions,
    state: &mut BTreeMap<&'b str, Visit>,
    order: &mut Vec<&'b UserFunction>,
) -> Result<(), AlgorithmBlockLowerError> {
    let name = function.name.lexeme();
    match state.get(name) {
        Some(Visit::Done) => return Ok(()),
        Some(Visit::Open) => return Err(unsupported("recursive function", function.span)),
        None => {}
    }
    state.insert(name, Visit::Open);
    let mut callees = BTreeSet::new();
    statement_calls(&function.statements, &mut |call| {
        if functions.is_user(call) {
            callees.insert(call.function.lexeme().to_owned());
        }
    });
    for callee in callees {
        if let Some(callee) = by_name.get(callee.as_str()) {
            visit(callee, by_name, functions, state, order)?;
        }
    }
    state.insert(name, Visit::Done);
    order.push(function);
    Ok(())
}

fn statement_calls(statements: &[Spanned<Statement>], found: &mut impl FnMut(&FunctionCall)) {
    for statement in statements {
        match &statement.node {
            Statement::Assignment { target, value } => {
                reference_calls(target, found);
                expression_calls(value, found);
            }
            Statement::MultiAssignment { targets, call } => {
                targets
                    .iter()
                    .for_each(|target| reference_calls(target, found));
                call_calls(call, found);
            }
            Statement::Call(call) => call_calls(call, found),
            Statement::If(if_statement) => {
                if_statement
                    .branches
                    .iter()
                    .for_each(|branch| branch_calls(branch, found));
                if let Some(body) = &if_statement.else_body {
                    statement_calls(body, found);
                }
            }
            Statement::For(for_loop) => {
                expression_calls(&for_loop.start, found);
                if let Some(step) = &for_loop.step {
                    expression_calls(step, found);
                }
                expression_calls(&for_loop.stop, found);
                statement_calls(&for_loop.body, found);
            }
            Statement::Limit(_) | Statement::Signal(_) => {}
        }
    }
}

fn branch_calls(branch: &IfBranch, found: &mut impl FnMut(&FunctionCall)) {
    if let Condition::Expression(condition) = &branch.condition {
        expression_calls(condition, found);
    }
    statement_calls(&branch.body, found);
}

fn call_calls(call: &FunctionCall, found: &mut impl FnMut(&FunctionCall)) {
    found(call);
    call.arguments
        .iter()
        .for_each(|argument| expression_calls(argument, found));
}

fn reference_calls(reference: &Reference, found: &mut impl FnMut(&FunctionCall)) {
    let parts: &[RefPart] = match reference {
        Reference::Local(part) => std::slice::from_ref(part),
        Reference::State(parts) => parts,
    };
    for part in parts {
        part.subscripts
            .iter()
            .for_each(|subscript| expression_calls(subscript, found));
    }
}

fn expression_calls(expression: &Expression, found: &mut impl FnMut(&FunctionCall)) {
    match expression {
        Expression::Call(call) => call_calls(call, found),
        Expression::Ref(reference) | Expression::Neg(reference) => {
            reference_calls(reference, found);
        }
        Expression::Size { dimension, .. } => expression_calls(dimension, found),
        Expression::Paren(inner) | Expression::Not(inner) => expression_calls(inner, found),
        Expression::Binary { lhs, rhs, .. } => {
            expression_calls(lhs, found);
            expression_calls(rhs, found);
        }
        Expression::If(if_expression) => {
            for (condition, value) in &if_expression.branches {
                expression_calls(condition, found);
                expression_calls(value, found);
            }
            expression_calls(&if_expression.else_value, found);
        }
        Expression::Array(items) => items.iter().for_each(|item| expression_calls(item, found)),
        Expression::Bool(_) | Expression::Integer(_) | Expression::Real(_) => {}
    }
}
