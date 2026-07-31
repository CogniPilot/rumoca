//! MLS §11.2.1 zero-output call statements whose only effect is checking.
//!
//! An initial algorithm may call a function purely to check its arguments —
//! `Modelica.Blocks.Sources.BooleanTable` writes `isValidTable(table)` — and
//! such a call writes nothing. Its declarative meaning is exactly the set of
//! assertions the body would raise, so it has the same owner a guarded
//! `assert` written directly in the section has, provided the body is proven
//! to have no other effect.
//!
//! ## Acceptance contract
//!
//! A statement call `f(a₁, …, aₙ);` in an initial algorithm replays as
//! assertion owners when every one of these holds:
//!
//! 1. the call binds no output, and `f` declares none, so nothing it computes
//!    is observable;
//! 2. `f` is a function the Flat function table collected, so its body is
//!    known rather than assumed;
//! 3. `f` is `pure` and has no `external` implementation, so calling it has no
//!    effect outside its own frame;
//! 4. every assignment in the body targets one of `f`'s own protected locals,
//!    never an input, so no argument is written back; and
//! 5. every statement in the body is one the initialization partition already
//!    owns: `assert`, an `if` chain over such statements, a `for` over such
//!    statements whose range is parameter-evaluable, an assignment to a local,
//!    or an empty statement.
//!
//! Under (1)–(5) the only way the call can be observed is by an assertion
//! failing, so replaying it as assertions loses nothing and invents nothing.
//! The replay binds each input to the call's argument, each local to its
//! declared default and then to each assignment in order, unrolls every `for`
//! over its evaluated range (MLS §11.2.2 defines a `for` with a
//! parameter-evaluable range as exactly that unrolled sequence), and hands the
//! resulting `assert`/`if` statements back to the section replay, which folds
//! every enclosing guard into `guard implies condition` as it already does for
//! a guarded check written in the section.
//!
//! Each replayed assertion keeps the span of the `assert` it came from, so a
//! failure names the check that failed rather than the call that raised it;
//! every rejection below names the call statement instead, because that is the
//! statement the model wrote and the one that has no owner.
//!
//! Anything outside (1)–(5) — a bound output, an external or impure callee, a
//! write to an input, a `while`/`when`/`reinit`, a nested call, a read of a
//! name that is not an input, local, or loop index, or a range that is not
//! parameter-evaluable — keeps a typed rejection naming the call statement and
//! what about it has no owner. A missing capability is reported, never
//! replaced by a guess.

use super::*;

/// A zero-output statement call whose callee the Flat function table collected.
pub(super) struct CheckingCall<'flat> {
    flat: &'flat flat::Model,
    function: &'flat rumoca_core::Function,
    arguments: &'flat [Expression],
    span: Span,
}

/// Recognize a zero-output statement call to a collected function.
///
/// A statement whose callee the function table does not register is not a
/// checking call: it is either the `assert` operator, which
/// [`super::assertion_call`] owns, or an absent callee, which the caller
/// rejects by name.
pub(super) fn checking_call<'flat>(
    flat: &'flat flat::Model,
    statement: &'flat rumoca_core::Statement,
) -> Option<CheckingCall<'flat>> {
    let rumoca_core::Statement::FunctionCall {
        comp,
        args,
        outputs,
        span,
    } = statement
    else {
        return None;
    };
    if !outputs.iter().all(Option::is_none) {
        return None;
    }
    let function = flat.functions.get(&comp.to_var_name())?;
    Some(CheckingCall {
        flat,
        function,
        arguments: args,
        span: *span,
    })
}

/// Prove the callee's body has no effect other than raising assertions.
///
/// This runs in the statement-grammar gate, before anything evaluates
/// expressions, so a call with an effect is reported as the missing owner
/// rather than as a consequence of replaying it.
pub(super) fn reject_unsupported_checking_call(call: &CheckingCall<'_>) -> Result<(), ToDaeError> {
    require_span(call.span, "initial algorithm checking call")?;
    let name = call.function.name.as_str();
    if !call.function.outputs.is_empty() {
        return Err(rejected(
            call,
            format!(
                "`{name}` declares {} output(s) that the call discards; a call statement the \
                 initialization partition owns computes nothing",
                call.function.outputs.len()
            ),
        ));
    }
    if call.function.external.is_some() {
        return Err(rejected(
            call,
            format!(
                "`{name}` is implemented externally, so its effects cannot be proven from its \
                 body"
            ),
        ));
    }
    if !call.function.pure {
        return Err(rejected(
            call,
            format!("`{name}` is declared `impure`, so calling it is itself an effect"),
        ));
    }
    if call.arguments.len() > call.function.inputs.len() {
        return Err(rejected(
            call,
            format!(
                "the call passes {} argument(s) to `{name}`, which declares {} input(s)",
                call.arguments.len(),
                call.function.inputs.len()
            ),
        ));
    }
    let locals = call
        .function
        .locals
        .iter()
        .map(|local| local.name.as_str())
        .collect::<HashSet<_>>();
    reject_unsupported_body(call, &call.function.body, &locals)
}

fn reject_unsupported_body(
    call: &CheckingCall<'_>,
    statements: &[rumoca_core::Statement],
    locals: &HashSet<&str>,
) -> Result<(), ToDaeError> {
    for statement in statements {
        // A body `assert(...)` reaches Flat as a call to the predefined
        // operator, exactly as it does in a model section.
        if assertion_call(call.flat, statement).is_some() {
            continue;
        }
        match statement {
            rumoca_core::Statement::Empty { .. } | rumoca_core::Statement::Assert { .. } => {}
            rumoca_core::Statement::Assignment { comp, .. } => {
                let target = comp.to_var_name();
                if comp.parts().len() != 1
                    || comp.parts().iter().any(|part| !part.subs.is_empty())
                    || !locals.contains(target.as_str())
                {
                    return Err(rejected(
                        call,
                        format!(
                            "`{}` assigns `{target}`, which is not one of its own whole protected \
                             locals; a checking call writes nothing an owner can observe",
                            call.function.name
                        ),
                    ));
                }
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    reject_unsupported_body(call, &block.stmts, locals)?;
                }
                if let Some(statements) = else_block {
                    reject_unsupported_body(call, statements, locals)?;
                }
            }
            rumoca_core::Statement::For { equations, .. } => {
                reject_unsupported_body(call, equations, locals)?;
            }
            _ => {
                return Err(rejected(
                    call,
                    format!(
                        "`{}` contains a {} statement; a checking call's body is accepted as \
                         `assert`, `if`, `for`, and assignments to its own locals",
                        call.function.name,
                        super::statement_kind(statement)
                    ),
                ));
            }
        }
    }
    Ok(())
}

/// Replay the call into the `assert` and `if` statements its body raises.
///
/// The result is expressed in the section's own statement grammar, so the
/// caller folds guards and validates conditions through the owners it already
/// has.
pub(super) fn expand_checking_call(
    call: &CheckingCall<'_>,
    constants: &EvalContext,
) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
    let mut frame = Frame {
        call,
        constants,
        values: HashMap::new(),
    };
    for (ordinal, input) in call.function.inputs.iter().enumerate() {
        let bound = match call.arguments.get(ordinal) {
            Some(argument) => argument.clone(),
            None => {
                let default = input.default.as_ref().ok_or_else(|| {
                    rejected(
                        call,
                        format!(
                            "input `{}` of `{}` is neither passed nor defaulted",
                            input.name, call.function.name
                        ),
                    )
                })?;
                frame.substitute(default)?
            }
        };
        frame.values.insert(VarName::new(&input.name), bound);
    }
    for local in &call.function.locals {
        let Some(default) = &local.default else {
            continue;
        };
        let bound = frame.substitute(default)?;
        frame.values.insert(VarName::new(&local.name), bound);
    }
    let body = call.function.body.clone();
    frame.statements(&body)
}

/// One call frame: every name the body may read, bound to its exact value.
struct Frame<'call> {
    call: &'call CheckingCall<'call>,
    constants: &'call EvalContext,
    values: HashMap<VarName, Expression>,
}

impl Frame<'_> {
    fn statements(
        &mut self,
        statements: &[rumoca_core::Statement],
    ) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
        let mut replayed = Vec::new();
        for statement in statements {
            self.statement(statement, &mut replayed)?;
        }
        Ok(replayed)
    }

    fn statement(
        &mut self,
        statement: &rumoca_core::Statement,
        replayed: &mut Vec<rumoca_core::Statement>,
    ) -> Result<(), ToDaeError> {
        if let Some(assertion) = assertion_call(self.call.flat, statement) {
            let replayed_assert = rumoca_core::Statement::Assert {
                condition: self.substitute(assertion.condition)?,
                message: Box::new(self.substitute(assertion.message)?),
                level: assertion
                    .level
                    .map(|level| self.substitute(level))
                    .transpose()?
                    .map(Box::new),
                span: assertion.span,
            };
            replayed.push(replayed_assert);
            return Ok(());
        }
        match statement {
            rumoca_core::Statement::Empty { .. } => Ok(()),
            rumoca_core::Statement::Assignment { comp, value, .. } => {
                let value = self.substitute(value)?;
                self.values.insert(comp.to_var_name(), value);
                Ok(())
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            } => self.conditional(cond_blocks, else_block.as_deref(), *span, replayed),
            rumoca_core::Statement::For {
                indices, equations, ..
            } => self.unrolled(indices, equations, replayed),
            _ => Err(rejected(
                self.call,
                format!(
                    "`{}` contains a {} statement; a checking call's body is accepted as \
                     `assert`, `if`, `for`, and assignments to its own locals",
                    self.call.function.name,
                    super::statement_kind(statement)
                ),
            )),
        }
    }

    /// Replay an `if` chain, keeping each branch's writes inside that branch.
    ///
    /// A local a branch assigns is observable only while that branch runs, so
    /// it is unbound again on the way out: a later read of it would have a
    /// value that depends on the branch taken, and the frame keeps only values
    /// it can state exactly.
    fn conditional(
        &mut self,
        blocks: &[rumoca_core::StatementBlock],
        fallback: Option<&[rumoca_core::Statement]>,
        span: Span,
        replayed: &mut Vec<rumoca_core::Statement>,
    ) -> Result<(), ToDaeError> {
        let entry = self.values.clone();
        let mut written = HashSet::new();
        let mut cond_blocks = Vec::with_capacity(blocks.len());
        for block in blocks {
            // Every branch condition of the chain is read in the state the
            // chain was entered in, not in the state the previous branch left.
            self.values = entry.clone();
            let cond = self.substitute(&block.cond)?;
            let stmts = self.statements(&block.stmts)?;
            written.extend(self.branch_writes(&entry));
            cond_blocks.push(rumoca_core::StatementBlock { cond, stmts });
        }
        let else_block = match fallback {
            Some(statements) => {
                self.values = entry.clone();
                let replayed = self.statements(statements)?;
                written.extend(self.branch_writes(&entry));
                Some(replayed)
            }
            None => None,
        };
        self.values = entry;
        for name in written {
            self.values.remove(&name);
        }
        if cond_blocks.iter().all(|block| block.stmts.is_empty())
            && else_block.as_ref().is_none_or(|stmts| stmts.is_empty())
        {
            return Ok(());
        }
        replayed.push(rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            span,
        });
        Ok(())
    }

    fn branch_writes(&self, entry: &HashMap<VarName, Expression>) -> Vec<VarName> {
        self.values
            .iter()
            .filter(|(name, value)| entry.get(*name) != Some(*value))
            .map(|(name, _)| name.clone())
            .collect()
    }

    /// Unroll a `for` over its evaluated range (MLS §11.2.2).
    ///
    /// The range is evaluated in the model's parameter context after the
    /// frame's bindings are substituted, so `for i in 2:size(table, 1)` inside
    /// a call passing a parameter array unrolls to exactly the iterations that
    /// array has.
    fn unrolled(
        &mut self,
        indices: &[rumoca_core::ForIndex],
        body: &[rumoca_core::Statement],
        replayed: &mut Vec<rumoca_core::Statement>,
    ) -> Result<(), ToDaeError> {
        let Some((index, rest)) = indices.split_first() else {
            replayed.extend(self.statements(body)?);
            return Ok(());
        };
        let name = VarName::new(&index.ident);
        let shadowed = self.values.remove(&name);
        let (start, step, end) = self.evaluated_range(&index.range)?;
        let range_span = expression_span(&index.range)?;
        let mut current = start;
        while (step > 0 && current <= end) || (step < 0 && current >= end) {
            self.values.insert(
                name.clone(),
                Expression::Literal {
                    value: rumoca_core::Literal::Integer(current),
                    span: range_span,
                },
            );
            self.unrolled(rest, body, replayed)?;
            let Some(next) = current.checked_add(step) else {
                break;
            };
            current = next;
        }
        self.values.remove(&name);
        if let Some(shadowed) = shadowed {
            self.values.insert(name, shadowed);
        }
        Ok(())
    }

    fn evaluated_range(&mut self, range: &Expression) -> Result<(i64, i64, i64), ToDaeError> {
        let Expression::Range {
            start, step, end, ..
        } = range
        else {
            return Err(rejected(
                self.call,
                format!(
                    "`{}` iterates over an expression that is not a range; only a range with \
                     parameter-evaluable bounds unrolls exactly",
                    self.call.function.name
                ),
            ));
        };
        let start = self.evaluated_integer(start)?;
        let step = step
            .as_deref()
            .map(|step| self.evaluated_integer(step))
            .transpose()?
            .unwrap_or(1);
        let end = self.evaluated_integer(end)?;
        if step == 0 {
            return Err(rejected(
                self.call,
                format!("`{}` iterates with a zero step", self.call.function.name),
            ));
        }
        Ok((start, step, end))
    }

    fn evaluated_integer(&mut self, bound: &Expression) -> Result<i64, ToDaeError> {
        let bound = self.substitute(bound)?;
        eval_expr(&bound, self.constants)
            .ok()
            .and_then(|value| value.as_integer())
            .ok_or_else(|| {
                rejected(
                    self.call,
                    format!(
                        "a loop bound of `{}` is not a parameter-evaluable Integer, so the \
                         iterations it raises assertions for are not known",
                        self.call.function.name
                    ),
                )
            })
    }

    fn substitute(&mut self, expression: &Expression) -> Result<Expression, ToDaeError> {
        let mut binding = FrameSubstitution {
            call: self.call,
            values: &self.values,
            error: None,
        };
        let rewritten = binding.rewrite_expression(expression);
        match binding.error {
            Some(error) => Err(error),
            None => Ok(rewritten),
        }
    }
}

/// Replace every name the body reads with the value the frame bound it to.
struct FrameSubstitution<'frame> {
    call: &'frame CheckingCall<'frame>,
    values: &'frame HashMap<VarName, Expression>,
    error: Option<ToDaeError>,
}

impl ExpressionRewriter for FrameSubstitution<'_> {
    /// A checking call's body reaches the DAE only as the assertions it
    /// raises, so no specialization of it is selected and no callee inside it
    /// is discovered. A call in one of its expressions would therefore reach
    /// lowering without a resolved callee, so it is named here instead.
    fn walk_function_call_expression(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[Expression],
        is_constructor: bool,
        span: Span,
    ) -> Expression {
        self.error.get_or_insert_with(|| {
            rejected(
                self.call,
                format!(
                    "`{}` calls `{}` while checking; a checking call's expressions are accepted \
                     as operators and predefined functions alone",
                    self.call.function.name,
                    name.var_name()
                ),
            )
        });
        Expression::FunctionCall {
            name: name.clone(),
            args: self.rewrite_expressions(args),
            is_constructor,
            span,
        }
    }

    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Expression {
        let subscripts = self.rewrite_subscripts(subscripts);
        let Some(bound) = self.values.get(name.var_name()) else {
            self.error.get_or_insert_with(|| {
                rejected(
                    self.call,
                    format!(
                        "`{}` reads `{}`, which is not one of its inputs, its locals with a \
                         value on this path, or a loop index",
                        self.call.function.name,
                        name.var_name()
                    ),
                )
            });
            return Expression::VarRef {
                name: name.clone(),
                subscripts,
                span,
            };
        };
        if subscripts.is_empty() {
            return bound.clone();
        }
        // A subscripted read of a bound name restates the binding's own
        // coordinate under those subscripts. That is exact only when the
        // binding is one whole coordinate; any other value has no coordinate
        // to subscript.
        let Expression::VarRef {
            name: bound_name,
            subscripts: bound_subscripts,
            span: bound_span,
        } = bound
        else {
            self.error.get_or_insert_with(|| {
                rejected(
                    self.call,
                    format!(
                        "`{}` reads `{}` under subscripts, but it is bound to an expression \
                         rather than to one whole coordinate",
                        self.call.function.name,
                        name.var_name()
                    ),
                )
            });
            return Expression::VarRef {
                name: name.clone(),
                subscripts,
                span,
            };
        };
        if !bound_subscripts.is_empty() {
            self.error.get_or_insert_with(|| {
                rejected(
                    self.call,
                    format!(
                        "`{}` reads `{}` under subscripts, but it is bound to an already \
                         subscripted coordinate",
                        self.call.function.name,
                        name.var_name()
                    ),
                )
            });
        }
        Expression::VarRef {
            name: bound_name.clone(),
            subscripts,
            span: *bound_span,
        }
    }
}

fn rejected(call: &CheckingCall<'_>, detail: impl std::fmt::Display) -> ToDaeError {
    ToDaeError::unsupported_algorithm(
        "initial",
        format!(
            "the call statement to `{}` has no checked initialization owner: {detail}",
            call.function.name
        ),
        call.span,
    )
}
