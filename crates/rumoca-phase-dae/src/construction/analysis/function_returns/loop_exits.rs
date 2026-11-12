//! Early loop exits: MLS §11.2.4 `break` ends its innermost enclosing loop,
//! and MLS §11.2.6 `return` inside a loop ends the enclosing function
//! algorithm.
//!
//! Each exit is one mutable generated Boolean, its *exit state*: the function
//! owns one return state, defined false on entry, and every loop whose body
//! breaks owns one break state, defined false before each execution of that
//! loop. An exit stores true in its state. Every statement that may run after
//! an exit is selected by the conjunction of the active states being false:
//! in a loop body that holds every statement, since from the second pass on
//! each one may follow an exit of an earlier pass. A pass that exits therefore
//! runs nothing after its exit, later passes change nothing, and every value
//! keeps the value it held at the exit.
//!
//! The selection is the same expression throughout one loop body, so it is the
//! common selection the fold's continuation is derived from
//! (`function_body::loop_exit`): every executor ends the fold at the pass
//! after the exit.
use super::*;

/// Whether `statements` leave a loop early: a `break`, or a `return` inside a
/// `for` or `while` loop.
pub(in crate::construction::analysis) fn contains_loop_exit(
    statements: &[rumoca_core::Statement],
) -> bool {
    contains_break(statements) || contains_loop_return(statements)
}

fn contains_loop_return(statements: &[rumoca_core::Statement]) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::For { equations, .. } => contains_return(equations),
        rumoca_core::Statement::While { block, .. } => contains_return(&block.stmts),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| contains_loop_return(&block.stmts))
                || else_block.as_deref().is_some_and(contains_loop_return)
        }
        _ => false,
    })
}

/// Whether `statements` contain a `break` at any depth.
pub(in crate::construction::analysis) fn contains_break(
    statements: &[rumoca_core::Statement],
) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::Break { .. } => true,
        rumoca_core::Statement::For { equations, .. } => contains_break(equations),
        rumoca_core::Statement::While { block, .. } => contains_break(&block.stmts),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks.iter().any(|block| contains_break(&block.stmts))
                || else_block.as_deref().is_some_and(contains_break)
        }
        _ => false,
    })
}

/// Whether `statements` can leave their innermost enclosing loop through
/// `break` (a `break` inside a nested loop leaves only that loop).
pub(in crate::construction::analysis) fn statements_break(
    statements: &[rumoca_core::Statement],
) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::Break { .. } => true,
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| statements_break(&block.stmts))
                || else_block.as_deref().is_some_and(statements_break)
        }
        _ => false,
    })
}

pub(super) fn normalize(
    statements: &[rumoca_core::Statement],
    span: Span,
) -> Result<NormalizedFunctionReturns, ToDaeError> {
    require_span(span, "function loop exit state")?;
    let mut exits = LoopExits { guards: Vec::new() };
    let returned = contains_return(statements).then(|| ExitState {
        target: rumoca_core::function_return_state_name(span.start.0),
        span,
    });
    let mut normalized = Vec::new();
    if let Some(state) = &returned {
        normalized.push(exits.store(state, false, span));
    }
    let active = Active {
        returned,
        broken: None,
    };
    let body = exits.sequence(statements, &active, false, &mut normalized)?;
    normalized.extend(body);
    Ok(NormalizedFunctionReturns {
        statements: normalized,
        guards: exits.guards,
        loop_selections: Vec::new(),
        has_returns: active.returned.is_some(),
    })
}

/// One exit state and the span every read of it carries, so the selection it
/// makes is one expression wherever it is read.
#[derive(Clone)]
struct ExitState {
    target: VarName,
    span: Span,
}

impl ExitState {
    /// `not state`: no exit has happened.
    fn active(&self) -> Expression {
        Expression::Unary {
            op: OpUnary::Not,
            rhs: Box::new(Expression::VarRef {
                name: Reference::generated(self.target.as_str()),
                subscripts: Vec::new(),
                span: self.span,
            }),
            span: self.span,
        }
    }
}

/// The exit states a statement sequence can set: the function's return state
/// where the sequence can return, and the break state of its innermost loop
/// where that loop breaks.
struct Active {
    returned: Option<ExitState>,
    broken: Option<ExitState>,
}

impl Active {
    /// The selection of a statement that may follow an exit.
    fn selection(&self) -> Option<Expression> {
        match (&self.returned, &self.broken) {
            (Some(returned), Some(broken)) => Some(and_condition(
                returned.active(),
                broken.active(),
                broken.span,
            )),
            (Some(state), None) | (None, Some(state)) => Some(state.active()),
            (None, None) => None,
        }
    }

    fn exits(&self, statement: &rumoca_core::Statement) -> bool {
        let statement = std::slice::from_ref(statement);
        (self.returned.is_some() && contains_return(statement))
            || (self.broken.is_some() && statements_break(statement))
    }
}

struct LoopExits {
    guards: Vec<GeneratedBooleanDefinition>,
}

impl LoopExits {
    /// The definition marker of one store of `value` into `state`.
    fn store(&mut self, state: &ExitState, value: bool, span: Span) -> rumoca_core::Statement {
        let id = rumoca_core::GeneratedBooleanId(self.guards.len());
        self.guards.push(GeneratedBooleanDefinition {
            mutable: true,
            target: state.target.clone(),
            value: Expression::Literal {
                value: Literal::Boolean(value),
                span,
            },
            span,
        });
        rumoca_core::Statement::Empty {
            span,
            generated_boolean: Some(id),
        }
    }

    /// `statements` with their exits stored and every statement after a
    /// possible exit selected. `resets` receives the definitions of the break
    /// states of the loops `statements` hold outside any nested loop; the
    /// caller places them where each loop's every execution follows them.
    fn sequence(
        &mut self,
        statements: &[rumoca_core::Statement],
        active: &Active,
        mut may_have_exited: bool,
        resets: &mut Vec<rumoca_core::Statement>,
    ) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
        let mut normalized = Vec::with_capacity(statements.len());
        for statement in statements {
            let span = required_statement_span(statement, "function loop exit continuation")?;
            let next = self.statement(statement, active, span, resets)?;
            normalized.push(match active.selection() {
                Some(selection) if may_have_exited => guarded_statement(&next, selection),
                _ => next,
            });
            may_have_exited |= active.exits(statement);
        }
        Ok(normalized)
    }

    fn statement(
        &mut self,
        statement: &rumoca_core::Statement,
        active: &Active,
        span: Span,
        resets: &mut Vec<rumoca_core::Statement>,
    ) -> Result<rumoca_core::Statement, ToDaeError> {
        Ok(match statement {
            rumoca_core::Statement::Return { .. } => {
                let Some(state) = &active.returned else {
                    unreachable!("a sequence holding a return has the return state")
                };
                self.store(state, true, span)
            }
            rumoca_core::Statement::Break { .. } => {
                let Some(state) = &active.broken else {
                    return Err(unsupported_statement(
                        statement,
                        "function break",
                        "function loop exit",
                        "a `break` must be inside a `for` or `while` loop (MLS §11.2.4)",
                    ));
                };
                self.store(state, true, span)
            }
            rumoca_core::Statement::For {
                indices, equations, ..
            } => rumoca_core::Statement::For {
                indices: indices.clone(),
                equations: self.loop_body(equations, active, span, resets)?,
                span,
            },
            // A branch runs only where its conditional runs, so it starts
            // with no exit since the conditional; its own exits select what
            // follows them in it.
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => rumoca_core::Statement::If {
                cond_blocks: cond_blocks
                    .iter()
                    .map(|block| {
                        Ok(rumoca_core::StatementBlock {
                            cond: block.cond.clone(),
                            stmts: self.sequence(&block.stmts, active, false, resets)?,
                        })
                    })
                    .collect::<Result<_, ToDaeError>>()?,
                else_block: else_block
                    .as_ref()
                    .map(|body| self.sequence(body, active, false, resets))
                    .transpose()?,
                span,
            },
            // Loop compaction lowers every `while` whose pass bound it proves
            // to a `for` first; one that is left has no bound.
            rumoca_core::Statement::While { block, .. }
                if statements_break(&block.stmts) || contains_return(&block.stmts) =>
            {
                return Err(unsupported_statement(
                    statement,
                    "unbounded function while loop",
                    "function loop domain",
                    format!(
                        "a `while` loop that leaves through `break` or `return` has no translation-time iteration bound: its condition {}",
                        super::super::loop_compaction::UNBOUNDED_LOOP_DOMAIN
                    ),
                ));
            }
            _ => statement.clone(),
        })
    }

    /// The body of a loop at `span` that runs under `enclosing`.
    ///
    /// Its break state is defined false in `resets`, which the enclosing
    /// sequence places before every execution of the loop. The break states
    /// of the loops the body holds are defined first in the body itself, so
    /// each pass enters them false and they are never carried across passes.
    fn loop_body(
        &mut self,
        equations: &[rumoca_core::Statement],
        enclosing: &Active,
        span: Span,
        resets: &mut Vec<rumoca_core::Statement>,
    ) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
        let broken = statements_break(equations).then(|| ExitState {
            target: rumoca_core::function_break_state_name(span.start.0, self.guards.len()),
            span,
        });
        if let Some(state) = &broken {
            resets.push(self.store(state, false, span));
        }
        let active = Active {
            returned: enclosing
                .returned
                .clone()
                .filter(|_| contains_return(equations)),
            broken,
        };
        let mut body = Vec::new();
        let may_have_exited = active.returned.is_some() || active.broken.is_some();
        let statements = self.sequence(equations, &active, may_have_exited, &mut body)?;
        body.extend(statements);
        Ok(body)
    }
}
