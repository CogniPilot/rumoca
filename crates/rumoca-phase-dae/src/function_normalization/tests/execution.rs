//! A bounded planning-graph evaluator for differential controls only. Source
//! expressions and the original-function oracle use Rumoca's existing evaluator;
//! this is not a compiler backend or evidence of DAE/Solve/WASM admission.

use std::collections::HashMap;

use rumoca_core::{GeneratedFunctionLocalCatalog, GeneratedFunctionLocalKey, Literal, Statement};
use rumoca_eval_flat::constant::{EvalContext, EvalLimits, Value, eval_expr, eval_function};

use super::{support::*, *};

struct Execution<'locals> {
    source: EvalContext,
    locals: HashMap<GeneratedFunctionLocalKey<'locals>, bool>,
}

impl<'locals> Execution<'locals> {
    fn new() -> Self {
        Self {
            source: EvalContext::new(),
            locals: HashMap::new(),
        }
    }

    fn boolean(&self, guard: &Guard<'locals>) -> Result<bool, String> {
        match guard {
            Guard::Source(expression) => eval_expr(expression, &self.source)
                .map_err(|error| error.to_string())?
                .as_bool()
                .ok_or_else(|| "not Boolean".into()),
            Guard::Local(local) => self
                .locals
                .get(&local.key())
                .copied()
                .ok_or_else(|| "undefined generated local".into()),
            Guard::Literal(value) => Ok(*value),
            Guard::Not(value) => Ok(!self.boolean(value)?),
            Guard::And(left, right) => {
                let left = self.boolean(left)?;
                let right = self.boolean(right)?;
                Ok(left && right)
            }
            Guard::If {
                condition,
                if_true,
                if_false,
            } => self.boolean(if self.boolean(condition)? {
                if_true
            } else {
                if_false
            }),
        }
    }

    fn sequence(&mut self, statements: &[NormalizedStatement<'locals>]) -> Result<(), String> {
        for statement in statements {
            self.statement(statement)?;
        }
        Ok(())
    }

    fn statement(&mut self, statement: &NormalizedStatement<'locals>) -> Result<(), String> {
        match statement {
            NormalizedStatement::Source(source) => self.source_statement(source.source()),
            NormalizedStatement::Definition(definition) => {
                let value = self.boolean(definition.rhs())?;
                self.locals.insert(definition.target(), value);
                Ok(())
            }
            NormalizedStatement::For {
                indices,
                statements,
                ..
            } => {
                let [index] = indices.as_slice() else {
                    return Err("fixture requires one binder".into());
                };
                let Value::Array(values) =
                    eval_expr(&index.range, &self.source).map_err(|error| error.to_string())?
                else {
                    return Err("fixture requires an Integer range".into());
                };
                assert!(values.len() <= 20, "bound test evaluation");
                let previous = self.source.parameters.get(&index.ident).cloned();
                for value in values {
                    self.source.add_parameter(&index.ident, value);
                    self.sequence(statements)?;
                }
                match previous {
                    Some(value) => self.source.add_parameter(&index.ident, value),
                    None => self.source.remove_parameter(&index.ident),
                }
                Ok(())
            }
            NormalizedStatement::If {
                branches, fallback, ..
            } => self.conditional(branches, fallback.as_deref()),
            NormalizedStatement::While { .. } | NormalizedStatement::When { .. } => {
                Err("outside evaluation fixture".into())
            }
        }
    }

    fn conditional(
        &mut self,
        branches: &[Branch<'locals>],
        fallback: Option<&[NormalizedStatement<'locals>]>,
    ) -> Result<(), String> {
        for branch in branches {
            if self.boolean(&branch.condition)? {
                return self.sequence(&branch.statements);
            }
        }
        if let Some(fallback) = fallback {
            self.sequence(fallback)?;
        }
        Ok(())
    }

    fn source_statement(&mut self, statement: &Statement) -> Result<(), String> {
        match statement {
            Statement::Assignment { comp, value, .. } => {
                assert!(comp.parts().iter().all(|part| part.subs.is_empty()));
                let value = eval_expr(value, &self.source).map_err(|error| error.to_string())?;
                self.source
                    .add_parameter(comp.to_var_name().as_str(), value);
                Ok(())
            }
            Statement::Assert { condition, .. } => {
                let accepted =
                    eval_expr(condition, &self.source).map_err(|error| error.to_string())?;
                if accepted.as_bool() == Some(true) {
                    Ok(())
                } else {
                    Err("selected assertion failed".into())
                }
            }
            Statement::Empty { .. } => Ok(()),
            _ => Err("outside evaluation fixture".into()),
        }
    }
}

fn original_result(function: &rumoca_core::Function) -> Result<Value, String> {
    eval_function(
        function,
        vec![],
        &EvalContext::new(),
        &EvalLimits::default(),
        0,
        function.span,
    )
    .map_err(|error| error.to_string())
}

#[test]
fn normalized_graph_matches_source_when_the_predicate_mutates_and_outer_iterations_refresh_it() {
    let (sources, span) = source();
    let function = mutable_gate_function(span);
    let oracle = original_result(&function).unwrap();
    assert_eq!(oracle, Value::Integer(4));
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let normalized = normalize(&function, catalog).unwrap();
        let mut execution = Execution::new();
        execution.sequence(&normalized).unwrap();
        assert_eq!(execution.source.parameters.get("result"), Some(&oracle));
        assert_eq!(
            execution.locals.len(),
            1,
            "one source site reused on three iterations"
        );
        assert_eq!(
            execution.locals.values().copied().collect::<Vec<_>>(),
            [true]
        );
    });
}

#[test]
fn later_elseif_predicates_remain_lazy_after_the_winning_body_changes_its_source_predicate() {
    let (sources, span) = source();
    let mut function = mutable_gate_function(span);
    function.body.truncate(2);
    function.body[1] = assignment("valid", VALID, boolean(true, span), span);
    let increment = assignment("result", RESULT, integer(2, span), span);
    function.body.push(Statement::If {
        cond_blocks: vec![
            rumoca_core::StatementBlock {
                cond: variable("valid", VALID, span),
                stmts: vec![
                    for_loop(
                        "j",
                        2,
                        vec![assignment("valid", VALID, boolean(false, span), span)],
                        span,
                    ),
                    increment,
                ],
            },
            rumoca_core::StatementBlock {
                cond: variable("missingAndInvalidRead", DATA, span),
                stmts: vec![for_loop(
                    "j",
                    1,
                    vec![assignment("result", RESULT, integer(99, span), span)],
                    span,
                )],
            },
        ],
        else_block: None,
        span,
    });
    let oracle = original_result(&function).unwrap();
    assert_eq!(oracle, Value::Integer(2));
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let mut execution = Execution::new();
        execution
            .sequence(&normalize(&function, catalog).unwrap())
            .unwrap();
        assert_eq!(execution.source.parameters.get("result"), Some(&oracle));
        assert_eq!(execution.locals.values().filter(|value| **value).count(), 1);
    });
    function.body[1] = assignment("valid", VALID, boolean(false, span), span);
    assert!(
        original_result(&function).is_err(),
        "the hazard is real when selected"
    );
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        assert!(
            Execution::new()
                .sequence(&normalize(&function, catalog).unwrap())
                .is_err()
        );
    });
}

#[test]
fn conditional_loops_retain_assertions_even_when_the_loop_has_no_data_result() {
    let (sources, span) = source();
    let mut function = mutable_gate_function(span);
    function.body.truncate(2);
    function.body.push(conditional(
        variable("valid", VALID, span),
        vec![for_loop(
            "j",
            2,
            vec![Statement::Assert {
                condition: boolean(false, span),
                message: Box::new(Expression::Literal {
                    value: Literal::String("selected".into()),
                    span,
                }),
                level: None,
                span,
            }],
            span,
        )],
        span,
    ));
    assert_eq!(original_result(&function).unwrap(), Value::Integer(0));
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        Execution::new()
            .sequence(&normalize(&function, catalog).unwrap())
            .unwrap();
    });
    function.body[1] = assignment("valid", VALID, boolean(true, span), span);
    assert!(original_result(&function).is_err());
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let error = Execution::new()
            .sequence(&normalize(&function, catalog).unwrap())
            .unwrap_err();
        assert_eq!(error, "selected assertion failed");
    });
}
