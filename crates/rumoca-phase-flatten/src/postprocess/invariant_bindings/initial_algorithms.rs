//! An initial algorithm that only String comparisons decide runs at
//! translation (MLS 3.7 §8.6, §11.1; SPEC_0022 ALG-015).
//!
//! `Modelica.Fluid.Sensors.TraceSubstances` finds the index of its substance
//! in an initial algorithm: `ind := -1; for i in 1:nC loop if
//! Strings.isEqual(names[i], substanceName) then ind := i; ...`. The runtime
//! owns no String storage, so such an algorithm can only run here. It runs
//! when its one target is a scalar `fixed = false` parameter that nothing else
//! initializes, every other name it reads is fixed at translation (a constant,
//! or a String, `evaluate` or `final` parameter with a literal binding), and it
//! reads a String's value. The target then takes the result as its binding,
//! fixed and not tunable, and the algorithm is removed. Anything the
//! interpreter refuses leaves the algorithm as it was.
//!
//! The algorithm's top-level assertions (the sensor's `assert(ind > 0, ...)`),
//! when all of them follow the last statement assigning the target, are
//! decided with the target's value (MLS §8.3.7): a true one goes; a false
//! one at error level is the EF030 translation error at its span, with its
//! message; a false one at warning level stays as a warning assertion with
//! its condition and message evaluated, which the runtime reports.

use std::borrow::Cow;

use rumoca_core::{
    Expression, ExpressionRewriter as _, ExpressionVisitor, Function, FunctionParam, Statement,
    VarName, Variability,
};
use rumoca_eval_flat::constant::{
    DeferredParameterSource, EvalEnvironment, EvalLimits, Value, eval_expr, eval_function,
};
use rumoca_eval_flat::translation_reads::ResourceRoots;
use rumoca_ir_flat as flat;

use super::{FunctionInventory, value_expression};
use crate::FlattenError;

/// One initial algorithm decided at translation: its index, its target, the
/// target's value, and the warning assertions that stay.
pub(super) struct Decided {
    index: usize,
    target: VarName,
    value: Value,
    warnings: Vec<Statement>,
}

/// The initial algorithms the module documentation describes, run.
pub(super) fn decide_string_initial_algorithms(
    flat: &flat::Model,
    inventory: &FunctionInventory<'_>,
) -> Result<Vec<Decided>, FlattenError> {
    let mut decided = Vec::new();
    for (index, algorithm) in flat.initial_algorithms.iter().enumerate() {
        if let Some(found) = decide(flat, inventory, index, algorithm)? {
            decided.push(found);
        }
    }
    Ok(decided)
}

/// Give each decided target its value, fixed and not tunable, and remove the
/// algorithms that computed them.
pub(super) fn apply(flat: &mut flat::Model, decided: Vec<Decided>) {
    for Decided {
        index,
        target,
        value,
        warnings,
    } in decided.into_iter().rev()
    {
        let algorithm = if warnings.is_empty() {
            flat.initial_algorithms.remove(index)
        } else {
            let algorithm = &mut flat.initial_algorithms[index];
            algorithm.statements = warnings;
            algorithm.clone()
        };
        let Some(variable) = flat.variables.get_mut(&target) else {
            continue;
        };
        variable.binding = value_expression(&value, algorithm.span);
        variable.fixed = Some(vec![true]);
        variable.evaluate = true;
    }
}

/// The algorithm decided, when it qualifies and runs; the EF030 error when
/// one of its top-level assertions fails at error level.
fn decide(
    flat: &flat::Model,
    inventory: &FunctionInventory<'_>,
    index: usize,
    algorithm: &flat::Algorithm,
) -> Result<Option<Decided>, FlattenError> {
    let Some((target, value, asserts)) = run(flat, inventory, index, algorithm) else {
        return Ok(None);
    };
    let with_target = FixedReads {
        flat,
        inventory,
        target: &target,
        decided: Some(&value),
    };
    let mut warnings = Vec::new();
    for statement in asserts {
        let Statement::Assert {
            condition,
            message,
            level,
            span,
        } = statement
        else {
            continue;
        };
        let Ok(Value::Bool(holds)) = eval_expr(condition, &with_target) else {
            return Ok(None);
        };
        if holds {
            continue;
        }
        let Ok(Value::String(text)) = eval_expr(message, &with_target) else {
            return Ok(None);
        };
        match flat::AssertionLevel::of_settled(level.as_deref()) {
            Some(flat::AssertionLevel::Error) => {
                return Err(FlattenError::structural_assertion_failed(text, *span));
            }
            Some(flat::AssertionLevel::Warning) => warnings.push(Statement::Assert {
                condition: Expression::Literal {
                    value: rumoca_core::Literal::Boolean(false),
                    span: condition.span().unwrap_or(*span),
                },
                message: Box::new(Expression::Literal {
                    value: rumoca_core::Literal::String(text),
                    span: message.span().unwrap_or(*span),
                }),
                level: level.clone(),
                span: *span,
            }),
            None => return Ok(None),
        }
    }
    Ok(Some(Decided {
        index,
        target,
        value,
        warnings,
    }))
}

/// The target, its value, and the algorithm's top-level assertions, when the
/// algorithm qualifies and its body runs.
fn run<'a>(
    flat: &flat::Model,
    inventory: &FunctionInventory<'_>,
    index: usize,
    algorithm: &'a flat::Algorithm,
) -> Option<(VarName, Value, Vec<&'a Statement>)> {
    // The top-level assertions are decided after the body, with the target's
    // final value, so each must follow the last statement that assigns the
    // target: one written before it would see a value the target no longer
    // has (MLS 3.7 §11.1, statements execute in order). RoomCO2's sensor
    // asserts after its loop.
    let assigns_target = |statement: &Statement| {
        let mut targets = Vec::new();
        collect_targets(std::slice::from_ref(statement), &mut targets)
            .is_none_or(|()| !targets.is_empty())
    };
    let last_assignment = algorithm.statements.iter().rposition(assigns_target);
    let first_assert = algorithm
        .statements
        .iter()
        .position(|statement| matches!(statement, Statement::Assert { .. }));
    if let (Some(first_assert), Some(last_assignment)) = (first_assert, last_assignment)
        && first_assert < last_assignment
    {
        return None;
    }
    let (asserts, body): (Vec<&Statement>, Vec<&Statement>) = algorithm
        .statements
        .iter()
        .partition(|statement| matches!(statement, Statement::Assert { .. }));
    let body = body.into_iter().cloned().collect::<Vec<_>>();
    let mut targets = Vec::new();
    collect_targets(&body, &mut targets)?;
    targets.dedup();
    let [target] = targets.as_slice() else {
        return None;
    };
    let variable = flat.variables.get(target)?;
    let scalar_parameter = matches!(variable.variability, Variability::Parameter(_))
        && variable.dims.is_empty()
        && variable.fixed_uniform() == Some(false)
        && variable.binding.is_none()
        && !is_string(flat, variable);
    if !scalar_parameter || initialized_elsewhere(flat, index, target) {
        return None;
    }
    let environment = FixedReads {
        flat,
        inventory,
        target,
        decided: None,
    };
    let mut reads = Reads {
        environment: &environment,
        string_value: false,
        unfixed: false,
    };
    reads.statements(&algorithm.statements);
    if reads.unfixed || !reads.string_value {
        return None;
    }
    let effective_type = flat.effective_types.get(&variable.type_id)?.clone();
    // The body runs as a function whose output is the target under a plain
    // local name: a component path (`sensor.ind`) would read as a record
    // field of `sensor`.
    let mut function = Function::new("initial algorithm", algorithm.span);
    function.outputs.push(FunctionParam::new(
        LOCAL_TARGET,
        "",
        effective_type,
        algorithm.span,
    ));
    let mut rename = RenameTarget { target };
    function.body = rename.statements(&body)?;
    let value = eval_function(
        &function,
        Vec::new(),
        &environment,
        &EvalLimits::default(),
        0,
        algorithm.span,
    )
    .ok()?;
    Some((target.clone(), value, asserts))
}

/// The local name the target takes in the function the algorithm runs as.
/// `$` is not a Modelica identifier character, so no name the body reads can
/// be spelled this way.
const LOCAL_TARGET: &str = "$target";

/// Rewrites the target's assignments and reads to [`LOCAL_TARGET`].
struct RenameTarget<'t> {
    target: &'t VarName,
}

impl RenameTarget<'_> {
    fn statements(&mut self, statements: &[Statement]) -> Option<Vec<Statement>> {
        statements
            .iter()
            .map(|statement| self.statement(statement))
            .collect()
    }

    fn statement(&mut self, statement: &Statement) -> Option<Statement> {
        Some(match statement {
            Statement::Assignment { comp, value, span } => {
                let comp = if comp.to_var_name() == *self.target {
                    let part = comp.parts().last()?;
                    rumoca_core::ComponentReference::construct(
                        false,
                        comp.span(),
                        vec![rumoca_core::ComponentRefPart {
                            ident: LOCAL_TARGET.to_string(),
                            span: part.span,
                            subs: Vec::new(),
                            def_id: part.def_id,
                        }],
                    )
                    .ok()?
                } else {
                    comp.clone()
                };
                Statement::Assignment {
                    comp,
                    value: self.rewrite_expression(value),
                    span: *span,
                }
            }
            Statement::For {
                indices,
                equations,
                span,
            } => Statement::For {
                indices: indices
                    .iter()
                    .map(|index| rumoca_core::ForIndex {
                        range: self.rewrite_expression(&index.range),
                        ..index.clone()
                    })
                    .collect(),
                equations: self.statements(equations)?,
                span: *span,
            },
            Statement::While { block, span } => Statement::While {
                block: self.block(block)?,
                span: *span,
            },
            Statement::If {
                cond_blocks,
                else_block,
                span,
            } => Statement::If {
                cond_blocks: cond_blocks
                    .iter()
                    .map(|block| self.block(block))
                    .collect::<Option<_>>()?,
                else_block: match else_block {
                    Some(block) => Some(self.statements(block)?),
                    None => None,
                },
                span: *span,
            },
            Statement::Assert {
                condition,
                message,
                level,
                span,
            } => Statement::Assert {
                condition: self.rewrite_expression(condition),
                message: Box::new(self.rewrite_expression(message)),
                level: level.clone(),
                span: *span,
            },
            other => other.clone(),
        })
    }

    fn block(
        &mut self,
        block: &rumoca_core::StatementBlock,
    ) -> Option<rumoca_core::StatementBlock> {
        Some(rumoca_core::StatementBlock {
            cond: self.rewrite_expression(&block.cond),
            stmts: self.statements(&block.stmts)?,
        })
    }
}

impl rumoca_core::ExpressionRewriter for RenameTarget<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> Expression {
        if name.as_str() == self.target.as_str() {
            return Expression::VarRef {
                name: rumoca_core::Reference::new(LOCAL_TARGET),
                subscripts: subscripts.to_vec(),
                span,
            };
        }
        self.walk_var_ref_expression(name, subscripts, span)
    }
}

/// The variables the statements assign, or `None` when one is not a whole
/// variable (a subscripted or record-field target).
fn collect_targets(statements: &[Statement], targets: &mut Vec<VarName>) -> Option<()> {
    for statement in statements {
        match statement {
            Statement::Assignment { comp, .. } => {
                if comp.parts().iter().any(|part| !part.subs.is_empty()) {
                    return None;
                }
                targets.push(comp.to_var_name());
            }
            Statement::For { equations, .. } => collect_targets(equations, targets)?,
            Statement::While { block, .. } => collect_targets(&block.stmts, targets)?,
            Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    collect_targets(&block.stmts, targets)?;
                }
                if let Some(block) = else_block {
                    collect_targets(block, targets)?;
                }
            }
            Statement::Empty { .. } | Statement::Assert { .. } => {}
            // A call statement, a `when`, a `reinit`, `return` or `break` is
            // not the shape this pass decides.
            _ => return None,
        }
    }
    Some(())
}

/// Whether another initial algorithm or any initial equation reads or
/// assigns `target`.
fn initialized_elsewhere(flat: &flat::Model, index: usize, target: &VarName) -> bool {
    let mut mentions = Mentions {
        target,
        found: false,
    };
    for equation in &flat.initial_equations {
        mentions.visit_expression(&equation.residual);
    }
    for (other, algorithm) in flat.initial_algorithms.iter().enumerate() {
        if other != index {
            let mut targets = Vec::new();
            if collect_targets(&algorithm.statements, &mut targets).is_none()
                || targets.contains(target)
            {
                return true;
            }
        }
    }
    mentions.found
}

struct Mentions<'t> {
    target: &'t VarName,
    found: bool,
}

impl ExpressionVisitor for Mentions<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::VarRef { name, .. } if name.as_str() == self.target.as_str() => {
                self.found = true;
            }
            _ => self.walk_expression(expr),
        }
    }
}

fn is_string(flat: &flat::Model, variable: &flat::Variable) -> bool {
    flat.effective_types
        .get(&variable.type_id)
        .is_some_and(|effective| effective.canonical_type() == flat.predefined_types.string)
}

/// The model's names the algorithm may read: those fixed at translation, as
/// their literal values. The target is the algorithm's own output; once the
/// algorithm has run, `decided` is its value, which the algorithm's
/// top-level assertions read.
struct FixedReads<'a> {
    flat: &'a flat::Model,
    inventory: &'a FunctionInventory<'a>,
    target: &'a VarName,
    decided: Option<&'a Value>,
}

impl FixedReads<'_> {
    fn fixed(&self, name: &str) -> Option<&flat::Variable> {
        let variable = self.flat.variables.get(&VarName::new(name))?;
        let fixed_here = match variable.variability {
            Variability::Constant(_) => true,
            Variability::Parameter(_) => {
                variable.evaluate
                    || is_string(self.flat, variable)
                    || self
                        .flat
                        .variable_final_flags
                        .get(&variable.name)
                        .copied()
                        .unwrap_or(false)
            }
            _ => false,
        };
        fixed_here.then_some(variable)
    }
}

impl EvalEnvironment for FixedReads<'_> {
    fn get_value(&self, name: &str) -> Option<Cow<'_, Value>> {
        if let Some(value) = self.decided
            && name == self.target.as_str()
        {
            return Some(Cow::Borrowed(value));
        }
        let binding = self.fixed(name)?.binding.as_ref()?;
        eval_expr(binding, self.inventory).ok().map(Cow::Owned)
    }

    fn get_enum(&self, name: &str) -> Option<&(String, String)> {
        self.inventory.get_enum(name)
    }

    fn enumeration_type(&self, declaration: rumoca_core::DefId) -> Option<&str> {
        self.inventory.enumeration_type(declaration)
    }

    fn get_function(&self, name: &str) -> Option<&Function> {
        self.inventory.get_function(name)
    }

    fn get_array_dimensions(&self, name: &str) -> Option<&[i64]> {
        Some(self.fixed(name)?.dims.as_slice())
    }

    fn deferred_parameter(&self, _name: &str) -> Option<DeferredParameterSource> {
        None
    }

    fn translation_resources(&self) -> Option<&ResourceRoots> {
        self.inventory.translation_resources()
    }
}

/// What the algorithm reads: whether it reads a String's value, and whether
/// it reads a model name that is not fixed at translation.
struct Reads<'e, 'a> {
    environment: &'e FixedReads<'a>,
    string_value: bool,
    unfixed: bool,
}

impl Reads<'_, '_> {
    fn statements(&mut self, statements: &[Statement]) {
        for statement in statements {
            self.statement(statement);
        }
    }

    fn statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Assignment { value, .. } => self.visit_expression(value),
            Statement::For {
                indices, equations, ..
            } => {
                indices
                    .iter()
                    .for_each(|index| self.visit_expression(&index.range));
                self.statements(equations);
            }
            Statement::While { block, .. } => self.block(block),
            Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                cond_blocks.iter().for_each(|block| self.block(block));
                self.statements(else_block.as_deref().unwrap_or_default());
            }
            Statement::Assert {
                condition, message, ..
            } => {
                self.visit_expression(condition);
                self.visit_expression(message);
            }
            _ => {}
        }
    }

    fn block(&mut self, block: &rumoca_core::StatementBlock) {
        self.visit_expression(&block.cond);
        self.statements(&block.stmts);
    }
}

impl ExpressionVisitor for Reads<'_, '_> {
    fn visit_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Literal {
                value: rumoca_core::Literal::String(_),
                ..
            } => self.string_value = true,
            Expression::VarRef { name, .. } => {
                let model_name = self
                    .environment
                    .flat
                    .variables
                    .get(&VarName::new(name.as_str()));
                match model_name {
                    Some(variable) if is_string(self.environment.flat, variable) => {
                        self.string_value = true;
                    }
                    Some(_) if name.as_str() == self.environment.target.as_str() => {}
                    Some(_) if self.environment.fixed(name.as_str()).is_none() => {
                        self.unfixed = true;
                    }
                    // A loop index, or a fixed model name.
                    _ => {}
                }
                self.walk_expression(expr);
            }
            _ => self.walk_expression(expr),
        }
    }
}
