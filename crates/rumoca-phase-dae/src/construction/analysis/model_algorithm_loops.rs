//! Model-algorithm `for` statements that carry a value from one iteration to
//! the next.
//!
//! A loop whose body assigns elements of an array (`y[i] := ...`) is a tensor
//! definition and keeps its compact domain. A loop whose body instead assigns
//! whole variables (`y := if time >= t[i] then x[i] else y`) threads one
//! value through its iterations in order (MLS 3.7 §11.1.2), which a compact
//! domain cannot hold. When the range is fixed at translation (MLS §11.2.2,
//! see `fixed_loops`), such a loop is exactly its unrolled sequence, and each
//! relation of each iteration is its own event owner (MLS §8.5). A loop whose
//! body assigns nothing is the same: its unrolled sequence holds only the
//! body's checks, or nothing at all.
//!
//! The unrolled algorithms replace the source ones before analysis, so every
//! analysis and the construction that follows read one statement list.

use std::borrow::Cow;

use super::*;
use rumoca_core::StatementRewriter;

/// The model with every carrying `for` of its algorithms unrolled, borrowed
/// unchanged when no algorithm has such a loop.
pub(in crate::construction) fn unroll_carrying_algorithm_loops(
    flat: &flat::Model,
) -> Result<Cow<'_, flat::Model>, ToDaeError> {
    if !flat
        .algorithms
        .iter()
        .any(|algorithm| contains_carrying_loop(&algorithm.statements))
    {
        return Ok(Cow::Borrowed(flat));
    }
    let constants = constant_context(flat)?;
    let evaluable = evaluable_parameters(flat);
    let shapes = FunctionShapeAnalysis::analyze_model(flat, &constants, Some(&evaluable))?;
    let unroll = CarryingLoops {
        flat,
        shapes: shapes.model_values(),
    };
    let mut unrolled = flat.clone();
    for algorithm in &mut unrolled.algorithms {
        if contains_carrying_loop(&algorithm.statements) {
            algorithm.statements = unroll.statements(&algorithm.statements)?;
        }
    }
    Ok(Cow::Owned(unrolled))
}

fn contains_carrying_loop(statements: &[rumoca_core::Statement]) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::For { equations, .. } => {
            carries_whole_values(equations) || contains_carrying_loop(equations)
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| contains_carrying_loop(&block.stmts))
                || else_block.as_deref().is_some_and(contains_carrying_loop)
        }
        rumoca_core::Statement::When { blocks, .. } => blocks
            .iter()
            .any(|block| contains_carrying_loop(&block.stmts)),
        _ => false,
    })
}

/// Whether a loop body writes only whole variables (or nothing), so no
/// compact element domain can own it.
fn carries_whole_values(body: &[rumoca_core::Statement]) -> bool {
    let mut writes = BodyWrites::default();
    writes.collect(body);
    !writes.elements && !writes.opaque
}

#[derive(Default)]
struct BodyWrites {
    /// Some assignment writes an element of an array.
    elements: bool,
    /// Some statement other than an assignment, `if`, `for`, or `assert`
    /// (a call, a `when`, `while`, `break`, or `return`), whose iterations
    /// are not one straight-line sequence of assignments and checks.
    opaque: bool,
}

impl BodyWrites {
    fn collect(&mut self, statements: &[rumoca_core::Statement]) {
        for statement in statements {
            self.collect_statement(statement);
        }
    }

    fn collect_statement(&mut self, statement: &rumoca_core::Statement) {
        match statement {
            rumoca_core::Statement::Assignment { comp, .. } => {
                self.elements |= comp.parts().iter().any(|part| !part.subs.is_empty());
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    self.collect(&block.stmts);
                }
                if let Some(fallback) = else_block {
                    self.collect(fallback);
                }
            }
            rumoca_core::Statement::For { equations, .. } => self.collect(equations),
            rumoca_core::Statement::Assert { .. } | rumoca_core::Statement::Empty { .. } => {}
            _ => self.opaque = true,
        }
    }
}

struct CarryingLoops<'flat> {
    flat: &'flat flat::Model,
    shapes: &'flat ShapeEnvironment,
}

impl CarryingLoops<'_> {
    fn statements(
        &self,
        statements: &[rumoca_core::Statement],
    ) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
        let mut unrolled = Vec::with_capacity(statements.len());
        for statement in statements {
            self.statement(statement, &mut unrolled)?;
        }
        Ok(unrolled)
    }

    fn statement(
        &self,
        statement: &rumoca_core::Statement,
        unrolled: &mut Vec<rumoca_core::Statement>,
    ) -> Result<(), ToDaeError> {
        match statement {
            rumoca_core::Statement::For {
                indices,
                equations,
                span,
            } if carries_whole_values(equations) => {
                self.iterations(indices, equations, *span, unrolled)?;
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            } => unrolled.push(rumoca_core::Statement::If {
                cond_blocks: cond_blocks
                    .iter()
                    .map(|block| {
                        Ok(rumoca_core::StatementBlock {
                            cond: block.cond.clone(),
                            stmts: self.statements(&block.stmts)?,
                        })
                    })
                    .collect::<Result<_, ToDaeError>>()?,
                else_block: else_block
                    .as_deref()
                    .map(|fallback| self.statements(fallback))
                    .transpose()?,
                span: *span,
            }),
            rumoca_core::Statement::When { blocks, span } => {
                unrolled.push(rumoca_core::Statement::When {
                    blocks: blocks
                        .iter()
                        .map(|block| {
                            Ok(rumoca_core::StatementBlock {
                                cond: block.cond.clone(),
                                stmts: self.statements(&block.stmts)?,
                            })
                        })
                        .collect::<Result<_, ToDaeError>>()?,
                    span: *span,
                });
            }
            _ => unrolled.push(statement.clone()),
        }
        Ok(())
    }

    fn iterations(
        &self,
        indices: &[rumoca_core::ForIndex],
        body: &[rumoca_core::Statement],
        span: Span,
        unrolled: &mut Vec<rumoca_core::Statement>,
    ) -> Result<(), ToDaeError> {
        let Some((index, rest)) = indices.split_first() else {
            unrolled.extend(self.statements(body)?);
            return Ok(());
        };
        let range = fixed_range(self.flat, self.shapes, &index.range, "model", span)?;
        for value in range_values(range) {
            let mut binding = IndexBinding {
                name: &index.ident,
                value,
                span,
            };
            let rest = binding.rewrite_for_indices(rest);
            let body = binding.rewrite_statements(body);
            self.iterations(&rest, &body, span, unrolled)?;
        }
        Ok(())
    }
}
