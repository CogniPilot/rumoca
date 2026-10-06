//! Targets for which a one-pass interval cannot prove every reaching value.

use super::*;
use rumoca_core::Reference;

/// A mutable recurrence needs a fixed-point proof; observing one iteration is
/// never such a proof. Unknown writes also invalidate a previous finite fact.
pub(super) fn invalidated_integer_targets(
    statements: &[rumoca_core::Statement],
) -> HashSet<VarName> {
    let mut finder = Writes::default();
    for statement in statements {
        rumoca_ir_flat::visitor::StatementVisitor::visit_statement(&mut finder, statement);
    }
    let mut invalidated = finder.unknown_writes.clone();
    for (target, definitions) in &finder.definitions {
        if definitions.iter().any(|definition| {
            let mut references = Vec::new();
            definition.collect_var_refs(&mut references);
            references
                .into_iter()
                .any(|reference| &reference == target || finder.loop_writes.contains(&reference))
        }) {
            invalidated.insert(target.clone());
        }
    }
    invalidated
}

#[derive(Default)]
struct Writes {
    definitions: HashMap<VarName, Vec<Expression>>,
    loop_writes: HashSet<VarName>,
    unknown_writes: HashSet<VarName>,
    loop_depth: usize,
}

pub(super) fn written_integer_targets(statements: &[rumoca_core::Statement]) -> HashSet<VarName> {
    let mut finder = Writes::default();
    for statement in statements {
        rumoca_ir_flat::visitor::StatementVisitor::visit_statement(&mut finder, statement);
    }
    finder
        .definitions
        .into_keys()
        .chain(finder.unknown_writes)
        .collect()
}

impl rumoca_core::ExpressionVisitor for Writes {}

impl rumoca_ir_flat::visitor::StatementVisitor for Writes {
    fn visit_assignment(
        &mut self,
        component: &rumoca_core::ComponentReference,
        value: &Expression,
    ) {
        if let Some(target) = integer_assignment_target(component) {
            if self.loop_depth > 0 {
                self.loop_writes.insert(target.clone());
            }
            self.definitions
                .entry(target)
                .or_default()
                .push(value.clone());
        }
    }

    fn visit_for_statement(
        &mut self,
        _: &[rumoca_core::ForIndex],
        statements: &[rumoca_core::Statement],
    ) {
        self.loop_depth += 1;
        for statement in statements {
            self.visit_statement(statement);
        }
        self.loop_depth -= 1;
    }

    fn visit_statement_function_call(
        &mut self,
        _: &Reference,
        _: &[Expression],
        outputs: &[Option<rumoca_core::ComponentReference>],
    ) {
        for output in outputs.iter().flatten() {
            let Some(target) = integer_assignment_target(output) else {
                continue;
            };
            if self.loop_depth > 0 {
                self.loop_writes.insert(target.clone());
            }
            self.unknown_writes.insert(target);
        }
    }

    fn visit_statement(&mut self, statement: &rumoca_core::Statement) {
        if let rumoca_core::Statement::While { block, .. } = statement {
            self.loop_depth += 1;
            for statement in &block.stmts {
                self.visit_statement(statement);
            }
            self.loop_depth -= 1;
        } else {
            self.walk_statement(statement);
        }
    }
}
