//! Targets for which a one-pass interval cannot prove every reaching value.

use super::*;
use rumoca_core::Reference;

/// A mutable recurrence needs a fixed-point proof; observing one iteration is
/// never such a proof. A definition inside a loop that reads the target
/// itself, or any Integer the same loop writes, may read a value carried from
/// an earlier iteration, so its target is invalidated. A read after the loop
/// sees the loop's merged interval, which is sound for a target whose own
/// definitions carry nothing. Unknown writes also invalidate a previous
/// finite fact.
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
            definition.value.collect_var_refs(&mut references);
            references
                .into_iter()
                .any(|reference| &reference == target || definition.carried.contains(&reference))
        }) {
            invalidated.insert(target.clone());
        }
    }
    invalidated
}

/// One Integer definition and the Integers its enclosing loops write, whose
/// values it may observe from an earlier iteration.
struct Definition {
    value: Expression,
    carried: HashSet<VarName>,
}

#[derive(Default)]
struct Writes {
    definitions: HashMap<VarName, Vec<Definition>>,
    unknown_writes: HashSet<VarName>,
    enclosing_loop_writes: Vec<HashSet<VarName>>,
}

impl Writes {
    fn carried(&self) -> HashSet<VarName> {
        self.enclosing_loop_writes
            .iter()
            .flatten()
            .cloned()
            .collect()
    }

    fn visit_loop_body(&mut self, statements: &[rumoca_core::Statement]) {
        self.enclosing_loop_writes
            .push(written_integer_targets(statements));
        for statement in statements {
            rumoca_ir_flat::visitor::StatementVisitor::visit_statement(self, statement);
        }
        self.enclosing_loop_writes.pop();
    }
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
            let carried = self.carried();
            self.definitions
                .entry(target)
                .or_default()
                .push(Definition {
                    value: value.clone(),
                    carried,
                });
        }
    }

    fn visit_for_statement(
        &mut self,
        _: &[rumoca_core::ForIndex],
        statements: &[rumoca_core::Statement],
    ) {
        self.visit_loop_body(statements);
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
            self.unknown_writes.insert(target);
        }
    }

    fn visit_statement(&mut self, statement: &rumoca_core::Statement) {
        if let rumoca_core::Statement::While { block, .. } = statement {
            self.visit_loop_body(&block.stmts);
        } else {
            self.walk_statement(statement);
        }
    }
}
