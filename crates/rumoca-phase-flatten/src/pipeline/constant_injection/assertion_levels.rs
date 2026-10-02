use super::flat;
use rumoca_core::{DefId, Expression, Statement};

/// The predefined `AssertionLevel` literal declarations (MLS §8.3.7).
#[derive(Clone, Copy)]
pub(crate) struct PredefinedAssertionLevels {
    pub(crate) error: Option<DefId>,
    pub(crate) warning: Option<DefId>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum AssertionLevel {
    /// The default level, written or omitted.
    Error,
    Warning,
    /// A level this pass cannot identify; downstream owners decide.
    Unidentified,
}

impl PredefinedAssertionLevels {
    /// The literal declarations the scope tree predefines.
    pub(crate) fn of(tree: &rumoca_ir_ast::ClassTree) -> Self {
        let literal = |name| {
            tree.scope_tree
                .predefined_member(&rumoca_core::ComponentPath::from_parts([
                    "AssertionLevel",
                    name,
                ]))
        };
        Self {
            error: literal("error"),
            warning: literal("warning"),
        }
    }

    /// A level is a predefined literal exactly when its structured reference
    /// targets that literal's declaration, never by a rendered spelling.
    fn classify(self, level: Option<&Expression>) -> AssertionLevel {
        let Some(level) = level else {
            return AssertionLevel::Error;
        };
        let Expression::VarRef {
            name, subscripts, ..
        } = level
        else {
            return AssertionLevel::Unidentified;
        };
        let target = name
            .component_ref()
            .filter(|_| subscripts.is_empty())
            .map(rumoca_core::ComponentReference::target_def_id);
        match target {
            Some(target) if Some(target) == self.error => AssertionLevel::Error,
            Some(target) if Some(target) == self.warning => AssertionLevel::Warning,
            _ => AssertionLevel::Unidentified,
        }
    }
}

/// Settle the MLS §8.3.7 level of every assertion of the model and its
/// functions.
///
/// "If the level is AssertionLevel.warning, the current evaluation is not
/// aborted" and "the assert(..) statement shall have no influence on the
/// behavior of the model": a warning-level assertion never fails a call, an
/// event iteration, or a step, and its condition must not create events.
/// Such an assertion therefore owns no runtime action and is removed here,
/// at its single owner, so no later phase can evaluate its condition. An
/// explicit `AssertionLevel.error` is the default level and is normalized to
/// the omitted form. A level that is not one of the predefined literals keeps
/// the assertion unchanged for the downstream owners, which reject it.
pub(crate) fn settle_assertion_levels(flat: &mut flat::Model, levels: PredefinedAssertionLevels) {
    for assertions in [
        &mut flat.assert_equations,
        &mut flat.initial_assert_equations,
    ] {
        assertions.retain_mut(|assertion| {
            match levels.classify(assertion.level.as_ref()) {
                AssertionLevel::Error => assertion.level = None,
                AssertionLevel::Warning => return false,
                AssertionLevel::Unidentified => {}
            }
            true
        });
    }
    for algorithm in flat
        .algorithms
        .iter_mut()
        .chain(flat.initial_algorithms.iter_mut())
    {
        settle_statements(&mut algorithm.statements, levels);
    }
    for function in flat.functions.values_mut() {
        settle_statements(&mut function.body, levels);
    }
    for chain in &mut flat.when_chains {
        for branch in chain.branches_mut() {
            settle_when_equations(&mut branch.equations, levels);
        }
    }
}

fn settle_statements(statements: &mut Vec<Statement>, levels: PredefinedAssertionLevels) {
    statements.retain_mut(|statement| settle_statement(statement, levels));
}

/// Settle one statement; returns whether it stays.
fn settle_statement(statement: &mut Statement, levels: PredefinedAssertionLevels) -> bool {
    match statement {
        Statement::Assert { level, .. } => match levels.classify(level.as_deref()) {
            AssertionLevel::Error => *level = None,
            AssertionLevel::Warning => return false,
            AssertionLevel::Unidentified => {}
        },
        Statement::FunctionCall {
            comp,
            args,
            outputs,
            ..
        } if outputs.iter().all(Option::is_none)
            && rumoca_core::runtime_flow_action_function_short_name(comp.as_str())
                == Some("assert")
            && args.len() == 3 =>
        {
            match levels.classify(args.get(2)) {
                AssertionLevel::Error => args.truncate(2),
                AssertionLevel::Warning => return false,
                AssertionLevel::Unidentified => {}
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                settle_statements(&mut block.stmts, levels);
            }
            if let Some(block) = else_block {
                settle_statements(block, levels);
            }
        }
        Statement::For { equations, .. } => settle_statements(equations, levels),
        Statement::While { block, .. } => settle_statements(&mut block.stmts, levels),
        Statement::When { blocks, .. } => {
            for block in blocks {
                settle_statements(&mut block.stmts, levels);
            }
        }
        _ => {}
    }
    true
}

fn settle_when_equations(
    equations: &mut Vec<flat::WhenEquation>,
    levels: PredefinedAssertionLevels,
) {
    equations.retain_mut(|equation| match equation {
        flat::WhenEquation::Assert { level, .. } => match levels.classify(level.as_deref()) {
            AssertionLevel::Error => {
                *level = None;
                true
            }
            AssertionLevel::Warning => false,
            AssertionLevel::Unidentified => true,
        },
        flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (_, branch) in branches.iter_mut() {
                settle_when_equations(branch, levels);
            }
            if let Some(branch) = else_branch {
                settle_when_equations(branch, levels);
            }
            true
        }
        _ => true,
    });
}
