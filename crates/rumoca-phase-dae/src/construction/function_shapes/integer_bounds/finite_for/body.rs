//! At most one exact increment site on each ordinary conditional path.
use super::*;
use rumoca_core::ComponentReference;

pub(super) fn whole_counter(component: &ComponentReference, declaration: DefId) -> bool {
    let [part] = component.parts() else {
        return false;
    };
    part.subs.is_empty() && part.def_id == declaration
}

pub(super) fn try_updates(statements: &[Statement], declaration: DefId) -> Option<u8> {
    let mut updates = 0u8;
    for statement in statements {
        updates = updates.checked_add(try_statement(statement, declaration)?)?;
        if updates > 1 {
            return None;
        }
    }
    Some(updates)
}

fn try_statement(statement: &Statement, declaration: DefId) -> Option<u8> {
    match statement {
        Statement::Empty { .. } => Some(0),
        Statement::Assignment { comp, value, .. } => {
            if comp.root_def_id() != declaration {
                return Some(0);
            }
            (whole_counter(comp, declaration) && is_unit_increment(value, declaration)).then_some(1)
        }
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            let mut maximum = 0;
            for branch in cond_blocks {
                maximum = maximum.max(try_updates(&branch.stmts, declaration)?);
            }
            if let Some(fallback) = else_block {
                maximum = maximum.max(try_updates(fallback, declaration)?);
            }
            Some(maximum)
        }
        // Unknown calls, nested loops, break/return, assertions and events
        // have no proof in this family, even if a branch looks unreachable.
        _ => None,
    }
}

fn is_unit_increment(value: &Expression, declaration: DefId) -> bool {
    let Expression::Binary {
        op: OpBinary::Add,
        lhs,
        rhs,
        ..
    } = value
    else {
        return false;
    };
    matches!(
        rhs.as_ref(),
        Expression::Literal {
            value: Literal::Integer(1),
            ..
        }
    ) && matches!(lhs.as_ref(), Expression::VarRef { name, subscripts, .. }
            if subscripts.is_empty()
                && name.target_def_id() == Some(declaration)
                && name.component_ref().is_some_and(|component| whole_counter(component, declaration)))
}
