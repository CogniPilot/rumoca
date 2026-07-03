//! Dependency ordering of the lowered `DoStep` update assignments.
//!
//! MLS B.1b treats the `f_z`/`f_m` rows firing at one event as
//! *simultaneous*: Modelica equations are declarative, so the canonical DAE
//! row order carries no causality guarantee. GALEC `DoStep` is a sequential
//! method, so emitting the rows in raw DAE order would silently compute
//! stale values whenever a row reads a variable assigned by a later row.
//!
//! [`order_by_dependencies`] therefore topologically orders the lowered
//! flat assignments by their **current-tick** reads:
//!
//! - a read of `self.x` creates an edge from the statement assigning `x`
//!   (must run first) to the reader;
//! - `self.'previous(x)'` reads are previous-tick state — the pre slots are
//!   only committed *after* every update (module `methods`), so they never
//!   create edges (their distinct quoted names match no update target);
//! - reads introduced by condition inlining participate like any other read
//!   because the walk runs over the *lowered* expressions;
//! - element-level targets are matched element-wise (`x[1] := f(x[2])` only
//!   depends on the row assigning `x[2]`); a read without statically known
//!   subscripts conservatively depends on every row assigning that base.
//!
//! The sort is stable: rows without an ordering constraint keep their DAE
//! order, so already-causal input orders round-trip unchanged. Rows whose
//! current-tick reads form a cycle are a discrete algebraic loop —
//! simultaneous discrete equations a sequential block cannot express — and
//! are rejected with a stable diagnostic instead of being emitted in a
//! silently wrong order (GAL-007).

use rumoca_ir_galec::ast::{Expression, Name, Reference, Statement};

use crate::diagnostic::GalecTargetError;
use crate::mangle::manifest_name;

/// An assignment target or reference, keyed for dependency matching: the
/// (single-part) name plus its literal subscripts, when all subscripts are
/// integer literals.
struct AccessKey {
    name: Name,
    /// `Some` when every subscript is a literal integer; `None` means the
    /// element cannot be determined statically (matches conservatively).
    subscripts: Option<Vec<i64>>,
}

impl AccessKey {
    /// Whether a read with this key can observe the target `other`.
    fn overlaps(&self, other: &Self) -> bool {
        if self.name != other.name {
            return false;
        }
        match (&self.subscripts, &other.subscripts) {
            (Some(read), Some(target)) => read == target,
            // Statically unknown elements overlap conservatively.
            _ => true,
        }
    }
}

/// Stable topological order of the lowered `DoStep` assignments by their
/// current-tick reads (module docs).
///
/// # Errors
///
/// `unsupported-feature:discrete-algebraic-loop` (ET017) when rows read
/// each other's current-tick values cyclically; `ET018` when a statement is
/// not the single-part flat assignment lowering produces.
pub(crate) fn order_by_dependencies(
    statements: Vec<Statement>,
) -> Result<Vec<Statement>, GalecTargetError> {
    let targets = statements
        .iter()
        .map(target_key)
        .collect::<Result<Vec<_>, _>>()?;
    let reads: Vec<Vec<AccessKey>> = statements.iter().map(read_keys).collect();
    let mut indegree = vec![0_usize; statements.len()];
    let mut dependents: Vec<Vec<usize>> = vec![Vec::new(); statements.len()];
    for (reader, reader_reads) in reads.iter().enumerate() {
        for (writer, target) in targets.iter().enumerate() {
            if reader_reads.iter().any(|read| read.overlaps(target)) {
                dependents[writer].push(reader);
                indegree[reader] += 1;
            }
        }
    }
    stable_kahn(statements, &targets, indegree, &dependents)
}

/// Kahn's algorithm picking the smallest ready index each round, so rows
/// without ordering constraints keep their DAE order.
fn stable_kahn(
    statements: Vec<Statement>,
    targets: &[AccessKey],
    mut indegree: Vec<usize>,
    dependents: &[Vec<usize>],
) -> Result<Vec<Statement>, GalecTargetError> {
    let count = statements.len();
    let mut emitted = vec![false; count];
    let mut order = Vec::with_capacity(count);
    while order.len() < count {
        let Some(next) = (0..count).find(|&index| !emitted[index] && indegree[index] == 0) else {
            let cycle: Vec<String> = (0..count)
                .filter(|&index| !emitted[index])
                .map(|index| format!("`{}`", manifest_name(&targets[index].name)))
                .collect();
            return Err(GalecTargetError::UnsupportedFeature {
                feature: "discrete-algebraic-loop".to_owned(),
                detail: format!(
                    "discrete updates of {} read each other's current-tick \
                     values, forming a discrete algebraic loop (simultaneous \
                     discrete equations cannot be ordered into a sequential \
                     DoStep)",
                    cycle.join(", ")
                ),
                span: None,
            });
        };
        emitted[next] = true;
        for &dependent in &dependents[next] {
            indegree[dependent] -= 1;
        }
        order.push(next);
    }
    let mut slots: Vec<Option<Statement>> = statements.into_iter().map(Some).collect();
    Ok(order
        .into_iter()
        .map(|index| slots[index].take().expect("each index emitted once"))
        .collect())
}

/// The access key of a lowered flat assignment's target. Lowering only
/// produces single-part `self.<name>[literal…]` targets; anything else is a
/// projection bug.
fn target_key(statement: &Statement) -> Result<AccessKey, GalecTargetError> {
    let Statement::Assignment { target, .. } = statement else {
        return Err(GalecTargetError::LoweringInternal {
            detail: "DoStep ordering saw a non-assignment update statement".to_owned(),
        });
    };
    reference_key(target).ok_or_else(|| GalecTargetError::LoweringInternal {
        detail: "DoStep ordering saw a multi-part or local assignment target".to_owned(),
    })
}

/// Key of a single-part state reference; `None` for local or multi-part
/// references (which lowering never produces for block variables).
fn reference_key(reference: &Reference) -> Option<AccessKey> {
    let Reference::State(parts) = reference else {
        return None;
    };
    let [part] = parts.as_slice() else {
        return None;
    };
    let subscripts = part
        .subscripts
        .iter()
        .map(|subscript| match subscript {
            Expression::Integer(value) => Some(*value),
            _ => None,
        })
        .collect::<Option<Vec<i64>>>();
    Some(AccessKey {
        name: part.name.clone(),
        subscripts,
    })
}

/// Every state reference read by the assignment's value expression
/// (including reads inside subscripts).
fn read_keys(statement: &Statement) -> Vec<AccessKey> {
    let mut reads = Vec::new();
    if let Statement::Assignment { value, .. } = statement {
        collect_reads(value, &mut reads);
    }
    reads
}

fn collect_reference_reads(reference: &Reference, reads: &mut Vec<AccessKey>) {
    if let Some(key) = reference_key(reference) {
        reads.push(key);
    }
    let parts = match reference {
        Reference::State(parts) => parts.as_slice(),
        Reference::Local(part) => std::slice::from_ref(part),
    };
    for part in parts {
        for subscript in &part.subscripts {
            collect_reads(subscript, reads);
        }
    }
}

fn collect_reads(expression: &Expression, reads: &mut Vec<AccessKey>) {
    match expression {
        Expression::Bool(_) | Expression::Integer(_) | Expression::Real(_) => {}
        Expression::Ref(reference) | Expression::Neg(reference) => {
            collect_reference_reads(reference, reads);
        }
        Expression::Size { array, dimension } => {
            collect_reference_reads(array, reads);
            collect_reads(dimension, reads);
        }
        Expression::Call(call) => {
            for argument in &call.arguments {
                collect_reads(argument, reads);
            }
        }
        Expression::Paren(inner) | Expression::Not(inner) => collect_reads(inner, reads),
        Expression::If(if_expression) => {
            for (condition, value) in &if_expression.branches {
                collect_reads(condition, reads);
                collect_reads(value, reads);
            }
            collect_reads(&if_expression.else_value, reads);
        }
        Expression::Array(elements) => {
            for element in elements {
                collect_reads(element, reads);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_reads(lhs, reads);
            collect_reads(rhs, reads);
        }
    }
}
