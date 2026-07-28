//! A scalar-width rank witness over the unscalarized continuous partition.
//!
//! [`crate::incidence::build_incidence`] counts one row per equation but one
//! column per *scalar* unknown, so it is a rank witness only after
//! scalarization. Index reduction cannot wait that long: the symbolic
//! derivative closure keys defining expressions on aggregate variable names
//! (`b0.frame_b.r_0`), and after scalarization no row assigns that name any
//! more — every candidate is then rejected as an unresolved `der` leaf.
//!
//! So the deficiency has to be found on the aggregate DAE, with rows counted
//! the way columns already are. This view expands each equation into the number
//! of scalar rows the flattener recorded on it as `scalar_count`, falling back
//! to a measured `residual_scalar_width` only where the flattener recorded
//! nothing at all (`scalar_count == 0`) — see [`row_width`] for why the
//! recorded count wins. Every one of those rows is given the *union* of the
//! scalar columns the equation's variables occupy.
//!
//! That union is a deliberate over-approximation: the real scalarized row
//! `b0.frame_b.r_0[1] = j2.frame_a.r_0[1]` touches two columns, and this view
//! gives all three of the family's rows all six. Extra edges can only make the
//! matching larger, so a deficiency reported here is a deficiency in the real
//! scalarized system too — never the reverse. The pass therefore under-reports
//! rather than inventing work, which is the safe direction for a transformation
//! that rewrites equations.

use std::collections::HashMap;

use crate::incidence::rows::{IncidenceRows, IncidenceRowsBuilder};

use super::row_shape::residual_scalar_width;
use super::{Dae, Expression, VarName, collect_rhs_var_refs};

/// Scalar-width compatibility graph plus the source row each scalar row came
/// from.
pub(super) struct ScalarRankView {
    pub(super) n_eq: usize,
    pub(super) n_var: usize,
    pub(super) rows: IncidenceRows,
    /// Scalar row index → index in `dae.continuous.equations`.
    pub(super) source_equation: Vec<usize>,
}

/// Column ranges of the unknowns, keyed by aggregate variable name.
struct ColumnLayout {
    /// `der(x)` columns for each state `x`.
    derivative: HashMap<String, (usize, usize)>,
    /// Value columns for each algebraic and output.
    value: HashMap<String, (usize, usize)>,
    total: usize,
}

/// Build the rank witness, or `None` when the model does not admit one.
///
/// `None` is returned for an empty system and for a system with a row whose
/// scalar width cannot be determined. Declining to analyse is the honest
/// outcome there: this pass rewrites equations, and it must not act on a rank
/// claim it could not compute.
pub(super) fn build(dae: &Dae) -> Option<ScalarRankView> {
    build_with_row_filter(dae, &mut |_, _| true)
}

/// [`build`], counting only the equations `keep` accepts.
///
/// `keep` receives each equation's index and the equation itself, and every row
/// it declines is left out of both the row count and the matching. Dropping a
/// row can only lower the deficiency, so a filtered view stays a lower bound on
/// the real one — the direction this module's soundness argument needs.
fn build_with_row_filter(
    dae: &Dae,
    keep: &mut dyn FnMut(usize, &rumoca_ir_dae::Equation) -> bool,
) -> Option<ScalarRankView> {
    let layout = column_layout(dae);
    if layout.total == 0 {
        return None;
    }
    let mut builder = IncidenceRowsBuilder::with_row_capacity(dae.continuous.equations.len());
    let mut source_equation = Vec::new();
    for (index, equation) in dae.continuous.equations.iter().enumerate() {
        let width = row_width(dae, equation)?;
        if !keep(index, equation) {
            continue;
        }
        let columns = row_columns(&layout, equation);
        for _ in 0..width {
            builder.push_unsorted(columns.iter().copied());
            source_equation.push(index);
        }
    }
    Some(ScalarRankView {
        n_eq: source_equation.len(),
        n_var: layout.total,
        rows: builder.finish(),
        source_equation,
    })
}

/// Scalar width of one row, or `None` when the shape is not determinable.
///
/// Two sources can disagree: `scalar_count` is the number of scalar rows the
/// flattener recorded for this equation, and `residual_scalar_width` is what
/// the residual's own shape says now, after index reduction has rewritten it.
///
/// The declared count wins wherever it exists. The module's soundness argument
/// covers extra *columns* only — extra edges can only enlarge a matching, so an
/// over-approximated column set can only under-report deficiency. Extra *rows*
/// run the other way: a row this view invents has no column to match and is
/// counted as deficiency the real scalarized system does not have, which is the
/// one direction that would make the pass differentiate equations for no
/// reason. So `residual_scalar_width` is consulted only where the flattener
/// recorded nothing at all (`scalar_count == 0`), and a disagreement is traced
/// rather than resolved upwards.
fn row_width(dae: &Dae, equation: &rumoca_ir_dae::Equation) -> Option<usize> {
    let declared = equation.scalar_count;
    let measured = residual_scalar_width(dae, &equation.rhs)
        .ok()
        .filter(|width| *width > 0);
    if declared == 0 {
        return measured;
    }
    if measured.is_some_and(|width| width != declared) {
        crate::structural_trace!(
            "[sim-trace] scalar-rank-view row width disagreement declared={declared} measured={:?} origin='{}'",
            measured,
            equation.origin
        );
    }
    Some(declared)
}

/// True when every row of the continuous partition stands for exactly one
/// scalar row, so this view has one row per real row and each carries its own
/// column set.
///
/// The module expands an equation into the number of scalar rows the flattener
/// recorded on it and hands all of them the *union* of the columns the equation
/// reads. Where an equation is one scalar wide that union is just its own
/// columns and the matching runs row-for-row with the residual system; where it
/// is wider the matching is an aggregate over a whole family, and a deficiency
/// computed from it says correspondingly less about any individual row.
///
/// A caller that reads the deficiency only as a lower bound does not need this.
/// A caller that wants to act on the *absence* of a change in it does — see
/// [`super::demotion_rank_check::demotion_is_rank_justified`].
pub(super) fn every_row_is_one_scalar_wide(dae: &Dae) -> bool {
    dae.continuous
        .equations
        .iter()
        .all(|equation| row_is_one_scalar_wide(dae, equation))
}

/// True when this one row stands for exactly one scalar row, so the view's row
/// for it carries its own columns and nothing else's.
///
/// A caller rewriting a *single* row does not need
/// [`every_row_is_one_scalar_wide`]: the partitions and every other row are
/// untouched, so the before and after views differ in exactly this row's column
/// set, and the aggregation that makes a family's reading meaningless never
/// enters. It does need this, because a wide row's columns are the union over
/// its whole family and a change to that union says nothing about any one
/// scalar row.
pub(super) fn row_is_one_scalar_wide(dae: &Dae, equation: &rumoca_ir_dae::Equation) -> bool {
    row_width(dae, equation) == Some(1)
}

/// How many rows read no unknown at all.
///
/// Such a row relates only quantities the residual evaluation already knows —
/// states and parameters — so no matching can ever assign it a variable. It is
/// unmatchable on its own terms, independently of what the rest of the system
/// looks like, and it is exactly the shape a position constraint between two
/// states has before index reduction touches it: `accelerate.s = mass.s - L/2`
/// with both sides states carries no column.
///
/// Demoting one of those states gives the row a column and retires the defect
/// outright. That is worth counting separately, because the same demotion also
/// appends the differentiated constraint, and in a constraint chain the appended
/// row lands on a column another row already wanted — so the *matching* can come
/// out level while a genuinely unmatchable row has just been removed.
pub(super) fn columnless_row_count(dae: &Dae) -> usize {
    let layout = column_layout(dae);
    dae.continuous
        .equations
        .iter()
        .filter(|equation| row_columns(&layout, equation).is_empty())
        .count()
}

/// Number of scalar rows the view cannot match: a lower bound on the real
/// scalarized system's rank deficiency.
///
/// `None` when the model does not admit a view at all (see [`build`]).
pub(super) fn deficiency(dae: &Dae) -> Option<usize> {
    deficiency_of(build(dae)?)
}

/// [`deficiency`] over the distinct rows only.
///
/// Two residual rows that are the same equation are one equation. The pipeline
/// removes the copy downstream, so counting both here reports a defect the
/// solved system will not have — and index reduction *creates* such a pair
/// routinely: rewriting `der(x)` to the generated dummy turns an existing
/// `der(x) = der(q)` row into the very row the differentiated constraint
/// appends. Judging a demotion on a reading that counts its own removable copy
/// refuses the demotion for a defect that does not survive to the matcher.
///
/// Rows are bucketed on [`rumoca_core::expression_semantic_fingerprint`], which
/// is span-insensitive, and confirmed inside the bucket with
/// [`rumoca_core::expressions_semantically_equal`] — the fingerprint is a lookup
/// accelerator, never an identity on its own.
pub(super) fn deficiency_over_distinct_rows(dae: &Dae) -> Option<usize> {
    let mut seen: HashMap<u64, Vec<usize>> = HashMap::new();
    let mut keep = |index: usize, equation: &rumoca_ir_dae::Equation| {
        let bucket = seen
            .entry(rumoca_core::expression_semantic_fingerprint(&equation.rhs))
            .or_default();
        if bucket.iter().any(|earlier| {
            rumoca_core::expressions_semantically_equal(
                &dae.continuous.equations[*earlier].rhs,
                &equation.rhs,
            )
        }) {
            return false;
        }
        bucket.push(index);
        true
    };
    deficiency_of(build_with_row_filter(dae, &mut keep)?)
}

fn deficiency_of(view: ScalarRankView) -> Option<usize> {
    if view.n_eq == 0 || view.n_var == 0 {
        return Some(0);
    }
    let (match_eq, _) = crate::matching::maximum_matching(view.n_eq, view.n_var, &view.rows, &[]);
    Some(match_eq.iter().filter(|matched| matched.is_none()).count())
}

/// Assign every unknown a contiguous run of scalar columns.
fn column_layout(dae: &Dae) -> ColumnLayout {
    let mut derivative = HashMap::new();
    let mut value = HashMap::new();
    let mut next = 0usize;
    for (name, variable) in &dae.variables.states {
        let size = variable.size();
        if size == 0 {
            continue;
        }
        derivative.insert(name.as_str().to_string(), (next, size));
        next += size;
    }
    for (name, variable) in dae
        .variables
        .algebraics
        .iter()
        .chain(dae.variables.outputs.iter())
    {
        let size = variable.size();
        if size == 0 {
            continue;
        }
        value.insert(name.as_str().to_string(), (next, size));
        next += size;
    }
    ColumnLayout {
        derivative,
        value,
        total: next,
    }
}

/// Every scalar column the row's variables occupy.
///
/// A state contributes a column only where the row actually reads `der(state)`:
/// the state's own value is known at the time the residual is evaluated, so
/// treating a plain state reference as an unknown would hand the matcher a
/// column that does not exist and hide the deficiency this view exists to find.
/// The over-approximation is confined to *components*: a row reading one
/// element of a vector is given the whole vector's columns.
fn row_columns(layout: &ColumnLayout, equation: &rumoca_ir_dae::Equation) -> Vec<usize> {
    let mut columns = Vec::new();
    for name in collect_rhs_var_refs(&equation.rhs) {
        extend_with_columns(&layout.value, name.as_str(), &mut columns);
    }
    for name in differentiated_names(&equation.rhs) {
        extend_with_columns(&layout.derivative, name.as_str(), &mut columns);
    }
    columns
}

/// Columns of the unknown `name` names, resolving a component reference onto
/// the variable it belongs to.
///
/// The layout is keyed on aggregate names, but a row writes whatever the model
/// wrote — `x[1]` for one element of `x`. Looking that up literally misses, and
/// a miss *drops* a column, which over-reports deficiency: the one direction
/// this view must never take, since it would have the pass differentiate rows
/// for a defect the real system does not have. So a miss falls back to the
/// component's base name and takes the whole variable's range, which is the
/// module's usual over-approximation and stays on the safe side.
fn extend_with_columns(
    layout: &HashMap<String, (usize, usize)>,
    name: &str,
    columns: &mut Vec<usize>,
) {
    let entry = layout.get(name).copied().or_else(|| {
        let base = rumoca_ir_dae::component_base_name(name)?;
        layout.get(&base).copied()
    });
    if let Some((start, size)) = entry {
        columns.extend(start..start + size);
    }
}

/// Variable names read under a `der(...)` in this row.
fn differentiated_names(expr: &Expression) -> Vec<VarName> {
    let mut collector = DifferentiatedNames { names: Vec::new() };
    rumoca_core::ExpressionVisitor::visit_expression(&mut collector, expr);
    collector.names
}

struct DifferentiatedNames {
    names: Vec<VarName>,
}

impl rumoca_core::ExpressionVisitor for DifferentiatedNames {
    fn visit_builtin_call(&mut self, function: &rumoca_core::BuiltinFunction, args: &[Expression]) {
        if *function == rumoca_core::BuiltinFunction::Der {
            for arg in args {
                arg.collect_var_refs(&mut self.names);
            }
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}
