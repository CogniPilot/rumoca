//! One evaluation of each constant lookup table per loop-owning scope.
//!
//! An array literal such as a stencil offset table lowers to a tree of scalar
//! constants and concatenations. Lowering it where a loop body reads it
//! rebuilds that tree on every pass of the loop. A table built only from
//! literals depends on no coordinate, binder, parameter or definition, so the
//! scope that opens the loop issues it once, before the loop region is built,
//! and the region reads it as one capture (`environment_requirements`),
//! exactly like a value the scope computed earlier.
//!
//! The property that decides it is ownership, not the shape of the reader: a
//! literal is hoisted when the loop only reads it. A literal that is the entry
//! value of a carried array of any loop of the function, or the base of an
//! element update, must stay a private value the loop updates in place: a
//! hoisted copy would be a shared capture that every update copies whole. A
//! generated fill such as `zeros(n)` is one operation already and is not a
//! literal table.

use std::collections::BTreeSet;

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::ExpressionLowerer;

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    /// Issue the literal tables `expressions` only read that this scope has
    /// not issued, so a loop region over them captures instead of rebuilding.
    pub(super) fn issue_constant_aggregates(
        &mut self,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>>,
        function: Option<dae::FunctionId<'dae>>,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let mut owned = function.map_or_else(BTreeSet::new, |function| {
            self.carried_entry_literals(function)
        });
        let mut tables = Vec::new();
        let mut traversal = dae::ExpressionTraversal::new();
        self.visit_capture_expressions(&mut traversal, expressions, |id, node| {
            if let dae::ExpressionOperation::ArrayUpdate { base, .. } = node.operation() {
                owned.insert(base);
            }
            // A table is issued whole; the literals beneath it are its own
            // construction and are neither tables to issue nor reads of the loop.
            if self.cache.contains_key(&id) {
                return false;
            }
            if self.is_literal_table(id) {
                tables.push(id);
                return false;
            }
            true
        });
        for table in tables.into_iter().filter(|table| !owned.contains(table)) {
            self.expression(table)?;
        }
        Ok(())
    }

    /// The right-hand sides that define the entry value of a carried target of
    /// some loop of `function`.
    pub(super) fn carried_entry_literals(
        &self,
        function: dae::FunctionId<'dae>,
    ) -> BTreeSet<dae::ExprId<'dae>> {
        let mut owned = BTreeSet::new();
        let Some(view) = self.view.function(function) else {
            return owned;
        };
        for ordinal in 0..view.fold_count() {
            let initials = view
                .fold_id(ordinal)
                .and_then(|fold| self.view.function_fold(fold))
                .map(|fold| fold.initial_values());
            owned.extend(
                (0..initials.map_or(0, |values| values.len())).filter_map(|at| {
                    initials
                        .and_then(|values| values.get(at))
                        .map(|definition| definition.rhs())
                }),
            );
        }
        owned
    }

    /// Whether `id` is a multi-element array built only from numeric or
    /// Boolean literals.
    fn is_literal_table(&self, id: dae::ExprId<'dae>) -> bool {
        self.view.expression(id).is_some_and(|node| {
            self.view
                .value_type(node.value_type_id())
                .is_some_and(|value_type| value_type.dimensions().iter().product::<u32>() > 1)
        }) && is_literal_tree(self.view, id)
    }
}

fn is_literal_tree<'dae>(view: dae::DaeView<'dae>, id: dae::ExprId<'dae>) -> bool {
    let Some(node) = view.expression(id) else {
        return false;
    };
    match node.operation() {
        dae::ExpressionOperation::Literal(literal) => {
            !matches!(literal, dae::DaeLiteral::String(_))
        }
        dae::ExpressionOperation::Unary { operand, .. } => is_literal_tree(view, operand),
        dae::ExpressionOperation::Array(operands) => operands
            .iter()
            .all(|operand| is_literal_tree(view, operand)),
        dae::ExpressionOperation::Builtin {
            builtin: dae::PureBuiltin::PromotedCat1 | dae::PureBuiltin::PromotedCat2,
            arguments,
        } => arguments
            .iter()
            .all(|operand| is_literal_tree(view, operand)),
        _ => false,
    }
}
