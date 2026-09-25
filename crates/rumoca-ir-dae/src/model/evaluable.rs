//! Checked evaluability of `final` and `Evaluate=true` parameters.
//!
//! The `evaluable` attribute licenses STRUCT-T10(a) to replace a parameter by
//! its value (MLS §4.5, §18.6). Construction proves the license in two steps:
//! the attribute itself must describe a fixed, non-tunable parameter with a
//! binding of at most parameter variability, and once every declaration is
//! complete each such binding may read only constants and other evaluable
//! parameters through pure calls.

use super::*;
use crate::expression::Coordinate;

impl<'dae> Variables<'_, 'dae> {
    pub(super) fn validate_evaluable(
        &self,
        variable: VariableId<'dae>,
        attributes: &VariableAttributes<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        if !attributes.evaluable {
            return Ok(());
        }
        let entry = self.storage.variable(variable.index(), provenance)?;
        let fixed = attributes
            .fixed
            .as_deref()
            .is_none_or(|values| values.iter().all(|&value| value));
        let bound = match attributes.binding {
            Some(binding) => matches!(
                self.storage.expr_variability(binding, provenance)?,
                ExpressionVariability::Constant | ExpressionVariability::Parameter
            ),
            None => false,
        };
        if entry.role == VariableRole::Parameter && !attributes.is_tunable && fixed && bound {
            return Ok(());
        }
        Err(DaeConstructionError::InvalidEvaluableParameter {
            name: entry.name.clone(),
            span: provenance.span(),
        })
    }
}

impl Storage {
    /// Every evaluable binding reads only constants and evaluable parameters.
    ///
    /// Forward declarations make this a whole-model fact, so it runs once all
    /// attributes are attached.
    pub(super) fn validate_evaluable_dependencies(&self) -> Result<(), DaeConstructionError> {
        let mut visited = rustc_hash::FxHashSet::default();
        for entry in &self.variables {
            let Some(binding) = entry
                .attributes
                .as_ref()
                .filter(|attributes| attributes.evaluable)
                .and_then(|attributes| attributes.binding)
            else {
                continue;
            };
            visited.clear();
            if !self.evaluable_binding(binding, entry.declaration, &mut visited)? {
                return Err(DaeConstructionError::InvalidEvaluableParameter {
                    name: entry.name.clone(),
                    span: entry.declaration.span(),
                });
            }
        }
        Ok(())
    }

    /// Whether every node reachable from `binding` is evaluable.
    fn evaluable_binding(
        &self,
        binding: u32,
        at: DaeProvenance,
        visited: &mut rustc_hash::FxHashSet<u32>,
    ) -> Result<bool, DaeConstructionError> {
        let mut pending = vec![binding];
        while let Some(expression) = pending.pop() {
            if !visited.insert(expression) {
                continue;
            }
            let node = &self.expressions.nodes[expression as usize];
            if !self.evaluable_node(node, at)? {
                return Ok(false);
            }
            // `size(a, k)` reads only the declared shape of `a`, which is
            // fixed at translation, never its values.
            if let ExprNode::Builtin {
                builtin: crate::PureBuiltin::Size,
                operands,
            } = node
            {
                pending.extend(self.expressions.operands[operands.indices()].iter().skip(1));
                continue;
            }
            node.for_each_child(&self.expressions, |child| pending.push(child));
        }
        Ok(true)
    }

    fn evaluable_node(
        &self,
        node: &ExprNode,
        at: DaeProvenance,
    ) -> Result<bool, DaeConstructionError> {
        Ok(match node {
            ExprNode::Coordinate(Coordinate::Parameter(variable)) => {
                let read = self.variable(*variable, at)?;
                read.role == VariableRole::Constant
                    || read
                        .attributes
                        .as_ref()
                        .is_some_and(|attributes| attributes.evaluable)
            }
            ExprNode::Coordinate(Coordinate::Binder { .. }) => true,
            ExprNode::Coordinate(_) => false,
            ExprNode::Call { function, .. } => self.function_is_pure(*function, at)?,
            _ => true,
        })
    }
}
