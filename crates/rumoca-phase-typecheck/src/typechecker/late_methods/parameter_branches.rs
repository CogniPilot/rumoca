//! MLS §8.3.4 parameter if-equation branch selection.
//!
//! An if-equation whose branch conditions are parameter expressions is
//! resolved at translation time: only the equations of the selected branch
//! become part of the flat model, and the branches that are not selected are
//! removed before any later rule (array bounds of MLS §10.5.1, equation type
//! compatibility of MLS §6.7) can apply to them. This is what lets
//! `Modelica.Fluid.Pipes.BaseClasses.PartialTwoPortFlow` write `lengths[2]`
//! in a branch that only exists when `n >= 2`.
//!
//! The late typecheck pass therefore asks this module which branch a
//! parameter if-equation selects, and walks only that branch. When the
//! conditions are not evaluable in the current instance the answer is `None`
//! and every branch is walked, which is the behaviour MLS §8.3.4 requires for
//! conditions that vary at simulation time.

use super::*;
use rumoca_ir_ast::Visitor;
use std::ops::ControlFlow;

impl TypeChecker {
    /// Index of the branch a parameter if-equation selects (MLS §8.3.4).
    ///
    /// Returns `Some(index)` into `cond_blocks`, `Some(cond_blocks.len())`
    /// when the `else` branch is selected, and `None` when the conditions are
    /// not translation-time evaluable in the current instance scope.
    pub(crate) fn select_parameter_if_branch(
        &self,
        cond_blocks: &[rumoca_ir_ast::EquationBlock],
    ) -> Option<usize> {
        let scope = self.current_instance_scope.as_ref()?.to_flat_string();
        for (index, block) in cond_blocks.iter().enumerate() {
            if !self.condition_is_translation_time(&block.cond) {
                return None;
            }
            let selected = rumoca_eval_ast::eval::eval_boolean_with_scope(
                &block.cond,
                &self.eval_ctx,
                &scope,
            )?;
            if selected {
                return Some(index);
            }
        }
        Some(cond_blocks.len())
    }

    /// MLS §3.8: an if-equation is resolved at translation time only when its
    /// condition is a parameter expression. A reference this instance declares
    /// with discrete or continuous variability rules that out, so the branch
    /// stays in the model and is checked. References the instance does not
    /// declare (enumeration literals, package constants reached through a type
    /// scope) do not by themselves disqualify the condition; whether they have
    /// a translation-time value is then the evaluator's answer.
    fn condition_is_translation_time(&self, condition: &Expression) -> bool {
        let mut collector = ConditionReferences::default();
        let _ = collector.visit_expression(condition);
        collector
            .references
            .iter()
            .all(|reference| !self.is_simulation_time_reference(reference))
    }

    fn is_simulation_time_reference(&self, reference: &rumoca_ir_ast::ComponentReference) -> bool {
        if reference.parts.len() == 1
            && reference.parts[0].ident.text.as_ref() == "time"
            && reference.parts[0].subs.iter().flatten().next().is_none()
        {
            return true;
        }
        match self.lookup_component_reference_variability(reference) {
            SemanticLookup::Found(
                rumoca_eval_ast::eval::VariabilityLevel::Constant
                | rumoca_eval_ast::eval::VariabilityLevel::Parameter,
            )
            | SemanticLookup::Missing => false,
            SemanticLookup::Found(_) | SemanticLookup::Ambiguous => true,
        }
    }
}

/// Collects the component-reference paths that appear in one expression.
#[derive(Default)]
struct ConditionReferences {
    references: Vec<rumoca_ir_ast::ComponentReference>,
}

impl Visitor for ConditionReferences {
    fn visit_component_reference_ctx(
        &mut self,
        cr: &rumoca_ir_ast::ComponentReference,
        _context: rumoca_ir_ast::ComponentReferenceContext,
    ) -> ControlFlow<()> {
        self.references.push(cr.clone());
        self.visit_component_reference(cr)
    }
}
