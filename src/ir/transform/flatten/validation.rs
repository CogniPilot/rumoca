//! Validation checks for flatten operations.
//!
//! This module contains validation functions for checking:
//! - Out-of-bounds subscripts on nested component references
//! - Cardinality arguments for array connectors

use crate::ir;
use crate::ir::ast::{Expression, TerminalType};
use crate::ir::visitor::{Visitable, Visitor};
use anyhow::Result;
use std::collections::HashMap;

// =============================================================================
// Nested Subscript Checker Visitor
// =============================================================================

/// Visitor that checks for out-of-bounds subscripts on nested component references.
struct NestedSubscriptChecker<'a> {
    comp_name: &'a str,
    shape: &'a [usize],
    error: Option<String>,
}

impl<'a> NestedSubscriptChecker<'a> {
    fn new(comp_name: &'a str, shape: &'a [usize]) -> Self {
        Self {
            comp_name,
            shape,
            error: None,
        }
    }

    fn into_result(self) -> Result<()> {
        match self.error {
            Some(msg) => anyhow::bail!("{}", msg),
            None => Ok(()),
        }
    }
}

impl Visitor for NestedSubscriptChecker<'_> {
    fn enter_expression(&mut self, node: &Expression) {
        // Skip if we already have an error
        if self.error.is_some() {
            return;
        }

        let Expression::ComponentReference(comp_ref) = node else {
            return;
        };

        // Check if this is a nested reference to our component (comp_name.something)
        let Some(first) = comp_ref.parts.first() else {
            return;
        };

        if comp_ref.parts.len() < 2 || first.ident.text != self.comp_name {
            return;
        }

        // Check subscripts on the first part
        let Some(subs) = &first.subs else {
            return;
        };

        for (dim_idx, sub) in subs.iter().enumerate() {
            if dim_idx >= self.shape.len() {
                self.error = Some(format!(
                    "Wrong number of subscripts in {}[...] ({} subscripts for {} dimensions)",
                    self.comp_name,
                    subs.len(),
                    self.shape.len()
                ));
                return;
            }
            // Check for literal integer subscripts
            let ir::ast::Subscript::Expression(Expression::Terminal {
                token,
                terminal_type: TerminalType::UnsignedInteger,
            }) = sub
            else {
                continue;
            };

            let Ok(idx) = token.text.parse::<usize>() else {
                continue;
            };

            let dim_size = self.shape[dim_idx];
            if idx < 1 || idx > dim_size {
                self.error = Some(format!(
                    "Subscript '{}' for dimension {} (size = {}) of {} is out of bounds",
                    idx,
                    dim_idx + 1,
                    dim_size,
                    self.comp_name
                ));
                return;
            }
        }
    }
}

// =============================================================================
// Cardinality Array Checker Visitor
// =============================================================================

/// Visitor that checks that cardinality() arguments are scalar connectors.
struct CardinalityArrayChecker<'a> {
    comp_shapes: &'a HashMap<String, Vec<usize>>,
    error: Option<String>,
}

impl<'a> CardinalityArrayChecker<'a> {
    fn new(comp_shapes: &'a HashMap<String, Vec<usize>>) -> Self {
        Self {
            comp_shapes,
            error: None,
        }
    }

    fn into_result(self) -> Result<()> {
        match self.error {
            Some(msg) => anyhow::bail!("{}", msg),
            None => Ok(()),
        }
    }
}

impl CardinalityArrayChecker<'_> {
    /// Check if a component reference has insufficient subscripts for its shape.
    fn check_array_arg(
        &self,
        comp_ref: &ir::ast::ComponentReference,
        first: &ir::ast::ComponentRefPart,
        name: &str,
    ) -> Option<String> {
        let shape = self.comp_shapes.get(name)?;
        if shape.is_empty() {
            return None;
        }

        let num_subscripts = first
            .subs
            .as_ref()
            .map_or(0, |s: &Vec<ir::ast::Subscript>| s.len());
        if num_subscripts >= shape.len() {
            return None;
        }

        let effective_shape: Vec<usize> = shape[num_subscripts..].to_vec();
        let shape_str = effective_shape
            .iter()
            .map(|s: &usize| s.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        Some(format!(
            "Type mismatch for positional argument 1 in cardinality(c={}). The argument has type:\n  Connector[{}]\nexpected type:\n  Connector",
            comp_ref, shape_str
        ))
    }
}

impl Visitor for CardinalityArrayChecker<'_> {
    fn enter_expression(&mut self, node: &Expression) {
        // Skip if we already have an error
        if self.error.is_some() {
            return;
        }

        let Expression::FunctionCall { comp, args, .. } = node else {
            return;
        };

        // Check if this is a cardinality call
        let is_cardinality = comp.parts.len() == 1
            && comp
                .parts
                .first()
                .is_some_and(|p| p.ident.text == "cardinality");

        if !is_cardinality || args.is_empty() {
            return;
        }

        let Expression::ComponentReference(comp_ref) = &args[0] else {
            return;
        };

        let Some(first) = comp_ref.parts.first() else {
            return;
        };

        // Check for nested references like a1.c where a1 is an array
        if comp_ref.parts.len() >= 2 {
            if let Some(err) = self.check_array_arg(comp_ref, first, &first.ident.text) {
                self.error = Some(err);
            }
        } else if comp_ref.parts.len() == 1 {
            // Single component reference like `c` - check if it's an array
            if let Some(err) = self.check_array_arg(comp_ref, first, &first.ident.text) {
                self.error = Some(err);
            }
        }
    }
}

// =============================================================================
// Public API
// =============================================================================

/// Check for out-of-bounds subscripts on nested component references.
///
/// This function checks all component references in a class that start with the given
/// component name and validates any subscripts against the component's shape.
/// For example, for `A a[2]` and an expression `a[3].x[1]`, this detects that `a[3]`
/// is out of bounds since `a` only has 2 elements.
///
/// This check must be done BEFORE SubCompNamer transforms the references, because
/// SubCompNamer removes the first part of nested references along with their subscripts.
pub(super) fn check_nested_component_subscripts(
    fclass: &ir::ast::ClassDefinition,
    comp_name: &str,
    comp: &ir::ast::Component,
) -> Result<()> {
    // Only check if component has a shape (is an array)
    if comp.shape.is_empty() {
        return Ok(());
    }

    let mut checker = NestedSubscriptChecker::new(comp_name, &comp.shape);
    fclass.accept(&mut checker);
    checker.into_result()
}

/// Check that cardinality() arguments are scalar connectors (not arrays).
///
/// When we have `A a1[2]` where A contains connector `c`, the reference `a1.c`
/// (without subscripts) refers to `C[2]` - an array of connectors. cardinality()
/// requires a scalar connector, so this should be an error.
///
/// This check must be done BEFORE SubCompNamer transforms the references, because
/// SubCompNamer may alter the component reference structure.
pub(super) fn check_cardinality_array_connectors(
    fclass: &ir::ast::ClassDefinition,
    comp_shapes: &HashMap<String, Vec<usize>>,
) -> Result<()> {
    let mut checker = CardinalityArrayChecker::new(comp_shapes);
    fclass.accept(&mut checker);
    checker.into_result()
}
