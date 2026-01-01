//! A visitor implementation for renaming subcomponents in a component reference.
//!
//! The `SubCompNamer` struct is used to modify the names of subcomponents within
//! a component reference in the intermediate representation (IR) of the code.
//! It operates by checking if the first part of the component reference matches
//! the specified component name (`comp`). If a match is found, the first part
//! of the reference is removed, and the next part is renamed by prefixing it
//! with the component name using dot notation.
//!
//! # Fields
//! - `comp`: The name of the component to match and use as a prefix for renaming.
//! - `is_operator_record`: If true, subscripts from the first part are moved to the
//!   new flattened name (e.g., `u[1].re` -> `u.re[1]` for Complex arrays).
//!
//! # Example
//! Given a component reference like `comp.subcomp`, if `comp` is set to `"comp"`,
//! the visitor will transform it into `comp.subcomp` (flattened with dot separator).
//!
//! For operator records like Complex with `is_operator_record=true`:
//! Given `comp[1].subcomp`, it transforms to `comp.subcomp[1]`.
//!
//! # Trait Implementations
//! Implements the `Visitor` trait, specifically overriding the
//! `exit_component_reference` method to perform the renaming logic.
//!
//! # Method
//! - `exit_component_reference`: Modifies the `ComponentReference` node by
//!   renaming its parts based on the specified component name.
use crate::ir;
use crate::ir::visitor::MutVisitor;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct SubCompNamer {
    pub comp: String,
    /// If true, subscripts from the component are moved to its subcomponents.
    /// This is needed for:
    /// 1. Operator records like Complex where `u[1].re` becomes `u.re[1]`
    /// 2. Array components where `C[idx].p` becomes `C.p[idx]`
    ///
    /// In both cases, the subcomponent becomes an array after flattening.
    pub is_operator_record: bool,
}

impl MutVisitor for SubCompNamer {
    fn exit_component_reference(&mut self, node: &mut ir::ast::ComponentReference) {
        // Only transform if there are at least 2 parts (e.g., comp.subcomp)
        // A single-part reference doesn't need transformation
        if node.parts.len() >= 2 && node.parts[0].ident.text == self.comp {
            // Always move subscripts from the first part to the flattened subcomponent.
            // This transforms:
            // - `u[1].re` to `u.re[1]` for operator records
            // - `C[idx].p` to `C.p[idx]` for array components
            // The subscripts must be preserved because after flattening, the subcomponent
            // becomes an array with the same dimensions as the parent component.
            let first_part_subs = node.parts[0].subs.take();

            node.parts.remove(0);
            node.parts[0].ident.text = format!("{}.{}", self.comp, node.parts[0].ident.text);

            // Move subscripts to the new first part if it doesn't already have subscripts
            if node.parts[0].subs.is_none() {
                node.parts[0].subs = first_part_subs;
            }
        }
    }
}
