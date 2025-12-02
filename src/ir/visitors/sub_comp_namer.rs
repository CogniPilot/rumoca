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
//!
//! # Example
//! Given a component reference like `comp.subcomp`, if `comp` is set to `"comp"`,
//! the visitor will transform it into `comp.subcomp` (flattened with dot separator).
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
}

impl MutVisitor for SubCompNamer {
    fn exit_component_reference(&mut self, node: &mut ir::ast::ComponentReference) {
        // Only transform if there are at least 2 parts (e.g., comp.subcomp)
        // A single-part reference doesn't need transformation
        if node.parts.len() >= 2 && node.parts[0].ident.text == self.comp {
            node.parts.remove(0);
            node.parts[0].ident.text = format!("{}.{}", self.comp, node.parts[0].ident.text);
        }
    }
}
