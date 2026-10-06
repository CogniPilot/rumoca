//! Evaluation of constants through their declaration identity (MLS 3.7 section
//! 5.3); the index itself is [`rumoca_ir_ast::DeclaredConstants`].

use rumoca_ir_ast::{ComponentReference, DeclaredConstants, Expression};

/// Cycle guard for evaluating constant alias chains: a constant that is being
/// evaluated is never entered again.
#[derive(Debug, Default)]
pub struct ConstantWalk {
    active: std::cell::RefCell<Vec<rumoca_core::DefId>>,
}

impl ConstantWalk {
    /// Run `eval` on the binding of the constant `reference` names, unless that
    /// constant is already being evaluated (a cyclic declaration has no value).
    pub fn enter<T>(
        &self,
        constants: &DeclaredConstants,
        reference: &ComponentReference,
        eval: impl FnOnce(&Expression) -> Option<T>,
    ) -> Option<T> {
        let def_id = constants.constant_id(reference)?;
        if self.active.borrow().contains(&def_id) {
            return None;
        }
        self.active.borrow_mut().push(def_id);
        let value = constants.binding_of(reference).and_then(eval);
        self.active.borrow_mut().pop();
        value
    }
}
