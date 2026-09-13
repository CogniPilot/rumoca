//! Borrow stable Flatten inventories for the shared constant interpreter.

use std::borrow::Cow;

use rumoca_core::Function;
use rustc_hash::FxHashMap;

use crate::constant::{DeferredParameterSource, EvalContext, EvalEnvironment, Value};

use super::ParamEvalContext;
use super::enum_identity::EnumCanonicalizer;

pub(super) struct BorrowedContext<'a> {
    parameters: ParamEvalContext<'a>,
    pub(super) literals: EvalContext,
    functions: FxHashMap<&'a str, &'a Function>,
}

impl<'a> BorrowedContext<'a> {
    pub(super) fn new(
        parameters: &ParamEvalContext<'a>,
        canonicalizer: &EnumCanonicalizer,
    ) -> Self {
        let mut literals = EvalContext::new();
        for (name, literal) in parameters.known_enums {
            if let Some(identity) = canonicalizer.canonicalize(literal) {
                let value = identity.to_value();
                literals.add_parameter(name.clone(), value.clone());
                literals.add_parameter(identity.to_flat_string(), value);
            }
        }
        let mut functions = FxHashMap::default();
        for function in parameters.functions.values() {
            functions.insert(function.name.as_str(), function);
            functions
                .entry(function.name.last_segment())
                .or_insert(function);
        }
        Self {
            parameters: *parameters,
            literals,
            functions,
        }
    }

    pub(super) fn contains_parameter(&self, name: &str) -> bool {
        self.exact_value(name).is_some()
    }

    fn exact_value(&self, name: &str) -> Option<Cow<'_, Value>> {
        if let Some(value) = self.literals.parameters.get(name) {
            return Some(Cow::Borrowed(value));
        }
        // Preserve the typed inventory's insertion precedence without copying
        // entries into a second map. Only the selected scalar becomes a Value.
        let value = if let Some(value) = self.parameters.known_bools.get(name) {
            Value::Bool(*value)
        } else if let Some(value) = self.parameters.known_reals.get(name) {
            Value::Real(*value)
        } else {
            Value::Integer(*self.parameters.known_ints.get(name)?)
        };
        Some(Cow::Owned(value))
    }
}

impl EvalEnvironment for BorrowedContext<'_> {
    fn get_value(&self, name: &str) -> Option<Cow<'_, Value>> {
        self.literals
            .lookup(name, |candidate| self.exact_value(candidate))
    }

    fn get_enum(&self, name: &str) -> Option<&(String, String)> {
        self.literals.get_enum(name)
    }

    fn get_function(&self, name: &str) -> Option<&Function> {
        self.functions.get(name).copied()
    }

    fn get_array_dimensions(&self, name: &str) -> Option<&[i64]> {
        self.literals.lookup(name, |candidate| {
            self.parameters.array_dims.get(candidate).map(Vec::as_slice)
        })
    }

    fn deferred_parameter(&self, name: &str) -> Option<DeferredParameterSource> {
        self.literals.deferred_parameter(name)
    }
}
