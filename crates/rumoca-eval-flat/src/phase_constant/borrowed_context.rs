//! Borrow stable Flatten inventories for the shared constant interpreter.

use std::borrow::Cow;

use rumoca_core::Function;
use rustc_hash::FxHashMap;

use crate::constant::{DeferredParameterSource, EvalContext, EvalEnvironment, Value};
use crate::translation_reads::ResourceRoots;

use super::ParamEvalContext;
use super::enum_identity::EnumCanonicalizer;

pub(super) struct BorrowedContext<'a> {
    parameters: ParamEvalContext<'a>,
    pub(super) literals: EvalContext,
    functions: FxHashMap<&'a str, &'a Function>,
    /// The enumeration identities of the borrowed inventory, resolved per read
    /// rather than copied into `literals` for every evaluator.
    pub(super) canonicalizer: EnumCanonicalizer,
}

impl<'a> BorrowedContext<'a> {
    pub(super) fn new(parameters: &ParamEvalContext<'a>) -> Self {
        let mut functions = FxHashMap::default();
        for function in parameters.functions.values() {
            functions.insert(function.name.as_str(), function);
            functions
                .entry(function.name.last_segment())
                .or_insert(function);
        }
        Self {
            parameters: *parameters,
            literals: EvalContext::new(),
            functions,
            canonicalizer: EnumCanonicalizer::new(parameters.known_enums),
        }
    }

    pub(super) fn contains_parameter(&self, name: &str) -> bool {
        self.exact_value(name).is_some()
    }

    fn exact_value(&self, name: &str) -> Option<Cow<'_, Value>> {
        if let Some(value) = self.literals.parameters.get(name) {
            return Some(Cow::Borrowed(value));
        }
        // An enumeration parameter, then the canonical path of a literal one
        // holds, precede the scalar inventories, as their eager copies did.
        if let Some(identity) = self
            .parameters
            .known_enums
            .get(name)
            .and_then(|literal| self.canonicalizer.canonicalize(literal))
            .or_else(|| self.canonicalizer.held_canonical(name))
        {
            return Some(Cow::Owned(identity.to_value()));
        }
        // Preserve the typed inventory's insertion precedence without copying
        // entries into a second map. Only the selected scalar becomes a Value.
        let value = if let Some(value) = self.parameters.known_bools.get(name) {
            Value::Bool(*value)
        } else if let Some(value) = self.parameters.known_reals.get(name) {
            Value::Real(*value)
        } else if let Some(value) = self.parameters.known_ints.get(name) {
            Value::Integer(*value)
        } else {
            return self.parameters.known_values.get(name).map(Cow::Borrowed);
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

    fn translation_resources(&self) -> Option<&ResourceRoots> {
        self.parameters.resources
    }
}
