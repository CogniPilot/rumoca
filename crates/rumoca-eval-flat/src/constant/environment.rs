//! Read-only inventories consumed by the shared constant interpreter.

use std::borrow::Cow;

use super::{DeferredParameterSource, EvalContext, Function, Value};
use crate::translation_reads::ResourceRoots;

/// Values and definitions available to constant expression evaluation.
///
/// Storage may be owned or borrowed. Expression dispatch, operators, function
/// execution, and diagnostics remain in the constant interpreter.
pub trait EvalEnvironment {
    /// Look up a value using the inventory's current lexical scope.
    fn get_value(&self, name: &str) -> Option<Cow<'_, Value>>;
    /// Whether a declaration has a lexical binding, even without a settled value.
    fn declaration_is_lexical(&self, _id: rumoca_core::DefId) -> bool {
        false
    }
    /// Read a lexical source iterator by its Resolve-issued declaration identity.
    fn get_declaration_value(&self, _id: rumoca_core::DefId) -> Option<Cow<'_, Value>> {
        None
    }
    fn get_reference_value(&self, reference: &rumoca_core::Reference) -> Option<Cow<'_, Value>> {
        if let Some(id) = reference.target_def_id()
            && self.declaration_is_lexical(id)
        {
            self.get_declaration_value(id)
        } else {
            self.get_value(reference.as_str())
        }
    }
    /// Look up an explicitly registered enumeration literal.
    fn get_enum(&self, name: &str) -> Option<&(String, String)>;
    /// Find a function definition under a registered call name.
    fn get_function(&self, name: &str) -> Option<&Function>;
    /// Read shape metadata without constructing array elements.
    fn get_array_dimensions(&self, name: &str) -> Option<&[i64]>;
    /// Identify declared parameters whose values await initialization.
    fn deferred_parameter(&self, name: &str) -> Option<DeferredParameterSource>;
    /// The resource roots of a translation that evaluates parameter bindings,
    /// which alone admits the cataloged foreign file readers (SPEC_0040
    /// FLAT-C06). Any other evaluation leaves their calls unevaluated.
    fn translation_resources(&self) -> Option<&ResourceRoots> {
        None
    }
}

impl EvalEnvironment for EvalContext {
    fn get_value(&self, name: &str) -> Option<Cow<'_, Value>> {
        self.get(name).map(Cow::Borrowed)
    }

    fn get_enum(&self, name: &str) -> Option<&(String, String)> {
        self.get_enum(name)
    }

    fn get_function(&self, name: &str) -> Option<&Function> {
        self.functions.get(name)
    }

    fn get_array_dimensions(&self, name: &str) -> Option<&[i64]> {
        self.get_array_dimensions(name)
    }

    fn deferred_parameter(&self, name: &str) -> Option<DeferredParameterSource> {
        self.deferred_parameter(name)
    }
}
