//! Identities of compiler-introduced locals, scoped to one planning run.
//!
//! Source declarations keep their Resolve-issued `DefId`. These locals have
//! a separate identity domain and cannot be reconstructed from display text.

use std::{collections::HashMap, marker::PhantomData};

use crate::{DefId, Function, FunctionInstanceId, SourceMap, Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct FunctionOwner {
    declaration: DefId,
    instance: FunctionInstanceId,
}

/// An opaque local identity. Its invariant brand is unique to one catalog run.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GeneratedFunctionLocalKey<'locals> {
    owner: FunctionOwner,
    ordinal: u32,
    brand: PhantomData<&'locals mut &'locals ()>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GeneratedLocalError {
    MissingFunctionDeclaration,
    MissingFunctionInstance,
    MissingProvenance,
    OrdinalOverflow,
    UnknownLocal,
}

/// A scalar Boolean declaration. Reservation does not initialize its value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GeneratedBooleanLocal<'locals> {
    key: GeneratedFunctionLocalKey<'locals>,
    provenance: Span,
}

impl<'locals> GeneratedBooleanLocal<'locals> {
    pub const fn key(self) -> GeneratedFunctionLocalKey<'locals> {
        self.key
    }

    pub const fn provenance(self) -> Span {
        self.provenance
    }
}

/// A phase-wide catalog. Planning and consumption stay inside `construct`.
/// Function specializations map these declarations to their own runtime IDs.
///
/// A key cannot escape the run, even if both runs use the same function IDs:
///
/// ```compile_fail
/// use rumoca_core::{GeneratedFunctionLocalCatalog, Function, SourceMap, Span};
/// fn escape(function: &Function, sources: &SourceMap, at: Span) {
///     let key = GeneratedFunctionLocalCatalog::construct(sources, |catalog| {
///         catalog.function(function).unwrap().reserve_boolean(at).unwrap().key()
///     });
/// }
/// ```
///
/// Nor can a key from one run enter a nested, independently branded catalog:
///
/// ```compile_fail
/// use rumoca_core::{GeneratedFunctionLocalCatalog, Function, SourceMap, Span};
/// fn cross_run(function: &Function, sources: &SourceMap, at: Span) {
///     GeneratedFunctionLocalCatalog::construct(sources, |first| {
///         let key = first.function(function).unwrap().reserve_boolean(at).unwrap().key();
///         GeneratedFunctionLocalCatalog::construct(sources, |second| {
///             second.function(function).unwrap().declaration(key).unwrap();
///         });
///     });
/// }
/// ```
pub struct GeneratedFunctionLocalCatalog<'locals, 'source> {
    sources: &'source SourceMap,
    functions: HashMap<FunctionOwner, Vec<GeneratedBooleanLocal<'locals>>>,
    brand: PhantomData<&'locals mut &'locals ()>,
}

impl<'source> GeneratedFunctionLocalCatalog<'_, 'source> {
    pub fn construct<R>(
        sources: &'source SourceMap,
        build: impl for<'locals> FnOnce(&mut GeneratedFunctionLocalCatalog<'locals, 'source>) -> R,
    ) -> R {
        build(&mut GeneratedFunctionLocalCatalog {
            sources,
            functions: HashMap::new(),
            brand: PhantomData,
        })
    }
}

impl<'locals> GeneratedFunctionLocalCatalog<'locals, '_> {
    pub fn function<'borrow>(
        &'borrow mut self,
        function: &Function,
    ) -> Result<GeneratedFunctionLocals<'borrow, 'locals>, GeneratedLocalError> {
        let declaration = function
            .def_id
            .filter(|id| id.index() != 0)
            .ok_or(GeneratedLocalError::MissingFunctionDeclaration)?;
        let instance = function
            .instance_id
            .ok_or(GeneratedLocalError::MissingFunctionInstance)?;
        let owner = FunctionOwner {
            declaration,
            instance,
        };
        Ok(GeneratedFunctionLocals {
            owner,
            sources: self.sources,
            locals: self.functions.entry(owner).or_default(),
        })
    }
}

/// A temporary borrow of one function's declarations, sharing the phase brand.
pub struct GeneratedFunctionLocals<'borrow, 'locals> {
    owner: FunctionOwner,
    sources: &'borrow SourceMap,
    locals: &'borrow mut Vec<GeneratedBooleanLocal<'locals>>,
}

impl<'locals> GeneratedFunctionLocals<'_, 'locals> {
    pub fn reserve_boolean(
        &mut self,
        provenance: Span,
    ) -> Result<GeneratedBooleanLocal<'locals>, GeneratedLocalError> {
        let source = self.sources.get_source(provenance.source);
        if provenance.is_dummy()
            || provenance.start.0 > provenance.end.0
            || source.is_none_or(|(_, text)| provenance.end.0 > text.len())
        {
            return Err(GeneratedLocalError::MissingProvenance);
        }
        let ordinal =
            u32::try_from(self.locals.len()).map_err(|_| GeneratedLocalError::OrdinalOverflow)?;
        let local = GeneratedBooleanLocal {
            key: GeneratedFunctionLocalKey {
                owner: self.owner,
                ordinal,
                brand: PhantomData,
            },
            provenance,
        };
        self.locals.push(local);
        Ok(local)
    }

    pub fn declaration(
        &self,
        key: GeneratedFunctionLocalKey<'locals>,
    ) -> Result<GeneratedBooleanLocal<'locals>, GeneratedLocalError> {
        self.locals
            .get(key.ordinal as usize)
            .copied()
            .filter(|local| local.key == key)
            .ok_or(GeneratedLocalError::UnknownLocal)
    }

    /// Allocation order is stable and belongs to this resolved function owner.
    pub fn declarations(&self) -> &[GeneratedBooleanLocal<'locals>] {
        self.locals
    }
}

#[cfg(test)]
mod tests;
