//! Discovery order of value-proven function specializations.
//!
//! A specialization's certificate (its shapes and settled input values) is
//! minted before its body is walked, so a caller never waits on its callee's
//! body: discovering that body only finds the calls it reaches. Bodies are
//! walked depth first, as a call nests, up to `INLINE_BODY_DEPTH` levels below
//! the point that started the walk; a deeper body waits in a pending list that
//! the walk's root drains before it returns. The root is a model-scope call, a
//! derivative target, or a run-time conditional arm that rolls back as a unit,
//! so every body a root reaches is discovered, and its errors raised, before
//! that root completes. Native stack use is bounded by `INLINE_BODY_DEPTH`
//! body walks (times the nesting of run-time arms), never by the length of a
//! value-keyed recursion chain, which `SPECIALIZATION_DEPTH_LIMIT` bounds on
//! its own.

use super::*;

/// Function bodies walked on the native stack below one root before deeper
/// bodies are queued. Shallow call trees keep depth-first discovery order.
const INLINE_BODY_DEPTH: usize = 8;

/// A minted specialization whose body is still to be walked.
pub(super) struct PendingBody {
    index: usize,
}

impl ShapeAnalyzer<'_> {
    pub(super) fn ensure_specialization(
        &mut self,
        key: FunctionSpecializationKey,
        call_span: Span,
    ) -> Result<usize, ToDaeError> {
        let caller = self.active_specializations.last().copied();
        if let Some(index) = self.analysis.certificate_by_key.get(&key).copied() {
            self.record_dependency(caller, index);
            return Ok(index);
        }
        let depth = caller.map_or(0, |caller| self.chain_depths[caller]);
        if depth >= SPECIALIZATION_DEPTH_LIMIT {
            return Err(ToDaeError::unsupported_flat(
                "function shape specialization",
                format!(
                    "`{}` needs more than {SPECIALIZATION_DEPTH_LIMIT} nested value-proven \
                     specializations, which is the bound this analysis admits; no activation in \
                     the chain repeated an earlier proven argument",
                    key.function
                ),
                call_span,
            ));
        }
        let function =
            self.flat.functions.get(&key.function).ok_or_else(|| {
                ToDaeError::unresolved_reference(key.function.as_str(), call_span)
            })?;
        let certificate = resolve_certificate(
            self.flat,
            function,
            key.clone(),
            call_span,
            &self.analysis.model_values,
        )?;
        let index = self.analysis.certificates.len();
        self.analysis.certificate_by_key.insert(key, index);
        self.analysis.certificates.push(certificate);
        self.analysis.dependencies.push(Vec::new());
        self.chain_depths.push(depth + 1);
        self.record_dependency(caller, index);

        let root = self.active_specializations.len() == self.inline_base;
        if self.active_specializations.len() - self.inline_base >= INLINE_BODY_DEPTH {
            self.pending_bodies.push(PendingBody { index });
            return Ok(index);
        }
        let mark = self.pending_bodies.len();
        self.discover_body(index)?;
        if root {
            self.drain_pending_bodies(mark)?;
        }
        Ok(index)
    }

    /// Walk every body queued since `mark`, each as the root of its own
    /// depth-first walk, until none remains.
    pub(super) fn drain_pending_bodies(&mut self, mark: usize) -> Result<(), ToDaeError> {
        while self.pending_bodies.len() > mark {
            let Some(PendingBody { index }) = self.pending_bodies.pop() else {
                break;
            };
            let base = std::mem::replace(&mut self.inline_base, self.active_specializations.len());
            let result = self.discover_body(index);
            self.inline_base = base;
            result?;
        }
        Ok(())
    }

    /// Discover the calls the body of specialization `index` reaches, with
    /// `index` as their caller.
    fn discover_body(&mut self, index: usize) -> Result<(), ToDaeError> {
        let flat = self.flat;
        let certificate = &self.analysis.certificates[index];
        let function = &flat.functions[&certificate.key.function];
        let values = certificate.values.clone();
        self.active_specializations.push(index);
        let result = self
            .discover_parameter_defaults(function, &values)
            .and_then(|()| self.discover_statements(&function.body, &values));
        let completed = self.active_specializations.pop();
        debug_assert_eq!(completed, Some(index));
        result
    }
}
