//! Whether one call site keeps its call, and what substituting it costs the
//! artifact's traceability.
//!
//! Every decision here is a DECLINE-by-default decision. The GALEC ABI can
//! represent the calls this module is asked about, so emitting the call is
//! always legal and always available; substituting the body is the option that
//! has to earn itself. That is what makes the default emission provably a
//! no-op, and what makes every refusal below a missed optimization rather than
//! a rejected model.
//!
//! # RELOCATION DEBT: this is not where the transform belongs
//!
//! An eFMI container ships two representations of one model, and only one of
//! them is meant to be optimized: `AlgorithmCode` is the reviewable semantic
//! reference and `ProductionCode` is its optimized C rendering. A transform
//! that runs during DAE-to-GALEC lowering reshapes the reference itself, which
//! is why a container target refuses a non-default policy outright
//! (`target_manifest::admitted_emission_policy`) and only the container-free
//! `embedded-c-galec` export can use one.
//!
//! The end-state home is the GALEC-to-Solve refinement, where the two
//! representations can legitimately diverge: the `.alg` stays the structured
//! reference, and only the Solve program the C emitter consumes is optimized.
//! Moving this module there is owed work, not a settled location, and the
//! refusal above is the seam that keeps the interim placement honest instead of
//! letting it quietly become the design.

use super::*;

/// How many distinct call sites each checked function has.
///
/// Counted once per projection over the whole expression arena and keyed by the
/// construction-issued call OWNER, never by the expression: one source-level
/// call to a four-result function reaches lowering as many scalar projections
/// of one owner, and counting projections would make every multi-result call
/// look duplicated when it is not.
///
/// The census covers the entire DAE, not one block, which is the conservative
/// direction: a callee with one owner anywhere has at most one owner here.
pub(super) struct CallSiteCensus {
    owners: std::collections::BTreeMap<u32, std::collections::BTreeSet<u32>>,
}

/// The census a lowering that was never handed the projection's own has.
///
/// It reports no call sites for anything, so every cost-model question answers
/// "not a single-site callee" and the lowering decides nothing. That is the
/// right answer for a unit-test construction that builds a lowerer directly:
/// deciding from facts it does not have is the failure mode to design out.
static NO_CALL_SITES: CallSiteCensus = CallSiteCensus {
    owners: std::collections::BTreeMap::new(),
};

impl CallSiteCensus {
    pub(super) fn derive(view: dae::DaeView<'_>) -> Self {
        let mut owners: std::collections::BTreeMap<u32, std::collections::BTreeSet<u32>> =
            std::collections::BTreeMap::new();
        for index in 0..view.expression_count() {
            let Some(node) = view.expression_id(index).and_then(|id| view.expression(id)) else {
                continue;
            };
            if let dae::ExpressionOperation::Call {
                function, owner, ..
            } = node.operation()
            {
                owners
                    .entry(function.index())
                    .or_default()
                    .insert(owner.index());
            }
        }
        Self { owners }
    }

    /// Whether this callee is called from exactly one place.
    ///
    /// A callee with one call site is the one population that needs no cost
    /// model at all. Substituting it duplicates nothing, because there is
    /// nothing to duplicate: the body moves rather than being copied. What it
    /// removes is unconditional, namely the argument staging and result
    /// read-back at that boundary, and one edge of the call chain that the
    /// storage overlay refuses to let a caller and a callee share.
    ///
    /// The rule is therefore not a heuristic and has no threshold to tune. It
    /// is the statement that a boundary crossed once is a boundary that buys
    /// nothing.
    fn is_single_site(&self, function: u32) -> bool {
        self.owners
            .get(&function)
            .is_some_and(|owners| owners.len() == 1)
    }
}

/// The emission decisions and the facts they are decided from, as one value.
///
/// The policy alone is not enough to decide a call site: the cost model needs
/// the call-site census, and a lowering that held one without the other could
/// not answer. They travel together for the same reason the block facts do.
#[derive(Clone, Copy)]
pub(super) struct EmissionFacts<'a> {
    pub(super) policy: EmissionPolicy,
    pub(super) call_sites: &'a CallSiteCensus,
}

impl EmissionFacts<'_> {
    /// The facts a lowering starts from before the projection hands it its own:
    /// keep every call, and know nothing about call sites.
    pub(super) const fn structured() -> Self {
        Self {
            policy: EmissionPolicy::reviewable(),
            call_sites: &NO_CALL_SITES,
        }
    }
}

impl<'a, 'dae> ExpressionLowerer<'a, 'dae> {
    /// Whether the policy, the model, legality and termination all admit
    /// substituting this callee's body at this site.
    ///
    /// The order of authority is the whole surface a model author sees:
    ///
    /// 1. `annotation(Inline = false)` refuses, at every setting. A refusal a
    ///    compiler flag can override is not a refusal.
    /// 2. Legality: substituting must not re-enter a body already being
    ///    expanded, and must not scalarize a tensor the scalarization axis has
    ///    declined to expand.
    /// 3. The inline policy decides the rest: `none` substitutes nothing,
    ///    `annotated` honours what the model asked for, `cost-model` adds the
    ///    sites its rules pay for, and `all` takes everything left legal.
    ///
    /// Termination is not a preference: a callee already being expanded at this
    /// site would expand forever, so it is refused here rather than left to
    /// fail deeper in. Refusing means the call is emitted as a call, which is
    /// what a recursive Modelica function has to become anyway.
    pub(super) fn admits_inline(&self, function: dae::FunctionId<'dae>) -> bool {
        if matches!(self.emission.policy.inline, InlinePolicy::None) {
            return false;
        }
        // Declining is free (D2), so an identity this view cannot resolve is a
        // reason to emit the call, never a reason to assert.
        let Some(function_view) = self.view.function(function) else {
            return false;
        };
        let requested = function_view.inline();
        if requested == rumoca_core::InlineAnnotation::Never
            || self.expansion_reenters(function)
            || !self.substitution_preserves_tensors(function_view)
        {
            return false;
        }
        match self.emission.policy.inline {
            InlinePolicy::None => false,
            InlinePolicy::Annotated => requested == rumoca_core::InlineAnnotation::Requested,
            // The one cost-model rule that needs no tuning: a callee called
            // from exactly one place has no duplication to pay for.
            InlinePolicy::CostModel => {
                requested == rumoca_core::InlineAnnotation::Requested
                    || self.emission.call_sites.is_single_site(function.index())
            }
            InlinePolicy::All => true,
        }
    }

    /// Whether substituting this callee leaves its tensor operations tensors.
    ///
    /// This projection substitutes EXPRESSIONS, not statements: the caller asks
    /// for one scalar of one result at a time, and the callee's defining
    /// expression is lowered for each such request. For a scalar result that is
    /// one evaluation, exactly what the call did. For an AGGREGATE result it is
    /// one evaluation per element, which is not inlining at all: it is
    /// scalarization, and it destroys the index sets, symmetry and tensor
    /// identity the result carried. The scalarization axis declines that by
    /// default, so this call site declines with it.
    ///
    /// The cost is measured, not assumed: substituting aggregate results took
    /// the RDD2 trajectory planner's emitted translation unit from 4,033 lines
    /// to 136,464, and its compile from one second to thirty-one.
    ///
    /// This is also the exact reason inlining alone does not yet close the
    /// working-memory gap against a flat generator. The chain that holds the
    /// estimator's residual scratch live is a chain of callees returning
    /// matrices, and every one of them is refused here. Collapsing them needs
    /// an inliner that splices the callee's STATEMENTS, loop nests included,
    /// into the caller, which keeps the tensors AND removes the boundary. That
    /// mechanism does not exist yet; refusing is how its absence stays a missed
    /// optimization instead of becoming an emitted defect.
    fn substitution_preserves_tensors(&self, function: dae::FunctionView<'dae>) -> bool {
        if !self.emission.policy.is_certifiable() {
            // A caller that asked for expansion has accepted it; nothing here
            // has to refuse on the tensors' behalf. Unreachable today, because
            // the projection refuses a non-default scalarization axis outright.
            return true;
        }
        function.result_types().iter().all(|result| {
            self.view
                .value_type(result)
                .is_some_and(|ty| ty.dimensions().is_empty() && !ty.is_record())
        })
    }

    /// Whether substituting `function` here would re-enter a body already being
    /// expanded, directly or through a cycle of callees.
    ///
    /// The body being lowered counts: a function that calls itself reaches this
    /// with an empty frame stack and its own scope set.
    fn expansion_reenters(&self, function: dae::FunctionId<'dae>) -> bool {
        self.function_scope == Some(function)
            || self
                .call_frames
                .iter()
                .any(|frame| frame.function == function)
    }

    /// Lower one call by substituting the callee's body, or `None` when that
    /// lowering declines.
    ///
    /// The attempt runs on a clone, which is what makes declining free: a body
    /// that cannot be substituted leaves behind no temporary, no cached value,
    /// no emitted statement and no recorded call, and the caller falls back to
    /// the call form the ABI can always represent. Committing is assigning the
    /// clone back, so a successful substitution keeps everything it built.
    pub(super) fn try_inline_call(
        &mut self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        span: Span,
    ) -> Option<TypedExpression> {
        let mut speculative = self.clone();
        let result = speculative
            .enter_function_call(call, function, output, arguments, indices, span)
            .ok()?;
        if let Some(frame) = speculative.call_frames.last_mut() {
            frame.substituted_by_policy = true;
        }
        let lowered = speculative
            .lower_substituted_result(function, output, indices, result)
            .ok()?;
        speculative.call_frames.pop();
        *self = speculative;
        Some(lowered)
    }

    /// Name the local that holds one substituted function value.
    ///
    /// A call the emission policy deleted would have named its result
    /// temporaries `rumoca.tmp.<namespace>.<callee>.<result>.call<id>.result<n>`
    /// ([`ExpressionLowerer::append_materialized_result`]). Deleting the call
    /// must not delete that path, so a value inside a body the policy
    /// substituted is named after the same facts, plus the whole chain of
    /// callees it was substituted through and the callee value it realizes.
    /// This is the traceability obligation stated concretely: flattening
    /// extends the provenance path, it never collapses it to a counter.
    ///
    /// A body substituted because the GALEC ABI has no form for the call is a
    /// different case: no call form existed, so no names were lost, and those
    /// values keep the counter spelling they have always had.
    pub(super) fn substituted_value_name(
        &self,
        key: &MaterializedFunctionValueKey,
    ) -> Result<gast::Name, GalecTargetError> {
        let counter = self.temporary_counter;
        let Some(frame) = self
            .call_frames
            .last()
            .filter(|frame| frame.substituted_by_policy)
        else {
            return Ok(gast::Name::ident(format!(
                "rumoca_{}_value_{counter}",
                self.temporary_namespace
            )));
        };
        let chain = self
            .call_frames
            .iter()
            .map(|frame| self.function_label(frame.function.index()))
            .collect::<Vec<_>>()
            .join(".");
        // The call identity and the ordinal share ONE trailing segment, and
        // that is not cosmetic. A target's symbol allocator spells a dotted
        // name as its shortest unambiguous suffix, so a trailing `.value7`
        // would reach the emitted C as `value7` and the call site would be
        // gone from the identifier a reviewer actually reads. Keeping them
        // together makes the shortest suffix `call1751_value7`, which carries
        // the same facts the materialized form's `call1751_result3` does.
        crate::mangle::galec_variable_name(&format!(
            "rumoca.tmp.{}.{}.{}.call{}_value{counter}",
            self.temporary_namespace,
            chain,
            self.substituted_value_label(key),
            frame.call.index(),
        ))
    }

    /// The callee's own name for one function identity, or its index when the
    /// arena no longer resolves it.
    fn function_label(&self, function: u32) -> String {
        self.view
            .function_id(function as usize)
            .and_then(|id| self.view.function(id))
            .map_or_else(
                || format!("function{function}"),
                |function| function.name().to_string(),
            )
    }

    /// The callee-side name of the value a substituted local realizes.
    ///
    /// The definition's target is the function value the callee assigned, so
    /// this is the identifier a reader finds in the Modelica source at the span
    /// the emitted statement still carries.
    fn substituted_value_label(&self, key: &MaterializedFunctionValueKey) -> String {
        let fallback = || format!("definition{}", key.definition);
        let Some(function) = self
            .view
            .function_id(key.function as usize)
            .and_then(|id| self.view.function(id))
        else {
            return fallback();
        };
        function
            .definition_id(key.definition as usize)
            .and_then(|definition| self.view.function_definition(definition))
            .and_then(|definition| {
                function
                    .values()
                    .find(|value| value.id() == definition.target())
            })
            .map_or_else(fallback, |value| value.name().to_string())
    }

    /// Lower the selected output of a body being substituted.
    ///
    /// The output is lowered through its own function-value definition rather
    /// than through the bare right-hand side that defines it, and the
    /// difference is entirely traceability. Lowering the right-hand side folds
    /// the callee's arithmetic into whatever caller statement consumes it, so
    /// the emitted statement carries the CALL SITE's span and the callee's line
    /// stops being named by the artifact at all. Lowering the definition binds
    /// the value to a local of its own, spanned at the callee's line and named
    /// after the call chain, so the flattened code still says where each piece
    /// of arithmetic was written and which call produced it.
    ///
    /// A value the projection cannot bind (inside a conditional, or with a
    /// non-literal index) falls back to the right-hand side, which is the
    /// behaviour a substituted body has always had.
    fn lower_substituted_result(
        &mut self,
        function: dae::FunctionId<'dae>,
        output: u32,
        indices: &[gast::Expression],
        result: dae::ExprId<'dae>,
    ) -> Result<TypedExpression, GalecTargetError> {
        // Every `else` here falls back to lowering the right-hand side, which
        // is what a substituted body did before this binding existed: a worse
        // trace, never a wrong value.
        let (Some(function_view), Some(node)) =
            (self.view.function(function), self.view.expression(result))
        else {
            return self.lower_at(result, indices);
        };
        let Some(definition) = function_view.result_values().get(output as usize) else {
            return self.lower_at(result, indices);
        };
        let scalar = scalar_type(
            node.value_type().scalar_type(),
            function_view.name().as_str(),
            node.provenance().span(),
        )?;
        self.lower_function_value(definition, indices, scalar)
    }
}
