//! Evaluate a staging nest's loop-invariant guard once instead of once per
//! element.
//!
//! # The shape this exists for
//!
//! An array actual that is not a whole-aggregate copy is staged element by
//! element ([`super::expression_functions::ExpressionLowerer::materialize_tensor_function_argument`]).
//! When the element value came from a conditional, every element carries the
//! same test:
//!
//! ```text
//! for i in 1:16 loop
//!   for j in 1:3 loop
//!     if planAccepted and nominalSpeed > 0.0 and minSegmentDuration > 0.0 then
//!       t := result[i, j];
//!     else
//!       t := previous[i, j];
//!     end if;
//!     argument[i, j] := t;
//!   end for;
//! end for;
//! ```
//!
//! The condition mentions no iterator, so it computes the same bits 48 times.
//! Binding it to a Boolean once ahead of the nest leaves one evaluation and 48
//! loads. On `Planning.Bezier.WaypointTrajectoryPlanner` twelve such nests
//! account for 269 evaluations of one guard per tick, at about nineteen
//! Cortex-M7 instructions each.
//!
//! [`super::user_functions::fuse_guarded_tensor_loop`] does not reach this
//! case: it PAIRS an already-hoisted prefix guard with a matching body guard,
//! and here there is no prefix guard to pair with, because the conditional's
//! arms are entirely index-dependent and nothing was hoisted.
//!
//! # Why this is sound
//!
//! A GALEC Real comparison signals `NAN` on a qNaN operand, so this guard is
//! not a pure test: it writes `ErrorSignalStatus`. Changing how often it runs
//! is therefore changing an eFMI-visible output unless SPEC_0034 GAL-040 says
//! otherwise, and GAL-040 says so only under conditions. [`permission`] carries
//! each of them, and the token it mints is the only way to reach the rewrite.
//!
//! Written out, with `S` the bit set one evaluation raises:
//!
//! - the operands are loop-invariant, so every iteration would raise the same
//!   `S`;
//! - accumulation is `|=` with a constant mask, so after the first evaluation
//!   the word already contains `S` and every later evaluation is a no-op on it;
//! - nothing between the binding and the first iteration observes the word, so
//!   moving the first evaluation from just inside the loop to just outside it
//!   is invisible; and
//! - the trip count is at least one, so the hoisted evaluation happens on
//!   exactly the executions where the original happened at least once. This is
//!   the clause that would fail for a possibly-empty loop, where hoisting would
//!   raise `S` on an execution that never raised it.
//!
//! The final per-method word is therefore bit-identical, not merely equivalent.
//! The values are untouched: the same bits select the same arm, and the arms
//! are unchanged.

use rumoca_core::Span;
use rumoca_ir_galec::ast as gast;

use super::user_functions::collect_defined_names;

/// What one accepted rewrite adds ahead of the nest.
pub(super) struct BoundGuard {
    /// The Boolean local the condition now lives in.
    pub(super) declaration: gast::VariableDeclaration,
    /// `<local> := <condition>;`, to be emitted immediately before the nest.
    pub(super) binding: gast::Spanned<gast::Statement>,
}

/// Bind every hoistable branch condition of `body`'s top-level guards to a
/// Boolean local, rewriting the guard in place to test that local.
///
/// `body` is the innermost element body of a staging nest, `iterators` are the
/// nest's loop variables and `extents` their literal bounds, one per iterator.
/// `next_temporary` is the caller's temporary counter; it is bumped once per
/// bound condition so the names stay injective with the rest of the namespace.
///
/// Returns the bindings in emission order. An empty result means nothing was
/// provable, which is the default: declining costs a few instructions per
/// element, a wrong rewrite changes an eFMI-visible status word.
pub(super) fn bind_invariant_guards(
    body: &mut [gast::Spanned<gast::Statement>],
    iterators: &[gast::Name],
    extents: &[u32],
    namespace: &str,
    next_temporary: &mut usize,
    span: Span,
) -> Vec<BoundGuard> {
    if !every_axis_runs_at_least_once(extents) {
        return Vec::new();
    }
    // One refusal anywhere in the body refuses the whole nest: a construct this
    // pass cannot bound may consume signals, and consuming is what makes the
    // count observable.
    if !body.iter().all(is_signal_repeatable) {
        return Vec::new();
    }
    let defined = defined_names(body);
    let mut bound = Vec::new();
    for statement in body {
        let gast::Statement::If(guard) = &mut statement.node else {
            continue;
        };
        for branch in &mut guard.branches {
            let Some(permission) =
                permission::HoistableGuard::prove(&mut branch.condition, iterators, &defined)
            else {
                continue;
            };
            bound.push(bind(branch, permission, namespace, next_temporary, span));
        }
    }
    bound
}

/// Move one proven condition into a fresh Boolean local.
fn bind(
    branch: &mut gast::IfBranch,
    permission: permission::HoistableGuard,
    namespace: &str,
    next_temporary: &mut usize,
    span: Span,
) -> BoundGuard {
    let name = gast::Name::ident(format!("rumoca_{namespace}_guard_{next_temporary}"));
    *next_temporary += 1;
    let condition = permission.bind(branch, &name);
    BoundGuard {
        declaration: gast::VariableDeclaration {
            ty: gast::TypeRef::Primitive(gast::ScalarType::Boolean),
            name: name.clone(),
            dimensions: Vec::new(),
            range: gast::RangeAttributes::default(),
            span,
        },
        binding: gast::Spanned::new(
            gast::Statement::Assignment {
                target: gast::Reference::local(name),
                value: condition,
            },
            span,
        ),
    }
}

/// Whether every axis of the nest runs at least once.
///
/// Staging nests are built as `for i in 1:extent loop`, so an extent of zero is
/// the only way the body can be skipped. GAL-020 already requires literal
/// dimensions of at least one, but the hoist's soundness rests on this, so it is
/// checked here rather than inherited: a proof that reads its premise from
/// another rule's prose is not a proof.
fn every_axis_runs_at_least_once(extents: &[u32]) -> bool {
    !extents.is_empty() && extents.iter().all(|&extent| extent >= 1)
}

/// Whether a statement's effect on `ErrorSignalStatus` survives being reordered
/// around, per SPEC_0034 GAL-040.
///
/// `Consume` is the construct that breaks the contract; `Opaque` is a user call,
/// whose body is not in scope here and may contain one.
fn is_signal_repeatable(statement: &gast::Spanned<gast::Statement>) -> bool {
    rumoca_ir_galec::signal_effect::statement_effect(&statement.node).is_repeatable()
}

/// Every local the body assigns, at any depth.
fn defined_names(body: &[gast::Spanned<gast::Statement>]) -> Vec<gast::Name> {
    let mut names = Vec::new();
    for statement in body {
        collect_defined_names(statement, &mut names);
    }
    names
}

/// The permission to evaluate one branch condition ahead of its loop.
///
/// The token's field is private to this module and the only constructor is
/// [`HoistableGuard::prove`], so the rewrite cannot run without the proof
/// (`dev/2026-08-26-compiler-architecture-decisions.md` D2). It carries the
/// [`RepeatableSignalEffect`] it was minted from rather than re-deriving it, so
/// the signal-channel obligation is visible in the type rather than remembered.
mod permission {
    use rumoca_ir_galec::ast as gast;
    use rumoca_ir_galec::signal_effect::RepeatableSignalEffect;

    use super::super::expression_helpers::any_expression;
    use super::super::user_functions::expression_depends_on;

    /// Evidence that one branch condition may be evaluated once ahead of the
    /// loop nest whose body holds it.
    ///
    /// The token owns the expression it was minted against rather than pointing
    /// back at the branch, which is what makes [`HoistableGuard::bind`] total:
    /// there is no second look at a condition that might by then be something
    /// else, so no "this cannot happen" to assert.
    pub(super) struct HoistableGuard {
        /// The GAL-040 half of the proof: repeating or not repeating this
        /// condition leaves the final status word bit-identical.
        #[expect(
            dead_code,
            reason = "held as evidence: the obligation is that the token exists, \
                      not that anything reads it back"
        )]
        signal: RepeatableSignalEffect,
        /// The proven condition, moved out of the branch by `prove`.
        expression: gast::Expression,
    }

    impl HoistableGuard {
        /// Discharge every obligation, or decline.
        ///
        /// `defined` is every local the loop body assigns, which is what makes
        /// "invariant" mean invariant across iterations rather than merely
        /// free of iterator mentions.
        pub(super) fn prove(
            condition: &mut gast::Condition,
            iterators: &[gast::Name],
            defined: &[gast::Name],
        ) -> Option<Self> {
            // A signal check is refused here as well as by the token below: it
            // is not an expression, so there would be nothing to bind, and
            // catching is exactly what GAL-040 excludes.
            let gast::Condition::Expression(expression) = condition else {
                return None;
            };
            // Binding a bare reference or literal costs a store and a load and
            // saves nothing, so the rewrite is not taken where it cannot pay.
            if !is_computed(expression) {
                return None;
            }
            if expression_depends_on(expression, iterators) {
                return None;
            }
            if expression_depends_on(expression, defined) {
                return None;
            }
            // A call could write state the condition itself reads, or the loop
            // body reads, and deciding that needs a resolved callee this pass
            // does not have. The one guard shape this pass exists for holds
            // comparisons and Boolean operators, never a call.
            if any_expression(expression, &mut |node| {
                matches!(node, gast::Expression::Call(_)).then_some(true)
            }) {
                return None;
            }
            let signal = RepeatableSignalEffect::prove(expression)?;
            // Every obligation has closed, so the condition is taken out of the
            // branch here and the branch is left testing nothing until `bind`
            // names the local. `prove` is the only writer of that gap and `bind`
            // is the only way to close it, because it is the only public method
            // the token has.
            let expression = std::mem::replace(expression, gast::Expression::Bool(false));
            Some(Self { signal, expression })
        }

        /// Point `branch` at `name` and hand back the expression that must be
        /// assigned to it.
        ///
        /// Consuming the token is the point: a permission is single-use
        /// evidence about one condition, and once the condition has moved into
        /// a local there is nothing left to permit.
        pub(super) fn bind(
            self,
            branch: &mut gast::IfBranch,
            name: &gast::Name,
        ) -> gast::Expression {
            branch.condition = gast::Condition::Expression(gast::Expression::Ref(
                gast::Reference::local(name.clone()),
            ));
            self.expression
        }
    }

    /// Whether an expression computes something, rather than naming a value the
    /// emitted C could read just as cheaply twice.
    fn is_computed(expression: &gast::Expression) -> bool {
        !matches!(
            expression,
            gast::Expression::Bool(_)
                | gast::Expression::Integer(_)
                | gast::Expression::Real(_)
                | gast::Expression::Ref(_)
                | gast::Expression::Neg(_)
        )
    }
}

#[cfg(test)]
mod tests;
