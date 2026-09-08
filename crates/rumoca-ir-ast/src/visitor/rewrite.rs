//! In-place expression rewriting with a single traversal authority.
//!
//! Traversal is owned by one private kernel in this module, driven through a
//! private, sealed client contract. Two clients exist, and only one of them
//! owns structural effects:
//!
//! - implementations of the public [`ExpressionTransformer`] trait supply the
//!   two semantic reference hooks and nothing else. They have no node hook,
//!   no prune, no replacement: the kernel always descends for them, so an
//!   ordinary semantic or reference transformer has no structural effect of
//!   any kind. The hooks receive a [`SemanticReferenceEditor`] whose write
//!   surface is per-part declaration identity and the qualified display
//!   spelling, and whose read surface is the opaque
//!   [`ComponentReferenceView`]. No hook input yields a structural node -
//!   `ComponentReference`, part, subscript, or expression - so a hook can
//!   neither rewrite structure nor clone the visited reference to re-enter a
//!   kernel entry on it: within safe code, duplicate traversal of the visited
//!   subtree is unrepresentable, not merely unobserved;
//! - loop-index substitution is the separate AST-owned named operation
//!   [`substitute_integer_loop_index`]. Its strategy is a private kernel
//!   client, and it alone owns integer replacement. Iterator shadowing is
//!   not a prune: the kernel walks a comprehension's ranges before any of
//!   its iterators bind, so the substituter replaces range occurrences of
//!   an outer index and suppresses only binder-owned occurrences (body and
//!   filter under a rebinding iterator).
//!
//! Iterator scoping is construct-specific, and the two MLS 3.7 rules are
//! REVERSED relative to each other, not merely different. For-equations and
//! for-statements nest first-textual-outer (§11.2.2.3 with §8.3.2): walk
//! index N's range, bind index N, with indices 1..N-1 visible to range N -
//! [`schedule_loop_iterators_mut`]. Array constructors and reductions nest
//! last-textual-outer (§10.4.1.2's expansion composed with §10.4.1's scope
//! sentence applied to the expanded form): walk and bind in reverse textual
//! order, so a first textual range may see a later textual iterator and not
//! the converse - [`schedule_comprehension_iterators_mut`], which this
//! kernel applies to `Expression::ArrayComprehension`. Each schedule is a
//! single loop handing out [`IteratorStep`]s, so a consumer cannot reorder
//! binds against ranges; which schedule applies at which construct is
//! written explicitly at each site, never selected implicitly.
//!
//! Within one entry through a public `*_in_place` function or the named
//! substitution, the kernel visits every child of every node it descends
//! into, in structural field order, exactly once. The public entries
//! themselves take `&mut` roots, so how often a whole tree is entered
//! remains the caller's authority; the kernel's guarantee is per entry, not
//! a global exactly-once over a program.

use crate::{ComponentReference, Expression, ForIndex, Subscript, TerminalType};
use rumoca_core::{DefId, Location, OpBinary, OpUnary, Span, Token};
use std::sync::Arc;

#[cfg(test)]
thread_local! {
    static COPY_ON_WRITE_EVENTS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static KERNEL_TRACE: std::cell::RefCell<Vec<KernelTraceEvent>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

#[cfg(test)]
pub(super) fn reset_copy_on_write_events() {
    COPY_ON_WRITE_EVENTS.set(0);
}

#[cfg(test)]
pub(super) fn copy_on_write_events() -> usize {
    COPY_ON_WRITE_EVENTS.get()
}

/// Test-only ordered record of one kernel event: a node entry keyed by its
/// span start, a reference or callee hook dispatch keyed by its dotted
/// spelling, or an iterator bind/unbind from the canonical schedule.
#[cfg(test)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum KernelTraceEvent {
    Node(usize),
    Cref(String, ComponentReferenceSite),
    Callee(String, CalleeSite),
    Bind(String),
    Unbind(String),
}

#[cfg(test)]
pub(super) fn reset_kernel_trace() {
    KERNEL_TRACE.with_borrow_mut(Vec::clear);
}

#[cfg(test)]
pub(super) fn take_kernel_trace() -> Vec<KernelTraceEvent> {
    KERNEL_TRACE.with_borrow_mut(std::mem::take)
}

#[cfg(test)]
fn record_kernel_event(event: KernelTraceEvent) {
    KERNEL_TRACE.with_borrow_mut(|events| events.push(event));
}

#[cfg(test)]
fn reference_trace_name(reference: &ComponentReference) -> String {
    reference
        .parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".")
}

/// Site role carried by [`ExpressionTransformer::transform_component_reference`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComponentReferenceSite {
    /// An `Expression::ComponentReference` in value position.
    Value,
    /// The target of an `Expression::ClassModification` or
    /// `Expression::Modification`.
    ModificationTarget,
    /// A reference owned by an equation or statement at a phase boundary,
    /// entered through [`transform_component_reference_in_place`].
    OwnedTarget,
}

/// Site role carried by [`ExpressionTransformer::transform_callee`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CalleeSite {
    /// The callee of an `Expression::FunctionCall`.
    ExpressionCall,
    /// The callee of an equation- or statement-level function call at a phase
    /// boundary, entered through [`transform_callee_in_place`].
    OwnedCall,
}

/// Borrow-scoped editor for the semantic layer of one component reference.
///
/// Created only by the kernel, after the reference's part subscripts have
/// been traversed. Its write surface is exactly the semantic layer: per-part
/// declaration identity through [`Self::part_identity_slots`], and the
/// qualified display spelling through
/// [`Self::set_qualified_display_name`]. Its read surface is the opaque
/// [`ComponentReferenceView`], which exposes semantic facts only. No editor
/// or view method yields a structural node - not `ComponentReference`, not a
/// part, not a subscript, not an expression - so a hook cannot rewrite,
/// clone, or re-enter the kernel on any piece of the visited reference:
///
/// ```compile_fail,E0308
/// use rumoca_ir_ast::{
///     ComponentReferenceSite, ExpressionTransformer, SemanticReferenceEditor,
///     transform_component_reference_in_place,
/// };
///
/// struct GuardedCloneMutant {
///     reentered: bool,
/// }
/// impl ExpressionTransformer for GuardedCloneMutant {
///     fn transform_component_reference(
///         &mut self,
///         reference: SemanticReferenceEditor<'_>,
///         _site: ComponentReferenceSite,
///     ) {
///         if !self.reentered {
///             self.reentered = true;
///             // The view clones as a view of borrowed facts, never as the
///             // structural reference a re-entry would need.
///             let mut duplicate: rumoca_ir_ast::ComponentReference = reference.view().clone();
///             transform_component_reference_in_place(self, &mut duplicate);
///         }
///     }
/// }
/// ```
///
/// The raw reference is not reachable either:
///
/// ```compile_fail,E0616
/// use rumoca_ir_ast::{ComponentReferenceSite, ExpressionTransformer, SemanticReferenceEditor};
///
/// struct RawFieldMutant;
/// impl ExpressionTransformer for RawFieldMutant {
///     fn transform_component_reference(
///         &mut self,
///         reference: SemanticReferenceEditor<'_>,
///         _site: ComponentReferenceSite,
///     ) {
///         let _raw = reference.reference; // private field
///     }
/// }
/// ```
pub struct SemanticReferenceEditor<'r> {
    reference: &'r mut ComponentReference,
}

impl<'r> SemanticReferenceEditor<'r> {
    fn new(reference: &'r mut ComponentReference) -> Self {
        Self { reference }
    }

    /// The opaque read-only view of the reference's semantic facts.
    pub fn view(&self) -> ComponentReferenceView<'_> {
        ComponentReferenceView {
            reference: self.reference,
        }
    }

    /// The per-part declaration-identity slots, in declaration order.
    ///
    /// Each slot pairs the part's identifier text with write access to that
    /// part's `DefId` and nothing else, so identity can be proved per part
    /// without exposing the part itself.
    pub fn part_identity_slots(&mut self) -> impl Iterator<Item = PartIdentitySlot<'_>> {
        self.reference
            .parts
            .iter_mut()
            .map(|part| PartIdentitySlot {
                ident: &part.ident,
                def_id: &mut part.def_id,
            })
    }

    /// Record the non-semantic qualified display spelling.
    pub fn set_qualified_display_name(&mut self, name: impl Into<String>) {
        self.reference.set_qualified_display_name(name);
    }
}

/// Opaque immutable view of one component reference's semantic facts.
///
/// The view exposes the global-lookup flag, the span, the part count, the
/// root and target identities, and per-part views; it never yields a
/// structural node, so nothing read through it can be rewritten, cloned as a
/// `ComponentReference`, or fed back into a kernel entry.
#[derive(Clone, Copy)]
pub struct ComponentReferenceView<'v> {
    reference: &'v ComponentReference,
}

impl<'v> ComponentReferenceView<'v> {
    /// Whether the reference starts with `.` and uses global lookup.
    pub fn local(&self) -> bool {
        self.reference.local
    }

    /// The reference's source span.
    pub fn span(&self) -> Span {
        self.reference.span
    }

    /// The number of parts.
    pub fn part_count(&self) -> usize {
        self.reference.parts.len()
    }

    /// The first part's declaration identity.
    pub fn root_def_id(&self) -> Option<DefId> {
        self.reference.root_def_id()
    }

    /// The last part's declaration identity.
    pub fn target_def_id(&self) -> Option<DefId> {
        self.reference.target_def_id()
    }

    /// Per-part views, in declaration order.
    pub fn parts(&self) -> impl ExactSizeIterator<Item = ComponentReferencePartView<'v>> {
        self.reference
            .parts
            .iter()
            .map(|part| ComponentReferencePartView { part })
    }
}

/// Opaque immutable view of one reference part: identifier text and
/// declaration identity, nothing structural.
#[derive(Clone, Copy)]
pub struct ComponentReferencePartView<'v> {
    part: &'v crate::ComponentRefPart,
}

impl<'v> ComponentReferencePartView<'v> {
    /// The part's identifier text.
    pub fn ident_text(&self) -> &'v str {
        self.part.ident.text.as_ref()
    }

    /// The part's declaration identity.
    pub fn def_id(&self) -> Option<DefId> {
        self.part.def_id
    }
}

/// Write access to one part's declaration identity, and nothing else.
pub struct PartIdentitySlot<'p> {
    ident: &'p Token,
    def_id: &'p mut Option<DefId>,
}

impl PartIdentitySlot<'_> {
    /// The part's identifier text.
    pub fn ident_text(&self) -> &str {
        self.ident.text.as_ref()
    }

    /// The part's currently recorded declaration identity.
    pub fn def_id(&self) -> Option<DefId> {
        *self.def_id
    }

    /// Record the part's declaration identity.
    pub fn set_def_id(&mut self, def_id: DefId) {
        *self.def_id = Some(def_id);
    }

    /// Drop the part's declaration identity.
    pub fn clear_def_id(&mut self) {
        *self.def_id = None;
    }
}

/// Hook-only expression transformer.
///
/// The trait owns the two semantic reference hooks and nothing else: no node
/// hook, no traversal, no structural effect. The kernel always descends for
/// an implementor, so an ordinary semantic or reference transformer cannot
/// prune, replace, skip, or reorder anything. All recursion belongs to the
/// private kernel in this module; enter it through
/// [`transform_expression_in_place`] and the other `*_in_place` functions.
///
/// A structural effect cannot be requested through the trait, because the
/// surface that would receive the request does not exist:
///
/// ```compile_fail,E0407
/// use rumoca_ir_ast::ExpressionTransformer;
///
/// struct IdentityOnly;
/// impl ExpressionTransformer for IdentityOnly {
///     fn node_action(&mut self) {} // no such member: no prune, no replacement
/// }
/// ```
pub trait ExpressionTransformer {
    /// Component-reference hook, invoked by the kernel after the reference's
    /// part subscripts have been traversed. `site` states where the reference
    /// sits; callee references go to [`Self::transform_callee`] instead.
    fn transform_component_reference(
        &mut self,
        reference: SemanticReferenceEditor<'_>,
        site: ComponentReferenceSite,
    ) {
        let _ = (reference, site);
    }

    /// Callee hook, invoked by the kernel after the callee's part subscripts
    /// have been traversed and before the call's arguments are.
    fn transform_callee(&mut self, callee: SemanticReferenceEditor<'_>, site: CalleeSite) {
        let _ = (callee, site);
    }
}

/// One step of an iterator schedule.
pub enum IteratorStep<'s> {
    /// An index's range expression, handed out for walking in whatever
    /// context the owning schedule's rule assigns to ranges.
    Range(&'s mut Expression),
    /// An index to bind, handed out at the point the owning schedule's rule
    /// brings it into scope.
    Bind(&'s ForIndex),
}

/// The for-equation / for-statement iterator schedule.
///
/// MLS 3.7 §11.2.2.3 defines several iterators as nested loops ("replacing
/// each ',' by 'loop for'"), and §8.3.2 evaluates each loop's range "in the
/// scope immediately enclosing the for-equation" - which, for the expanded
/// inner loops, contains the earlier iterators. So index N's range is walked
/// before index N binds, with indices 1..N-1 already bound: the dependent
/// form `for i in 1:3, j in 1:i` is legal and `i` in `j`'s range is the
/// earlier iterator. This function owns that interleaving as a single loop
/// handing out [`IteratorStep`]s, so a consumer cannot bind an index before
/// its own range is walked and cannot hoist a later range above an earlier
/// binding. Scope exit stays with the consumer, after the loop body.
pub fn schedule_loop_iterators_mut(
    indices: &mut [ForIndex],
    mut step: impl FnMut(IteratorStep<'_>),
) {
    for index in indices {
        step(IteratorStep::Range(&mut index.range));
        step(IteratorStep::Bind(index));
    }
}

/// The array-constructor / reduction iterator schedule: reverse textual
/// nesting.
///
/// MLS 3.7 §10.4.1.2: "The notation with several iterators is a shorthand
/// notation for nested array constructors. The notation can be expanded
/// into the usual form by replacing each ',' by '} for' and prepending the
/// array constructor with a '{'." So `{e for i in ri, j in rj}` is
/// `{{e for i in ri} for j in rj}`: the LAST textual iterator is the
/// OUTERMOST binder. §10.4.1's scope sentence - iterator expressions "are
/// evaluated in the scope immediately enclosing the array constructor" -
/// must then be applied to the EXPANDED form: the inner constructor sits in
/// the outer's body, so `ri` is evaluated where `j` is already bound, while
/// `rj` is evaluated in the parent scope. Derived rule: walk ranges and
/// bind in REVERSE textual order - `rj`, bind `j`, `ri`, bind `i`, then the
/// body. A first textual range may see a later textual iterator; the
/// converse is not licensed. Warning to the next reader: applying the
/// §10.4.1 scope sentence to the flat, unexpanded form concludes that no
/// iterator is visible in any range - that reading is wrong, and "fixing"
/// this schedule toward it reintroduces the backwards bug. The same
/// derivation applies to reduction expressions through §10.3.4.1 and
/// §11.2.2.3's "(or reduction expressions)".
///
/// This is the mirror of [`schedule_loop_iterators_mut`], not the same
/// order: for-loops nest first-textual-outer, constructors nest
/// last-textual-outer. Scope exit stays with the consumer, which unbinds in
/// textual order (innermost first) after body and filter.
pub fn schedule_comprehension_iterators_mut(
    indices: &mut [ForIndex],
    mut step: impl FnMut(IteratorStep<'_>),
) {
    for index in indices.iter_mut().rev() {
        step(IteratorStep::Range(&mut index.range));
        step(IteratorStep::Bind(index));
    }
}

/// Substitute an integer value for a loop index throughout one expression.
///
/// The AST-owned named substitution operation: every single-part component
/// reference spelled `index_name` that MLS scoping attributes to the outer
/// binding is replaced by the literal shape of `value`; occurrences owned by
/// a rebinding comprehension iterator are left untouched. Comprehension
/// scoping follows the §10.4.1.2 reverse textual nesting (see
/// [`schedule_comprehension_iterators_mut`]): in `{j for j in 1:j}` the
/// range `j` is the outer binding and is replaced while the body `j` is the
/// iterator and is not; in a multi-iterator constructor a LAST iterator's
/// range never sees an earlier iterator (so its spelling of an outer name
/// is replaced), while a FIRST iterator's range does see a later rebinding
/// iterator (so that occurrence is the iterator's, not the outer binding's,
/// and is preserved).
///
/// The operation is total over `i64`. A nonnegative value becomes an
/// unsigned-integer terminal; a negative value becomes a unary minus over
/// its unsigned magnitude, the shape the parser produces for negative
/// literals; `i64::MIN`, whose magnitude has no unsigned i64 spelling,
/// becomes the expression `(-9223372036854775807) - 1`, the MLS §4.9.2
/// lower-bound shape generalized to i64, which the checked integer
/// evaluators execute exactly. The replaced reference's children (its part
/// subscripts) are not visited: substitution ends the branch, and the minted
/// literal's own nodes are kernel-built, not hooked.
///
/// The replacement strategy is a private kernel client; it cannot be
/// implemented or invoked with any other behavior from outside this module:
///
/// ```compile_fail,E0603
/// struct RogueClient;
/// impl rumoca_ir_ast::visitor::rewrite::KernelClient for RogueClient {}
/// ```
pub fn substitute_integer_loop_index(expr: &mut Expression, index_name: &str, value: i64) {
    let mut substituter = IntegerIndexSubstituter {
        index_name,
        value,
        shadow_depth: 0,
    };
    traverse_expression(&mut substituter, expr);
}

/// Rewrite one expression tree in place through the traversal kernel.
pub fn transform_expression_in_place<T: ExpressionTransformer + ?Sized>(
    transformer: &mut T,
    expr: &mut Expression,
) {
    traverse_expression(&mut HookClient { transformer }, expr);
}

/// Rewrite an equation- or statement-owned component reference in place.
///
/// Part subscripts are traversed by the kernel; the reference itself reaches
/// [`ExpressionTransformer::transform_component_reference`] with
/// [`ComponentReferenceSite::OwnedTarget`].
pub fn transform_component_reference_in_place<T: ExpressionTransformer + ?Sized>(
    transformer: &mut T,
    reference: &mut ComponentReference,
) {
    traverse_component_reference(
        &mut HookClient { transformer },
        reference,
        ComponentReferenceSite::OwnedTarget,
    );
}

/// Rewrite an equation- or statement-owned callee reference in place.
///
/// Part subscripts are traversed by the kernel; the callee reaches
/// [`ExpressionTransformer::transform_callee`] with [`CalleeSite::OwnedCall`].
pub fn transform_callee_in_place<T: ExpressionTransformer + ?Sized>(
    transformer: &mut T,
    callee: &mut ComponentReference,
) {
    traverse_callee(
        &mut HookClient { transformer },
        callee,
        CalleeSite::OwnedCall,
    );
}

/// Rewrite a declaration-owned subscript list in place.
pub fn transform_subscripts_in_place<T: ExpressionTransformer + ?Sized>(
    transformer: &mut T,
    subscripts: &mut [Subscript],
) {
    let mut client = HookClient { transformer };
    for subscript in subscripts {
        traverse_subscript(&mut client, subscript);
    }
}

/// Rewrite a loop index in place (its range expression).
pub fn transform_for_index_in_place<T: ExpressionTransformer + ?Sized>(
    transformer: &mut T,
    index: &mut ForIndex,
) {
    traverse_expression(&mut HookClient { transformer }, &mut index.range);
}

/// Sealed kernel client: the private contract between the traversal kernel
/// and the two in-module drivers. It is not nameable outside this module, so
/// no external code can supply a node decision, receive iterator binding
/// events, or drive the kernel with any behavior defined elsewhere.
trait KernelClient {
    /// Node decision, invoked before a node's children. The hook driver
    /// always descends; only the substitution driver ever replaces.
    fn node(&mut self, expr: &Expression) -> NodeStep {
        let _ = expr;
        NodeStep::Descend
    }

    /// Reference dispatch, after the reference's part subscripts.
    fn component_reference(
        &mut self,
        reference: &mut ComponentReference,
        site: ComponentReferenceSite,
    ) {
        let _ = (reference, site);
    }

    /// Callee dispatch, after the callee's part subscripts.
    fn callee(&mut self, callee: &mut ComponentReference, site: CalleeSite) {
        let _ = (callee, site);
    }

    /// An iterator was bound by the canonical schedule.
    fn bind_iterator(&mut self, name: &str) {
        let _ = name;
    }

    /// An iterator went out of scope after body and filter.
    fn unbind_iterator(&mut self, name: &str) {
        let _ = name;
    }
}

/// The kernel's answer for one node: descend into its children, or install a
/// kernel-built replacement and end the branch.
enum NodeStep {
    Descend,
    Replace(Expression),
}

/// Kernel driver for public hook transformers: always descends, forwards the
/// two reference hooks through borrow-scoped editors, ignores binding events.
struct HookClient<'t, T: ExpressionTransformer + ?Sized> {
    transformer: &'t mut T,
}

impl<T: ExpressionTransformer + ?Sized> KernelClient for HookClient<'_, T> {
    fn component_reference(
        &mut self,
        reference: &mut ComponentReference,
        site: ComponentReferenceSite,
    ) {
        self.transformer
            .transform_component_reference(SemanticReferenceEditor::new(reference), site);
    }

    fn callee(&mut self, callee: &mut ComponentReference, site: CalleeSite) {
        self.transformer
            .transform_callee(SemanticReferenceEditor::new(callee), site);
    }
}

/// Kernel driver for [`substitute_integer_loop_index`]: replaces unshadowed
/// single-part occurrences of the index with the literal shape of the value,
/// tracking shadowing through the canonical schedule's binding events.
struct IntegerIndexSubstituter<'s> {
    index_name: &'s str,
    value: i64,
    shadow_depth: usize,
}

impl KernelClient for IntegerIndexSubstituter<'_> {
    fn node(&mut self, expr: &Expression) -> NodeStep {
        if self.shadow_depth > 0 {
            return NodeStep::Descend;
        }
        let Expression::ComponentReference(reference) = expr else {
            return NodeStep::Descend;
        };
        let [part] = reference.parts.as_slice() else {
            return NodeStep::Descend;
        };
        if part.ident.text.as_ref() != self.index_name {
            return NodeStep::Descend;
        }
        NodeStep::Replace(signed_integer_literal(
            self.value,
            part.ident.location.clone(),
            reference.span,
        ))
    }

    fn bind_iterator(&mut self, name: &str) {
        self.shadow_depth += usize::from(name == self.index_name);
    }

    fn unbind_iterator(&mut self, name: &str) {
        self.shadow_depth -= usize::from(name == self.index_name);
    }
}

/// The literal shape of a signed integer value, total over `i64`.
///
/// A nonnegative value is an unsigned-integer terminal. A negative value
/// above the minimum is a unary minus over its exact unsigned magnitude, the
/// shape the parser produces for a negative literal in source. `i64::MIN`
/// has no spellable unsigned magnitude, so it is minted in the MLS §4.9.2
/// lower-bound shape `(-9223372036854775807) - 1`: a checked subtraction
/// whose operands and result are all representable, which the integer
/// evaluators execute without overflow. No value is wrapped or clamped, and
/// no unsigned token ever carries a sign.
fn signed_integer_literal(value: i64, location: Location, span: Span) -> Expression {
    if value == i64::MIN {
        let magnitude = unsigned_integer_terminal(i64::MAX.unsigned_abs(), location.clone(), span);
        return Expression::Binary {
            op: OpBinary::Sub,
            lhs: Arc::new(Expression::Unary {
                op: OpUnary::Minus,
                rhs: Arc::new(magnitude),
                span,
            }),
            rhs: Arc::new(unsigned_integer_terminal(1, location, span)),
            span,
        };
    }
    let magnitude = unsigned_integer_terminal(value.unsigned_abs(), location, span);
    if value >= 0 {
        return magnitude;
    }
    Expression::Unary {
        op: OpUnary::Minus,
        rhs: Arc::new(magnitude),
        span,
    }
}

/// An unsigned-integer terminal whose synthesized token stands at the
/// replaced reference's source location and carries no lexer stream identity.
fn unsigned_integer_terminal(magnitude: u64, location: Location, span: Span) -> Expression {
    Expression::Terminal {
        terminal_type: TerminalType::UnsignedInteger,
        token: Token {
            text: Arc::from(magnitude.to_string().as_str()),
            location,
            token_number: 0,
            token_type: 0,
        },
        span,
    }
}

/// The single traversal kernel.
///
/// Together with [`traverse_children`] this owns all recursion: the private
/// client's node decision runs first, and the kernel either descends into
/// every child edge or installs the kernel-built replacement. It is private
/// and free-standing, so no implementation can override or bypass it.
fn traverse_expression<C: KernelClient + ?Sized>(client: &mut C, expr: &mut Expression) {
    #[cfg(test)]
    record_kernel_event(KernelTraceEvent::Node(expr.span().start.0));
    match client.node(expr) {
        NodeStep::Descend => traverse_children(client, expr),
        NodeStep::Replace(replacement) => *expr = replacement,
    }
}

/// The kernel's per-variant dispatch.
///
/// Owns the exhaustive 17-variant match, all `Arc` copy-on-write, all
/// optional and list children, component-part and subscript order, and the
/// canonical iterator schedule for comprehensions.
fn traverse_children<C: KernelClient + ?Sized>(client: &mut C, expr: &mut Expression) {
    match expr {
        Expression::Empty { .. } | Expression::Terminal { .. } => {}
        Expression::Range {
            start, step, end, ..
        } => {
            traverse_arc(client, start);
            if let Some(step) = step.as_mut() {
                traverse_arc(client, step);
            }
            traverse_arc(client, end);
        }
        Expression::Unary { rhs, .. } => traverse_arc(client, rhs),
        Expression::Binary { lhs, rhs, .. } => {
            traverse_arc(client, lhs);
            traverse_arc(client, rhs);
        }
        Expression::ComponentReference(reference) => {
            traverse_component_reference(client, reference, ComponentReferenceSite::Value);
        }
        Expression::DerivativeCall { args, .. } => {
            for argument in args {
                traverse_expression(client, argument);
            }
        }
        Expression::FunctionCall { comp, args, .. } => {
            traverse_callee(client, comp, CalleeSite::ExpressionCall);
            for argument in args {
                traverse_expression(client, argument);
            }
        }
        Expression::ClassModification {
            target,
            modifications,
            ..
        } => {
            traverse_component_reference(
                client,
                target,
                ComponentReferenceSite::ModificationTarget,
            );
            for modification in modifications {
                traverse_expression(client, modification);
            }
        }
        Expression::NamedArgument { value, .. } => traverse_arc(client, value),
        Expression::Modification { target, value, .. } => {
            traverse_component_reference(
                client,
                target,
                ComponentReferenceSite::ModificationTarget,
            );
            if let Some(value) = value {
                traverse_arc(client, value);
            }
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            for element in elements {
                traverse_expression(client, element);
            }
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                traverse_expression(client, condition);
                traverse_expression(client, value);
            }
            traverse_arc(client, else_branch);
        }
        Expression::Parenthesized { inner, .. } => traverse_arc(client, inner),
        Expression::ArrayComprehension {
            expr: body,
            indices,
            filter,
            ..
        } => traverse_comprehension_children(client, body, indices, filter),
        Expression::ArrayIndex {
            base, subscripts, ..
        } => {
            traverse_arc(client, base);
            for subscript in subscripts {
                traverse_subscript(client, subscript);
            }
        }
        Expression::FieldAccess { base, .. } => traverse_arc(client, base),
    }
}

/// Traverse comprehension children in their construct-specific scope order.
fn traverse_comprehension_children<C: KernelClient + ?Sized>(
    client: &mut C,
    body: &mut Arc<Expression>,
    indices: &mut [ForIndex],
    filter: &mut Option<Arc<Expression>>,
) {
    // Constructor/reduction scoping (MLS 3.7 §10.4.1.2 reverse textual
    // nesting with §10.4.1's scope rule applied to the expanded form - see
    // the schedule's derivation): ranges and binds proceed in reverse textual
    // order, then body and filter are binder-owned.
    schedule_comprehension_iterators_mut(indices, |step| match step {
        IteratorStep::Range(range) => traverse_expression(client, range),
        IteratorStep::Bind(index) => {
            let name = index.ident.text.as_ref();
            #[cfg(test)]
            record_kernel_event(KernelTraceEvent::Bind(name.to_string()));
            client.bind_iterator(name);
        }
    });
    traverse_arc(client, body);
    if let Some(filter) = filter.as_mut() {
        traverse_arc(client, filter);
    }
    // Binds ran in reverse textual order, so unbinding innermost first is
    // textual order.
    for index in indices.iter() {
        let name = index.ident.text.as_ref();
        #[cfg(test)]
        record_kernel_event(KernelTraceEvent::Unbind(name.to_string()));
        client.unbind_iterator(name);
    }
}

/// Traverse an `Arc`-held child without deep-copying the subtree.
///
/// A uniquely owned child is rewritten in place. A shared child uses
/// copy-on-write so the other holder keeps the old subtree. Neither path
/// installs a semantically empty placeholder in the AST.
fn traverse_arc<C: KernelClient + ?Sized>(client: &mut C, arc: &mut Arc<Expression>) {
    #[cfg(test)]
    if Arc::strong_count(arc) > 1 {
        COPY_ON_WRITE_EVENTS.with(|events| events.set(events.get() + 1));
    }
    traverse_expression(client, Arc::make_mut(arc));
}

/// Traverse a component reference: every part's subscripts in declaration
/// order, then the client's reference dispatch for the whole reference.
fn traverse_component_reference<C: KernelClient + ?Sized>(
    client: &mut C,
    reference: &mut ComponentReference,
    site: ComponentReferenceSite,
) {
    traverse_component_reference_subscripts(client, reference);
    #[cfg(test)]
    record_kernel_event(KernelTraceEvent::Cref(
        reference_trace_name(reference),
        site,
    ));
    client.component_reference(reference, site);
}

/// Traverse a callee reference: part subscripts first, then the callee
/// dispatch.
fn traverse_callee<C: KernelClient + ?Sized>(
    client: &mut C,
    callee: &mut ComponentReference,
    site: CalleeSite,
) {
    traverse_component_reference_subscripts(client, callee);
    #[cfg(test)]
    record_kernel_event(KernelTraceEvent::Callee(reference_trace_name(callee), site));
    client.callee(callee, site);
}

/// The one shared in-place component-reference subscript traversal.
fn traverse_component_reference_subscripts<C: KernelClient + ?Sized>(
    client: &mut C,
    reference: &mut ComponentReference,
) {
    for subscript in reference
        .parts
        .iter_mut()
        .filter_map(|part| part.subs.as_mut())
        .flatten()
    {
        traverse_subscript(client, subscript);
    }
}

fn traverse_subscript<C: KernelClient + ?Sized>(client: &mut C, subscript: &mut Subscript) {
    match subscript {
        Subscript::Empty | Subscript::Range { .. } => {}
        Subscript::Expression(expr) => traverse_expression(client, expr),
    }
}
