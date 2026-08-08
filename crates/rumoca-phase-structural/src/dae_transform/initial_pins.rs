//! Carry a stated initial value onto the coordinate the runtime seeds.
//!
//! MLS 3.6 §8.6: "For every Real variable vc with fixed = true, the equation
//! vc = startExpression is added to the initialization equations." A runtime
//! that seeds one value per *state* from that state's own `start` and then
//! projects the algebraic unknowns onto the residuals answers that equation
//! only when the pinned variable is itself a state. A pin declared on an
//! aliased coordinate — `s_rel(start = 1, fixed = true)` on a translational
//! spring, whose position is `flange_b.s - flange_a.s` — has no equation of its
//! own left: the state the alias determines keeps whatever guess its `start`
//! carries, and the pinned coordinate lands wherever that guess puts it.
//!
//! The equalities the source system already asserts decide the transfer. Every
//! accepted residual is an unconditional continuous equation, so
//! `su·u + sv·v + K = 0` proves `u ≡ -su·sv·v - su·K` for all time whenever `K`
//! is time-invariant, and `s·v + K = 0` proves `v` equal to that constant. Both
//! facts are exact at the initialization instant too, so a pin on `u` states
//! exactly one value for `v`, displacement included. That value is a
//! *definition* of the state, not one more residual to solve: the closure has
//! already done the solving.
//!
//! A residual that names a third coordinate proves nothing on its own, but a
//! coordinate this closure has already proved constant is not an unknown —
//! substituting it turns `s_rel - (flange_b.s - flange_a.s)` into a two-term
//! equality the moment `flange_a.s` is known to sit at a fixed support. The
//! reading is therefore repeated while it keeps learning constants.
//!
//! Two members of one class whose stated values differ by a *proved nonzero
//! constant* state an initialization with no solution, and are rejected with
//! both declarations rather than resolved by picking one. A difference that
//! still reads a parameter proves nothing here — `a(start = 3)` and
//! `b(start = 1)` under `a = b + L` agree exactly when `L = 2` — so it is left
//! to the initialization instant as a residual the runtime checks with numbers.
//!
//! Only a class holding exactly one state is acted on. A class with none has no
//! coordinate the runtime seeds, so there is nothing to define; its members are
//! still checked against each other for contradiction, which is the only claim
//! this phase can make about them. A class holding several states is a
//! redundancy the reduction has to resolve first — it is the state demotion's
//! subject — so writing a value onto one of them here would state an initial
//! condition the model never made about the others.
//!
//! The reduction and this transfer answer the same MLS 3.6 §8.6 obligation from
//! two sides, so they are checked against each other rather than trusted to
//! agree: `constraints.rs` accepts a reduction that drops a pinned coordinate
//! only after [`represented_initial_values`] proves the system it produces still
//! states that value, which is exactly the reading `transfers` acts on.
//!
//! Two gaps are known and deliberate. A `fixed = true` start this closure cannot
//! write down — absent, so the MLS 3.6 §4.8 default with no expression of the
//! system to name, or one that varies — is carried by nothing here, so a
//! reduction that would demote its coordinate is refused instead: `x = time + 1`
//! with `x(start = 1, fixed = true)` reports `ES012` where OpenModelica warns
//! that it cannot decide consistency and simulates. Honouring it needs a §8.6
//! *check* residual written about a coordinate that is not a state, which is
//! exactly the row shape the paragraph above rules out, so it is tracked
//! separately (task #96) rather than approximated here.
//!
//! The other is bounded by the same three-valued policy: an undecided
//! restatement is *not* refused, and where the class holds no state to write a
//! check about, nothing re-checks it either. That is the deliberate choice —
//! `Rotational.Components.Fixed` states `flange.phi = phi0` for a parameter
//! `phi0`, so refusing it would refuse every rigidly held mechanism over a
//! number this phase does not have.

use rumoca_ir_dae as dae;

use super::constraints::{numeric_literal, states_the_same_expression};
use super::equalities::{AdditiveOperands, flatten_additive, is_scalar_real, is_time_invariant};
use crate::StructuralError;

/// One signed operand of a transferred initial value.
///
/// `expression` is a whole-model expression ordinal that the closure proved
/// time-invariant, so evaluating it at the initialization instant is exact.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PinTerm {
    pub expression: u32,
    pub negated: bool,
}

/// How the runtime has to enforce one MLS 3.6 §8.6 initial equation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InitialValueRole {
    /// A definition: the asserted equalities prove this value for a coordinate
    /// the runtime seeds, so nothing is left to solve.
    Definition,
    /// A check: another declaration already states the class's initial value,
    /// and whether the two agree depends on parameter values this phase does not
    /// evaluate. The equation stays a residual so the initialization instant
    /// answers it with numbers instead of this phase guessing.
    Check,
}

/// One MLS 3.6 §8.6 initial equation, carried to the state that holds it.
#[derive(Clone, Debug)]
pub struct InitialValuePin {
    /// The state the value is stated about, whichever member of its class the
    /// model declared it on.
    pub coordinate: u32,
    /// Signed terms whose sum is the stated value.
    pub value: Vec<PinTerm>,
    /// How the runtime must enforce it.
    pub role: InitialValueRole,
    /// The variable whose `fixed = true` start the value was read from.
    pub source: u32,
    /// That variable's declaration.
    pub provenance: dae::DaeProvenance,
}

/// Every stated initial value the runtime would otherwise drop, rewritten onto
/// the state that carries it.
pub(super) fn transferred_initial_values(
    view: dae::DaeView<'_>,
) -> Result<Vec<InitialValuePin>, StructuralError> {
    ValueClosure::collect(view).transfers(view)
}

/// Every stated initial value this system still enforces at the initialization
/// instant, by variable ordinal.
///
/// MLS 3.6 §8.6 states one equation per `fixed = true` declaration, and it
/// states it about the *quantity* the declaration names — not about whichever
/// coordinate a runtime happens to integrate. An index reduction may therefore
/// move a stated value onto another coordinate of the same quantity, and may
/// not drop it. This reports which values a given system still states, so the
/// demotion in [`super::demote_direct_state`] can read the set before a rebuild
/// and again after it and refuse any rebuild that lost one: a checked
/// postcondition rather than an assumption about what a demotion preserves.
///
/// Exactly four proofs answer the §8.6 equation for one declaration, and
/// nothing else does:
///
///   * the declaration is a state, so the runtime seeds that coordinate from
///     this very start;
///   * its class holds exactly one state, so [`ValueClosure::transfers`] writes
///     the value onto that state — as a definition, or as a residual the
///     initialization instant checks;
///   * its class holds several states and one of them is pinned too, so that
///     state's own seeding answers this equation as far as anything here can;
///   * its class holds no state at all, and the asserted equalities prove the
///     class holds a value the stated one is not proved to differ from.
///
/// A declaration whose stated value this closure cannot write down — an absent
/// `start`, which is the MLS 3.6 §4.8 default with no expression in the system
/// to name, or a start that is not time-invariant — is reported under the first
/// proof only. Nothing here can carry a value it cannot name, so a demotion
/// that turns such a declaration into an algebraic drops it, and the check
/// above refuses that demotion instead of accepting it on a value nobody
/// proved.
///
/// Cost: one whole closure per call, and the caller makes one call per rebuilt
/// system it is willing to take. That is deliberate — the answer is a property
/// of the *rebuilt* system, so nothing cheaper is an answer — but two things
/// keep it bounded. A system that states no initial value at all skips the
/// comparison outright ([`super::constraints::discarded_stated_initial_value`]
/// returns on an empty set), and a candidate that does not reduce is dropped
/// before the closure is built at all. Note that [`flatten_additive`] reads
/// every residual to its leaves with no early bail on arity — that is what makes
/// a four-terminal balance readable however it is spelled, and it makes one
/// closure linear in the whole system rather than in its two-term residuals.
pub(super) fn represented_initial_values(view: dae::DaeView<'_>) -> Vec<u32> {
    ValueClosure::collect(view).represented(view)
}

/// Where one variable sits relative to the root of its class.
struct Resolved {
    root: u32,
    /// Whether the variable is the negation of the root's value.
    negated: bool,
    /// The displacement between the variable and the (signed) root value.
    offset: Vec<PinTerm>,
}

/// What one residual reading produced.
enum Reading {
    /// An equality this closure recorded; the residual is spent.
    Learned,
    /// More than two coordinates are still unknown, so the residual states no
    /// equality *yet*; a later round may know enough of them for it to.
    Pending,
    /// Nothing this closure can ever read: a leaf that is neither a scalar Real
    /// coordinate nor a time-invariant expression, however the rest reduces.
    Unusable,
}

/// Coordinate classes closed over the exact equalities a system asserts, with
/// the time-invariant displacement between each member and its class root.
struct ValueClosure {
    /// Union-find parent per variable ordinal.
    parent: Vec<u32>,
    /// Whether each variable is the negation of its parent's value.
    parity: Vec<bool>,
    /// Displacement of each variable from its (signed) parent value.
    offset: Vec<Vec<PinTerm>>,
    /// Per root: the constant value the system proves the root holds.
    invariant: Vec<Option<Vec<PinTerm>>>,
    /// Per root: classes whose recorded equalities contradict each other, which
    /// prove nothing a transfer may stand on.
    contradicted: Vec<bool>,
}

impl ValueClosure {
    fn collect(view: dae::DaeView<'_>) -> Self {
        let count = view.variable_count();
        let mut closure = Self {
            parent: (0..count as u32).collect(),
            parity: vec![false; count],
            offset: vec![Vec::new(); count],
            invariant: vec![None; count],
            contradicted: vec![false; count],
        };
        let mut pending = residual_ordinals(view);
        // Each round consumes at least one residual to keep going, so the walk
        // stops after at most one round per residual — and stops immediately
        // once a round learns nothing, which is the usual case after the first.
        while !pending.is_empty() {
            let (deferred, learned) = closure.read_round(view, pending);
            if !learned {
                break;
            }
            pending = deferred;
        }
        closure
    }

    /// Read every residual still pending, reporting the ones still unread and
    /// whether the round learned anything.
    fn read_round(&mut self, view: dae::DaeView<'_>, pending: Vec<u32>) -> (Vec<u32>, bool) {
        let mut deferred = Vec::new();
        let mut learned = false;
        for residual in pending {
            match self.read_residual(view, residual) {
                Reading::Learned => learned = true,
                Reading::Pending => deferred.push(residual),
                Reading::Unusable => (),
            }
        }
        (deferred, learned)
    }

    /// Read one residual as an equality over the coordinates it leaves unknown.
    ///
    /// A coordinate this closure already proved constant is folded into the
    /// invariant sum rather than counted as an unknown, which is what lets a
    /// balance over three coordinates — or a four-terminal flow node — reduce
    /// once enough of its terminals are known. How many coordinates the residual
    /// mentions never decides whether it is read: only how many are still
    /// unknown when it is read does, and a residual is offered again on every
    /// round that learns something new.
    fn read_residual(&mut self, view: dae::DaeView<'_>, residual: u32) -> Reading {
        let Some(expression) = view.expression_id(residual as usize) else {
            return Reading::Unusable;
        };
        let mut operands = AdditiveOperands::default();
        if !flatten_additive(view, expression, false, &mut operands) {
            return Reading::Unusable;
        }
        let mut constants = invariant_terms(&operands);
        let mut unknowns = Vec::new();
        for (variable, negated) in operands.variables {
            match self.value_of(variable) {
                Some(value) => constants.extend(signed(&value, negated)),
                None => unknowns.push((variable, negated)),
            }
        }
        match *unknowns.as_slice() {
            // `s·v + K = 0` puts `v` at `-s·K`.
            [(variable, negated)] => {
                self.note_invariant(variable, signed(&constants, !negated));
                Reading::Learned
            }
            // `su·u + sv·v + K = 0` puts `u` at `-su·sv·v - su·K`.
            [(left, left_negated), (right, right_negated)] if left != right => {
                self.union(
                    left,
                    right,
                    left_negated == right_negated,
                    signed(&constants, !left_negated),
                );
                Reading::Learned
            }
            [] => Reading::Unusable,
            _ => Reading::Pending,
        }
    }

    /// The class root of `variable`, with the sign and displacement that carry
    /// the root's value back to it.
    fn find(&self, variable: u32) -> Resolved {
        let mut current = variable;
        let mut negated = false;
        let mut offset = Vec::new();
        while self.parent[current as usize] != current {
            offset.extend(signed(&self.offset[current as usize], negated));
            negated ^= self.parity[current as usize];
            current = self.parent[current as usize];
        }
        Resolved {
            root: current,
            negated,
            offset: reduced(offset),
        }
    }

    /// Record `left = (opposite ? -1 : 1)·right + offset`.
    fn union(&mut self, left: u32, right: u32, opposite: bool, offset: Vec<PinTerm>) {
        let left = self.find(left);
        let right = self.find(right);
        // value(left.root) = left.negated·(sign·(right.negated·value(right.root)
        //   + right.offset) + offset - left.offset)
        let sign = opposite;
        let parity = left.negated ^ sign ^ right.negated;
        let displacement = reduced(signed(
            &[
                signed(&right.offset, sign),
                offset,
                signed(&left.offset, true),
            ]
            .concat(),
            left.negated,
        ));
        if left.root == right.root {
            if parity || !displacement.is_empty() {
                self.contradicted[left.root as usize] = true;
            }
            return;
        }
        let carried = self.invariant[left.root as usize].take();
        let contradicted = self.contradicted[left.root as usize];
        self.parent[left.root as usize] = right.root;
        self.parity[left.root as usize] = parity;
        self.offset[left.root as usize] = displacement;
        self.contradicted[right.root as usize] |= contradicted;
        if let Some(value) = carried {
            self.note_invariant(left.root, value);
        }
    }

    /// Record that the system proves `variable` holds the constant `value`.
    fn note_invariant(&mut self, variable: u32, value: Vec<PinTerm>) {
        let resolved = self.find(variable);
        // value(variable) = negated·value(root) + offset, so the root holds
        // `negated·(value - offset)`.
        let at_root = reduced(signed(
            &[value, signed(&resolved.offset, true)].concat(),
            resolved.negated,
        ));
        match &self.invariant[resolved.root as usize] {
            None => self.invariant[resolved.root as usize] = Some(at_root),
            Some(known) => {
                if !terms_cancel(known, &at_root) {
                    self.contradicted[resolved.root as usize] = true;
                }
            }
        }
    }

    /// The constant value the system proves `variable` holds, if it proves one.
    fn value_of(&self, variable: u32) -> Option<Vec<PinTerm>> {
        let resolved = self.find(variable);
        if self.contradicted[resolved.root as usize] {
            return None;
        }
        let root = self.invariant[resolved.root as usize].as_ref()?;
        Some(reduced(
            [signed(root, resolved.negated), resolved.offset].concat(),
        ))
    }
}

/// Every unconditional continuous residual of one system, by expression ordinal.
fn residual_ordinals(view: dae::DaeView<'_>) -> Vec<u32> {
    view.continuous_owners()
        .filter_map(|owner| match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => {
                Some(equation.residual().index())
            }
            dae::ContinuousOwnerView::Structured { .. } => None,
        })
        .collect()
}

/// The invariant operands of one residual, as signed terms.
///
/// An operand the residual proves exactly zero contributes nothing, so it is
/// dropped here rather than carried through every later sum.
fn invariant_terms(operands: &AdditiveOperands) -> Vec<PinTerm> {
    operands
        .invariants
        .iter()
        .filter(|invariant| !invariant.zero)
        .map(|invariant| PinTerm {
            expression: invariant.expression,
            negated: invariant.negated,
        })
        .collect()
}

/// `terms`, negated when `negated`.
fn signed(terms: &[PinTerm], negated: bool) -> Vec<PinTerm> {
    terms
        .iter()
        .map(|term| PinTerm {
            expression: term.expression,
            negated: term.negated != negated,
        })
        .collect()
}

/// `terms` with every pair that cancels removed.
///
/// Cancellation is by expression identity, which is the only equality this
/// phase proves without evaluating anything. A sum that fails to reduce to
/// nothing is still an exact displacement; it is only the *emptiness* test that
/// this makes conservative.
fn reduced(terms: Vec<PinTerm>) -> Vec<PinTerm> {
    let mut kept: Vec<PinTerm> = Vec::with_capacity(terms.len());
    for term in terms {
        match kept.iter().position(|candidate| {
            candidate.expression == term.expression && candidate.negated != term.negated
        }) {
            Some(index) => {
                kept.remove(index);
            }
            None => kept.push(term),
        }
    }
    kept
}

/// Whether two signed sums cancel term for term, by expression identity.
fn terms_cancel(left: &[PinTerm], right: &[PinTerm]) -> bool {
    reduced([left.to_vec(), signed(right, true)].concat()).is_empty()
}

/// What this phase can prove about two stated initial values for one quantity.
#[derive(Clone, Copy, PartialEq, Eq)]
enum StatedAgreement {
    /// The difference cancels: the two declarations state the same value.
    Same,
    /// The difference reduces to a numeric constant that is not zero. No
    /// parameter value can close that gap, so the two equations MLS 3.6 §8.6
    /// adds have no common solution.
    Contradicted,
    /// The difference still reads a parameter. Whether it vanishes depends on
    /// values this phase does not evaluate, so it decides nothing.
    Undecided,
}

/// Compare two stated initial values for the same quantity.
///
/// Terms are first matched by expression identity, then folded — literals
/// numerically, everything else as written — so `0` and `0.0` agree and two
/// spellings of one parameter agree. A leftover parameter is the *undecided*
/// answer, never the contradicted one: `a(start = 3)` against `b(start = 1)`
/// with `a = b + L` states one value exactly when `L = 2`, which is a question
/// about a number this phase never has. Only a difference that survives as a
/// nonzero constant is a contradiction, because no parameter value can close it.
fn stated_agreement(
    view: dae::DaeView<'_>,
    left: &[PinTerm],
    right: &[PinTerm],
) -> StatedAgreement {
    let difference = reduced([left.to_vec(), signed(right, true)].concat());
    let mut constant = 0.0;
    let mut symbolic: Vec<PinTerm> = Vec::new();
    for term in difference {
        match view
            .expression_id(term.expression as usize)
            .and_then(|expression| numeric_literal(view, expression))
        {
            Some(value) if term.negated => constant -= value,
            Some(value) => constant += value,
            None => symbolic.push(term),
        }
    }
    match cancel_symbolic_terms(view, &symbolic) {
        false => StatedAgreement::Undecided,
        true if constant == 0.0 => StatedAgreement::Same,
        true => StatedAgreement::Contradicted,
    }
}

/// Whether the non-literal remainder of a difference cancels as written.
fn cancel_symbolic_terms(view: dae::DaeView<'_>, terms: &[PinTerm]) -> bool {
    let mut open: Vec<PinTerm> = Vec::new();
    for term in terms.iter().copied() {
        let matched = open.iter().position(|candidate| {
            candidate.negated != term.negated
                && same_expression(view, candidate.expression, term.expression)
        });
        match matched {
            Some(index) => {
                open.remove(index);
            }
            None => open.push(term),
        }
    }
    open.is_empty()
}

/// The state coordinates of the class rooted at `root`.
fn class_states(states: &[(u32, Vec<u32>)], root: u32) -> &[u32] {
    states
        .iter()
        .find(|(candidate, _)| *candidate == root)
        .map_or([].as_slice(), |(_, states)| states.as_slice())
}

fn same_expression(view: dae::DaeView<'_>, left: u32, right: u32) -> bool {
    match (
        view.expression_id(left as usize),
        view.expression_id(right as usize),
    ) {
        (Some(left), Some(right)) => states_the_same_expression(view, left, right),
        _ => false,
    }
}

/// One `fixed = true` declaration, resolved against its class root.
struct PinnedMember {
    variable: u32,
    /// The value the pin states for the class root.
    at_root: Vec<PinTerm>,
    provenance: dae::DaeProvenance,
}

impl ValueClosure {
    /// Rewrite every stated initial value the runtime would otherwise drop.
    ///
    /// The contradiction check runs first and over every class, whatever shape
    /// it has: two declarations that state different values for one quantity are
    /// a defect of the model, not of the coordinate the runtime happens to seed.
    fn transfers(&self, view: dae::DaeView<'_>) -> Result<Vec<InitialValuePin>, StructuralError> {
        let mut classes: Vec<(u32, Vec<PinnedMember>)> = Vec::new();
        for (root, member) in self.pinned_members(view) {
            match classes.iter_mut().find(|(candidate, _)| *candidate == root) {
                Some((_, members)) => members.push(member),
                None => classes.push((root, vec![member])),
            }
        }
        let states = self.states_by_root(view);
        let mut pins = Vec::new();
        for (root, members) in classes {
            self.reject_contradicted_pins(view, &members)?;
            let [state] = class_states(&states, root) else {
                continue;
            };
            pins.extend(self.class_pins(view, *state, &members));
        }
        Ok(pins)
    }

    /// The initial equations one class of pinned members needs the runtime to
    /// apply, given the single state it seeds.
    ///
    /// The class's value is stated once — by the state's own declaration when it
    /// has one, otherwise by the first pinned member, whose value the closure
    /// carries onto the state. Every other declaration in the class restates it:
    /// silently when this phase proved the two agree, and otherwise as a
    /// residual the initialization instant checks.
    ///
    /// Every equation here is written about the *state*, including the checked
    /// ones. That is what makes a check answerable: the state holds a value from
    /// the moment the runtime seeds it, while the pinned member itself may be an
    /// algebraic whose value only exists after the projection the check would
    /// have to precede.
    fn class_pins(
        &self,
        view: dae::DaeView<'_>,
        state: u32,
        members: &[PinnedMember],
    ) -> Vec<InitialValuePin> {
        let Some(source) = members
            .iter()
            .find(|member| member.variable == state)
            .or_else(|| members.first())
        else {
            return Vec::new();
        };
        let mut pins = Vec::new();
        if source.variable != state {
            pins.push(self.pin(state, source, InitialValueRole::Definition));
        }
        pins.extend(
            members
                .iter()
                .filter(|member| member.variable != source.variable)
                .filter(|member| {
                    stated_agreement(view, &source.at_root, &member.at_root)
                        == StatedAgreement::Undecided
                })
                .map(|member| self.pin(state, member, InitialValueRole::Check)),
        );
        pins
    }

    /// What one pinned declaration states about `state`.
    fn pin(&self, state: u32, member: &PinnedMember, role: InitialValueRole) -> InitialValuePin {
        let resolved = self.find(state);
        InitialValuePin {
            coordinate: state,
            value: reduced([signed(&member.at_root, resolved.negated), resolved.offset].concat()),
            role,
            source: member.variable,
            provenance: member.provenance,
        }
    }

    /// Every stated initial value this system still enforces. See
    /// [`represented_initial_values`].
    fn represented(&self, view: dae::DaeView<'_>) -> Vec<u32> {
        let states = self.states_by_root(view);
        let members = self.pinned_members(view);
        view.variables()
            .filter(|(_, variable)| carries_a_stated_initial_value(*variable))
            .filter(|(id, variable)| {
                variable.role() == dae::VariableRole::State
                    || self.class_states_the_value(view, &states, &members, id.index())
            })
            .map(|(id, _)| id.index())
            .collect()
    }

    /// Whether the asserted equalities still state `variable`'s pinned value
    /// about a coordinate the runtime answers, for a declaration that is not
    /// itself a state.
    ///
    /// The three class-shaped proofs of [`represented_initial_values`], read off
    /// the same closure [`ValueClosure::transfers`] acts on, so the two agree by
    /// construction: a value reported here is one `transfers` writes down, one
    /// another seeded state already carries, or one the system asserts outright.
    ///
    /// Three-valued, exactly like [`stated_agreement`] and for the reason stated
    /// at the top of this module: a difference that still reads a parameter
    /// proves nothing. `x(start = 1, fixed = true)` in a class the system pins
    /// to a parameter `c` states one value exactly when `c = 1`, which is a
    /// question about a number this phase never has, so it is left to the
    /// initialization instant — as the checked residual `class_pins` writes
    /// where the class has a state to write it about, and otherwise as the
    /// numeric solve of the very equation that asserts the value. Only a
    /// *proved* mismatch means the equation MLS 3.6 §8.6 adds was discarded;
    /// refusing an undecided one would refuse consistent models over the way
    /// their starts happen to be spelled, and §8.6 states the
    /// balanced-initialization rule as a "should". `Rotational.Components.Fixed`
    /// alone — `flange.phi = phi0` with `phi0` a parameter — puts every rigidly
    /// held mechanism in that undecided case.
    ///
    /// What the caller does with this is a *superset* test across one rebuild,
    /// so the question each arm answers is whether the demotion changed the
    /// answer: a class that asserts a value asserts the same one afterwards, and
    /// a class another declaration seeds is seeded by that same declaration
    /// afterwards.
    fn class_states_the_value(
        &self,
        view: dae::DaeView<'_>,
        states: &[(u32, Vec<u32>)],
        members: &[(u32, PinnedMember)],
        variable: u32,
    ) -> bool {
        let Some((root, member)) = members
            .iter()
            .find(|(_, member)| member.variable == variable)
        else {
            return false;
        };
        match class_states(states, *root) {
            [_] => true,
            [] => self.value_of(variable).is_some_and(|proved| {
                stated_value(view, variable).is_some_and(|stated| {
                    stated_agreement(view, &proved, &stated) != StatedAgreement::Contradicted
                })
            }),
            // A pinned member of the class that the runtime seeds answers this
            // declaration's equation as far as anything here can: exactly when
            // the two agree, and undecidably when their difference reads a
            // parameter. A *proved* difference is the one case where it does not
            // — the surviving seed states a different value for the same
            // quantity — so it counts as no answer at all, and a demotion that
            // leaves only that behind is refused by name.
            _ => members.iter().any(|(other_root, other)| {
                other_root == root
                    && other.variable != variable
                    && is_seeded_state(view, other.variable)
                    && stated_agreement(view, &member.at_root, &other.at_root)
                        != StatedAgreement::Contradicted
            }),
        }
    }

    /// Every `fixed = true` scalar Real coordinate, keyed by its class root.
    fn pinned_members(&self, view: dae::DaeView<'_>) -> Vec<(u32, PinnedMember)> {
        let mut members = Vec::new();
        for (id, variable) in view.variables() {
            if !carries_a_stated_initial_value(variable) {
                continue;
            }
            // A start that is not time-invariant is not a value this phase can
            // hand a runtime as a definition, so its class is left alone.
            let Some(start) = variable
                .start()
                .filter(|start| is_time_invariant(view, *start))
            else {
                continue;
            };
            let resolved = self.find(id.index());
            if self.contradicted[resolved.root as usize] {
                continue;
            }
            // value(variable) = negated·value(root) + offset, and the pin
            // states value(variable) = start.
            let at_root = reduced(signed(
                &[
                    vec![PinTerm {
                        expression: start.index(),
                        negated: false,
                    }],
                    signed(&resolved.offset, true),
                ]
                .concat(),
                resolved.negated,
            ));
            members.push((
                resolved.root,
                PinnedMember {
                    variable: id.index(),
                    at_root,
                    provenance: variable.declaration(),
                },
            ));
        }
        members
    }

    /// The state coordinates of every class that has one, keyed by class root.
    fn states_by_root(&self, view: dae::DaeView<'_>) -> Vec<(u32, Vec<u32>)> {
        let mut classes: Vec<(u32, Vec<u32>)> = Vec::new();
        for (id, variable) in view.variables() {
            if variable.role() != dae::VariableRole::State {
                continue;
            }
            let root = self.find(id.index()).root;
            match classes.iter_mut().find(|(candidate, _)| *candidate == root) {
                Some((_, states)) => states.push(id.index()),
                None => classes.push((root, vec![id.index()])),
            }
        }
        classes
    }

    /// MLS 3.6 §8.6 gives one quantity one initial value.
    ///
    /// Only a difference this phase *proves* nonzero is reported. Two members
    /// whose agreement turns on a parameter value are left to the initialization
    /// instant, which has the numbers: rejecting them here would refuse
    /// consistent models over the way their starts happen to be spelled, and MLS
    /// 3.6 §8.6 states the balanced-initialization rule as a "should".
    ///
    /// Every pair is compared, not just every member against the first: two
    /// declarations can each be undecidable against a third and still contradict
    /// each other outright.
    fn reject_contradicted_pins(
        &self,
        view: dae::DaeView<'_>,
        members: &[PinnedMember],
    ) -> Result<(), StructuralError> {
        let contradiction = members.iter().enumerate().find_map(|(index, member)| {
            let other = members[index + 1..].iter().find(|other| {
                stated_agreement(view, &member.at_root, &other.at_root)
                    == StatedAgreement::Contradicted
            })?;
            Some((member, other))
        });
        match contradiction {
            None => Ok(()),
            Some((member, other)) => Err(StructuralError::ConflictingStatedInitialValues {
                variable: variable_name(view, member.variable),
                other: variable_name(view, other.variable),
                span: member.provenance.span(),
                other_span: other.provenance.span(),
            }),
        }
    }
}

/// Whether MLS 3.6 §8.6 turns this declaration into an initialization equation
/// this phase is responsible for.
///
/// Restricted to the continuous scalar Reals the closure reads: a discrete
/// coordinate is initialized by its own §8.6 owner, and a parameter with
/// `fixed = false` is an unknown of the initialization system rather than a
/// stated value.
fn carries_a_stated_initial_value(variable: dae::VariableView<'_>) -> bool {
    variable.fixed() == Some(true)
        && is_scalar_real(variable)
        && matches!(
            variable.role(),
            dae::VariableRole::State | dae::VariableRole::Algebraic | dae::VariableRole::Output
        )
}

/// The value one declaration states about itself, as a signed sum.
///
/// Only a start this closure can name is reported: an absent one is the MLS 3.6
/// §4.8 default, which no expression of the system spells, and a start that
/// varies states nothing about the initialization instant on its own.
fn stated_value(view: dae::DaeView<'_>, variable: u32) -> Option<Vec<PinTerm>> {
    let start = view
        .variable_id(variable as usize)
        .and_then(|id| view.variable(id))
        .and_then(|variable| variable.start())
        .filter(|start| is_time_invariant(view, *start))?;
    Some(vec![PinTerm {
        expression: start.index(),
        negated: false,
    }])
}

/// Whether the runtime seeds `variable` from its own declaration.
fn is_seeded_state(view: dae::DaeView<'_>, variable: u32) -> bool {
    view.variable_id(variable as usize)
        .and_then(|id| view.variable(id))
        .is_some_and(|variable| variable.role() == dae::VariableRole::State)
}

fn variable_name(view: dae::DaeView<'_>, variable: u32) -> String {
    view.variable_id(variable as usize)
        .and_then(|id| view.variable(id))
        .map(|variable| variable.name().as_str().to_string())
        .unwrap_or_default()
}
