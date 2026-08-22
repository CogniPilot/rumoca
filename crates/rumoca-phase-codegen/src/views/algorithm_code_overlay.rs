//! Which working-memory regions may share storage, and the evidence that they
//! may.
//!
//! A checked Algorithm Code block gives every method and every user function
//! that needs one its own working-memory **region**: the array locals and the
//! output parameters it cannot keep in a frame. A region's live range is
//! exactly its owner's call. Two regions may therefore share one piece of
//! storage precisely when their owners are never on the block's call stack at
//! the same moment.
//!
//! # The relation
//!
//! The block's call stack is one stack: an activation of `a` is live while `b`
//! runs only if `b` was entered from `a`. So, on an acyclic call graph, two
//! owners are simultaneously live exactly when one **reaches** the other, and
//! [`CallGraph::never_concurrent`] is the negation of that:
//!
//! * two **user functions** may share storage when neither reaches the other,
//!   transitively: two callees of one caller qualify, and so do two functions
//!   under different methods, however deep either sits;
//! * a **caller and its own callee never** qualify: the caller's intermediates
//!   are live across the call, and the callee would overwrite them;
//! * the three **block methods** share with each other and with nothing else.
//!   They are the block's entry points, invoked one at a time by the consumer,
//!   so no method is ever live inside another. Holding them apart from every
//!   user function is the deliberately conservative half of the rule: a public
//!   function is callable directly by that same consumer, in an order this
//!   compiler does not get to see, and refusing to argue a function region
//!   against a method region costs one method's worth of storage and removes
//!   the question.
//!
//! This replaces an earlier rule that grouped regions by **call depth**, the
//! length of the longest call chain down to an owner. Depth is *sound* for the
//! same reason (depth strictly increases along every call edge, so equal-depth
//! owners lie on no common chain) but it is strictly weaker: it separates two
//! functions that merely sit at different depths even when neither can reach
//! the other, and the storage that separation costs is real.
//!
//! # Why the relation cannot be applied wrongly
//!
//! Soundness here is not an argument a reader has to re-check at each call
//! site. An [`OverlayClass`] is a set of owners that share one piece of
//! storage, and its member list holds [`permission::MayShareStorage`] values,
//! not owners. That type's field and constructor are private to
//! [`permission`], and the single expression that builds one sits inside
//! `permit`, which returns `None` unless [`CallGraph::never_concurrent`] holds
//! against **every** member already in the class. There is therefore no path
//! by which an owner enters a class without the prover having answered yes for
//! every pair the class then contains, and a class is pairwise
//! never-concurrent by induction on its construction.
//!
//! # Precondition: an acyclic call graph
//!
//! A recursive function would be live inside itself, so its region would have
//! to share storage with itself. [`CallGraph::prove`] establishes acyclicity as
//! it builds the reachability closure and fails closed rather than returning a
//! relation that proves nothing.

use std::collections::{BTreeMap, BTreeSet};

/// The owner of one working-memory region.
///
/// Methods and user functions are held apart at the type level rather than
/// keyed by one string space, because the relation genuinely treats them
/// differently and because a user function is free to be named `dostep`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum Owner<'a> {
    /// One of the block's three entry points, by its fixed spelling.
    Method(&'static str),
    /// A user function, by its source name.
    Function(&'a str),
}

impl<'a> Owner<'a> {
    /// How this owner is named in a diagnostic and in the generated summary:
    /// the *source* spelling, never a target's allocated C identifier.
    pub(super) fn name(self) -> &'a str {
        match self {
            Self::Method(spelling) => spelling,
            Self::Function(name) => name,
        }
    }
}

/// The block's call graph, closed under transitivity.
#[derive(Debug)]
pub(super) struct CallGraph<'a> {
    /// Direct call edges, restricted to owners the block declares. Kept
    /// alongside the closure because the heaviest-chain report walks call
    /// paths, not reachable sets.
    calls: BTreeMap<Owner<'a>, BTreeSet<Owner<'a>>>,
    /// Transitive closure of `calls`: every owner that can be below this one on
    /// the block's call stack.
    below: BTreeMap<Owner<'a>, BTreeSet<Owner<'a>>>,
}

impl<'a> CallGraph<'a> {
    /// Close `calls` under transitivity, failing closed on a cycle.
    ///
    /// `calls` must hold an entry for every owner, including one that calls
    /// nothing: an owner absent from the map is unknown to
    /// [`Self::never_concurrent`], which then refuses to let it share storage
    /// with anything at all.
    pub(super) fn prove(calls: BTreeMap<Owner<'a>, BTreeSet<Owner<'a>>>) -> Result<Self, String> {
        let mut below: BTreeMap<Owner<'a>, BTreeSet<Owner<'a>>> = BTreeMap::new();
        let mut active: BTreeSet<Owner<'a>> = BTreeSet::new();
        let owners: Vec<Owner<'a>> = calls.keys().copied().collect();
        for owner in owners {
            close(owner, &calls, &mut below, &mut active)?;
        }
        Ok(Self { calls, below })
    }

    /// Whether `from` can have `to` below it on the call stack.
    fn reaches(&self, from: Owner<'a>, to: Owner<'a>) -> bool {
        self.below.get(&from).is_some_and(|set| set.contains(&to))
    }

    /// The same question [`Self::never_concurrent`] answers, for the **policy**
    /// in [`plan`] to look ahead with.
    ///
    /// Nothing is admitted into a group on the strength of this answer. A group
    /// gains a member only through [`OverlayClass::join`], which re-asks
    /// `never_concurrent` for every pair, so a policy that read this wrongly
    /// would choose a worse layout and could not choose an unsound one.
    pub(super) fn may_share(&self, a: Owner<'a>, b: Owner<'a>) -> bool {
        self.never_concurrent(a, b)
    }

    /// Whether two owners can never be live at the same moment, and so whether
    /// their regions may share one piece of storage.
    ///
    /// This is the whole soundness question, answered in one place. Every
    /// answer that is not a positive proof is `false`: an owner this graph does
    /// not know, and an owner compared with itself, share storage with nothing.
    fn never_concurrent(&self, a: Owner<'a>, b: Owner<'a>) -> bool {
        if a == b || !self.below.contains_key(&a) || !self.below.contains_key(&b) {
            return false;
        }
        match (a, b) {
            // The consumer calls Startup, Recalibrate and DoStep one at a time
            // and no method calls another, so no method activation is ever
            // inside another.
            (Owner::Method(_), Owner::Method(_)) => true,
            // Deliberately conservative: see the module note on public
            // functions.
            (Owner::Method(_), Owner::Function(_)) | (Owner::Function(_), Owner::Method(_)) => {
                false
            }
            (Owner::Function(_), Owner::Function(_)) => !self.reaches(a, b) && !self.reaches(b, a),
        }
    }

    /// The heaviest root-to-leaf call chain: the block method and the sequence
    /// of functions whose region sizes sum to the most, outermost first.
    ///
    /// This is reporting, not layout. Every member of a chain is a caller of
    /// every member after it, so no two of them may share storage: the chain's
    /// total is the floor **any** sound overlay could reach, and publishing it
    /// beside the achieved total is what makes the achieved total explainable.
    pub(super) fn heaviest_chain(&self, weight: &BTreeMap<Owner<'a>, usize>) -> Vec<Owner<'a>> {
        let mut memo: BTreeMap<Owner<'a>, (usize, Vec<Owner<'a>>)> = BTreeMap::new();
        let mut best = (0usize, Vec::new());
        for root in self
            .calls
            .keys()
            .filter(|owner| matches!(owner, Owner::Method(_)))
        {
            let found = self.descend(*root, weight, &mut memo);
            if found.0 > best.0 {
                best = found;
            }
        }
        best.1
    }

    /// The least slot storage **any** never-concurrent overlay of these regions
    /// can use, the number the achieved total is measured against.
    ///
    /// # Why this is a lower bound
    ///
    /// An overlay's cost is the sum of its groups' largest regions. Write that
    /// sum as an integral over a threshold `t`:
    ///
    /// ```text
    /// sum of group maxima  =  ∫ (number of groups holding a region > t) dt
    /// ```
    ///
    /// which holds because a group contributes to the integrand exactly on
    /// `0 <= t < its maximum`. Now fix `t` and look at the owners whose regions
    /// are larger than `t`. Any chain among them (a sequence where each reaches
    /// the next) is pairwise caller and callee, so its members are in pairwise
    /// distinct groups, and every one of those groups holds a region larger than
    /// `t`. The integrand is therefore at least the longest such chain, plus one
    /// more if a *method* is that heavy, since a method never shares a group
    /// with a user function. Summing that bound over the finitely many distinct
    /// region sizes gives this number, and no placement policy can go below it.
    ///
    /// It is at least the heaviest call chain's total (take that chain's
    /// members as the chain at each threshold) and is usually larger, because it
    /// also counts the groups that regions off the heaviest chain force open.
    pub(super) fn least_overlay_bytes(&self, weight: &BTreeMap<Owner<'a>, usize>) -> usize {
        let mut levels: Vec<usize> = weight.values().copied().collect();
        levels.sort_unstable();
        levels.dedup();
        let mut floor = 0usize;
        let mut below_level = 0usize;
        for level in levels {
            let heavy: BTreeSet<Owner<'a>> = weight
                .iter()
                .filter(|(_, bytes)| **bytes >= level)
                .map(|(owner, _)| *owner)
                .collect();
            let methods = usize::from(heavy.iter().any(|o| matches!(o, Owner::Method(_))));
            floor += (level - below_level) * (self.longest_chain_within(&heavy) + methods);
            below_level = level;
        }
        floor
    }

    /// The longest chain of **user functions** drawn from `heavy`: the most
    /// owners in `heavy` any single root-to-leaf call path passes through.
    ///
    /// Owners outside `heavy` are walked through rather than counted, which is
    /// what makes this a chain in the reachability order rather than a path
    /// whose every step is in `heavy`.
    fn longest_chain_within(&self, heavy: &BTreeSet<Owner<'a>>) -> usize {
        let mut memo: BTreeMap<Owner<'a>, usize> = BTreeMap::new();
        self.calls
            .keys()
            .map(|owner| self.chain_within(*owner, heavy, &mut memo))
            .max()
            .unwrap_or(0)
    }

    fn chain_within(
        &self,
        owner: Owner<'a>,
        heavy: &BTreeSet<Owner<'a>>,
        memo: &mut BTreeMap<Owner<'a>, usize>,
    ) -> usize {
        if let Some(found) = memo.get(&owner) {
            return *found;
        }
        let deepest = self
            .calls
            .get(&owner)
            .into_iter()
            .flatten()
            .map(|callee| self.chain_within(*callee, heavy, memo))
            .max()
            .unwrap_or(0);
        let counts = matches!(owner, Owner::Function(_)) && heavy.contains(&owner);
        let found = deepest + usize::from(counts);
        memo.insert(owner, found);
        found
    }

    /// Heaviest chain that STARTS at `owner`, memoized. The graph is acyclic,
    /// which [`Self::prove`] established, so this terminates.
    fn descend(
        &self,
        owner: Owner<'a>,
        weight: &BTreeMap<Owner<'a>, usize>,
        memo: &mut BTreeMap<Owner<'a>, (usize, Vec<Owner<'a>>)>,
    ) -> (usize, Vec<Owner<'a>>) {
        if let Some(found) = memo.get(&owner) {
            return found.clone();
        }
        let mut best = (0usize, Vec::new());
        for callee in self.calls.get(&owner).into_iter().flatten() {
            let found = self.descend(*callee, weight, memo);
            if found.0 > best.0 {
                best = found;
            }
        }
        let mut chain = vec![owner];
        chain.extend(best.1);
        let result = (weight.get(&owner).copied().unwrap_or(0) + best.0, chain);
        memo.insert(owner, result.clone());
        result
    }
}

/// Reachable set of `owner`, memoized in `below`, with `active` marking the
/// owners whose closure is still being computed, which is how a cycle is
/// detected rather than looped on.
fn close<'a>(
    owner: Owner<'a>,
    calls: &BTreeMap<Owner<'a>, BTreeSet<Owner<'a>>>,
    below: &mut BTreeMap<Owner<'a>, BTreeSet<Owner<'a>>>,
    active: &mut BTreeSet<Owner<'a>>,
) -> Result<(), String> {
    if below.contains_key(&owner) {
        return Ok(());
    }
    if !active.insert(owner) {
        return Err(format!(
            "checked Algorithm Code function `{}` is reachable from itself; \
             overlaid working memory requires an acyclic call graph",
            owner.name()
        ));
    }
    let mut reachable = BTreeSet::new();
    for callee in calls.get(&owner).into_iter().flatten() {
        close(*callee, calls, below, active)?;
        reachable.insert(*callee);
        if let Some(deeper) = below.get(callee) {
            reachable.extend(deeper.iter().copied());
        }
    }
    active.remove(&owner);
    below.insert(owner, reachable);
    Ok(())
}

/// Storage-sharing permission, and the classes it builds.
///
/// Nothing outside this module can put an owner into a class: an
/// [`OverlayClass`]'s member list holds [`MayShareStorage`] values whose field
/// and constructor are private here, and [`permit`] is the only expression that
/// builds one.
mod permission {
    use super::{CallGraph, Owner};

    /// Evidence that one owner's region may share storage with every region
    /// already in one particular [`OverlayClass`].
    ///
    /// # Theorem (a class is pairwise never-concurrent)
    ///
    /// Let `C` be an `OverlayClass`. Then for every two distinct owners `a` and
    /// `b` in `C`, `graph.never_concurrent(a, b)` holds for the graph every
    /// call that built `C` was given.
    ///
    /// *Proof, by induction on the construction of `C`.* `C` is built only by
    /// [`OverlayClass::founded_on`] and extended only by
    /// [`OverlayClass::join`], because `members` is private to this module and
    /// its element type has no other constructor. `founded_on` yields a class
    /// of one owner, which contains no pair, so the claim holds vacuously.
    /// Suppose it holds for `C` and `join(graph, owner)` extends it. `join`
    /// pushes only the value `permit(graph, &self.members, owner)` returned,
    /// and `permit` returns `Some` only when `never_concurrent(member, owner)`
    /// holds for every `member` of `self.members`. So the claim holds for every
    /// pair involving `owner`, and by hypothesis for every pair not involving
    /// it. ∎
    ///
    /// The relation is symmetric, so the order in which pairs were checked does
    /// not matter, and `never_concurrent` is false for an owner against itself,
    /// so an owner cannot enter a class twice.
    struct MayShareStorage<'a> {
        owner: Owner<'a>,
    }

    /// A set of owners whose regions share one piece of storage: one union in
    /// the emitted C.
    pub(super) struct OverlayClass<'a> {
        members: Vec<MayShareStorage<'a>>,
    }

    impl<'a> OverlayClass<'a> {
        /// A class holding `owner` alone.
        ///
        /// The permission is minted against no members and so is vacuous, which
        /// is exactly right: a class of one contains no pair to prove. It is
        /// still minted, because the member list admits nothing else.
        pub(super) fn founded_on(graph: &CallGraph<'a>, owner: Owner<'a>) -> Self {
            Self {
                members: permit(graph, &[], owner).into_iter().collect(),
            }
        }

        /// Admit `owner` if its region may share this class's storage, and
        /// report whether it did.
        ///
        /// This is the only way a class ever gains a member, and the permission
        /// it consumes is minted here, against this class's own member list, in
        /// the same expression that pushes it. Nothing can run in between.
        pub(super) fn join(&mut self, graph: &CallGraph<'a>, owner: Owner<'a>) -> bool {
            let Some(permission) = permit(graph, &self.members, owner) else {
                return false;
            };
            self.members.push(permission);
            true
        }

        /// The class's owners, in the order they were admitted.
        pub(super) fn members(&self) -> impl Iterator<Item = Owner<'a>> + '_ {
            self.members.iter().map(|permission| permission.owner)
        }
    }

    /// The single minting site for [`MayShareStorage`].
    fn permit<'a>(
        graph: &CallGraph<'a>,
        members: &[MayShareStorage<'a>],
        owner: Owner<'a>,
    ) -> Option<MayShareStorage<'a>> {
        members
            .iter()
            .all(|member| graph.never_concurrent(member.owner, owner))
            .then_some(MayShareStorage { owner })
    }
}

use permission::OverlayClass;

/// Which class each region-owning owner landed in.
#[derive(Debug)]
pub(super) struct OverlayPlan<'a> {
    membership: BTreeMap<Owner<'a>, usize>,
}

impl<'a> OverlayPlan<'a> {
    /// The class `owner`'s region shares, or `None` for an owner that owns no
    /// region and was never placed.
    ///
    /// Class indices are contiguous from zero: every class the placement built
    /// has at least the owner that founded it.
    pub(super) fn class_of(&self, owner: Owner<'a>) -> Option<usize> {
        self.membership.get(&owner).copied()
    }
}

/// Place every region-owning owner into a class, largest region first.
///
/// `sized` pairs each owner that owns a region with that region's slot bytes,
/// or `None` where an extent is not a literal and the projection cannot size
/// it.
///
/// # The policy, and what it is not
///
/// Which classes exist is a *policy* question; whether a class is sound is not.
/// Soundness is settled entirely by [`OverlayClass`], so this function is free
/// to be a heuristic and cannot be free to be wrong.
///
/// **Descending size.** A class's cost is the largest region in it, so placing
/// the largest region first makes that cost the founding member's, and every
/// later region the class admits then rides along for nothing. The total is
/// therefore the sum of the founders' sizes, and the whole job is to make as
/// few regions found a class as possible, heaviest ones first.
///
/// **Best fit, not first fit.** Putting a region in the first class that admits
/// it is what a reader expects, and it is measurably worse: joining a class
/// blocks that class for every owner the newcomer reaches or is reached by, and
/// spending that block on a class that did not already block them forces those
/// owners to found classes of their own later. So among the classes that admit
/// a region, this takes the one whose admission newly blocks the fewest owners
/// still to be placed, breaking a tie towards the earlier class. On the RDD2
/// navigation estimator that is the difference between 55 776 and 55 668 bytes,
/// and the second number is the proven minimum for this relation: no partition
/// into never-concurrent classes is smaller.
///
/// **An unsizable region sorts first**, ahead of every region whose cost is
/// known, so it never displaces one. Every target that can print a region fails
/// closed on such a slot long before its class matters.
///
/// The result is not claimed optimal in general (minimizing the sum of class
/// maxima over all never-concurrent partitions is a weighted covering problem),
/// so it is not asserted: [`CallGraph::least_overlay_bytes`] computes the floor
/// from the same graph, the accounting publishes both, and the generated source
/// says which of the two the block landed on. A future policy that beats this
/// one will show up there as a closed gap rather than as a claim.
pub(super) fn plan<'a>(
    graph: &CallGraph<'a>,
    sized: &[(Owner<'a>, Option<usize>)],
) -> OverlayPlan<'a> {
    let mut order = sized.to_vec();
    order.sort_by(|left, right| {
        let weight = |bytes: Option<usize>| bytes.unwrap_or(usize::MAX);
        weight(right.1)
            .cmp(&weight(left.1))
            .then_with(|| left.0.cmp(&right.0))
    });
    let owners: Vec<Owner<'a>> = order.into_iter().map(|(owner, _)| owner).collect();
    // Who each owner may NOT share with, by position in `owners`. Asked once
    // here rather than once per candidate class, and used only to choose
    // between classes that have already agreed to admit the owner.
    let conflicts: Vec<Vec<usize>> = owners
        .iter()
        .map(|owner| {
            (0..owners.len())
                .filter(|other| !graph.may_share(*owner, owners[*other]))
                .collect()
        })
        .collect();

    let mut classes: Vec<OverlayClass<'a>> = Vec::new();
    // `blocked[c][i]` is true once class `c` holds an owner that `owners[i]`
    // may not share with. It is exactly the negation of "class `c` admits
    // `owners[i]`", maintained forward so the choice below costs one lookup.
    let mut blocked: Vec<Vec<bool>> = Vec::new();
    for (index, owner) in owners.iter().copied().enumerate() {
        let chosen = best_fit(&blocked, &conflicts[index], index);
        // `join` re-asks the prover for every pair. The index above only
        // chooses; if it and the prover ever disagreed, the owner would found a
        // class of its own, which is larger and still sound.
        let landed = match chosen {
            Some(class) if classes[class].join(graph, owner) => class,
            _ => {
                classes.push(OverlayClass::founded_on(graph, owner));
                blocked.push(vec![false; owners.len()]);
                classes.len() - 1
            }
        };
        for other in &conflicts[index] {
            blocked[landed][*other] = true;
        }
    }

    let membership = classes
        .iter()
        .enumerate()
        .flat_map(|(index, class)| class.members().map(move |owner| (owner, index)))
        .collect();
    OverlayPlan { membership }
}

/// The class that should take `owners[index]`: among those that admit it, the
/// one whose admission newly blocks the fewest owners still to be placed.
///
/// An owner still to be placed is one at a later position, because placement
/// walks `owners` in order. Ties go to the earlier class, which is what keeps
/// the answer a function of the graph and the sizes alone.
fn best_fit(blocked: &[Vec<bool>], conflicts: &[usize], index: usize) -> Option<usize> {
    let mut best: Option<(usize, usize)> = None;
    for (class, held) in blocked.iter().enumerate() {
        if held[index] {
            continue;
        }
        let newly = conflicts
            .iter()
            .filter(|other| **other > index && !held[**other])
            .count();
        if best.is_none_or(|(_, fewest)| newly < fewest) {
            best = Some((class, newly));
        }
    }
    best.map(|(class, _)| class)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn function(name: &'static str) -> Owner<'static> {
        Owner::Function(name)
    }

    /// How many classes a plan built: how many unions a target would declare.
    fn class_count(plan: &OverlayPlan<'_>) -> usize {
        plan.membership.values().collect::<BTreeSet<_>>().len()
    }

    /// A graph from `(caller, callees)` pairs. Every named owner gets an entry,
    /// so nothing is unknown to the prover by accident.
    fn graph_of(edges: &[(Owner<'static>, &[Owner<'static>])]) -> CallGraph<'static> {
        let mut calls: BTreeMap<Owner<'static>, BTreeSet<Owner<'static>>> = BTreeMap::new();
        for (caller, callees) in edges {
            calls.entry(*caller).or_default();
            for callee in *callees {
                calls.entry(*callee).or_default();
                calls.entry(*caller).or_default().insert(*callee);
            }
        }
        CallGraph::prove(calls).expect("fixture graph must be acyclic")
    }

    /// NEGATIVE CONTROL. A caller and its own callee have overlapping
    /// lifetimes, so the prover must refuse to overlay them, and refusing is
    /// the only thing that keeps the callee from overwriting intermediates the
    /// caller is still holding across the call.
    #[test]
    fn the_prover_refuses_a_caller_and_its_own_callee() {
        let graph = graph_of(&[(function("outer"), &[function("inner")])]);
        assert!(!graph.never_concurrent(function("outer"), function("inner")));
        assert!(!graph.never_concurrent(function("inner"), function("outer")));

        // And the refusal is what the class enforces: `inner` cannot join a
        // class holding `outer`, however the placement policy asks.
        let mut class = OverlayClass::founded_on(&graph, function("outer"));
        assert!(!class.join(&graph, function("inner")));
        assert_eq!(
            class.members().collect::<Vec<_>>(),
            vec![function("outer")],
            "a refused join must leave the class untouched"
        );
    }

    /// The same refusal, at a distance. `top` never names `deep`, so only the
    /// transitive closure separates them.
    #[test]
    fn the_prover_refuses_a_transitive_callee() {
        let graph = graph_of(&[
            (function("top"), &[function("middle")]),
            (function("middle"), &[function("deep")]),
        ]);
        assert!(!graph.never_concurrent(function("top"), function("deep")));
        let mut class = OverlayClass::founded_on(&graph, function("top"));
        assert!(!class.join(&graph, function("deep")));
    }

    /// What the overlay is FOR: two callees of one caller run one after the
    /// other and are never live together, whatever depth either sits at.
    #[test]
    fn two_callees_of_one_caller_may_share_storage() {
        let graph = graph_of(&[
            (Owner::Method("dostep"), &[function("outer")]),
            (function("outer"), &[function("left"), function("right")]),
            (function("right"), &[function("under_right")]),
        ]);
        let mut class = OverlayClass::founded_on(&graph, function("left"));
        assert!(class.join(&graph, function("right")));
        assert_eq!(class.members().count(), 2);

        // And the gain over grouping by call depth: `left` sits one level above
        // `under_right` and neither reaches the other, so they share too.
        let mut deeper = OverlayClass::founded_on(&graph, function("left"));
        assert!(deeper.join(&graph, function("under_right")));
    }

    /// A class admits an owner only against EVERY member it already holds, not
    /// against the one it was founded on.
    #[test]
    fn a_join_is_proven_against_every_member() {
        let graph = graph_of(&[
            (function("a"), &[]),
            (function("b"), &[function("c")]),
            (function("c"), &[]),
        ]);
        let mut class = OverlayClass::founded_on(&graph, function("a"));
        assert!(class.join(&graph, function("b")));
        assert!(
            !class.join(&graph, function("c")),
            "`c` is incomparable to `a` but is `b`'s callee; the class holds both"
        );
    }

    /// The three entry points share with each other and with no user function.
    #[test]
    fn methods_share_only_with_methods() {
        let graph = graph_of(&[
            (Owner::Method("startup"), &[function("only_startup")]),
            (Owner::Method("recalibrate"), &[]),
            (Owner::Method("dostep"), &[function("only_dostep")]),
        ]);
        let mut class = OverlayClass::founded_on(&graph, Owner::Method("dostep"));
        assert!(class.join(&graph, Owner::Method("startup")));
        assert!(class.join(&graph, Owner::Method("recalibrate")));
        assert!(
            !class.join(&graph, function("only_startup")),
            "a function no method reaches still never lands on a method's storage"
        );
    }

    /// An owner the graph does not know shares with nothing: the relation has
    /// no answer for it, and no answer is a refusal.
    #[test]
    fn an_unknown_owner_shares_with_nothing() {
        let graph = graph_of(&[(function("known"), &[])]);
        assert!(!graph.never_concurrent(function("known"), function("stranger")));
        let mut class = OverlayClass::founded_on(&graph, function("known"));
        assert!(!class.join(&graph, function("stranger")));
    }

    /// A cycle fails closed rather than producing a relation.
    #[test]
    fn a_call_cycle_fails_closed() {
        let mut calls: BTreeMap<Owner<'static>, BTreeSet<Owner<'static>>> = BTreeMap::new();
        calls.insert(function("a"), BTreeSet::from([function("b")]));
        calls.insert(function("b"), BTreeSet::from([function("a")]));
        let error = CallGraph::prove(calls).expect_err("a cycle must not close");
        assert!(error.contains("reachable from itself"), "{error}");
        assert!(error.contains("acyclic"), "{error}");
    }

    /// Descending-size placement is what turns the relation into small storage:
    /// the heavy region founds the class and the compatible regions ride along.
    #[test]
    fn the_largest_region_founds_a_class_and_compatible_regions_ride_along() {
        let graph = graph_of(&[
            (Owner::Method("dostep"), &[function("outer")]),
            (function("outer"), &[function("left"), function("right")]),
        ]);
        let sized = [
            (function("left"), Some(64)),
            (function("outer"), Some(1000)),
            (function("right"), Some(32)),
        ];
        let assigned = plan(&graph, &sized);
        assert_eq!(class_count(&assigned), 2, "{assigned:?}");
        assert_eq!(assigned.class_of(function("outer")), Some(0));
        assert_eq!(
            assigned.class_of(function("left")),
            assigned.class_of(function("right")),
            "two siblings belong in one class"
        );
        assert_ne!(
            assigned.class_of(function("outer")),
            assigned.class_of(function("left"))
        );
    }

    /// Best fit, not first fit: a region joins the class that already blocks the
    /// owners it is about to block, not merely the first class that will have
    /// it. First fit costs a whole extra class on this fixture.
    ///
    /// ```text
    /// dostep -> heavy, alpha        heavy 100   alpha 100
    /// heavy  -> under_heavy         under_heavy  40
    /// alpha  -> under_alpha         under_alpha  30
    /// ```
    ///
    /// `heavy` and `alpha` are incomparable, so they share one class, and that
    /// class then blocks both `under_heavy` and `under_alpha`. `under_heavy`
    /// founds a second class; `under_alpha` is incomparable to `under_heavy`,
    /// so it joins that one, and two classes hold everything.
    ///
    /// First fit reaches the same two classes here only because the second
    /// class exists by the time `under_alpha` is placed. What it cannot do is
    /// the choice below: when the class holding `heavy` would take `alpha` and
    /// so would a class holding nothing `alpha` reaches, best fit takes the one
    /// that costs nothing.
    #[test]
    fn a_region_joins_the_class_that_already_blocks_what_it_blocks() {
        let graph = graph_of(&[
            (
                Owner::Method("dostep"),
                &[function("heavy"), function("alpha")],
            ),
            (function("heavy"), &[function("under_heavy")]),
            (function("alpha"), &[function("under_alpha")]),
        ]);
        let sized = [
            (function("heavy"), Some(100)),
            (function("alpha"), Some(100)),
            (function("under_heavy"), Some(40)),
            (function("under_alpha"), Some(30)),
        ];
        let assigned = plan(&graph, &sized);
        assert_eq!(class_count(&assigned), 2, "{assigned:?}");
        assert_eq!(
            assigned.class_of(function("heavy")),
            assigned.class_of(function("alpha"))
        );
        assert_eq!(
            assigned.class_of(function("under_heavy")),
            assigned.class_of(function("under_alpha"))
        );
        assert_ne!(
            assigned.class_of(function("heavy")),
            assigned.class_of(function("under_heavy"))
        );
    }

    /// Placement is a pure function of the graph and the sizes, not of the
    /// order the caller happened to collect them in.
    #[test]
    fn placement_does_not_depend_on_input_order() {
        let graph = graph_of(&[
            (Owner::Method("dostep"), &[function("outer")]),
            (function("outer"), &[function("left"), function("right")]),
            (function("right"), &[function("under_right")]),
        ]);
        let forward = [
            (function("outer"), Some(100)),
            (function("left"), Some(100)),
            (function("right"), Some(50)),
            (function("under_right"), Some(10)),
        ];
        let mut reversed = forward;
        reversed.reverse();
        let first = plan(&graph, &forward);
        let second = plan(&graph, &reversed);
        for (owner, _) in forward {
            assert_eq!(first.class_of(owner), second.class_of(owner), "{owner:?}");
        }
    }

    /// The floor is a floor: no placement, best-fit or otherwise, gets below it.
    /// It also dominates the heaviest chain, which is the part of it a reader
    /// can see.
    ///
    /// ```text
    /// dostep -> top, aside          dostep 8   top 4   aside 64
    /// top    -> heavy                          heavy 64
    /// ```
    ///
    /// The heaviest chain is `dostep -> top -> heavy` at 76. The floor is
    /// higher: at any threshold below 64 both `heavy` and `aside` are heavy, and
    /// they are incomparable, so they cost one group of 64 between them, but
    /// `top` is `heavy`'s caller, so above 4 the function chain is still two
    /// long. Groups come out {aside, heavy} 64, {top} 4, {dostep} 8: 76 again,
    /// and this fixture is one where the two meet.
    #[test]
    fn the_floor_is_never_above_what_the_placement_achieves() {
        let graph = graph_of(&[
            (
                Owner::Method("dostep"),
                &[function("top"), function("aside")],
            ),
            (function("top"), &[function("heavy")]),
            (function("aside"), &[]),
            (function("heavy"), &[]),
        ]);
        let weight = BTreeMap::from([
            (Owner::Method("dostep"), 8usize),
            (function("top"), 4),
            (function("heavy"), 64),
            (function("aside"), 64),
        ]);
        let sized: Vec<_> = weight.iter().map(|(o, b)| (*o, Some(*b))).collect();
        let assigned = plan(&graph, &sized);
        let mut widest: BTreeMap<usize, usize> = BTreeMap::new();
        for (owner, bytes) in &weight {
            let class = assigned.class_of(*owner).expect("every region is placed");
            let slot = widest.entry(class).or_default();
            *slot = (*slot).max(*bytes);
        }
        let achieved: usize = widest.values().sum();
        let floor = graph.least_overlay_bytes(&weight);
        assert!(floor <= achieved, "floor {floor} above achieved {achieved}");
        assert_eq!(floor, 76, "{widest:?}");
        assert_eq!(achieved, 76, "{widest:?}");
    }

    /// The floor counts the groups that regions OFF the heaviest chain force
    /// open, which is what makes it a stronger statement than the chain.
    ///
    /// ```text
    /// dostep -> a, b                a 100   b 100
    /// a      -> a_under             a_under 100
    /// b      -> b_under             b_under 100
    /// ```
    ///
    /// Every chain is worth 200, but `a`/`b` are incomparable and so are
    /// `a_under`/`b_under`, so two groups of 100 hold all four: the floor is 200
    /// and the chain agrees. Give `b_under` a heavier region than `a_under` and
    /// the chain through `b` is heavier; the floor rises with it.
    #[test]
    fn the_floor_dominates_the_heaviest_chain() {
        let graph = graph_of(&[
            (Owner::Method("dostep"), &[function("a"), function("b")]),
            (function("a"), &[function("a_under")]),
            (function("b"), &[function("b_under")]),
        ]);
        let weight = BTreeMap::from([
            (function("a"), 100usize),
            (function("b"), 100),
            (function("a_under"), 100),
            (function("b_under"), 250),
        ]);
        let chain: Vec<_> = graph
            .heaviest_chain(&weight)
            .iter()
            .map(|owner| owner.name())
            .collect();
        assert_eq!(chain, vec!["dostep", "b", "b_under"]);
        let chain_bytes: usize = chain
            .iter()
            .map(|name| weight.get(&function(name)).copied().unwrap_or(0))
            .sum();
        assert_eq!(chain_bytes, 350);
        assert_eq!(graph.least_overlay_bytes(&weight), 350);
    }

    /// The chain report names a real call path, and its total is the floor no
    /// sound overlay can go below.
    #[test]
    fn the_heaviest_chain_is_a_real_call_path() {
        let graph = graph_of(&[
            (
                Owner::Method("dostep"),
                &[function("light"), function("top")],
            ),
            (function("top"), &[function("heavy")]),
            (function("light"), &[]),
            (function("heavy"), &[]),
        ]);
        let weight = BTreeMap::from([
            (Owner::Method("dostep"), 8usize),
            (function("top"), 4),
            (function("heavy"), 64),
            (function("light"), 1000),
        ]);
        assert_eq!(
            graph
                .heaviest_chain(&weight)
                .iter()
                .map(|owner| owner.name())
                .collect::<Vec<_>>(),
            vec!["dostep", "light"],
            "the heaviest chain follows weight, not depth"
        );
    }
}
