//! Where a callee's array output may LIVE in storage its caller already owns,
//! so the read-back that moves it there is never emitted.
//!
//! # What the read-back is
//!
//! A GALEC function returns its outputs in slots of its own context region and
//! the caller copies each one out (`model.c.jinja`'s `read_back`). That copy is
//! the whole of the calling convention's cost: it is a full traversal of the
//! aggregate, and the callee's slot and the caller's destination are two live
//! objects where the model has one value.
//!
//! # What this module does, and what it deliberately does not
//!
//! It does **not** add an out parameter. GALEC has no such concept
//! (`ast::Statement` has none), the `.alg` is re-parsed by
//! `rumoca-phase-parse-galec`, and out pointers were removed from this target
//! once already: passing a caller object by address lets a callee write it
//! *during* its body, so an operand that aliases the destination stops seeing
//! the pre-call value. The template records that rationale at `read_back`.
//!
//! Instead this is a pure C **storage placement**. The statement view is
//! untouched — `d := f(x)` stays exactly that, and `model.alg.jinja` prints the
//! same bytes with and without every placement here. Only two things change:
//!
//! * the callee's output slot leaves the callee's region and is reached through
//!   a typed pointer the callee's entry declares at the caller's slot, exactly
//!   as an arena slot is reached through a pointer at its arena offset; and
//! * the caller emits no read-back for that output.
//!
//! The value therefore lands in the destination during the call rather than
//! immediately after it, which is observable only to code that runs in between
//! — and nothing runs in between: the read-back is emitted inside the same
//! template macro as the call.
//!
//! # The obligations, and why each is discharged
//!
//! [`permission::Placement`] is the only permission to place an output, its
//! fields are private to [`permission`], and [`permission::place`] is the only
//! expression that builds one. It discharges all of:
//!
//! 1. **One call site.** The pointer the callee declares is a single static
//!    address, so a second call with a different destination could not be
//!    spelled. The whole block is searched, methods included; two call
//!    statements refuse the callee outright. Only a *protected* function is a
//!    candidate: a public one is part of the block's API and a consumer that
//!    calls it reads its outputs where the header says they are.
//! 2. **A destination the caller owns outright, and pays nothing to lend.** The
//!    destination is a whole, unsubscripted array slot of the *calling
//!    function's* region with the callee output's exact scalar and extents, and
//!    it is a PLAIN MEMBER of that region: claimed by neither the arm overlay,
//!    nor the value arena, nor the marshalling rewrite. A callee writes it
//!    through a pointer at a moment none of those three can see, so a slot any
//!    of them claimed may not be lent.
//!
//!    That fact is READ from a first projection ([`super::block`]), never
//!    forced on a second, and the difference is bytes. Forcing a slot out of
//!    the three to lend it buys one copy and pays for it in the storage they
//!    were saving: measured on the RDD2 estimator, that cost 2,472 B of
//!    scratch. Reading their answer costs nothing and declines exactly the
//!    slots that were paying their way. A block method is never a destination
//!    owner: its array locals are what the value arena places, so they would
//!    almost all decline anyway, and the second pass exists only for the
//!    functions.
//! 3. **Disjointness from every operand.** The callee writes the destination
//!    while its own body runs, so the destination must not be reachable from
//!    any actual. It cannot be: an emitted argument is spelled from the calling
//!    owner's own namespace (a local, an arena slot, or block state) and no
//!    argument this target emits ever names another owner's region member, so
//!    the only way an actual and the destination can meet is by naming the same
//!    local — which [`permission::place`] refuses by name. An output parameter
//!    of the caller may itself be placed in turn, which resolves it into an
//!    ancestor's region and opens one more route: an array actual that is a
//!    formal parameter aliases storage handed down from that ancestor. Such a
//!    call is refused.
//! 4. **A total write.** With the copy, the destination receives every element
//!    of the callee's slot. Without it, the destination keeps its own bytes
//!    wherever the callee writes none. The two agree exactly when the callee
//!    writes the whole output, so `place` demands a syntactic witness: one
//!    top-level statement writes the output, nothing else writes it anywhere,
//!    and that statement covers every element (a whole-object assignment, a
//!    whole-object multi-assignment slot, or a perfect unguarded nest over the
//!    declared extents).
//! 5. **Error paths.** GALEC has no early return: `signal` sets status bits and
//!    execution continues to the end of the body (`ast::Statement::Signal`). A
//!    function that raises therefore still reaches its total write, so the
//!    destination holds what the copy would have moved.
//! 6. **Reentrancy (SPEC_0034 GAL-039).** Every address minted here is a member
//!    path from `self`. No file-scope storage is introduced, and two state
//!    objects place their outputs in their own working memory.
//! 7. **Injectivity.** At most one output is placed at any one destination, so
//!    two callees can never be handed the same bytes. A chain — an output
//!    placed at a destination that is itself placed — is the one shape where
//!    several placements resolve to a single address, and it is sequential by
//!    construction: the inner callee writes it, and the outer one's read-back,
//!    which was its own total-write witness, is the statement that was dropped.
//!
//! Everything else fails closed. Declining costs one copy and nothing else.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use rumoca_ir_galec::ast;

use super::{Owner, output_parameters, slot_shape};

/// The storage a placed output is reached through: a slot of some owner's
/// region, named by the owner and the member.
///
/// Ordered, not hashed: the plan decides emitted storage, so every set and map
/// it keeps has to iterate in one order on every run, and a derived `Hash` over
/// string-shaped fields is refused by the string-hashing architecture gate.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct Destination<'a> {
    /// The owner whose region holds the member. Always a user function.
    pub(super) owner: &'a str,
    /// The member's source name inside that region.
    pub(super) member: &'a str,
}

/// Every placement the block admits, keyed by the callee and the output it
/// places.
#[derive(Debug, Default)]
pub(super) struct Plan<'a> {
    placed: BTreeMap<(&'a str, &'a str), Destination<'a>>,
}

impl<'a> Plan<'a> {
    /// Where `output` of `callee` lives, or `None` when it stays in `callee`'s
    /// own region.
    pub(super) fn destination_of(
        &self,
        callee: &'a str,
        output: &'a str,
    ) -> Option<&Destination<'a>> {
        self.placed.get(&(callee, output))
    }

    /// The output names `callee` delivers through a caller-owned destination.
    pub(super) fn placed_outputs(&self, owner: Owner<'a>) -> HashSet<&'a str> {
        let Owner::Function(callee) = owner else {
            return HashSet::new();
        };
        self.placed
            .keys()
            .filter(|(name, _)| *name == callee)
            .map(|(_, output)| *output)
            .collect()
    }

    /// Whether nothing is placed, which is when the caller may keep the
    /// projection it already has.
    pub(super) fn is_empty(&self) -> bool {
        self.placed.is_empty()
    }

    /// Whether the emitted body of `function` still spells `output` anywhere.
    ///
    /// A placed output is written by exactly one statement (the total-write
    /// witness) and read by nothing but its caller. When that one statement is
    /// itself a call whose result this plan placed at `output`, the read-back
    /// that named `output` is dropped and the emitted body mentions it nowhere
    /// at all: the middle link of a chain passes the value through and adds no
    /// text of its own. Its pointer would then be a declaration no path
    /// reaches, which the assurance profile reports as an unused variable, so
    /// this is what the emission asks before declaring one.
    pub(super) fn body_names_output(
        &self,
        function: &ast::UserFunction,
        output: &'a str,
        functions: &HashMap<&'a str, &'a ast::UserFunction>,
    ) -> bool {
        self.names(&function.statements, output, functions)
    }

    fn names(
        &self,
        statements: &[ast::Spanned<ast::Statement>],
        output: &str,
        functions: &HashMap<&'a str, &'a ast::UserFunction>,
    ) -> bool {
        statements.iter().any(|statement| match &statement.node {
            // The one shape whose target the emission drops. Every other
            // mention, including a subscripted target and any read, is printed.
            ast::Statement::Assignment {
                target,
                value: ast::Expression::Call(call),
            } => {
                let dropped = whole_named(target, output) && self.delivers(call, 0, functions);
                (!dropped && mentions_reference(target, output)) || mentioned_in_call(call, output)
            }
            ast::Statement::MultiAssignment { targets, call } => {
                targets.iter().enumerate().any(|(index, target)| {
                    let dropped =
                        whole_named(target, output) && self.delivers(call, index, functions);
                    !dropped && mentions_reference(target, output)
                }) || mentioned_in_call(call, output)
            }
            ast::Statement::Assignment { target, value } => {
                mentions_reference(target, output) || mentions_expression(value, output)
            }
            ast::Statement::Call(call) => mentioned_in_call(call, output),
            ast::Statement::If(conditional) => {
                conditional.branches.iter().any(|branch| {
                    mentions_condition(&branch.condition, output)
                        || self.names(&branch.body, output, functions)
                }) || conditional
                    .else_body
                    .as_ref()
                    .is_some_and(|body| self.names(body, output, functions))
            }
            ast::Statement::For(loop_statement) => {
                mentions_expression(&loop_statement.start, output)
                    || loop_statement
                        .step
                        .as_ref()
                        .is_some_and(|step| mentions_expression(step, output))
                    || mentions_expression(&loop_statement.stop, output)
                    || self.names(&loop_statement.body, output, functions)
            }
            ast::Statement::Limit(targets) => targets.iter().any(|target| match target {
                ast::LimitTarget::SelfState => false,
                ast::LimitTarget::Reference(reference) => mentions_reference(reference, output),
            }),
            ast::Statement::Signal(_) => false,
        })
    }

    /// Whether result `index` of `call` is placed, which is exactly when the
    /// emission drops that result's read-back.
    ///
    /// Asked by signature position, exactly as the template asks it: a target's
    /// place in the statement is the callee's output parameter at the same
    /// place, and a placement is keyed by the callee's own name for it.
    fn delivers(
        &self,
        call: &ast::FunctionCall,
        index: usize,
        functions: &HashMap<&'a str, &'a ast::UserFunction>,
    ) -> bool {
        let callee = call.function.lexeme();
        let Some(declared) = functions.get(callee) else {
            return false;
        };
        output_parameters(declared)
            .nth(index)
            .is_some_and(|parameter| {
                self.placed
                    .contains_key(&(callee, parameter.decl.name.lexeme()))
            })
    }
}

fn whole_named(reference: &ast::Reference, output: &str) -> bool {
    matches!(reference, ast::Reference::Local(part)
        if part.name.lexeme() == output && part.subscripts.is_empty())
}

fn mentions_reference(reference: &ast::Reference, output: &str) -> bool {
    let mut found = HashSet::new();
    mentions_in_reference(reference, &mut found);
    found.contains(output)
}

fn mentions_expression(expression: &ast::Expression, output: &str) -> bool {
    let mut found = HashSet::new();
    mentions(expression, &mut found);
    found.contains(output)
}

fn mentioned_in_call(call: &ast::FunctionCall, output: &str) -> bool {
    call.arguments
        .iter()
        .any(|argument| mentions_expression(argument, output))
}

fn mentions_condition(condition: &ast::Condition, output: &str) -> bool {
    match condition {
        ast::Condition::Expression(expression) => mentions_expression(expression, output),
        // A signal check's fallback is a checked expression this view never
        // projected, so a mention inside it is invisible here. Answering
        // "named" keeps the declaration, which is the safe direction.
        ast::Condition::SignalCheck(_) => true,
    }
}

/// One call statement, as the search over the block sees it.
struct CallSite<'a> {
    /// The function whose body spells the call, or `None` for a block method,
    /// which is never a destination owner.
    owner: Option<&'a str>,
    targets: Vec<&'a ast::Reference>,
    call: &'a ast::FunctionCall,
}

/// Plan every placement one block admits.
///
/// `plain_members` is the first projection's answer to the one question this
/// cannot ask of the AST: which of an owner's region slots own their storage
/// outright, having been claimed by neither the arm overlay, nor the value
/// arena, nor the marshalling rewrite. A destination must be one of those, and
/// reading it rather than forcing it is what keeps a placement from costing the
/// bytes those three mechanisms were saving. See [`super::block`].
pub(super) fn plan<'a>(
    block: &'a ast::Block,
    plain_members: &BTreeMap<&'a str, BTreeSet<&'a str>>,
) -> Plan<'a> {
    let functions: HashMap<&'a str, &'a ast::UserFunction> = block
        .protected_functions
        .iter()
        .chain(&block.public_functions)
        .map(|function| (function.name.lexeme(), function))
        .collect();
    let functions = &functions;
    let mut sites: HashMap<&'a str, Vec<CallSite<'a>>> = HashMap::new();
    for method in [&block.startup, &block.recalibrate, &block.do_step] {
        collect(&method.statements, None, functions, &mut sites);
    }
    for function in block
        .protected_functions
        .iter()
        .chain(&block.public_functions)
    {
        collect(
            &function.statements,
            Some(function.name.lexeme()),
            functions,
            &mut sites,
        );
    }

    // Declaration order, never hash order: the plan decides emitted storage, so
    // the same block must produce the same layout on every run.
    let mut placed: BTreeMap<(&'a str, &'a str), Destination<'a>> = BTreeMap::new();
    let mut claimed: BTreeSet<Destination<'a>> = BTreeSet::new();
    for callee in &block.protected_functions {
        let name = callee.name.lexeme();
        let Some([site]) = sites.get(name).map(Vec::as_slice) else {
            continue;
        };
        for placement in permission::place(callee, site, functions, plain_members, &claimed) {
            let (output, destination) = placement.into_parts();
            claimed.insert(destination.clone());
            placed.insert((name, output), destination);
        }
    }
    resolve(placed)
}

/// Follow every chain to the slot that actually owns the bytes.
///
/// A destination may itself be an output some other placement moved, which is
/// the shape a call chain produces: an inner callee's output is placed at its
/// caller's output, which is placed at ITS caller's slot. Only the last link
/// names storage, so every pointer is declared at that address. The walk is
/// bounded by the number of placements and drops a placement it cannot bottom
/// out — the block's acyclic call graph makes a cycle impossible, and this
/// refuses to depend on that proof having run.
fn resolve<'a>(placed: BTreeMap<(&'a str, &'a str), Destination<'a>>) -> Plan<'a> {
    let limit = placed.len();
    let mut plan = Plan::default();
    for (key, destination) in &placed {
        let mut root = destination.clone();
        let mut hops = 0;
        while let Some(next) = placed.get(&(root.owner, root.member)) {
            hops += 1;
            if hops > limit {
                break;
            }
            root = next.clone();
        }
        if hops > limit {
            continue;
        }
        plan.placed.insert(*key, root);
    }
    plan
}

/// Record every user call one statement list performs, at any depth.
fn collect<'a>(
    statements: &'a [ast::Spanned<ast::Statement>],
    owner: Option<&'a str>,
    functions: &HashMap<&'a str, &'a ast::UserFunction>,
    sites: &mut HashMap<&'a str, Vec<CallSite<'a>>>,
) {
    for statement in statements {
        match &statement.node {
            ast::Statement::Assignment {
                target,
                value: ast::Expression::Call(call),
            } => record(call, vec![target], owner, functions, sites),
            ast::Statement::MultiAssignment { targets, call } => {
                record(call, targets.iter().collect(), owner, functions, sites);
            }
            ast::Statement::Call(call) => record(call, Vec::new(), owner, functions, sites),
            ast::Statement::If(conditional) => {
                for branch in &conditional.branches {
                    collect(&branch.body, owner, functions, sites);
                }
                if let Some(body) = &conditional.else_body {
                    collect(body, owner, functions, sites);
                }
            }
            ast::Statement::For(loop_statement) => {
                collect(&loop_statement.body, owner, functions, sites);
            }
            ast::Statement::Assignment { .. }
            | ast::Statement::Limit(_)
            | ast::Statement::Signal(_) => {}
        }
    }
}

fn record<'a>(
    call: &'a ast::FunctionCall,
    targets: Vec<&'a ast::Reference>,
    owner: Option<&'a str>,
    functions: &HashMap<&'a str, &'a ast::UserFunction>,
    sites: &mut HashMap<&'a str, Vec<CallSite<'a>>>,
) {
    let name = call.function.lexeme();
    if !functions.contains_key(name) {
        return;
    }
    sites.entry(name).or_default().push(CallSite {
        owner,
        targets,
        call,
    });
}

/// Every local name an expression mentions, at any depth, including subscripts.
fn mentions<'a>(expression: &'a ast::Expression, found: &mut HashSet<&'a str>) {
    match expression {
        ast::Expression::Bool(_) | ast::Expression::Integer(_) | ast::Expression::Real(_) => {}
        ast::Expression::Ref(reference) | ast::Expression::Neg(reference) => {
            mentions_in_reference(reference, found);
        }
        ast::Expression::Paren(inner) | ast::Expression::Not(inner) => mentions(inner, found),
        ast::Expression::Binary { lhs, rhs, .. } => {
            mentions(lhs, found);
            mentions(rhs, found);
        }
        ast::Expression::Call(call) => {
            for argument in &call.arguments {
                mentions(argument, found);
            }
        }
        ast::Expression::If(conditional) => {
            for (condition, value) in &conditional.branches {
                mentions(condition, found);
                mentions(value, found);
            }
            mentions(&conditional.else_value, found);
        }
        ast::Expression::Array(elements) => {
            for element in elements {
                mentions(element, found);
            }
        }
        ast::Expression::Size { array, dimension } => {
            mentions_in_reference(array, found);
            mentions(dimension, found);
        }
    }
}

fn mentions_in_reference<'a>(reference: &'a ast::Reference, found: &mut HashSet<&'a str>) {
    let parts = match reference {
        ast::Reference::Local(part) => {
            found.insert(part.name.lexeme());
            std::slice::from_ref(part)
        }
        ast::Reference::State(parts) => parts.as_slice(),
    };
    for part in parts {
        for subscript in &part.subscripts {
            mentions(subscript, found);
        }
    }
}

/// Permission to place one output, and the only place it is minted.
mod permission {
    use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

    use rumoca_ir_galec::ast;

    use super::{CallSite, Destination, mentions, output_parameters, slot_shape};

    /// Evidence that one callee output may live in one caller-owned slot.
    ///
    /// # Theorem (a placement preserves every emitted value)
    ///
    /// Let `P` place output `o` of callee `f` at slot `d` of calling function
    /// `A`. Emitting `f`'s references to `o` against `d`'s storage and dropping
    /// the read-back leaves every object other than `f`'s retired slot holding
    /// the value it held before, at every point after the call returns.
    ///
    /// *Proof.* [`place`] establishes, in the expression that builds `P`:
    ///
    /// 1. `f` is protected and has exactly one call statement in the whole
    ///    block, and it is in `A`. So `d` is the destination of every activation
    ///    of `f`, and no consumer of the block calls `f` at all.
    /// 2. `d` is a whole, unsubscripted array slot of `A`'s region with `o`'s
    ///    scalar and extents, and `A` keeps it as a plain member (the projection
    ///    pins it out of the arm overlay, the value arena, bound equalization
    ///    and marshalling retirement). So `d` is at a fixed offset from `self`,
    ///    its bytes belong to nothing else, and element `i` of `o` and element
    ///    `i` of `d` are the same byte range.
    /// 3. No actual of the call names `d`, and — when `d` is an output parameter
    ///    of `A`, and so may itself be placed into an ancestor's region — no
    ///    actual is an array formal of `A`. With the module note's observation
    ///    that an emitted argument never names another owner's region member,
    ///    `d` is disjoint from every operand `f` reads.
    /// 4. Exactly one statement of `f` writes `o`, it is at top level, and it
    ///    writes every element.
    /// 5. `d` is claimed by no other placement.
    ///
    /// Before the call, `d` holds some value and `f`'s slot another; both are
    /// dead, because (4) makes the call's write total and (2) makes it cover
    /// exactly `d`. During the call `f` reads only its actuals, which by (3) are
    /// disjoint from `d`, so every value `f` computes is the value it computed
    /// before. `f` writes those values over `d` rather than over its own slot,
    /// and by (4) it writes all of them, so on return `d` holds exactly what the
    /// read-back would have moved into it. Nothing observes `d` between the call
    /// and the dropped read-back: the read-back is emitted inside the call's own
    /// macro, with no statement between. By (5) no other callee writes `d`, and
    /// by (1) no other activation of `f` does. ∎
    pub(super) struct Placement<'a> {
        output: &'a str,
        destination: Destination<'a>,
    }

    impl<'a> Placement<'a> {
        pub(super) fn into_parts(self) -> (&'a str, Destination<'a>) {
            (self.output, self.destination)
        }
    }

    /// Mint a placement for every output of `callee` that its one call site
    /// admits.
    pub(super) fn place<'a>(
        callee: &'a ast::UserFunction,
        site: &CallSite<'a>,
        functions: &HashMap<&'a str, &'a ast::UserFunction>,
        plain_members: &BTreeMap<&'a str, BTreeSet<&'a str>>,
        claimed: &BTreeSet<Destination<'a>>,
    ) -> Vec<Placement<'a>> {
        let mut minted = Vec::new();
        // A block method never lends a destination: its array locals are the
        // ones the value arena places.
        let Some(owner) = site.owner else {
            return minted;
        };
        // A self-call would need the region live inside itself. The block's
        // acyclicity proof already refuses that; refusing locally means this
        // permission does not depend on that proof having run.
        if owner == callee.name.lexeme() {
            return minted;
        }
        let Some(caller) = functions.get(owner).copied() else {
            return minted;
        };
        let outputs: Vec<&'a ast::Parameter> = output_parameters(callee).collect();
        if outputs.is_empty() || outputs.len() != site.targets.len() {
            return minted;
        }
        let outputs_of_caller: HashSet<&'a str> = output_parameters(caller)
            .map(|parameter| parameter.decl.name.lexeme())
            .collect();
        let mut operands = HashSet::new();
        for argument in &site.call.arguments {
            mentions(argument, &mut operands);
        }
        let array_formal_operand = caller.parameters.iter().any(|parameter| {
            parameter.direction == ast::Direction::Input
                && !parameter.decl.dimensions.is_empty()
                && operands.contains(parameter.decl.name.lexeme())
        });

        for (output, target) in outputs.iter().zip(&site.targets) {
            let Some(shape) = slot_shape(&output.decl) else {
                continue;
            };
            // A scalar output is read back by a single assignment: there is no
            // traversal to remove and no array slot to retire.
            if shape.1.is_empty() {
                continue;
            }
            let ast::Reference::Local(part) = target else {
                continue;
            };
            if !part.subscripts.is_empty() {
                continue;
            }
            let member = part.name.lexeme();
            // The destination has to be a slot `A`'s region holds as a plain
            // member of its own, at a fixed offset, with exactly this shape.
            //
            // "Plain member" is the first projection's answer, not a demand
            // made of the second, and the difference is bytes. A callee writes
            // the destination through a pointer at a moment neither the arm
            // overlay, nor the value arena, nor the marshalling rewrite can
            // see, so a slot any of the three claimed may not be lent. FORCING
            // a slot out of them to lend it would buy one copy and pay for it
            // in storage those mechanisms were saving: measured on the RDD2
            // estimator, that cost 2,472 B of scratch. READING their answer
            // costs nothing and declines exactly the slots that were paying
            // their way.
            if !plain_members
                .get(owner)
                .is_some_and(|members| members.contains(member))
            {
                continue;
            }
            let Some(declaration) = caller
                .parameters
                .iter()
                .map(|parameter| &parameter.decl)
                .chain(&caller.locals)
                .find(|declaration| declaration.name.lexeme() == member)
            else {
                continue;
            };
            if slot_shape(declaration).as_ref() != Some(&shape) {
                continue;
            }
            // The destination is the caller's own slot, so an operand naming it
            // is the aliasing case this cannot reason about.
            if operands.contains(member) {
                continue;
            }
            // An output parameter of `A` may itself be placed, which resolves
            // this destination into an ancestor's region where an array formal
            // of `A` can alias. The question is asked of the shape rather than
            // of whether that further placement has been decided yet, so the
            // answer does not depend on the order the plan considers callees in.
            if outputs_of_caller.contains(member) && array_formal_operand {
                continue;
            }
            let destination = Destination { owner, member };
            if claimed.contains(&destination)
                || minted
                    .iter()
                    .any(|other: &Placement<'a>| other.destination == destination)
            {
                continue;
            }
            if !writes_whole_output(callee, output.decl.name.lexeme(), &shape.1) {
                continue;
            }
            minted.push(Placement {
                output: output.decl.name.lexeme(),
                destination,
            });
        }
        minted
    }

    /// Whether `output` is written by exactly one top-level statement of
    /// `function`, that statement writes every element, and nothing else in the
    /// body writes it.
    ///
    /// `writes` counts every write inside a top-level statement, so a second
    /// writer anywhere — nested or not — makes some top-level statement's count
    /// exceed the one its total-write shape accounts for, or makes a second
    /// top-level statement report a write. Both are refused.
    fn writes_whole_output(function: &ast::UserFunction, output: &str, extents: &[usize]) -> bool {
        let mut witnessed = false;
        for statement in &function.statements {
            // Exactly one write, and it is the one the shape accounts for.
            // Every shape `covers_whole` admits performs a single write to the
            // output, so a second one inside the same statement is a partial
            // write the coverage argument does not cover.
            match writes(&statement.node, output) {
                0 => continue,
                1 => {}
                _ => return false,
            }
            if witnessed || !covers_whole(&statement.node, output, extents) {
                return false;
            }
            witnessed = true;
        }
        witnessed
    }

    /// How many writes to `output` one statement performs, counting the writes
    /// of every statement nested inside it.
    fn writes(statement: &ast::Statement, output: &str) -> usize {
        match statement {
            ast::Statement::Assignment { target, .. } => usize::from(names(target, output)),
            ast::Statement::MultiAssignment { targets, .. } => targets
                .iter()
                .filter(|target| names(target, output))
                .count(),
            ast::Statement::Call(_) | ast::Statement::Signal(_) => 0,
            ast::Statement::Limit(targets) => targets
                .iter()
                .filter(|target| match target {
                    // `limit self` saturates block state, never a local.
                    ast::LimitTarget::SelfState => false,
                    ast::LimitTarget::Reference(reference) => names(reference, output),
                })
                .count(),
            ast::Statement::If(conditional) => conditional
                .branches
                .iter()
                .flat_map(|branch| &branch.body)
                .chain(conditional.else_body.iter().flatten())
                .map(|inner| writes(&inner.node, output))
                .sum(),
            ast::Statement::For(loop_statement) => loop_statement
                .body
                .iter()
                .map(|inner| writes(&inner.node, output))
                .sum(),
        }
    }

    fn names(reference: &ast::Reference, output: &str) -> bool {
        matches!(reference, ast::Reference::Local(part) if part.name.lexeme() == output)
    }

    /// Whether one top-level statement writes every element of `output`.
    ///
    /// Three shapes qualify, and nothing else does: a whole-object assignment
    /// and a whole-object multi-assignment slot, both of which the target emits
    /// as a traversal of the declared array; and a perfect unguarded `for` nest
    /// over the declared extents whose innermost statement assigns the element
    /// those iterators address.
    fn covers_whole(statement: &ast::Statement, output: &str, extents: &[usize]) -> bool {
        match statement {
            ast::Statement::Assignment { target, .. } => whole(target, output),
            ast::Statement::MultiAssignment { targets, .. } => {
                targets
                    .iter()
                    .filter(|target| whole(target, output))
                    .count()
                    == 1
            }
            ast::Statement::For(_) => nest_covers(statement, output, extents, &mut Vec::new()),
            ast::Statement::Call(_)
            | ast::Statement::If(_)
            | ast::Statement::Limit(_)
            | ast::Statement::Signal(_) => false,
        }
    }

    fn whole(reference: &ast::Reference, output: &str) -> bool {
        matches!(reference, ast::Reference::Local(part)
            if part.name.lexeme() == output && part.subscripts.is_empty())
    }

    /// Whether a perfect `for` nest traverses `extents` once and assigns the
    /// element its iterators address.
    fn nest_covers<'a>(
        statement: &'a ast::Statement,
        output: &str,
        extents: &[usize],
        iterators: &mut Vec<&'a str>,
    ) -> bool {
        match statement {
            ast::Statement::For(loop_statement) => {
                let Some(iterator) = &loop_statement.iterator else {
                    return false;
                };
                let Some(extent) = extents.get(iterators.len()) else {
                    return false;
                };
                if loop_statement.step.is_some()
                    || loop_statement.body.len() != 1
                    || !matches!(loop_statement.start, ast::Expression::Integer(1))
                    || !matches!(loop_statement.stop, ast::Expression::Integer(stop)
                        if usize::try_from(stop).ok() == Some(*extent))
                    || iterators.contains(&iterator.lexeme())
                {
                    return false;
                }
                iterators.push(iterator.lexeme());
                let covered = nest_covers(&loop_statement.body[0].node, output, extents, iterators);
                iterators.pop();
                covered
            }
            ast::Statement::Assignment { target, .. } => {
                if iterators.len() != extents.len() {
                    return false;
                }
                let ast::Reference::Local(part) = target else {
                    return false;
                };
                part.name.lexeme() == output
                    && part.subscripts.len() == iterators.len()
                    && part
                        .subscripts
                        .iter()
                        .zip(iterators.iter())
                        .all(|(subscript, iterator)| {
                            matches!(subscript, ast::Expression::Ref(ast::Reference::Local(index))
                                if index.name.lexeme() == *iterator
                                    && index.subscripts.is_empty())
                        })
            }
            ast::Statement::MultiAssignment { .. }
            | ast::Statement::Call(_)
            | ast::Statement::If(_)
            | ast::Statement::Limit(_)
            | ast::Statement::Signal(_) => false,
        }
    }
}
