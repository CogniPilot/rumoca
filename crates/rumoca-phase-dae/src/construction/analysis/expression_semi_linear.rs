//! The MLS §3.7.4.5 Rule 1 / Rule 2 equation-set owner for `semiLinear`.
//!
//! §3.7.4.5 defines `semiLinear(x, positiveSlope, negativeSlope)` as
//! `smooth(0, if x >= 0 then positiveSlope*x else negativeSlope*x)` and then
//! states the defect this module owns: *"In some situations, equations with the
//! `semiLinear` function become underdetermined if the first argument (`x`)
//! becomes zero, i.e., there is an infinite number of solutions. It is
//! recommended that the following rules are used to transform the equations
//! during the translation phase in order to select one meaningful solution in
//! such cases."* Both rules rewrite a *set* of equations, so neither can live
//! in the expression lowering of one call.
//!
//! Rule 1, verbatim from §3.7.4.5, replaces
//!
//! ```text
//! y = semiLinear(x, sa, s1);
//! y = semiLinear(x, s1, s2);
//! ...
//! y = semiLinear(x, sN, sb);
//! ```
//!
//! by
//!
//! ```text
//! s1 = if x >= 0 then sa else sb;
//! s2 = s1;
//! ...
//! sN = sN-1;
//! y = semiLinear(x, sa, sb);
//! ```
//!
//! and Rule 2 replaces `x = 0; y = 0; y = semiLinear(x, sa, sb);` by
//! `x = 0; y = 0; sa = sb;`. Both keep the equation count, so the source
//! balance certificate that ran on the untransformed rows still holds.
//!
//! # Why the chain is not written that way in a connected model
//!
//! No MSL model literally writes the chain. `Modelica.Thermal.FluidHeatFlow`
//! writes one `port.H_flow = semiLinear(port.m_flow, port.h, h)` per port, and
//! the connection equations `pa.m_flow + pb.m_flow = 0`,
//! `pa.H_flow + pb.H_flow = 0`, `pa.h = pb.h` are what make two such equations
//! share one `x` and one `y`. §3.7.4.5 supplies the identity that closes the
//! gap — *"`semiLinear(m_flow, port_h, h)` is identical to
//! `-semiLinear(-m_flow, h, port_h)`"* — so an equation whose `x` and `y` are
//! both the negation of the group's is the same equation with its two slopes
//! exchanged. [`SignedAliases`] proves those `≡`/`≡ -` facts exactly, from
//! unconditional continuous equations only, and normalization applies the
//! identity. Nothing here inspects a name.
//!
//! # Acceptance contract
//!
//! A row is *recognized* when its residual is exactly `y - semiLinear(x, a, b)`
//! or `semiLinear(x, a, b) - y` with `y`, `x`, `a`, `b` all subscript-free
//! scalar variable references, and the row is lowered as an unconditional
//! continuous equation. Recognized rows are grouped by the signed alias class
//! of `(y, x)`. A group is *transformed* when
//!
//! * Rule 2: the `x` class and the `y` class are both proven identically zero;
//!   every row becomes `a - b`.
//! * Rule 1: every member carries the same `(y, x)` sign skew as the group's
//!   first member — that is, every member either states the group's canonical
//!   pair or states its exact negation, never one of the two negated alone —
//!   every slope is alias-equal (not anti-equal) to its class, and the
//!   normalized positive/negative slope pairs form one simple directed path
//!   over at least two rows.
//!
//! The skew is measured, not assumed to be zero. [`SignedAliases::opposite`]
//! reports a sign *relative to the class root*, and the `y` class and the `x`
//! class pick their roots independently (see [`SignedAliases`]), so the two
//! sign bits of one row carry an arbitrary common offset. Testing them for
//! equality would make acceptance depend on which operand a connection balance
//! happened to name first; testing them against the group's own first member
//! does not.
//!
//! Every other shape is left exactly as written. That is deliberate and is the
//! SPEC 0008 acceptance decision for this owner: §3.7.4.5 *recommends* the
//! transformation, so the untransformed equation set is conformant, and a
//! structurally identical model whose `x` never reaches zero (for example
//! `FluidHeatFlow.Examples.SimpleCooling`, whose flow is constant) simulates
//! correctly without it. Rejecting the shape would reject those models too, so
//! this owner never rejects; it transforms what it can prove.
//!
//! # What "every other shape" covers, and why that is not a gap
//!
//! The largest untransformed family is the junction of more than two ports.
//! Such a node's balance is `Ha + Hb + Hc = 0`: three live terms, which
//! [`SignedAliases`] deliberately does not read as an alias, because a
//! three-term sum proves no `≡`/`≡ -` fact about any one pair. Each `H` then
//! roots its own class, every group holds a single row, and
//! [`Chain::assemble`] rejects it for having fewer than two edges.
//!
//! This is a per-node property, not a per-model one, and no `FluidHeatFlow`
//! example is on one side of it alone. `ParallelPumpDropOut` and `TwoMass` have
//! the same mixed topology: each joins three `FlowPort`s at a node (in
//! `ParallelPumpDropOut`, `pump.flowPort_b` with `pipe1`/`pipe2.flowPort_a`),
//! and each *also* carries ordinary two-port nodes whose rows this owner does
//! chain and rewrite. A model is therefore never simply "transformed" or
//! "untransformed": its two-port chains transform and its junction rows stay as
//! written, side by side in one equation set.
//!
//! That is the honest answer rather than a missing case. §3.7.4.5 states Rule 1
//! over a *chain*: a sequence of equations that all share one `y` and one `x`,
//! whose slopes link end to end. A three-way junction shares neither — the
//! three port enthalpy flows are three different variables constrained only by
//! one sum — so the MLS supplies no rule, and there is no "one meaningful
//! solution" for this owner to select. The junction's mixing enthalpy stays
//! underdetermined at exactly zero flow, which is the defect §3.7.4.5
//! describes and only *recommends* transforming away where its rules apply.
//! rumoca therefore leaves those rows exactly as the model wrote them.
//!
//! # Events
//!
//! The relation Rule 1 writes is not given a §8.5 crossing owner. It is the
//! same `x >= 0` relation §3.7.4.5 puts inside the operator's own
//! `smooth(0, ...)`, and §3.7.5 leaves a tool free not to generate events for
//! relations inside `smooth`; rumoca already takes that freedom for the
//! operator itself, and keeps taking it for the relation the rule lifts out of
//! it. OMC resolves it the same way: in its generated C for
//! `FluidHeatFlow.Examples.PumpDropOut` the Rule 1 selectors appear as ordinary
//! equations (`tmp = GreaterEq(pipe.flowPort_a.m_flow, 0.0)` feeding
//! `pump.flowPort_b.h`), while the model's zero-crossing table lists only the
//! four `volumeFlow` ramp time events and the two `pipe.volumeFlow` friction
//! relations. Analysis therefore keys no plan on the generated relation's span,
//! and the recognizer only accepts operands that are variable references, so no
//! relation is ever discarded together with the residual it replaced.

use super::*;

/// The replacement residual of every transformed `flat::Model::equations` row.
///
/// A Rule 1 chain of `K` rows contributes `K` entries: one
/// `s1 - (if x >= 0 then sa else sb)`, then `K - 2` links `si - si-1`, then the
/// surviving `y - semiLinear(x, sa, sb)`. A Rule 2 group contributes one
/// `sa - sb` per row.
#[derive(Default)]
pub(in crate::construction) struct SemiLinearRules {
    rows: HashMap<usize, Expression>,
}

impl SemiLinearRules {
    /// The residual row `index` is lowered from, when a rule replaced it.
    pub(in crate::construction) fn residual(&self, index: usize) -> Option<&Expression> {
        self.rows.get(&index)
    }
}

/// Which `flat::Model::equations` rows this owner may read as a fact and which
/// it may replace.
///
/// The two sets differ on purpose. A `≡`/`≡ -`/`≡ 0` fact only has to be an
/// unconditional statement of the model, which every scalar row of an unclocked
/// partition is — including rows some *other* owner lowers, such as the
/// constant binding a derived parameter is built from. That distinction is what
/// lets Rule 2 see the `x = 0` an unconnected flow variable states. Replacing a
/// row, by contrast, requires that this owner is the row's only owner, so the
/// replacement set additionally excludes every row already claimed elsewhere.
pub(in crate::construction) struct SemiLinearRowFilter<'analysis> {
    pub(in crate::construction) excluded: &'analysis HashSet<usize>,
    pub(in crate::construction) records: &'analysis HashMap<usize, RecordEquationPlan>,
    pub(in crate::construction) clocked: &'analysis HashMap<usize, ClockPlan>,
}

impl SemiLinearRowFilter<'_> {
    /// A row whose residual states an unconditional fact about the model.
    fn admits_fact(&self, index: usize, equation: &flat::Equation) -> bool {
        equation.scalar_count == 1 && !self.clocked.contains_key(&index)
    }

    /// A row this owner may rewrite: unconditional, unclocked, continuous, and
    /// claimed by no other semantic owner.
    fn admits_rewrite(&self, index: usize, equation: &flat::Equation, continuous: bool) -> bool {
        self.admits_fact(index, equation)
            && continuous
            && !self.excluded.contains(&index)
            && !self.records.contains_key(&index)
    }
}

/// Prove the MLS §3.7.4.5 rule transformations over the model equation set.
pub(super) fn analyze_semi_linear_rules(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    connection_ranks: &HashMap<VarName, usize>,
    aggregate_connections: &AggregateDiscreteConnections,
    filter: &SemiLinearRowFilter<'_>,
) -> SemiLinearRules {
    let facts: Vec<usize> = flat
        .equations
        .iter()
        .enumerate()
        .filter(|(index, equation)| filter.admits_fact(*index, equation))
        .map(|(index, _)| index)
        .collect();
    let rows: Vec<usize> = facts
        .iter()
        .copied()
        .filter(|index| {
            let equation = &flat.equations[*index];
            filter.admits_rewrite(
                *index,
                equation,
                matches!(
                    equation_partition(
                        flat,
                        *index,
                        equation,
                        roles,
                        connection_ranks,
                        aggregate_connections,
                    ),
                    Ok(EquationPartition::Continuous)
                ),
            )
        })
        .collect();
    let aliases = SignedAliases::prove(flat, &facts);
    let calls: Vec<SemiLinearCall> = rows
        .iter()
        .filter_map(|index| recognize(flat, *index))
        .collect();
    let mut rules = SemiLinearRules::default();
    for group in group_by_pair(&aliases, &calls) {
        transform_group(&aliases, &calls, &group, &mut rules);
    }
    rules
}

/// One recognized `y = semiLinear(x, a, b)` row, kept as source expressions so
/// every emitted operand is an occurrence the model itself wrote.
struct SemiLinearCall {
    row: usize,
    span: Span,
    value: VarOccurrence,
    operand: VarOccurrence,
    positive: VarOccurrence,
    negative: VarOccurrence,
}

/// A subscript-free scalar variable reference and its flat ordinal.
#[derive(Clone)]
struct VarOccurrence {
    ordinal: u32,
    expression: Expression,
}

fn recognize(flat: &flat::Model, row: usize) -> Option<SemiLinearCall> {
    let equation = &flat.equations[row];
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &equation.residual
    else {
        return None;
    };
    let (value, call) = match (semi_linear_arguments(lhs), semi_linear_arguments(rhs)) {
        (None, Some(arguments)) => (lhs.as_ref(), arguments),
        (Some(arguments), None) => (rhs.as_ref(), arguments),
        _ => return None,
    };
    let [operand, positive, negative] = call else {
        return None;
    };
    Some(SemiLinearCall {
        row,
        span: equation.span,
        value: occurrence(flat, value)?,
        operand: occurrence(flat, operand)?,
        positive: occurrence(flat, positive)?,
        negative: occurrence(flat, negative)?,
    })
}

fn semi_linear_arguments(expression: &Expression) -> Option<&[Expression]> {
    match expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::SemiLinear,
            args,
            ..
        } => Some(args.as_slice()),
        _ => None,
    }
}

fn occurrence(flat: &flat::Model, expression: &Expression) -> Option<VarOccurrence> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let ordinal = flat.variables.get_index_of(name.var_name())?;
    Some(VarOccurrence {
        ordinal: u32::try_from(ordinal).ok()?,
        expression: expression.clone(),
    })
}

/// Recognized rows that share one signed `(y, x)` class pair, in row order.
struct Group {
    members: Vec<usize>,
}

fn group_by_pair(aliases: &SignedAliases, calls: &[SemiLinearCall]) -> Vec<Group> {
    let mut order: Vec<(u32, u32)> = Vec::new();
    let mut members: HashMap<(u32, u32), Vec<usize>> = HashMap::new();
    for (position, call) in calls.iter().enumerate() {
        let key = (
            aliases.root(call.value.ordinal),
            aliases.root(call.operand.ordinal),
        );
        let slot = members.entry(key).or_default();
        if slot.is_empty() {
            order.push(key);
        }
        slot.push(position);
    }
    order
        .into_iter()
        .filter_map(|key| members.remove(&key))
        .map(|members| Group { members })
        .collect()
}

fn transform_group(
    aliases: &SignedAliases,
    calls: &[SemiLinearCall],
    group: &Group,
    rules: &mut SemiLinearRules,
) {
    let Some(&first) = group.members.first() else {
        return;
    };
    if aliases.is_zero(calls[first].value.ordinal) && aliases.is_zero(calls[first].operand.ordinal)
    {
        for &position in &group.members {
            let call = &calls[position];
            rules.rows.insert(
                call.row,
                difference(
                    &call.positive.expression,
                    &call.negative.expression,
                    call.span,
                ),
            );
        }
        return;
    }
    let Some(normalized) = normalize_edges(aliases, calls, group) else {
        return;
    };
    let Some(chain) = Chain::assemble(&normalized.edges) else {
        return;
    };
    emit_chain(aliases, calls, &normalized, &chain, rules);
}

/// A group member reduced to the directed slope edge MLS §3.7.4.5 chains on.
struct Edge {
    position: usize,
    /// Alias class of the normalized positive slope.
    from: u32,
    /// Alias class of the normalized negative slope.
    to: u32,
    /// `true` when the row states the negation of the group's canonical
    /// `(y, x)` pair, so the §3.7.4.5 identity exchanged the two slopes.
    mirrored: bool,
}

/// A group whose members all state one `(y, x)` pair up to a common negation.
struct NormalizedGroup {
    edges: Vec<Edge>,
    /// `opposite(y) ^ opposite(x)`, shared by every member.
    ///
    /// This is the offset between the two classes' independently chosen roots,
    /// not a property of any row: it is `false` exactly when the `y` root and
    /// the `x` root happen to have been picked from the same side of their
    /// respective balances, and either value describes the same equation set.
    /// Because [`Edge::mirrored`] measures each row against the `y` root, the
    /// canonical `x` has to absorb this offset for the pair to flip together.
    skew: bool,
}

fn normalize_edges(
    aliases: &SignedAliases,
    calls: &[SemiLinearCall],
    group: &Group,
) -> Option<NormalizedGroup> {
    let &first = group.members.first()?;
    let skew = aliases.opposite(calls[first].value.ordinal)
        ^ aliases.opposite(calls[first].operand.ordinal);
    let edges = group
        .members
        .iter()
        .map(|&position| {
            let call = &calls[position];
            let mirrored = aliases.opposite(call.value.ordinal);
            if mirrored ^ aliases.opposite(call.operand.ordinal) != skew {
                return None;
            }
            if aliases.opposite(call.positive.ordinal) || aliases.opposite(call.negative.ordinal) {
                return None;
            }
            let positive = aliases.root(call.positive.ordinal);
            let negative = aliases.root(call.negative.ordinal);
            let (from, to) = if mirrored {
                (negative, positive)
            } else {
                (positive, negative)
            };
            (from != to).then_some(Edge {
                position,
                from,
                to,
                mirrored,
            })
        })
        .collect::<Option<Vec<_>>>()?;
    Some(NormalizedGroup { edges, skew })
}

/// The simple directed path MLS §3.7.4.5 Rule 1 requires, as edge positions in
/// head-to-tail order together with the slope classes they visit.
struct Chain {
    /// Indices into the edge slice, in chain order.
    order: Vec<usize>,
    /// `nodes[i]` is the slope class before `order[i]`; `nodes.last()` is the
    /// chain tail. Length is `order.len() + 1`.
    nodes: Vec<u32>,
}

impl Chain {
    fn assemble(edges: &[Edge]) -> Option<Self> {
        if edges.len() < 2 {
            return None;
        }
        let mut outgoing: HashMap<u32, usize> = HashMap::new();
        let mut incoming: HashSet<u32> = HashSet::new();
        for (index, edge) in edges.iter().enumerate() {
            if outgoing.insert(edge.from, index).is_some() || !incoming.insert(edge.to) {
                return None;
            }
        }
        let mut heads = outgoing.keys().filter(|from| !incoming.contains(from));
        let head = *heads.next()?;
        if heads.next().is_some() {
            return None;
        }
        let mut order = Vec::with_capacity(edges.len());
        let mut nodes = vec![head];
        let mut node = head;
        while let Some(&index) = outgoing.get(&node) {
            order.push(index);
            node = edges[index].to;
            nodes.push(node);
            if order.len() > edges.len() {
                return None;
            }
        }
        (order.len() == edges.len()).then_some(Self { order, nodes })
    }

    fn head(&self) -> u32 {
        self.nodes[0]
    }

    fn tail(&self) -> u32 {
        self.nodes[self.nodes.len() - 1]
    }
}

/// Build the Rule 1 replacement residuals for one proven chain.
///
/// MLS §3.7.4.5 does not say which end of the chain `sa` is: reversing the
/// orientation of `x` maps the chain onto itself with `sa` and `sb` exchanged
/// (that is the same identity the recognizer uses), and both orientations
/// satisfy the rule. Only the value at exactly `x == 0` differs, which is the
/// case the rule exists to make definite rather than correct. rumoca fixes the
/// orientation from the model itself: `sa` is the endpoint whose slope class
/// roots on the earlier-declared variable. On the `FluidHeatFlow` models this
/// owner reaches, that is the upstream-in-forward-flow end. No claim is made
/// about which end another tool picks: OMC's selection at `x == 0` is itself a
/// function of the operand order its connection balances were emitted in, so it
/// is not a stable anchor to calibrate against.
///
/// The choice survives the way the connection balances are typed even though
/// [`Chain`] on its own does not. Flipping which variable roots the `y` class
/// flips every [`Edge::mirrored`], hence the direction of every edge and of the
/// assembled chain; `reversed` flips with it, and the two compose back to one
/// orientation. What it does not pin is a chain endpoint whose slope class holds
/// two variables proven *equal*: that class roots on whichever of them the union
/// merged first, and the tie-break can then name the opposite end. No
/// `FluidHeatFlow` node proves two slopes equal, and the difference is confined
/// to `x == 0` — which, in a zero-flow window, is held for the duration of that
/// window. "Only at a point" understates it: the endpoint the tie-break names
/// is the value the node's enthalpy sits at for as long as the flow stays zero,
/// which is a whole trajectory segment and is visible to a trace comparator.
fn emit_chain(
    aliases: &SignedAliases,
    calls: &[SemiLinearCall],
    normalized: &NormalizedGroup,
    chain: &Chain,
    rules: &mut SemiLinearRules,
) {
    let edges = normalized.edges.as_slice();
    let reversed = chain.tail() < chain.head();
    let (nodes, order) = if reversed {
        let mut nodes = chain.nodes.clone();
        nodes.reverse();
        let mut order = chain.order.clone();
        order.reverse();
        (nodes, order)
    } else {
        (chain.nodes.clone(), chain.order.clone())
    };
    let Some(slopes) = nodes
        .iter()
        .map(|node| slope_expression(calls, edges, *node))
        .collect::<Option<Vec<_>>>()
    else {
        return;
    };
    let last = order.len() - 1;
    let ends = (&slopes[0], &slopes[nodes.len() - 1]);
    let orientation = Orientation {
        value: reversed,
        operand: reversed ^ normalized.skew,
    };
    for (step, &index) in order.iter().enumerate() {
        let call = &calls[edges[index].position];
        let residual = if step == last {
            collapsed_residual(aliases, call, orientation, ends)
        } else if step == 0 {
            selector_residual(aliases, call, orientation, &slopes[1], ends)
        } else {
            difference(&slopes[step + 1], &slopes[step], call.span)
        };
        rules.rows.insert(call.row, residual);
    }
}

/// Which signed representative of each class the emitted rows are written over.
///
/// Each field is the [`SignedAliases::opposite`] value an occurrence must have
/// to be usable verbatim; an occurrence with the other sign is negated. The two
/// fields differ by the group's skew, because the `y` and `x` classes chose
/// their roots independently and only their *relative* sign is a fact about the
/// model.
#[derive(Clone, Copy)]
struct Orientation {
    value: bool,
    operand: bool,
}

/// One source occurrence of the slope class `node`, taken from the group's own
/// rows so the emitted equation never invents a reference.
///
/// Every slope of an accepted group is alias-equal (never anti-equal) to its
/// class, so any occurrence of the class states the class's value directly.
fn slope_expression(calls: &[SemiLinearCall], edges: &[Edge], node: u32) -> Option<Expression> {
    edges.iter().find_map(|edge| {
        let call = &calls[edge.position];
        let (from, to) = if edge.mirrored {
            (&call.negative, &call.positive)
        } else {
            (&call.positive, &call.negative)
        };
        if edge.from == node {
            Some(from.expression.clone())
        } else if edge.to == node {
            Some(to.expression.clone())
        } else {
            None
        }
    })
}

/// `s = if x >= 0 then sa else sb`, written as a residual.
fn selector_residual(
    aliases: &SignedAliases,
    call: &SemiLinearCall,
    orientation: Orientation,
    intermediate: &Expression,
    ends: (&Expression, &Expression),
) -> Expression {
    let operand = oriented(aliases, &call.operand, orientation.operand, call.span);
    let condition = Expression::Binary {
        op: OpBinary::Ge,
        lhs: Box::new(operand),
        rhs: Box::new(real_zero(call.span)),
        span: call.span,
    };
    let selection = Expression::If {
        branches: vec![(condition, ends.0.clone())],
        else_branch: Box::new(ends.1.clone()),
        span: call.span,
    };
    difference(intermediate, &selection, call.span)
}

/// `y = semiLinear(x, sa, sb)`, written as a residual.
fn collapsed_residual(
    aliases: &SignedAliases,
    call: &SemiLinearCall,
    orientation: Orientation,
    ends: (&Expression, &Expression),
) -> Expression {
    let value = oriented(aliases, &call.value, orientation.value, call.span);
    let operand = oriented(aliases, &call.operand, orientation.operand, call.span);
    let collapsed = Expression::BuiltinCall {
        function: BuiltinFunction::SemiLinear,
        args: vec![operand, ends.0.clone(), ends.1.clone()],
        span: call.span,
    };
    difference(&value, &collapsed, call.span)
}

/// The occurrence rewritten into the chain's chosen orientation.
fn oriented(aliases: &SignedAliases, slot: &VarOccurrence, wanted: bool, span: Span) -> Expression {
    if aliases.opposite(slot.ordinal) == wanted {
        slot.expression.clone()
    } else {
        Expression::Unary {
            op: OpUnary::Minus,
            rhs: Box::new(slot.expression.clone()),
            span,
        }
    }
}

fn difference(lhs: &Expression, rhs: &Expression, span: Span) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs.clone()),
        rhs: Box::new(rhs.clone()),
        span,
    }
}

fn real_zero(span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Real(0.0),
        span,
    }
}

/// Signed variable classes proven by the model's own additive equations.
///
/// An accepted row states a signed sum of subscript-free scalar variables plus
/// a numeric constant. With the constant zero, one surviving term proves the
/// variable identically zero and two surviving terms prove `a ≡ ±b`; terms
/// already proven zero drop out, which is what turns the three-term flow
/// balance `H_a + H_b + Q_flow = 0` into the alias `H_a ≡ -H_b` once
/// `Q_flow = 0` is known. The pass repeats until nothing new is proven, so the
/// *facts* it proves do not depend on the order of the equations: the partition
/// of variables into classes, the relative sign of any two members of one
/// class, and which classes are identically zero are all order-independent.
///
/// Which member of a class becomes its root is *not*. [`Self::union`] keeps the
/// left root, so first-merge order decides, and both `a + b = 0` and
/// `b + a = 0` prove the same fact `a ≡ -b` while rooting the class on
/// different variables. [`Self::opposite`] reports a variable's sign relative to
/// that root, so its absolute value carries the arbitrary choice: `a + b = 0`
/// gives `opposite(a) == false`, `b + a = 0` gives `opposite(a) == true`.
///
/// A caller may therefore only ever compare `opposite` values, and only within
/// one class. Testing an `opposite` against `false`, or comparing two `opposite`
/// values drawn from *different* classes, reads the union-find's bookkeeping as
/// if it were a statement of the model.
pub(super) struct SignedAliases {
    parent: Vec<u32>,
    parity: Vec<bool>,
    zero: Vec<bool>,
}

/// One accepted additive row: signed variable ordinals and the constant sum.
struct SignedSum {
    terms: Vec<(bool, u32)>,
    constant: f64,
}

impl SignedAliases {
    fn prove(flat: &flat::Model, rows: &[usize]) -> Self {
        let count = flat.variables.len();
        let mut aliases = Self {
            parent: (0..u32::try_from(count).unwrap_or(u32::MAX)).collect(),
            parity: vec![false; count],
            zero: vec![false; count],
        };
        let sums: Vec<SignedSum> = rows
            .iter()
            .filter_map(|row| signed_sum(flat, &flat.equations[*row].residual))
            .collect();
        let mut changed = true;
        while changed {
            changed = false;
            for sum in &sums {
                changed |= aliases.apply(sum);
            }
        }
        aliases
    }

    fn apply(&mut self, sum: &SignedSum) -> bool {
        if sum.constant != 0.0 {
            return false;
        }
        let live: Vec<(bool, u32)> = sum
            .terms
            .iter()
            .copied()
            .filter(|(_, ordinal)| !self.is_zero(*ordinal))
            .collect();
        match live.as_slice() {
            [(_, only)] => self.pin_zero(*only),
            [(left_sign, left), (right_sign, right)] => {
                self.union(*left, *right, left_sign == right_sign)
            }
            _ => false,
        }
    }

    /// The class root of `ordinal` and whether it is the class's negation.
    fn find(&self, ordinal: u32) -> (u32, bool) {
        let mut node = ordinal;
        let mut opposite = false;
        while self.parent[node as usize] != node {
            opposite ^= self.parity[node as usize];
            node = self.parent[node as usize];
        }
        (node, opposite)
    }

    pub(super) fn root(&self, ordinal: u32) -> u32 {
        self.find(ordinal).0
    }

    pub(super) fn opposite(&self, ordinal: u32) -> bool {
        self.find(ordinal).1
    }

    pub(super) fn is_zero(&self, ordinal: u32) -> bool {
        self.zero[self.root(ordinal) as usize]
    }

    fn pin_zero(&mut self, ordinal: u32) -> bool {
        let root = self.root(ordinal);
        if self.zero[root as usize] {
            return false;
        }
        self.zero[root as usize] = true;
        true
    }

    fn union(&mut self, left: u32, right: u32, opposed: bool) -> bool {
        let (left_root, left_opposite) = self.find(left);
        let (right_root, right_opposite) = self.find(right);
        if left_root == right_root {
            return false;
        }
        self.parent[right_root as usize] = left_root;
        self.parity[right_root as usize] = opposed ^ left_opposite ^ right_opposite;
        let zero = self.zero[left_root as usize] || self.zero[right_root as usize];
        self.zero[left_root as usize] = zero;
        true
    }
}

/// Read a residual as a signed sum of scalar variables plus a constant.
fn signed_sum(flat: &flat::Model, residual: &Expression) -> Option<SignedSum> {
    let mut sum = SignedSum {
        terms: Vec::new(),
        constant: 0.0,
    };
    accumulate(flat, residual, false, &mut sum).then_some(sum)
}

fn accumulate(
    flat: &flat::Model,
    expression: &Expression,
    negated: bool,
    sum: &mut SignedSum,
) -> bool {
    match expression {
        Expression::Binary {
            op: op @ (OpBinary::Add | OpBinary::Sub),
            lhs,
            rhs,
            ..
        } => {
            accumulate(flat, lhs, negated, sum)
                && accumulate(flat, rhs, negated ^ matches!(op, OpBinary::Sub), sum)
        }
        Expression::Unary {
            op: op @ (OpUnary::Minus | OpUnary::Plus),
            rhs,
            ..
        } => accumulate(flat, rhs, negated ^ matches!(op, OpUnary::Minus), sum),
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => match flat.variables.get_index_of(name.var_name()) {
            Some(ordinal) => match u32::try_from(ordinal) {
                Ok(ordinal) => {
                    sum.terms.push((negated, ordinal));
                    true
                }
                Err(_) => false,
            },
            None => false,
        },
        Expression::Literal { value, .. } => match numeric_literal(value) {
            Some(number) => {
                sum.constant += if negated { -number } else { number };
                true
            }
            None => false,
        },
        _ => false,
    }
}

fn numeric_literal(value: &Literal) -> Option<f64> {
    match value {
        Literal::Real(number) => Some(*number),
        Literal::Integer(number) => Some(*number as f64),
        _ => None,
    }
}
