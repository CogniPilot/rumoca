//! Bounded verification of the ordinal-stable DAE wire round trip.
//!
//! [`DAE_SCHEMA_VERSION`] records two near-misses that a name-tagged round trip
//! could not have caught. 14 *inserted* `ConditionNodeWire::Always` at ordinal
//! 1, shifting every later condition ordinal so a superseded payload decodes to
//! the wrong variant rather than failing; 15 *appended* `CoordinateWire::PreState`
//! and `CoordinateWire::PreAlgebraic`. `bincode` tags an enum variant by its
//! declaration index, so it is the codec that observes either change, while JSON
//! names its variants and would have stayed green through both.
//!
//! This harness therefore pins two obligations together. Every `CoordinateInput`
//! and `ConditionInput` variant is named through a total match with no wildcard
//! arm, and the universe the fixture is measured against is *walked out of that
//! same match* rather than listed beside it — see [`coordinate_universe`] for
//! exactly what that does and does not force. The property then re-encodes what
//! it decoded under *both* codecs and demands identical bytes.
//!
//! `mod tests` is `#[cfg(test)]`-gated in `lib.rs`, so the `#[cfg(kani)]` proof
//! module below is inert today. It is written anyway because it is the exact
//! shape the proof takes once this harness moves out from under that gate.

use super::*;

/// One step of the walk over an enum's declaration order: the tag of the variant
/// matched, and a representative of the variant declared after it.
struct VariantStep<T> {
    tag: &'static str,
    next: Option<T>,
}

/// Names every [`CoordinateInput`] variant and links it to the one declared
/// after it.
///
/// This match is deliberately total and deliberately has no wildcard arm: adding
/// a coordinate makes this fail to compile. The arm that must then be written
/// carries both facts the round trip is measured against — the variant's tag and
/// the successor link — so [`coordinate_universe`] is *derived* from this match
/// instead of restating it in a second list that a new variant could bypass.
fn coordinate_step<'dae>(input: &CoordinateInput<'_>) -> VariantStep<CoordinateInput<'dae>> {
    match input {
        CoordinateInput::Parameter(_) => VariantStep {
            tag: "parameter",
            next: Some(CoordinateInput::Input(InputId::from_raw(0))),
        },
        CoordinateInput::Input(_) => VariantStep {
            tag: "input",
            next: Some(CoordinateInput::State(StateId::from_raw(0))),
        },
        CoordinateInput::State(_) => VariantStep {
            tag: "state",
            next: Some(CoordinateInput::Derivative(StateId::from_raw(0))),
        },
        CoordinateInput::Derivative(_) => VariantStep {
            tag: "derivative",
            next: Some(CoordinateInput::Algebraic(AlgebraicId::from_raw(0))),
        },
        CoordinateInput::Algebraic(_) => VariantStep {
            tag: "algebraic",
            next: Some(CoordinateInput::DiscreteReal(DiscreteRealId::from_raw(0))),
        },
        CoordinateInput::DiscreteReal(_) => VariantStep {
            tag: "discrete_real",
            next: Some(CoordinateInput::DiscreteValue(DiscreteValueId::from_raw(0))),
        },
        CoordinateInput::DiscreteValue(_) => VariantStep {
            tag: "discrete_value",
            next: Some(CoordinateInput::PreDiscreteReal(DiscreteRealId::from_raw(
                0,
            ))),
        },
        CoordinateInput::PreDiscreteReal(_) => VariantStep {
            tag: "pre_discrete_real",
            next: Some(CoordinateInput::PreDiscreteValue(
                DiscreteValueId::from_raw(0),
            )),
        },
        CoordinateInput::PreDiscreteValue(_) => VariantStep {
            tag: "pre_discrete_value",
            next: Some(CoordinateInput::PreState(StateId::from_raw(0))),
        },
        CoordinateInput::PreState(_) => VariantStep {
            tag: "pre_state",
            next: Some(CoordinateInput::PreAlgebraic(AlgebraicId::from_raw(0))),
        },
        CoordinateInput::PreAlgebraic(_) => VariantStep {
            tag: "pre_algebraic",
            next: Some(CoordinateInput::Time),
        },
        CoordinateInput::Time => VariantStep {
            tag: "time",
            next: Some(CoordinateInput::ClockInterval(PeriodicClockId::from_raw(0))),
        },
        CoordinateInput::ClockInterval(_) => VariantStep {
            tag: "clock_interval",
            next: Some(CoordinateInput::Condition(ConditionId::from_raw(0))),
        },
        CoordinateInput::Condition(_) => VariantStep {
            tag: "condition",
            next: Some(CoordinateInput::Previous(PreviousId::from_raw(0))),
        },
        CoordinateInput::Previous(_) => VariantStep {
            tag: "previous",
            next: Some(CoordinateInput::Terminal(TerminalId::from_raw(0))),
        },
        CoordinateInput::Terminal(_) => VariantStep {
            tag: "terminal",
            next: Some(CoordinateInput::FunctionParameter(
                FunctionParameterId::from_raw(0, 0),
            )),
        },
        CoordinateInput::FunctionParameter(_) => VariantStep {
            tag: "function_parameter",
            next: None,
        },
    }
}

/// Names every [`ConditionInput`] variant and links it to the one declared after
/// it, for the same reason and with the same absent wildcard arm. This is the
/// enum the 14 insertion moved.
fn condition_step<'dae>(input: &ConditionInput<'_>) -> VariantStep<ConditionInput<'dae>> {
    let condition = ConditionId::from_raw(0);
    match input {
        ConditionInput::Initial => VariantStep {
            tag: "initial",
            next: Some(ConditionInput::Always),
        },
        ConditionInput::Always => VariantStep {
            tag: "always",
            next: Some(ConditionInput::Relation(RelationId::from_raw(0))),
        },
        ConditionInput::Relation(_) => VariantStep {
            tag: "relation",
            next: Some(ConditionInput::Discrete(ExprId::from_raw(0))),
        },
        ConditionInput::Discrete(_) => VariantStep {
            tag: "discrete",
            next: Some(ConditionInput::Clock(ClockId::from_raw(0))),
        },
        ConditionInput::Clock(_) => VariantStep {
            tag: "clock",
            next: Some(ConditionInput::Not(condition)),
        },
        ConditionInput::Not(_) => VariantStep {
            tag: "not",
            next: Some(ConditionInput::And(condition, condition)),
        },
        ConditionInput::And(..) => VariantStep {
            tag: "and",
            next: Some(ConditionInput::Or(condition, condition)),
        },
        ConditionInput::Or(..) => VariantStep {
            tag: "or",
            next: Some(ConditionInput::AnyRise(condition, condition)),
        },
        ConditionInput::AnyRise(..) => VariantStep {
            tag: "any_rise",
            next: None,
        },
    }
}

fn coordinate_variant_tag(input: &CoordinateInput<'_>) -> &'static str {
    coordinate_step(input).tag
}

fn condition_variant_tag(input: &ConditionInput<'_>) -> &'static str {
    condition_step(input).tag
}

/// Walk a variant chain from its first variant, collecting one representative of
/// every variant it reaches.
///
/// The bookkeeping is a repeat guard, not a coverage claim. It catches the two
/// ways an arm can be written wrongly at all: a successor link that points
/// backwards, which would otherwise spin here forever instead of failing, and
/// two arms sharing one tag, which would let a variant hide behind its
/// neighbour's name in the coverage comparison.
fn walk_variants<T, S>(first: T, step: S) -> Vec<T>
where
    S: Fn(&T) -> VariantStep<T>,
{
    let mut universe = Vec::new();
    let mut tags: Vec<&'static str> = Vec::new();
    let mut next = Some(first);
    while let Some(variant) = next {
        let reached = step(&variant);
        assert!(
            !tags.contains(&reached.tag),
            "the variant walk reached the tag `{}` twice: either a successor \
             link points backwards, or two arms share one tag",
            reached.tag
        );
        tags.push(reached.tag);
        universe.push(variant);
        next = reached.next;
    }
    universe
}

/// One representative of each [`CoordinateInput`] variant, naming the universe
/// the fixture is measured against.
///
/// These values are never handed to a constructor — they exist only to be read
/// by [`coordinate_variant_tag`], so their raw indices need not resolve against
/// any arena.
///
/// What this forces: a variant added to `CoordinateInput` stops the file
/// compiling until an arm is written in [`coordinate_step`], and an arm linked
/// into the walk puts the new variant in this universe, where
/// [`assert_variant_coverage`] then fails until the fixture actually constructs
/// it. A new arm that reused an existing tag instead of coining one trips the
/// repeat guard inside [`walk_variants`].
///
/// What it does not force: the link itself. Stable Rust cannot enumerate an
/// enum's variants, so an arm written as a dead end (`next: None`) leaves its
/// variant off the walk with nothing to notice. Nor does the walk see the erased
/// `Coordinate` — the enum `bincode` actually tags — gaining a variant that has
/// no `CoordinateInput` counterpart; `Delay` and `Binder` are already in that
/// position, and `DAE_SCHEMA_VERSION` is what covers them.
fn coordinate_universe<'dae>() -> Vec<CoordinateInput<'dae>> {
    walk_variants(
        CoordinateInput::Parameter(ParameterId::from_raw(0)),
        coordinate_step,
    )
}

/// One representative of each [`ConditionInput`] variant; see
/// [`coordinate_universe`] for why the values are inert and for the exact reach
/// of the guard.
fn condition_universe<'dae>() -> Vec<ConditionInput<'dae>> {
    walk_variants(ConditionInput::Initial, condition_step)
}

/// Bounded variation over one fixture shape.
///
/// Every field moves the *encoded ordinal stream* rather than the meaning of the
/// model: the order coordinates enter the arena permutes the coordinate tags the
/// wire carries and the operand indices that name them, the three literal
/// payloads select different `DaeLiteral` ordinals, and swapping the binary
/// condition operands exchanges the indices a shifted condition table would
/// silently reinterpret.
#[derive(Debug, Clone)]
struct FixturePlan {
    coordinate_order: Vec<usize>,
    /// Eighths, so the payload is a dyadic rational.
    ///
    /// The Real payload is deliberately not an arbitrary double. The workspace
    /// builds `serde_json` without its `float_roundtrip` feature (root
    /// `Cargo.toml` asks only for `preserve_order`), and its default number
    /// parser can land one ULP away from the correctly rounded value: reading
    /// back `-122.50221691932535` yields the neighbouring double, whose own
    /// shortest form is `-122.50221691932536`. That is a payload-fidelity defect
    /// of the name-tagged codec, not an ordinal defect — `bincode` carries the
    /// eight raw bytes and is unaffected — so this harness draws Reals from the
    /// domain both codecs represent exactly and leaves the defect to be fixed
    /// where it lives, in the workspace dependency declaration.
    real_eighths: i32,
    integer: i64,
    boolean: bool,
    swap_condition_operands: bool,
}

/// Bound on [`FixturePlan::real_eighths`], keeping the payload inside ±1000.
const REAL_EIGHTHS_BOUND: i32 = 8_000;

/// Coordinates the top-level pass may permute.
///
/// `FunctionParameter` is excluded and constructed inside its function scope
/// instead, because that is the only place its owner-local identity exists.
const PERMUTED_COORDINATES: usize = 16;

impl FixturePlan {
    fn canonical() -> Self {
        Self {
            coordinate_order: (0..PERMUTED_COORDINATES).collect(),
            real_eighths: 12,
            integer: -7,
            boolean: true,
            swap_condition_operands: false,
        }
    }

    fn real(&self) -> f64 {
        f64::from(self.real_eighths) / 8.0
    }
}

const VARIANT_SOURCE: &str = "parameter Real p; constant Real c; input Real u; Real x; Real y; \
     output Real o; discrete Real z; discrete Boolean m; \
     function f input Real q; output Real w; algorithm w := q; end f; \
     when time > 0 then end when; when initial() then end when; \
     when trigger then end when; when not trigger then end when; \
     when {trigger, not trigger} then end when; when Clock(1, 10) then end when; \
     algorithm x := 1; previous(z); interval(z); terminal();";

#[derive(Clone, Copy)]
struct WireVariantSpans {
    roles: VariableRoleSpans,
    function: DaeProvenance,
    function_input: DaeProvenance,
    function_output: DaeProvenance,
    body: DaeProvenance,
    time: DaeProvenance,
    initial: DaeProvenance,
    trigger: DaeProvenance,
    negated: DaeProvenance,
    vector: DaeProvenance,
    clock: DaeProvenance,
    section: DaeProvenance,
    previous: DaeProvenance,
    interval: DaeProvenance,
    terminal: DaeProvenance,
}

fn wire_variant_spans(source: &TestSource) -> WireVariantSpans {
    WireVariantSpans {
        roles: variable_role_spans(source),
        function: source.source("function f", 0),
        function_input: source.source("input Real q", 0),
        function_output: source.source("output Real w", 0),
        body: source.source("algorithm w := q", 0),
        time: source.source("time > 0", 0),
        initial: source.source("initial()", 0),
        trigger: source.source("when trigger then end when", 0),
        negated: source.source("when not trigger then end when", 0),
        vector: source.source("{trigger, not trigger}", 0),
        clock: source.source("Clock(1, 10)", 0),
        section: source.source("algorithm x := 1", 0),
        previous: source.source("previous(z)", 0),
        interval: source.source("interval(z)", 0),
        terminal: source.source("terminal()", 0),
    }
}

/// Runtime owners the coordinate pass needs, all constructed before it runs so
/// that permuting the pass cannot change which owner a coordinate names.
struct VariantRuntime<'dae> {
    clock: PeriodicClockId<'dae>,
    previous: PreviousId<'dae>,
    terminal: TerminalId<'dae>,
    condition: ConditionId<'dae>,
    trigger: ExprId<'dae>,
}

/// Operands the condition catalog is built from.
struct ConditionSeeds<'dae> {
    clock: PeriodicClockId<'dae>,
    trigger: ExprId<'dae>,
    relation: RelationId<'dae>,
}

struct WireVariantFixture {
    dae: Dae,
    coordinate_tags: Vec<&'static str>,
    condition_tags: Vec<&'static str>,
}

/// Insert one coordinate occurrence and record which variant it was.
fn record_coordinate<'dae>(
    expressions: &mut Expressions<'_, 'dae>,
    tags: &mut Vec<&'static str>,
    at: DaeProvenance,
    input: CoordinateInput<'dae>,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    tags.push(coordinate_variant_tag(&input));
    expressions.at(at).coordinate(input)
}

/// Reserve, define, and record one condition.
fn record_condition<'dae>(
    dae: &mut DaeConstruction<'dae>,
    tags: &mut Vec<&'static str>,
    at: DaeProvenance,
    input: ConditionInput<'dae>,
) -> Result<ConditionId<'dae>, DaeConstructionError> {
    let condition = dae.conditions(|conditions| conditions.reserve(at))?;
    tags.push(condition_variant_tag(&input));
    dae.conditions(|conditions| conditions.define(condition, input, at))?;
    Ok(condition)
}

fn define_variant_function<'dae>(
    dae: &mut DaeConstruction<'dae>,
    spans: WireVariantSpans,
    tags: &mut Vec<&'static str>,
) -> Result<(), DaeConstructionError> {
    let real =
        dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), spans.function))?;
    dae.function(
        FunctionSignature::new(VarName::new("f"), [real], [real], spans.function),
        |dae, reservation| {
            let parameter = dae.functions(|functions| {
                functions.parameter(&reservation, VarName::new("q"), 0, spans.function_input)
            })?;
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("w"), 0, spans.function_output)
            })?;
            let value = dae.expressions(|expressions| {
                record_coordinate(
                    expressions,
                    tags,
                    spans.body,
                    CoordinateInput::FunctionParameter(parameter),
                )
            })?;
            let mut body =
                dae.functions(|functions| functions.begin(reservation, spans.function))?;
            dae.functions(|functions| functions.assign(&mut body, output, value, spans.body))?;
            dae.functions(|functions| functions.define(body, spans.body))
        },
    )
    .map(|_| ())
}

/// Build every condition variant over a single pair of leaves.
///
/// `Always` and `AnyRise` were added in one change and land on opposite sides of
/// the ordinal-tagged encoding — the first inserted, the second appended — so
/// they and the neighbours the insertion moved (`Relation`, `Discrete`, `Not`,
/// `Or`) have to travel in the same payload for a mis-shifted table to be
/// observable at all.
fn define_variant_conditions<'dae>(
    dae: &mut DaeConstruction<'dae>,
    seeds: ConditionSeeds<'dae>,
    spans: WireVariantSpans,
    plan: &FixturePlan,
    tags: &mut Vec<&'static str>,
) -> Result<ConditionId<'dae>, DaeConstructionError> {
    let discrete = record_condition(
        dae,
        tags,
        spans.trigger,
        ConditionInput::Discrete(seeds.trigger),
    )?;
    let negated = record_condition(dae, tags, spans.negated, ConditionInput::Not(discrete))?;
    let (lhs, rhs) = if plan.swap_condition_operands {
        (negated, discrete)
    } else {
        (discrete, negated)
    };
    record_condition(
        dae,
        tags,
        spans.time,
        ConditionInput::Relation(seeds.relation),
    )?;
    record_condition(dae, tags, spans.initial, ConditionInput::Initial)?;
    record_condition(dae, tags, spans.section, ConditionInput::Always)?;
    record_condition(
        dae,
        tags,
        spans.clock,
        ConditionInput::Clock(seeds.clock.into()),
    )?;
    record_condition(dae, tags, spans.vector, ConditionInput::AnyRise(lhs, rhs))?;
    record_condition(dae, tags, spans.vector, ConditionInput::Or(lhs, rhs))?;
    record_condition(dae, tags, spans.trigger, ConditionInput::And(lhs, rhs))?;
    Ok(discrete)
}

fn define_variant_runtime<'dae>(
    dae: &mut DaeConstruction<'dae>,
    variables: VariableRoleIds<'dae>,
    spans: WireVariantSpans,
    plan: &FixturePlan,
    tags: &mut Vec<&'static str>,
) -> Result<VariantRuntime<'dae>, DaeConstructionError> {
    let clock = dae.clocks(|clocks| {
        let lattice =
            ClockLattice::new(ClockRational::new(1, 10).unwrap(), ClockRational::ZERO).unwrap();
        let clock = clocks.periodic(lattice, spans.clock)?;
        clocks.own_discrete_real(clock.into(), variables.discrete_real, spans.clock)?;
        Ok(clock)
    })?;
    let previous = dae.temporal(|temporal| {
        temporal.previous_discrete_real(clock.into(), variables.discrete_real, spans.previous)
    })?;
    let terminal = dae.temporal(|temporal| temporal.terminal(spans.terminal))?;
    let (trigger, comparison) = dae.expressions(|expressions| {
        let trigger = expressions
            .at(spans.trigger)
            .literal(DaeLiteral::Boolean(plan.boolean))?;
        // A third literal ordinal, so the encoded `DaeLiteral` stream carries
        // every payload shape the plan varies.
        expressions
            .at(spans.section)
            .literal(DaeLiteral::Integer(plan.integer))?;
        let lhs = expressions
            .at(spans.time)
            .literal(DaeLiteral::Real(plan.real()))?;
        let rhs = expressions.at(spans.time).literal(DaeLiteral::Real(0.0))?;
        let comparison = expressions
            .at(spans.time)
            .binary(BinaryOperator::Greater, lhs, rhs)?;
        Ok((trigger, comparison))
    })?;
    let relation = dae.conditions(|conditions| conditions.relation(comparison, spans.time))?;
    let seeds = ConditionSeeds {
        clock,
        trigger,
        relation,
    };
    let condition = define_variant_conditions(dae, seeds, spans, plan, tags)?;
    Ok(VariantRuntime {
        clock,
        previous,
        terminal,
        condition,
        trigger,
    })
}

fn variant_coordinate_slots<'dae>(
    variables: VariableRoleIds<'dae>,
    runtime: &VariantRuntime<'dae>,
    spans: WireVariantSpans,
) -> Vec<(DaeProvenance, CoordinateInput<'dae>)> {
    let roles = spans.roles;
    vec![
        (
            roles.parameter,
            CoordinateInput::Parameter(variables.parameter),
        ),
        (roles.input, CoordinateInput::Input(variables.input)),
        (roles.state, CoordinateInput::State(variables.state)),
        (roles.state, CoordinateInput::Derivative(variables.state)),
        (roles.state, CoordinateInput::PreState(variables.state)),
        (
            roles.algebraic,
            CoordinateInput::Algebraic(variables.algebraic),
        ),
        (
            roles.algebraic,
            CoordinateInput::PreAlgebraic(variables.algebraic),
        ),
        (
            roles.discrete_real,
            CoordinateInput::DiscreteReal(variables.discrete_real),
        ),
        (
            roles.discrete_real,
            CoordinateInput::PreDiscreteReal(variables.discrete_real),
        ),
        (
            roles.discrete_value,
            CoordinateInput::DiscreteValue(variables.discrete_value),
        ),
        (
            roles.discrete_value,
            CoordinateInput::PreDiscreteValue(variables.discrete_value),
        ),
        (spans.time, CoordinateInput::Time),
        (
            spans.interval,
            CoordinateInput::ClockInterval(runtime.clock),
        ),
        (spans.trigger, CoordinateInput::Condition(runtime.condition)),
        (spans.previous, CoordinateInput::Previous(runtime.previous)),
        (spans.terminal, CoordinateInput::Terminal(runtime.terminal)),
    ]
}

fn add_variant_coordinates<'dae>(
    dae: &mut DaeConstruction<'dae>,
    slots: &[(DaeProvenance, CoordinateInput<'dae>)],
    order: &[usize],
    tags: &mut Vec<&'static str>,
) -> Result<(), DaeConstructionError> {
    assert_eq!(
        (order.len(), slots.len()),
        (PERMUTED_COORDINATES, PERMUTED_COORDINATES),
        "the plan has to order every permutable coordinate slot exactly once"
    );
    dae.expressions(|expressions| {
        for &slot in order {
            let (at, input) = slots[slot];
            record_coordinate(expressions, tags, at, input)?;
        }
        Ok(())
    })
}

fn close_discrete_value_topology<'dae>(
    dae: &mut DaeConstruction<'dae>,
    variables: VariableRoleIds<'dae>,
    spans: WireVariantSpans,
    value: ExprId<'dae>,
) -> Result<(), DaeConstructionError> {
    let at = spans.roles.discrete_value;
    dae.b1c([variables.discrete_value], |topology| {
        topology.owner(at, [variables.discrete_value], |owner| {
            owner.always(at, [(value, at)])
        })?;
        Ok(())
    })
}

/// One checked `Dae` carrying every coordinate and condition variant at once.
fn all_variant_fixture(plan: &FixturePlan) -> WireVariantFixture {
    let source = TestSource::new(VARIANT_SOURCE);
    let spans = wire_variant_spans(&source);
    let mut coordinate_tags = Vec::new();
    let mut condition_tags = Vec::new();
    let dae = Dae::construct(source.map, |dae| {
        let variables = define_variable_role_catalog(dae, spans.roles)?;
        define_variant_function(dae, spans, &mut coordinate_tags)?;
        let runtime = define_variant_runtime(dae, variables, spans, plan, &mut condition_tags)?;
        let slots = variant_coordinate_slots(variables, &runtime, spans);
        add_variant_coordinates(dae, &slots, &plan.coordinate_order, &mut coordinate_tags)?;
        close_discrete_value_topology(dae, variables, spans, runtime.trigger)
    })
    .expect("every wire variant is jointly constructible in one checked DAE");
    WireVariantFixture {
        dae,
        coordinate_tags,
        condition_tags,
    }
}

/// # Property (wire round-trip, ordinal-stable)
///
/// For a checked `Dae` carrying every condition and coordinate variant, decoding
/// an encoding reconstructs a value that re-encodes to identical bytes, under
/// both the ordinal-tagged codec (bincode) and the name-tagged codec (JSON).
/// SPEC_0037 phase-proof obligation "Wire — Decode reconstructs only checked
/// current IR; round trip preserves identity".
///
/// `Dae` has no `PartialEq`, so identity is byte equality of the re-encoding —
/// the idiom the existing wire tests already use.
///
/// What this does *not* claim: one process encoding and decoding with one
/// variant table is self-consistent under any table, so a round trip alone
/// cannot see an ordinal shift. The shift is caught by the wildcard-free matches
/// above, which stop this file compiling until a new variant is accounted for,
/// and by `DAE_SCHEMA_VERSION`, which rejects a payload written under the
/// superseded table. What the round trip adds is that every variant, carried
/// together in one payload, decodes back into a *checked* model — including the
/// operand indices the 14 insertion would have reinterpreted.
fn property_wire_round_trip_is_ordinal_stable(dae: &Dae) {
    let ordinal_tagged = bincode::serialize(dae).expect("a checked DAE encodes");
    let decoded: Dae =
        bincode::deserialize(&ordinal_tagged).expect("ordinal-tagged wire reconstructs");
    assert_eq!(decoded.schema_version(), DAE_SCHEMA_VERSION);
    assert_eq!(
        bincode::serialize(&decoded).expect("a reconstructed DAE re-encodes"),
        ordinal_tagged,
        "bincode identifies a variant positionally, so it is the codec under \
         which a decode that lands on the wrong variant changes the bytes"
    );

    let name_tagged = serde_json::to_string(dae).expect("a checked DAE encodes");
    let decoded: Dae = serde_json::from_str(&name_tagged).expect("name-tagged wire reconstructs");
    assert_eq!(decoded.schema_version(), DAE_SCHEMA_VERSION);
    assert_eq!(
        serde_json::to_string(&decoded).expect("a reconstructed DAE re-encodes"),
        name_tagged,
        "the name-tagged codec is carried because it is the one a 14-style \
         insertion left green; it must still agree"
    );
}

fn distinct_sorted(tags: &[&'static str]) -> Vec<&'static str> {
    let mut tags = tags.to_vec();
    tags.sort_unstable();
    tags.dedup();
    tags
}

/// Runtime partner to the wildcard-free matches: the fixture really did build a
/// coordinate and a condition for every variant the walk over those matches
/// reaches.
fn assert_variant_coverage(fixture: &WireVariantFixture) {
    let coordinates = coordinate_universe()
        .iter()
        .map(coordinate_variant_tag)
        .collect::<Vec<_>>();
    let conditions = condition_universe()
        .iter()
        .map(condition_variant_tag)
        .collect::<Vec<_>>();
    assert_eq!(
        distinct_sorted(&coordinates).len(),
        coordinates.len(),
        "each coordinate variant has to carry its own tag"
    );
    assert_eq!(
        distinct_sorted(&conditions).len(),
        conditions.len(),
        "each condition variant has to carry its own tag"
    );
    assert_eq!(
        distinct_sorted(&fixture.coordinate_tags),
        distinct_sorted(&coordinates),
        "the round-trip fixture has to construct every coordinate variant"
    );
    assert_eq!(
        distinct_sorted(&fixture.condition_tags),
        distinct_sorted(&conditions),
        "the round-trip fixture has to construct every condition variant"
    );
}

/// Build the fixture for one plan and check both halves of the obligation: that
/// the fixture really carried every variant, and that its payload round-trips.
///
/// Every driver goes through here, so a variant the fixture stopped constructing
/// fails every driver rather than only the deterministic test below.
fn check_all_variants_round_trip(plan: &FixturePlan) -> WireVariantFixture {
    let fixture = all_variant_fixture(plan);
    assert_variant_coverage(&fixture);
    property_wire_round_trip_is_ordinal_stable(&fixture.dae);
    fixture
}

#[test]
fn all_wire_variants_round_trip_ordinal_stably() {
    let fixture = check_all_variants_round_trip(&FixturePlan::canonical());

    // The plan has to move the encoded stream, or the fallback driver would be
    // re-checking one payload 64 times.
    let mut varied = FixturePlan::canonical();
    varied.coordinate_order.reverse();
    varied.real_eighths = -3;
    varied.swap_condition_operands = true;
    let varied = check_all_variants_round_trip(&varied);
    assert_ne!(
        bincode::serialize(&varied.dae).expect("a checked DAE encodes"),
        bincode::serialize(&fixture.dae).expect("a checked DAE encodes"),
        "bounded variation has to reach the encoding"
    );
}

/// Graduation path for the same property under bounded model checking.
///
/// `mod tests` is `#[cfg(test)]`-gated in `lib.rs`, so this module compiles under
/// no configuration today; it records the shape the proof takes and is left
/// inert rather than restructuring production gating to reach it.
#[cfg(kani)]
mod proof {
    use super::*;

    /// # Property (wire round-trip, ordinal-stable)
    ///
    /// For a checked `Dae` carrying every condition and coordinate variant,
    /// decoding an encoding reconstructs a value that re-encodes to identical
    /// bytes, under both the ordinal-tagged codec (bincode) and the name-tagged
    /// codec (JSON). SPEC_0037 phase-proof obligation "Wire — Decode
    /// reconstructs only checked current IR; round trip preserves identity".
    #[kani::proof]
    fn wire_round_trip_is_ordinal_stable() {
        let real_eighths: i32 = kani::any();
        // See `FixturePlan::real_eighths`: the name-tagged codec is only
        // value-preserving over the doubles it can spell exactly.
        kani::assume((-REAL_EIGHTHS_BOUND..REAL_EIGHTHS_BOUND).contains(&real_eighths));
        let plan = FixturePlan {
            coordinate_order: (0..PERMUTED_COORDINATES).collect(),
            real_eighths,
            integer: kani::any(),
            boolean: kani::any(),
            swap_condition_operands: kani::any(),
        };
        check_all_variants_round_trip(&plan);
    }
}

/// Fallback driver: the same property under proptest, because Kani is not yet in
/// the dev shell. The generated variation is bounded and always constructible —
/// it moves the encoded ordinal stream, never the validity of the model.
#[cfg(not(kani))]
mod fallback {
    use super::*;

    use proptest::prelude::*;

    fn fixture_plan() -> impl Strategy<Value = FixturePlan> {
        (
            Just((0..PERMUTED_COORDINATES).collect::<Vec<usize>>()).prop_shuffle(),
            // See `FixturePlan::real_eighths`: the name-tagged codec is only
            // value-preserving over the doubles it can spell exactly.
            -REAL_EIGHTHS_BOUND..REAL_EIGHTHS_BOUND,
            any::<i64>(),
            any::<bool>(),
            any::<bool>(),
        )
            .prop_map(
                |(coordinate_order, real_eighths, integer, boolean, swap_condition_operands)| {
                    FixturePlan {
                        coordinate_order,
                        real_eighths,
                        integer,
                        boolean,
                        swap_condition_operands,
                    }
                },
            )
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(64))]

        /// # Property (wire round-trip, ordinal-stable)
        ///
        /// For a checked `Dae` carrying every condition and coordinate variant,
        /// decoding an encoding reconstructs a value that re-encodes to
        /// identical bytes, under both the ordinal-tagged codec (bincode) and
        /// the name-tagged codec (JSON). SPEC_0037 phase-proof obligation
        /// "Wire — Decode reconstructs only checked current IR; round trip
        /// preserves identity".
        #[test]
        fn wire_round_trip_is_ordinal_stable(plan in fixture_plan()) {
            check_all_variants_round_trip(&plan);
        }
    }
}
