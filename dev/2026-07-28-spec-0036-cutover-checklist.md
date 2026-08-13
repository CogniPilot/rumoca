# SPEC 0036 Cutover Checklist

This is the living work ledger for the valid-by-construction compiler cutover.
Normative requirements remain in the linked specs; this page records unfinished
work and the evidence needed to close it. Removing code or tests does not close
a capability item.

Last audited: 2026-07-29.

## Completion rule

An item may be checked only when its implementation, replacement tests, and
named end-to-end evidence all exist in the current tree. A focused unit test
does not establish a workspace-, MSL-, wire-, or runtime-wide claim.

## Current priority: canonical checked DAE

- [x] Remove the superseded DAE root and wire readers. The 2026-07-29
  adversarial audit found one private-field `rumoca_ir_dae::Dae`, one
  `Dae::construct` production mint, and current-schema wire replay through the
  same construction operations; names containing `legacy` are rejection
  fixtures, not compatibility paths.
- [x] Route fixed and bounded delay-coordinate creation through
  `expressions.at(coordinate_provenance).delay*`. Delete the direct expression
  arena push and the old `Temporal::delay*` APIs; retain one atomic
  delay-owner/coordinate operation and add an architecture regression guard.
  - Signed commit `32362950` supplies canonical wire/structural replay,
    exact operand-span and atomic-failure tests, consumed-evidence compile-fail
    coverage, and a repository guard confining raw insertion facts.
- [x] Make effective Flat `TypeId` authoritative during DAE construction:
  reject a second conflicting layout and use erased construction-only indexes
  so type interning remains linear overall.
  - Signed commit `41575036` gives effective identities and structural layouts
    separate construction-only lookup proofs, rejects conflicting layouts with
    exact dual provenance, preserves distinct effective identities, reuses the
    earliest derived layout, and erases both indexes at freeze.
- [x] Preserve the exact start, explicit-step, and end occurrence provenance
  of range expressions. An omitted step stays structurally omitted or becomes
  typed generated semantics; it must not claim a source occurrence.
  - Signed commit `6da13baa` stores bounds as ordinary provenance-bearing
    expression children, distinguishes omitted from explicit steps, replays
    wire-v11 through checked range construction, and exercises production DAE,
    structural, Solve, evaluator, GALEC, and codegen consumers.
  - The schema-v4 generic template projection preserves each bound's child
    identity and provenance, and the MiniJinja target now owns Modelica range
    punctuation without a Rust presentation renderer.
- [x] Reserve only variables proven to need forward attributes, bindings, or
  references. Dependency-order complete variables instead of reserving every
  non-clock declaration.
  - Signed commit `0e20c9f4` preserves source-order IDs while completing
    backward-only variables directly, reserves only self/later or
    function-registry-dependent definitions, finalizes genuine forward groups
    in deterministic dependency-first SCC order, keeps ordinary attribute
    cycles representable, and rejects only recursive calculated bindings at
    their first exact internal use. Focused DAE tests and strict complexity
    Clippy are green.
- [ ] Replace borrowed whole-root structural replay with consuming,
  aggregate-bound closed DAE transforms that transfer unchanged immutable
  arenas and reconstruct only changed owners.
- [ ] Replace ToDAE provenance `expect` calls and broad owner-span fallbacks
  with exact guaranteed spans or typed missing-provenance failures.
  - Keep invariant `expect` sites that are already proven by the single
    recursive analysis pass; do not add redundant whole-tree validation.
  - [x] Delete record-array projection analysis that fabricates a
    source-attributed `0.0` merely to pass generic validation. Signed commit
    `edcb871a` validates the original occurrence through a private
    occurrence-keyed certificate, checks original subscripts and materialized
    roles at exact spans, and retains the end-to-end numeric/provenance test.
  - [x] Nested statement occurrences with no span fail as ED007 rather than
    borrowing an enclosing owner span. Signed commit `1d5d0f2f` removes all
    eight statement-owner fallbacks and tests both unlabelled ED007 and exact
    source-backed ED013 occurrences.
  - [x] Structural projection/reconstruction errors retain their exact typed
    provenance or remain global. Signed commit `f7fb1626` preserves the exact
    subscript occurrence and leaves source-free ES014 diagnostics unlabelled.
- [x] Add construction-only indexes for clock ownership and
  `(clock, variable, role)` temporal history so checked construction remains
  linear without changing dense canonical storage.
  - Signed commit `6cb405aa` erases variable-to-ownership/previous indexes and
    the aligned condition-owner clock cache at freeze, retains dense canonical
    arenas and first exact provenance, and replaces conflicts with a typed
    dual-provenance error. Focused DAE, wire, compile-fail, and strict-clippy
    gates are green.
- [x] Remove deep DAE cloning from rendering; share the immutable checked root
  without weakening branded construction ownership.
  - Signed commit `4f29093b` makes canonical DAE storage non-cloneable, shares
    one `Arc<Dae>` at retained compilation/cache boundaries, makes transient
    Solve rendering borrow `&Dae`, proves cache-hit pointer identity, and adds
    architecture guards against copy adapters. Production code is net -8
    physical lines and renderer setup no longer copies the arena or SourceMap.
- [ ] Replace result-expression-only functions with owner-scoped, typed
  function values and structured statement bodies.
- [ ] Support function locals, defaults, arrays, control flow, multi-output
  calls, assertions, and returns through checked construction operations.
  - Materialize executable call arguments into declaration order and substitute
    dependent defaults by exact formal identity. Preserve legal partial
    applications as function values; closure-convert their bound arguments
    into typed capture inputs before DAE construction. In particular,
    `QuadratureLobatto3`'s `fun7(A=A,w=ws)` intentionally leaves `u` unbound
    and must not be padded or rejected as an ordinary two-of-three call.
  - Current workspace contract frontier: restore structured function
    `while`, early `return`, and `break` in runtime-dependent finite `for`
    loops (`alg_005_while_boolean`, `alg_013_return_in_function`, and
    `alg_012_break_in_for`). Solve must gain executable checked control flow;
    do not specialize these only for constant test calls.
- [ ] Extend compact function folds to nested loops and runtime-dependent
  finite domains without DAE unrolling or guessed bounds.
- [ ] Construct conditional, indexed/slice assignment, and comprehension
  function bodies; the current real-MSL frontier is
  `Modelica.Electrical.Polyphase.Functions.symmetricOrientation`.
- [ ] Replace generic `expressions.at(...).array_update(...)` construction with
  an owner-scoped indexed-assignment operation. A checked array update should
  be constructible only as the direct root of its function assignment owner;
  target templates must continue to fail closed if one appears elsewhere.
  - Signed commit `b6b0b233` makes the Modelica template reject non-primary
    call outputs, mismatched array-update owners, and undeclared record
    variable types instead of emitting plausible source. Signed commit
    `a60c7333` transports those failures through typed DAE provenance so the
    diagnostic identifies the model occurrence rather than a template line.
    Constructor ownership remains open until the generic operation is removed.
- [ ] Add a compact executable Solve operation for checked function folds;
  Solve currently rejects a pending fold projection instead of silently
  unrolling or returning a default.
- [ ] Decode every wire-v12 function value and statement through those same
  operations.
  - After live replay is green, make v12 an operation-shaped canonical format,
    not a frozen-storage snapshot: omit constructor-derived expression
    type/variability/domain/scope columns and generated/result/fold facts that
    replay deterministically re-issues. Keep only semantic owners, exact
    provenance, explicit operands, and references needed to perform the same
    construction operations. Reject the superseded v12 shape; do not add a
    reader or adapter. This is the primary safe path toward the 11,000-line
    DAE cap.
- [x] Give every function assignment/fold transition a unique branded
  `FunctionDefinitionId`; `ExprId` is not definition identity because the same
  RHS may be assigned at distinct program points.
- [x] Derive exact construction-only function read sets with witness
  provenance, erase them at freeze, and replace whole-DAG assignment scans.
- [x] Delete crate-private raw function occurrence/fold wire reconstruction.
  Wire-v12 must replay live and dead occurrences through the exact
  `FunctionBody`/`FunctionLoop` owner and consume every occurrence once.
- [ ] Replace blanket `reserve_recursive` use with call-graph/SCC construction:
  acyclic functions construct directly in dependency order, and only genuinely
  self- or mutually-recursive SCC members receive linear forward authority.
- [ ] Closure-convert every legal enclosing constant before DAE function
  construction, including arrays/records/default arguments. Enclosing
  parameters and runtime coordinates remain illegal.
- [ ] Restore pure external functions as purity-bearing callables and represent
  legal impure initial/when/function calls as one ordered effectful action,
  never duplicated expression calls.
  - 2026-07-30 (commit `8143a0c6`): rumoca-ir-dae owns a checked MLS §12.9
    external function-definition kind (typed purity/language/symbol/ABI
    arguments/link facts, exact provenance, wire-v12 replay with negative
    fixtures); phase-dae lowers resolved external functions through it;
    Solve/eval/projection/GALEC fail explicitly with the exact symbol; the
    bare-purity advisory stays a nonfatal warning and func_032 is green with
    the whole workspace matrix (226 binaries, 0 failures). Open remainder:
    the one ordered effectful action owner for impure calls is not built
    (an impure call is still an ordinary expression call in its legal
    context), and no intrinsic-backed external executes yet.
  - 2026-07-30: `rumoca-ir-dae` now owns a checked MLS §12.9 external
    function-definition kind (`Functions::define_external`) carrying typed
    purity, a typed language, an exact entry-point symbol, ordered ABI
    arguments (`Input(ExprId)` / `Output(FunctionValueId)`), the return-form
    result binding, and `Library`/`Include`/`IncludeDirectory`/
    `LibraryDirectory` link facts. Every declared output is proven to be
    produced exactly once, argument expressions are proven closed over the
    function's own formals, and wire v12 replays the interface through the same
    construction op with negative fixtures for both-bodies, forged output
    identity, and unproduced outputs. ToDAE lowers resolved externals through
    it (`func_032` is green; MSL's `ModelicaRandom_impureRandom_xorshift1024star`
    constructs with its exact link facts). Solve, numeric evaluation, scalar
    projection, and the GALEC projection now fail explicitly with the exact
    symbol instead of looking through a body that does not exist. Still open:
    the *ordered effectful action* owner itself — an impure call is still an
    ordinary expression call inside its legal context, so nothing yet forbids a
    later transform from duplicating it; no intrinsic-backed external executes
    (the `LinearOp::ImpureRandom*` ops still have no producer); and Flat does
    not preserve the declared simple name, so an omitted external entry point
    is rejected rather than defaulted.
- [ ] Restore compact nested function folds with explicit lexical parent-domain
  ownership; nested Modelica algorithm loops must not be rejected or unrolled.
- [ ] Migrate structural transformations to checked DAE-to-DAE construction:
  alias elimination, BLT preparation, tearing, Pantelides, and dummy
  derivatives.
  - Direct state demotion and index-one holonomic differentiation now rebuild
    a finalized provenance-bearing DAE, and `gear_loop_regression` reaches the
    physical trajectory. Continuous and initialization structured families,
    nested domains, binders, arrays, indexing, conditionals, comprehensions,
    ranges, array updates, pure builtins, discrete-real equations, B.1c
    assignments, discrete/pre coordinates, conditions, relations, roots, time
    events, event actions, periodic and triggered clocks, clock ownership,
    previous values, terminal values, and fixed/bounded delays now survive this
    reconstruction with their exact semantic-owner provenance. Delay
    construction atomically inserts its unique coordinate, so no generic delay
    coordinate path or unconsumed-owner validator remains.
    Record layouts, constructors, field projections, parameter bindings, and
    attributes now survive reconstruction through checked record operations,
    including exact source/generated provenance. Recursive functions, compact
    folds, record values, global calls, dead scoped expressions, and all
    declaration/use/generated provenance now survive both state demotion and
    holonomic reconstruction through checked function-owner operations. Finish
    exact definition identity and body-owned wire replay before calling this
    boundary closed.
- [ ] Restore checked Solve lowering for every DAE partition: continuous,
  initialization, B.1b, B.1c, events, clocks, history, delays, and terminals.
  - Static `div`, `mod`, and `rem` now have constructor-proven finite operands,
    exact nonzero-divisor checks, wire reconstruction, and computable Solve
    programs. Restore dynamic quotient expressions only after DAE construction
    creates the MLS discontinuity owner (`floor(x/y)` for `mod`, truncated
    `x/y` for `div`/`rem`) and `noEvent` can suppress that owner explicitly.
    Add runtime domain evidence for parameter/input divisors before accepting
    them; never delegate a zero divisor or missed discontinuity to plausible
    floating-point output.
  - Scalar single-output model algorithms now remain continuous/algebraic
    where required, lower sequential assignments and conditionals through
    checked expressions, round-trip wire-v11, and reject implicit memory.
    Add one compact checked atomic vector-equation owner before accepting
    multi-output, array, or mixed continuous/event algorithms.
  - Total array-definition model `for` loops now retain one checked compact
    domain, comprehension, and structured equation owner
    (`alg_016_for_range`). Restore the multi-output sequential fold required by
    `alg_for_loop_algorithm` without scalar-unrolling its array or loop domain.
- [ ] Prove all compiler, evaluator, simulation, codegen, serialization, and
  binding consumers use the canonical `rumoca_ir_dae::Dae`.
- [ ] Prove no production source imports, constructs, decodes, or adapts a
  legacy DAE representation.
- [ ] Replace every removed legacy capability with its checked equivalent and
  equivalent tests; use the restoration table in SPEC 0036 as the inventory.
- [ ] Add compile-fail coverage for escaped IDs, cross-DAE IDs, cross-function
  values, missing provenance, and bypassed semantic owners.
- [ ] Add exact source/generated provenance coverage for every expression,
  declaration, statement, equation, event, clock, delay, and transformation.
- [ ] Restore browser-worker parsed-source acceleration with a proof-carrying
  cache entry that binds source identity, exact source bytes, parser version,
  and parsed tree. The current wire deliberately reparses retained source
  instead of accepting an unchecked serialized AST.
- [ ] Keep checked-DAE production within the reviewed split budgets: semantic,
  constructor, proof, and view core at or below 11,000 lines; private
  current-version wire replay at or below 3,250; total at or below 14,250.
  - 2026-07-29 adversarial audit: 14,203 production lines by the repository
    `production_source_lines` convention, 3,203 over the cap. The wire family
    is about 3,290 lines; consolidate operation-shaped wire replay, repeated
    owner views, function grammars, dense-arena accessors, and insertion/index
    helpers without moving code or deleting capability merely to meet the
    metric.
  - Treat the numeric cap as a bloat-review trigger. After the identified
    duplication is removed, retain additional clear, proof-oriented code when
    a module-by-module review shows it carries necessary semantics or
    construction evidence; amend SPEC 0036 with that evidence instead of
    obscuring behavior to hit a number.
  - Signed commit `9d2ff000` records these evidence-based split budgets and
    requires a new module review for any future increase.
  - 2026-07-29 16:05 cutover snapshot using the architecture-hardening
    production-line filter: worktree = 13,480, HEAD `39558ff1` = 12,620.
    The cap is failing by 2,480 lines and this dirty slice is +860; neither the
    cap nor DAE-local net-negative requirement is complete.
  - 2026-07-29 checkpoint `1f4e1c4a`: 13,306 physical production lines by the
    same convention. Branded-ID consolidation, constructor-derived wire fields,
    inline function construction operations, a single-pass function
    insertion-fact fold, declarative construction errors, and production/test
    separation are committed; 2,306 lines remain. The next active slices are
    equation-owner construction operations and proof-readable view ceremony
    reduction.
  - 2026-07-29 interim audit using the repository code-size convention
    (physical Rust lines under `src`, excluding `/tests/`, `tests.rs`,
    `*_tests.rs`, and `/generated/`): checkpoint `327f528c` has 13,031 DAE
    production lines and the moving shared tree measured about 13,918 before
    SourceMap/wire consolidation. This is not acceptance evidence; the final
    tree must remove about 2,918 lines under the same method.
  - 2026-07-29 checkpoint `61e045a7`: checked wire-v11 function/B.1c DTO
    consolidation removed 71 production lines, leaving 13,417 by the
    SPEC 0036 physical-line filter. The cap still fails by 2,417 lines.
  - 2026-07-29 checkpoint `a60c7333`: the same physical Rust filter measures
    14,492 DAE production lines. This includes the reviewed semantic-owner,
    function-body, wire replay, provenance, range, temporal-index, and
    effective-type work; it exceeds the 14,250 review trigger by 242 lines.
    Do not code-golf or remove capability: finish the operation-shaped wire,
    reservation, owner-scoped update, and consuming-transform reviews and
    document the necessary remainder module by module.
  - 2026-07-31: the three SPEC 0043 §1 triggers are now measured and gated by
    `crates/rumoca/tests/dae_loc_trigger_test.rs` (suite_gates member, no
    compiler link). Measured on the working tree by the §1a convention
    (physical Rust lines under `crates/rumoca-ir-dae/src`, excluding `tests/`,
    `generated/`, `tests.rs`, `*_tests.rs`): core = 12,503 (trigger 11,000),
    wire = 3,613 (trigger 3,250), total = 16,116 (trigger 14,250). All three
    exceedances are acknowledged in the SPEC 0043 §1b ledger with ceilings
    12,750 / 3,750 / 16,250 (measured rounded up to the next 250 lines). The
    acknowledgment records debt only; the module reviews this item demands are
    still open, and reduction stays routed through the operation-shaped wire
    item above and the ranked non-wire reductions below. Crossing a 250-line
    step in either direction fails the gate until the ledger row is rewritten.
- [ ] Record repository production LOC before/after and prove a net decrease.
  - 2026-07-29 16:05 snapshot with the same filter: worktree = 331,441,
    HEAD = 331,136, merge-base `1acf3641` = 389,693. Repository production is
    -58,252 versus the merge-base but +305 in the current dirty slice. This is
    progress evidence, not final acceptance evidence.
  - 2026-07-29 interim snapshot under the same physical-line convention:
    merge-base `1acf3641` = 440,610; branch HEAD = 371,787; moving worktree =
    373,109. Repository production is net -67,501 versus the merge-base, but
    the worktree adds 1,322 versus HEAD and the DAE-local cap remains open.
    Rerun only on the final clean tree for acceptance evidence.
  - 2026-07-29 clean checkpoint `a60c7333`, using the same path exclusions and
    physical-line count: merge-base `1acf3641` = 440,257; branch = 369,722;
    net = -70,535 repository production lines. This is a reproducible
    checkpoint, not final acceptance evidence while SPEC 0036 items remain.
- [ ] Prove release-build compile/structural wall time and peak RSS do not
  regress. The ordinary nonsingular path must remain borrowed; transformed
  paths must avoid repeated whole-arena reconstruction, per-expression scratch
  allocation, and quadratic semantic-owner insertion.
  - Function assignment construction no longer allocates an arena-sized
    visited bitmap, and definition-ID read facts make assignment work
    proportional to distinct local reads. Finish the consumed
    `FunctionBody -> FunctionLoop -> FunctionBody` transition so loop entry
    moves one environment instead of cloning all function values and parent
    mutation is unrepresentable. Structural function reconstruction is
    iterative and deterministic with sparse occurrence indexing; retain its
    O(E + U log U) bound or improve it.
  - Replace `Storage::finish_construction`'s global B.1c dependency-graph
    validator and per-assignment duplicate scans with the SPEC 0036
    incremental issued-order/topology capability. Construction must reject a
    cycle or duplicate at the responsible assignment, and successful root
    finalization must return to O(1) before freezing.
    - Pinned in SPEC 0036 by `99066096`: one atomic source-priority
      `when`/`elsewhen` owner per target; independent `when` duplicates are
      forbidden; value/guard/trigger/condition/relation current-`m`
      dependencies participate; `pre(m)` is a leaf; event-only `m` updates are
      B.1c definitions rather than generic event actions.
    - Canonical replacement: one ordered B.1c owner arena with packed target,
      branch, and value columns. A branch activation is either `Always` or
      `When { trigger: ConditionId, guard: ConditionId }`; clock ownership
      remains a separate typed relation and must not be smuggled into the
      trigger identity. Each branch value range has exactly the owner's target
      arity and is positionally aligned with it. A direct equation is one
      owner with one `Always` branch; one source `when`/`elsewhen` chain is one
      owner with source-priority ordered branches. An otherwise held `m` gets
      an explicit generated `m := pre(m)` owner with the declaration as its
      responsible provenance.
    - Construction ends through one consuming, non-cloneable branded topology
      capability. Its exact target plan is a permutation of the canonical
      non-input `m` role-view. Each atomic owner must match the next contiguous
      target slice, validates all branches before insertion, and advances one
      issued cursor. A private completed-construction token is obtainable only
      after the cursor reaches the plan length, including for the empty plan.
      The transient plan and issued bits are erased at freeze; no persistent
      seal, proof map, definition counter, or repair pass remains.
    - Every current-`m` dependency reachable through branch values, guards,
      triggers, condition nodes, relation expressions, expression operands,
      or index/subscript expressions must already be issued. `pre(m)` is a
      leaf, although expressions used to subscript an outer indexing operation
      still participate. Validate a staged owner completely before appending
      it or advancing issued state so a rejected owner cannot partially mutate
      the DAE.
    - Phase DAE stages semantic owners and stable-toposorts an owner graph,
      with a stable internal target order for multi-target owners. Guard and
      trigger dependencies apply to the whole owner; value dependencies add
      target/owner edges. Stable order derives from semantic source-owner and
      declaration/instance ordinals, never names, spans, or hash iteration.
      The constructor independently verifies the supplied order through the
      same capability.
    - Wire-v12 stores only ordered B.1c owner operations and replays them
      through the consuming topology capability. Remove the separate
      `discrete_assignments` wire column and discrete-value event-action
      variant; reject that superseded shape rather than decoding it. Structural
      transformations replay owners atomically, and Solve/codegen consume the
      same priority-preserving owner view.
    - Delete `EventActionKind::AssignDiscreteValue`, the legacy discrete
      assignment arena and ID, `unassigned_discrete_values`, duplicate scans,
      event-group reconstruction scans, and every final B.1c dependency
      validator in the same cutover. Successful construction costs O(M) plan
      validation plus O(owner payload + dependency edges), uses O(M)
      transient topology state, and returns to O(1) root finalization.
    - Required negative evidence covers duplicate or missing owners, invalid
      target roles, incomplete/shuffled plans, dependency cycles and
      not-yet-issued reads hidden in values/guards/triggers/conditions/
      relations/subscripts, malformed branch arity, obsolete wire fields, and
      escaped/cross-DAE/reused topology capabilities. Positive and property
      evidence covers `pre(m)` leaves, generated holds, stable acyclic orders,
      source-priority branch evaluation, wire round trips, transformations,
      algorithms, clocks, arrays, representative MSL, and OMC parity.
    - Mandatory prerequisite A: replace Flat's `Vec<WhenClause>` result from
      `flatten_when_blocks` with one `WhenChain` semantic owner retaining its
      ordered branches, and mint one Flat-owned branded `InstanceValueId` for
      every concrete value occurrence. `DefId`, rendered names, and spans are
      not concrete instance or chain identity and must not be used as an
      adapter.
    - Implementation graph: A is the atomic Flat `WhenChain`/
      `InstanceValueId` replacement across Flat, flattening, and all Flat
      consumers. B is independent constructor-derived read-set hardening.
      C depends on A (and uses B when available) and is one atomic integration
      commit assembled from disjoint workstreams: C1 checked-DAE core
      storage/API/errors/views; C2 phase-DAE owner analysis, stable topology,
      and algorithm lowering; C3 wire-v12 operation replay; C4 structural,
      Solve, codegen, GALEC, evaluator, and runtime consumers. C1-C4 must not
      land separately because coexistence or a temporarily broken consumer is
      prohibited. D adds extended compile-fail/property/MSL gates and removes
      dead code, while C itself includes the basic replacement tests needed
      for a coherent commit.
    - Remaining semantic blockers are exactly the erased Flat chain boundary,
      absent concrete instance identity, and statement-by-statement algorithm
      lowering. Algorithms must produce one explicit final owner for each
      `m` target, including structured outputs, or reject an unsupported form
      before DAE construction; preserving generic event assignments is not an
      option.
  - Ranked non-wire reductions after the operation-shaped wire work:
    consolidate one-source construction/storage/view tables; make issued
    branded-ID access total inside its owning DAE; derive insertion facts in
    one structural pass; move diagnostic formatting to declarative variant
    metadata; replace B.1c final scans with the specified incremental topology;
    and remove serialization derives from invariant-bearing children once the
    borrowed wire projection owns serialization.

## Required DAE cutover evidence

- [ ] Focused `rumoca-ir-dae` and `rumoca-phase-dae` suites pass.
- [ ] Wire-v12 positive round trips and malformed-wire negative cases pass.
- [ ] Strict Clippy passes for all targets and features.
- [ ] Complete DAE, structural, Solve, simulation, codegen, and workspace
  suites pass with 16-way Cargo scheduling.
- [ ] Representative real MSL models cover scalar, array, function, event,
  clock, delay, and high-index mechanics paths.
- [ ] Full MSL/OMC parity gate runs after the reliable-core gates pass.
- [ ] Every coherent change is committed and final Git status is clean.
- [ ] Before PR publication, add the SPEC 0025 DCO sign-off trailer to every
  branch commit in one reviewed history rewrite; current local cutover commits
  predate that final publication step.

## Subsequent correct-by-construction IR work

- [ ] Enforce checkpoint `180f183f` across every `rumoca-ir-*` crate: source
  parsers, generated grammars, recoverable CST/state, and syntax diagnostics
  live only in `rumoca-phase-parse*`; IR crates retain compact checked data,
  constructors, read-only views, and invariant-replaying current wire decode.
- [ ] Flat IR makes structurally invalid flattened classes unrepresentable,
  including classes that still own nested class children.
  - Make `WhenChain` structurally nonempty: private `first: WhenBranch`,
    ordered `else_when: Vec<WhenBranch>`, required owner span, and
    allocation-free immutable/mutable iterators. Remove the empty constructor,
    public branch vector, and generic `add_branch`; derived decode must require
    the first branch.
  - Preserve all event-equation capability while hardening the boundary:
    assignment, reinit, assert with optional level, terminate, nested
    conditional with source-present `Option` else, ordered multi-output call,
    for expansion, clock/sample/previous context, and exact branch/action
    provenance. Unsupported side-effect calls must error at their source span,
    never disappear.
  - Required event evidence includes strict assert/terminate argument
    decoding, duplicate target rejection, nested one-if-plus-else target
    equality including explicit-empty else, CLK-014 rejection of clocked
    elsewhen, exact for-expansion order, all-variant rewrite fixtures, and two
    full-pipeline priority simulations: later rise masked while the first
    condition remains true, and simultaneous rise selecting the first branch.
  - The checked DAE B.1c owner, not generic event-action insertion order,
    retains the complete chain provenance, a nonempty ordered branch slice,
    per-condition/action provenance, and one claimed target set. Wire replays
    that owner through construction and Solve consumes its order directly.
  - A3 exact equation provenance is one atomic AST-to-DAE cutover. Replace the
    unspanned `ast::Equation` enum with an opaque `Equation { kind:
    EquationKind, span: Span }`; remove `Default`, the variant-local function
    call span, and duplicate `InstanceEquation.span`. Parser rules retain
    opening/closing boundary tokens so every nested simple/connect/for/if/when/
    call/assert equation owns its exact byte range before resolve.
  - A3 rewriters use `map_kind`/`kind_mut` so transformation cannot drop or
    replace the occurrence span. Recursive instantiate/flatten lowering reads
    each child's own span rather than threading the enclosing owner span.
    Generated equations use typed generation plus the responsible source
    equation span. No `DerefMut`, dummy/source-free constructor, public fields,
    old serde shape, or span-reconstruction adapter is permitted.
  - A3 evidence includes parser source-substring checks for every equation
    form and nested actions, rewrite span preservation, removal of all
    first-token span fallback, and a real Session source through Flat and
    checked DAE showing distinct action source text within one when owner and
    across elsewhen branches.
  - Production flatten now rejects declarations without structured identity
    and non-generated occurrences without a resolved identity or matching Flat
    declaration (`EF023`/`EF024`). Close the remaining direct public-field
    assembly bypass in `rumoca-ir-flat`.
  - Mint one Flat-owned branded `InstanceValueId` for each concrete aggregate
    value occurrence. `DefId` remains declaration/ancestry identity and cannot
    distinguish repeated instances. Arrays retain one aggregate ID plus
    structured subscripts.
  - Replace loose core references in Flat expressions with a total typed
    reference grammar (`Value(InstanceValueId)`, binder, function parameter,
    intrinsic), each retaining its exact use span. Variables retain the
    concrete ID, declaration span, and source `DefId`.
  - Make post-Flat constant evaluation exclusively `InstanceValueId` keyed and
    return `Evaluated` or a typed `RuntimeDependent` reason. Delete
    scoped/suffix/string lookup rather than adding a resolved mode. Flat wire
    must replay typed reservations and references through construction.
  - A2 architecture precondition: amend SPEC 0029's accepted semantic-equality
    ownership in the same change that implements the cutover. Core retains one
    parametric structural shape and structural equality; each IR owns its
    concrete target payload and invariants. Do not land a default generic
    payload, an `Expression = Expression<Reference>` compatibility alias, or an
    intermediate untyped Flat instantiation.
  - The core structural family is generic over one total target:
    `SemanticExpression<T>`, `SemanticSubscript<T>`,
    `SemanticComprehensionIndex<T>`, `ComprehensionTemplate<T>`,
    `SemanticStatement<T>`, `StatementBlock<T>`, `ForIndex<T>`,
    `SemanticFunction<T>`, `FunctionParam<T>`, and external-function
    expression payloads. AST remains its syntax-preserving tree; Flat must not
    duplicate this semantic tree.
  - Flat's private canonical target grammar distinguishes
    `Value(InstanceValueId)`, `Function(FunctionId)`,
    `FunctionValue(FunctionValueId)`, `DomainBinder(DomainBinderId)`,
    enum literals, `Intrinsic(Time)`, and typed generated semantic owners.
    Public construction methods remain kind-specific, then erase to the
    private total enum. Function/domain IDs carry nested brands and cannot
    escape their owner closures.
  - Target entries store one canonical display name and either exact source
    declaration provenance (`DefId` plus declaration span) or typed generated
    provenance with its responsible span. Expression nodes store exact use
    spans. `DefId`, names, and source paths never participate in Flat identity
    or lookup after construction.
  - Producer sequence: reserve all concrete primitive/aggregate values and
    proven recursive function headers; build one transient
    `ast::InstanceId -> flat::InstanceValueId` transition map from the
    authoritative overlay; lower every expression inside its semantic-owner
    closure; mint function locals and domain binders only inside nested
    closures; issue generated connection/alias/array-view targets before use;
    erase all transition indexes at freeze. Undefined, ambiguous, wrong-kind,
    or missing-provenance references fail at their exact occurrence.
  - Delete `pipeline/instance_identity.rs`, its seeding/call sites in
    `flatten_pipeline.rs` and `context_and_tests.rs`, and its fabricated
    `max(source DefId) + InstanceId` namespace. Delete
    `postprocess_def_id.rs` and `structured_refs.rs` late whole-model repair;
    neither may survive as validation or wire compatibility.
  - Flat wire is a root-owned construction-operation projection. It serializes
    owner ordinals, typed references, and provenance rather than process-local
    brands or repeated display text. Decode reserves semantic owners and
    replays through `flat::Model::construct`. CLI/JSON output is a one-way
    root-owned display projection, not a deserializable child shape.
  - A2 must integrate as one commit assembled from disjoint workstreams:
    A2.1 generic shape/structural visitors in `rumoca-core`; A2.2 Flat target
    arenas, construction, views, display, and wire in `rumoca-ir-flat`; A2.3
    phase-flatten reservation/resolution/emission and repair deletion; A2.4
    `rumoca-eval-flat`; A2.5 phase-DAE typed transition maps and exhaustive
    target matches; A2.6 codegen/CLI projection plus all fixtures and tests.
    None may land separately or introduce an adapter.
  - Core surface: `ir_primitives.rs`,
    `ir_primitives/{component_refs_and_functions,expression_semantics,reference_serde}.rs`,
    `expression_{visitor,rewriter}.rs`, `statement_rewriter.rs`,
    `subscript.rs`, `structured_domain.rs`, and exports/tests. Producer surface
    spans phase-flatten expression/algorithm/function/equation/connection/
    postprocess/pipeline modules. Consumers span `rumoca-eval-flat`,
    phase-DAE construction and analysis, phase-codegen renderers, compile/CLI
    output, and Flat/DAE/MSL tests.
  - Required evidence: compile-fail cross-Flat, escaped function/binder,
    wrong-kind call/value, and missing-provenance cases; negative unresolved,
    ambiguous, malformed-wire, and forged-ordinal cases; exact
    declaration/use/generated provenance; deterministic readable snapshots;
    semantic-equality and wire round trips; representative MSL and OMC parity.
    Record construction time, peak memory, serialized size, and expression
    traversal time. Target leaves should be compact integer identity and must
    remove per-occurrence cloned names/component paths and global repair scans.
- [ ] Resolve IR represents only resolved references; undefined references are
  phase errors and cannot inhabit the successful IR.
  - Full-path function lookup now clears partial package identity, so
    `Known.missing()` fails as `ER002` at the exact call target while
    `Known.present()` retains the member `DefId`. `ResolvedTree` is now owned
    and privately constructed by phase-resolve through one completion gate;
    failed whole-root resolution yields only a planning ClassTree. Strict
    compilation prunes to the exact reachable definition/ancestor closure and
    re-resolves it, so an unreachable broken sibling in the same source
    document cannot enter or block the valid target proof.
  - Benchmark selected-closure re-resolution for bulk source-root compilation.
    Successful target proofs are cached; if repeated targets with identical
    source closures remain material, group them by deterministic closure
    fingerprint and resolve each unique closure once.
  - [ ] Make the resolved ExternalObject lifecycle one typed owner fact:
    specialized `class` restriction, direct canonical `ExternalObject` base
    identity, exactly the owned non-replaceable constructor/destructor
    declarations, their checked signatures, and exact declaration provenance.
    Strict reachable-closure planning and Flat must consume this same fact so
    pruning cannot separate a class from either lifecycle function. Reject
    short definitions, use of a derived ExternalObject as an extends base,
    non-constructor returns of ExternalObjects, constructor defaults on
    function inputs, and any post-binding modification/assignment before this
    item is complete.
  - [ ] Replace Flat's temporary constructor-only executable projection with a
    typed external-object lifecycle target carrying both constructor and
    destructor identities. Checked DAE/FMI lowering must own exactly-once
    construction/destruction semantics; until then, a genuinely used external
    object must fail at the lifecycle boundary rather than become a structural
    record or a plausible default value.
- [ ] Solve IR represents only executable/computable programs; missing runtime
  capabilities and structurally singular systems fail construction.
  - Complete scalar programs now require an explicit output, single-result
    evaluation requires exactly one output, and setup prefixes use an explicit
    zero-output evaluator. ScalarProgramBlock now has private parallel columns,
    typed/exact provenance, constructor-proven register flow and outputs, and
    checked current-wire decoding; consumers no longer revalidate it. Close the
    remaining public ComputeNode/ComputeBlock and Solve root bypasses.
- [ ] All three migrations retain readable fixtures, compact storage, branded
  ownership, exact provenance, and wire construction invariants.

## Proof-carrying sparsity and complex values

- [x] Replace caller-claimed sparsity enums with private, provenance-bearing
  structural patterns derived from checked dependency facts.
- [x] Preserve `Known` versus `Unknown` dependency state and reject missing
  registers, outputs, provenance, and out-of-range seeds instead of treating
  them as structural zeros.
- [x] Derive deterministic checked column coloring and omit derived structural
  artifacts from canonical Solve serialization.
- [x] Make tensor linear-solve kernel selection depend on checked structure,
  never pointwise floating-point zeros.
- [x] Feed the checked may-depend relation to the Diffsol/Faer construction
  probe so a derivative that is numerically zero at the initial point is not
  permanently omitted.
- [ ] Add the compact checked affine representation required by SPEC 0039;
  canonical metadata must remain independent of domain cardinality.
  - The 2026-07-31 affine-frontier work landed the *decomposition*, not the
    representation. Solve lowering now decomposes a matched state row into a
    coefficient and a numerator across a translation-time conditional, folds a
    coefficient declaration through the scalar pure builtins, and resolves a
    derivative an algebraic or initial row reads through the continuous row
    that defines it. Accepted coefficient declarations are literals, parameter
    bindings, unary/binary arithmetic and relations, the scalar-valued pure
    builtins, and conditionals; accepted branch guards are literals and
    parameters a simulation cannot override (`final` /
    `annotation(Evaluate=true)`, i.e. `is_tunable == false`). Everything else
    keeps its previous EL005 rejection.
  - Honest remainder for this item:
    - There is still no first-class `StructuralPattern::Affine` owner. The
      coefficient remains an ordinary DAE expression compiled into the row, so
      no affine metadata exists to keep independent of domain cardinality.
    - The zero-coefficient obligation is still a compile-time probe of the
      *declared* value. A tunable coefficient a run overrides to zero divides
      by zero at run time (loud non-finite, not a silent trace). Discharging it
      at initialization needs a checked guard operation, which
      `rumoca-ir-solve` does not have.
    - Derivative resolution is scoped to continuous algebraic rows and
      initialization residuals. Discrete, event, clocked, and retained-manifold
      programs still reject a derivative coordinate, and the resolution
      recomputes the defining right-hand side instead of sharing the derivative
      row's output; a Solve derivative slot would remove the recomputation.
    - The translation-time guard is resolved at Solve lowering, not at DAE
      construction where `Evaluate=true` is already known. Structural incidence
      therefore still admits the derivative in every branch, which is
      conservative-correct for matching; a guard that deletes the derivative
      reaches Solve matched to it and is rejected as a non-affine product
      rather than silently divided by that branch's zero coefficient.
- [ ] Execute compressed AD when the checked color count is lower than the
  column count and prove dense/compressed equivalence.
- [ ] Complete deterministic dense/sparse execution-policy estimates and
  representative structured-MSL benchmarks.
- [ ] Introduce precision-neutral checked Real/Complex element kinds and prove
  each complex-to-real lane expansion; unproved rules remain conservative
  full 2-by-2 blocks.
- [ ] Add malformed affine, coloring, complex-lane, and wire reconstruction
  negative/property tests before promoting SPEC 0039.

## Fail-early trust audit

- [ ] Delete the public allow-unbalanced ToDAE/session/worker lane. Balance
  diagnostics may retain Flat plus `BalanceDetail`, but an unbalanced model
  must never materialize a DAE that can reach simulation.
- [x] Preserve the former Flat `WhenClause` action surface in the owned
  nonempty `WhenChain`: assignment, reinit, assert with optional level,
  terminate, nested conditional, and multi-output function-call actions.
- [x] Preserve source-priority `when`/`elsewhen` ownership and compile-time
  selection of structural conditionals; inactive alternatives must not create
  duplicate-definition failures.
- [x] Reject sequential duplicate event definitions and independent event
  owners in Resolve, Flat production, and ToDAE analysis while permitting
  mutually exclusive `if` and `elsewhen` alternatives.
- [x] Remove `ResolveOptions`, every session/config/snapshot/query strictness
  copy, and the Wasm compile-behavior entry points atomically. They were
  compatibility bypasses that could downgrade mandatory ANN-008/ER070 and
  EQN-020/ER053 and admit an invalid `ResolvedTree`.
- [ ] Give event multi-output function calls a checked typed DAE action owner;
  Flat preserves them today and ToDAE fails explicitly rather than dropping
  them.
- [ ] Restore `edge(...)` conditions through typed current/pre coordinates and
  checked condition ownership. The checked ToDAE expression whitelist rejects
  them today; do not replace the failure with an unowned expression call.
- [ ] Restore dynamic and concatenated assertion/termination messages as
  executable checked message programs. Solve accepts only literal messages
  today and must continue to fail explicitly until the runtime consumes the
  checked result.
- [ ] Add full source-to-simulation event regressions for nested conditional
  actions, expanded multi-index `for` actions, and a legal single-branch
  clocked `when`; focused Flat or direct checked-DAE fixtures are not
  sufficient evidence.
- [ ] Add a source-pipeline negative regression for multi-output event calls
  until the checked function-action owner is implemented.
- [ ] Define checked assertion-level execution semantics in Solve. Flat, DAE,
  and wire preserve the optional level today, but Solve must keep failing
  explicitly until it can execute the typed level rather than ignore it.
- [ ] Restore clocked assert/terminate execution through the owning clock
  capability. Solve rejects these actions today rather than running them
  outside their clock domain.
- [ ] Add a source `when`/`elsewhen` wire-v12 round trip followed by Solve and
  simulation, including the case where a later condition rises while the
  first remains true and then remains true after the first falls. The later
  branch must not execute belatedly.
- [ ] Add source-to-simulation coverage for conditional `reinit`, vector when
  conditions, and clocked `previous`, plus malformed-wire rejection for forged
  duplicate or cross-owner event assignments.
- [ ] Remove or replace the orphan `rumoca-ir-flat::ClockPartitions` prototype
  during the checked Flat cutover. It is not owned or populated by
  `flat::Model`, uses raw `u32` partition IDs, and duplicates whole `Equation`
  values, so it cannot be a second clock-partition authority. The replacement
  must be constructed by the Flat root, use branded equation/variable IDs, and
  store only ownership/partition relations over the model's canonical arenas.
  Until that atomic cutover, ToDAE is the earliest phase that proves Appendix-B
  clock ownership and must remain the single production authority; do not
  translate through or attach the orphan prototype as an adapter.
- [ ] Complete the opaque spanned AST `EquationKind` cutover so each Flat event
  action receives its own exact source span instead of the enclosing
  `when`-owner span.
- [ ] Make lockstep transport advance only after a typed, validated frame has
  been atomically applied; distinguish timeout, disconnect, backpressure, I/O,
  and malformed frames.
- [ ] Replace name-plus-`f64` model input writes with lifecycle-checked typed
  FMI value references and all-or-nothing prepared batches.
- [ ] Carry strict/non-strict root zero ownership into the FMI-ME event
  indicator contract; remove the currently unused parallel metadata vector.
- [ ] Replace optional/zip-truncated pre, previous, relation-memory, and root
  copies with constructor-proven branded slot bindings.
- [ ] Preserve the first provenance-bearing model-evaluation error through
  Diffsol callbacks; NaN must remain data rather than an error channel.
- [ ] Make `SolveProblem`/`SolveModel` private and computable by construction;
  remove public defaults, raw index assembly, whole-root validation, and
  `filter_map` loss of dynamic events.
  - Amend SPEC 0029/0007/0036 before the Solve constructor cutover: move the
    closed semantic-generation classification into `rumoca-core`, while DAE
    and Solve retain stage-owned checked provenance wrappers. Every scalar or
    tensor program must carry typed owner provenance; a program synthesized
    from multiple semantic occurrences must also carry a nonempty,
    source-ordered packed contributor range.
  - Replace `ScalarProgramBlock`'s raw `program_spans` interface atomically
    with private parallel program/provenance/output columns and a packed
    contributor buffer. Conditional B.1c rows retain owner provenance plus
    every branch-activation and target-value provenance in source-priority
    order; selecting the first branch span is prohibited.
  - Bump the Solve wire schema and replay only checked construction. Reject
    the superseded shape without a reader or default. Required evidence covers
    mixed source/generated branch contributors, JSON and binary round trips,
    malformed/dummy/misaligned/range-overflow rejection, and evaluator
    diagnostics selecting the actually executed failing contributor.
- [ ] Advance periodic clocks by exact typed tick ordinals; conversion failure
  must be a provenance-bearing host error, never a floating fallback or a
  skipped event.
- [ ] Remove unique-suffix/textual constant recovery from ToDAE analysis and
  stop swallowing exact-identity evaluation failures. Do this only after Flat
  mints a typed concrete-instance value identity: `DefId` identifies the
  declaration but cannot distinguish repeated instances, and rendered path
  plus `DefId` is still a prohibited textual semantic key. `EvalContext` must
  switch exclusively to the typed identity, without resolved/text modes.

## Verification and certification design

- [ ] Reactivate and refine deferred
  [SPEC 0037](../spec/archive/deferred/SPEC_0037_FORMALLY_VERIFIED_COMPILER.md)
  after the IR shapes stabilize.
- [ ] Define per-phase refinement relations, proof obligations, translation
  validation certificates, deterministic semantics, and evidence artifacts.
- [ ] Connect source provenance, requirements, tests, generated code, and
  compiler-version evidence into an auditable certification chain.
- [ ] Define and pin the generated-C assurance profile: ISO C dialect,
  MISRA C:2023 guideline set, MISRA Compliance:2020 enforcement methods,
  analyzer/tool versions, reviewed deviations, compiler/ABI/floating-point
  assumptions, and deterministic template hashes.
- [ ] Add per-artifact translation validation and a generated-source map from
  every C operation through the closed GALEC projection to checked-DAE
  provenance. Reject operations whose defined C behavior cannot be proved;
  integer division by zero and signed overflow must never reach emitted C.
- [ ] Map Rumoca lifecycle evidence to applicable DO-178C objectives and the
  DO-330/DO-331/DO-333 roles without claiming that generated source alone is
  certified. Have the profile and deviation process reviewed by qualified
  certification expertise before making a compliance claim.
- [ ] Make wrong-but-plausible simulation results impossible where a checked
  precondition or runtime capability is absent; fail at the first responsible
  phase.

## Main runtime and code-generation targets

- [ ] Move every language-specific emitted fragment out of Rust codegen
  helpers and into its owning Minijinja target. Rust may expose typed,
  language-neutral checked projections and generic packaging/capability
  validation only; it must not assemble C, JavaScript, Python, Rust, WGSL,
  CUDA, or MLIR syntax with strings.
  - Reserve `rumoca-phase-codegen/src/codegen/` for the documented custom
    MiniJinja command API only. Move generic rendering orchestration out to
    renderer modules and typed IR adapters under `src/views/`. Every command
    needs one registry entry with template syntax, typed contract, failure
    behavior, complexity, and tests; commands never return target fragments.
  - Audit baseline after moving GALEC ownership: 873 production Rust uses of
    `format!`/`write!`/`writeln!`/`concat!`/`push_str`/`replace`/`replacen`
    under `rumoca-phase-codegen/src`. The architecture gate target is zero
    generated/context-text assembly sites; only sealed diagnostics and generic
    template/file transport may remain.
  - The universal boundary is `(proven-valid IR by construction) -> typed
    read-only MiniJinja view -> target.toml directory -> artifacts`. A new
    target over an existing IR must require no Rust change. Syntax, Resolve,
    Flat, DAE, Solve, GALEC, and future checked export IRs each expose one
    target-neutral semantic view; the target manifest selects the exact input
    artifact. Rendering must not resolve, typecheck, lower, mutate, or repair
    it. Generic Rust helpers may derive schedules, shapes, dependency proofs,
    bounds, semantic IDs, and provenance, but never target tokens,
    punctuation, statements, identifiers, or expressions.
  - Preserve and strengthen target evidence before each atomic cutover: C
    compilation, Rust compilation, CasADi/JAX evaluation and AD, WGSL
    structure/size, MLIR execution, and the built-in target capability sweep.
  - Remove the C/Rust linear-solve template helpers' singular/invalid-system
    `0.0` fallback. A checked capability/proof must exclude the operation or
    the generated ABI must return explicit failure status; never emit a
    plausible numeric default for an uncomputable solve.
  - Migrate in capability-owned slices: checked DAE Modelica; Flat/AST
    Modelica; C/CUDA; Rust; CasADi/JAX; WGSL plus its JSON manifest; MLIR.
    Each slice exposes structured operations, dimensions, schedules, symbols,
    and provenance to templates and deletes its Rust dialect in the same
    change. No compatibility renderer remains.
  - Extend the generic template failure transport to accept semantic
    provenance and report the responsible model span; target `fail(...)`
    diagnostics must not point only at a MiniJinja source line.
  - Give the DAE Modelica target one deterministic identifier allocation and
    escaping policy for flattened component/function/synthetic names. Add
    golden coverage for structured names, every expression form, function
    folds and multi-output calls, structured families, and initialization
    equations before treating the target as a round-trip artifact.
  - The current production string emitters are concentrated in
    `checked_modelica.rs`, `render_expr.rs`, `render_stmt.rs`,
    `render_solve_ops.rs`, and the syntax portions of `render_solve.rs`,
    `render_solve/dense_solve_render.rs`,
    `render_solve/template_partition.rs`, and
    `render_solve/mlir_family.rs`. JavaScript has no production Rust emitter.
  - Delete unused registrations/wrappers only after proving no target or
    capability test consumes them. The removed `render_c.rs` and
    `render_c/discrete_statespace.rs` were unused by every built-in template
    and recovered removed equation shapes with guessed/default RHS values.
  - Delete the `rumoca-galec-codegen` crate atomically. Checked GALEC/package
    data and constructors belong in `rumoca-ir-galec`; DAE/Solve semantic
    lowering and admissibility belong in `rumoca-phase-galec`; generic
    rendering sees only `src/views/algorithm_code.rs`. The
    `rumoca-phase-codegen/src/galec/` directory must not exist. No facade,
    alias, duplicate module, or compatibility dependency may retain the old
    ownership.
  - The remaining GALEC C path currently supplies preprinted `c_lines`, while
    `galec-production` passes through whole pre-rendered C/header strings.
    Replace both atomically with a closed typed language-neutral semantic view
    and MiniJinja-owned GALEC/C syntax, then remove the Rust printers. Do not
    claim GAL-008 compliance while the context contains target fragments or
    passes through open `serde_json::Value`.
  - Complete the GALEC language/projection valid-by-construction follow-up:
    interior expression nodes currently lack exact spans, and
    `AlgorithmCodePackage` remains publicly field-mutable with rendering-time
    validation. Move exact typed provenance onto every GALEC node, make the
    package opaque and constructor-issued, then replace package validators
    with boundary parsing/checked construction. C statement projections may
    inherit a checked enclosing owner span only until that cutover lands.
  - Delete every Rust eFMI manifest/schema/context model. Checkpoint
    `30cacf7e` makes the entire eFMI package a MiniJinja/`target.toml` target:
    templates own XML hierarchy, constants, references, filenames, C mapping,
    and artifact policy. Rust retains only target-neutral checked semantic
    facts and generic documented commands for hashing rendered bytes,
    validating declared schemas, and assembling the declared artifact graph.
    Replace `TargetBuildKind::Efmu`, `compile_efmu_target`,
    `write_efmu_zip`, `efmi_asset_source`, and hardcoded
    `__content.xml`/`.efmu`/`efmi-schemas` policy with generic declared
    package/archive/required-file/asset operations. Target directories own
    the eFMI names and vendored schema bundle; preserve deterministic atomic
    directory and zip output plus the exact license bytes.
    - Checkpoint `f9e48722` closes the asset loophole: builtin discovery embeds
      arbitrary target-directory assets recursively; external targets resolve
      asset sources relative to their own directory. Delete the CLI build
      script/schema registry and do not replace it with another logical bundle
      registry.
    - GALEC Production Code currently pipes component identifiers through the
      Rust `sanitize` filter. Replace that lossy target spelling with one
      template-declared `allocate_symbols` policy reused by the C header,
      source, and Production Code manifest. No GALEC/C-specific Rust filter or
      mangler may replace it.
  - Make the Production Code Integer domain explicit in `target.toml` and
    derive generic provenance-bearing interval proofs for every emitted
    Integer operation. Unproved overflow must reject code generation; neither
    the evaluator nor generated C may wrap, saturate, guess, or invoke signed
    overflow. Differential tests must exercise both i32 boundaries.
  - Move `.alg` parsing, its generated grammar, recoverable syntax state, and
    parser-only dependencies/tests from `rumoca-ir-galec` into
    `rumoca-phase-parse-galec`, as specified by checkpoint `8d606299`.
    Remove the IR parse feature and update LSP/Wasm/round-trip consumers
    directly; no re-export or compatibility facade may remain.
    - Public production parsing returns `CheckedAlgorithmBlock`. Invalid raw
      syntax remains opaque inside a phase-owned editor document exposing only
      diagnostics and navigation; raw blocks/expressions do not escape.
  - Add `rumoca-eval-galec` as a small independent executable semantics for
    checked Algorithm Code. Differentially compare checked DAE/Solve behavior,
    eval-galec, and generated GALEC/C/eFMI, including statement order,
    lifecycle methods, arrays, calls, `limit`, NaN comparisons, conversions,
    signals, and failure paths. The host state-setting API must preserve
    checked type/shape/interface invariants; the evaluator must not fabricate
    zero dimensions/outputs, silently ignore unsupported limits, truncate
    arity, or permit nonterminating saturated loop counters.
    - Split `phase-galec/lower.rs` and `eval-galec/interpreter.rs` into
      cohesive modules before landing; both exceeded SPEC 0021's 1,000-line
      review threshold during the cutover.
  - Delete `rumoca-lsp-position`; keep its Unicode/CRLF capability once in
    protocol-neutral `rumoca-core::text_position`, with local `lsp_types`
    adapters in both language servers.
- [ ] Implement deferred
  [SPEC 0038](../spec/archive/deferred/SPEC_0038_UNIFIED_FMI_EXECUTION.md)
  before this branch is considered complete.
- [ ] Replace the current Rumoca-specific solver/model boundary with exactly
  the FMI 3 Model Exchange interface; numerical methods must not consume
  `SolveModel`, Solve layouts, or Solve opcodes directly.
- [ ] Use one checked model kernel and FMI lifecycle for FMI 2/3 ME/CS.
- [ ] Treat native C, linked in-process execution, packaged FMUs, and Wasm as
  deployment forms rather than separate semantic lowering paths.
  - Restore the former browser-standalone user capability only through this
    FMI 3/Wasm path. The old direct DAE-to-JavaScript simulator read removed
    fields, guessed symbols, and emitted an empty model under the checked
    schema; it is not a compatibility path to retain.
- [ ] Treat the Diffsol runtime as an in-process FMI 3 ME host; allow CS to
  embed the same solver when selected.
- [ ] Refactor `rumoca-solver` into the FMI ME importer/host layer; keep
  Diffsol, RK45, BDF, and future methods behind one internal integrator
  contract instead of parallel model/runtime APIs.
- [ ] Preserve `rumoca-input` device polling, local controls, debounce,
  preconditions, and signal mapping, but replace `(name, f64)` plus private
  `SimulationSession::set_input` calls with metadata-resolved, typed, batched
  FMI 3 setters and legal FMI lifecycle transitions.
- [ ] Remove backend-specific and binding-specific model-input setters after
  every caller uses the FMI input contract; reject unknown names, wrong types,
  wrong shapes, non-input variables, and illegal lifecycle writes before
  model evaluation.
- [ ] Keep `rumoca-codec` transport-neutral, but replace scheduled-simulation
  frame-to-model `set_input`/lookup calls with a prepared typed mapping to
  batched FMI 3 setters/getters; reject invalid mappings before stepping.
- [ ] Compile `rumoca-input`/codec mappings into explicit ME or CS I/O
  schedules: linked simulation uses ME lifecycle points, CS uses communication
  points, and sub-step CS updates require advertised Intermediate Update
  capability. Never drop, delay, or interpolate an unsupported update.
- [ ] Keep UDP/Zenoh/WebSocket transport crates payload/control-only and
  compose transport → codec → FMI binding in the scenario runtime. Replace
  swallowed send failures and timeout/error collapsing with typed outcomes;
  lockstep I/O failure must not advance a successful model step.
- [ ] Keep eFMI Algorithm Code and Production Code/GALEC as primary
  safety-oriented generation targets.
- [ ] Keep CasADi, JAX, SymPy, SymForce, Julia ModelingToolkit, and ONNX as
  checked primal-Solve analysis projections, not alternate semantic pipelines;
  compiler AD/Jacobian artifacts are an optional separate product.
- [ ] Add native/packaged/Wasm and ME/CS cross-form trace-equivalence evidence.
- [ ] Preserve the OMC model inventory, trace comparator, tolerances, and
  diagnostics behind a backend-neutral runner contract; exercise both native
  FMI 3 and Wasm FMI-LS runners without Diffsol-specific harness coupling.
- [ ] Set and measure batched native/Wasm FMI state-access latency and
  simulation-throughput budgets; native in-process access should remain
  eligible for zero-copy implementation.

## Cross-cutting repository gates

- [ ] Re-run the production-LOC report at final cutover. The 2026-07-30
  intermediate exact-identity/String boundary measured 456,357 → 459,575
  repository production Rust lines (+3,218) and 17,431 → 17,908
  `rumoca-ir-dae` production lines (+477), including wire 1,660 → 1,672
  (+12). This exceeds the SPEC 0036 core review trigger, so the final module
  inventory must distinguish necessary typed construction/provenance/wire
  replay from obsolete validation or duplicated ceremony and remove the latter.
  Do not reduce this metric by deleting capability or tests.
  - 2026-07-30 exact-identity cutover commit, using the documented repository
    convention (physical Rust lines under `src`, excluding `/tests/`,
    `tests.rs`, `*_tests.rs`, `/generated/`): merge-base `1acf3641` = 438,804;
    prior HEAD `6151825d` = 373,660; this commit = 376,885 (net -61,919 versus
    the merge-base, +3,225 in this change). `rumoca-ir-dae` under the same
    convention: core 11,581 / wire 3,501 / total 15,082 — above all three
    SPEC 0036 review triggers; the operation-shaped wire item remains the
    routed reduction path. The earlier 456k/459k figures in this entry used a
    different, undocumented filter and are not comparable.
- [x] Restore the SPEC 0000 size-budget gate without deleting normative
  requirements. The offenders SPEC 0007, 0029, 0034, and 0036 were split into
  REFERENCE annexes that carry only lookup catalogs and evidence: SPEC 0040
  (stage contract rows `DAE-C01`–`DAE-C13`, `SOLVE-C01`–`SOLVE-C19`,
  `STRUCT-T01`–`STRUCT-T07`), SPEC 0041 (single-source helper, session,
  layering, and composition ownership), SPEC 0042 (GALEC traps `T1`–`T14` and
  decisions `D1`–`D11`), SPEC 0043 (DAE milestone/LOC triggers, reservation
  owners, canonical arenas, equation contracts, Solve and Flat aggregate rows,
  enforcement evidence). Each parent keeps its ACCEPTED/DRAFT status and its
  governing rule, and cites the annex row set that is normative by reference;
  no requirement was deleted. Sizes are now 0007 = 1690 w/268 l,
  0029 = 2020 w/316 l, 0034 = 1989 w/159 l, 0036 = 2036 w/278 l against the
  2500 w/350 l hard cap. REFERENCE annexes are uncapped, so the
  ACCEPTED+DRAFT count stays at 15. Routing was updated atomically in
  `AGENTS.md`, `spec/README.md`, the parent specs, and this ledger.

## Parity after the reliable core

- [ ] Replace the transitional resolved-reference root/target split with the
  SPEC 0036 stage-owned target catalogs before calling Flat correct by
  construction.
  - Resolve targets must distinguish exact declarations, owner-local enum
    literals, and typed deferred instance members. Bare `None` means failure;
    it cannot also mean "prove this later."
  - Flat references must use `FlatReferenceTarget` variants for values,
    functions, function values, binders, enum owner/ordinal pairs, intrinsics,
    and generated targets. Names remain display/protocol data.
  - Delete synthetic instance `DefId` allocation, late reference repair, and
    the unused global `symbol_ancestry` map. Dense Flat target entries own
    source-declaration ancestry and distinct sibling-instance identity.
    - 2026-07-30 exact-identity cutover: synthetic `DefId` allocation
      (`InstanceIdentitySpace`) and the write-only `symbol_ancestry` map are
      deleted; `with_appended_parts` is gone and `with_appended_field`
      requires the exact member `DefId`.
    - 2026-07-31 (commit `1db32f1e`): `postprocess_def_id.rs` late textual
      repair is deleted with its producer completed (subscripted references
      keep their qualified spelling; the repair had been silently binding
      TransformerYD element references to wrong top-level scalars);
      unique-suffix constant recovery and the untagged reference wire shape
      are deleted the same way. Canary profile unchanged (4 sim_ok,
      per-model codes identical). The open remainder of this item is
      `FlatReferenceTarget` (the A2 atomic cutover).
  - Replace `Variable::is_enumeration` and the ownerless DAE enumeration scalar
    with a checked Flat type catalog and owner-typed enumeration values. Two
    enum types with the same literal spelling must remain distinct.
  - Flat wire decoding must replay checked root construction; remove
    fieldwise `Deserialize`, the untagged reference compatibility shape, and
    any old/missing-field acceptance.
  - Remove constant and function lookup paths that convert exact IDs back to
    rendered scope strings, plus field/parts append APIs that can retain a
    stale final target.
- [ ] Make the fixed 20-model canary compile and simulate honestly before
  returning to the 566-model cohort. Keep one 10-second attempt per model and
  phase; a timeout, panic, unsupported operation, or non-finite result is a
  failure rather than a retry/default path.
  - Normative source: SPEC_0033 §6a (two-tier verification cadence). This file
    is the working ledger it requires; the fixed target list is
    `dev/msl-canary-20.json`. Tier 1 runs
    `CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 cargo xtask verify msl-parity
    --sim-targets-file dev/msl-canary-20.json` and its snapshot is `partial`,
    so no canary count may be quoted as a cohort parity number.
  - The initial post-Resolve-owner baseline was 0/20: 1 Resolve, 4 Flat, and
    15 ToDAE failures. Exact dependency ownership reduced Resolve failures
    from six to one.
  - 2026-07-30 post-cutover snapshot (lint- and matrix-green tree, commit
    `911c9199` era): 0 Resolve/Instantiate/Typecheck failures, 2 Flatten
    (AutomaticSeed EF019 inconsistent resolved function reference at a second
    site; FullAdder unresolved component dimension), 13 ToDae (ED008 x5,
    ED019 x5, ED009 x2, ED013 x1), and 5 models reach the solver and fail
    there (EL005 x3, EX002 x2: TimeBasedStep, TickBasedRamp, OpAmps.Add,
    ShowSaturatingInductor, FirstGrounded). sim_ok remains 0/20; the frontier
    is now DAE construction and Solve, no longer the frontend.
  - `Modelica.Electrical.Polyphase.Examples.TestSensors` reaches an upstream
    MSL 4.1.0 declaration that violates MLS §18.6:
    `protected Integer m=size(x, 1) annotation(Evaluate=true)` in
    `Electrical.Machines.SpacePhasors.Functions.ToSpacePhasor`. Do not weaken
    ER070 or add an MSL compatibility exception. Preserve a conformance test
    for the rejection and replace this arbitrary canary member with another
    Polyphase simulation target; the full 566-model report must retain the
    upstream-invalid classification.
  - Materialize exact-`DefId` package constants in every function parameter
    `shape_expr` before Flat freezes. `AutomaticSeed` requires
    `Xorshift128plus.nState=4`; body/default substitution alone is
    insufficient. Test two packages with the same constant leaf name.
  - Replace the separate clock-pattern recognizers with one recursive typed
    clock-plan constructor covering rational `Clock`, aliases, and derived
    clock operators. `Sample1` requires nested
    `subSample(Clock(factor), resolutionFactor)` with the exact 20/1000
    lattice.
  - Add `interval` as a typed clock-partition intrinsic with explicit runtime
    ownership. `ClockedWithDiscreteController` currently lowers it as an
    unresolved generic function call; no textual builtin-name fallback is
    acceptable.
    - 2026-07-30: `interval()` is implemented as `Coordinate::ClockInterval`
      end to end (wire v12, structural replay, Solve lowering, evaluation,
      GALEC, Modelica target), and the recursive typed clock-plan constructor
      over exact instances replaced the separate pattern recognizers. Canary
      evidence for `Sample1` and `ClockedWithDiscreteController` is still
      pending a post-cutover run and both bullets stay open until it exists.
  - Implement exact-identity closure conversion for partial function values:
    target `FunctionInstanceId`, declaration-ordered captured slots, and a
    checked residual signature. `QuadratureLobatto3` captures `A` and `w` in
    `fun7` while retaining `u`; treating that partial application as an
    executable under-applied call is invalid.
- [ ] Re-establish the MSL baseline using only checked end-to-end paths.
  - This is SPEC_0033 §6a Tier 2: the full 566-model sweep runs per milestone
    or as nightly CI shards, and is the only source of cohort parity claims.
    Every number recorded below names the run's commit.
  - 2026-07-30 `verify full` sweep (commit `090d45ae`): first nonzero honest
    post-cutover parity — 107/566 compile, 106/106 balanced, 99 attempted,
    **33 sim_ok**, 66 sim_solver_fail, 0 NaN / 0 timeout / 0 balance-fail;
    ic 35/36. Canary subset 4/20 sim_ok (TimeBasedPulse, TimeBasedStep,
    TickBasedRamp, ShowSaturatingInductor), 4 sim_solver_fail, 12 ToDae
    (ED013 x5 initial algorithm, ED019 x4, ED008 x3). Cohort failure mass:
    ToDae 269, resolve 66, sim-solver 66, typecheck 62, flatten 53,
    instantiate 9. The quality-gate ratchet fails loudly against the
    pre-cutover baseline as intended; do not touch the baseline until the
    checked paths recover the cohort.
- [ ] Reach at least 340 models in OMC trace-parity high agreement (or
  carrying an MLS-cited divergence adjudication), over the deterministic
  cohort. `sim_ok` is completion, never the score (James, 2026-07-31);
  the metric is the comparator's strict-high band.
  - 2026-07-31 Tier-2 certification, landed tree (results-landed,
    commits 7c8c5c1d..bf9de519, comparator un-skippable): **strict-high
    54/566**, minor 6, deviation 5, 65 compared. CORRECTED by post-hoc
    review: the delta vs 38 is **+17 gained, -1 LOST** — DCPM_Start
    regressed strict-high -> sim_solver_fail and silently left the
    compared set (unattributed; task #68), and Sample3/SignalGenerator
    (the prior real findings) also left comparison. The cohort is not
    pinned per-model; comparisons must diff band tables between runs.
  - 2026-07-31 reference-growth correction: coverage was NEVER the
    comparator's limit — models_compared is structurally bounded by
    sim_ok (agreement_high <= models_compared <= sim_ok; the earlier
    "202" figure matches no artifact, the pre-cutover comparator figure
    is 166 of 170 sim_ok). References now staged for 524/566 (four are
    OMC-unwinnable: FluxTubes actuator/solenoid components where OMC
    itself errors). The score moves ONLY by making models simulate
    correctly: the winnable set is 458 models where OMC succeeds and
    rumoca does not yet simulate (Electrical 183, Mechanics 67,
    Magnetic 66, Clocked 44, Blocks 28, Media 23, Fluid 23). UNTIL
    task #71 lands, every Tier-2 sweep MUST pass --all-omc-targets or
    the reference cache is silently erased.
    initial-conditions accurate on 96.92% of compared. Compile ceiling
    107 -> 136. Reference coverage is now the comparator's limit
    (65 compared vs 202 pre-cutover baseline) — grow the omc_parity_cache
    alongside each newly-simulating cohort. Ratchet floor to clear
    next: 85 strict-high (15%).
  - 2026-07-31 capability wave 3 (commits `a28d97b1`, `b6de8e20`,
    `836cec9a`, `cdff8c39`): initial algorithms as declarative owners
    (cohort ED013 75 -> 39), parameter-coefficient affine derivatives
    (cohort SOLVE_OK 50 -> 62, zero regressions, affine/escaped EL005
    extinct), modifier-binding lexical occurrence pairing, rayon pools
    in the derived budget. Harness-certified canary: **6/20 sim_ok**
    including Sample1 and ClockedWithDiscreteController for the first
    time.
  - 2026-07-31 Tier-2 certification (full 566 sweep, results-wave3):
    **sim_ok 49/566** (was 33), sim_solver_fail 66 -> 50, ic 56/59,
    0 NaN / 0 timeout / 0 balance-fail, 106/106 balanced, 49 traces
    written. Compiled stays 107 — the ED013 gains land at other typed
    ToDae frontiers (ED008 array-connectors x18, ED018 semiLinear x12,
    ED019 x4, ED010 x2), which with isValidTable (18+3), the T_start
    DiscreteReal family (18), and the remaining 45 ES010s form the
    ranked target list. The quality ratchet fails loudly against the
    pre-cutover baseline as designed.
  - 2026-07-31 OMC comparator certification (results-wave3-omcref, repo
    tooling, fresh 120s references, 48 compared + 1 stochastic exclusion):
    **high 38 / minor 6 / deviation 4 — strict high agreement 38/49
    (77.6%)**; the quotable parity number is 38/566. Real findings:
    SignalGenerator is sim_ok with a DEAD CIRCUIT (47/66 channels
    identically zero; LCOscillator shares the signature at 1e-6 scale);
    Sample3's clocked feedback freezes after one update (and the
    comparator's step-hold shape label masks such cases — classifier
    fix filed); CharacteristicIdealDiodes carries a sustained -10000A
    ideal-switch current; five clocked models share a systematic
    one-tick sampling lead (first-tick semantics decision needed);
    ChuaCircuit long-horizon divergence needs chaos-aware adjudication.
    sim_ok alone is hereby demonstrated insufficient — the SPEC 0033
    comparator rule (38464fd8) exists for exactly this reason.
  - 2026-07-31 index-reduction wave (commits `03497a7d`, `26148370`):
    equality-closure candidate detection and connection-joined clocked
    ownership. Cohort SOLVE_OK 43 -> 50 against a control binary with zero
    regressions; ES010 54 -> 45; ES014 extinct; canary 6/20 sim_ok. New
    typed frontiers exposed: EL005 affine-product x3 (DCPM family),
    escaped-derivative x5. The A2 FlatReferenceTarget plan is validated
    against the current tree at dev/2026-07-31-a2-flat-reference-target-plan.md
    with 11 checklist amendments pending.
  - 2026-07-31 affine-derivative frontier (uncommitted, `rumoca-phase-solve`
    only): both EL005 families the wave exposed are extinct across the 566
    cohort. Cohort SOLVE_OK 50 -> 62 with zero regressions (no model lost
    SOLVE_OK); the twelve gains are exactly `DCPM_{Start,Temperature,
    CurrentControlled}`, `{Series,Parallel}Resonance`,
    `OpAmps.{LCOscillator,Multivibrator}`, `Heating{Rectifier,MOSInverter}`,
    `Spice3.CoupledInductors`, `Rotational.EddyCurrentBrake`, and
    `HeatTransfer.TwoMasses`. Canary unchanged at 6/20 sim_ok (the canary set
    contains none of the twelve). Trace-checked against OMC 4.1.0 on three of
    them: `Rotational.EddyCurrentBrake` (worst scaled deviation 8.0e-3 on a
    channel decaying to zero, 1e-5 on `heatCapacitor.T`), `DCPM_Start`
    (1.4e-4), and `SeriesResonance` (3.2e-4).
    `Translational.EddyCurrentBrake` stays structurally singular — an
    index-reduction frontier, not an EL005 one — and `HeatingRectifier` now
    lowers but its diode loop fails the algebraic projection at an event
    boundary, which is the next honest frontier for that model.
  - 2026-07-31 evening review cycle (all work uncommitted, landing batch
    forming). (a) #44/#45 initialization-ordering review verdict: the §8.6
    pre() snapshot move is correctly ordered at all four driver sites (event
    iteration is a capped fixed point with per-pass iter_pre refresh, not
    snapshot-once), and the four regression tests pin MLS/algebra-derived
    values with no over-pinning. CLAIM WITHDRAWN: "canary trace-neutral 6/6
    high" was vacuous — `target/msl/task4445-after` compared 0 models (rumoca
    trace dir empty); no artifact covers the snapshot move. The update rows
    are neutral by construction for DCPowerSupply (every fixed=false
    parameter there is bound, so no rows fire). Findings filed as task #74
    (fix in flight): Gauss-Seidel re-application rejects well-posed
    `g = 2*q; initial equation g + q = 30` (loop gain >= 1 diverges — must
    substitute the binding into the residual); `when initial()` rows land
    AFTER the snapshot (spurious `a>1` edge at t=0; `pre(a)+1` yields 3 —
    needs OMC/MLS adjudication); String dependents wrongly rejected instead
    of filtered; `{error:?}` span leak; contract-as-unsupported wrapping.
  - 2026-07-31 #65 aliased fixed=true pins (uncommitted): initial-value
    closure over signed+displaced equalities in phase-structural transfers
    pins across alias/displacement chains (MSL spring-mass `s_rel(start=1,
    fixed=true)` now yields OMC's values); new ES013
    ConflictingStatedInitialValues matches OMC's error on conflicting pins.
    Full-cohort comparator run (`task65-after`, --all-omc-targets,
    parity_measured): bands BYTE-IDENTICAL to the bf9de519 baseline —
    strict-high 54, near 6, deviation 5, 65 compared — neutrality is honest
    (the 3 sim_ok models gaining rows write values their states already
    had). OPEN CONTRADICTION under re-verification: the #44/#45 reviewer saw
    `initial equation x = 5` on a state fail EX001 projection and the
    committed homotopy_branch_selection test red, attributed to the pins
    window, while the pins agent's final run reports 469/469 green —
    probably a mid-edit probe at 15:29; the pins agent is re-checking the
    final tree. NOTHING LANDS until that is resolved. Follow-up task #75:
    pin transfer on demoted StateSelect.prefer states must wait for #67;
    `--inspect eval` under-reports settled values.
  - 2026-07-31 #22 external purity default (uncommitted): MLS 3.6 §12.3 has
    two independent halves — bare externals are IMPURE (deprecated
    semantics) but WITHOUT call restriction; restriction attaches to the
    written `impure` prefix only. `Function::body_is_pure()` is the one
    owner; restriction checks still key on the written prefix (acceptance
    preserved). The §-citation was wrong repo-wide (§12.9 -> §12.3) and the
    legal-context list was incomplete (missing `pure(...)`, parameter
    bindings, external-object bindings) — corrected in resolve, phase-dae,
    SPEC_0022 FUNC-022/FUNC-032. MSL blast radius none: 9 bare externals in
    MSL 4.1.0, every cohort model reaching them already fails ED019 before
    the external plan; canary phase profile 20/20 identical to bf9de519.
    Adversarial review in flight. Gaps filed as #76: impurity needs a
    call-graph closure (a Modelica wrapper of an impure external reads
    pure at equations.rs:404); `pure(impure(...))` wrapper still falsely
    rejected; IllegalImpureCallContext dead variant with stale list.
  - 2026-07-31 pins contradiction RESOLVED (exoneration with evidence): the
    committed homotopy_branch_selection test passes 12/12 in the final tree;
    the `initial equation x = 5` EX001 reproduces byte-identically on a
    pristine worktree at clean HEAD bf9de519 (residual value -5.0 = the row
    at the start value); the #44/#45 reviewer's -3.75e-1 was x^3 - x at
    x = 0.5, a homotopy-continuation intermediate — a mid-edit or misread
    probe, not the final tree. The pins code structurally cannot cause it:
    pin extraction requires fixed == Some(true) (both repros are
    fixed=false), emits zero update rows for both models, and only ever
    appends. REAL FINDING extracted as task #77 (HIGH, pre-existing at
    HEAD): an initial equation reading a STATE has no checked plan — only
    rows whose sole unknowns are fixed=false parameters are planned
    (lower.rs, c15de628, documented); everything else dies as typed EX001
    unless start values happen to satisfy it. The full §8.6 initialization
    system (states + discretes as projection unknowns) is likely a large
    share of the 458 winnable models. Independent adversarial review of
    #65 launched per policy (self-re-check is not review). NEW LANDING
    BLOCKER: crates/xtask/src/verify_cmd.rs at 2065 lines crossed the
    SPEC_0021 hard threshold 2000 (1982 at HEAD) from uncommitted band-table
    CI wiring — suite_gates code_size_budget_test + architecture hard_limit
    both red; the band-table agent has been told to extract a submodule,
    alongside its "legacy" banned-word rename.
  - 2026-07-31 #22 purity review verdict: REJECT WITH FINDINGS (fix cycle
    in flight). Confirmed correct: bare external -> Impure mapping, written-
    prefix restriction, truth table, zero cohort movement structurally
    guaranteed (no FunctionPurity consumer alters control flow; the "9 bare
    externals" count corrected to 6 in cohort scope, all ExternalObject
    ctor/dtor pairs in Blocks/Types.mo). Findings: (1) HIGH edition
    conflict — SPEC_0022 declares MLS 3.7 as source; 3.7 §12.3 removes
    "without any restriction on calling them" and makes wrapper propagation
    normative+recursive (pure(...) is the sanctioned escape); decision:
    align rows+doctrine to 3.7, keep acceptance (deprecated = diagnostic).
    (2) MED the §12.3 "a diagnostic must be given" never reaches `rumoca
    compile`/`lint` (WR001 is Session/LSP-only; OMC equally silent, so no
    parity gap — but fail-loud demands it). (3) MED messages advertise
    pure(...) which rumoca rejects (parse lowers it to a call to a
    component named "pure": ER088/ER002; OMC accepts both probes). (4-7)
    LOW: serde-default purity_declared flips old flat payloads (latent, no
    in-tree decoder); reject_unsettled_reads keys on written prefix not
    body_is_pure (bare_disc_init dies as generic ED020 not owned ED013);
    purity_declared missing from the incremental interface fingerprint
    (stale reuse in session/LSP); constant-binding rationale comment claims
    translation-time evaluation that does not happen. #76 RAISED TO
    REQUIRED: under 3.7 the call-graph impurity closure is definitional;
    real exposure Modelica.Math.Random.Utilities.initializeImpureRandom
    reads as pure today. Implementer resumed with the fix mandate.
  - 2026-07-31 #65 pins review verdict: REJECT WITH FINDINGS (fix cycle in
    flight). Verified sound: closure algebra (sign/offset composition,
    termination), no double-write, no unsound transfer on any adversarial
    probe (anchor-free / discrete / input / conditional displacements all
    skip rather than mis-transfer), no wire exposure, comparator neutrality
    REAL (task65-after read directly: 65 compared 54/6/5; zero non-timing
    diffs across 566). Findings: F1 HIGH — ES013 false-positives: conflict
    detection folds only literal terms, so consistent parameter-displaced
    pin pairs hard-reject (the task's own spring/mass with m1.s(start=1.25,
    fixed=true) — the exact value its test asserts — EL005s while OMC
    simulates; boundary is literal-vs-parameter, not consistent-vs-
    inconsistent). F2 MED-HIGH — SILENT WRONG ANSWER by spelling: leaf cap
    is checked per recursive entry, so `f1+f2+f3+f4 = 0` reads s_rel(0)=6
    (OMC 7) while `0 = f1+f2+f3+f4` is correct; four-terminal nodes are
    the standard MSL connector shape. F3 MED — zero-state classes skip the
    conflict check (single-state gate runs first): silent 0/0 where OMC
    hard-errors alias conflict. F4 doc/loudness on the one-state drop; F6
    cap-widening claim off by one (3->4 not 2->3). Also flagged: two more
    vacuous comparison artifacts (task65-canary 0 compared;
    task65-canary-parity missing the JSON) — never cite; and artifact dirs
    carry no commit stamp, so binary provenance for after-runs is
    unverifiable — positive control deferred to the fresh landing
    certification. Implementer resumed with the mandate; required
    regressions: P16 consistent pair, P13/P14/P15 re-association trio,
    P9 zero-state conflict.
  - 2026-07-31 #74 fix cycle complete (uncommitted, under review). (A)
    FIXED: calculated-parameter solving rewritten from Gauss-Seidel
    iteration to binding SUBSTITUTION into the projection residual
    (ScalarCompiler::with_parameter_substitutions), one owner per
    coordinate (bound fixed=false parameters excluded from projection
    unknowns per §8.6 "solved from the binding equation"), and the planner
    upgraded to maximum bipartite matching with surplus rows as
    consistency checks. OMC-verified: g=2*q with g+q=30 was EX001
    non-convergence, now der(x)=10 exact; the gain-0.5 case now exact
    (was 60.0000272); genuinely over-specified models still fail loudly
    (OMC also rejects). (C) FIXED: String dependents filtered not
    rejected; {error:?} span leak and contract-as-unsupported wrapping
    both fixed. (D) FIXED: clippy debt. SPEC_0021 side-effect handled:
    planner moved out of lower.rs (1814/715). (B) ADJUDICATED AS RUMOCA
    DEFECT, folded into task #44 with OMC+§8.6 evidence (OMC: a=5,b=0 and
    a=1; rumoca: b=1 spurious, a=3): when initial() rows run in the event
    machinery after the snapshot; fix needs branch-level ownership
    splitting in events.rs guarded_assignments_program — the phase-dae
    initial-algorithm declarative-owner lane owns it. #74 canary died on
    a co-tenant build break (band-table agent mid-edit in rumoca-test-msl)
    — canary delta OUTSTANDING, covered by the landing certification.
    689/689 focused tests green. Tooling correction recorded: `cargo
    xtask msl` does not exist — canary is `verify msl-parity
    --sim-targets-file dev/msl-canary-20.json`. Adversarial review of the
    fix cycle launched (substitution soundness, Jacobian through
    bindings, matching correctness, one-owner interactions with the #65
    pin lane).
  - 2026-07-31 #73 band-table repair complete (uncommitted, under
    verification review). All 15 findings + 2 additions worked: rotation
    keyed on run identity (blake3 of the comparator outputs — re-persist
    rewrites in place, real diff still entered 20 / left 3); tables bound
    to their directory's comparator digest (planted tables ->
    not_comparable with reason); cohort pinned — rows for all 566 from
    sim_target_models (566 = 48 compared + 1 excluded + 467 not_attempted
    + 50 sim_failed, reconciles exactly on both real dirs); typed skip
    attribution {policy_excluded, comparator_failed, rumoca_trace_missing,
    omc_trace_missing} so comparator crashes can never be filed as policy;
    exclusions file gained per-entry reasons (absorbs task #72) and is
    wired into the previously dead param; vacuous comparisons
    (models_compared==0 or missing JSON) are loud typed rejections —
    task4445-after / task65-canary / task65-canary-parity all now reject
    with reasons while results-landed passes; artifacts stamped with
    commit + dirty flag; partial runs cannot displace full-cohort tables;
    provenance from the certification's git_commit; crash-atomic writes;
    --clean-results preserves tables; departed-strict-high gate rule +
    SPEC_0033 rows. Cross-agent blockers cleared: banned word purged,
    verify_cmd.rs 2065 -> 1917 (extraction). suite_gates + architecture
    138/138; rumoca-test-msl + rumoca-sim 231/231. Three feature-suite
    failures attributed to in-flight co-tenants (phase-flatten
    zero-cardinality, phase-solve implicit-Jacobian, one environmental) —
    re-check at the landing gate. DCPM_Start / Sample3 / SignalGenerator
    departures now carry solver error detail in the diff.
  - 2026-07-31 #69 review verdict: ACCEPT WITH FINDINGS — first acceptance
    of the landing batch. Skew absorption proven MLS-exact by hand
    derivation (all four value/skew combos emit byte-identical §3.7.4.5
    Rule 1 output; OMC cross-check agrees to <=1.8e-15 everywhere except
    the x==0 point §3.7.4.5 leaves free — where OMC itself is
    operand-order sensitive and rumoca is invariant). Strict additivity
    PROVEN from the old per-member predicate, and causal attribution
    established by a same-tree old-gate binary: exactly 5 TwoMass channels
    move (+5 high / -5 dev / -4 severe), old gate emitted enthalpy 0.0 K
    absolute at zero flow where the fix gives 293.15 K. Evidence claim
    refuted: ParallelPumpDropOut is NOT bit-identical — a second skew=1
    chain also unlocked (5 channels, max delta 2.09e-3, corroborated by
    the artifact's own mean-L1 7th-decimal move). Three doc corrections
    in flight (bit-identical wording; the x==0 "confined" sentence must
    note zero-flow windows hold that value macroscopically; junction
    section re-scoped to TwoMass's identical topology). New tasks: #78
    (the anti-equal-slope check at :388 repeats the root-choice flaw the
    fix removed — spelling-dependent transformation, frozen zero-flow
    channel, OMC invariant; pre-existing, MSL-unreachable, old-gate
    binary preserved) and #79 (dae-mo emitter broken: tojson filter
    feature + whitespace-eating else). Determinism verified (6 repeat
    compiles identical; HashMap use order-safe). Reviewer correction
    adopted: determinism authority is SPEC_0021:165-179 + SPEC_0031:57,
    not SPEC 0036.
  - 2026-07-31 #74 review verdict: core CONFIRMED, REJECT only at the #65
    seam (both blockers routed to the in-flight pin-lane fix cycle). All
    targeted mechanisms survived adversarial probing OMC-confirmed:
    substitution chains to depth 4 exact, nonlinear Jacobian through
    substituted bindings converges to OMC's root (q^4+q=30 to 6 digits),
    the Kuhn matching cannot miss a perfect matching (Berge), surplus
    consistency rows verified both consistent and inconsistent, 5 repeat
    compiles byte-identical, the code move drift-free, all three new
    tests' OMC doc values independently re-derived, and the #44 repro
    records re-confirmed accurate. Blockers (both false compile-time
    rejections of OMC-clean models, both in the pin lane's files): S1 —
    pin Check rows compile WITHOUT parameter substitutions, so they read
    the stale seed and acceptance depends on the declared start guess
    (start=5 fails, start=1 passes, same model); S2 — pin check rows are
    appended with no parameter incidence, so a pin-lane row can never
    determine a fixed=false parameter (unsatisfiable by construction; OMC
    q=2). Accepted risks to be RECORDED in module docs (routed with the
    seam fixes): under-determined components fail loud-but-untyped
    naming a row not the unowned parameters; the matching is rank-blind
    (can pick a numerically singular square block, p16 repro); projection
    accuracy tracks solver tolerance (~1e-7 relative on fixed=false
    parameters — parity bands must budget it); the §8.6 recommended
    both-binding-and-fixed=false diagnostic deliberately not emitted.
    New task #80: `time` in a parameter binding escapes the variability
    check (EX002 at runtime where OMC rejects at compile) + HashSet-
    ordered diagnostic name. Note: the "689/689" figure was a wider crate
    selection; the reviewer's -p phase-solve -p rumoca count is 511/511 —
    both green, reconciled.
  - 2026-07-31 #73 review verdict: REJECT WITH FINDINGS, round-2 repair in
    flight. The four original HIGH repairs held under direct attack:
    rotation byte-identical on re-persist with truthful A->B->A history;
    planted/tampered tables refused with both digests named; cohort
    reconciles exactly on both real dirs; a comparator crash on a
    tracked-excluded model files comparator_failed (typed record wins);
    crash atomicity clean; continue-on-error cannot swallow the new
    checks. Second layer found (this artifact pins certifications):
    H1 — tracked_exclusions() swallows the loud reader's errors
    (unwrap_or_default) and resolves the exclusions file from CWD, so
    policy-vs-defect attribution flips on ambient state over the untyped
    legacy skipped strings both real dirs carry; H2 — ensure_comparable
    never enforces one-row-per-cohort-target (SPEC_0033:163 says it
    does): a dir without msl_results.json yields the pre-repair 48-row
    shape, fabricates policy-absence rows for exclusions the run never
    considered, and accepts git_commit=""; M1 — the digest binds inputs
    not contents, so a row-edited table reaches transition-diff (the
    quoted tool; the gate itself re-derives and is safe); M2 — declared
    counts never re-validated against rows and consumers mix the two
    sources; M3 — the departed-strict-high rule is INERT on every CI
    lane (persisted.previous is always None there: shards never write a
    band table and nothing restores the prior certification's); M4 —
    full-over-partial rotation unguarded. Plus L5-L12 (dead recovery
    path with a hand-constructed test blessing it, residual string-sniff,
    lost clean_results seam test, weak source-text pin, two SPEC_0033
    citation errors, missing spec row for a rule that can fail the gate,
    summary_line lacks the skipped/missing counts SPEC_0033:130 demands,
    three doc overstatements). All routed back to the implementer with
    fix directions; gates green baseline re-confirmed by the reviewer
    (138/138 gates, 165+344 test-msl, foreign failures re-attributed).
  - 2026-07-31 #22 fix cycle complete (uncommitted, verification review in
    flight). All seven findings addressed: repo retargeted to its declared
    MLS 3.7 edition (treated-as-impure list; "without any restriction" and
    the diagnostic mandate are 3.6-only, cited as historical); the
    diagnostic now REACHES USERS — `rumoca compile` and `lint` print
    warning[WR001] with spans (new report_compile_warnings + an
    external-purity-undeclared lint rule); pure() is a real intrinsic
    (typed, arity-checked ET008, erased in ast_lower — pure(pureCall)
    compiles; pure(impureCall) still refused at its earliest owner with
    the call-site-marker surgery filed into #57, advertised lists
    corrected everywhere); purity_declared is a REQUIRED Flat wire field
    (old bytes fail loudly per the wire's own precedent — the serde
    default silent-flip is gone); reject_unsettled_reads keys on
    body_is_pure() (bare external in discrete init now gets the owned
    ED013); purity_declared joined the incremental interface fingerprint
    (three distinct fingerprints pinned); constant-binding comment now
    states the truth. 3784/3784 over 20 crates; canary phase profile
    20/20 identical to bf9de519. NEW DECISION FILED (#81, James): MSL
    compiles now print WR001/WR005/WR006 because MSL itself ships bare
    ExternalObject constructors — recommend a library-origin filter with
    a count summary; needs a call. Probe-debris flag: #64's scratch
    example in crates/rumoca/examples/ ordered out of the tree.
  - 2026-07-31 #51/#52 connection fixes complete (uncommitted, review in
    flight). #51 CONFIRMED and fixed: two sites counted a subscripted
    endpoint as 1 scalar (resolve_var_scalar_count and the equality
    path) while CONN-008 validation used correct dims — slice connects
    on Real[2,3] connectors false-rejected ED001 balance -4 where OMC
    checks 14=14 and simulates; now one shared connection_endpoint_dims
    (MLS §10.5: subscripts consume leading dimensions) feeds validation,
    equality, flow, and preferred_dims, so counting cannot drift; sim
    values match OMC exactly. #52 was a SILENT SUBSCRIPT DROP (connect
    (a[1], b) on dimension-less a connected the whole component,
    surfacing as misleading unbalance; OMC rejects) — now typed EF026
    with connect-site + declaration-site labels, checked BEFORE path
    matching (which is where the subscript was being normalized away),
    acceptance contract written first per SPEC 0008. Evidence: OMC
    cross-checks both ways; 1182/1182 flatten+contracts; suite_core
    connection members 7/7; hand-compared full 566 sweep vs bf9de519:
    563 identical, 3 methodological deltas, ZERO EF026 firings in MSL
    (no false rejections introduced). New tasks from its findings: #82
    (typecheck ET009 as long-term owner + the rank>=1 over-subscript
    silent-drop remainder), #83 (MED-HIGH: stream-vs-non-stream connect
    member pair yields NO equation and NO error per §15.1 — silent
    under-constraint, the worst class; plus parameter/constant member
    silent skip and the embedded-index count-1 branch), #84 (CLI drops
    secondary labels/notes/help — EF026's declaration label invisible on
    CLI, present for LSP/API). Adversarial review launched (expandable
    connectors §9.1.3, for-equation connects, superclass declarations,
    drift-proofing grep, sweep spot-checks).
  - 2026-07-31 #22 verification verdict: ACCEPT WITH FINDINGS — all seven
    fixes semantically correct and pinned (MLS 3.7/3.6 texts fetched and
    diffed verbatim; probes confirm WR001-with-span + exit 0, pure()
    erasure span-preserving with ED013 landing on the wrapped call, wire
    field genuinely required with the old-bytes decode failing on
    `missing field`, fingerprint unpersisted + caches salted by compiler
    source fingerprint so no poisoning; 1754/1754). Final fix round in
    flight, all in the new reporting surface: printed warning locations
    are 0-based in a 1-based convention (every warning off by one; the
    regression only grepped "WR001" so it could not catch it — location
    assertion added to the mandate); report_compile_warnings re-runs the
    whole pipeline on cold caches (flat-only emits now cost a DAE
    compile — cheap fix attempted, else filed with measurements); two
    test doc comments still quote 3.6-only sentences as current §12.3.
    New task #85: the target-file warning filter collapses when any
    sibling file carries a resolve error (foreign warnings printed;
    pre-existing, newly visible). Also verified: NO unauthorized commits
    — a reviewer note about "HEAD moved" was a misread of the earlier
    13:18 landing batch; HEAD remains bf9de519 and every work-set is
    uncommitted as intended.
  - 2026-07-31 #73 round-2 complete (uncommitted, focused verification in
    flight). Every round-1 finding addressed with the attack probe
    re-reproduced: exclusions propagate loudly and the table records
    which list attributed it (H1); tables require the roster, rows ==
    cohort, and a non-empty commit — the fabricated-absences path is
    closed and the blessing test rebuilt as a refusal test (H2); a
    rows_digest over canonical fixed-precision rendering catches
    band/reason/count/metric edits while surviving the JSON float
    round-trip that briefly made every table fail its own check — a
    real defect found inside the fix (M1); counts recomputed and mixed
    reads eliminated (M2); the departed-strict-high rule now FAILS a
    full run with no predecessor and CI/nightly download the prior
    certification's table before gating, spec row added (M3); both
    rotation directions guarded with run_scope_mismatch (M4); all eight
    ride-alongs done incl. typed NoComparableSamples replacing the last
    string-sniff. Real diff intact: entered 20 / left 3, cohort
    566→566. Count discrepancies reconciled (invocation differences,
    now 175 + 361). NEW LANDING-GATE TASK #86: two persistent
    msl-full-test failures are foreign to every agent that hit them and
    must be root-caused before commit — zero-cardinality Flat coordinate
    (phase-flatten) and "implicit ODE requires a derived implicit
    Jacobian pattern" (phase-solve sim-worker path; suspects: #74
    Jacobian rework or #65 solve edits, though their crate suites are
    green).
  - 2026-07-31 evening: #51/#52 review split verdict — #51 counting fix
    CORRECT and OMC-exact on every probe (keep); #52 EF026 has one HIGH
    false positive (fires on MLS §7.3-legal redeclared array dimensions
    because rumoca's own flatten drops the redeclare's dims — a compiler
    gap converted into a user-blaming error; fix: reject only when rank
    is authoritative) plus contract over-claims (end/colon subscripts
    are erased upstream and composite-array slices still drop — narrowed
    to literal-integer-on-primitive, remainder filed as #87) and the F3
    discovery that a dropped connect emits a DUPLICATE flow-zero row
    that SATISFIES the ED001 balance gate (#82 severity raised: the
    balance gate is not the guard for dropped connects). Fix round in
    flight. #65 fix cycle reported complete with all F/S findings fixed
    (three-valued consistency for ES013, cap removed from
    flatten_additive with per-reader counts, conflict check before the
    state gate, substitutions threaded into both pin roles, typed
    InitialRowIncidence giving pin rows parameter incidence) and a
    task65-final full-cohort run claiming bands unchanged at 54
    strict-high with channel accuracy improved (bad 124->118, severe
    32->27, localized to FluidHeatFlow = the #69 fix in the same tree).
    HOWEVER: DCPM_Start FAILS RIGHT NOW in the current tree (probed
    17:47, fixed=true guard: "demoting dcpm.phiMechanical leaves no
    equation that states it") while baseline is Success — the agent's
    zero-changes claim cannot describe the tree that produced this;
    resumed with mandatory attribution (stale comparator vs own F2
    equalities rework vs a co-tenant hunk) — NOTHING LANDS until
    DCPM_Start is green or the culprit hunk is named. Second gate break
    routed: RUMOCA_MEASURE_SKIP_WARNINGS (new in compiler.rs) violates
    the zero-env-var policy — #22 told to replace with a flag.
  - 2026-07-31 two work-sets CLOSED. #73 band-table: round-2 verification
    verdict CERTIFICATION-READY — every round-1 finding re-verified by
    live attack probes (foreign-cwd hard-error, roster enforcement
    before digest, six-field digest blind-spot sweep all caught, CI
    predecessor restore before gating with no continue-on-error, both
    rotation directions refused with named scopes), real wave3->landed
    evidence unchanged (566->566, entered 20 / left 3, departures
    sim_failed: Sample3, SignalGenerator, DCPM_Start); four low
    non-blocking polish items filed as #88; #72 absorbed and closed.
    #22 purity: final round done and workstream CLOSED — printed
    locations one-based with an exact-location assertion that fails
    without the fix; the double-pipeline cost measured honestly (+0.64s
    plain / +0.52s flat / +2.10s dae-emit) with both cheap fixes
    rejected on evidence and the session-plumbing fix filed as #89; the
    env-var was a pre-removed temporary guard (gates re-verified
    138/138); edition attributions corrected; pure(1+2) transparency
    pinned. #58 closed (replay-as-input verified + pinned; dead-variant
    remainder lives in #76). LANDING CRITICAL PATH: #86 investigator
    launched (head-worktree bisect protocol) for the two foreign
    failures — flatten zero-cardinality coordinate and solve-IR
    implicit-Jacobian-pattern in the sim-worker path; #65 DCPM_Start
    attribution still in flight; #51/#52 EF026 fix round in flight; #64
    in flight. Landing batch status: CLOSED #69 #22 #73(+#72,#58);
    PENDING-FIX #51/#52, #65-seam; CONFIRMED-CORE #74 (rides #65 seam);
    BLOCKED-ON #86 + DCPM attribution.
  - 2026-07-31 #64 strict time crossings complete (uncommitted, review in
    flight). Root cause: `when time > 0.5` lowered as a zero-crossing
    ROOT, and runtime root application snaps to the target when the
    crossing coincides with a sample — the event applied AT the instant
    where the strict relation reads false, crossing consumed, activation
    never true (`>=` survived by accident; compound `and` forms never
    fired on either solver). Fix: when-chain branch conditions and
    algorithm when blocks now contribute scheduled TimeEvent plans, and
    the condition lowering emits no root for a leaf the plan owns; §8.5
    vector activation `when {initial(), time > 0.5}` went from compile
    error to OMC-matching. OMC falsification table across 8 shapes, all
    matching at the instant (OMC makes no strict/non-strict distinction
    there per §8.5's buffered-relation rule — verified 0,0,1 rows both
    ways). Self-rescheduling `time >= pre(y)` boundary preserved (the
    Pulse/Trapezoid counter idiom keeps its crossing, pinned). Canary
    identical 8/12/6 with honest "parity unmeasured" statement; the one
    suspicious code drift A/B-attributed to co-tenant churn by
    self-revert. TWO NEW DIVERGENCES found and filed, not fixed: #90
    (MED-HIGH, every when whose condition is true at t=0 fires at the
    initial event — §8.5 demands initial()-only; interacts with #44) and
    #91 (MED, elsewhen priority is level-based where §8.3.5 wants the
    edge — y stuck at 1 forever where OMC reaches 2). Corroboration
    routed to the #65 lane: 9 red tests in initial_value_alias_transfer
    in the current tree added to its closure criteria.
  - 2026-07-31 DCPM_Start FALSE ALARM, fully resolved with clean-worktree
    evidence: the E011 refusal is byte-identical at clean HEAD bf9de519 —
    pre-existing #67 over-refusal class, NOT a batch regression. The
    apparent regression was a category error: results-landed records
    DCPM_Start as phase_reached=Success AND sim_status=sim_solver_fail
    (the structural lane refuses after the compile phase; solve-json
    surfaces the refusal). Bisect evidence: pre-F2 cap restored -> red;
    pin lane disabled entirely -> red; clean HEAD -> red, identical
    bytes. The 18:00 nine red umbrella tests were the agent's OWN bisect
    experiment live in the shared tree 17:45-17:53 (disclosed, restored,
    230/230 green now) — process note: in/out bisects in the shared tree
    are visible to every co-tenant and must be announced. task65-final
    NOT stale (last edit 17:14, run started 17:18:44, finished 17:42;
    zero phase/sim changes across 566; five of the twelve affine-wave
    models Success/sim_solver_fail in BOTH runs, two sim_ok in both,
    five absent from the root-examples cohort in both). LANDING
    UNBLOCKED on this item; #67 is now the top-value next fix (it
    gates DCPM_Start's return). #65 fix-cycle verification launched
    (three-valued agreement redesign, cap-deletion identity argument,
    InitialRowIncidence). Also: one of the #65 agent's cohort attempts
    lost its results dir to my disk cleanup mid-run — future sweeps:
    verify no active run owns a results dir before deleting (lsof came
    back clean only because the race had already completed). PROBE
    eprintln found in the #51/#52 lane's mod.rs routed for removal.
  - 2026-07-31 #86 investigation: both msl-full-test failures PRE-EXISTING
    at clean HEAD bf9de519 (pristine-worktree reproduction, byte-
    identical) — downgraded from landing blockers; fixes launched.
    (1) zero-cardinality: a live contradiction between two committed
    tests — 39558ff1 deliberately retains zero-sized primitive arrays
    (its own worker test pins retention and passes) while the older
    balance test written before that fix still asserts erasure; both
    landed the same day, the balance test gated behind msl-full-test so
    nobody saw the clash. Decision: retention is correct; the stale test
    is being updated with an explicit count-semantics decision.
    (2) implicit-Jacobian: check_initialization_inner lacks the
    can_use_state_only_bdf branch build_simulation_inner has, so pure
    explicit ODEs (empty implicit_rhs by design) fail the mandatory
    pattern derivation — fix is the symmetric branch with an equivalent
    (not weaker) initialization check; blast radius ~zero in MSL (error
    string absent from every stored cohort). Worktree registry pruned
    (dozens of stale entries); scratch_t64 confirmed gone.
  - 2026-07-31 #51/#52 fix round complete (uncommitted, focused
    verification in flight). F1 fixed with a clean discriminator:
    declared_rank_is_authoritative — a visible rank is evidence about
    the source only when the declaration's source scope equals the
    instantiating class's (no extends modification could intervene);
    non-authoritative ranks abstain to path matching. Root cause traced
    DEEPER than the review: collect_redeclarations (phase-instantiate)
    discards redeclare array subscripts at the first step, so the dims
    exist nowhere downstream — and typecheck's ET009 already tells the
    same false story on §7.3-legal models; filed as #92. Contract text
    aligned to the shipped predicate with the over-subscript arm stated
    honestly (deliberately not judged; guard E011; owner #82); every
    excluded shape named with its task (#82/#87/#92); fourth acceptance
    arm tested; regression proven non-vacuous by assertion inversion;
    PROBE debug print removed before the flag arrived (verified 3 ways).
    1189/1189; canary 20/20 identical. Verification probes the
    discriminator's edges (true-positive THROUGH inheritance must still
    fire — over-abstention would regress the fix's purpose).
  - 2026-07-31 #65 fix-cycle verification: ACCEPTANCE-READY — the whole
    initialization workstream (#65 + seam + #74, tasks #45/#65/#74)
    accepted and closed. All five fixes verified in source and behavior
    with OMC agreement on every decisive probe plus five adversarial
    variants the verifier built (incl. V4/V5 two-algebraic-pins proving
    the Check residual reads the SEEDED STATE — the silent-pass mode of
    the first cut is closed; V10 all-pairs contradiction that
    each-vs-first would miss). The F2 identity argument formally closed:
    the old cap fired only at >=3 accumulated variables where classify
    returns None — universal coincidence proven. task65-final artifacts
    corroborated (65 compared / 54 strict-high, git_commit bf9de519; the
    FluidHeatFlow improvement is exactly TwoMass + IndirectCooling, the
    only per-model changes in the cohort = the #69 fix); its band table
    fails the NEW #73 tooling's digest check as expected (written by a
    pre-round-2 binary — freshness, not integrity; landing cert will be
    fresh). New task #93 (MED, pre-existing): the closure's conservative
    Unusable/Pending paths silently DROP a fixed=true §8.6 pin on
    discrete/state/conditional displacements with no diagnostic —
    OMC-divergent, needs typed ES012-family loudness. Claim-text
    correction noted (counts were 78+293, not 114+272). Landing batch
    now: ACCEPTED #69 #22 #73 #65/#74; awaiting #51/#52 verification,
    #64 review, #86 fixes — then gates, signed commits, certification.
  - 2026-07-31 #51/#52 verification round: NOT ACCEPTANCE-READY — the
    authority discriminator is wrong in both directions. Still-live HIGH
    false positive: MLS §7.3 component-modification redeclares
    (Holder h(redeclare C a[2])) pass the declared-in-instantiating-
    class test and EF026 fires on OMC-legal code; and the ENTIRE
    inherited true-positive class is lost — d_B probe: declaration and
    connect both inside Base, `model M extends Base; end M;` with NO
    modification anywhere, authority denied, and the abstained path
    FABRICATES flat references (Real[1] a.e on a scalar declaration,
    a.f[1] undeclared, spurious flow-zero) violating the repo's own
    every_equation_reference_names_a_declared_variable invariant before
    dying as wrong-story ED001. Round 3 mandate: per-instance
    had_redeclare flag set where collect_redeclarations consumes (the
    predicate must test what the doc says — "no redeclare touched this
    component on any route"); territory extended minimally into
    phase-instantiate; required outcome matrix d_B/C/D -> EF026,
    d_L/redecl2/redecl4 -> abstain, locals unchanged. ALSO LAUNCHED:
    #67 fixer into freed structural territory — the top-value item: the
    fixed=true guard learns to TRANSFER stated values through the
    accepted pin closure (per #75's design) with a post-demotion
    completeness check instead of refusing; acceptance evidence is
    DCPM_{Start,Temperature,CurrentControlled} through solve + comparator
    band, SpringDamper transfer, untransferable still ES012, #24 + alias
    transfer regressions green.
  - 2026-07-31 #64 review verdict: REJECT WITH FINDINGS, fix round in
    flight. Mechanism confirmed correct (§8.5 scheduled instants;
    strict/compound/parameter/algorithm forms all OMC-matching;
    pre-threshold reschedule boundary holds — MSL's 7 when-time sites
    all use discrete thresholds and are safe; panic->EL001 and
    vector-initial() gains real). Three REGRESSIONS, A/B-attributed
    with only the three #64 files reverted: F1 CRITICAL — plans keyed
    by raw source Span with first-wins insert, so a class instantiated
    twice with different thresholds loses the second instance's instant
    AND its root (never fires; baseline matched OMC; array instances
    dead too) — key by occurrence, refuse differing-instant drops;
    F2 HIGH — instant==0 accepted, so `when time > 0` schedules a stop
    at t=0 where strict reads false (never fires — the fix's own defect
    class relocated), plus a permanent 2e-6 state offset from the t=0
    right-limit restart, plus >/>= at zero INVERTED vs OMC; F3 MED —
    scheduled instant coinciding with a state crossing splits into two
    event iterations changing pre() (OMC: one iteration, common pre).
    Pre-existing divergences measured and filed as #94 (falling/<=
    reads-true activation — one new classification test BLESSES the
    wrong instant and must be fixed; vector activation lowered as OR
    losing per-element edges; state-root snap gives two answers for one
    trajectory). Doc corrections: OMC also right-limit stamps
    (~1.5e-10); falling divergence is solver-path dependent.
  - 2026-07-31 #86 fixes complete (uncommitted, focused verification in
    flight). (1) The stale erasure test rewritten to pin RETENTION
    (dims==[0], zero scalars, still standalone) with the count-semantics
    decision made by reading every consumer: declaration-count for
    *_variables, scalar-weighted *_scalars is what balance accounting
    already reads — no production change needed; semantics documented on
    CheckedDaeCounts. MLS citation corrected: zero-sized legality is
    §10.1/§10.7 (not §4.8 as my task text said); §4.7 balance counting
    was right. (2) uses_state_only_path extracted as the single source
    of truth for the Bdf state-only split, used by BOTH the simulation
    builder and check_initialization; check_state_only_initialization
    runs the same three init steps the state-only simulator runs PLUS
    initial-derivative finiteness — claimed equivalent-or-stronger;
    discriminating unit test proven (branch disabled -> the exact old
    error). Canary: zero flips, aggregates byte-identical; the one
    delta is a baseline HARNESS FLAKE resolving in the fix's favor
    (AutomaticSeed: 60s startup-budget timeout at baseline vs the
    genuine deterministic ED019 impure-call rejection now — the #22
    purity semantics surfacing on a real MSL model). Coverage caveat
    recorded: the pure-explicit-ODE shape is absent from the canary
    entirely (why the defect survived); the diffsol unit test is the
    real coverage. Verification probes the equivalence claim (init
    inconsistency, infinite derivative, boundary 1-state+1-algebraic)
    and the cohort no-flip grounding.
  - 2026-07-31 #86 verification: ACCEPTANCE-READY with one required edit
    (in flight). The equivalence claim VERIFIED by an 11-probe A/B that
    forced the general path via Esdirk34 on identical DAEs — check[BDF]
    verdict == simulate[BDF] verdict on all 11; initial-equation
    enforcement is byte-identically shared (both branches call
    initialize_state_runtime_values with identical arguments); the
    detached-algebraic gap the verifier hypothesized is disproven by
    probe; and the new path is strictly STRONGER on one shape — the
    GENERAL path false-accepts InfDerivativeWithAlgebraic (check OK,
    simulation immediately non-finite). Blast radius grounded both
    directions: zero cohort models are pure-explicit (0 with
    algebraics==0 && states>0 across three 566-cohorts), zero error-
    string hits, zero reverse-flip candidates among ic_ok+non-sim_ok.
    The one defect: the added finiteness check is UNREACHABLE dead code
    (the runtime's validate_finite_derivatives precedes it) with a false
    doc claim — removal/re-documentation ordered. Canary caveats
    recorded: not a clean A/B (co-tenant tree movement; AutomaticSeed
    delta is a baseline harness flake resolving to the genuine #22
    ED019 impure-call rejection) and the canary has ZERO discriminating
    power for this fix (no pure-explicit shape among its models) — the
    probe suite is the positive evidence. New task #95: SimulationSession
    (viewer/scheduled entry) still lacks the state-only branch, and the
    general init check's derivative-RHS blindness (the false-accept).
    #51/#52 round 3 landed the had_redeclare marker per the verifier's
    design (full probe matrix green incl. the over-abstention control;
    1540/1540) — targeted re-verification by the same verifier in
    flight with three fresh edge probes ordered.
  - 2026-07-31 #51/#52 round-3 verification: all five round-2 findings
    CLOSED (inherited true positives restored d_B/C/D, component-
    modification hole plugged d_L, corruption path unreachable for
    non-redeclared components, docs and contract test aligned), marker
    precision verified exact (sibling redeclare does not invalidate its
    neighbor; ordinary modifiers do not suppress; same-type redeclares
    mark; different-type with dims abstains), no MSL drift from the
    inheritance-merge change (6 models re-measured). ONE remaining
    blocker, round 4 in flight: a redeclare nested inside a component
    sub-modifier of an extends modification (extends Wrap(h(redeclare
    C a[2]))) escapes marking — collect_redeclarations only marks
    direct components; the one-branch fix (mark the enclosing component
    when the target is nested, mirroring the component-modifier route)
    plus a sibling-non-invalidation unit test. Documented conservatism
    accepted: redeclaring one member suppresses judgment on sibling
    members beneath the same enclosing declaration (stated in the doc,
    one lost true positive per redeclared enclosure).
  - 2026-07-31 #86 CLOSED (accepted): dead finiteness check deleted with
    independent re-derivation of the unreachability proof — deletion
    chosen over defense-in-depth because an unreachable branch is
    untestable ballast that would shadow the runtime's better message
    (NonFiniteDerivative names the state; the dead check only had an
    index); doc rewritten to attribute the guarantee to
    eval_state_derivatives_with_guess/validate_finite_derivatives;
    retention test extended to p (parameter role, zero scalars) via an
    extracted helper rather than a nesting allow. All green after final
    edits (82 + 115, both target tests, fmt/clippy). #40 launched into
    the freed slot: the comparator's event_time_mismatch share cap
    (0.498 < 0.50) mislabels a REAL sustained discrete disagreement
    (Ideal.off) as an event-timing artifact — fix direction: the label
    must require mismatches CONFINED to event neighborhoods, not merely
    a share under a cap; honest band DROPS from Ideal-family models
    moving into deviation are expected and desired. Remaining lanes
    before landing: #51/#52 round 4 (one-branch nested-redeclare mark),
    #64 fix round (occurrence keys, t=0 semantics, coincidence join),
    #67 (transfer machinery), #40 (fresh).
  - 2026-07-31 quadruple harvest, review wave launched. #67 COMPLETE
    (under review): the fixed=true guard now asks the REBUILT system
    whether the stated value survived — admissible/conditional split,
    Invariant arm DELETED (proved only "some constant", not the stated
    one), represented_initial_values under four proofs with a
    superset completeness check as a checked construction op, ES013 for
    genuine conflicts. FLAGSHIP RESULT: DCPM_Start compiles, sim_ok,
    ic_ok, HIGH BAND — 165/165 channels high, model L1 1.24e-17, IC
    error 0.0, solver states exactly OMC's three; DCPM_Temperature/
    CurrentControlled compile+ic_ok (sim_solver_fail matching cohort);
    SpringDamperPrefer transfers OMC-exact (half of #75 done); canary
    zero deltas. Score poised for 55 at certification. Recorded gap ->
    task #96 (time-varying defining equation still ES012). #64 fix
    round COMPLETE (under review): occurrence-keyed plans (operands) w/
    fallible insert, P10/P13/P14 all MATCH; t=0 semantics adjudicated
    on OMC's fine grid (right-limit application; instant<=0 not
    schedulable; offset gone, x(1)=1 exact); F3 half-fixed (diffsol
    right-limit carry — every scheduled event had been discarding 2e-6
    of state; rk-like attempt regressed and was honestly reverted ->
    task #97, incl. pre-existing P8-on-diffsol never-fires); found and
    fixed its own test-integrity bug (tests claimed rk-like, ran
    diffsol). #51/#52 round 4 COMPLETE (under combined verification):
    nested-extends leak fixed PLUS its symmetric component-modifier
    twin (recursive expression_contains_redeclare walk marks the
    enclosing component), sibling-precision unit test added,
    1541/1541. #40 COMPLETE (under combined verification): per-hold
    majority-agreement rule (strictly implies the removed horizon cap),
    new DiscreteLevelDisagreement shape, offline replica validated
    0-mismatch vs landed then reclassified all 2416 channels — exactly
    ONE moves (Ideal.off, worst-hold 0.996 vs nearest survivor 0.398),
    zero band changes (the model was already Deviation — the mislabel
    corrupted DIAGNOSIS, not score; recorded honestly). Two file-size
    gate violations (connections/mod.rs 2035, solver-diffsol tests
    2064) under mechanical extraction. Review wave: #67, #64, #40+#51/
    #52 combined, size fixer — four agents.
  - 2026-07-31 #64 second review: F1 VERIFIED (nine probes incl.
    collision cases, fallible insert never false-fired), F2 VERIFIED,
    driver-kernel gain confirmed (baseline lost scheduled-event
    exactness and dropped an event on rk-like). NOT acceptance-ready on
    two cheap blockers, fix round dispatched: (1) the P8 header
    misdescribes its own regression — the diffsol "joined" claim is
    false as evidence (P8 fires NOTHING on diffsol either way) and the
    b: 10->11 change is presented as pre-existing when it is a
    regression vs the pre-change tree; (2) undisclosed scope creep —
    the vector-activation arm converts the MSL TimeTable/CombiTimeTable
    idiom (when {time >= pre(nextEvent), initial()}) from a LOUD ED018
    into a silently wrong trace (y stalls at 1; OMC counts 1,2,3,4);
    decision: targeted typed rejection for non-parameter-evaluable
    time-relations in vector activations (keeps the verified static-
    vector gains), underlying reschedule stall filed as #98 (affects
    the counter idiom family; scalar form with in-body threshold update
    stalls too). Reviewer's judgment adopted: the bounded rk-like
    coincidence regression is landable WITH truthful recording (no MSL
    when gains a scheduled instant — all 7 sites have discrete
    thresholds the scheduler rejects). Size extractions: flatten
    connections (2049->1811 + endpoint_subscripts.rs) and diffsol tests
    (2064->1609 + state_path_integration.rs) done, 668/668 preserved;
    third file (construction.rs 2006) dispatched now that its reviewer
    finished. Remaining before landing: #67 review, #40+#51/52
    verification, #64 blocker round, construction.rs extraction.
  - 2026-07-31 late evening: #40 and #51/#52 ACCEPTED AND CLOSED. #40:
    the per-hold rule verified well-defined under disagreeing boundaries
    (conjunction of both traces' partitions — the stricter governs);
    shape provably cannot feed bands (exhaustive consumer grep: two
    display-only sites); the one-channel-moves claim held under an
    independent 93-discrete-channel cross-reference; pure-addition test
    diff; five future-stability advisories filed as #99 (no minimum
    hold span and window-edge truncation are the churn risks worth
    watching on new cohorts). #51/#52: full probe matrix confirmed on a
    rebuilt binary, plus a fresh depth-2 modifier-nesting attack
    (extends W(h(nest(redeclare C a[2]))) and its component twin and a
    dotted target all correctly abstain) with a NEGATIVE CONTROL
    proving the ED001s come from redeclare marking, not topology;
    sibling non-invalidation unit-tested; scope doc names all four
    routes. LANDING NOTE: endpoint_subscripts.rs and
    endpoint_subscript_tests.rs are UNTRACKED — must be git added at
    commit. connections/mod.rs now 1811 (under threshold); the gate's
    sole remaining size offender is construction.rs 2006 (extraction in
    flight). Remaining lanes: #67 review, #64 blocker round,
    construction.rs extraction. Accepted batch now: #69, #22,
    #73(+#72), #65/#74(+#45), #86, #51/#52, #40 — with #64 and #67 to
    join on their verdicts.
  - 2026-07-31 #67 review verdict: REJECT WITH FINDINGS — the policy's
    biggest catch of the session. The DCPM gain is CONFIRMED (165/165
    high, IC error 0.0, solver states exactly OMC's three, comparator
    non-vacuous, 7/7 compared) and the F-S2 Invariant deletion is sound
    (proof-backed, OMC-consistent) — but the tree as it stands
    REGRESSES 23 OF 60 landed-Success models to bare ES010. Mechanism:
    F1 represented() treats Undecided (any parameter-valued difference)
    as hard refusal, contradicting the module's own stated policy and
    refusing shapes the deleted arm accepted (Rotational.Fixed's
    phi0-parameter pin; OMC simulates) — the fix is to count Undecided
    members as represented via emitted Check residuals, the same §8.6
    reading the transfer lane already implements; F2 a value-discard
    refusal that merely HOLDS the residue is never recorded, so it
    degrades to the bare structurally-singular error the module doc
    explicitly names as the outcome to avoid — which made the -23
    invisible; F4 the holonomic route is exempt from the postcondition
    on a factually wrong rationale (the residual is REPLACED by its
    second derivative, not added); F3 the submitted canary artifacts
    predate the final edit (18:58/19:07 vs 19:11) and do not describe
    the tree — full re-measurement ordered (60-model sweep must return
    to ~60/60, fresh canary, DCPM trio re-confirmed). Also: proofs
    (a)/(b)/(d) verified correct, pin-lane composition clean (no
    double-write), hygiene clean. New task #100: the comparator's
    state_selection block reads DAE states not solver states —
    every state-selection percentage in certification artifacts is
    untrustworthy (DCPM reports 13/mismatch while the solver set
    exactly matches OMC). Landing HOLDS for the #67 fix round + the
    #64 blocker round; seven work-sets remain accepted and entangled
    tests make partial landing unsafe (the alias-transfer file carries
    a #67-dependent test).
  - 2026-07-31 #64 CLOSED (accepted): blocker round done with an
    evidence-backed narrowing — the reviewer's rejection predicate
    verbatim would have broken the GREEN vector self-rescheduling
    counter test (closed-form thresholds compiled and simulated
    correctly before the arm), so the rejection is gated on exactly the
    newly-reachable case: a vector carrying initial() AND a
    rescheduling time relation. TimeTable/CombiTimeTable (the two MSL
    sites) now reject typed ED018 naming the reschedule gap (#98); the
    other nine MSL vector-initial() sites keep the arm's gains;
    pre-change reachability restored precisely. Header rewritten: the
    false diffsol claim replaced with the truth (P8 fires nothing on
    Bdf — separate pre-existing defect), b: 10->11 recorded AS A
    REGRESSION with its bounding evidence, five divergences tagged.
    Bdf simulation test added asserting the driver-kernel exactness
    (x(1)=1). 236+283 green; canary zero deltas, honestly labeled
    parity-unmeasured. Batch now: EIGHT accepted work-sets (#69, #22,
    #73, #65/#74, #86, #51/#52, #40, #64). Landing holds only on the
    #67 fix round (F1/F2/F4 + re-measurement); #84 CLI rendering in
    flight may join or follow.
  - 2026-07-31 #67 ACCEPTED — batch complete at TEN work-sets, landing
    begun. The fix round dissolved the review's central alarm: the "23
    regressions" premise conflated compile-Success with solve-success
    (24 of the sampled 60 already failed Solve with EL005 at every
    baseline). Three-way measurement over the reviewer's own population:
    pre-guard 16/60 -> landed 30/60 -> current 36/60 = #67 is +6,
    model-for-model reconciled (0 differing vs the reviewer's sweep);
    the six DCMachines that landed as ES012 now emit their pre-guard
    ES010 verbatim — demotion admitted, remaining singularity genuine
    (wave-5 EL005 backlog). F1 three-valued (only PROVED differences
    refuse; parameter-valued left to the initialization instant, the
    module's own policy, Rotational.Fixed cited); F2 every value-discard
    refusal recorded (unrelated singularities still self-report); F4
    holonomic route gated with the wrong rationale deleted; F3 all
    artifacts re-measured post-final-edit (canary zero deltas; DCPM
    trio fresh: Start sim_ok+ic_ok 165/165 high IC 0.0, solver states =
    OMC's 3; SpringDamperPrefer OMC-exact; 120+238+495 green; the two
    size-budget failures gone). ES012/ES013 acceptance contracts in
    diagnostic_codes.rs (the repo's per-code catalog surface); honest
    gaps recorded (#96). One follow-up nit noted: a pin contradicting
    its class's asserted constant reports ES012 where an ES013-shaped
    inconsistency diagnostic would fit better — needs a new typed path.
    LANDING: full-workspace nextest gate running; then ~10 signed
    commit slices; then Tier-2 certification --all-omc-targets, fresh
    results dir, on the hardened band-table tooling.
  - 2026-07-31 ~22:15 THE BATCH LANDED. Workspace gate 5222/5225 with
    all three failures triaged before commit: two fixed-wing GALEC
    EGT017 failures PRE-EXISTING at clean HEAD (pristine-worktree
    proof; filed #102 — coupled-discrete-real record assignment), one
    LSP timing test a load flake (passes solo). Ten signed commits on
    bf9de519, tree clean: b9ddfe43 sim per-hold classification,
    8630c228 semiLinear skew, 3dee93e0 purity MLS 3.7, 0b1006db
    band-table cohort pinning, 94de6a73 state-only init check,
    f7a774d0 connection endpoints + had_redeclare, 61e96c12 the
    initialization workstream (substitution, bipartite planner, pin
    transfer, demotion completeness), ea82ebe0 occurrence-keyed time
    events, a8477034 full CLI diagnostics, 19cdcbb2 spec/gates. Every
    slice adversarially reviewed before acceptance; tasks closed this
    session: #22 #40 #45 #51 #52 #58 #59 #60 #64 #65 #67 #69 #72 #73
    #74 #84 #86. CERTIFICATION RUNNING: verify msl-parity
    --all-omc-targets into fresh target/msl/results-landed-2; on
    completion record bands here, run the transition diff vs
    results-landed, close #68 if the cohort pins clean, then sweep the
    run's bulk artifacts (traces/omc_sim_work ~100G) keeping JSONs.
    Honest expectation: 55 strict-high (DCPM_Start returns); any other
    movement gets named by the diff and adjudicated.
  - 2026-07-31 ~22:45 CERTIFIED: **strict-high 56/566** (was 54), near 6,
    deviation 4, models_compared 66 (skipped 2), omc a96aa1a-cmake,
    results in target/msl/results-landed-2 with the first band-table-
    pinned, provenance-stamped, transition-diffed certification (task
    #68 CLOSED — the cohort is pinned and every departure is named).
    Movers, all named by the diff: ENTERED high — Translational
    Accelerate + Oscillator, FluidHeatFlow WaterPump (the initialization
    workstream's gains); LEFT — SaturatedInductor (was high ->
    sim_solver_fail EX002 projection divergence at an event boundary; a
    REAL batch regression, filed HIGH as #103, suspects the #64 right-
    limit carry or init-substitution event re-seeding); NandGate (was
    deviation -> no comparable samples, filed #104). DCPM_Start
    simulates 165/165 high with IC 0.0 (proven in the #67 focused run)
    but TIMED OUT the harness's 10s sim budget by 0.549s — excluded
    honestly, filed in #104 as a measurement-config decision (+1 when
    resolved; +1 more when #103 lands = 58 in reach). sim_ok 66->69.
    The gate exits 1 on the promoted-baseline floors (pre-existing
    branch gap, misses unchanged) and the 85 hard floor (the milestone,
    56 < 85 — correct and loud). Certification pipeline lessons
    recorded in memory: reference traces ARE the cache (my sweep
    destroyed them and forced a killer-tripping full regeneration; the
    resumable omc-simulation-reference stage at 6 pinned workers is the
    recovery path). #77 (full §8.6 solve) building in a worktree
    against this certified base.
  - 2026-07-31 #77 implemented in worktree (under adversarial review).
    The full §8.6 unknown space: states with fixed != true join unbound
    fixed=false parameters as projection unknowns (fixed=true is an
    EQUATION per §8.6, never an unknown); der(x) discharged by
    substitution through the structurally-matched continuous row;
    parameters-first maximum matching, square blocks only, surplus rows
    stay consistency checks; row_targets now name coordinates in
    failures; two new typed contract checks; five boundaries recorded
    (algebraic reads disqualify — needs the refresh folded into the
    init Newton loop; discretes -> #44; array states; multi-scalar
    rows; the under-determination tie-break diverges from OMC and is
    pinned as such since §8.6 does not pick). Canonical repro x(0)=5
    exact; all #74 probes OMC-exact; homotopy 12/12; full worktree
    suite green. STRATEGIC RECALIBRATION (to be verified by the
    review): the §8.6 gap's MSL surface is ~2 models TODAY — only 19
    of 566 both reach Solve and carry initial equations; the IC cliffs
    are masked by the DAE-construction wall (239 models) and the EL005
    Solve-lowering wall (44) upstream. If confirmed, the score levers
    are the wave-queue walls (parameter-valued shapes ~108,
    ExternalObject/ED019 ~36, EL005 index-reduction family 44), and
    the #77 depth-work is correctness infrastructure that pays as the
    walls fall. #103 bisect continues on the main tree.
  - 2026-07-31 #77 review: mechanism VERIFIED everywhere (U/E accounting
    on all probes incl. fixed=true-plus-restating and nonlinear der-root
    selection matching OMC per guess; parameters-first proven a correct
    matroid greedy; tie-break deterministic and exactly pinned; both
    contract guards proven unreachable-by-construction; 5170-test gate
    lane green) — reject-with-findings on TRUTH-TELLING: the new header
    claims excluded rows "stay consistency checks" but algebraic-reading
    rows are checked against UNREFRESHED SEEDS — vacuous both ways
    (P1e2 silently simulates x(0)=0 where OMC gives 5; consistent P1e4
    refuses on the stale seed where OMC succeeds). Fix round in flight:
    typed refusal for algebraic-reading excluded rows (fail-loud),
    GATED on first checking the 19-model init population — if any of
    the 3 high-band models trips, stop and decide rather than trade
    certified score for loudness; message split for the surplus-vs-
    outside-unknown-space cases (IdealTriacCircuit's row targets a
    discrete — genuinely unsolved, not surplus); over-refusal costs
    added to the boundary record; tie-break divergence moved to the
    header. STRATEGIC CLAIM VERIFIED EXACTLY with the right framing:
    19 init-carrying models is a FLOOR measured over the 136 that
    compile (initial_equation_scalars is null for all 430 non-Success
    models; 149 MSL files carry init sections) — initialization depth
    pays out progressively as the DAE/Solve walls fall, which is what
    the shapes wave (in flight) attacks.
  - 2026-08-01 #103 root-caused (fix under review): the certification's
    -1 was NOT a semantic regression — the occurrence-keyed time events
    correctly stopped scheduling a t=0 pseudo-event for SineVoltage's
    `if time < startTime` (startTime=0, never true), and the solver
    restart that event had been forcing was MASKING a latent projection
    defect: past the permeability peak the damped Newton's tangent
    extrapolates mu_r through zero, and the 1/G_m^2 ~ 1e11 row scaling
    makes a reluctance wrong by 1e8 read as a SMALL scaled residual, so
    the branch-leaving step LOWERS the merit function and the line
    search accepts it (61e96c12 diverges identically on interior rhs
    evals where BDF retries; the output sample has no retry). Fix: a
    trust-region retry (per-unknown step cap 0.25) that runs ONLY on
    the currently-failing path, restores the entry snapshot, re-raises
    the original diagnostic on failure — not a fallback. Evidence:
    reduced 5-unknown fixture matches OMC to 5 digits (B=1.493654,
    mu_r=854.297); SaturatedInductor restored to HIGH 59/3/0/0; 10-model
    focused comparator 10/10 high with every non-target bit-identical
    to results-landed-2; zero cost on converging paths. Bisect lesson
    recorded: the harness --dt is load-bearing (first pass invalid
    without it). Review probing the root-selection question (can a
    capped retry converge to a nonphysical root and report success?)
    and the 0.25 constant's sensitivity. Pattern note: third
    "regression" today to dissolve into a pre-existing defect on
    inspection (DCPM phase-vs-solve, fixed-wing GALEC, this).
  - 2026-08-01 shapes wave complete (worktree, under review) — THE WALL
    MAP, first full failure census of the 430 non-compiling models:
    resolve walls ER003 base-class-not-found 70 + ER002 unresolved 53
    (=123, the Fluid/Media/MultiBody frontend — BIGGER than the DAE
    walls); ED018 runtime operators 58; ED019 families (shape-proof 52,
    other 43, value-type/ExternalObject 36); ED008 24; EF024 23; ED010
    15; long tail. Sub-family LANDED: value-proven function
    specialization — FunctionSpecializationKey carries proven input
    VALUES (f(3) and f(5) own distinct certificates; §12.2/§4.4.2/§4.5
    cited; locals settle extents only when never assigned §12.4.4;
    validator and lowering share one proven_extent predicate; >256
    nesting typed-rejected; formals never inherit shadowed model
    coordinates). Result: shape wall 52 -> 6 (46 advanced, 0
    regressions, canary 8/8, one test STRENGTHENED-retargeted with a
    positive twin) — and the honest structural finding: 0 newly
    compile; each advanced model hits the named NEXT wall, bucketed
    with signatures as #105 (24: function conditionals + comprehension
    shapes), #106 (25: constant-evaluator gaps incl. one suspicious
    index-out-of-bounds), #107 (5 tuple-assignment + 6 rank/arity
    residue). The walls are now a ranked, signed queue instead of a
    fog. task65-final bulk swept (107G reclaimed, JSONs kept).
  - 2026-08-01 #77 fix round complete — STOP CONDITION HONORED: the
    typed refusal for algebraic-reading init rows fires on 5/19 incl.
    2 of the 3 certified high-band models, and BOTH are correct today
    (Init.InitialOutput outputs equal to their seeds; OMC values
    match), so the refusal was implemented, measured, and deliberately
    NOT landed — two right answers will not be traded for a
    diagnostic. Landed instead: per-kind honest headers with both
    measured failure directions (P1e2 silent x(0)=0 vs OMC 5; P1e4
    consistent-but-refused) pinned by a drift-guard test; the
    diagnostic split via a schema-versioned row_roles field (24->25,
    golden updated) — IdealTriacCircuit now reports "outside the
    planned unknown space: discrete" instead of the false "consistency
    check", genuine surplus reports §8.6-check language; over-refusal
    costs named vs OMC; tie-break + guess-dependence records in the
    header; rustdoc -D warnings lane CLEAN (the 61e96c12 private-link
    fixed). Population identical pre/post (3 ok / 7 EX001 / 7 EL005 /
    2 EX002). The REAL fix filed as #108: fold the algebraic refresh
    into the §8.6 solve so the vacuous checks become real — then the
    silent class closes without sacrificing the two models. Pending
    landings: #77 (worktree), shapes wave (worktree, review running),
    #103 (main tree, review running).
  - 2026-08-01 #103 review: CODE ACCEPTED, narrative rejected. The
    root-selection question answered with thousands of vendored-module
    trials: 0 branch switches on-manifold; the only switches (18/1904)
    sit within ±7% of a fold on off-manifold warm starts — and the
    PRE-FIX unlimited pass switches MORE (it leaps folds the cap must
    traverse); the 0.25 trust constant proven speed-only (identical
    root 0.01-0.9); blast radius exactly 1 model of 566 (4 other
    projection-error models unchanged with byte-identical diagnostics);
    SaturatedInductor's actual solution space has NO wrong root
    (mu_r >= 1 forced, g(B) strictly decreasing). MAJOR DISCOVERY
    (finding 2, filed HIGH as #109): event lowering is BUILD-DEPENDENT
    — byte-identical sources yield different DAE IR per binary
    (scheduled event present/absent, roots 1 vs 2, p_scalars 42 vs 43;
    stable per binary, immune to RAYON/cache) — a bit-for-bit
    determinism violation in the occurrence-keyed plan machinery that
    invalidated the bisect (the ea82ebe0 attribution does NOT
    reproduce: both commits' solve IR byte-identical, both simulate at
    the cert invocation, both fail at finer grids) and explains the
    harness-vs-CLI split. Required before landing: narrative reduced
    to the demonstrable (latent defect, fatal at parent; exposed by
    #109), the MSL regression re-pinned to a provably-failing config
    (current one passes on clean pre-fix builds), the vacuous restore
    fixture replaced with the reviewer's mutating shape. Fix round
    dispatched. The identity-proxy robustness theme strikes again —
    the third instance (spans, scopes, now interned-ID ordering).
  - 2026-08-01 shapes review: REJECT WITH FINDINGS, fix round dispatched.
    Core verified (shadowing both directions, redeclare chains, mixed
    sites, census direction confirmed over ALL 566 with 0 regressions /
    0 newly-compiling reproduced) but three HIGHs: (1) binder shadowing
    fixed in lowering AND discovery but MISSED IN VALIDATION — silent
    wrong loop domains (18 iterations where the triangular 3 is
    correct, no diagnostic) — the split-environment disease again;
    (2) "validator and lowering cannot disagree" REFUTED — shared
    predicate, different environments (certificate.values unshadowed
    vs loop_shapes shadowed) — typed ED019 downgrades to internal
    ED020; (3) REGRESSION on legal bounded recursion — the 256 cap
    walks both conditional arms so base cases never prune; f(3) with
    an n-1 base case compiled at baseline, now rejected with a message
    asserting a false fact (needs proven-condition pruning + the
    SPEC_0008 contract + positive twins). MEDs: If/While/When-assigned
    locals settle (§12.4.4 not actually computed); zeros/ones/fill
    extents not widened (ED020 downgrade); CENSUS CORRECTION — 19 of
    47 "advanced" are a constant_context reordering artifact (shape
    proof never ran for them): honest split 24 conditional + 4
    statement + 19 unattributable; specialization-key explosion (10
    byte-identical DAE functions; GALEC resolves by name — identity
    hazard). Also measured: the specialization machinery is INERT for
    every currently-compiling model (22/22 sampled emit zero DAE
    functions) — the wave's present-day effect is exclusively which
    typed rejection 28 genuinely-advanced models report; its value
    realizes when the conditional/statement walls fall (#105/#107).
  - 2026-08-01 #109 RETRACTED — determinism HOLDS. The "build-dependent
    lowering" was the bisect harness's own stale-artifact bug: a shared
    target/ dir across checkouts with preserved backwards-moving mtimes
    meant cargo never rebuilt phase-dae, so the "culprit" binary linked
    a pre-ea82ebe0 rlib (proven by marker-string absence, a behavioral
    probe showing pre-#64 when-collection, and fingerprint timestamps);
    the observed divergence is exactly the real F2 boundary change
    (instant < vs <= 0.0) between actual commits. Determinism proven
    bit-for-bit across two independently-built binaries; occurrence
    keys audited source-stable (FNV SourceId, structural equality,
    lookup-only maps); one test added pinning the equation-residual
    collector's start-instant behavior both sides of the boundary.
    Harness lesson recorded in memory (bisect-harness-isolation): every
    bisect checkout gets its own CARGO_TARGET_DIR; verify binary
    provenance via marker strings before trusting per-commit results.
    #103's narrative guidance corrected accordingly. Remaining before
    the second landing: shapes fix round + #103 fix round.
  - 2026-08-01 #103 LANDED (bda51ff6) + the #109 boundary test
    (573dcbb2); tree clean at 573dcbb2. The fix round closed all three
    required items: narrative reduced to the demonstrable (latent
    defect, fatal at parent, reached via the CORRECT F2 §8.5 boundary
    change — no #109/nondeterminism/masking claims survive a grep);
    the MSL member re-pinned to dt 2e-4 and MUTATION-VERIFIED (retry
    neutered -> test fails with the exact projection error); the
    rollback fixture replaced with a mutating two-block shape, also
    mutation-verified (deleting the restore fails the assert). Retry
    orchestration extracted to projection/retry.rs (projection.rs
    1970/2000); retry-also-failed now traced; focused parity 10/10
    high with SaturatedInductor 59/3/0/0 and the nine others
    bit-identical to results-landed-2. SaturatedInductor's +1 is now
    in the tree — certified 57 expected at the next certification.
    Remaining before the second certification: the shapes fix round
    (worktree, in flight) and the #77 worktree harvest (accepted; its
    projection.rs/initial_diagnostics edits must be reconciled against
    bda51ff6's retry extraction at apply time).
  - 2026-08-01 ~01:50 SECOND LANDING COMPLETE: 2dba4812 (the §8.6
    unknown space — states as projection unknowns) + 08c13b10
    (value-proven function specialization) on top of 573dcbb2/bda51ff6
    (#103 retry + #109 boundary test). Tree clean at 08c13b10; real
    combined gate 5188/5188 at the spawn-safe 10-job budget. The
    landing survived a self-inflicted scare (shell cwd persisted into
    a worktree after a patch-export cd; a reset --hard hit the
    worktree's branch and the main tree was never damaged — the
    "missing commits" were a worktree HEAD misread; lesson in memory:
    pwd + rev-parse before destructive git ops, git -C always) and two
    spawn-churn kills of the gate (threshold refined in memory: 10
    jobs max for wide rebuilds; 14+ dies regardless of free memory).
    THIRD CERTIFICATION RUNNING into results-landed-2 (cached
    references; rotation produces the transition diff). Expected: 57
    strict-high (SaturatedInductor returns; the initialization and
    specialization slices measured neutral-to-positive in their
    reviews). #77 CLOSED.
  - 2026-08-01 ~02:00 THIRD CERTIFICATION: **strict-high 58/566** (near
    6, deviation 4; models_compared 68, skipped 2; omc a96aa1a-cmake;
    tree 08c13b10). Rotation diff against the 56-baseline: **ENTERED 2,
    LEFT 0, BAND-CHANGED 0** — DCPM_Start -> high (back INSIDE the 10s
    sim budget on the new tree; its #104 half resolves without touching
    measurement config) and SaturatedInductor -> high (the #103 retry
    working as designed). sim_ok 70. Session arc: 54 -> 56 -> 58, every
    point earned through reviewed work-sets and named movers, zero
    unexplained movement across three certifications. Gate exits 1 only
    on the promoted-baseline floors and the 85 milestone (58 < 85 —
    correct and loud). Remaining #104 half: NandGate no-comparable-
    samples. Next wave dispatched into the walls: #105 (function
    conditionals, 24 models) and #83 (the §15.1 silent stream hole).
  - 2026-08-01 #105 review: mechanism VERIFIED (OMC-confirmed fold
    semantics, first-unproven-stop, definedness both directions, zero
    lost passes over byte-identical sampling; all 24 bucket models
    advanced — 2 to checked-DAE [E011 downstream, parity unmeasured],
    20 to aggregate-expression, 2 to range-end; the SuperCap hard
    panic closed). Reject-with-findings, fix round dispatched with the
    DECISION: dead branches stay TYPE-CHECKED per §11.2.1 (base and
    OMC both reject an ill-typed dead arm; the fold's widening is
    provability-acceptance, not type-acceptance) while remaining
    exempt from value-proof requirements (no dead-callee certificates
    — the P4 property stands). Also: the external-function-argument
    comprehension panic survives both sides (wrong discriminator —
    function_body None vs the true predicate "specialization scope";
    one-line fix + regression); the ValueReadInputs fold gate is
    defensible (§4.5 structural-parameter discipline) but unstated —
    contract + test pair ordered; two contract tests strengthened
    (one was vacuously passing with no value-keyed input); doc example
    misplacement; the hardcoded filter: None made a future silent
    wrong domain — pass the real filter. #83 stream-hole agent still
    in flight.
  - 2026-08-01 #83 complete (worktree, under review): all three connect
    holes closed with zero cohort movement (correctness, not blockers
    — as predicted). The stream hole was WORSE than filed: beyond the
    silent skip, connect_sub_variable was emitting a §15.1-FORBIDDEN
    stream potential equality plus a degenerate self-equation — now
    typed EF027 with both member spans (§9.3 verbatim contract; OMC
    rejects the balanced repro but is NOT authoritative for the
    unbalanced variant, where it warns and generates cross-matched
    garbage — recorded). The §9.1 second sentence (parameter-to-
    parameter, constant-to-constant) had NO enforcement anywhere — now
    EF028 with the OMC divergence OWNED (OMC emits an equality assert
    rumoca lacks, so acceptance is provably under-constrained;
    relaxable if the assert lands); Polyphase Plug m pinned accepted.
    The embedded-index count-1 branch resolves through
    connection_endpoint_dims, None distinct from 1. INFRASTRUCTURE
    CATCH filed as #110: the CLI cache auto-prune behind a global lock
    was serializing parallel sweeps ~10x — plausibly the true
    mechanism of DCPM_Start's original 0.5s budget miss (it passed
    comfortably post-prune in cert 3); a prune-storm guard or per-run
    cache dirs would pay for itself across every timing-sensitive
    measurement.
  - 2026-08-01 #105 fix round ACCEPTED (worktree, pending landing): dead
    branches get ordinary §11.2.1 statement checking via one-sided
    call-free shape rules (can fail to report, never accept a proved
    mismatch) while staying value-proof-exempt — contract now reads
    "Not proven, and never guessed"; the comprehension discriminator
    keys on specialization scope (external-argument panic -> typed
    fold, d_P11 exit 0); the ValueReadInputs gate documented with both
    loads and its cost named as a language-rule property; contract
    tests strengthened (scan-order no longer vacuous, elseif lock,
    specific messages); real filter threaded; movement restated
    honestly (ToDae-OK advances, parity unmeasured; E011 downstream
    for the two checked-DAE models); function_shapes.rs split
    (1504 + expression_rules 550). Post-fix 489-sweep byte-identical;
    456/456. Third landing assembles on the #83 review verdict; #106
    evaluator wave still in flight.
  - 2026-08-01 ~03:45 THIRD LANDING: 4d7d4e9e (proven-branch folding +
    per-specialization comprehension domains — all 24 conditional
    models advance, the SuperCap panic closed, dead branches
    type-checked never guessed) + e1bc54de (§9.3 member pairing —
    EF027 stream, EF028 structural, CONN-030 cataloged, the forbidden
    stream equality eliminated). Gate 5230/5230; tree clean at
    e1bc54de; 18 signed commits on the branch this session. #83 and
    #105 CLOSED. Fourth certification running to pin no band
    regressions from the compile-phase movement. #106 evaluator wave
    still in its worktree for the next batch.
  - 2026-08-01 ~03:55 FOURTH CERTIFICATION: **strict-high 59/566**
    (near 6, deviation 4, compared 69, tree e1bc54de). Rotation diff:
    ENTERED 1, LEFT 0, BAND-CHANGED 0 — the entrant is
    SuperCapDischargeCharge at HIGH: yesterday a hard compiler panic
    (exit 101 on a function-body comprehension index), today a
    strict-high OMC-matching simulation via the #105 wave's
    discriminator fix. Session arc: 54 -> 56 -> 58 -> 59, four
    certifications, every mover named, zero unexplained movement.
    not-attempted dropped 438 -> 435 (the ToDae advances feeding
    the funnel). Bulk swept, reference cache preserved. #106
    evaluator wave continues; queue behind it: aggregate-expression
    bucket (20), #107 tuples, event semantics (#90/#91/#94/#98),
    #92 redeclare dims, the resolve-stage frontend walls (123).
  - 2026-08-01 #106 wave complete (worktree, under review) — the
    biggest silent-wrong harvest of the campaign: THREE of the four
    census signatures were WRONG-VALUE FOLDS, not missing folds.
    (A) assignment targets dropped their subscripts (whole-component
    replacement: symmetricOrientation(6) returned 3 elements as 6;
    y[1]:=1 degraded to a whole write) — fixed per §10.5 with
    element/slice dispatch, no whole-value fallback; (B) function
    entry discarded declaration bindings for type defaults (pi
    entered bodies as 0.0; the "division by zero" was division by a
    wrongly-zero binding, not eager branches) — fixed per §12.4.4
    with in-order evaluation and refusal on unsettleable bindings;
    (C) unbound dotted references guessed into enum literals (z.im ->
    Enum) — fixed with bound-head field resolution. Plus §12.4.6
    element-wise builtins (OMC bit-exact), §12.6 record constructors,
    §10.6.5 array/scalar division. 26-model bucket cleared (17 land
    on the conditional wall #105 JUST FIXED on main — rebase should
    advance them further), zero regressions binary-vs-binary over
    202, canary 20/20, 1914 tests green, every fix carrying its OMC
    value. Review launched with the full silent-wrong mandate (the
    evaluator feeds bindings, extents, and branch conditions).
    Landed worktrees removed (126G reclaimed). #111 aggregate wave
    in flight.
  - 2026-08-01 #106 review: headline diagnoses CONFIRMED bit-exact on
    real MSL IR, verdict reject-with-findings — the fix made
    previously-dead code LIVE unhardened: two reachable panics
    (descending-slice overflow at :1415, zero-step unconditional
    div-by-zero — OMC rejects both cleanly), the Colon arm is a
    comment-labeled unchecked whole-overwrite (v[:]:={1,2,3,4} on
    Integer[2] folds size to 4 — the exact class the fix set out to
    kill), and unsettleable extents fall back to READABLE zero-length
    containers feeding infer_array_dimensions. Also: §12.4.4 verbatim
    mandates TOPOLOGICAL binding order (the fixed outputs-then-locals
    refuses legal acyclic programs OMC folds); element writes miss
    §10.6.13 coercion (mixed Integer/Real arrays in declared Real[n]);
    the movement claim needs re-measurement (the reviewer found
    array-touching functions do NOT fold through the CLI path — an
    entry-point gap to investigate). Fix round dispatched (F1/F2/F3
    blockers + topo sort + coercion + fold-path investigation +
    re-measurement); four residuals filed as #112 (enum-guess branch
    steering, readable uninitialized zeros vs §12.4.4's error rule,
    user-function vectorization folding DOT PRODUCTS where OMC
    vectorizes — sq({1,2,3})→14.0 vs {1,4,9}, operator ==
    overloads). The dead-code-revival lesson now has three instances
    (EF026 validation, #105 unexecuted branches, this) — hardening
    what a fix newly reaches is part of the fix.
  - 2026-08-01 ~04:55 #106 LANDED (682dc47c — "constant folds are exact
    or refused, never guessed") + 1fd8ad6c (history-policy walk
    excludes .claude/). #106 CLOSED. The fix round closed all
    blockers: signed budget-bounded slice indices (descending {3,2,1}
    OMC-exact, zero-step typed RangeError, empty-range OMC-conformant),
    Colon conformance restoring the no-whole-value invariant, extents
    refusing not zeroing (the OMC-103 shape now folds to exactly 103),
    the genuine §12.4.4 TOPOLOGICAL sort (a=b+1;b=2 -> 3), §10.6.13
    write coercion, and the fold-path mystery dissolved (same defect
    pair via the compile path — no entry-point gap); movement honestly
    re-measured (0 of 26 compile fully; 17 land on the conditional
    wall #105 already fixed — the NEXT certification may chain them).
    PROCESS INCIDENT recorded in memory: a `tail && git commit` chain
    committed on a red gate (the failure proved foreign — #111
    worktree OMC debris tripping the repo-wide scan — and the landing
    was validated 5249/5250, but the pattern is banned: read EXIT=
    before committing, in separate commands). 21 signed commits; tree
    clean at 1fd8ad6c; #111 aggregate wave remains in flight.
  - 2026-08-01 #111 complete (worktree, under review): the member-slice
    projection (ac.pin[:].v) fixed at its identity roots — the anchor
    matched the reference ROOT against scope-owned components (§10.5
    binds the subscript to the part it is written on) and coordinates
    were keyed by root-absolute chains where in-class references spell
    scope-visible suffixes (§5.3.1); component_ancestry now walks the
    occurrence graph stepping over §7.1 class occurrences, and the
    occurrence key gains per-subscript identity (materialized family
    rows vs comprehension templates collided — StateGraph
    byte-identical only with the fix). 20/20 bucket cleared: 18 ->
    ED018 sample-start (the next named wall), 2 -> BALANCED DAEs for
    the first time (Rectifier12pulse 1027/1034 matched, EL005
    downstream). Cohort compiled 139 -> 141; ED019 136 -> 116; zero
    regressions; 20/20 byte-identical sampled artifacts; DCPM_Start
    timing flake honestly attributed (2.22s isolated). The identity-
    layer thesis pays again: both defects were proxy-identity bugs of
    exactly the class the robustness assessment named. #90/#91 event-
    semantics wave in flight alongside the #111 review.
  - 2026-08-01 #90/#91 complete (worktree, under review): both event-
    semantics defects root-caused as ONE unseeded buffer and ONE wrong
    guard. #90: condition_memory started at 0 and was never seeded, so
    every condition already true at t_start read as a rising edge at
    the initial event (§8.5/§8.6 violated on both solver paths; the
    rk-like path only looked correct because it skips the boundary
    when no initial event exists — adding `when initial()` beside a
    falling `when` made it fail identically, proving one bug). Fix:
    seed_condition_memory_for_initialization evaluates every
    ConditionMemory row at t_start with initial() CLEARED (§8.3.5.1,
    §8.6: when-clauses active during initialization iff initial()),
    called once at the shared apply_projected_initial_event_boundary.
    #91: elsewhen branch guards were `cond_i and not (earlier
    LEVELS)` — a first condition staying true suppressed later
    branches forever; guard is now the branch's own condition with
    structural select-chain priority (§8.3.5.1 same-LHS rule).
    Constant conditions lose their memory row (edge degenerates to
    level) so §8.3.7 assertions in initial equations/algorithms fire
    on value, not edge. Evidence: 17-probe OMC matrix all-agree after
    (7 disagreed before, including 3 elsewhen-frozen shapes); two OMC
    falsifications re-pinned tests honestly (InitialSelfRescheduling
    count=0 forever; PersistentFirstPriority selected=2); focused
    comparator 69 models before/after at base: bands 59/6/4 BOTH
    sides, transition-diff all zeros — a correctness fix with zero
    compared-cohort movement (the 11 event_time_mismatch sampler
    channels are a clock-tick artifact, out of scope). Tests 15->27
    asserting both solver sessions; gates 5326/5328 (2 fixed_wing
    GALEC failures proven pre-existing at base, = open #102).
    Out-of-scope defect recorded: discrete `b = time < 0.5` rows that
    read time are skipped by the initialization settle (omc flips b,
    rumoca pins it). Adversarial review launched per policy.
  - 2026-08-01 #111 review verdict: ACCEPT WITH FINDINGS. Movement
    reproduced STRONGER than claimed: reviewer swept all 566 with
    both binaries — 139 -> 141 compiled (exactly AsymmetricalLoad +
    Rectifier12pulse), 0 regressions, 139/139 both-compiled models
    byte-identical dae-json, canary-20 identical in outcome AND hash;
    ancestry-walk attack probes (same-slice-two-scopes, full
    DefId-suffix collision written at root, one/two-level extends,
    shadowing-unreachability argument) all pass with OMC agreement to
    6.4e-16; Rectifier12pulse's residual EL005 proven downstream
    (unscalarized unsubscripted whole-array connector equation, out
    of contract); DCPM_Start timing flake CLEARED (5 interleaved reps
    overlap; the +23% was the reviewer's own load). Two MEDIUM
    evidence-quality defects: (1) the second-array-part rejection
    test passes with its arm DELETED (falls through to the
    dense-rectangular rejection sharing feature+span; detail elided)
    and the arm is source-unreachable (projection_pattern skips
    FieldAccess-rooted bases — legal §10.5 leaf[1].ac.pin[:].v still
    ED019 both sides); (2) SubscriptIdentity key change proven INERT
    by 566-model ablation (0 differences) and its StateGraph
    byte-identity evidence REFUTED (all 7 StateGraph.Examples fail
    ED019 on base/patched/ablation — they never compile; literal refs
    route to the Materialized branch so the claimed collision cannot
    occur). Plus: rank>=2 slices mint a certificate the rank-1
    lowering cannot realize (ED019 -> ED020, typed both ways, base
    equivalent); three contract arms only vacuously exercised
    (redeclare blocked by a pre-existing ET009 0-dims typecheck gap);
    unclaimed WIDENING found: materialized member access on inherited
    component arrays (ac.pin[2].v under extends) also fixed —
    untested. Fix round sent to the implementer: pin the rejection
    arm by detail, REVERT the inert key to rank, rank==1 acceptance
    condition, pin the widening, honesty notes; re-gate; corrected
    commit-message evidence (no StateGraph claim). #90/#91 review and
    #107 (tuple assignments, worktree at 1fd8ad6c) run alongside —
    three build lanes at 8 jobs each within the 28 budget.
  - 2026-08-01 #90/#91 review verdict: REJECT (do not land as-is);
    the #91 elsewhen half is verified correct on both backends and
    kept verbatim; the #90 seed introduces TWO HIGH regressions the
    69-model movement audit was structurally blind to. H1: the
    structural condition_is_constant guard cannot distinguish a user
    `when true` from the synthesized always-condition (identical
    structure), so a user literal-when becomes LEVEL-activated and
    runs at every event — omc y=0 forever vs ours counting to 10, and
    `when true then reinit(x,5)` refires every sample event,
    corrupting continuous state; fix = mark the synthesized condition
    explicitly, never structurally. H2: lower_vector_condition folds
    `when {c1..cn}` to ONE scalar Or with ONE buffer where §8.3.5.1
    requires one b_i per element; the seed then makes any
    permanently-true disjunct kill the when FOREVER, including its
    initial() element — silently zeroing three real MSL blocks
    (TriggeredTrapezoid `{initial(),u,not u}`, LogicalDelay
    `{u, not u}`, ContinuousSignalExtrema `{u<=x,u>=x,terminal()}`);
    omc's own falsification: `when {true, c}` fires but scalar
    `true or c` never — one lowering node cannot match both. This
    CONFIRMS open task #94's vector-OR half as a live regression
    class; the mandated per-element buffers close it. M-findings:
    seed reads uninitialized pre() slots (0.0 not start values —
    eval params must go through the pre-mode rewrite); movement
    evidence honestly relabeled "MSL-inert on the currently-
    comparable cohort" (elsewhen coverage in the 69 is ZERO; the H2
    blocks are invisible by construction; band_diff.py needs a
    self-comparison guard; top-10-truncated worst_variables counts
    are not cohort totals); neither new mechanism had a test (the
    two visibility fixtures pass vacuously). Cleared: structural
    .rev() priority, seed-sees-settled-values, four-call-site
    completeness, both surprising OMC re-pins independently
    reproduced (count=0 forever and selected 0->1->2 are genuine omc
    and MLS-consistent per §8.3.5.1 start-clause + §8.6 v=pre(v)).
    Also recorded: rk-like silently ignores equation/initial-
    equation assertions (only bdf raises; pre-existing), bdf initial-
    event triple-fire (pre-existing). Blocking 7-item fix list sent
    to the implementer; re-review required before landing.
  - 2026-08-01 #111 LANDED at 969795f1 (with style commit aeb4738a).
    Fix round verified by self-ablation: the second-array-part test
    now pins its arm by detail (deleting the arm fails the test);
    the inert SubscriptIdentity key REVERTED with the StateGraph
    claim struck (the implementer confirmed the reviewer's ablation:
    what it had compared was a truncated ED019 detail string, not
    compiled output); written-rank==1 acceptance condition added
    inside plan_projection (no new parameter — the function sits at
    the 7-arg deny threshold) making multi-dim slices a named
    abstention; the reviewer's claimed materialized-inherited
    widening could NOT be reproduced (literal-index access folds to
    a plain VarRef in index_collapse before this analysis — four
    spellings probed, compile on both binaries), but the REAL
    widening was pinned: a slice through an inherited connector
    array (PositivePlug extends Plug, the Polyphase shape), base
    ED019 -> patched {10,20,30}, der(x)=20. Landing gate: fmt was
    RED at first attempt — my own 1fd8ad6c history-policy edit was
    unformatted (both agents had flagged it; the prior landing's
    gate read the wrong exit line) — fixed as its own commit;
    clippy clean; full workspace nextest 5321/5323 with exactly the
    two fixed_wing GALEC failures pre-existing at base (= open
    #102). Also caught my second gate script omitting pipefail
    (NEXTEST_EXIT=0 from tail while the log said 2 failed +
    fail-fast) — re-ran no-fail-fast with pipefail before
    committing. Evidence in the commit: 20-model ED019 bucket
    cleared, compiled 139 -> 141, balanced 138 -> 140, ED018 58 ->
    76 (next wall), byte-identity 20/20 sampled (reviewer: 139/139),
    zero regressions, no StateGraph claim. Follow-ups filed by the
    wave: projection_pattern declines FieldAccess-rooted bases
    (legal §10.5 leaf[1].ac.pin[:].v stays ED019), StateGraph
    empty-domain templates (6 models), multi-dim member slices as
    named abstention. Certification #5 launched (results-landed-2,
    stage 10 / sim 8, reference cache preserved).
  - 2026-08-01 CERTIFICATION #5 (post-969795f1): strict-high 59/566
    (10.42%), bands near 6 / deviation 4, models_compared 69 -> 69,
    entered 0, left 0, band-changed 0, coverage-dropped 0
    (omc=a96aa1a-cmake). The #111 landing is certified regression-
    free with ZERO band movement — as predicted, its movers reach
    balanced DAEs but not yet simulation. Compile-side movement
    confirmed by the cert snapshot: Flatten 376, DAE pass 142,
    Balanced 141, IR-Solve 133. The run exits 1 BY DESIGN: the
    quality gate's hard floor demands >=85 strict-high (the 15%
    ratchet milestone) and its per-stage floors compare against the
    checked-in pre-cutover baseline (Compile 545/566, strict-high
    143 from the permissive pipeline this branch deleted) — the
    parity/transition lines are the certification substance, and
    they are green. Score holds at 59; next milestone remains 85.
  - 2026-08-01 #90/#91 fix round complete (worktree, under
    RE-review): all seven blocking items addressed. H2 fixed
    properly — vector when lowers to a new nested AnyRise condition
    node with PER-ELEMENT activation buffers (§8.3.5.1 one b_i per
    element), trigger = edge(b1) or .. or edge(bn); GALEC refuses
    vector activation in DoStep clocks (no per-element edge there).
    H1 fixed structurally — ConditionOperation::Always replaces the
    literal-true synthesized condition and condition_is_constant is
    DELETED, so a user `when <literal>` keeps its buffer and never
    fires. M3: seed evaluates under pre(v)=v via the pre-mode
    rewrite. Review-probe mismatches 14 -> 7, every remainder a
    recorded divergence; the three silently-dead MSL blocks
    (TriggeredTrapezoid, LogicalDelay, ContinuousSignalExtrema) go
    dead -> EXACT against omc on all but 3 attributed channels
    (within-event pre() ordering; the dynamic-threshold reschedule
    gap = #98 class, UNMASKED not introduced). One honest
    introduced-and-recorded regression: P15 in-chain coincident
    crossings on rk-like (scheduled-instant vs located-crossing
    iteration split, #97 class). Band re-run at base: 59/6/4 both
    sides, 0 movement, "MSL-inert on the currently-comparable
    cohort, nothing more" — the H2 parity evidence is the OMC
    block-shape matrix, not the band table. Gates: clippy 0,
    974/974 targeted, 5334/5336 workspace (only #102 pre-existing).
    Re-review dispatched to the original reviewer per policy.
  - 2026-08-01 #107 complete (worktree at 1fd8ad6c, under review):
    §11.2.1.1 multi-result call statements construct in function
    bodies (operands lowered once, one call(function, ordinal) per
    read result; four spellings OMC-exact, max|err| 4.44e-16); the
    §10.4.2 matrix-literal rank defect fixed — the DAE shape prover
    DISCARDED is_matrix, so [0,1,1,0,0] proved rank 1, which was the
    real mechanism behind every "declared rank 2 vs call-site rank
    1" arity report; partial application and the Tuple/FieldAccess/
    Empty catch-alls get typed named rejections. Zero regressions
    across 566 (identical per-model failure sets); one REAL
    regression self-caught mid-flight (TransformerTestbench — two
    producers write is_matrix under different row conventions; the
    class is now a review attack line). Honest negatives recorded:
    NO bucket model crosses a phase boundary (each masks a second
    independent gap — external-object types, loop/conditional
    definedness, symbolically-indexed record arrays = next census
    targets); parse IR proven LOSSY ([[1,2],[3,4]] vs [1,2;3,4]
    byte-identical nodes, OMC 1x4 vs 2x2) — documented in-code.
    Adversarial review launched with priorities: impure/expensive
    call multi-evaluation vs §12.4.3 evaluate-once, lossy-IR silent
    wrong shapes, the is_matrix two-convention census, rejection
    pin-by-detail ablations, zero-regression provenance.
  - 2026-08-01 #90/#91 RE-REVIEW verdict: ACCEPT-WITH-FIXES,
    non-blocking — "Land it." All seven round-1 items verified
    genuinely fixed; 47 probe comparisons vs omc, rk-like
    mismatches 25 -> 7 (residuals: 4 recorded scalar-initial()
    family, P15, 2 within-event-ordering); V09 is the decisive
    vector probe (a falling second element's own edge tracked);
    determinism verified (byte-identical artifacts across runs,
    element-order-independent); exhaustiveness sweep over all 16
    ConditionOperation/Node/Input match sites CLEAN, zero wildcard
    arms; the discriminating seed unit test confirmed real by
    fixture inspection. P15 judgment settled by a 2x2 over crossing
    kind x coincidence: failure only in the mixed scheduled+located
    coincident cell — a solver-scheduling defect (#97 class) the old
    code masked by being wrong more broadly; "not one regression for
    three blocks: a semantics provably right across 47 probes vs one
    recorded scheduling artifact." Reviewer self-corrections (its
    round-1 exposure claim): the three MSL blocks are NOT reachable
    today (TriggeredTrapezoid ED018 initial()-in-expr;
    ContinuousSignalExtrema ED018 terminal() + ED019 pre(u);
    LogicalDelay compiles but projection fails to converge both
    builds) — benefit is prospective; and the addendum corrected its
    own M6: 61/69 cohort models DO reach the seeding boundary, so
    the 0-movement run (69/69 sha256-identical sim-traces) is a real
    no-regression measurement for the seeding half; 0/69 reach the
    vector path (probe matrix is that half's sole evidence). New
    findings: N1 DAE_SCHEMA_VERSION not bumped though Always was
    inserted at ordinal 1 (no live bincode path, but the invariant
    is false as written); N2 no wire round-trip test for the new
    variants; N3 doc overclaim; N4/N5 pre-existing §8.3.5.1
    same-body-reference violation (V12: omc b=100, rumoca b=-100)
    -> board #114; N6 n-1 dead memory rows per vector, N7 GALEC
    refusal rides trigger==guard -> board #115. Pre-landing polish
    round (N1-N5 records/docs/test) dispatched to the implementer;
    landing follows its return.
  - 2026-08-01 #107 review verdict: ACCEPT-WITH-FIXES. No introduced
    wrong answer; every measured delta moves toward OMC or from
    silently-wrong to typed rejection (base SILENTLY TRANSPOSED
    [v1,v2] vector rows: y=2 vs omc 3.0 — now rejected). The claim-2
    causal chain reproduced end-to-end on real MSL with a
    base-archive binary: LossyGearDemo1 "M2 declared rank 2" ->
    "function value type ExternalCombiTable1D", exactly the claimed
    unmasking; TransformerTestbench genuinely fixed. One semantic
    hole found (F1): multi-result statements never consult purity,
    so `(p,q) := ext(u)` on an external impure function mints TWO
    call nodes — two invocations where §12.4.3 gives ONE evaluation;
    base rejected the form so the wave newly admits it; fix =
    refuse external-and-impure multi-result statements by name.
    Also: N-times body re-lowering per read result (cost, not
    correctness — doc must say operands-only sharing); the
    is_scalar_operand_row predicate tests syntax not scalar-ness
    and its ACCEPTANCE CONTRACT is false as written; the second
    is_matrix producer is MISATTRIBUTED (real root:
    array_comprehension.rs:145 marks { } comprehensions as [ ]
    matrices -> board #116 with the three-convention census across
    five consumers); only 4/18 new rejection messages test-pinned;
    zero-regression claim verified at band/exit_reason granularity
    (566/566 identical) with 14 intended exit_detail deepenings;
    lossy-IR [[..]] finding pre-existing, zero MSL code exposure.
    10-item fix round dispatched to the implementer; landing after
    its return + re-gate.
  - 2026-08-01 #113 ED018 wave complete: HONEST NEGATIVE that
    re-prioritizes the queue. The premise failed investigation:
    bucket is 52 models (not 18), all reducing to TWO MSL source
    sites (Blocks.Math.Mean/SignalExtrema: parameter t0(fixed=false)
    with initial equation t0 = time and when sample(t0+1/f, 1/f));
    root cause is NEITHER identity nor literal-only plans — t0's
    value IS the simulation start instant, a runtime option, while
    the clock lattice phase is an absolute translation-time
    rational. OMC's own compiled model shifts its whole grid with
    t_start (P2 t_start=0 ticks 0.25.. vs P3 t_start=2 ticks
    2.25..), so folding t0 at construction would be exactly the
    silent default SPEC 0036 forbids: the correct fix is an
    ARCHITECTURAL start-relative periodic schedule (anchor flag +
    t_start threaded through timeline.rs, ~11 files/35 phase sites).
    AND the bucket is masked: a throwaway probe binding deferred
    params to 0 showed 7 of 8 owner families then hit ED019
    pre-on-continuous at the very next line (Blocks/Math.mo:2272
    f*pre(x), x continuous) — fixing ED018 alone moves at most 1 of
    52. Delivered instead: zero-behavior-change typed diagnostics
    (DeferredParameterSource StartInstant/InitializationSystem; the
    old "unknown variable: t0" was never an identity failure — the
    name resolved, only its number was absent), 3 ablation-checked
    tests, 21/21 byte-identity, gates 5324/5326 (only #102). Under
    review before landing. QUEUE RE-PRIORITIZED: ED019
    pre-on-continuous launched as the new primary wave (#117, ~108
    models first-error + gates the 52; genuine band-movement
    candidate since Mean-family blocks are widely instantiated).
    Process lessons persisted to memory: worktree isolation bases
    on the repo DEFAULT branch (282 commits behind — reset to the
    campaign base explicitly), and omc writes into CWD (cd() inside
    .mos or scan gates trip on worktree litter).
  - 2026-08-01 #90/#91 LANDED at 4032af2a (24 files, +1817/-204,
    DAE_SCHEMA_VERSION 13 -> 14). Polish round applied all four
    pre-landing items: schema bump with the ordinal-shift rationale
    in-doc; wire round-trip test covering Always, AnyRise AND the
    shifted neighbours (a single-variant round trip cannot catch a
    mis-shifted table); MSL-block claims softened to "written in
    the shape, none compiles today, becomes live when initial()-in-
    expression/terminal()/pre-on-continuous land"; the §8.3.5.1
    same-body read recorded as SPEC VIOLATION with V12 verbatim and
    the ContinuousSignalExtrema channels re-attributed to it.
    Epistemic note worth keeping: the implementer's own attempt to
    re-derive the 61/69 seeding-reach number FAILED (its harness
    could not load ModelicaServices) and it DISCARDED the artifact
    rather than report it, attributing the number to the reviewer —
    exactly the standard. Landing gate on main: fmt 0 (aeb4738a
    covers the history-policy file), clippy 0, nextest 5342/5344
    (only #102). Board: #90/#91 closed; #94 narrowed to its
    falling-relation half (the vector half is closed by AnyRise).
    Certification #6 launched.
  - 2026-08-01 CERTIFICATION #6 (post-4032af2a): strict-high 59/566
    (10.42%), near 6 / deviation 4, compared 69 -> 69, entered 0,
    left 0, band-changed 0, coverage-dropped 0 (omc=a96aa1a-cmake).
    The #90/#91 event-semantics landing certified regression-free —
    the byte-identity prediction held through certification. Exit 1
    remains the designed >=85 ratchet floor. Score 59; the queue's
    band-movement hopes ride on #117 (ED019 pre-on-continuous) and
    the eventual ED018 start-relative schedule feature.
  - 2026-08-01 post-landing addendum from the #90/#91 reviewer: the
    band_diff.py it critiqued is agent SCRATCH — the landed diff is
    verified clean (21+1 .rs files, zero .py). But the repo already
    ships `cargo xtask repo msl -- transition-diff`
    (msl_tools/transition_diff.rs), spec-governed per SPEC 0008,
    whose own header forbids exactly the fail-open the Python had
    ("no models left the cohort" and "we could not tell" must not
    be spelled the same way) — and the implementer ran the Rust
    tool in round 1, then hand-rolled the weaker Python for round 2
    anyway. Second scratch-tooling-weaker-than-shipped instance in
    one review (truncated worst_variables was the first). Reviewer
    retracted its own item-9 prescription (fix the .py) in favor
    of: evidence through transition-diff BY NAME, missing
    capabilities added to the Rust tool, harness-native per-model
    trace digests as primary inertness evidence. Verdict and band
    substance unaffected (the 69/69 sha256 identity is
    tool-independent). Memory and board #115 updated accordingly.
  - 2026-08-01 #107 LANDED (12 files, +1457/-44, three-way merge
    over the construction.rs overlap with 4032af2a applied
    cleanly). Fix round had applied all 10 review items with two
    honest self-corrections: the F3 check was INERT where the
    review prescribed it (the model-scope validator runs with
    values: None — its own fixture proved it by constructing at
    rank 3) and moved to ShapeAnalyzer::discover_calls; and a
    BandTableMeta site behind the msl-full-test feature gate broke
    the MSL build while clippy --all-targets passed clean (->
    board #118: add the feature to the lint lane). Landed
    substance: §11.2.1.1 multi-result statements (operands lowered
    once, per-read-result call nodes, §12.4.3 impure-external
    refusal BY NAME bounding the documented k-evaluations cost),
    the is_matrix rank drop fixed (the silent [v1,v2] transpose is
    now a named §10.4.2.1 cat-promotion refusal), §12.4.2.1
    partial application wired into both resolution paths, explicit
    Empty/Tuple arms, band tables stamped with a working-tree
    content digest (F11 provenance). Zero regressions, 12 bucket
    models honestly recorded as double-masked (parity unmeasured).
    Landing gate: fmt 0, clippy 0, nextest 5355/5357 (only #102).
    Board: #107 closed; #119 (declarative buffer starts) and #120
    (DECISION James: MLS-conformance vs OMC-parity pinning) filed
    from the reviewer's trajectory assessment. Certification #7
    launched.
  - 2026-08-01 CERTIFICATION #7 (post-#107 landing): strict-high
    59/566 (10.42%), near 6 / deviation 4, compared 69 -> 69,
    entered 0, left 0, band-changed 0, coverage-dropped 0
    (omc=a96aa1a-cmake). #107 certified regression-free as
    predicted (its bucket stays in ToDae behind second gaps).
    Session tally: 26 landed commits, 7 certifications, score arc
    54 -> 56 -> 58 -> 59 held with zero unexplained movement.
    Remaining in-flight: ED018-diff review, #117 ED019 wave (the
    live band-movement candidate).
  - 2026-08-01 James directives (direct message): (1) rumoca-
    contracts keeps a FORMAL-STATEMENT REGISTRY — spec-sourced
    statements (MLS-cited) vs statements NOT in the spec implied by
    OMC runs, as distinct tiers, so oracle pins are re-litigable
    (board #121, seeding wave launched at light budget; #120
    narrowed to the genuine-conflict authority question); (2) the
    diffsol and rk backends should eventually run through the SAME
    FMI v3 Model Exchange layer, which should collapse many of
    their recorded divergences (board #122; motivating evidence:
    the ~13-probe backend split, diffsol initial-event multi-fire,
    P15 scheduling class). Both persisted to memory.
  - 2026-08-01 #113 diagnostics-diff review verdict: ACCEPT (2
    optional LOW findings). Zero-behavior-change verified THREE
    ways: static consumer census (exactly two variant-sensitive
    EvalError consumers, both extended), 17/17 probe differentials
    rc+code identical, 34/34 MSL artifacts byte-identical with
    stderr changing ONLY on the 3 sampled t0-owners' ED018 detail;
    byte-identity re-run independently (git-archive base, own
    target dir, marker strings); BOTH short-circuit arms proven
    independently load-bearing by ablation ("the highest-risk item
    and the wave got it right"); both test ablations reproduced
    exactly; OMC P2/P3 grid-shift independently reproduced.
    Findings: F1 the classification prose overclaims what the
    syntactic Sub-residual rule proves (three reachable shapes —
    indirect, initial algorithm, non-Sub residual — get the
    true-but-weaker label); F2 deferred-DEPENDENT bindings still
    say "unknown variable" (real MSL: ContinuousMean
    actualStartTime = max(t_0, startTime)) — unacknowledged gap,
    not a regression. Prose round sent to the implementer (narrow
    the prose, record F2 as known-remaining, fix the ablation-A
    count); landing follows.
  - 2026-08-01 #117 ED019 wave complete (worktree, under review):
    PREMISE CORRECTED — pre-on-continuous has ZERO first-error
    models; the 108-model ED019 census splits into ~19 OTHER
    families (37 unsupported-type, 12 clocked §16.5.2, 9 impure-
    call, 9 record-field, 6 unowned statements, ...), and the pre
    wall lives entirely BEHIND ED018 (52 sample-start models, 51 of
    them Mean instances; Blocks/Math.mo:2272 is the only continuous
    pre() site in MSL 4.1.0). The wave implemented it anyway with
    an OMC-derived contract that corrected its own initial design:
    accept pre(continuous) in when-BODIES only as the frozen left
    limit at event entry (before same-body reinit — reads 1.0 not
    0.0; pre(a)=1 while live a=11 proves aliasing illegal; frozen
    across 3 iteration passes), REJECT in when-conditions (OMC
    rejects), plain equations, functions. New PreState/PreAlgebraic
    coordinates reporting Discrete variability; pre lanes minted
    only-if-read (layout preserved); no incidence; index reduction
    carries PreState -> PreAlgebraic. Decisive ablation: Mean's
    verbatim body with the ED018 mask lifted goes base-exact-ED019
    -> Success/balanced/simulates at OMC's exact value (y=2.0).
    Zero regressions 566/566 including error TEXT; byte identity
    42/42 + 30/30 Solve-stage including bit-identical sim traces;
    NO band claim (parity honestly unmeasured until ED018's
    start-relative schedule lands). Gates 5326/5328 (only #102).
    Merge flag: events.rs:1297-1298 expression_contains_pre gains
    the variants (post-4032af2a merge must preserve both). Out-of-
    scope census recorded (initial-equation pre — OMC accepts,
    rumoca typed-rejects; when-statement pre; pre(parameter);
    GALEC fallback; redundant reinit(x,pre(x)) special case).
    Review launched with the frozen-left-limit attack matrix and
    the initial-event first-tick probe.
  - 2026-08-01 #113 LANDED at 5bb5de33 (7 files, +481/-13,
    diagnostics-only): the ED018 sample-start detail now names the
    deferred-parameter source (StartInstant proven shape vs
    InitializationSystem floor) instead of "unknown variable" for a
    name that resolves fine; ContinuousMean derived-binding gap
    recorded in-module as known-remaining for the start-relative
    schedule work. Landing gate GREEN including the NEW
    msl-full-test check (#118's gap closed operationally in the
    gate script): fmt 0, clippy 0, mslcheck 0, nextest 5358/5360
    (only #102). No certification needed beyond the review's
    34/34 byte-identity — but cert #8 will fold into the next
    landing's run. James direction recorded: SPEC 0038 UNIFIED FMI
    EXECUTION (archive/deferred) is the already-written design for
    the FMI3 ME+CS convergence — anything simulating uses that one
    framework; symbolic apps (jax/casadi/sympy) use IR-projection
    templates (casadi from DAE IR). Board #122 rewritten as the
    SPEC 0038 revival; memory updated.
  - 2026-08-01 James: FMI3 consolidation EARLY ("it will save
    duplicate backend work") — #122 promoted to in_progress ahead
    of the ED018 architecture and further solver waves. Phase 1
    wave launched: revive SPEC 0038 to active with a 4-phase plan,
    define the internal FMI3 ME kernel trait over SolveModel
    (lifecycle/event-mode/discrete-state-iteration ops; the
    landed seed + event boundary become kernel-internal), migrate
    the rk-like session onto it with the private-surface census
    driven to zero and an architecture test enforcing the
    boundary; evidence bar is BIT-IDENTITY vs the pre-migration
    binary (this phase freezes behavior, recorded divergences
    included — fixes come in phase 2 when diffsol joins). Overlap
    with the pending #117 landing flagged in the mandate.
  - 2026-08-01 #121 registry seed complete (worktree, under
    review): 66 machine-checkable rows in
    rumoca-contracts/data/formal_statements.toml (51 SpecSourced —
    34 Verbatim / 17 Paraphrase; 15 OracleImplied; 48 Enforced /
    12 RecordedDivergence / 6 Unimplemented) with real-enum
    parse-don't-validate loading, anchor-based pins (Test asserts
    the fn exists; Record binds a verbatim phrase so rewording a
    divergence note forces revisiting the row), and QUOTE
    GROUNDING: a Verbatim SpecSourced quote must already appear in
    the file named by quote_source — the registry can never
    introduce an MLS quote. 18 invariant tests; three rows failed
    the guards on first run and were corrected, not the guards
    relaxed. Honest judgment list for James: the seeder CORRECTED
    the coordinator's brief on §8.5 (the tree's probe records show
    omc firing AT the instant, so freedom [SpecSourced verbatim
    "quality of implementation"] and choice [OracleImplied pinned
    to the omc table] are split rows FS-EQN-012/013); FS-SIM-007
    stale-seed read arguably SpecSourced-§8.6; semiLinear chain
    orientation has NEITHER spec nor usable oracle ("consulted and
    rejected") -> recommend a SpecSilent third tier; FS-CONN-003's
    test name endorses divergent behavior. Wave-2 material named:
    the 2026-07-22 static review's ~25 spec-vs-oracle
    adjudications. Gates: fmt/clippy/doc 0, nextest 626/626,
    contracts crate only. Review launched (guard-soundness
    ablations + row-accuracy sampling + session-record
    completeness).
  - 2026-08-01 #117 review verdict: ACCEPT-WITH-FIXES (2 must-fix,
    3 should-fix). Accept side VERIFIED across the reviewer's own
    probe matrix (frozen across clauses/iteration passes/same-body
    reinit; first tick at t_start reads v(t_start)=5 — the
    discriminating start!=0 form; arrays; noEvent). Must-fix F1
    HIGH: pre(continuous) in a CLOCKED when is NEWLY silently
    accepted and simulates where OMC hard-errors and base rejected
    — the one shape exceeding the wave's own OMC-derived contract
    (when_chains.rs:220 ignores the clocked flag it already
    computes); F2 HIGH: DAE_SCHEMA_VERSION un-bumped on a MID-ENUM
    wire insertion (Time would decode as PreState(u32)) — the SAME
    omission class the #90/#91 review caught, twice in one day.
    F3: the "only pre-on-continuous site in MSL" census claim is
    FALSE — ContinuousSignalExtrema newly accepted with zero
    coverage (its scalar-trigger reduction matches OMC exactly;
    the vector-when form goes live only after the AnyRise merge).
    F4: accept contract half-implemented (algorithm when-
    statements + continuous Input rejected where OMC accepts;
    Input gets the generic pre-wave message). F5: the
    reinit(x,pre(x)) eraser is now inconsistent — OMC r3b proves
    the frozen left limit wins INSIDE reinit values too, and the
    eraser makes reinit(x,pre(x)+1) the unsolvable reinit(x,x+1)
    (r3c: EX001 both builds, OMC simulates 1->2->3) — mandate:
    drop the eraser, lower reinit pre() through PreState, re-sweep.
    Pre-existing divergences censused: D1 live-read-after-reinit
    (event-entry vs post-reinit — #114 family), D3 duplicate-
    reinit last-wins (-> board #123), D5 GALEC safe fallback.
    MERGE NOTE recorded: take main's events.rs wholesale + the two
    expression_contains_pre lines; post-merge re-verify the Mean
    ablation (reads 1.0) and cse3 vs the OMC reference; re-run the
    30-model Solve byte-identity after #107's ordering changes.
    Fix round dispatched.
  - 2026-08-01 #92 complete (worktree, under review): the
    redeclare-dimension drop was at PARSE time — the redeclared
    name was built via ident_to_comp_ref which hardcodes
    subs: None, so decl.declaration_opt's array_subscripts never
    survived; collect_redeclarations then carried name->type only
    (its own doc confessing "array dimensions above all — is lost
    here"). Fix: parser attaches subscripts to the redeclared
    name's ComponentRefPart; instantiate collects
    CollectedRedeclarations::dims and applies after validation.
    Contract grounded in the §A.2.5 GRAMMAR (a component
    redeclaration IS a full declaration and may carry subscripts)
    + 17 OMC probes: stated dims REPLACE rank and extent (no
    error — the boarded "typed rejection for conflicts" premise
    was WRONG, OMC accepts every rank/extent change); absent dims
    keep the declared ones; dimension expressions resolve where
    the redeclaration is WRITTEN. Two honest corrections: MSL
    EXPOSURE ZERO (0 of 985 MSL redeclares carry subscripts —
    scanner with positive controls), and the 56 ET009s are a
    DIFFERENT defect (plain array declarations with parameter
    dims -> board #125, the wave-queue parameter-valued-shapes
    family, third-largest wall). Bigger defect uncovered:
    component-modifier redeclare applies NEITHER type nor dims
    (Holder h(redeclare B a) yields A's members; OMC gives B's)
    -> board #124. Evidence: extends-route probes match OMC in
    variable sets; 566-cohort counters identical, 565/566 error
    texts byte-identical (2 wall-clock flakes); transition-diff BY
    NAME all zeros; byte identity 30/30 with independent
    source/target/cache dirs. Gates 5361/5364 (the #102 pair + 1
    LSP wall-clock flake passing in isolation, untouched crate).
    7 ablation-checked contracts tests (both directions). Review
    launched (scope-resolution attack, constrainedby dims, parser
    blast radius incl. fmt round-trip, MSL-zero-scan verification).
  - 2026-08-01 LOAD-58 INCIDENT (James: "watch your load, target
    is 16"; "don't kill any existing jobs"): CARGO_BUILD_JOBS caps
    do NOT bound cohort sweeps — each sweep worker is a full
    rumoca process at 100-400%, and two lanes' fan-out phases
    stacked on two lanes' builds. Throttled by DIRECTIVE only: all
    four lanes told sweep-workers<=6, new builds 6/6, builds and
    sweeps sequential per lane; durable rules to memory (one
    cohort sweep on the box at a time; wall-clock results in a
    contention window are contention-suspect — retroactively
    explains the LSP timing flake and DCPM bucket flip).
  - 2026-08-01 #121 registry review verdict: ACCEPT-WITH-FIXES —
    and the flagship finding is a LIVE INSTANCE of the exact
    failure mode the registry exists to prevent: FS-FUNC-005's
    Verbatim quote is a MIS-TRANSCRIPTION that originated in a
    tree code comment (function_bodies.rs:454) and was laundered
    through the grounding guard (which proves origin, not
    fidelity — "B for grounding, D for attribution"). Guards
    otherwise held: 7/7 stated-invariant ablations caught BY NAME;
    all 66 pins resolve; five ceiling gaps identified (fabricated-
    at-source A8, non-#[test] pins, short anchors, consumer-file
    Site pins, format-only section check). Other must-fixes:
    FS-SIM-013 records a divergence the seeding landing FIXED
    (stale record contradicted by its own sibling row — "worse
    than no record"); FS-SIM-007 mis-tiers a §8.6 VIOLATION as
    latitude (initialization is simultaneous, x(0)=5 is REQUIRED);
    FS-EQN-009 quotes §8.3.5.4 labeled §8.3.5; FS-FUNC-006
    section swap; pin-POLARITY inversion on four divergence rows.
    D1 SpecSilent third tier ACCEPTED (FS-EXPR-007/008 mis-housed;
    "consulted and rejected" strains OracleImplied past breaking)
    — implementing this round, flagged to James as extending his
    two-tier directive. Wave-2 census: 14 missing statements incl.
    an entire EVAL category (9 rows, largest hole); FS-CLK-001
    must split (deferred t0 IS legal §3.7.5 — 52 models). Fix
    round dispatched at light budget.
  - 2026-08-01 #117 LANDED at 9f522659 (29 files, +814/-139,
    DAE_SCHEMA_VERSION 14 -> 15 with the tail-append rationale:
    decode reads the payload before checking the version, so
    appended ordinals fail cleanly where mid-enum inserts
    mis-decode). Fix round had closed the clocked-when
    over-acceptance with a typed PreContext (three contexts, three
    diagnostics), dropped the reinit pre-eraser (r3c: Newton
    divergence -> matches omc 1,2,3 with ZERO cohort movement),
    corrected the census claim, and folded the discriminating
    start=5 form into the seed test. The landing merge followed
    the reviewer's procedure (main's events.rs wholesale + the two
    expression_contains_pre lines; the schema-version conflict
    resolved to 15 with both rationales). POST-MERGE VERIFICATION:
    the AnyRise x PreState composition WORKS — cse3's vector when
    now fires (pre-merge silently dead) and y_min reads OMC-exact
    -1.0 at t=0.8; residuals attributed: t_min/t_max are the #114
    same-body-read class (their bodies read y_min after its own
    assignment), y_max misses the FIRST peak only (folded into
    #114/#94 scope; the block needs terminal() so it is not MSL-
    compilable regardless; the pinned regression test is the
    scalar-trigger form that matches OMC exactly). Gate 5366/5368
    (only #102).
  - 2026-08-01 #121 registry LANDED (68 rows, 3 tiers incl.
    SpecSilent, loader-enforced pin polarity, 26 invariant tests
    — all green against the post-#117 tree, which IS the anchor
    check). Three comment-only phase-dae corrections rode along
    where the tree originated a wrong quote or stale claim.
    #92 fix round in flight (Boolean-dimension arm + 4 doc items);
    its review verified all four no-regression legs independently
    at the final binary (30/30 byte-identity + determinism
    control). Certification #8 launched over the #117+#121 batch
    at reduced parallelism (8/6) per the load directive.
  - 2026-08-01 CERTIFICATION #8 (post-9f522659 + registry):
    strict-high 59/566 (10.42%), near 6 / deviation 4, compared
    69 -> 69, entered 0, left 0, band-changed 0. The #117+#121
    batch certified regression-free. Exit 1 = the designed 85
    floor. Score 59 across eight certifications, zero unexplained
    movement all session.
  - 2026-08-01 SPEC 0038 phase-1 agent STOPPED BY JAMES mid-gates
    (last status: bit-identity holds on the final binary; clippy +
    nextest remaining). Its worktree and diff preserved untouched;
    NOT resuming or replacing without his direction. #92 fix round
    complete meanwhile — the Boolean-arm fix done by LIFTING the
    helper to the shared AST node (Subscript::literal_dimension,
    both arms, drift incident recorded in its doc; ablation now
    fails BOTH paths together, so the drift class is structurally
    unrepeatable); [:] fallback documented against the
    declaration-path gap; def_id latent risk annotated; no-op
    write dropped. #92 3-way applied to main cleanly (zero
    conflicts); full workspace landing gate running (closes the
    fix round's honest ir-ast re-run caveat).
  - 2026-08-01 #92 LANDED at e7307f2b (5 files, +475/-56) after
    its full gate caught a LATENT #121 red: the registry loader's
    panic! tripped the architecture no-panic gate (which lives in
    the rumoca crate — my #121 gate was crate-targeted and missed
    it; the full-workspace gate is the floor for ANY landing).
    Fixed as dd5281c7 (documented-invariant expect), both gates
    re-verified, commits split properly after the 3-way apply's
    staged files merged them. James resumed the SPEC 0038 lane
    himself; phase 1 COMPLETE in-worktree: 1:1 FMI 3.0 ME op
    mapping, private-surface census 192 -> 0 with STRUCTURAL
    enforcement (Solve IR crates removed from rk45's manifest —
    compile error, not lint — plus a 20-surface architecture test
    with a performed negative control), 48/48 bit-identical traces
    with determinism controls, honest extension census
    (project_continuous_states, event_boundary left limit, FSAL,
    delay ceiling), rk45 production 2441 -> 1305 lines, gates
    5359/5361 (only #102). GOVERNANCE FLAG for James: the
    active-spec cap sat at 15/15, so the agent raised it to 16 in
    SPEC_0000 §3 + spec_budget_test rather than defer someone
    else's spec — one-line visible amendment to an ACCEPTED spec,
    keep-or-revert is his call. RESOLVED same day: James sets the
    cap to 20 ("we can increase the spec cap to 20 for now") —
    applied at the kernel landing merge in SPEC_0000 §3 +
    spec_budget_test.rs, superseding the agent's 16. Adversarial review launched with
    the FREEZE as the acceptance bar (any trace divergence =
    REJECT) + diffsol-path preservation spot-checks. #125 wave
    (56-model ET009 parameter-dims family) launched per James's
    headroom question — frontend, disjoint from the kernel
    surface. Session: 33 landed commits, 8 certifications,
    59/566 with zero unexplained movement.
  - 2026-08-01 SPEC 0038 phase-2 SCOPING complete (read-only) —
    three findings reshape the plan. (1) diffsol is THREE event
    loops, not one: L1 batch (driver.rs — whose only remaining
    consumer after phase 1 is diffsol), L2 interactive (BdfSession
    with its OWN loop), L3 zero-state; 448 private-surface lines
    (2.3x phase 1's census) + the 1049-line driver. The
    general/implicit path (OdeModel — a SECOND complete SolveModel
    projection, singular mass matrix, solver-vector integration)
    is NOT expressible in FMI 3.0 ME: decision B1 = retire it
    (recommended, pending census) vs a non-standard extension
    profile that phases 3/4 would inherit — the ONE-WAY DOOR.
    (2) D11, recorded NOWHERE: the interactive BDF session SKIPS
    §8.6 initialization entirely (settles algebraics only — no
    initial event, no delay history, no init system) — every
    scenario/LSP/wasm consumer on Auto gets it -> board #126.
    (3) The MSL band harness is BACKEND-BLIND (plot_compare
    :282-284 + Auto -> diffsol): every band number in the tree is
    a diffsol measurement, so no recorded rk-vs-bdf divergence is
    band-visible, and phase-2 fixes 8.1/8.2/8.4-8.7 WILL move
    bands while 8.3/8.8 must move zero — expectations now stated
    before any wave runs. Full divergence inventory D1-D12 with
    kernel-vs-adapter attribution (8 kernel, 4 adapter; on
    D3/D3b/D6 NEITHER side matches OMC so the shared loop needs a
    NEW adjudicated behavior); registry obligations mapped
    per-fix (Record->Test, PinsDivergence->Asserts, five NEW rows
    needed incl. D11); 8-step green-at-every-step sequence with
    the recommended split (0-1 blocking, 2-7 migration, 8 one
    wave per fix). Steps 0-1 wave LAUNCHED (path census deciding
    B1, reentrancy spike deciding B3 — safe concurrent with the
    phase-1 review per R10; one-sweep discipline enforced in the
    mandate).
  - 2026-08-01 SPEC 0038 phase-1 REVIEW verdict: ACCEPT-WITH-FIXES
    — the FREEZE HOLDS under independent measurement: 108
    artifacts byte-identical vs a rebuilt base (own harness, own
    16-model corpus incl. delay at 1e-11 tolerances, dt 1e-4
    bouncing ball, reinit/terminate/assert texts, 11 diffsol
    CSVs proving the unmigrated path bit-preserved), made
    NON-VACUOUS by controls: a one-ulp parameter change flips the
    hash. Boundary verified structurally (MeModelSource::model is
    pub(crate) — a host cannot unwrap the model; no unsafe/Any/
    transmute) with the reviewer's own negative controls, which
    ALSO found two real bypass gaps: the tests.rs FILENAME skip
    and target-specific dependency tables both evade the gate —
    fix round hardens both with re-run controls. Other fixes all
    doc-cheap: four undocumented FMI deviations (MeEventEntry
    payload, AtStateEvent, initial_observations,
    MeDiscreteStates::time), completed_integrator_step's missing
    mandated out-params, ONE dead speculative op to delete
    (next_event_time — zero readers, mislabeled), two FMI citation
    errors, the double fmi3GetFMUState mapping, spec wording vs
    the dev-dependency reality. Governance amendment verified
    exactly minimal (cap really was 15/15); James's 20 applied in
    the fix round. CRITICAL MERGE STEP mandated: rebase onto
    e7307f2b and RE-PROVE the freeze there — 9f522659's
    pre-on-continuous flows through the most relocation-sensitive
    code (event_pre/capture_pre_event_state), so bit-identity must
    be re-established on the rebased base with new pre(continuous)
    probes. Phase-2 backlog recorded (findings 7/10/12/13/14:
    zero-state trace evidence, two-times quirk doc, unphased
    normative rules incl. 9 leftover SolveRuntime::new projections
    outside the backends, the orchestration loop's mock-only
    implementor, the ban-list snapshot vs rumoca-solver's public
    re-exports). Fix round dispatched.
  - 2026-08-01 SPEC 0038 phase-2 PRE-FLIGHT complete — both
    decisions made by MEASUREMENT. B1 = B1-a (retire the general/
    implicit path): instrumented all 566 — ZERO models construct
    PreparedSimulationState::General (53 state-only + 37 no-state
    reach the backend; 90/90 builds Bdf; the harness-integrity
    check reproduced the landed phase tally exactly). The census's
    key argument is testability: the general path is a SILENT
    FALLBACK that would absorb a phase-solve regression by
    switching 53 models to a different integrator with no
    diagnostic — so retirement condition (1) is that
    can_use_state_only_bdf()==false becomes a hard, named error;
    (2) retire the path NOT the IR (implicit_rhs and
    full_jacobian_v have live non-general consumers; only the
    full-vector MassOperator dies); no schema bump needed
    (SolveProblemWire excludes artifacts). SDIRK is definitionally
    general-path-only -> deleted with the path (1 test, CLI/
    python/doc flags enumerated; silent-downgrade bug on the
    session path erased incidentally). B3 = B3-a (Rc<RefCell<
    kernel>>): decided by diffsol's Fn-closure constraint (cannot
    capture &mut — B3-b would be a second non-FMI idiom, exactly
    the drift SPEC 0038 exists to prevent), proven by a 7-test
    spike (integration through Fn closures, nested-borrow panics
    as the enforcement, host re-sync idiom matching rk45's own);
    Send was ALREADY absent in phase 1 and the only Send-bounded
    pipeline is dead code. MANDATORY companion: fix the Box::leak
    (session.rs:410) with the wiring — under B3-a it leaks a
    SolveRuntime per session and terminate() never runs. L2
    census: BdfSession PROVEN unreachable from the MSL harness
    (closed three-arm match, no session arm); LSP corrected — not
    a session consumer; D11 mechanism precise (only lib.rs call
    sites run the §8.6 boundary; session.rs never does). Backend
    pin inventory delivered incl. THREE files with inert RkLike
    pins actually running diffsol (the simulate_dae trap) and ~22
    never-compiled tests (autotests=false) -> board #127.
    SEQUENCING: B1-a is the FIRST phase-2 wave, after phase 1
    lands (both touch solver.rs). Readiness list for step 2
    cross-checked against the phase-1 review: op set stays
    extensions, getters stay &self, MeError untouched by the fix
    round — spike assumptions hold.
  - 2026-08-01 SPEC 0038 PHASE 1 LANDED at 2276c4d9 (18 files;
    spec moved out of archive/deferred; active-spec cap 20 per
    James). Fix round had: moved completed_integrator_step into
    the extension list (honest omission over fake always-false
    out-params), documented the four undocumented deviations,
    DELETED the dead next_event_time, closed BOTH enforcement
    bypasses (the module-graph rewrite after its own first
    attempt FAILED control C — the controls doing their job; all
    dependency-table shapes scanned), fixed the citations and the
    double fmi3GetFMUState mapping, and RE-PROVED the freeze on
    the rebased base: 184 artifacts byte-identical incl. 22 new
    pre(continuous) probes (P17 Mean shape with the ulp-sensitive
    witness -1 -> 5 -> 0.9999999999999999). Landing gate
    5402/5404 (only #102). James's four design pillars recorded
    to memory this window (no legacy, correct by construction,
    formal-verification-ready, TENSOR NATIVE for neural ODE/PDE
    — board #128 owns the 0-vs-310 tensor-preservation gap).
    Certification #9 launched over the #92 + phase-1 batch.
    NEXT: B1-a wave (general-path retirement) opens phase 2.
  - 2026-08-01 #125 complete (worktree at 7b4536b3, under review):
    ROOT CAUSE CORRECTS THE BOARD — the 56-model ET009 family had
    NOTHING to do with parameter-valued dimensions. Typecheck read
    declared shapes off the COMPACTION descriptor
    (InstanceComponentFamily), so any array refused compaction
    (array-valued modifiers etc. per homogeneity.rs) measured its
    elements against a SCALAR ("c has 0 dimension(s)") — the
    diagnostic reported the compiler's optimization choice, the
    exact observability SPEC_0032 §1 PROHIBITS. A literal-dim
    probe with an array modifier reproduces; same dims without it
    passes. Fix: array_extents_by_owner_def from
    InstanceOverlay::array_parent_dims (written for EVERY expanded
    array before compaction is decided), removing the LAST reader
    of component_families. Bucket 56 -> 22 (34 progressed, 0
    regressed; next errors 4 EF024 / 4 EF004 / 20 ED008 / 5
    ED019); cohort typecheck 62 -> 28, flatten passes 376 -> 402,
    compiled 142 -> 143; transition-diff all zeros, no band
    movement (CompareLineTrunks newly balanced but solve-fails —
    honestly not cited); ablation two-directional; byte identity
    20/20; OMC matrix incl. the rank in TooManySubscripts now
    matching omc's message; registry row FS-ARR-007. Its nextest
    correctly identified the #121 panic as pre-existing AT ITS
    BASE (fixed later on main as dd5281c7). Residual census ->
    board #129 (22 QuasiStatic scalar-owner member arrays — a
    different family needing the Complex chain; the kDegraded
    [:,2] flexible-dim drop). Review launched (indistinguishability
    pair-probes, the unknown-extent path, the #92 merge question,
    dead-descriptor deletion per the no-legacy pillar).
  - 2026-08-01 CERTIFICATION #9 (post-2276c4d9): strict-high
    59/566 (10.42%), near 6 / deviation 4, compared 69 -> 69,
    entered 0, left 0, band-changed 0, coverage-dropped 0. The
    #92 + SPEC 0038 phase-1 batch certified regression-free —
    the FMI3 ME kernel's freeze proof held through the full
    comparator. Nine certifications, zero unexplained movement.
    Session: 35 landed commits.
  - 2026-08-01 #128 TENSOR INVESTIGATION complete (read-only) —
    the 0-vs-310 line is BOTH measurement rot AND a total
    scalarization regression. The cutover commit b14683d1 deleted
    the metric's producer (rumoca-worker.rs:1052 hardwired
    tensor_kpi = None; tensor_report.rs deleted) AND every
    Map/AffineStencil/MatMul construction site — production
    lowering has exactly two ComputeNode producers (one
    narrow-shape LinSolve; the universal ScalarPrograms sink), so
    tensor preservation is 0.00% BY CONSTRUCTION for every model.
    A SECOND live CI tensor gate (rumoca-tensor-scaling
    --enforce, ci.yml:388) is RED on the tree right now (DAE half
    passes — families + compact domains to 2048 points — Solve
    emits 0 nodes). SPEC_0007:230 and SPEC_0032 §4 (ACCEPTED)
    violated in effect; SPEC_0039 is BLOCKED (sparsity over
    scalar rows attaches to nothing; only producer is
    ConservativeFull). The old 310/4.86% baseline was itself weak
    (95.1% scalarized pre-cutover; 310 counted report-filers, not
    tensor quality). JAX/CasADi templates IGNORE the
    native_families/native_dense_nodes the render layer already
    passes (only mlir/wgsl consume them) — a neural ODE projects
    to per-state scalar assignments; neural_ode_jax_parity.py
    never drove --target jax-solve and is broken end-to-end on
    ED019. The tests that would have caught all of this are the
    never-compiled examples_smoke orphans (#127). ED019
    family-row-overlap (structured_families.rs:63-71) kills every
    2-D nested-for discretization and is the largest ToDae bucket
    (111/234). Work program boarded: #130 N1+N8 (restore
    measurement + spec rows, S — measure first), #131 N2+N5 (the
    core: re-establish Map/AffineStencil emission + shape in
    SolveLayout, L; acceptance = the red CI gate exits 0; #116
    sequenced first), #132 N4 (lift the ED019 overlap, M —
    unlocks 2-D PDEs + the benchmark rewrite N7), #133 N6 (target
    tensor capabilities + the solve-json-vs-pipeline divergence;
    #79's dae-mo confirmed with a second mechanism, unregistered
    tojson filter). #127 updated to absorb the orphaned-tests
    find; #128 closed.
  - 2026-08-01 #125 review verdict ACCEPT-WITH-FIXES, fix round
    (metadata/prose only, each item independently re-verified by
    the implementer with probes), and LANDED at 356a6410 (4 files
    +368/-17, fast-forward merge). Review highlights:
    indistinguishability verified across NINE expanded/compacted
    pairs with identical verdicts AND text; the fix WIDER than
    claimed (member refs beneath element-expanded owners were
    poisoned too — proven with a heterogeneous-extent negative
    control); the residual 22 REFRAMED as the SAME defect via
    equation_compat.rs:262's drained-empty-vector Missing arm
    (12-line repro omc accepts; proven one-line fix direction ->
    board #129 with the component_families deletion companion);
    FS-ARR-007's section corrected §10.5.1 -> §10.6.9 (the
    reviewer's-job warning proving out AGAIN — second wrong
    section in two registry rounds); provenance lesson: band
    tables' working_tree_digest hashes the diff at DERIVATION
    time so both sides match — cite results_digest/
    trace_comparison_digest instead. Landing gate 5409/5411 (only
    #102). Cohort motion: compiled 142 -> 143, balanced 141 ->
    142, typecheck failures 62 -> 28, ET009 56 -> 22.
    Certification #10 launched. Session: 36 landed commits.
  - 2026-08-01 CERTIFICATION #10 (post-356a6410): strict-high
    59/566 (10.42%), near 6 / deviation 4, compared 69 -> 69,
    entered 0, left 0, band-changed 0. Note not-attempted moved
    432 -> 431 — the #125 landing's newly balanced model
    (CompareLineTrunks) now attempts simulation (fails solve, so
    the compared set is unchanged — exactly as predicted). Ten
    certifications, zero unexplained movement.
  - 2026-08-01 B1-a wave complete (worktree at 2276c4d9, under
    review): the general/implicit diffsol path, SDIRK, and the
    silent fallback are RETIRED (net -316 in lib.rs; DiffsolMode
    enum gone; hard error require_state_only_bdf with 5 typed
    variants naming the derivative AND coordinate — "retired, so
    this is a hard error rather than a silent switch to a
    different integrator"; the exhaustive-match break forced
    honest worker classification, SimBackendBuild/EX002). SDIRK
    fan-out fully dispositioned with one deliberate call flagged
    for review: scenario_config keeps recognizing the names so
    they route to the single rejection authority (the alternative
    silently keeps the prior solver — worse). FREEZE: 564/566
    identical (2 timing-string deltas), bands 59/6/4 identical,
    per-model metrics bit-identical. Tests: the pre-flight's 11
    exactly (the wave caught its own probe's false positive), 10
    rewritten onto state-only with completed fixtures, 1 deleted,
    82 -> 82. HONEST LIMIT recorded in FS-SIM-015 itself:
    ode.rs lost ZERO lines — BdfSession is a second independent
    general-path construction site (OdeModel + mass matrix +
    implicit Jacobian unconditionally) and the no-state path
    consumes ImplicitProjectionModel — so the full implicit-
    surface retirement belongs to the L2 session migration
    (phase-2 step 6, with D11). #129 wave (Missing-arm fix +
    component_families deletion) launched in parallel. B1-a
    review launched.
  - 2026-08-01 B1-a review verdict: ACCEPT-WITH-FIXES — the
    engineering held completely (freeze independently re-derived
    on 10 provenance-checked models, byte-identical; the same run
    doubling as the false-positive check on the hard error up to
    159 algebraics; bucket histograms identical; no silent
    downgrade on any of four traced consumer paths; all 10 test
    rewrites assertion-byte-unchanged with mathematically-correct
    fixture rows; SPEC_0038's own phasing rule — bit-identical
    traces for a move-only phase — satisfied). Blockers all
    prose/metadata: FS-SIM-015's evidence clause FALSE (diffsol
    does not project through the ME kernel yet — that is the L2
    exit criterion), its statement universally quantified while
    the session path bypasses the check, and "retired" wording
    overtaken by James's re-homing direction. James then EXTENDED
    the no-legacy pillar to the CLI mid-round: no flags/enum
    values/dropdown entries/type stubs for features that do not
    work today — the fix round REVERSED to delete all SDIRK
    surfaces (intent lives on board #134 + the registry note;
    surfaces return WITH the working host). And the architecture
    got its crisp user-facing formulation: --solver is a
    HOST-SELECTION KNOB — every method is an integrator host over
    the one ME kernel; event semantics are kernel-owned and
    identical across every choice; the flag trades accuracy/
    stiffness, never behavior (this becomes the testable
    differential property when the BDF host lands). Fix round
    dispatched with the corrected mandate.
  - 2026-08-01 FORMAL VERIFICATION TRACK OPENED (James: "start
    now before it grows too large"). The plan: verified semantic
    core + validated pipeline (CompCert-shaped, honestly scoped —
    whole-compiler proofs are not the target). Four tracks:
    typestate the ME lifecycle (illegal FMI transitions =
    compile errors; after L2/L3 lands); Kani bounded verification
    (wire round-trips — the property that would have caught both
    schema-ordinal near-misses; timeline rational arithmetic;
    seed properties; kernel lifecycle invariants); an EXECUTABLE
    REFERENCE SEMANTICS for the discrete/event core (the
    denotational-model gap — brutally simple definitional
    interpreter as the third oracle, later the Lean 4 port with
    registry rows as lemmas); the registry gains MachineChecked.
    W1 wave launched (additive only: Kani harnesses + reference
    crate slice 1 with hand-written differentials pinning the
    session's hardest-won semantics). Preconditions in flight:
    #119, #120, phase 2's one loop.
  - 2026-08-01 #129 complete (worktree at 356a6410, under
    review): MECHANISM CORRECTED vs the #125 assumption — the
    failing refs resolve from the RECORD-EXPANSION class instance
    (not the enclosing model's), dying at identity resolution
    before subscript logic; the MSL refs are synthesized
    per-element bindings. Fix: Missing merged into the abstain
    arm + the SIBLING accumulator in equation_shape.rs (same
    hazard, found by the mandated audit); 9 other consumers
    audited single-shot-clean; second cause (subscript-filter
    Missing losing the §10.6.9 name) deferred as FS-ARR-009 with
    the fix direction. ET009 22 -> 2; 5 probes FAIL->OK with OMC
    agreement, zero OK->FAIL; 19 progressed models land on the
    newly NAMED wall EF004 Connections.branch (§9.4
    overdetermined connectors, 23 models total -> board #135);
    zero regressions; byte identity 24/24; bands all zeros.
    Half 2: component_families FULLY DELETED (serde-safe,
    in-memory only) with the honest observability trade recorded
    in SPEC_0032 itself. Review launched with the DEEPER-BUG
    question leading (is abstention right, or is wrong-instance
    resolution the real defect that can silently SUCCEED
    elsewhere?) and the one-target-dir provenance wrinkle as
    load-bearing re-derivation (#109 pattern).
  - 2026-08-01 #129 review verdict: ACCEPT-WITH-FIXES — code
    change confirmed correct, minimal, monotone-safe (strictly
    Some->None; cannot introduce false rejections; 143/143 MSL +
    24/24 byte identity re-derived on separate-tree binaries,
    SEALING the provenance wrinkle). The deeper-bug question
    answered AGAINST structural safety: wrong-instance resolution
    silently SUCCEEDS with wrong answers in BOTH directions
    (probe A1 false-rejects OOB against the WRONG instance's
    extents where OMC accepts; A4 MISSES the OOB OMC names) —
    the real defect is instanced.rs:702 using the component
    owner's instance instead of binding_source_scope (computed
    two lines above with its own §7.2.4 comment) -> board #137
    HIGH, with the companion Found(None)-for-flexible-rows fix
    (the D1 redeclared-record-member-array reproducer falsifies
    the base unreachability comment; BatteryDischargeCharge +
    CCCV kDegraded[:,2] survive via Found(Some([]))). Also: the
    B1/B2 shapes drop the binding equation FROM THE DAE (not
    merely unnamed — ED001 balance noise); compaction now has NO
    liveness witness (SPEC_0032's new PROHIBITED clause needs
    Instance-IR scoping so a non-IR counter stays legal);
    FS-ARR-009 pins the enforced case leaving its own gap
    witness-less. 8-item light fix round dispatched (docs/
    registry/prose + D1 reproducer + B2 divergence pin). The
    quality-gate magnitude note stands: the branch baseline gap
    (compiled 545 -> 143) is pre-cutover drift, reargued from
    magnitude not the non-discriminating git_commit stamp.
  - 2026-08-01 B1-a LANDED at 8f33cea0 (34 files, +820/-740)
    after its fix round under the corrected no-legacy-CLI
    mandate: all SDIRK surfaces deleted (type deleted outright —
    nothing constructs it), THREE duplicate solver-name
    normalizers collapsed into one rumoca-core authority (the
    architecture gate itself forced the facade route — the fences
    working), anti-downgrade widened to ANY unrunnable solver
    name (audited harmless in-repo), FS-SIM-015 retiered
    SpecSilent with the scoped statement. Landing gate 5412/5415
    (the #102 pair + the LSP timing flake passing in isolation
    30.2s). #129 LANDED at 725d3500 (14 files, +527/-262): the
    Missing-arm abstention with MEASURED provenance (the fix
    round instrumented and REFUTED the review's D1 attribution —
    D1 is a positive wrong answer from the wrong-instance walk,
    #137, not the extent guards, whose unreachability stands
    unfalsified), two divergence pins, the compaction descriptor
    deleted, SPEC_0032 PROHIBITED scoped to Instance IR. Gate
    5421/5423 (only #102). Day's typecheck arc COMPLETE: ET009
    62 -> 2 with the residue attributed and pinned (#137).
    W1 VERIFICATION delivered (~4460 lines additive, under
    renumber/rebase then review): dual-driver Kani+proptest
    harnesses (mutation-checked; Kani needs a devShells.verify
    flake input — not in nixpkgs), the reference interpreter
    that DISCOVERED two MLS rules by failing (§8.6 pre-identity
    misread; buffer latching at an instant's limits, FS-EQN-019),
    RkLike ≡ reference on every differential, THREE new findings
    (-> #138 serde_json 1-ULP wire drift; #139 Bdf
    self-rescheduling never fires, FS-SIM-016 after the ID
    collision with B1-a's 015 was caught and renumbered;
    MeState recorded-never-enforced -> #136 scope). SPEC_0037
    DEFERRED->DRAFT promotion PROVISIONAL pending James.
  - 2026-08-01 CERTIFICATION #11 (post-725d3500): strict-high
    59/566 (10.42%), near 6 / deviation 4, compared 69 -> 69,
    entered 0, left 0, band-changed 0. The B1-a + #129 batch
    certified regression-free. Eleven certifications, zero
    unexplained movement, 38 landed commits this session.
  - 2026-08-01 PHASE-2 MIGRATION WAVE launched (steps 2-7: diffsol
    onto the ME kernel — trait additions with the B5 failure-stage
    gate, adapter skeleton with the MeRootProfile shim, L3
    replacement with the apply_without_initial_event
    reconciliation as a marked C step, the L2 rebuild fixing D11,
    driver.rs + private-path deletion with the extended boundary
    ban; B3-a wiring with the leak fix mandated). W1 rebase
    complete: both Bdf divergence pins held BYTE-IDENTICALLY
    post-B1-a (no accidental fix; fail-by-design discipline
    demonstrated); gates green on the rebased tree.
  - 2026-08-01 W1 REVIEW verdict: ACCEPT-WITH-FIXES, F1 HIGH —
    the reference DISAGREES WITH THE COMPILER AND THE COMPILER IS
    RIGHT on state-condition cascades (P1: when x>2 driven by
    another when's write — Appendix B's event iteration re-solves
    within the instant to y=1; the reference's UNIVERSAL buffer
    latching holds the entering value and y is wrong FOREVER; the
    latching is genuinely needed for the unsatisfiable
    self-rescheduling case — verified — but is broader than §8.5
    supports). The crate's own "this is the specification" header
    would have sent a reader to break a correct compiler; the
    differential harness had a coverage ILLUSION (state
    conditions present but never crossing). F2: FS-EQN-019
    re-tier (SpecSilent shape). Also: the wire-universe lists are
    hand-maintained (reviewer added a variant + demanded arms,
    round-trip stayed green never constructing it); a strictly
    better lint fix verified empirically (workspace check-cfg one-
    liner vs the blanket allows); the Lean obligation-4 witness
    REFUTED (converges under latching); next_up() load-bearing
    and unpinned; the op-alphabet count unpinned. What HELD:
    simultaneity real and order-independent (the reference
    correctly implements §8.3.5.1's simultaneous equations),
    §8.6-via-Appendix-B, the pre-seeding reading matching the
    landed compiler seed independently, differential discipline,
    FS-SIM-016's tier refusal, SPEC_0037 edit minimality, both
    mutation claims + two of the reviewer's own. MERGE ORDER: W1
    lands BEFORE the migration wave (cannot destabilize it; the
    migration will move W1's fixture-bound fields). Fix round
    dispatched.
  - 2026-08-01 PHASE-2 MIGRATION STOPPED AFTER STEP 2 — the
    correct stop, and the stop is the finding. D-B: THE KERNEL
    VIOLATES §8.6 — exit_initialization_mode_inner latches
    pending_event_pre_y BEFORE settle_initialization_system (the
    adjacent comment claims the opposite); `when initial() then
    c = pre(x)` with initial equation x=5 reads c=0 through the
    kernel where diffsol correctly reads 5. Almost certainly open
    #44's mechanism, now MEASURED. Migrating as-is would have
    silently regressed initialization-sensitive models. D-A:
    event-instant sampling (diffsol two rows left+right limit vs
    kernel right-only) — O(1) trace diffs, not shimmable. Also:
    no kernel counterpart for driver.rs:945's §8.5 coincident-
    event branch; settle tolerance differs; census corrected
    448 -> 508. LANDED IN-WORKTREE (green, 5448/5450): step-2
    trait additions (directional derivative; MeStage/
    MeError::Staged preserving the bucket mapping), 7 planted
    negative controls for the boundary test, 3 divergence pins
    asserting the tree's CURRENT inconsistency, FS-SIM-017/018,
    the owned-problem leak-fix design (unsafe ruled out by the
    workspace deny). Restart order: fix D-B first (rk45-side
    trace movement IS the fix; zero band expectation), OMC-
    adjudicate D-A, then steps 3/5/6/7 against the corrected
    kernel. HANDOFF DOC WRITTEN at James's request (usage
    limit): dev/2026-08-01-campaign-handoff.md — self-contained
    for a non-Claude successor; its §4b carries this stop-state.
- [x] 2026-08-01 W1 VERIFICATION FOUNDATION — FIX ROUND COMPLETE, READY
    TO LAND (worktree agent-ab49e0420865a961b, uncommitted, based
    725d3500). Final report received before session close. F1 (the
    HIGH: reference wrong where the compiler is right on
    state-condition cascades) fixed BY NARROWING: the defect was an
    extra inner fixed-point sweep Appendix B never asks for — the
    universal-latch was only compensation for it. Corrected rule =
    the compiler's own, reached independently: seed pre(b) once per
    instant at the left limit, condition stays LIVE and is solved
    with everything else, the OUTER Appendix B loop advances pre
    memory. P1 now y=1 agreeing with both solver sessions; the
    self-rescheduling case still settles (inner systems can be
    unsatisfiable — that is WHY the outer loop exists). New
    StateConditionCascade differential. F2 re-tiered SpecSilent
    (Appendix B says "solve" without defining it for a no-solution
    system). F3 check_admissible + UnlocatedCrossing refusals,
    pinned. F4 universes derived from the total match (variant-add
    breaks the round-trip). F5 workspace check-cfg line; a hidden
    #![expect] found and deleted; cfg(kanii) warns / cfg(kani)
    silent verified. F6-F16 done; obligation-4 witness replaced
    with a true one (bare `a = not pre(a)` outside any activation).
    Five mutations verified fail-when-broken. Gates on 725d3500:
    fmt/clippy clean, registry invariants 26/26, suite_gates 14/14,
    architecture 125/125, reference 5/5+14, nextest 5452/5454
    (only the pre-existing #102 pair). Diff 31 files +5227/−8,
    TWO production lines (mod registrations). SPEC_0037 stays
    provisional pending James. COLLISION: W1's FS-SIM-017 is also
    claimed by the phase-2 worktree — whichever lands second
    renumbers. Merge order per review: W1 lands FIRST, then
    certification #12, then the phase-2 restart (handoff §4b).
- [x] 2026-08-01 W1 LANDED at e62afc32 (feat(verify): seed the
    formal-verification foundation). 3-way apply from the worktree onto
    clean 725d3500, all 31 files clean, registry 75 rows zero duplicate
    IDs. Gate: FMT/CLIPPY/MSLCHECK all 0; nextest FULL suite 5454 run /
    5452 passed / 2 failed = the pre-existing #102 fixed-wing GALEC
    pair; NEXTEST_EXIT=100 honest. GATE INCIDENT (caught, re-run): the
    first nextest invocation piped through grep — the pipeline exit
    masked nextest's failure as EXIT=0 AND the run was cancelled
    fail-fast at 553/5454. A cancelled run is not a gate. Re-ran with
    --no-fail-fast and the exit captured unpiped. This is the SAME
    pipe-masking trap recorded earlier this campaign — the rule is now
    twice-earned: nextest exit goes to the log file directly, never
    through a pipe, and --no-fail-fast is part of the standard gate
    command. REGISTRY: FS-SIM maximum on the branch is now 017; the
    phase-2 worktree's FS-SIM-017/018 rows must renumber (017→next
    free) before its landing gate. Certification #12 launched after
    the landing; expectation: zero band movement (two production
    lines, both mod registrations — no behavior change).
- [x] 2026-08-01 CERTIFICATION #12 (post-e62afc32): strict-high
    58/566 (10.25%), near 6 / deviation 4, compared 69 -> 68,
    entered 0, left 1, band-changed 0, coverage-dropped 0. The sole
    named departure was Modelica.Electrical.Machines.Examples.DCMachines.DCPM_Start,
    whose one mandated attempt exceeded the 10-second Sim phase budget
    (10.503s). This is the already-tracked #104 timeout-sensitive model,
    not a changed trace: W1 changed only two production `mod` declarations,
    and no compared model changed bands. Per the no-retry rule the run was
    not retried or promoted; its honest score remains 58. Exit 1 also remains
    the designed >=85 ratchet/baseline failure. Evidence:
    `/home/jgoppert/.claude/jobs/244ea9ad/tmp/cert12.log` and the persisted
    e62afc32 band table in `target/msl/results-landed-2`.
- [x] 2026-08-01 D-B §8.6 FIX — WORKTREE TIER 1 COMPLETE
    (`.codex/worktrees/db_init_pre`, based e62afc32, uncommitted).
    Concrete negative control: a state declared `x(start=0,fixed=false)`
    and settled by `initial equation x=5`, then sampled by
    `when initial() then seenPre=pre(x)`, produced Bdf `seenPre=5` but
    RkLike/ME-kernel `seenPre=0`. The first divergent operation was
    `SolveMeKernel::exit_initialization_mode`: it saved
    `pending_event_pre_y/p` before `settle_initialization_system` while
    its comment claimed the opposite. The fix moves both snapshots after
    settle, algebraic projection, state copy, and relation-memory update,
    implementing MLS 3.6 §8.6 / EQN-035 (`v = pre(v)` before integration).
    FS-EQN-020 pins both hosts to the settled value. Negative control failed
    exactly on RkLike before the production edit; afterward the focused
    dual-host test passed, initialization_ordering 8/8, rumoca-solver
    201/201, event activation 32/32, and formal-statement invariants 26/26.
    Fixed 20-model Tier-1 canary (one attempt, default 10-second budgets,
    `target/msl/canary-db-init-pre`) remained on the existing red baseline:
    20 common, 8 compiled, 6 sim_ok, 2 solver failures, 12 ToDae failures,
    zero timeouts. The repository `transition-diff` against the identical
    `target/msl/task64-canary2` list reported sim gains 0, sim regressions 0,
    high gains 0, high regressions 0. Parity is explicitly UNMEASURED because
    selected failures prevented the comparator; this is Tier 1 tripwire
    evidence, not a cohort parity number. Next: adversarial review, landing
    gate, signed commit, then a named full-cohort certification.
- [x] 2026-08-01 D-B LANDED at c494d2af (`fix(solver): seed initial
    pre values after initialization settles`). Three-way apply onto clean
    e62afc32; three intended tracked files, +85/-5. Adversarial path review
    confirmed both hosts now snapshot after the same initialization,
    algebraic, and relation-memory settle, and the ME kernel consumes the
    startup snapshots exactly once. Landing gate: fmt 0, workspace
    all-target/all-feature clippy 0, msl-full-test check 0, full unpiped
    nextest 5455 run / 5453 passed / 2 failed = only the pre-existing #102
    fixed-wing GALEC pair; NEXTEST_EXIT=100 honest. Signed commit complete.
    Full-cohort certification launched on c494d2af with zero band-movement
    expectation: the measured side uses the BDF host whose ordering was already
    correct, while this fix changes only the RK/FMI-ME path.
- [x] 2026-08-01 CERTIFICATION #13 (post-c494d2af): strict-high
    58/566 (10.25%), near 6 / deviation 4, compared 68 -> 68,
    entered 0, left 0, band-changed 0, coverage-dropped 0. This is the
    predicted zero-movement result: D-B changed only the RK/FMI-ME host,
    while the full-cohort measurement uses the already-correct BDF host.
    The two honest one-attempt Rumoca simulation timeouts were the known
    #104-sensitive
    `Modelica.Electrical.Machines.Examples.DCMachines.DCPM_Start`
    (10.494s) and
    `Modelica.Thermal.FluidHeatFlow.Examples.TwoTanks` (10.303s); neither
    reduced comparator coverage. The freshly generated OMC reference was
    525 successful / 36 failed / 5 timed out over all 566 targets. Exit 101
    remains the designed baseline and >=85 strict-high ratchet failure, not
    a D-B regression. Machine-readable evidence is retained in
    `target/msl/results-landed-3` with both predecessor/current band tables;
    after certification its reproducible `omc_sim_work` and `sim_traces`
    bulk subtrees were swept per campaign policy, freeing 112 GiB.
- [x] 2026-08-01 SPEC_0038 PHASE-2 D-A ADJUDICATED. The isolated
    `ScheduledStep` probe (`Vs = if time > 0.5 then 24 else 0`, 0.01 output
    grid) produced omc CSV rows `t=0.5, Vs=0`, then an event pair at
    `t=0.5000000000000306` with `Vs=0` and `Vs=24`. The first row is the
    ordinary aligned grid sample and the few-ULP displacement is omc's event
    location, but the same-time left/right pair decisively selects two-sided
    event-boundary observability. Phase 2 must preserve the diffsol-style
    left+right contract, not inherit the ME kernel host's current right-only
    convention. FS-SIM-018 records the measured oracle decision and remains a
    divergence pin until the shared event loop lands.
- [x] 2026-08-01 SPEC_0038 PHASE-2 STEP 2 LANDED at 671d49a7
    (`feat(solver): extend the model exchange host contract`). Rebased the
    preserved step-2 work onto c494d2af while retaining D-B's post-settlement
    initial-pre ordering. Adds FMI-faithful directional derivatives, producer-
    owned lifecycle staging on `MeError`, 13 component tests, the D-A
    divergence/trajectory pins, and a non-vacuous rk-like ME boundary guard.
    Adversarial review found and fixed three MEDIUM test holes: the D-A pin now
    proves finite `[0, 24]` left/right observations and rejects non-finite
    trajectories; the boundary guard derives private names from every runtime
    re-export; and dependency checks resolve inline, sub-table, and workspace-
    inherited package renames. Final independent recheck: no HIGH/MEDIUM.
    Worktree gates: solver 214/214, registry 26/26, D-A 2/2, architecture
    134/134, focused clippy clean, msl-full-test check clean. Main landing gate:
    fmt 0, workspace all-target/all-feature clippy 0, msl-full-test check 0,
    full unpiped nextest 5480 run / 5478 passed / 2 failed = only pre-existing
    #102 fixed-wing GALEC; signed commit complete. Certification #14 follows
    with zero band-movement expectation because the measured BDF host does not
    yet consume the new ME operations.
- [x] 2026-08-01 CERTIFICATION #14 (post-671d49a7): strict-high
    59/566 (10.42%), near 6 / deviation 4, compared 68 -> 69,
    entered 1, left 0, band-changed 0, coverage-dropped 0. The only cohort
    transition was
    `Modelica.Electrical.Machines.Examples.DCMachines.DCPM_Start` from absent
    to high: this one-attempt run completed it in 2.240s after certification
    #13's timeout, so the result is a timing-sensitive coverage gain rather
    than semantic movement from the phase-2 trait additions. The two honest
    Rumoca model-worker timeouts were
    `Modelica.Electrical.Spice3.Examples.Spice3BenchmarkFourBitBinaryAdder`
    in Typecheck (12.749s) and
    `Modelica.Thermal.FluidHeatFlow.Examples.TwoTanks` in Sim (10.305s).
    The freshly generated OMC reference remained 525 successful / 36 failed /
    5 timed out over all 566 targets. Exit 101 remains the designed quality-
    baseline and >=85 strict-high ratchet failure, not a step-2 regression.
    Machine-readable evidence is retained in `target/msl/results-landed-4`
    with both predecessor/current band tables; its reproducible
    `omc_sim_work` and `sim_traces` bulk subtrees are swept after evidence
    capture per campaign policy.
- [ ] Continue toward the branch trace-parity goal without accepting silent
  defaults, fallback semantics, or numerically plausible incorrect traces.
- [x] 2026-08-06 INITIAL-CONDITION COMPARATOR DEFECT TRIAGED. The full
    566-model run reported discrete `off` initialization mismatches for
    `Modelica.Electrical.PowerConverters.Examples.ACDC.RectifierBridge2Pulse.DiodeBridge2Pulse`
    and
    `Modelica.Electrical.PowerConverters.Examples.ACDC.RectifierCenterTap2Pulse.DiodeCenterTap2Pulse`.
    Raw traces show the same left limit (`off = false`) at exactly `t = 0` and
    the same immediate settled right limit (`off = true`). OMC emits duplicate
    event rows near `6.13e-21`; Rumoca emits its right limit near `2.05e-11`.
    The comparator absorbed OMC's merely nearby positive-time event into the
    start-time grid before computing initialization metrics, but did not absorb
    Rumoca's later row, manufacturing a unit initial error. This is a harness
    defect, not a refinement counterexample. SPEC_0033 now requires
    initial-condition parity to use the final superdense sample at the exact
    common start time while trajectory comparison retains tolerant right-limit
    event-grid handling. Clocked-model traces confirmed why the exact
    equivalence class matters: their event rows share exactly `t = 0` and must
    settle before comparison. Model-independent focused regressions preserve
    both obligations. The focused comparator suite passed 48/48. Recomparison
    of the complete 566-model Tier 2 artifacts (125 `sim_ok`, 120 compared, 5
    tracked exclusions) retained 120/120 strict-high trajectory agreement and
    moved initialization from 82.50% accurate models with 0.67% deviation
    channels to 100.00% accurate models, zero deviation channels, and zero
    violation mass. The two originating diode models remain in the cohort.
    The harness defect is closed; actionable semantic counterexamples remain
    zero.

## Normative sources

- [SPEC 0007](../spec/SPEC_0007_IR_PIPELINE.md) —
  catalog [SPEC 0040](../spec/SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md)
- [SPEC 0008](../spec/SPEC_0008_PHASE_ERRORS.md)
- [SPEC 0029](../spec/SPEC_0029_CRATE_BOUNDARIES.md) —
  catalog [SPEC 0041](../spec/SPEC_0041_CRATE_OWNERSHIP_CATALOG.md)
- [SPEC 0034](../spec/SPEC_0034_GALEC_EFMI_EXPORT.md) —
  catalog [SPEC 0042](../spec/SPEC_0042_GALEC_LANGUAGE_CATALOG.md)
- [SPEC 0036](../spec/SPEC_0036_VALID_BY_CONSTRUCTION_IR.md) —
  catalog [SPEC 0043](../spec/SPEC_0043_CONSTRUCTION_CATALOG.md)
