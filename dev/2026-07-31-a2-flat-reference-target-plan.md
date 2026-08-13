# A2 `FlatReferenceTarget` cutover — design re-validation

Re-validated against the working tree at `22447a1d` (branch `msl-trace-parity-50`,
clean, merge-base `1acf3641`). Every `file:line` anchor below was read at that
commit.

The normative requirements stay in SPEC 0036 and its annex SPEC 0043 §7. This
document only answers: *given the 27 commits that landed after the A2 checklist
text was written, what is actually left, in what order can it be built, and what
evidence closes it.*

## 0. One-paragraph verdict

A2 is still real work, but roughly a third of what the checklist describes has
already landed under other headings — and one of its central premises is now
wrong. Exact occurrence identity exists and is minted by instantiation
(`InstanceId`); the fabricated `max(DefId) + n` namespace, `postprocess_def_id.rs`,
`structured_refs.rs`, the untagged reference wire shape and unique-suffix
constant recovery are all deleted; `instance_relations` is a real occurrence
graph; and `postprocess/occurrence_graph.rs` already performs the exact
declaration-identity walk that A2.3's "producer sequence" bullet describes as
unwritten work. What has *not* landed is everything that makes the identity
**total and unbypassable**: there is no `FlatReferenceTarget`, no
`InstanceValueId`, no branded Flat function/binder ID, no `flat::Model::construct`,
no Flat wire replay, and `flat::Model` is still a 30-field public struct with
`Default` and a derived `Deserialize`. The single most important correction is
that `rumoca_core`'s semantic tree now has exactly **one** instantiation (Flat),
so the checklist's `SemanticExpression<T>` generic buys nothing today and should
be re-decided before any worktree forks.

## 1. Method and measured baseline

### 1.1 What was read

`rumoca-core` (`ir_primitives*`, visitors/rewriters, `subscript.rs`,
`structured_domain.rs`), all of `rumoca-ir-flat`, the `rumoca-phase-flatten`
pipeline/postprocess surface, `rumoca-eval-flat`'s evaluation context,
`rumoca-phase-dae`'s Flat-reference lowering, the codegen Flat context and CLI
Flat dump, `spec/SPEC_0043_CONSTRUCTION_CATALOG.md` §7, and the xtask gate
surface.

### 1.2 Production LOC baseline (reproducible)

Convention: physical Rust lines under `src`, excluding `/tests/`, `tests.rs`,
`*_tests.rs`, `/generated/`. Reproduced with

```
git grep -c '' <rev> -- 'crates/*/src/**/*.rs' 'crates/*/src/*.rs' \
 | awk -F: '{p=$2;n=$3; if(p~/\/tests\//||p~/tests\.rs$/||p~/_tests\.rs$/||p~/\/generated\//)next; s+=n} END{print s}'
```

| Revision | Repository production lines |
|---|---|
| merge-base `1acf3641` | 438,804 |
| `6151825d` | 373,660 |
| `1db32f1e` | 387,577 |
| `22447a1d` (this tree) | **388,173** |

The first two reproduce the checklist's own recorded numbers exactly, so the
filter is the documented one. The checklist's cross-cutting LOC bullet
(lines 920-928) is therefore **stale by +11,288 lines** and must be re-measured
at cutover, not carried forward.

A2 territory at `22447a1d`, same filter:

| Crate | Production lines |
|---|---|
| `rumoca-core` | 10,851 |
| `rumoca-ir-flat` | 4,309 |
| `rumoca-phase-flatten` | 50,179 |
| `rumoca-eval-flat` | 6,283 |
| `rumoca-phase-dae` | 21,016 |
| `rumoca-phase-codegen` | 12,463 |
| `rumoca` (CLI) | 7,651 |
| **A2 total** | **112,752** |

Test population in the same crates: core 177, ir-flat 51, phase-flatten 580,
eval-flat 104, phase-dae 75, codegen 99, `rumoca` 485 — 1,571 `#[test]`
functions inside the blast radius.

### 1.3 Rendered-name dependence (the metric A2 actually moves)

`ComponentPath::from_flat_path` (parse a rendered name) and `to_flat_string`
(render one for use as a key) are the operations A2 removes from identity
paths:

| Crate | `from_flat_path` | `to_flat_string` |
|---|---|---|
| `rumoca-core` | 5 | 11 |
| `rumoca-ir-flat` | 0 | 0 |
| `rumoca-phase-flatten` | 64 | 108 |
| `rumoca-eval-flat` | 12 | 15 |
| `rumoca-phase-dae` | 0 | 0 |
| `rumoca-phase-codegen` | 0 | 0 |

`rumoca-phase-dae` is already free of rendered-name *parsing*; it is not free of
rendered-name *keying* (§2.5). `rumoca-phase-flatten` at 172 sites is where the
work is.

## 2. Workstream-by-workstream re-validation

Each section states: current owners, what the checklist assumed that has since
changed, and the remaining true gap.

### 2.1 A2.1 — generic shape and structural visitors in `rumoca-core`

**Current owners.**
`crates/rumoca-core/src/ir_primitives.rs:1465` (`pub enum Expression`, 15
variants, every variant carrying `span: Span` with a
`Span::source_free_serde_default` serde default);
`crates/rumoca-core/src/ir_primitives.rs:558` (`pub struct Reference`);
`crates/rumoca-core/src/ir_primitives/component_refs_and_functions.rs:414`
(`ForIndex`), `:426` (`Statement`), `:706` (`Function`), `:933` (`FunctionParam`);
`crates/rumoca-core/src/subscript.rs:6` (`Subscript`);
`crates/rumoca-core/src/structured_domain.rs:186` (`ComprehensionTemplate`);
`crates/rumoca-core/src/expression_visitor.rs:10` and `:195`;
`crates/rumoca-core/src/expression_rewriter.rs:6` and `:345`;
`crates/rumoca-core/src/statement_rewriter.rs:6` and `:149`;
`crates/rumoca-core/src/ir_primitives/reference_serde.rs:31`/`:87`.

**What changed since the plan text.**

1. `Reference` is no longer a bag of optional text. `reference_serde.rs` now has
   one private `ReferenceWire` with `deny_unknown_fields`, explicit `required`
   deserializers for every `Option`, and a checked `Reference::construct`
   (`reference_serde.rs:87`) that rejects the empty name and
   `InstanceId::UNSET`. The untagged compatibility shape and the bare-name
   spelling are gone. The checklist's "remove ... the untagged reference
   compatibility shape" (line 975) is **done**.
2. `Reference` already carries `instance_id: Option<InstanceId>`
   (`ir_primitives.rs:562`, accessors `:696`/`:700`).

**Remaining true gap, and the premise that is now wrong.**

None of `SemanticExpression<T>`, `SemanticSubscript<T>`, `SemanticStatement<T>`,
`SemanticFunction<T>`, `ComprehensionTemplate<T>` exists — a repo-wide grep
returns zero hits. So A2.1 as written is entirely un-started.

But the *reason* given for it no longer holds. The checklist says "AST remains
its syntax-preserving tree; Flat must not duplicate this semantic tree"
(lines 489-490), implying core's tree is shared by two or more stages. It is
not:

* `rumoca-ir-ast` owns its own `Expression` at `crates/rumoca-ir-ast/src/nodes.rs:911`.
* `rumoca-ir-galec` owns its own at `crates/rumoca-ir-galec/src/ast.rs:650`.
* `rumoca-ir-dae` uses zero `rumoca_core::Expression` (arena-based).
* `rumoca_core::Statement`/`Function` are referenced only by `rumoca-eval-flat`
  (18), `rumoca-phase-dae` (288) and `rumoca-phase-flatten` (252) — all of which
  are Flat producers or Flat consumers.

`rumoca_core`'s semantic tree therefore has **exactly one instantiation**: Flat.
A type parameter with one instantiation is pure cost: it propagates through 916
qualified `rumoca_core::Expression` uses in phase-flatten and 162 in eval-flat,
every visitor/rewriter default method, and every `impl` in three crates, and
buys no second payload.

**Decision required before any worktree forks** (this is a fork in the road, not
a detail):

* **Option A — implement as specified.** `rumoca-core` keeps the tree and gains
  `<T>`. Satisfies SPEC 0043 §7 row *"Core expression shape is generic over its
  stage-owned reference payload"* verbatim. Cost: the generic parameter is the
  single largest mechanical diff in A2 and it lands in the crate every other
  crate depends on, so it serializes the whole cutover behind one file set.
* **Option B — move the tree to `rumoca-ir-flat` with a concrete payload.**
  `rumoca-core` retains only genuinely multi-stage vocabulary (`DefId`,
  `InstanceId`, `TypeId`, `Span`/`ProvenanceSpan`, `VarName`, `ComponentPath`,
  `Subscript` shape, `StructuredIndexDomain`, `BuiltinFunction`, `Literal`,
  `Variability`, `Causality`). Flat owns `Expression`/`Statement`/`Function`
  with `FlatReferenceTarget` inlined — no type parameter anywhere. Requires
  amending the SPEC 0043 §7 row and SPEC 0029 §3a's "shared semantic IR
  vocabulary" sentence. Removes ~3,500 lines from `rumoca-core` and removes the
  generic blast radius entirely.

The checklist already anticipates spec amendment inside this same change
(line 478: *"amend SPEC 0029's accepted semantic-equality ownership in the same
change that implements the cutover"*), so Option B is procedurally in scope.
**Recommendation: Option B**, on the SPEC 0029 §3b rule that shared helpers need
one owner and the tree currently has one owner pretending to be two. Record the
decision in the plan before Stage A (§5) starts; it cannot be revisited later
because it determines whether `rumoca-core` is on the critical path.

### 2.2 A2.2 — Flat target arenas, construction, views, display, wire

**Current owners.** All of `crates/rumoca-ir-flat/src/lib.rs`.

**What changed.**

* `WhenChain` is already the structurally-nonempty owner the checklist asks for:
  private `first: WhenBranch` + ordered `else_when` + required owner span +
  allocation-free `branches()`/`branches_mut()` iterators, no empty constructor,
  no public branch vector (`crates/rumoca-ir-flat/src/when_equations.rs:6-53`).
  The derived decode requires `first`, covered by
  `crates/rumoca-ir-flat/src/tests.rs:70`. Checklist lines 423-428 are **done**.
* `instance_relations: InstanceRelationMap` exists
  (`crates/rumoca-ir-flat/src/lib.rs:211`) with `InstanceRelation { owner,
  declaration, indices, kind }` at `:222` and `InstanceKind::{Class, Aggregate,
  Materialized}` at `:215`. This is the dense occurrence graph the checklist's
  "Dense Flat target entries own source-declaration ancestry and distinct
  sibling-instance identity" bullet (line 957) asks for, minus the branding.
* `Model::validate_shape_contract` (`lib.rs:363`) already rejects an unset or
  duplicate `InstanceId` on any variable, record instance or function, with
  exact spans (`lib.rs:502-521`).

**Remaining true gaps.**

1. **No `FlatReferenceTarget` and no `InstanceValueId`.** Zero hits repo-wide.
2. **No `flat::Model::construct`.** `Model` is
   `#[derive(Debug, Clone, Default, Serialize, Deserialize)]` (`lib.rs:87`) with
   ~30 `pub` fields. Every producer assembles it field-by-field. This is the
   "direct public-field assembly bypass" of checklist line 464-465, and it is
   what makes every other A2 invariant unenforceable.
3. **Flat mints into a namespace it does not own.** `Model::materialize_instance`
   (`lib.rs:311`) allocates an `InstanceId` via `next_instance_id`
   (`lib.rs:450`), which is `max(existing keys) + 1` over the map transferred
   from instantiation. `next_function_instance_id` (`lib.rs:463`) does the same
   for `FunctionInstanceId`. So two producers (phase-instantiate and
   phase-flatten) allocate into one unbranded `u32` namespace, and Flat's
   allocator is an O(n) scan whose correctness depends on the transfer having
   already happened. Its only caller today is
   `crates/rumoca-phase-flatten/src/zero_sized_arrays.rs:40`. **This — not
   "`DefId` cannot distinguish instances" — is the live justification for a
   Flat-owned branded `InstanceValueId`.** The checklist's justification
   (lines 466-469) is stale; the replacement justification is stronger.
4. **Value identity is `VarName`-keyed.** `variables: VarNameIndexMap<Variable>`
   (`lib.rs:107`), `functions: VarNameIndexMap<Function>` (`lib.rs:155`),
   `record_instances` (`lib.rs:113`), `variable_type_names` (`:123`),
   `variable_final_flags` (`:128`). `enum_literal_ordinals: IndexMap<String, i64>`
   (`:206`) is keyed by a rendered canonical literal path. `definite_roots`,
   `branches`, `optional_edges`, `potential_roots`, `top_level_connectors`,
   `top_level_input_components` are all `String`/`IndexSet<String>`
   (`:169`-`:193`).
5. **No branded function/binder IDs in Flat.** `FunctionInstanceId`
   (`rumoca-core/src/ir_primitives.rs:99`) is a bare `pub u32` with no brand and
   no owner; domain binders do not exist as IDs at all — a `for` binder is
   `ForIndex { ident: String, range }`
   (`component_refs_and_functions.rs:414`). The reusable pattern is
   `crates/rumoca-ir-dae/src/ids.rs:6` (`branded_ids!`) and `:38`
   (`owner_local_ids!`) — copy the shape, do not copy the code.
6. **No Flat wire replay.** There is no production Flat decoder anywhere: grep
   for `serde_json::from_*` into `flat::Model` returns nothing. The only encode
   sites are `crates/rumoca/src/cli.rs:1072` (CLI `--json` dump) and
   `crates/rumoca-phase-codegen/src/codegen/mod.rs:415`
   (`Value::from_serialize(flat_model)` as the MiniJinja context). So
   "Flat wire decode replays root construction operations" (SPEC 0043 §7) is
   **new capability**, not a migration — and the current serde shape doubles as
   the template view, so making `Model` opaque breaks codegen the same day
   (§2.6).
7. **`ClockPartitions` is still an orphan.** `crates/rumoca-ir-flat/src/clocks.rs`
   is 1,421 lines, `impl ClockPartitions` at `:45`, and the only reference
   outside the module is its own re-export at `lib.rs:71`. No production code
   constructs or reads it. That is **33% of `rumoca-ir-flat`'s production
   lines** with no consumer. The checklist already carries a delete-or-replace
   item for it (lines 658-666); A2.2 is the atomic cutover that item names, so
   it must be executed in the same commit and counted in the LOC ledger.

### 2.3 A2.3 — phase-flatten reservation/resolution/emission and repair deletion

**Current owners.**
`crates/rumoca-phase-flatten/src/pipeline/flatten_pipeline.rs` (the ordering),
`pipeline/instance_identity.rs`, `postprocess.rs` + `postprocess/*`,
`constant_extraction.rs`, `pipeline/constant_injection.rs`, `name_simplify.rs`,
and the producers `ast_lower.rs` (1,955), `equations/` (6,614), `connections/`
(4,735), `functions/` (3,067), `algorithms.rs` (861), `when_equations.rs` (851),
`variables.rs` (670).

**What changed — three checklist claims are now factually wrong.**

1. **`pipeline/instance_identity.rs` is no longer what the checklist describes.**
   Checklist line 511-513 says to delete it *and* "its fabricated
   `max(source DefId) + InstanceId` namespace". That namespace
   (`InstanceIdentitySpace`) is already gone. The file today (426 lines, 220 of
   them tests) does two legitimate things:
   * `transfer_instance_relations` (`instance_identity.rs:7`) copies the
     authoritative `ast::InstanceOverlay` occurrence graph into
     `flat.instance_relations`, called once at
     `pipeline/flatten_pipeline.rs:759`;
   * `attach_reference_scope` / `attach_statement_reference_scopes` /
     `attach_when_chain_reference_scopes` (`:68`, `:85`, `:104`) stamp the
     *enclosing class-body* `InstanceId` onto every `VarRef` via
     `InstanceScopeRewriter` (`:164`-`:193`), rejecting a reference that crosses
     scopes.

   The correct instruction is **not** "delete the file" but "delete
   `attach_*_reference_scope*` and fold `transfer_instance_relations` into
   `flat::Model::construct`'s reservation step".

2. **`postprocess_def_id.rs` and `structured_refs.rs` no longer exist.** Both
   deleted by `1db32f1e`. Checklist lines 513-515 are dead text.

3. **The resolver A2.3 is told to write already exists.**
   `crates/rumoca-phase-flatten/src/postprocess/occurrence_graph.rs:50`
   (`OccurrenceGraph`) performs exactly the walk the "producer sequence" bullet
   (lines 503-510) describes: start at the class occurrence, `select_member`
   (`:117`) by exact `DefId` + element indices, descend into `extends` base-class
   occurrences up to `MAX_INHERITED_SCOPE_DEPTH` (`:23`), park on
   `PathCursor::PendingIndices` when an array declaration is selected before its
   subscripts, `apply_indices` (`:189`), and `pending_elements` (`:147`) to
   expand an MLS §10.5 array-of-components member projection into ordered
   element occurrences — rejecting (as `None`, leaving the reference untouched)
   any multidimensional, repeated, missing or out-of-range element set rather
   than guessing.

   A2.3 does not write this. A2.3 **moves** it from postprocess-over-a-finished-
   model to emission-inside-the-semantic-owner-closure, and changes its result
   type from "a rewritten `Reference` with a rendered name" to
   "an `InstanceValueId`".

**Remaining true gaps.**

1. **Resolution runs late, repeatedly, and over a whole finished model.**
   `collapse_index_refs_to_known_varrefs` (`postprocess/index_collapse.rs:18`)
   is invoked at `flatten_pipeline.rs:963` and again at `:994`; each invocation
   calls `KnownFlatVars::build` (`index_collapse.rs:195`), which constructs
   **seven** whole-model tables (`names`, `record_instances`, `by_occurrence`,
   `records_by_occurrence`, `integer_values`, `integer_values_by_occurrence`,
   plus a fresh `OccurrenceGraph`) and clones a `Reference` per variable and per
   record instance. `substitute_known_constants_in_flat`
   (`postprocess.rs:85`) runs three times (`flatten_pipeline.rs:966`, `:992`,
   `:1003`), each time rebuilding a `FxHashSet<String>` of every live variable
   name (`postprocess.rs:89`) and cloning every expression it visits
   (`postprocess.rs:99`, `:307`).
2. **A rendered-name key is still the substitution key.**
   `canonicalize_varrefs_via_record_aliases` builds
   `HashSet<String>` from `flat.variables.keys()` (`postprocess.rs:46`) and
   `record_alias_rewrite_name` (`:67`) resolves by string prefix/suffix join.
   `equation_origin_scope` (`:145`) and `parent_component_scope` (`:159`) derive
   an evaluation scope by *rendering* a path and taking its parent.
3. **Late whole-model repair passes survive under new names.**
   `postprocess_field_access.rs` (641 lines) supplies
   `normalize_record_array_field_access_bindings`,
   `drop_invalid_field_access_bindings` and
   `resolve_nested_constructor_field_access_bindings` — all three are
   whole-model scans that rewrite bindings after the fact, keyed on
   `HashSet<VarName>` of rendered names (`postprocess_field_access.rs:20`).
   `postprocess_record_alias.rs` (106) is the same family. The checklist's
   "late reference repair ... are absent" row (SPEC 0043 §7) is not satisfied by
   having deleted the two files it named by name.
4. **A deep clone of the whole model in the display pass.**
   `name_simplify::simplify_flat_names`
   (`crates/rumoca-phase-flatten/src/name_simplify.rs:23`) opens with
   `let mut working = flat.clone();`. Its own doc comment (`:11`-`:19`) already
   records that its semantic responsibility moved to the occurrence graph, so
   after A2 it is a pure display projection over `InstanceValueId` and must not
   clone.
5. **Textual constant machinery.** Production lines whose reason to exist is
   "resolve a rendered name against a rendered-name table":
   `constant_extraction.rs` 1,781; `pipeline/constant_injection.rs` 1,961 +
   `constant_injection/` 206; `postprocess/constant_lookup.rs` 513;
   `postprocess/constant_substituter.rs` 541; `postprocess/constant_expansion.rs`
   99; `postprocess.rs` 378; `postprocess_field_access.rs` 641;
   `postprocess_record_alias.rs` 106; `name_simplify.rs` 1,037 — **7,263 lines**.
   Constant *evaluation* survives A2; constant *lookup by spelling* does not.

### 2.4 A2.4 — `rumoca-eval-flat`

**Current owners.** `crates/rumoca-eval-flat/src/constant/context.rs` (190),
`constant/*` (4,435 total), `phase_constant/mod.rs` (1,011).

**What changed.** `EvalContext` already has the identity-keyed inventory the
checklist wants as the *destination*: `values_by_instance: IndexMap<InstanceId,
Value>` (`constant/context.rs:36`), `add_instance_parameter`
(`:96`), `instance_value` (`:107`). `rumoca-phase-dae` already reads it by
identity at `crates/rumoca-phase-dae/src/construction/analysis.rs:641`/`:654`.
So the identity lane exists.

**Remaining true gaps.**

1. **The identity lane is a shadow, not the lane.** `add_instance_parameter`
   (`context.rs:102`-`:103`) writes the value into `values_by_instance` **and**
   into the rendered-name `parameters` map, and every actual expression lookup
   goes through the name map: `get` (`:117`) → `lookup_value` (`:131`) →
   `lookup_scoped` (`:151`) → `scoped_component_path_candidates`
   (`rumoca-core/src/ir_primitives/component_refs_and_functions.rs:369`).
   `EvalLookup` (`context.rs:169`-`:190`) is a four-method `&str`-keyed trait.
   The checklist's "Delete scoped/suffix/string lookup rather than adding a
   resolved mode" (line 475-476) is untouched.
2. **The whole parameter-evaluation entry point is `String`-keyed.**
   `ParamEvalContext` (`phase_constant/mod.rs:92`-`:104`) is six
   `&FxHashMap<String, _>` fields plus `var_context: Option<&str>`, and
   `build_eval_context` (`:32`) clones every key into an `EvalContext`.
3. **A textual heuristic decides semantic kind.**
   `looks_like_enum_literal_path` (`phase_constant/mod.rs:999`) classifies a
   reference as an enumeration literal *if any non-final path segment starts
   with an uppercase letter*. It gates real behaviour at
   `phase_constant/mod.rs:883` and `:951`,
   `crates/rumoca-phase-flatten/src/pipeline/constant_injection.rs:1101` and
   `:1583`, and
   `crates/rumoca-phase-flatten/src/pipeline/context_and_tests/param_binding.rs:25`.
   Under SPEC 0036 fail-early this is the highest-severity item in A2: a
   capitalisation convention is deciding whether an expression is a value read
   or an enum literal, and the wrong answer is a *plausible* wrong trace, not a
   failure. It is deletable only once `FlatReferenceTarget::EnumLiteral` exists.
4. **No `Evaluated` / typed `RuntimeDependent` result.** Every entry point
   returns `Option<...>` (`phase_constant/mod.rs:198`, `:217`, `:229`, `:237`,
   `:859`, `:900`), so "not a constant" and "we could not find the name" are the
   same value.

### 2.5 A2.5 — phase-DAE typed transition maps and exhaustive target matches

**Current owner.**
`crates/rumoca-phase-dae/src/construction/expression.rs`, function
`lower_variable_reference` at `:847`.

**What changed.** phase-dae has **zero** `from_flat_path`/`to_flat_string` calls,
and the clock path is already identity-keyed:
`crates/rumoca-phase-dae/src/construction/clocks.rs:39` and `:66` look plans up
by `variable.instance_id`. `construction/analysis.rs:433` already enforces that
no two Flat variables share an `InstanceId`. The DAE side is in better shape
than the checklist implies.

**Remaining true gap — one function, four untyped dispatches.** Inside
`lower_variable_reference`, the reference kind is recovered by trying four
lookups in order and taking the first hit:

| line | dispatch | key |
|---|---|---|
| `expression.rs:855` | `Intrinsic(Time)` | `name.as_str() == "time"` — a string literal |
| `expression.rs:862` | domain binder | `binders: &HashMap<VarName, DomainBinderId>` |
| `expression.rs:867`-`:869` | function value | `values: &HashMap<VarName, ExprId>` |
| `expression.rs:882`-`:886` | enum literal | `flat.enum_literal_ordinals.get(name.as_str())` |
| `expression.rs:904`-`:906` | coordinate | `coordinates: &HashMap<VarName, Coordinate>` |

A Modelica model with a variable literally named `time` in scope, or a flat
variable whose rendered path collides with an enum literal's canonical path,
takes the wrong arm silently. `InvalidVariableRole` (`:908`) is only reached when
*all* lookups miss — acceptance-before-rejection is currently implemented as
"first table that answers wins", which is the opposite of a total match.

A2.5 replaces the whole ladder with one `match target { … }` over
`FlatReferenceTarget`, exhaustive by construction, and turns the five
`HashMap<VarName, _>` symbol tables into `IndexMap<InstanceValueId, _>` /
owner-local arrays.

### 2.6 A2.6 — codegen / CLI projection plus fixtures and tests

**Current owners.**
`crates/rumoca-phase-codegen/src/codegen/mod.rs:410` (`render_flat_context`),
`crates/rumoca/src/cli.rs:1072`/`:1073`,
`crates/rumoca-phase-codegen/src/templates/flat-modelica/flat_modelica.mo.jinja`.

**What changed.** Nothing relevant; this workstream is untouched by the 27
commits.

**Remaining true gap — and the hardest coupling in A2.**
`render_flat_context` builds the template context as
`Value::from_serialize(flat_model)` (`codegen/mod.rs:415`). **The Flat wire shape
and the Flat template view are the same object.** The `flat-modelica` template
reads `flat.variables`, `flat.equations`, `flat.initial_equations`,
`flat.assert_equations`, `flat.initial_assert_equations`,
`flat.variable_type_names`, `flat.variable_final_flags`,
`flat.model_description`, plus the nested serde shape of `Variable` and
`Expression`. Making `Model` opaque therefore breaks the target the same day, so
A2.6 must land a typed read-only view (`src/views/flat_model.rs`, by analogy with
the existing `views/algorithm_code*.rs`) in the same commit. This satisfies both
SPEC 0043 §7 ("Flat display/serialization resolves targets through root-owned
projections") and the codegen boundary rule ("typed read-only MiniJinja view").
The CLI `--json` dump becomes a one-way display projection with no `Deserialize`.

## 3. Stale checklist text — proposed amendments

Line numbers are in `dev/2026-07-28-spec-0036-cutover-checklist.md` at
`22447a1d`.

| Lines | Current text | Status | Proposed amendment |
|---|---|---|---|
| 466-469 | "Mint one Flat-owned branded `InstanceValueId` … `DefId` remains declaration/ancestry identity and cannot distinguish repeated instances." | **Justification stale.** `InstanceId` already distinguishes repeated instances; the problem moved. | Re-justify: "Flat currently allocates into the instantiation-owned `InstanceId` namespace by `max+1` scan (`ir-flat/src/lib.rs:311`,`:450`,`:463`). Mint a Flat-owned branded `InstanceValueId` so one namespace has one owner and Flat's own materialisations (`zero_sized_arrays.rs:40`) cannot collide with a transferred identity." |
| 484-490 | "The core structural family is generic over one total target: `SemanticExpression<T>` …" | **Premise stale.** Core's tree has exactly one instantiation (Flat); AST/GALEC/DAE each own theirs. | Record the Option A / Option B decision (§2.1) explicitly, with the measured single-instantiation fact, and name which one A2.1 implements. Do not leave the generic implied. |
| 503-510 | "Producer sequence: … lower every expression inside its semantic-owner closure …" | **Partly done.** The exact-identity walk exists as `postprocess/occurrence_graph.rs`. | Add: "The declaration-identity walk already exists (`postprocess/occurrence_graph.rs:50`,`:117`,`:147`,`:189`). A2.3 relocates it from postprocess to emission and changes its result from a rewritten `Reference` to an `InstanceValueId`; it is not rewritten from scratch." |
| 511-515 | "Delete `pipeline/instance_identity.rs`, its seeding/call sites … and its fabricated `max(source DefId) + InstanceId` namespace. Delete `postprocess_def_id.rs` and `structured_refs.rs` …" | **Three-quarters dead text.** `InstanceIdentitySpace` deleted; `postprocess_def_id.rs` and `structured_refs.rs` deleted by `1db32f1e`. | Replace with: "Delete `attach_reference_scope`/`attach_statement_reference_scopes`/`attach_when_chain_reference_scopes` and `InstanceScopeRewriter` (`pipeline/instance_identity.rs:68`,`:85`,`:104`,`:164`); fold `transfer_instance_relations` (`:7`) into `flat::Model::construct`'s reservation step and delete its call site at `flatten_pipeline.rs:759`. Delete the remaining late whole-model repair family `postprocess_field_access.rs` and `postprocess_record_alias.rs`; deleting the two files previously named by name did not close this row." |
| 462-465 | "Close the remaining direct public-field assembly bypass in `rumoca-ir-flat`." | Accurate but under-specified. | Add the concrete anchor: "`flat::Model` is `#[derive(… Default, Serialize, Deserialize)]` with ~30 public fields (`ir-flat/src/lib.rs:87`-`:211`); the bypass is the whole struct, not a residual field." |
| 516-520 | "Flat wire is a root-owned construction-operation projection … Decode reserves semantic owners and replays through `flat::Model::construct`." | Accurate, but reads as a migration. | Add: "No production Flat decoder exists today; the only encode sites are `rumoca/src/cli.rs:1072` and `phase-codegen/src/codegen/mod.rs:415`. Replay is new capability and needs its own round-trip and forged-ordinal fixtures, not a schema bump." |
| 521-527 | "A2 must integrate as one commit assembled from disjoint workstreams: A2.1 … A2.6. None may land separately or introduce an adapter." | Correct in intent, unimplementable as literally written for Rust. | Add the staging rule from §5: "Disjointness is achieved by landing the new type surface first *in the working tree* (Stage A), forking per-consumer worktrees against it (Stage B), then integrating and deleting the old surface (Stage C), squashed to one commit. The coexistence window exists only in the working tree and never in a landed commit." |
| 658-666 | `ClockPartitions` orphan removal, listed under "Fail-early trust audit". | Correct, but scheduled separately from A2. | Cross-reference: "This is the A2.2 atomic cutover. 1,421 of `rumoca-ir-flat`'s 4,309 production lines; delete in the same commit and count it in the A2 LOC ledger." |
| 920-928 | LOC ledger: "prior HEAD `6151825d` = 373,660; this commit = 376,885" | **Stale by +11,288.** | Append the `22447a1d` = 388,173 measurement and the reproducible one-liner from §1.2. |
| 993-1000 | Canary snapshot "commit `911c9199` era … sim_ok remains 0/20" | **Superseded** by the `090d45ae` sweep recorded at lines 1037-1046 (canary subset 4/20 sim_ok). | Mark the `911c9199` entry as superseded rather than deleting it; A2's before/after delta must be measured against `090d45ae`, not against it. |
| 703-708 | "Remove unique-suffix/textual constant recovery from ToDAE analysis … `EvalContext` must switch exclusively to the typed identity" | Half done: unique-suffix recovery deleted by `1db32f1e`; `EvalContext` still name-keyed. | Narrow to the surviving half and anchor it: `constant/context.rs:117`,`:131`,`:151` and `component_refs_and_functions.rs:369`. |

## 4. Dependency graph

```
                    ┌── D0: Option A/B decision on core tree ownership (§2.1)
                    │        blocks everything; not a code change
                    ▼
  Stage A  ─────────────────────────────────────────────────────────────
   S1  FlatReferenceTarget + branded IDs + InstanceValueId (ir-flat)
   S2  flat::Model::construct skeleton: reservation → owner closures → freeze
   S3  Flat typed views (read-only) over the new arenas
        (S1 → S2 → S3, strictly sequential, one worktree)
                    ▼
  Stage B  ── parallel, all forked from Stage A, file-disjoint ──────────
   W1  phase-flatten producers      W4  phase-dae lowering
   W2  phase-flatten pipeline/      W5  codegen typed view + CLI + templates
       postprocess/constants        W6  Flat wire replay + negative fixtures
   W3  eval-flat identity keying
                    ▼
  Stage C  ─────────────────────────────────────────────────────────────
   S4  integrate W1..W6, delete the old core/Flat surface, fix residue
   S5  spec amendments (SPEC 0029 §3a/§3b, SPEC 0043 §7) + checklist edits
   S6  evidence sweep (§5), LOC/perf ledger (§6), squash to one commit
```

**Why Stage A cannot be parallelised and why Stage B can.** Rust has no way for
six worktrees to converge on a type signature none of them can see. Stage A
therefore lands the complete new surface *alongside* the old one in the working
tree — `rumoca-ir-flat` compiles, its own tests pass, and nothing else has
changed. Stage B worktrees each fork from that tree and convert exactly one
consumer; each can `cargo check -p <its crate>` because the new surface exists.
Stage C removes the old surface and squashes. **The coexistence window lives only
in the working tree and in no landed commit**, which is what SPEC 0036's
"no coexistence" rule actually forbids.

## 5. Slice list

Sizes are the production lines each slice touches, from §1.2/§2.

### Stage A — sequential, single worktree

| # | Slice | Files | Touched LOC | Notes |
|---|---|---|---|---|
| S1 | `FlatReferenceTarget`, `InstanceValueId`, `FlatFunctionId`, owner-local `FlatFunctionValueId`/`FlatDomainBinderId` | new `ir-flat/src/ids.rs`, `ir-flat/src/target.rs` | +400 | brand pattern from `ir-dae/src/ids.rs:6`,`:38`; nested brands so a binder ID cannot escape its owner closure |
| S2 | `flat::Model::construct` — reservation, semantic-owner closures, freeze; private arenas replacing the 30 public fields | `ir-flat/src/lib.rs` (1,317) + new `ir-flat/src/construct/` | +1,200 / −600 | absorbs `transfer_instance_relations`, `materialize_instance`, `next_instance_id`, `next_function_instance_id`, `validate_shape_contract` |
| S3 | Root-owned read-only views (variables, equations, functions, when-chains, targets) | new `ir-flat/src/views/` | +500 | the single surface W1..W6 program against |
| S3b | Delete the `ClockPartitions` orphan | `ir-flat/src/clocks.rs` (1,421) | −1,421 | no production consumer; closes checklist lines 658-666 |

If **Option B** (§2.1) is chosen, add S0 before S1: move
`Expression`/`Statement`/`Function`/`FunctionParam`/`ForIndex`/
`ComprehensionTemplate` from `rumoca-core` into `rumoca-ir-flat` with
`FlatReferenceTarget` inlined, and shrink `rumoca-core` to multi-stage
vocabulary (≈ −3,500 core lines, ≈ +3,500 Flat lines, no net change but the
generic parameter never exists). If **Option A**, add S0' instead:
parameterise the nine core files listed at checklist lines 528-531 over `<T>`
(≈ 900 lines edited in core, then a mechanical `<FlatReferenceTarget>`
annotation wave through W1..W5 — budget +25% on every Stage B slice).

### Stage B — parallel worktrees, disjoint by file

| # | Worktree | Owns | Touched LOC | Expected delta |
|---|---|---|---|---|
| W1 | phase-flatten producers | `ast_lower.rs`, `variables.rs`, `equations/`, `connections/`, `functions/`, `algorithms.rs`, `when_equations.rs`, `enum_literals.rs` | ~19,900 | −1,500 |
| W2 | phase-flatten resolution & repair | `pipeline/flatten_pipeline.rs`, `pipeline/instance_identity.rs`, `pipeline/constant_injection*`, `constant_extraction.rs`, `postprocess.rs`, `postprocess/*`, `postprocess_field_access.rs`, `postprocess_record_alias.rs`, `name_simplify.rs`, `qualify.rs`, `boolean_eval.rs` | ~11,500 | **−4,000 to −5,000** |
| W3 | eval-flat | `constant/context.rs`, `constant/*`, `phase_constant/*` | 6,283 | −1,200 |
| W4 | phase-dae | `construction/expression.rs`, `construction/analysis*`, `construction/clocks.rs`, `construction/function_*` | ~7,000 | −300 |
| W5 | codegen + CLI + templates | `codegen/mod.rs`, new `views/flat_model.rs`, `rumoca/src/cli.rs`, `templates/flat-modelica/` | ~2,000 | +400 |
| W6 | Flat wire replay | new `ir-flat/src/wire/` + fixtures | new | +900 |

**Ownership rules that prevent worktree conflicts.**
`crates/rumoca-phase-flatten/src/lib.rs` and `pipeline/mod.rs` are touched by both
W1 and W2 (module declarations and re-exports); they are **integrator-owned** —
W1/W2 may not edit them, and S4 makes every declaration change. Same rule for
`crates/rumoca-ir-flat/src/lib.rs` (Stage A owns it) and
`crates/rumoca-core/src/lib.rs`.

W2 is the critical path and the largest single risk; it should start first and
may need to be split at the `constant_*` / `postprocess_*` seam if it does not
converge.

### Stage C — sequential

| # | Slice | Notes |
|---|---|---|
| S4 | Integrate W1..W6, delete `Reference`/old `Model` surface, fix residue | expect a long tail in `rumoca` integration tests (485 `#[test]`) |
| S5 | SPEC 0029 §3a/§3b + SPEC 0043 §7 amendments, checklist edits from §3 | must be in the same commit per checklist line 478 |
| S6 | Evidence sweep (§5), LOC/perf ledger (§6), squash | |

**Honest sizing.** ~50,000 production lines are in the blast radius and ~1,571
tests. This is not a one-session commit. Stage A is one focused session; each
Stage B worktree is one to three sessions with W2 the outlier; Stage C is one to
two. Any plan that promises A2 in a single sitting is wrong, and the failure mode
of pretending otherwise is a partial landing that silently reinterprets
`Reference::instance_id` (§7).

## 6. Evidence list

Every item below is a gate, not a description. Grouped by the checklist's
"Required evidence" bullet (lines 536-543).

### 6.1 Compile-fail (`` ```compile_fail `` doctests, run by `xtask verify workspace`)

The established pattern is `crates/rumoca-ir-flat/src/clocks.rs:48` and
`crates/rumoca-ir-dae/src/lib.rs:7`.

1. An `InstanceValueId` minted by one `flat::Model::construct` used against
   another Flat root (cross-Flat brand).
2. A `FlatDomainBinderId` escaping its `for`-owner closure and used outside it.
3. A `FlatFunctionValueId` from function *A* used inside function *B*'s body
   closure.
4. `flat::Model::default()` / `flat::Model { .. }` struct-literal construction
   (must not compile once the fields are private).
5. `flat::Model: Deserialize` used directly (decode must go through the wire
   module's replay, never a derive).
6. Constructing a target entry without provenance — the kind-specific
   constructor must require a `ProvenanceSpan` or a typed generated owner.
7. Mutating a view (`views::VariableView` must expose no `&mut`).

### 6.2 Negative / runtime-rejection (unit tests in `rumoca-ir-flat`)

8. Malformed wire: unknown field, missing field, explicit `null` where a value
   is required (mirrors the existing `ReferenceWire` discipline at
   `reference_serde.rs:31`-`:53`).
9. Forged ordinal: a wire target ordinal beyond the reserved arena length.
10. Forged owner: a wire function-value ordinal naming a function that was never
    reserved.
11. Duplicate reservation of one `InstanceValueId`.
12. A reference to an unreserved target (unresolved).
13. An ambiguous member selection — two occurrences of one declaration sharing
    element indices (today `occurrence_graph.rs:242` returns `None`; after A2
    this must be a typed failure at the exact occurrence, not a silent
    non-rewrite).
14. Wrong-kind: a `Value` target in call position and a `Function` target in
    value position.
15. `InstanceValueId::UNSET`-equivalent sentinel rejected (port of
    `ModelShapeContractError::MissingVariableInstanceId`, `lib.rs:502`).

### 6.3 Provenance

16. Every target entry carries either exact declaration provenance
    (`DefId` + declaration span) or typed generated provenance with a
    responsible span — asserted by an exhaustive match, not a spot check.
17. Every reference occurrence carries its own use span, distinct from the
    declaration span — one fixture where a variable is read twice in one
    equation and the two occurrences report different spans.
18. A generated connection/alias/array-view target reports the responsible
    source equation span, never `Span::DUMMY` (the `span_debt` arch gate at
    `crates/rumoca/tests/architecture_hardening/size_and_validation/span_debt.rs`
    already enforces the no-dummy rule for production files; A2 must not add an
    exception).

### 6.4 Round trip

19. Flat wire JSON round trip: construct → serialise → replay through
    `flat::Model::construct` → structural equality, over a fixture exercising
    every `FlatReferenceTarget` variant.
20. Binary round trip through the same replay.
21. Semantic-equality round trip: `semantically_eq_ignoring_spans`
    (`ir_primitives.rs:1812`) survives the target change, with spans differing.
22. Deterministic readable snapshot: two compiles of the same source produce
    byte-identical CLI `--json` Flat output and byte-identical
    `flat-modelica` output. This is the regression guard against
    hash-iteration order leaking into the display projection.

### 6.5 Behaviour preservation (the acceptance-before-rejection set)

23. `postprocess/occurrence_graph.rs`'s existing behaviour is preserved
    verbatim in its new emission-time home: inherited-member lookup through
    `extends` (`:222`-`:226`), pending array selection (`:249`-`:253`), and the
    MLS §10.5 `arr.member` element expansion with its four rejection cases
    (`:147`-`:183`). Port the tests, do not rewrite them.
24. `crates/rumoca-phase-flatten/src/pipeline/instance_identity.rs:261`
    (`nested_algorithm_scopes_ranges_conditions_lhs_subscripts_and_values`) and
    `:310` (`when_chain_scopes_nested_conditions_and_assert_fields`) become
    emission-time equivalents — nested for/if/assignment-subscript and
    when/elsewhen/nested-assert references must each resolve to the right
    target.
25. `crates/rumoca-phase-flatten/src/pipeline/instance_identity.rs:388`
    (`flattening_gives_every_variable_and_record_an_allocated_occurrence_identity`)
    becomes a total-target assertion over the new arenas.
26. `crates/rumoca-ir-flat/src/tests.rs:208`
    (`flat_wire_accepts_only_the_current_reference_shape`) extends to the target
    grammar.
27. The `time` intrinsic and enum-literal dispatch that
    `expression.rs:855`/`:885` do by spelling are covered by two new
    source-to-DAE fixtures: a model declaring a component named `time` in an
    inner scope, and two enumeration types whose literals render to the same
    canonical path. Both must compile to the *correct* target or fail with a
    typed diagnostic; neither may take the wrong arm.
28. Deleting `looks_like_enum_literal_path` (`phase_constant/mod.rs:999`)
    requires a fixture with an enum literal whose non-final segments are all
    lowercase (today misclassified as a parameter read) and a parameter path
    whose middle segment is capitalised (today misclassified as an enum
    literal). Both are current latent wrong-trace risks and must become correct,
    not merely "unchanged".

### 6.6 Pipeline and cohort

29. `cargo nextest run --workspace` green (baseline 4,879 tests / 33 s).
30. Doctests green (carries §6.1).
31. `xtask verify architecture` green (124/124), including
    `code_size_budget_test.rs`'s 2,000-line ceiling — every new file must be
    under it without a SPEC 0021 exception.
32. `xtask verify lint` green: strict Clippy all-targets/all-features, plus the
    traversal-policy check
    (`crates/xtask/src/bin/rumoca-traversal-policy-check.rs:10` — if A2 adds a
    recursive lowering entry point to a covered file, register it, do not
    whitelist it).
33. Canary-20 before/after per SPEC 0033 §6a Tier 1, one 10 s attempt per model
    and phase:
    `CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 cargo xtask verify msl-parity --sim-targets-file dev/msl-canary-20.json`.
    Baseline is the `090d45ae` sweep's canary subset: **4/20 sim_ok**
    (TimeBasedPulse, TimeBasedStep, TickBasedRamp, ShowSaturatingInductor),
    4 sim_solver_fail, 12 ToDae (ED013 ×5, ED019 ×4, ED008 ×3). Record the
    per-model `failure_bucket`/`owner_category` delta from `msl_results.json`,
    not a headline count.
34. Tier 2 full 566-model sweep after the reliable-core gates. Baseline
    `090d45ae`: 107/566 compile, 33 sim_ok, cohort failure mass ToDae 269 /
    resolve 66 / sim-solver 66 / typecheck 62 / flatten 53 / instantiate 9.
    **A2's specific claim to test: `owner_category == Flatten` (53) must not
    grow, and `Frontend` must not grow.** Any model that A2 cannot make correct
    keeps its typed failure — no model may move from a typed failure to a
    numeric result without a trace comparison.
35. OMC trace parity for every model whose `sim_ok` status changes.

## 7. LOC and perf measurement plan

### 7.1 LOC

Measure at three points with the §1.2 one-liner, on clean trees only:

| Point | What |
|---|---|
| `22447a1d` | baseline, already measured: repo 388,173; A2 crates per §1.2 |
| end of Stage A | isolates the cost of the new surface before any deletion |
| final squashed commit | acceptance evidence |

Per-crate targets for the final commit (the number the ledger must beat, not a
number to code-golf toward):

| Crate | Baseline | Target | Driver |
|---|---|---|---|
| `rumoca-core` | 10,851 | 7,300 (Option B) / 11,700 (Option A) | tree moves out, or gains `<T>` |
| `rumoca-ir-flat` | 4,309 | 5,000 (Option A) / 8,500 (Option B) | +arenas/construct/views/wire, −1,421 `ClockPartitions` |
| `rumoca-phase-flatten` | 50,179 | ≤ 45,500 | −4,000 to −5,000 from W2 |
| `rumoca-eval-flat` | 6,283 | ≤ 5,100 | name-keyed lookup deleted |
| `rumoca-phase-dae` | 21,016 | ≤ 20,700 | symbol-table ladder → one match |
| `rumoca-phase-codegen` | 12,463 | ≤ 12,900 | +typed Flat view |
| **A2 total** | **112,752** | **≤ 106,000** | net **−6,700 or better** |

Rules carried from SPEC 0036: the reduction may not come from deleting
capability or tests; a module that grows must carry a written justification in
the commit message; and if the final number is worse than baseline, the commit
does not land — the ranked reductions in §2.3 (7,263 lines of textual constant
machinery) are the routed path, not a stretch goal.

The repository net-decrease gate (checklist line 281) is measured against
merge-base `1acf3641` = 438,804. A2 must keep the branch net negative; at
`22447a1d` the branch is **−50,631** and A2 should improve that.

### 7.2 Performance

Three budgets, all with existing infrastructure — no new harness.

**(a) Per-phase compile wall.** `target/msl/<run>/msl_parity_timing.json`
already records `core_pipeline.compile_flatten_seconds`,
`compile_todae_seconds`, `compile_typecheck_seconds`,
`compile_instantiate_seconds` and their call counts. Capture the canary-20 run
at `22447a1d` and at the final commit.

*Expected direction, and the reason A2 should be a speed-up rather than a
regression:* `collapse_index_refs_to_known_varrefs` runs 2× and
`substitute_known_constants_in_flat` 3× per compile (`flatten_pipeline.rs:963`,
`:994`, `:966`, `:992`, `:1003`); `KnownFlatVars::build` (`index_collapse.rs:195`)
rebuilds seven whole-model tables per invocation and clones a `Reference` per
variable and record; `name_simplify.rs:23` deep-clones the entire model.
Resolution-at-emission runs the occurrence walk once per reference and none of
the whole-model tables. **Budget: `compile_flatten_seconds` must not regress;
target −20%.**

**(b) Per-model ceilings — already live, must stay green.**
`SOLVE_IR_SIZE_LIMIT_MB_DEFAULT = 32`
(`crates/rumoca-test-msl/src/resource_budget.rs:64`) and
`MODEL_COMPILE_WALL_LIMIT_SECS_DEFAULT = 40.0` (`:73`), failing loudly as
`EMSL_BUDGET_SOLVE_IR_SIZE` (`:76`) / `EMSL_BUDGET_COMPILE_WALL` (`:79`) under
`ModelFailureBucket::ResourceBudget`. **No model may newly hit either.** A rise
in `owner_category == Performance` is an A2 regression regardless of what
`sim_ok` does.

**(c) Peak RSS and serialized size.** The MSL worker already enforces a
resident-plus-swap ceiling (`crates/rumoca-worker/src/lib.rs:32`) and records
`solve_ir_bytes` per model
(`crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_sim_worker.rs:230`).
Record the canary-20 distribution before and after. Separately, record the
serialized size of the Flat wire for the five largest canary models —
`InstanceValueId` ordinals replacing repeated rendered component paths should
shrink it materially, and if it does not, the wire is still carrying display
text.

**(d) Expression traversal time.** The checklist asks for it (line 541) and
there is no bench harness in the repo (`[[bench]]` returns nothing). Rather than
add a Criterion dependency for one number, derive it from (a): traversal time is
the dominant term in `compile_flatten_seconds` and is separately visible in the
`compile_todae_seconds` delta. If (a) is flat but the wire shrinks, add a
targeted `#[test]`-driven timing assertion on the largest canary model rather
than a new benchmark crate.

**Resource discipline for every measurement run:** prefix with
`CARGO_BUILD_JOBS=6 RUST_TEST_THREADS=6` under `nix develop --command`, and use
`CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4` for the msl-parity lane per SPEC 0033.

## 8. Risks

1. **`Reference::instance_id` changes meaning mid-flight.** Today it is the
   *enclosing class-body* occurrence, stamped by `InstanceScopeRewriter`
   (`instance_identity.rs:164`-`:193`) and consumed as a walk origin by
   `KnownFlatVars::path_cursor` (`index_collapse.rs:290`). After A2 the target
   names the value itself. A partial landing where some producers stamp the
   scope and some stamp the value **compiles and produces plausible wrong
   traces**. Mitigation: the field must be *deleted and replaced*, never
   reinterpreted — `Reference` itself goes away in the same commit, so no code
   path can read a scope-stamped identity as a value identity.
2. **W2 does not converge.** 11,500 lines with three re-entrant fixed-point
   loops (`collect_rewritten_functions_to_fixed_point`,
   `flatten_pipeline.rs:1017`-`:1048`). If two sessions do not close
   it, split at the `constant_*` / `postprocess_*` seam and accept a longer
   Stage B, rather than landing a partial A2.
3. **Codegen breaks silently.** `Value::from_serialize` (`codegen/mod.rs:415`)
   means a template reading a field that no longer exists renders empty rather
   than failing. Mitigation: the typed view (W5) must be a struct with named
   fields, and the `flat-modelica` golden output must be byte-compared, not
   smoke-tested.
4. **Spec drift.** S5 must land in the same commit. If the Option B decision is
   taken and SPEC 0043 §7 is not amended in the same change, the architecture
   gate and the annex disagree and the commit is unreviewable.

## 9. Anchor index

Every anchor cited above, verified present at `22447a1d`.

| Anchor | What |
|---|---|
| `crates/rumoca-core/src/ir_primitives.rs:40` | `pub struct InstanceId(pub u32)` |
| `crates/rumoca-core/src/ir_primitives.rs:99` | `pub struct FunctionInstanceId(pub u32)` — unbranded |
| `crates/rumoca-core/src/ir_primitives.rs:558` | `pub struct Reference` |
| `crates/rumoca-core/src/ir_primitives.rs:696` / `:700` | `instance_id()` / `with_instance_id()` |
| `crates/rumoca-core/src/ir_primitives.rs:1465` | `pub enum Expression` |
| `crates/rumoca-core/src/ir_primitives.rs:1812` | `semantically_eq_ignoring_spans` |
| `crates/rumoca-core/src/ir_primitives/reference_serde.rs:31` / `:87` | `ReferenceWire` / `Reference::construct` |
| `crates/rumoca-core/src/ir_primitives/component_refs_and_functions.rs:369` | `scoped_component_path_candidates` |
| `.../component_refs_and_functions.rs:414` / `:426` / `:706` / `:933` | `ForIndex` / `Statement` / `Function` / `FunctionParam` |
| `crates/rumoca-core/src/subscript.rs:6` | `pub enum Subscript` |
| `crates/rumoca-core/src/structured_domain.rs:186` | `ComprehensionTemplate` |
| `crates/rumoca-core/src/expression_visitor.rs:10` / `:195` | visitor traits |
| `crates/rumoca-core/src/expression_rewriter.rs:6` / `:345` | rewriter traits |
| `crates/rumoca-core/src/statement_rewriter.rs:6` / `:149` | statement rewriter traits |
| `crates/rumoca-ir-ast/src/nodes.rs:911` | AST's own `Expression` |
| `crates/rumoca-ir-galec/src/ast.rs:650` | GALEC's own `Expression` |
| `crates/rumoca-ir-dae/src/ids.rs:6` / `:38` | `branded_ids!` / `owner_local_ids!` |
| `crates/rumoca-ir-flat/src/lib.rs:87` | `#[derive(… Default, Serialize, Deserialize)]` on `Model` |
| `crates/rumoca-ir-flat/src/lib.rs:107` / `:155` / `:206` / `:211` | `variables` / `functions` / `enum_literal_ordinals` / `instance_relations` |
| `crates/rumoca-ir-flat/src/lib.rs:215` / `:222` | `InstanceKind` / `InstanceRelation` |
| `crates/rumoca-ir-flat/src/lib.rs:311` / `:450` / `:463` | `materialize_instance` / `next_instance_id` / `next_function_instance_id` |
| `crates/rumoca-ir-flat/src/lib.rs:363` / `:502` | `validate_shape_contract` / `MissingVariableInstanceId` |
| `crates/rumoca-ir-flat/src/lib.rs:545` | `Variable::instance_id` |
| `crates/rumoca-ir-flat/src/when_equations.rs:6` | `WhenChain` (private `first`) |
| `crates/rumoca-ir-flat/src/clocks.rs:45` | `impl ClockPartitions` — orphan |
| `crates/rumoca-ir-flat/src/tests.rs:70` / `:208` | first-branch decode / reference wire shape |
| `crates/rumoca-phase-flatten/src/pipeline/instance_identity.rs:7` | `transfer_instance_relations` |
| `.../instance_identity.rs:68` / `:85` / `:104` / `:164` | scope attachment + `InstanceScopeRewriter` |
| `.../instance_identity.rs:261` / `:310` / `:388` | behaviour tests to port |
| `crates/rumoca-phase-flatten/src/pipeline/flatten_pipeline.rs:759` | `transfer_instance_relations` call site |
| `.../flatten_pipeline.rs:963` / `:1011` | collapse pass / name simplification |
| `crates/rumoca-phase-flatten/src/postprocess/occurrence_graph.rs:50` / `:117` / `:147` / `:189` | `OccurrenceGraph` and its four operations |
| `crates/rumoca-phase-flatten/src/postprocess/index_collapse.rs:18` / `:176` / `:195` | collapse entry / `KnownFlatVars` / seven-table build |
| `crates/rumoca-phase-flatten/src/postprocess.rs:46` / `:85` / `:89` | `HashSet<String>` alias set / substitution entry / live-var set |
| `crates/rumoca-phase-flatten/src/postprocess_field_access.rs:15` | late whole-model binding repair |
| `crates/rumoca-phase-flatten/src/name_simplify.rs:23` | `let mut working = flat.clone();` |
| `crates/rumoca-phase-flatten/src/zero_sized_arrays.rs:40` | only `materialize_instance` caller |
| `crates/rumoca-eval-flat/src/constant/context.rs:13` / `:36` / `:96` / `:107` | `EvalContext` / `values_by_instance` / `add_instance_parameter` / `instance_value` |
| `crates/rumoca-eval-flat/src/constant/context.rs:117` / `:131` / `:151` / `:169` | name-keyed `get` / `lookup_value` / `lookup_scoped` / `EvalLookup` |
| `crates/rumoca-eval-flat/src/phase_constant/mod.rs:32` / `:92` | `build_eval_context` / `ParamEvalContext` |
| `crates/rumoca-eval-flat/src/phase_constant/mod.rs:883` / `:951` / `:999` | textual enum heuristic and its two gates |
| `crates/rumoca-phase-dae/src/construction/expression.rs:847` | `lower_variable_reference` |
| `.../expression.rs:855` / `:862` / `:885` / `:906` | `"time"` literal / binder map / enum ordinals / coordinate map |
| `crates/rumoca-phase-dae/src/construction/clocks.rs:39` / `:66` | identity-keyed clock plans |
| `crates/rumoca-phase-dae/src/construction/analysis.rs:433` / `:641` / `:654` | duplicate-instance rejection / `instance_value` / `add_instance_parameter` |
| `crates/rumoca-phase-codegen/src/codegen/mod.rs:410` / `:415` | `render_flat_context` / `Value::from_serialize` |
| `crates/rumoca/src/cli.rs:1072` / `:1073` | Flat JSON dump / Modelica dump |
| `crates/rumoca-test-msl/src/resource_budget.rs:64` / `:73` / `:76` / `:79` | live per-model ceilings and their codes |
| `crates/rumoca-worker/src/lib.rs:32` | worker RSS ceiling |
| `crates/rumoca-worker/src/failure_classification.rs:33` / `:104` | `ModelFailureBucket` / `ModelFailureOwner` |
| `crates/xtask/src/bin/rumoca-traversal-policy-check.rs:10` | traversal-policy covered-file registry |
| `crates/rumoca/tests/code_size_budget_test.rs:6` | 2,000-line per-file ceiling |
| `spec/SPEC_0043_CONSTRUCTION_CATALOG.md:124` | §7 Flat Aggregate Construction Catalog heading (rows 126-141) |
