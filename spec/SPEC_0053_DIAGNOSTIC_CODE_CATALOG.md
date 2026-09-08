# SPEC_0053: Diagnostic Code Catalog

## Status
REFERENCE

## Summary
Lookup catalog of phase-owned diagnostic ranges, retired codes, and rejection
acceptance contracts governed by SPEC_0008.

## Specification

Every row is normative by reference from SPEC_0008 and adds no independent
requirement.

### 1. Phase Ranges And Current Dispositions

| Range | Phase | Mnemonic | Description |
|---|---|---|---|
| `EP0xx` | parse | Parse | Syntax errors and parse-time source refusals |
| `ER0xx` | resolve | Resolve | Name-resolution errors |
| `ET0xx` | typecheck | Type | Type errors |
| `WT0xx` | typecheck | Type | Non-fatal diagnostics, including `WT003`, `WT006`, `WT007` |
| `EI0xx` | instantiate | Instantiate | Modification errors |
| `WI0xx` | instantiate | Instantiate | Non-fatal instantiation diagnostics |
| `EF0xx` | flatten | Flatten | Connection/Flat errors |
| `ED0xx` | DAE | DAE | Equation errors |
| `EC0xx` | codegen | Codegen | Code-generation errors |
| `EM0xx` | class merge | Merge | Class-tree merge errors |
| `ES0xx` | structural | Structural | Matching/BLT errors (`ES01x`) |
| `WS0xx` | structural | Structural | Non-fatal singularity/algebraic-loop diagnostics |
| `EL0xx` | solve lowering | Solve | `EL001`–`EL011`, `EL02x` assembly, `EL03x` overrides |
| `EX0xx` | simulation runtime | Execution | Solver, preparation, and override errors |
| `EG0xx` | GALEC IR | GALEC | Parse/validation errors |
| `EGT0xx` | GALEC projection | GALEC Target | DAE-to-GALEC projection errors |
| `EFM0xx` | eFMI packaging | eFMI | Manifest/package errors |
| `WP`/`WR`/`WT`/etc. | owning phase | Warning | Other warning ranges |

Stable identity is the bare mnemonic suffix (`ED001`), even when a renderer
prefixes it. Consumers match by suffix. Current dispositions:

| Code/range | Disposition |
|---|---|
| Former GALEC `ET001`–`ET023` | Retired; GALEC projection uses `EGT001`–`EGT023`; `ET0xx` is typecheck-only |
| `EI013` | Retired; synthesized-inner notice is `WI013` |
| `ER121` | Retired. It over-applied an implicit-only MLS rule to Resolve input that was not yet complete enough to decide it. No single code succeeds it: `EP004` owns the refusal of an omitted iterator range in recognized source, `ER129` owns the refusal of a forged or deserialized `ParsedTree` that carries one, and the MLS §11.2.2.1 whole-array restriction remains outstanding for a future post-instantiation/Flat owner, since it is conditioned on an inferred implicit range and cannot be decided before that inference exists |
| `EP004` | Iterator declared without the range Rumoca still requires |
| `ES001`/`ES002` | Retired; structural warnings are `WS001`/`WS002` |
| `EI036` | Unproved selected `equalityConstraint` exposure |
| `EI037` | Duplicate, unset, unallocated, or mismatched Instance occurrence insertion |
| `ET012` | Unresolved/cyclic/contradictory function-signature class graph |
| `ET013` | Effective `equalityConstraint` specialization cannot publish atomically |
| `ET014` | Canonical type identity or type-root construction could not publish the exact total class/enumeration/alias product because of identity exhaustion, a duplicate or mismatched declaration claim, a ghost/preexisting payload, a name collision, a missing or malformed Resolve-branded edge/identity/target, a cycle, or a foreign non-`UNKNOWN` external `TypeId` |
| `EF032` | Owns `TerminalType::Empty` |
| `EF035` | Required recovery node, empty operator, or binding-state mismatch |
| `EF036` | Invalid function-call output slot |
| `EF037` | Eager structural Integer range above 100,000 elements |
| `EF038` | Unrepresentable connection evidence before Flat/VCG mutation |
| `EF039` | Conflicting concrete component-dimension evidence |
| `EC009` | A checked target capability profile excludes a source-model feature before artifact construction |

### 2. Acceptance Contract Catalog

| Code | Rejected boundary | Accepted owner and evidence |
|---|---|---|
| `EP004` | An iterator declared without `in <range>`. MLS 3.7 §11.2.2.1 deduces such a range from the dimensions the iterator subscripts; that inference does not exist, so the parser refuses the omission at the source boundary instead of admitting a range-shaped hole into the AST. Scope is exactly the omission: the same rule's whole-array restriction is conditioned on an inferred implicit range and is not applied here | One parser-owned `ForIndex` converter serves for-equations, for-statements, array comprehensions, and reduction arguments, so the refusal has one construction site and cannot diverge between syntaxes. Explicit ranges stay accepted in all four syntaxes and in multi-iterator clauses; explicit-range whole-array assignment stays legal. `EP004` is a distinct `ParseError` variant, not a `SyntaxError`, so unrelated syntax corruption and other parse-semantic refusals still report `EP001` and cannot satisfy an `EP004` witness. `ER129` separately refuses a forged or deserialized `ParsedTree` carrying an omitted range; it is a different fact, not a successor |
| `EI036` | Unproved effective `equalityConstraint` exposure | Resolve/Instantiate accept absent slots and exact settled exposures |
| `EI037` | Invalid Instance occurrence insertion | Unique component/class occurrences; exact shared-ID pairs |
| `ET012` | Malformed or cyclic function-signature class graph | Typecheck accepts finite identity-complete acyclic graphs, deep chains, unambiguous diamonds, and exhaustively proved absence |
| `ET013` | Effective `equalityConstraint` specialization cannot publish | Typecheck accepts exact specialization and publishes atomically |
| `ET014` | Typecheck planning of the complete class/enumeration/alias inventory exhausts the `TypeId` domain, repeats one predefined/source or source/source `DefId`, admits a preexisting/ghost definition, index, or payload, would overwrite or reuse an existing/new type-name binding, mismatches a planned declaration and its exact name/kind/literals/base/payload, carries a foreign non-`UNKNOWN` component `TypeId`, or cannot close a mutated/malformed Resolve-branded alias graph because its sole extends edge, base identity, issued target, or finite root is missing or cyclic. Source-backed graph and external-identity failures label the owning occurrence or actual failing edge; allocation failures retain the first affected declaration-edge provenance. AST current-wire deserialization is a pre-phase trust boundary: the same sole checked issuer returns its typed current-root construction refusal there, but no phase mnemonic is emitted until Typecheck owns a phase diagnostic. Any error diagnostic from Typecheck, including a later `ET002`, publishes neither the detached standalone tree/table nor the detached instanced overlay candidate; legal warnings may accompany successful publication. | `ClassTree` structurally mints the opaque exact declaration inventory consumed by both the AST append plan and current-root replay. The plan accepts one checked identity product containing the canonical seven-type predefined prefix and complete uniquely named class, enumeration, and alias payloads, with exact structural/index association, declaration uniqueness, forward-edge, finite-root, capacity, construction-failure, existing-name, and new-name-collision evidence. Typecheck consumes that product without an independent declaration rescan and accepts one dense total root per issued ID. Paired standalone/instanced entries accept the same long valid chain and aliases to `StateSelect`/`AssertionLevel`; class/enum/alias predefined-collision, every canonical predefined claim deletion/substitution/duplicate, and duplicate-`DefId` witnesses retain exact identity; current-wire mutations cover cycles, deletion, rename, ghosts, and coordinated removal of every `StateSelect` declaration claim while its payload survives; genuine unresolved overlay data remains `ET001`; and missing-edge, missing-identity, missing-target, name-collision, foreign/out-of-range ID, warning-success, and late-`ET002` mutations prove typed publication/refusal with bit-exact tree/table and overlay nonpublication on error. |
| `EF035` | Required recovery syntax or inconsistent binding state | Flat accepts empty sections, absent starts, omitted outputs, and well-formed bindings |
| `EF036` | Invalid function-call output slot | AST lowering accepts references and exact omitted-slot `Empty` |
| `EF037` | Eager Integer range above the named budget | Conditional analysis accepts compact and within-budget ranges |
| `EF038` | Unproved connection selection, extent, or cardinality | Planning accepts exact scalar/full/zero domains and commits nothing on refusal |
| `EF039` | A deferred component axis has two fully concrete, equal-rank shape candidates whose extents disagree; the diagnostic names both extents and the one-based axis at the source declaration | Equal concrete candidates are accepted; explicit axes remain source-evaluated until Typecheck issues mixed-axis proofs; zero, singleton, binding-inferred colon, and structured parent-prefix shapes remain accepted when their local axes agree |
| `EC009` | A checked target profile explicitly marks the encountered feature unsupported. For `continuous_states`, rejection retains the first exact state-declaration span and occurs before GALEC/eFMI product or artifact construction | The same target accepts a source only when its complete checked capability profile admits every encountered feature; the registered `galec` and `efmu` targets retain a typed exact-span refusal for a continuous model, while their discrete accepted fixtures exercise real product construction |
| `ER088` | Impure constant/continuous binding call | Resolve accepts MLS parameter, initial, `when`, and impure-function contexts |
| `ED019` | Record assembly without exact identity/availability | DAE accepts complete one-level layouts with source-point availability |

## References

- [SPEC_0008](SPEC_0008_PHASE_ERRORS.md) — governing diagnostic and acceptance rules
