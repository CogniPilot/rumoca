# Retention rotation

Read-only diagnostic on the `associated-demand-retention` candidate. No producer change, no
adoption, no proof credit. It asks one question the retention packet's order test does not answer:
when a cycle has more than one Self-changing edge, does the retained set depend on declaration
order or on which trait the traversal enters first?

## Why the question is open

`retention_owner` in `growth-edge-retention.patch` returns the first Self-changing edge found
scanning from the rotation at which the root entered the cycle. The retention packet's fixture
(`Ring`/`Field`) has exactly one such edge, so every root resolves to the same owner by
construction, and its order test, which permutes the declaration order of that fixture, cannot
distinguish a structural choice from a coincidental one.

## Tool

The exact retained candidate launcher, bound by hash to
`associated-demand-retention/run-evidence.json`: `charon` and `charon-driver` under
`/tmp/rumoca-demand-retention-QVu1kr/`, run with the retention harness flags
`--lift-associated-types=* --error-on-warnings` and the environment its receipt records. The final
LLBC of every cell is printed with the pinned Aeneas `-print-llbc`; that step exits 1 on its
`--preset=aeneas` check only after the print completes.

## Cells

`cells/` retains one Rust source per cell. All have concrete impls, all items are `pub`, and every
cell compiles with zero rustc warnings (`rustc_warnings` in the receipt), since `--error-on-warnings`
governs Charon diagnostics, not rustc lints.

| shape | cycle | cells |
| --- | --- | --- |
| `two-edge` | `A { type X: B }`, `B { type Y: A }`: two Self-changing edges | `ab`/`ba` declaration order, times root `use_a`/`use_b` via `--start-from`, plus `ab`/`ba` with all items translated (`-all`), since `--start-from` prunes the impls |
| `mixed` | `A { type X: B }`, `B: C`, `C { type Z: A }`: two Self-changing edges separated by a supertrait edge | four declaration orders `abc`, `cba`, `bca`, `acb` |
| `equality` | `A { type X: B<Y = Self> }`, `B { type Y: A }`: the cycle is resolvable by a declared equality | `ab`/`ba` |

## Comparison key

Raw LLBC hashes cannot be equal across reordered source, because ids and spans move with it. Each
cell is instead reduced to a sorted textual extract of every trait header, every retained
associated type, every implied clause, and every impl obligation (`type X = ..` and
`proof (..) = impl ..`) from the printed final LLBC, and the extracts are hashed. That key sees
retained owners, lifted generics and resolved typed obligations, and is blind to order, ids and
spans. `cells/*.extract` retains one extract per shape.

## Result

Every cell of every shape produced the same extract, `run-evidence.json` `comparisons`:

```
two-edge   4 root cells + 2 all cells   1 distinct extract each   A keeps X, B keeps Y   both retained; impls identical
mixed      4 cells   1 distinct extract   A keeps X, C keeps Z      both owners retained, B untouched
equality   2 cells   1 distinct extract   nothing retained          both lifted, impls fully instantiated
```

No divergence was found. The hypothesised split, `{A}` under one order and `{B}` under the other,
cannot arise on these shapes for a structural reason visible in the output: retention is not
exclusive. A retained trait exports no paths, so retaining `A` does not resolve `B`'s demand, and
`B` is retained in the same fixpoint. The observed rule is that every trait whose own demand is
cyclic through a Self-changing edge and is not resolved by a declared equality is retained; that
set is the maximal one, so it is independent of order and root, and it is empty when equalities
resolve the cycle.

Consequences for the retention packet's claims. "One recursive boundary" is a property of the
single-edge fixture, not of the policy: on a two-edge cycle the policy retains two boundaries.
Every divergence-free outcome here is a valid boundary choice, not a change of meaning; the impl
obligations are identical across cells. Minimality is not established, and the policy is now known
to be non-minimal in the sense of retaining every cyclic owner rather than one.

## What this does not establish

Twelve cells over three shapes are evidence, not a theorem. Cycles with three or more Self-changing
edges, generic-argument-only cycles, quantified paths, and cycles mixing equality-resolved and
unresolved edges were not exercised. Root selection was varied through `--start-from`, which
selects the translation root; the settle loop's own visitation order follows trait ids, which the
declaration-order cells vary. Nothing here changes the frozen retention packet or its producer.

## Instrument guards

Two defects in the extraction were caught during construction and are guarded in the receipt. The
printed LLBC prefixes public items with `pub`, and an extractor matching only `trait` produced an
empty extract that would have compared identical across cells; `trait_lines` and `impl_lines` are
recorded per cell and a zero is invalid. And `--start-from` translates only the root-reachable
graph, which excludes the concrete impls of a generic root, so root cells carry no impl obligations;
the `-all` cells carry them.
