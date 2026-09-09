# ui snapshot expectations: inventory of the chain's 18 failures

Diagnostic only. Nothing is blessed here; no patch is proposed. Inputs:
`IN_CI=1 cargo test --test ui` on the composed base tree
(`ui-suite-base-in-ci.log`) and on the artifact-capture tree
(`ui-suite-artifact-capture-in-ci.log`), both 422 passed and 18 failed with the
same set, identical to the nix check phase. One diff per test under `diffs/`
(snapbox format: `-` expected only, `+` actual only).

## Premise

The expected side is not upstream's text for 15 of the 18: `capture-arguments.patch`
rewrites those `.out` files (upstream `no-gen-tuple-structs.out:372` reads
`closure<'1>`, the chain tree reads `closure<'2>`). Only `issue-394-rpit-with-lifetime`,
`simple/foreign-inline-const` and `monomorphization/issue-708-from-str-mismatched-generics`
compare against unmodified upstream text. The chain's own notes record the set
at the constructor stage: `method-constraints/constructor.md:90` ("16 snapshot
differences remain unapproved; two actual extraction refusals need foreign
facts") and `transport.md:114` ("Same failure set as predecessor"). Patches
after constructor (transport, explicit-self, finite-demands,
growth-edge-retention) do not touch closure translation, so by elimination the
owner of every remaining difference is `constructor.patch` (INFERRED; no
constructor-only binary exists for a bisect).

## Inventory, by severity

1. Refusals, a behavioural change against upstream: `simple/foreign-inline-const`,
   `monomorphization/issue-708-from-str-mismatched-generics`. Both fail with
   `error: Closure region facts unavailable: DependencyNotBuiltByCollector(...)`,
   the first for an aux crate compiled without a fact session, the second for
   the full-MIR sysroot's `liballoc` while translating `alloc::raw_vec::layout_array::{closure#0}`
   under `--extract-opaque-bodies --monomorphize`. Origin: the closure-regions
   fail-closed policy (`constructor.patch` raised it for every non-local closure
   owner; `transport.patch` routed it through `LoadedArtifact::load`, which
   returns that error when no sidecar exists; ui aux crates are built by
   `charon rustc` without a session, and the sysroot is never built by the
   collector). Deliberate by the candidate's stated policy, and a regression for
   every foreign closure relative to upstream. Not blessable: needs fact
   production for aux and sysroot crates, or a recorded policy decision.
2. Region renumbering inside generated closure impl methods (`call`,
   `call_mut`, `call_once`, `drop_glue`): `issue-394-rpit-with-lifetime` (against
   upstream), and the same pattern inside `closures.rs`, `issue-323-closure-borrow`,
   `simple/closure-with-non-upvar-lifetime`. Example: `&'1 mut closure<'a>`
   becomes `&'2 mut closure<'a>` for a capture-free closure. Body region numbering
   is semantically inert, but an extra region on a capture-free closure is
   unexplained; needs a constructor-only bisect before it becomes an owned
   expectation update. Verdict: unclear.
3. New `RegionOutlives` clauses on closure impls: `simple/issue-1040-closure-upvar-lifetime`,
   `simple/nested-closure-lifetime`, `simple/nested-closure`. Produced by
   `constructor.patch`'s `closure_constraints.rs` (`regions_outlive` extended from
   the collected `required_outlives`). For issue-1040 (`|| &*s` over `s: &u8`)
   the added `'_0: '_1` is exactly the missing contract of Charon issue 1040 and
   the absent `'_2: '_1` is right (the reborrow goes through the shared upvar).
   Verdict: intended by the patch; the expectation update belongs to
   constructor. The one-way `'_2: '_1` between two un-erased return slots in
   `nested-closure-lifetime` is unreviewed.
4. Stale capture-arguments snapshots at closure construction and call sites:
   `no-gen-tuple-structs`, `simple/closure-fn`, `simple/closure-fnmut`,
   `simple/closure-capture-ref-by-move`, `simple/closure-with-remove-adt-clauses`,
   `simple/issue-988-closure-outlives`, `simple/lending-iterator-gat`,
   `monomorphization/closure-fn`, `monomorphization/closures` (and the same lines
   in the three tests of group 2). On every diffed line the actual text equals
   upstream's, while capture-arguments' other rewrites survive: constructor
   reimplements the upvar translation and restores upstream's region count at
   those sites. Verdict: not a regression; the capture-arguments rewrite of these
   lines is stale and constructor should carry the re-bless.

Labels: the premise, the chain order, the refusal texts, the issue-1040 lines
and the three-way comparison were VERIFIED by reopening the files; the
attribution of groups 2 and 4 to constructor is INFERRED by elimination.
