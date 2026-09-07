# Source preservation — 2026-09-07

This is an unadopted source snapshot, not semantic acceptance of the candidate.
Codex writes; Claude independently reviews under SPEC_0033 §2a/§4/§6 and
SPEC_0037 §6. No production package or proof premise changed.

Measured checks:

- All 20 copied source/proof files are byte-identical to their originals.
- The relative-path Nix recipe builds successfully with sandboxing enabled.
- The reconstructed complete source equals the original candidate by both
  `git diff --no-index --exit-code` and NAR digest:
  `sha256-UvUVVORjjPrfeCkboWm87AAD9T1sk/ocdrPaRoQHqaA=`.
- An input-only Git-index export without a target directory evaluates to
  the same derivation as the workspace recipe. It contains only these inputs
  and their existing adopted package/patch dependencies.
- Relocated native fixtures pass 30 tests. The opaque fixture separately
  compiles but has zero native tests.
- Fresh Charon extraction and extensive Aeneas translation of the three
  proof-input modules succeed from relocated Rust. Fresh Lean checks pass
  those modules and all nine unchanged authored laws, including the new
  axiom guards on the two stored-region laws.
- The shipped Aeneas/Charon packages and adopted patches are unchanged.

The first build used standard whitespace markers on empty patch context
lines. Final formatting removes those markers only. Final-byte reconstruction
also builds, preserves the exact source NAR digest, and builds from the detached
input-only export with the same derivation. The mutation patch passes
`git apply --check`; scoped staged `git diff --check` passes.
Claude issued **ACCEPT for preservation only** at 2026-09-07T15:24:42Z.
It independently applied the implementation patch with zero fuzz, compared
the reconstructed source, checked all 180 hunks for anchoring, and verified
unchanged live package digests and absent scratch dependencies. This verdict
does not accept the candidate's semantics or authorize tool adoption.

The opaque-unknown control was edited LLBC, not production source: extract
`opaque_shared`, replace that opaque declaration's region mutability with
`Unknown`, and require refusal. Do not treat edited IR as source-bound proof.
The constant-return mutation is retained as a patch and a separate generated-
code counterexample; apply it to an output copy, never the original fixture.

Historical scratch copies are disposable, not authoritative. Downloaded
upstream trees, generated IR/Lean, binaries, caches, logs and abandoned
candidates are not vendored. Existing axiom-printing conventions are preserved
unchanged here; broader proof-guard audits are outside this preservation cut.

Canonical pilot/driver/tracer adoption gates and quick/full/CI were not rerun
for this packaging-only snapshot. The last quick remains red. Semantic review
and actual DAE translation remain open; UnitDerivative remains one of 18.
