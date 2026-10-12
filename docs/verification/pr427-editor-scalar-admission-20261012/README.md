# Editor scalar-target admission

LSP rendering now checks the existing scalar derivative-program admission
before rendering any file when a target declares `scalar_fallback = true`.
It calls the same renderer query used by CLI and WASM; it introduces no new
scalar lowering or expansion. This follows SPEC_0029 §§3b and 12 and the
bounded final-template scalar-view owner in SPEC_0041 §1.

The actual `rumoca.workspace.renderTarget` regression uses the existing
checked runtime Real-to-Integer function pattern and the c-ode capability
profile, with a constant-output template. Before the correction, LSP returned
`ok: true` and `admitted.txt` despite the derivative program's unsupported
scalar projection. Afterward it refuses with
`unsupported-feature:scalar.derivative_rhs` and returns no files. The paired
ordinary Real function still renders, so the negative is independent of
template traversal and does not pass by rejecting every model.

The whole-repository Rust caller census also confirms that
`SameTickDefinitions::issue_event_read_facts` has only one same-crate test
caller. It is now `#[cfg(test)] pub(crate)`; production retains its existing
expression/condition collector. `ReadSeeds::raw` still has production callers
and is unchanged. No scheduling dependency analysis or graph walk was added.

At clean source `daccf4f7f6f5af7576b2e348d2c76100a4d268b4`, all three LSP
render-target tests and eleven structural event-fact tests passed, with zero
failures or ignored tests. Both packages' all-target/all-feature Clippy and
workspace formatting passed. The complete guardian exited 0 in 297.025 s,
with identical source manifests before and after; both executed binaries are
sealed outside the returned Cargo cache. This is focused evidence, not a
final-tip, whole-workspace, or MSL qualification.

The receipt retains the initial fixture setup failure, the corrected actual
BEFORE counterexample, and the first AFTER attempt stopped by the 8 GiB
scratch reserve before any tests or lint. Capacity was restored before the
unchanged source, commands and bounds completed. These scopes follow
SPEC_0033 §§2, 3 and 6; no earlier failed attempt is counted as a pass.

[receipt.json](receipt.json) binds the compact outcomes, resource observations,
source identities, executed binary hashes, caller census and immutable raw
artifact hashes. Raw evidence paths are relative to the receipt's `$HOME`
evidence root. The cache-return audit found no visible readers or leases;
denied process reads are disclosed rather than treated as global idleness.
