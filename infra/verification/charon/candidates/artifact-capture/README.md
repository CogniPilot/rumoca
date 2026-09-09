# Artifact capture: publish only what exists, never state an identity without a crate hash

## Defect

`closure-region-facts` records closure region facts beside every artifact a
dependent can consume (metadata, rlib, dylib). `PendingArtifact::capture`
(`src/artifact.rs`) refused on rustc errors, then built its records, then
`Identity::current`, which calls `tcx.crate_hash`, and only then asked
`emitted_artifacts` which sidecars there were. rustc computes the HIR hash
behind `crate_hash` only when `needs_hir_hash()` holds
(`rustc_middle/src/ty/context.rs:1137`: debug assertions, incremental, metadata
needed, coverage or metrics). For an executable built without incremental it
does not, `OwnerInfo.opt_hash` is `None`, and `crate_hash` panics
(`rustc_hir/src/stable_hash_impls.rs:35`). Every dev-profile run hid this
because cargo's default incremental makes rustc compute the hash; the chain's
nix check phase (`IN_CI=1`, release, no incremental) failed 8 of Charon's 11
cargo-mode tests on every candidate, base included.

## Change (`artifact-capture.patch`, two files)

`capture` returns `Capture`, an opaque struct whose only state is a private
`Option<PendingArtifact>`: `Some` for a crate with artifacts to publish, `None`
for a crate with none. Only `capture` can construct it, so no other module can
declare that a crate has nothing to publish; the disposition comes from the
complete artifact list inside `capture`. Order: the rustc
error refusal first, unchanged; then `emitted_artifacts`; empty means
`NoArtifact` and no identity is computed, because no dependent can consume an
executable's facts; otherwise, if rustc computed no crate hash for an artifact
that does publish, the capture is refused with a typed
`TransportError::InvalidArtifact("rustc computed no crate hash for an
artifact that publishes facts")` rather than a fabricated or partial identity;
otherwise records and identity exactly as before. `Capture::publish` publishes the
pending artifact and does nothing when there is none. `session.rs` stores
`Option<Result<Capture, TransportError>>` and publishes through it. `Identity`,
`Artifact`, `Record`, the sidecar format and `LoadedArtifact::load` are
untouched: no schema change, no missing-sidecar bypass, no incremental
workaround, no disabled test.

## Controls (`controls/*.rs`, `logs/frozen/`, `run-evidence.json`)

Authoritative rows (`rows` in the receipt) were run against a frozen, read-only
tool pair under `~/rumoca-checkpoints/` (`frozen_tools` in the receipt names the
directory and every executable by absolute path with its sha256); each row
carries the literal argv, the absolute cwd and the full environment it ran with.
The earlier rows are kept under `attempt1_rows`, unchanged and labelled
superseded; one of them referenced a ui-suite log that was never produced and
says so.

Direct rustc mode with `CHARON_EMIT_ARTIFACTS=1`, base driver against repaired
driver (both cargo dev builds of the composed retention source):

| control | base | repaired |
|---|---|---|
| executable, no incremental | rustc panic, exit 101 | exit 0, no sidecar |
| executable, `-C incremental` | exit 0, no sidecar | exit 0, no sidecar |
| executable, `--emit=metadata` only | rustc panic | typed refusal, exit 1, no sidecar |
| library, `--emit=metadata` | sidecar for the rmeta | same |
| dependent reaching the library's closure | loads, exit 0 | same |
| sidecar `artifact_digest` tampered | "stale or mismatched dependency facts" | same |
| sidecar `crate_hash` tampered | same refusal | same |
| sidecar removed | `DependencyNotBuiltByCollector` | same |
| library with a rustc error | no sidecar, exit 2 | same |
| `IN_CI=1 CARGO_INCREMENTAL=0 cargo test --test cargo` | 3 passed, 8 failed, 7 panics | 11 passed |

The last row is the nix check phase reproduced locally to the number. Mixed
targets are inside that suite (`multi-targets`).

Limit: under the Miri sysroot the driver disables codegen for a translated
crate, so a plain `--crate-type lib` in direct mode writes no rlib sidecar on
either driver; libraries publish through their metadata, which is what the
cargo-mode suite and the consumption controls exercise. That is pre-existing
behaviour and not changed here.

## Retained artifact

`build.nix` evaluates `package.nix` (chained on `associated-demand-retention`)
with the pinned Aeneas flake, as the other candidates do. Its check phase is
the oracle; the result is recorded in the receipt as observed.

## Nix result

Derivation `27hjcbwvhpbdn35f966isacx6bb8j08j-charon-0.1.248.drv`: the check phase now passes Charon's cargo suite
(11 of 11, `multi-targets` included) and then fails the ui snapshot suite,
422 passed and 18 failed, no store path. The 18 are closure and lifetime
snapshots (`closures.rs`, `simple/closure-*.rs`, `issue-323-closure-borrow.rs`,
`issue-394-rpit-with-lifetime.rs`, `monomorphization/closure*.rs`, ...). They
fail identically under `IN_CI=1` on the base tree and on the repaired tree
(`logs/base/ui-suite-in-ci.log`, `logs/repaired/ui-suite-in-ci.log`): the
chain's own closure patches change that output and no chain patch updates the
expected files. That is the next inherited blocker for any store path on this
chain and is outside this packet.

## Revision: opaque carrier

Review asked that the no-publication disposition be unconstructible outside
`capture`. The enum was replaced by the opaque struct above; the two
dispositions and the zero extra checks are unchanged. Every control was rerun
on the revised bytes (receipt rows tagged `repaired2`, driver `7f6d684f`):
identical results, cargo suite 11 of 11 with incremental off. The retained
patch is the clean two-file diff; the nix build that ran on the revision took
a byte-different file with identical hunks and stray diff lines, recorded in
the receipt.
