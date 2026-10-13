# Closed fixed-guest receiver proposal

Historical dependency proposal: the receiver-only phase dependency below is
withdrawn. The reviewed [complete replay relocation](checked-solve-replay-owner-proposal-20261011.md)
uses the single evaluation owner instead, preserving the execution-to-phase ban.
This document grants no platform or engine admission; original qualification
obligations remain.

PROPOSAL ONLY. The staging leaf has no WIT export or static constructor. CPU and
WASIP2 native execution still refuse. The following private issuer must be
reviewed and qualified before implementing any new platform admission.

## Exact ownership and types

`rumoca-exec-wasm::component_build` would own opaque `FixedNativeGuestCore`.
`rumoca-sim::native_fmi_ls` would own `LinkedNativeFmiLsComponent` and consume its
own private paired bundle internally, avoiding an exec-to-sim dependency. The
proposed receiver-only phase dependency is explicitly scoped below; the emitter
and native component-build tool remain phase-free.
The former retains the once-built actual Rust core,
its source/build owner, checked WIT and allocation/import contract. It has no
public raw-byte constructor. The latter is issued by one consuming composition
factory from that core and `rumoca-sim::PreparedNativeFmiLsModel`. The model
bundle remains the only input supplying both the actual emitted library and its
internally encoded source capsule. The emitter cannot depend on sim; orchestration
passes the already correlated library/capsule as one consuming private product
at the existing permitted facade boundary, not two independently selectable
public inputs. No additional encoding or proof flags enter the capsule.

The fixed guest receiver would be private to
`rumoca-exec-wasm::retained_native_program::linked_guest`. Proposed private types:

```rust,ignore
struct LinkedNativeSlot { /* fixed internal imports, no caller fields */ }
struct ReplayedLinkedNativeSource {
    component: FmiComponent,
    slot: LinkedNativeSlot,
}
struct StaticLinkedEntry { /* same slot, checked source and original entry gate */ }
```

These are not serializable, cloneable public receipts, callbacks, hashes or
caller-created markers. `ReplayedLinkedNativeSource` cannot accept an arbitrary
already-decoded root. Its private receiver invokes the existing canonical FMI6
reader exactly once on the slot's bytes; fresh preparation, metadata and stage
owners are then derived from that actual checked component. The private static
factory consumes this one replay product into existing `NativeCode` ownership.
Ordinary constructors keep their exact original-allocation checks, so an
independent equal replay remains foreign. `StaticLinkedEntry` is usable only
through the existing lease/outcome gate and same five-argument entry path.

## DRAFT canonical-reader boundary

This section proposes an amendment to SPEC_0029 §12 and its SPEC_0041 ownership
catalog. It has not been accepted by a maintainer vote or implemented. The
existing ACCEPTED execution-adapter boundary remains in force until the
coherent amendment and implementation are reviewed together.

| Proposed rule | Owner/Where | Brief justification |
|---|---|---|
| Only the fixed WASIP2 receiver may enable the optional canonical replay dependency | `rumoca-exec-wasm/fixed-native-guest-receiver` | Reuse one checked decoder without changing normal execution dependencies. |
| Declare the optional phase dependency only under `cfg(all(target_arch = "wasm32", target_os = "wasi", target_env = "p2"))` | Execution crate manifest | Native tools and browser builds retain their existing dependency graph. |
| Invoke only `rumoca_phase_solve::fmi::deserialize_fmi_component`, once for each fixed component inventory | Private receiver | Replay owns artifact derivation and checked component construction. |
| No phase-to-execution or execution-to-sim edge is added | Manifest and architecture gates | The selected production graph remains acyclic. |
| The WIT leaf consumes the opaque receiver product through the existing ME factory | Fixed adapter and solver | Lifecycle, typed storage, Reset and accepted transactions keep one owner. |

The selected graph is
`fixed WIT leaf → solver → exec-wasm → phase-solve → IR/evaluation/structural`.
`phase-solve` production dependencies are core, IR-DAE, IR-Solve, eval-DAE,
phase-structural and eval-Solve; none depends on exec-wasm. The native build
profile remains `component-build`/`fixed-native-guest-build`, without the
receiver dependency. The leaf's current public `read_source_capsule` staging
helper is not an admission path and must be retired in the receiver cutover.
Canonical serialization stays in sim's existing phase-owned encoder.

Moving only the envelope to IR does not close this dependency. Actual
`phase-solve/src/fmi.rs::deserialize_fmi_component` calls the sole
`model_wire.rs::deserialize_solve_model`; that reader validates current schema,
initial/parameter/projection correlations, constructs `VisibleProjection`,
derives continuous/initial/discrete artifacts with `artifacts::lower_solve_artifacts`,
replays reduced-chart artifacts, validates the model and finally constructs
`FmiComponent`. Relocating that complete artifact owner is a larger phase/IR
cutover. This proposal neither duplicates it nor offers a second lighter reader.

## DRAFT private receiver signatures

The following are proposed source signatures, not existing callable APIs. They
would live in `exec-wasm/src/retained_native_program/linked_guest.rs`:

```rust,ignore
struct LinkedNativeGuestModel {
    component: FmiComponent,
    code: PreparedNativeWasmCode,
}

// No arguments: read only the fixed internal source/publication import slots.
fn receive_linked_model() -> Result<LinkedNativeGuestModel, WasmCompileError>;
fn replay_linked_source() -> Result<FmiComponent, WasmCompileError>;
fn prepare_linked_entry(component: FmiComponent)
    -> Result<LinkedNativeGuestModel, WasmCompileError>;
```

`replay_linked_source` checks the imported length, allocates the destination in
the actual guest public memory, demands an exact complete copy, runs the sole
canonical reader and rejects trailing data. It accepts no independently decoded
root. `prepare_linked_entry` is private to that receiver: it derives the native
preparation, layout and original fault inventory from this one replay and binds
the fixed entry to the same `NativeCode` owner. Preparation still issues no
lease, output or start-time resolution. The product retains the component and
code together; no consuming public `into_parts` exposes selectable halves.

The WIT adapter needs one input-free bootstrap at its module-private boundary;
the execution crate may expose a hidden opaque product with borrowed component
and code access for the existing ME factory, but no constructor accepting any
caller root, bytes, slot, pointer, marker or callback. Ordinary native factories
still compare physical root/metadata/schedule owners. An independently replayed
equal component cannot bind this product. Pending resource preparation uses
`MeRetainedComponent::prepare_retained_native_wasm` on that exact component/code,
then the existing actual-time EnterInitializationMode and retained session
handoff. Reset keeps the same compiled code, original source defaults and brand.

The static entry implementation is a private code variant, not an open callback.
`NativeInvocationLease::execute` alone calls the fixed five-parameter import,
with the actual work Y/P/scratch/typed buffers, original status inventory and
complete Boolean validation. Nonzero status publishes no normal output.
`NativeInvocationOutcome` keeps the same exclusive gate until the existing
accepted transaction commits. CPU and unqualified WASIP2 paths keep their
current engine refusal; a feature selection is not a graph proof.

The receiver cannot distinguish a maliciously replaced imported body by looking
at that import's signature. Therefore the official product's authority is the
trusted whole-graph constructor retaining the actual source-built receiver,
consumed model/capsule pair and unique provider wiring. Negative body/data/provider
controls must fail there, before a final component is issued. This proposal
does not claim a runtime integrity check for arbitrarily hand-linked copies of
the guest or a sandbox security boundary.

## Bounded implementation and qualification order

1. Implement the private fixed receiver, source-derived code inventory and
   static code variant together; retire the leaf's independent replay helper.
2. Implement sim's consuming whole-graph issuer with the actual linkable opaque
   model and source-built core. Generate only the canonical source-data provider,
   validate unique providers/aliases, allocator/data/table/arena and exact WIT.
3. Complete exported WIT adapters over the existing retained/session methods;
   unsupported types, snapshots or callbacks get their owning typed refusal.
4. ROOT qualifies actual fixed-core build and positive WIT execution, then
   altered body/data/provider and separate replay negatives. Default/native
   tool/browser graph gates must prove the phase dependency remains absent.

Source-only staging and structural linkage are not any of these positive gates.

## Core ABI and trusted graph issuer

The fixed core imports these internal functions through `env`:

| Symbol | Signature | Sole allowed provider |
|---|---|---|
| `eval_assignments` | `(i32,i32,f64,i32,i32)->i32` | Actual opaque emitted model library |
| `rumoca_source_capsule_len` | `()->i32` | Source-data module generated internally from the SAME bundle |
| `rumoca_source_capsule_copy` | `(i32,i32)->i32` | Same generated source-data module; complete bounded copy into actual guest allocation |
| `rumoca_publication_token_len` | `()->i32` | Same private graph's canonical publication identity |
| `rumoca_publication_token_copy` | `(i32,i32)->i32` | Same data provider, complete bounded copy of the existing artifact UUID |

The source-data module declares its real `dylink.0` data footprint/alignment and
imports the same public memory and actual library base. Its copying function is
generated by the existing composition owner from the bundle's canonical bytes.
It is not another capsule reader or static-entry constructor. These internal
core exports never appear in the public FMI-LS WIT world.

The whole-graph factory reads the existing rendered `instantiation-token` from
the same `PreparedBuildRequest` that retains the XML and actual artifact UUID;
its final opaque result retains that request's binding and checked slot.
`LinkedNativeGuestModel::validate_instantiation_token(&str)` compares this
publication value before a resource is prepared. No new UUID mint or public
token argument exists, and this comparison grants no source/entry authority.
Packaging consumes the sealed sim result directly rather than reconstructing an
execution result from arbitrary bytes.

The **trusted issuer is complete graph construction**, not the imported symbols.
Before issuing `LinkedNativeFmiLsComponent`, it must retain and check:

* The exact actual opaque model module/body, its original source/deployment/
  metadata association, layout, typed lanes, scratch and fault inventory.
* The fixed guest's source-issued ABI and body inventory; only its prescribed
  receiver can consume that internal slot. No public registration function,
  raw function-pointer argument or generic execution callback exists.
* Actual unique provider aliases for every internal symbol; no name-priority
  selection, second model provider, unresolved import or stub can remain.
* Complete checked module/instance/alias wiring, same unshared public memory,
  actual allocator/data/table/stack relocations, retained private model arena,
  initialization order and complete expected external WIT world.
* The source-data body generated from the very same consumed bundle, whose
  capsule cannot be replaced by a caller or independent serialization.

The fixed receiver rechecks current capsule construction, supported native
schedule/profile/layout and the source/metadata ownership of every derived
product. It does **not** independently inspect the body behind a function import.
Its trust in that actual binding must follow the private complete-graph issuer
above; a standalone core guest or separately supplied import is not that issuer.
This trusted-base boundary needs an explicit SPEC_0036 construction row before
the private static constructor is implemented. A feature or platform cfg alone
cannot stand in for it. The receiver must not expose a Rust API accepting a slot,
descriptor, callback, bytes or root from callers.

The existing C-target import allowance is not the fixed guest's platform proof:
its `target.toml` declares WASI 0.2.6 imports and omits monotonic-clock. The
installed Rust std lock's WASIP2 bindings are 0.2.4. The actual once-built core's
decoded platform imports must determine a precise declared fixed-runtime
contract, including the existing timeout's monotonic-clock import, through the
same `ExpectedInterface` checker. Do not broaden the old target or allow
arbitrary WASI imports merely to make the new guest validate.

## Required counterexamples and first proof

Use the actual original ReturnPredicates library and source bundle. Wrong module
with an equal signature, separately replayed/encoded capsule, foreign original
metadata, duplicate model provider, replaced copy-body/data, changed fifth-pointer
inventory, different public memory, omitted allocator relocation and changed
fixed receiver core must all refuse at their responsible owner. Private parser
tests may refuse malformed bytes but cannot construct an admitted library or
component. Independent replay passed to ordinary native factories remains foreign.

First positive proof composes the actual once-built guest with the actual emitted
model and canonical data, extracts the complete expected WIT world, and invokes
it through the existing host. Fresh source/root correlation, exact typed cells,
two resources, non-reentry, selected IndexBounds/atomic accepted output, actual
init2→DoStep→Reset→init5 and source Missing/defaults are mandatory. The old raw
linkage fixture and capsule replay tests supply prerequisites only. Whole SLAM,
FMU publication and host update-rate remain later independent gates.
