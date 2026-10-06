# Native stateless assignment preparation

This branch adds `prepare_native_assignments(source, model_name)` to the browser
binding with the explicit `native-assignments` feature, included by `full-web`.
Compile/LSP-only builds keep this optional runtime surface disabled. Its profile
is `native-direct-assignments-f64-v1`. It exports CPU WASM
from the canonical Solve model's construction-issued native assignment stages;
it does not evaluate residuals and label them as assigned values.

The binding routes lowering through `rumoca-sim`'s checked native-preparation
facade. That surface assembles the Solve model and host-input defaults without
constructing a solver or advancing time. It shares the existing GPU/FMI input
preparation rule: declaration bindings or checked start attributes supply values
until the host writes live inputs before each dispatch.

## Admission and ownership

The initial profile requires a complete stateless inventory of rectangular Map
or AffineStencil families. Each family has one scalar output per point, a dense
logical output map, one bounded dense solver-Y target range, and an exact direct
isolator issued by the existing Solve assignment-shape constructor. Target
ranges must uniquely cover the complete Y layout. All scalar source operations
are retained before the selected value projection. The profile accepts pure
scalar arithmetic, comparisons/selections, time, and bounded affine Y/P loads.

Construction rejects reads of neighboring targets in the same family, overlapping
targets, dependency cycles, unsupported node/operation families, varying
constants without a coefficient proof, repeated affine stride records, and
out-of-layout addresses. It derives a deterministic topological family order
from the checked affine read ranges. Interval dependencies are conservative:
an unsupported or overconstrained family falls back to the existing numerical
projection in ordinary Solve consumers; the native preparation API refuses it.
States, initialization equations, events, clocks, and external tables are outside
this preparation profile.

`ContinuousRefreshOwners` owns the resulting schedule. The certificate and final
value projections are derived data and are absent from serialized Solve IR.
Decoding reissues them from canonical source equations, structural row targets,
and the checked variable layout. Canonical source replacement invalidates an
already issued schedule. No new wire schema or reader for older schemas is added.

## Portable artifact ABI

The JSON artifact includes source/module SHA256, compiler version and revision,
Solve schema version, canonical variable/input layout, parameter defaults, and
ordered stages. Each stage contains portable module bytes and an exact target
Y start/count. The bytecode imports an **unshared** wasm32 `env.memory`, imports
the `env` Math functions it uses, and exports:

```
eval_residual(yPtr: i32, pPtr: i32, time: f64, seedPtr: i32, outPtr: i32)
```

The existing export name identifies the shared expression-kernel ABI. These
particular modules contain compiler-issued **assignment value** projections.
Pointers address f64 buffers in little-endian linear memory. The artifact gives
offsets, capacities, and required memory pages; memory is bounded to 64 MiB.
Initialize P from the supplied parameter vector, then write every live input
slot before each dispatch. Invoke stages in their issued order and copy each
complete temporary output into that stage's exact target Y range before invoking
its consumers. This host copying implements the compiler-issued storage map;
it contains no Modelica mathematics or scheduling heuristics. Intermediate
families are evaluated once per dispatch and reused by subsequent stages.

The caller supplies ordinary `Math` functions by explicit names from module
imports, plus its unshared memory. A threaded/shared-memory compiler build must
emit portable bytes without instantiating the kernel in its compiler memory.

## Current compilation boundary

Algebraic structured-family lowering uses the existing phase-owned affine
program certificate. That producer still enumerates source-domain points to
prove uniform affine accesses. General structural analysis also still materializes
some scalar relations. This patch provides compact native execution and
intermediate reuse; it does **not** prove bounded full-resolution Harris compile
time, complete tensor-family lowering, GPU execution, or a complete INS/ESKF
backend. Unsupported tensor arithmetic and clamped accesses remain explicit
capability refusals.

## Focused evidence

The owner tests exercise image-sized domains without introducing per-pixel
assignment metadata, reversed canonical source order, exact source-prefix
retention, dependency cycles, coupled neighboring targets, duplicate stride
records, overlapping targets, out-of-bounds loads, and source replacement.
Replay also rejects residual sign changes and reordered logical output maps
even when they produce identical final assignment kernels.
Constant replay uses IEEE binary64 identity: replacing a canonical `+0.0`
constant with `-0.0` invalidates the old certificate. The signed-zero regression
failed before the bitwise comparison and passes after it; reissuing from the
replacement source remains valid.
The binding fixture compiles actual edited Modelica equations for raw RGB to
grayscale to nonlinear scores, executes the resulting modules in Wasmi for
changing inputs, and compares all grayscale/scores bit-for-bit against an
independent arithmetic reference. A canonical Solve wire round trip must
reissue the native stage inventory. Test completion and the measured MSL canary
are reported separately after the branch freezes.

The final strengthened focused suite passes:

```sh
cargo test -p rumoca-ir-solve -p rumoca-bind-wasm --features rumoca-bind-wasm/native-assignments native_assignment
```

It executes seven owner tests and three binding tests. The full Solve IR
library passes 356 tests. Strict all-target/all-feature Clippy passes for Solve
IR, the execution adapter, and the browser binding. Before the final source
projection-fact strengthening, the five affected packages passed 1,077 ordinary
tests and ten active doctests, and strict all-target/all-feature Clippy. The
combined branch still requires its final workspace gates and measured fixed-20
canary; those earlier package results are not substituted for the final gates.
The first combined workspace attempt passed formatter and strict workspace
Clippy, then exited 101 during quiet test compilation with an empty diagnostic
log. Its cause is unconfirmed, and it provides no workspace-test pass evidence.
The subsequent captured workspace run passed formatter and strict workspace
Clippy, then reported three architecture failures: the binding bypassed its
facade, three new totality assertions exceeded pinned ceilings, and two Clippy
exception comments lacked the required directly preceding format. The repair
routes through optional native preparation, keeps failures fallible, and fixes
the comments. Gate ceilings and dependency bans remain unchanged. The new
host-input regression verifies declared start values survive preparation;
the full combined retry is still pending.
The repaired API's four focused binding tests pass, including actual Wasmi
execution of changing input/source values and declared host-input starts. The
three failed architecture checks pass against the repaired source. Solver-free
preparation also exposed an existing facade feature-gating error: its public
correlated lowering called an override helper gated behind solver features.
The helper is solver-neutral and is now available to that public caller; the
FMI module is gated to its actual FMI/solver users. The focused native feature
build passes without enabling a solver. Broader combined checks remain pending.

`lower_model_to_solve_json` can now expose existing canonical Map nodes for
admitted algebraic families instead of scalar program expansion. Consumers
that implement only ScalarPrograms must reject those nodes explicitly or use
the new native preparation API. The wire schema is unchanged; production
applications must evaluate compatibility before updating their package pin.

## Review scope and budget

The Solve IR refresh owner issues the certificate, the Solve lowering phase
preserves the canonical algebraic family, and the WASM binding consumes the
issued stages. This follows SPEC_0007/0040, SPEC_0029/0041, SPEC_0032,
SPEC_0021, SPEC_0033, and SPEC_0025. The construction/proof guidance in draft
SPEC_0036/0037/0039 and the construction catalog was consulted without treating
a checked runtime certificate as a formal theorem. Modelica equations remain
simultaneous equations (MLS §8.3); only a checked causal subset receives this
optional backend profile.

The main correctness risk is accepting base-point isolation as evidence for an
entire family. The constructor independently checks whole-domain target
addresses and dependencies, including an in-bounds read that aliases a later
point's target. The main maintenance risk is diverging assignment semantics:
the implementation reuses the existing exact isolator and value materializer,
and canonical wire decoding reissues the proof instead of trusting supplied
certificates. Conservative interval dependencies can reject a usable family;
ordinary Solve consumers retain their established projection fallback.

Initial native-certification patch budget, excluding this document and later
facade/feature-gating repairs:

| Metric | Lines/items |
| --- | ---: |
| Production lines added/deleted | 791 / 6 |
| Test lines added/deleted | 546 / 0 |
| Public items added/removed | 11 / 0 |
| Files touched | 14 |
| Net lines added | 1,331 |

Line deltas include the focused Cargo dependency changes and corresponding
Python wheel vendor pin. Public declarations
were compared mechanically against the preceding branch commit, excluding
reexports and crate-private declarations. The permanent public surface consists
of three opaque proof/refusal types, seven owner/stage access or construction
methods, and one browser preparation function. The production growth implements
the missing whole-family assignment proof, canonical replay validation, and
portable artifact binding. The compression plan is to share affine address
range evidence with the existing IR dependency owner when its certificate
accepts native families; the first profile deliberately avoids a new general
tensor solver or a second scalar assignment implementation.

The standalone browser package can be built without rebuilding unrelated
editor assets, GALEC, or Diffsol addons:

```sh
nice -n10 nix develop .#wasm --command env CARGO_BUILD_JOBS=2 \
  RUST_TEST_THREADS=2 RAYON_NUM_THREADS=2 \
  wasm-pack build crates/rumoca-bind-wasm --target web \
  --out-dir /tmp/rumoca-slam-native-full-web --release --no-opt \
  -- --features full-web
```

That directory contains `rumoca_bind_wasm.js`, `rumoca_bind_wasm_bg.wasm`,
and TypeScript declarations. It is a separate local compiler artifact; it does
not change an application's pinned package or imply package publication.

The Python binding's fixed-output dependency pin was refreshed for the new
lockfile and verified without building a wheel:

```sh
nix build .#packages.x86_64-linux.rumoca-python.cargoDeps --no-link \
  --print-out-paths --max-jobs 1 --cores 2
```

The dependency staging hash is
`sha256-vLwviSbDqSXzvNrjNPG0hm0CPDwlbhzXoibe8hKbG6o=`;
the successful normalized vendor output is
`/nix/store/4jj00vvaflfsvgr5pacwfyrl78kdf6in-rumoca-0.10.1-cargo-vendor-vendor`.
