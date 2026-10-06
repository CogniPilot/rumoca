# Compact native array assignment admission

Work in progress; this record does not certify a released compiler or full SLAM.

The unchanged six-model `ES15FilterStep` composition has source SHA-256
`99ce45b42482b79211913840a57b63b91471ab3ef00d27fb85d289e153886d48`.
Its checked canonical Solve source has 72 scalar-program owners terminating in
compact `StoreOutputRange` operations. Fourteen own 225 matrix outputs. The
native assignment constructor currently refuses the first nine-output rotation
copy because it admits only single terminal scalar stores. Actual Modelica
compilation and structural lowering succeed; no runtime input defaults or host
equation rewriting are needed to reproduce this admission refusal.

Grounding before implementation: SPEC_0032 sections 2/4 retain structured
ownership and compact tensor execution; SPEC_0036 Solve construction and
SPEC_0043 sections 6/6a require exact source/output binding, complete original
evaluation prefixes, compact immutable producer ranges, and checked replay.
SPEC_0040 SOLVE-C26 requires exact target isolation for generated assignments.
MLS sections 8.3.1 and 10.6 retain equality and array arithmetic semantics.

The intended profile extends the owning native constructor, not a host consumer.
One complete terminal range maps injectively to one bounded contiguous target
range. A direct elementwise subtraction has an exact target-load operand and a
target-independent opposite operand. Independence is proved conservatively
over compact immutable producer ranges, without coordinate certificates.
Every original prefix operation remains ordered in the emitted value program;
only the terminal output selects the already evaluated independent value.
An independent scalar broadcast adds one compact `TensorFill` copy, because
canonical output ranges require a positive stride; no coordinate stores are added.
Pure scalar/tensor arithmetic is admitted; unproved effects, seeds, overlapping
register writes, target coupling and output aliasing remain refusals.

Required evidence: compact-growth controls, scalar/matrix/broadcast independence,
coupled-target and source-prefix negative controls, exact source/output/provenance
replay, native numerical execution and the unchanged complete Modelica source.
Focused owner tests, strict Clippy and canary evidence are separate gates.

The first actual native numerical gate exposed a separate backend refusal:
compact tensor arithmetic has no implementation in the local-register emitter.
The new native-assignment storage profile uses a bounded, private wasm32 memory
for checked program registers, with one runtime loop per compact operation.
It retains the imported public memory and entrypoint ABI; it never borrows host
scratch or exports its register arena. The existing bounded local-register
profile remains unchanged. Private register storage is limited to 64 MiB and
supports one primal lane without seeds; unknown/effectful operations remain
refusals. This target-local representation is grounded in SPEC_0032 section 4.
Multi-memory execution must be checked in both the numerical runtime and browser
before any compatibility or release claim.

Actual unchanged source now reaches an affine target-order refusal at source
node 144: the 6×16 Cholesky solve visits `(column,i)` with target strides 1/16.
Its 96 targets are a bijection over one contiguous range, but the original native
family profile requires target order to equal domain order. The extension keeps
the source domain/prefix order and derives a compact affine target map from the
exact structural target inventory. Construction checks every supplied target,
bounds, affine equivalence, and complete injective coverage; it stores only the
rank-sized map in the issued value kernel. Coupled target reads, non-affine
permutations and holes remain refusals. SPEC_0040 SOLVE-C26 and SPEC_0043 section 6a
ground this target isolation/provenance extension; no source loop is reordered.

The repeated unchanged-source gate passes those two admission gaps and next
refuses node 167: six targets 8382/8398/8414/8430/8446/8462 have stride 16. They
do not own every slot inside their bounding interval. The current stage API
owns exact dense ranges; treating this bounding interval as ownership would
invalidate disjointness/dependency checks. A compact sparse/strided target-owner
extension and exact set-overlap/coverage proof were the next required work.

The constructor now owns compact periodic blocks: a bounded storage span,
actual scalar count, positive block stride and block width. Width-one blocks
are arithmetic progressions; contiguous targets retain their dense range API.
`target_range()` returns no range for a sparse stage; `target_span()` explicitly
returns only its storage bound. The exact original structural inventory proves
the affine map and injective block coverage. Pairwise exact intersections plus
the summed owned cardinality prove complete Y coverage. Progressions use bounded
congruence arithmetic; wider blocks query disjoint intervals without storing
per-coordinate proof objects. Affine reads with one gap use the same exact set;
unsupported read shapes conservatively retain their bounding read span and
cannot manufacture independence. Source order, prefix, output and replay
identity remain unchanged.

The direct-write backend preserves these gaps and verifies the output storage
bound separately from owned cardinality. The separate-stage copy ABI explicitly
refuses sparse targets. The single-program exporter uses profile
`native-direct-program-f64-v2` with mandatory stride and block-width provenance.
The staged application decoder admits v2 exact sets and keeps the dense v1
contract for older review artifacts. No production package pin was changed.

Constructor tests cover interleaved columns, complementary rectangular slices,
non-affine/aliased/missing targets, real dependency cycles, exact replay and
finite-set intersection oracles. Native execution tests check every output bit,
original unused prefixes, all input bytes and gap preservation at 160×90.
The unchanged filter passes the old column refusal and next requires the
rectangular-slice profile at node 174:15 blocks of width6 and stride15. Its
complete source gate was then repeated after this extension: actual compilation,
lowering and native emission pass, with 1,212 issued stages in a 636,340-byte module.
Nine independent numerical cases pass in Node and a dedicated Chromium worker,
including all 225 covariance entries, 252 public outputs per case, explicit 337 P
values, poisoned prior Y, input/guard immutability, recovery and atomic ABI
refusals. Warm worker execution averages 0.2241 ms over 1,000 calls; copying the
filter inputs/public outputs raises this to 0.2273 ms. This excludes GPU image
transfer, frontend registration and all other SLAM/graphics work. Full IR 383,
native array execution 9, binding/source-edit 7 and scoped strict Clippy gates
pass. Full-filter edits/persistence, native package integration and Tier1 canary
remain pending; no full SLAM, 10× whole-pipeline or release pass is claimed.
