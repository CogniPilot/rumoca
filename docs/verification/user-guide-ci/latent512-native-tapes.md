# Full Latent512 native tape failure and repair

The original c85128c50cd6e65198a712578c80679afc01aeaf guide test compiled
unchanged `NeuralLatentOscillator.mo` with the guide's documented width512
parameter declaration, then aborted during native initialization on its default
Rust test-thread stack. Width256 completed first, with 71,435 parameters in
27.174211 seconds. Width512 has 273,931 parameters. Neither source dimensions nor
stack settings were altered for this proof.

The sealed original executable and PID2692554 core are bound in
[the portable receipt](latent512-original-core-receipt.json). Their copied hashes
match the original files. The original coredump stack identifies
`CompiledAssignmentSchedule::call_with_external_tables` below the faulting
anonymous JIT code, called by Solve causal refresh during FMI initialization.
The native function's epilogue releases a 2,253,424-byte stack frame. Its parameter
copy loop spans 262,144 cells (512 squared), and the faulting instruction stores
cell17,416 into the stack-backed register tape. This isolates execution storage;
compilation completed before the fault. GDB's missing build-id/core-match and
libthread_db warnings are retained. It cannot unwind across anonymous JIT code,
so later nonsymbolic frames do not establish a recursive Rust compiler failure.

The responsible owner is `rumoca-exec-cranelift`, which physically materializes
already-checked Solve registers. SPEC0007's execution boundary and SPEC0032's
compact runtime tensor rules permit physical backing storage without creating
scalar IR children. SPEC0029 confines the private native ABI and unsafe memory
access to the existing execution adapter. SPEC0043's checked typed capture/result
contracts and SOLVE-C62 recursion limit remain in force; no source-language,
checker, evaluator, IR schema, or public API changes are needed.

Candidate9707b8d1f05e9720b1c04ff74adf1462b91fc1d4 gives each complete native
invocation a caller-owned scratch arena. Every generated helper acquires a frame;
all success/error returns release it, while failed frame acquisition releases
nothing. Former materialized stack slots retain only pointer-sized handles.
Their compilation-issued physical slot indexes select exact 64-bit-cell buffers.
Frame/site descriptors may move, but their separately allocated cell backing
cannot move while active. An active site's changed extent refuses; only inactive
frames can resize. Parent captures and output pointers survive nested and
recursive calls. Repeated inline Fold iterations reuse their static site instead
of allocating per iteration. Allocation failures propagate the private status
ABI, and the invocation wrapper checks balanced frame release before dropping
all storage. The existing complete-coordinate result cache retains its owner
and exact input/result bits.

The first actual integrated qualification at
c7b55f8a64539bfb46991a400b0cab6506e9c836 compiled the native crate and ran all
four scratch controls: three passed, while the pointer-sized handle control
panicked because entry acquisition requested the layout entry before Cranelift
had inserted its first instruction. The unchanged entry construction used by
production callers has the same ordering. This was a construction failure,
not an allocation, resource, or JIT execution failure.

Followup d1b16e8216d47b56712a4a1822637b7caef2c9ce obtains the entry arena from
the selected construction entry's appended parameters, and derives host-call
pointer type from that explicit arena argument. Subsequent accesses continue to
read the established layout entry. The same control now also checks that the
layout is absent before acquisition and that its parameter remains correct after
switching to the body block. The followup is runtime-unmeasured.

The retained attempt is `pr427-native-heap-and-read-after-c7b55-20261011`.
`heap-scratch-four.log` SHA256 is
`4cf429a90db5b079f603fac10f94779ab533fedc308540c80f76debdd6bcd35f`;
`heap-scratch-four.guardian.json` SHA256 is
`86abe7c8f27753aa4afcd5b37e21d506088d5c3443b1be61ee168d2144f81f4b`.
The gate exited101 after61.978 seconds, with peakRSS2,645,508KiB and no resource
stop. Later native ABI, whole-crate and full Latent guide gates did not run.

Focused qualification commands (now executed on the integrated source below):

```bash
cargo test -p rumoca-exec-cranelift --lib emit::scratch::tests:: -- --nocapture
cargo test -p rumoca-exec-cranelift --lib emit::register_storage_tests::full_width_tapes_execute_assignments_residual_batches_and_jvp_on_default_stack -- --exact --nocapture
cargo test -p rumoca-exec-cranelift --lib full_width_typed_integer_tapes_preserve_exact_cells_on_default_stack -- --nocapture
cargo test -p rumoca-bind-wasm --lib --features full-web user_guide_latent_width_variants_use_the_full_authored_source -- --nocapture
```

The scratch filter selects four controls: stable parent pointers through site and
frame growth, repeated-site storage reuse, invalid request refusal, and
pointer-sized physical stack handles. The next two tests preserve full512-squared
physical extents and independently check exact output bits, including wide
Integer cells, signed zero, cache hits and checked Integer failure cleanup. The
final existing guide test retains both full authored width variants and native
initialization/steps.

The actual integrated AFTER at d65f1f659e1c5b8cf4f107b5ae901eb05b8006b1 passed
scratch4, full512-squared native ABI1, full512-squared exact Integer1, and all138
Cranelift unit tests. The filtered controls overlap the full unit suite, so those
counts are not144 unique tests. The original guide width test separately passed
both256 and512 on the unchanged default stack, using the unchanged model with
only its guide-documented width declaration substituted. It initialized and
advanced to0/0.01/0.02, checking all eight public latent states for finite values.
This is a short runtime interval, not the full authored experiment horizon.

The model test timers were29.576781 seconds for256 and118.118849 seconds for512;
the complete guide test reported148.25 seconds after3m07 Cargo compilation. Its
guardian elapsed336.266 seconds, peakRSS3,548,748KiB, exit0 with no resource stop.
The log's parameter counts come from the documented fixed width/count roster;
they are not a newly measured runtime storage-cardinality query. These timings
are not cold-build benchmarks or a measured speedup. The private ABI tests do
not issue a public signature/WIT identity claim.

The [portable AFTER receipt](native-heap-after-d65f1.json) binds the exact source,
commands, log/guardian hashes and resources. Full guide, shipped browser artifact
and original Euler38 constructor-memory qualification remain independent; none
follows from these native storage results.
