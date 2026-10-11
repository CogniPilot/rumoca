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

Focused qualification commands (all currently unrun):

```bash
cargo test -p rumoca-exec-cranelift --lib emit::scratch::tests:: -- --nocapture
cargo test -p rumoca-exec-cranelift --lib emit::register_storage_tests::full_width_tapes_execute_assignments_residual_batches_and_jvp_on_default_stack -- --exact --nocapture
cargo test -p rumoca-exec-cranelift --lib full_width_typed_integer_tapes_preserve_exact_cells_on_default_stack -- --nocapture
cargo test -p rumoca-bind-wasm --lib user_guide_latent_width_variants_use_the_full_authored_source -- --nocapture
```

The scratch filter selects four controls: stable parent pointers through site and
frame growth, repeated-site storage reuse, invalid request refusal, and
pointer-sized physical stack handles. The next two tests preserve full512-squared
physical extents and independently check exact output bits, including wide
Integer cells, signed zero, cache hits and checked Integer failure cleanup. The
final existing guide test retains both full authored width variants and native
initialization/steps. Whole native crate tests are needed afterward to cover
existing nested Fold, selected conditional, recursive and projection callers.
Only rustfmt and `git diff --check` have passed on the candidate. No repaired
runtime outcome or complete-guide success is claimed yet.
