# Current-source original Euler38 qualification plan

Both prepared gates are unrun. Frozen source:
`d65f1f659e1c5b8cf4f107b5ae901eb05b8006b1`. Each recipe binds3963 tracked regular
source files, the unchanged original30 fixture, its independently reproduced
original38 derivative, input metadata, and the existing runner/count wrapper/
guardian/auditor hashes. The only nonregular tracked entry is the existing docs
live symlink. Each original test remains registered in ordinary suite_core CI;
the POSIX address-space test is Linux-only.

Only the two original nx/ny declarations change30 to38. Model name E30x30 and
every original equation/initial value remain unchanged. Original SHA256:
`dccdafed12d656db2c6758b6e0e5f7ca5d85accb89847d6715c4c6f5f5c4b89d`.
Derived38 SHA256:
`1bd65c874537a23acd43cde3d5d1879d84bf1b1c2affa685df1646d84e8c6a5d`.
The staged copies are archival input witnesses. The existing test helper pins
and derives the same bytes itself; no replacement loader or public API was added.

Execute the two independent recipes sequentially after ROOT's active native
child terminates and releases its shared-target lease. Each must execute exactly
one test, zero ignored, with the existing default CLI features:

```bash
cargo test --locked --offline --jobs 4 -p rumoca --test suite_core process_allocator_address_space::issue_425_original_38_bench_fits_a_two_gib_address_space -- --exact --nocapture
cargo test --locked --offline --jobs 4 -p rumoca --test suite_core reported_bug_runtime::issue_425_original_38_preserves_quiescent_states_and_native_receipts -- --exact --nocapture
```

The CYCLE attempts are `pr427-original-euler38-memory-d65f1-20261011` and
`pr427-original-euler38-states-d65f1-20261011`, with recipe SHA256 respectively
`2c941baa267cd7721fb59d3f70f95acd5d5da1c4c86ca98c0251ad6204cb2306` and
`6ef22776960d882b592a6160cd2fa60b2a8181795805880ba6f6f481ba01c72c`.
Use the existing `run-owned-cargo-parallel-safe-20261011.mjs` runner once per
attempt. One failed attempt does not suppress the independent other test.

The benchmark child retains RLIMIT_AS2GiB and RAYON_NUM_THREADS1. The outer
guardian retains600 seconds, aggregate process-groupRSS4GiB, owned cache8GiB,
available host memory16GiB and scratch-free floor8GiB. These distinct limits are
not interchangeable: AS success does not measure peakRSS, and outer groupRSS
includes the Cargo/parent processes. No limit, test-stack setting or source
dimension is raised. A guard stop before tests is an unmeasured outcome.

The read-only active-build snapshot showed target6,904,303,950 bytes, leaving
about1.686GB below the cache cap, with scratch150,857,097,216 bytes free and
MemAvailable56,435,804KiB. This is not an idle preflight or a future capacity
guarantee. Run the normal fresh lease/reference/resource preflight before each
attempt; additional default-feature linking can consume the remaining cache.

The memory command retains the original ordinary default native preparation
route. Its JSON identity/timing and all11-point benchmark checks do not alone
prove an engine choice. The companion full-state test independently requires
Cranelift, no refusal and no declined native capabilities at every sample. It
checks all5776 states and exact quiescence through0.1. Neither test is evidence
that the Latent heap-tape repair fixed the earlier Euler constructorOOM before
these original-path outcomes actually execute.
