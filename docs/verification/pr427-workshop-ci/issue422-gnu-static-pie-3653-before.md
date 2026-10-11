# Original issue 422 in GNU static PIE

Source `3653c44b2b9864bf993fa9a9bbf2ca578e1df05d` was actually built and
tested through the unchanged `.github/scripts/qualify-issue422-static-pie.sh`.
The canonical memcmp test passed; the unchanged original U4 failed during
explicit pure-call-table finalization with `can't resolve libcall memcpy`.
No U4 session or curve sample executed. Ordinary native results and the
memcmp control therefore do not qualify issue 422 as fixed at this source.

| Executed test | Passed | Failed | Ignored | Evidence |
|---|---:|---:|---:|---|
| `emit::host_runtime::tests::issue_422_memcmp_relocation_needs_no_process_symbol_export` | 1 | 0 | 0 | Canonical builder resolves memcmp without process symbol export. |
| `reported_bug_runtime::issue_422_original_array_function_runs_without_native_fallback` | 0 | 1 | 0 | U4 retains one checked pure-call owner; finalization cannot resolve memcpy. |

Both actual executables passed ELF64/x86-64/DYN checks and had no INTERP
segment or NEEDED entries. The script copied and hashed each executable
before running it. Its selectors, original model, native receipts, timeout,
link flags and ELF checks were unchanged.

The development shell supplied GNU Rust 1.95.0-nightly
`6a979b3e32522049d0acb4a47f7ae44b7c8abfd5`, GCC 15.2 and matching glibc
2.42-67 static libraries. The static library directory was prepended to
`LIBRARY_PATH` inside that shell, retaining any prior search paths.

This was a cold owned target. The memcmp build took 2m03s; the independent
U4 target build took 6m21s. The complete guardian elapsed 510.162 seconds,
exit 1, with no resource stop or signal. Peak owned aggregate RSS was
2,754,628 KiB; observed peak cache allocation was 1,855,021,056 bytes. Limits
remained 600 seconds, 4 GiB RSS, 8 GiB cache, 16 GiB available host memory and
8 GiB scratch reserve; jobs, Rayon and Rust test threads were four. These
are build-plus-test costs, not model runtime measurements.

The local evidence root is derived from HOME:
`scratch/rumoca/workshop427-staticpie-20261012`. The fresh qualification
directory is `artifacts/gnu-static-pie-3653-original`. All 4031 tracked source
bindings matched their before-run inventory after termination. The complete
receipt inventories the raw toolchain, Cargo, linker, ELF and test outputs.

SHA256 bindings:

- recipe: `90c429cca88311cadfa143dc103c8d45dbf4752b0ab99e51d339d306c3db57a5`
- complete receipt: `a3d49851821cb448994c267072869d9566695a58b1f031785e4b008d2bd19da7`
- guardian: `9c5f2a0f32ab44b91dbac3477e29f9f2c25ec1b50c0c9f9e36e6d07feb03c422`
- tracked source inventory: `4b5b65af210539bce8ad425199fe88e79608fae2d951d194cc5829eea118b6f0`
- memcmp executable: `dc602eea0a55d1eab4e28be1665965115a8abde040538d64c39d99072d9f3db4`
- memcmp test log: `d1cfaca8e4d995373fb70990f911884f6b72034f332e4d5792bcf34497df28fd`
- U4 executable: `738ad54f307d0dfefed12444fcc433dcfa87376fd09741c1175e7a8f20b02045`
- U4 test log: `2b3522f5a7cab66b8e431a952696f54433999bdc2c066c66453711b33227d1fc`

The earliest observed defect is the canonical native host libcall inventory:
`host_jit_builder` explicitly registers memcmp, while the retained U4 call
also needs memcpy. No production repair or AFTER success is claimed here.
