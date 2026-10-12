# Final combined source qualification plan

This isolated candidate starts at root's reviewed combined source
`f6ccd3d2e4c5c15c37a0d68560681a45c82b2f22`. It applies the two provenance
fixtures from d12a20525 exactly once, the source-map repair from 6be9b262,
and the separate optional OMC evidence capture from 4069d59ed. Root's original
combined tree and every authored workshop source/default remain unchanged.

The source preview renderer now passes the source map already owned by the
checked DAE. The direct fixture's leading newline puts its authored y equation
at `input.mo:8:5`; the inherited fixture's two package header lines put the same
equation at `package.mo:10:5`. The extending input does not acquire that
inherited equation's location. These fixtures retain the exact same assertions
before and after the one source-map change. They accompany the four existing
capability/source-file/artifact-obligation tests, giving six native preview tests.
SPEC_0008 source identity and SPEC_0033 before/after evidence govern the change;
no span is substituted, checker weakened, or model rewritten.

Root receives two separate frozen recipes. The focused recipe selects:

| Gate | Required executed count / evidence |
|---|---|
| Existing Node default workspace smoke | 3 tests; every actual configured codegen scenario is fetched from tracked files |
| Actual LSP workshop requests | 4 tests; all five configured scenarios succeed, plus Ball refusal, CasADi preview and FMI2/FMI3 full source projects |
| Strict actual Modelica interchange | 2 tests; 28 completed OMC controls and 28 fresh retained source/script/CSV/stdout/stderr/status cells |
| Browser native source previews | 6 tests; two exact source-location assertions and four retained source-preview obligations |
| Actual editor surface catalog validator | 1 test; both real catalogs resolve actual implementations/proofs |

The Rust gates reuse the existing bounded focused-test count helper. Missing
OMC/timeout prerequisites fail through the existing strict marker. The evidence
root is new and inside the raw scratch attempt. Its external receipt script
requires each exact original/export cell and actual CSV, then hashes all six
retained files per cell. This is artifact accounting, not another trace oracle.
All existing structure, required physical columns, settled initialization,
exact grid and trace comparisons retain the qualified c758 tolerances.

The separate actual browser recipe invokes the existing
`cargo xtask playground test` path. That path builds the current FullWeb WASM
package, then runs the actual exported target smoke alongside existing browser
simulation/source-root tests. A native helper pass alone does not qualify the
JavaScript API. No pre-existing generated WASM is treated as fresh proof.

All recipes retain fixed four workers and the existing 600-second/4-GiB RSS,
16-GiB available-memory, 8-GiB cache/free-scratch guards. These manifests are
prepared source-only: no build, test, cache mutation, publication or adoption
has run. Root must inspect terminal capacity and execute them under its lease.
The c758 receipt remains the proof for the prior source, not this followup.

The user's authorized notebook housekeeping removes the only tracked notebook,
zero-byte `examples/notebooks/test.ipynb`. No model/configuration changes or
notebook Markdown references accompany this removal; it needs no runtime test.
