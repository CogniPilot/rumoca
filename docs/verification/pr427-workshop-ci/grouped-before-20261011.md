# Grouped actual before evidence

Root executed five focused attempts against exactly
`8e5ba0c3778ca705ce8774ae23c0c885d82accf2`. Eight tests reached execution:
**2 passed, 6 failed, 0 ignored**. All five guardians terminated with Cargo
exit code 101 from assertion failures; no resource guard fired.
The passing controls were the small dense solve and constant-predicate JAX
function. This receipt does not claim full CI, GPU execution, or final workshop
qualification.

| Defect class / attempt suffix | Tests pass/fail | Actual first divergence |
|---|---:|---|
| `cross-before` | 0/1 | Scalar overlapping cross-product projection produced `[-3,30,-135]`; original checked execution and the independent expectation were `[-3,6,-3]`. |
| `dense-budget-before` | 1/1 | Small dense solve matched checked native and independent results; oversized final scalar operand expansion returned a product instead of the existing budget refusal. |
| `browser-provenance-before` | 0/2 | Standalone and workspace previews generated unresolved source comments for checked declaration identities. |
| `rendered-path-before` | 0/1 | Literal C path rendered successfully, then the same valid path expressed through the model-name template returned an empty-source preview error. |
| `jax-selected-region-before` | 1/1 | Native values/JVP and actual exported RHS/JIT/forward AD passed at both signs; reverse Jacobian and gradient were `NaN`. The constant-predicate control passed. |

Full attempt names, exact Cargo argument lists and required executed counts,
raw log/recipe/guardian/preflight/terminal-audit SHA-256 digests, and resource
measurements are in [grouped-before-20261011.json](grouped-before-20261011.json).
The JSON contains no source inventory or generated compiler cache. Raw artifacts
remain under `$HOME/scratch/rumoca/slam-integration-cycle-20261010/` in each named
attempt; they are not copied into Git.

Every command used `nix develop .#full`, locked offline Cargo, the existing
count-checking wrapper, four Cargo/Rust/Rayon jobs, disabled incremental builds
and zero debug information. Unchanged guards were 600 seconds per process
group, 4 GiB RSS, 16 GiB available host memory, 8 GiB cache and 8 GiB free scratch,
with zero owned swap. Peak observed RSS was 2,860,088 KiB, peak cache was
7,398,404,096 bytes, and the lowest scratch free value was 8,944,590,848 bytes.
Elapsed guardian times include compilation and startup; they are not runtime
benchmarks. Terminal audits recorded zero observed target references and zero
native tasks, with denied process reads explicitly unobserved.

Root's combined scalar candidate `db80f46da` includes the source repair
`879a404f4a6bdbb1d58af5d7f9bf3a994e395563`. Source review confirms all five new
consumer tests are reachable through the existing `TEMPLATE_RUNTIME_GROUPS`:

| Backend | Existing filter | New test count |
|---|---|---:|
| C | `c_ode_` | 1 |
| Rust | `rust_ode_`, `rust_fixed_ode_` | 2 |
| CasADi | `casadi_` | 1 |
| JAX | `jax_` | 1 |

The original `jax_lazy_conditionals` module's two probes are unchanged and also
match `jax_`. These seven tests are **not yet qualified by this receipt**.
The separate batched probe `8dda907a5495ae806ae44ff547394d4b8fc5de38` remains
unqualified and is excluded from the combined scalar freeze. Required final
actual workshop/OMC evidence capture and browser/CI cohorts remain pending.
