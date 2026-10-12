# Declared discrete Real source ownership

The parsed-source boundary now enforces MLS 3.7 §4.5.3 / SPEC0022 DECL-021: a nonempty explicitly declared `discrete Real` needs an actual when-clause owner. Missing ownership reports ED024 at its declaration. Neither external-input nor clock ownership exempts that source declaration. Plain Real/Integer inputs and plain clocked Real remain supported.

The source chain is `dc8fb0a90256971afa4068815d82cd748c5c7d60` → `fb0dafc724306b9bb8bbdeaa3324adeb2cb67ddc` → `67deb142b1d0f8a74c91dd3a6c3185f061a7d279`. The last commit only removes redundant casts in a diagnostic test; production bytes equal fb0.

The validator reuses the canonical target traversal with a private WhenOnly role. Broader event targets remain separate: an ordinary `if sample(...)` observer does not prove when ownership. Algorithm element writes retain their existing whole-storage ownership and unwritten pre values. Empty domains are vacuous. Target collection runs lazily once when a relevant declaration needs it; there is no tensor enumeration or new function-definedness analysis.

The direct checked-DAE B.1b contract is distinct from source-declaration validity. Its original derivative fixture is unchanged and now numerically proves matched substitution `d=-x` for positive, negative and zero states, retaining storage/clock checks. The legal parsed-source counterpart retains `a=der(x)` and `der(x)=-x`, moving only `d=der(x)` into an ordinary sample when. That counterpart reaches Solve through the actual 67d CLI.

## Actual observations

| Snapshot / scope | Actual outcome |
|---|---|
| Historical sealed a5 CLI | Accepted the invalid unconditional declared discrete Real derivative alias; OMC checkModel/instantiateModel refuse the same full source. |
| Dirty source, before Flat fixture correction | Source13/check passed; full DAE 442 passed / 2 failed. Both failing Flat fixtures lacked genuine source when owners. |
| Clean dc8 | Source14, DAE444 and Solve198 passed; full compile 435 passed / 1 failed at an invalid metadata fixture. Lint/fmt were unrun. Its then-present clock exemption is superseded. |
| Clean fb0 | Source17, DAE444, Solve198 and compile439 passed, all with zero failures/ignored tests. Clippy failed only on two redundant test casts; fmt was unrun. |
| Clean 67d | Source17, scoped all-target/all-feature Clippy and formatting passed. Current CLI emitted checked Solve JSON for the legal derivative when, preserving x/a/d and its event-storage run. |

The two Flat fixtures now contain genuine when owners while retaining their array shape and initialization assertions. The metadata fixture changes only `z=time` into an actual sample when; all metadata assertions remain. The source binding-only refusal remains an ED024 control.

The 17 source tests cover the original derivative alias refusal, continuous-time/binding/if-sample refusals, legal when equations/algorithms, inherited and partial arrays, input distinctions, empty domains, existing overdetermination, and explicit versus plain clocked Real. A literal-false when control retains one balanced equation/variable and one discrete scalar: inactive guards do not erase source ownership. OMC independently accepts and instantiates that exact false-when declaration, preserves its equation, accepts both legal clock forms, and refuses the explicit discrete sampled equation outside when.

The source17 selection is contained within compile439; those counts are not additional unique tests. ToDAE source controls, checked-IR numerical substitution, and CLI source-to-Solve acceptance are separate proofs. The CLI proof does not claim numerical simulation. Existing source-derived event-derivative runtime controls remain independently owned by their suite.

## Reproduction and evidence

```sh
cargo test -p rumoca-compile --lib discrete_real_ownership_tests -- --nocapture
cargo test -p rumoca-phase-dae --lib -- --nocapture
cargo test -p rumoca-phase-solve --lib -- --nocapture
cargo test -p rumoca-compile --lib -- --nocapture
cargo clippy -p rumoca-phase-dae -p rumoca-phase-solve -p rumoca-compile --all-targets --all-features -- -D warnings
cargo fmt --all -- --check
```

[receipt.json](receipt.json) binds exact sources, recipes, commands, pre/post tracked-source manifests, logs, guardians, OMC inputs and sealed executables. Raw evidence remains immutable under the recorded `$HOME/scratch/rumoca` paths; no process histories or raw logs are embedded. Each gate retains 600 seconds, 4 GiB aggregate RSS plus swap, 16 GiB host reserve, 8 GiB target/scratch limits, four jobs/test threads/Rayon threads, no incremental compilation and zero debug info.

The loaned final-exports target was returned only after terminal source guards, executable sealing and a fresh exact-path audit: zero visible references, 417 denied process reads disclosed and zero lease files. This is target-specific evidence, not global process idleness. No target deletion or movement occurred.

Governing primary source: [Modelica Language Specification 3.7](https://specification.modelica.org/maint/3.7/MLS.pdf), §4.5.3; top-level input balance in §4.8 supplies no discrete-prefix exemption. Appendix B.1b is the separate residual capability. Active project owners are SPEC0007, SPEC0022 DECL-021, SPEC0008, SPEC0029 and SPEC0033.

These are focused source-bound observations. They do not qualify the final combined Root head, full workspace, all guide/browser runtimes, exports or issue closure.
