# Scalar Boolean value selection — PR427 / issue403

The unchanged original `GuardedOperands` function now returns `y=0` for `k=4` and `y=1` for `k=3`. Active or independently demanded invalid accesses still fail. This implements the scalar Boolean VALUE choice in EXPR-042 through the existing checked conditional owner; activation Boolean DAGs and Boolean tensors retain their existing semantics.

MLS 3.7 §3.3 permits the short-circuit choice and §3.6.5 specifies selected `if` branches. The previous eager behavior was permitted by MLS but did not meet issue403's requested guard behavior. The governing primary source is [MLS 3.7 operators and expressions](https://github.com/modelica/ModelicaSpecification/blob/b7361fab8cad41971d2fe55e329f09c951d3a090/chapters/operatorsandexpressions.tex#L172). Active compiler ownership rules are SPEC0007 and SPEC0029; EXPR-042 is recorded in SPEC0022.

## Actual source-bound qualification

The final runtime snapshot is `6c610bafa22515a7d9490eb100048f6340d75956`, based on `8a0853cafe290844a5ded691e721c97de9137bab`. All commands retained equal tracked source manifests and ran with 600-second limits, four workers, 4 GiB aggregate RSS plus swap, 12 GiB cache ceiling, 16 GiB host reserve, and 8 GiB scratch reserve.

| Scope | Actual result |
| --- | --- |
| Checked scalar/rank boundary, including genuine Boolean tensor `[1]` | 1 PASS |
| Original source and fault controls through main assignment schedule in Wasmi | 5 PASS |
| Original Interpreter/Auto, explicit-if, active fault, issued native helper | 4 PASS |
| Or, calls/assertions, independently demanded RHS, nested truth tables, state-root ownership and executed JSON replay | 6 PASS |
| Existing event/activation ownership, vector rises and initial/sample lifecycle | 48 PASS |
| Existing C, Rust, fixed Rust, CasADi and JAX selected-region / lazy-AD controls | 8 PASS |
| Scoped Clippy and full formatting check | PASS |

These are 72 unique passing tests, with no ignored tests in the selected scopes. The exporter tests executed the real C/Rust tools and CPU CasADi 3.7.2 / JAX 0.9.2; the C test checks `FE_INVALID` and JAX checks JIT and differentiation. No GPU is required.

The original model has no continuous states, so its Auto simulation chooses Interpreter. The native regression compiles its actual source-derived pure-call table and directly invokes the issued Cranelift helper for `k=3,4,2`, then checks active bounds failure, atomic output preservation and recovery. It has no interpreter fallback. This is not a whole-session Cranelift receipt. The source-WASM proof executes the actual main assignment schedule, not a forged function wrapper.

## BEFORE and retained unsuccessful attempts

The sealed, unpatched 8a CLI (`SHA256 6bab159379f1966e0e44266f7fbdd960efa8df75fdea666d49cb0f662a39aa58`) actually fails the exact original source at `k=4` with EX001, index4 outside1..3, bytes198..199. It uses the same explicit runtime input mapping as the permanent regression. The isolated in-bounds k3 control completes all eleven grid samples with y1. The original source SHA256 is `f999d518f24cc731b460b23f27aaf3fb05ce00bd67d9c51be35c4736c477c378`.

The receipt also preserves the stopped BEFORE compilation (zero tests), an Arc/Dae test compilation error (zero tests), a native-test preparation error from its missing runtime input (3 PASS/1 FAIL, helper unrun), and an exporter wrapper count mismatch (all eight runtime tests passed, but expected five rather than six selected-region tests). Those are not compiler counterexamples. The final corrected source and expected roster passed.

[receipt.json](receipt.json) binds raw commands, guardians, tracked source seals, retained failures and sealed test ELFs. External evidence is under `$HOME/scratch/rumoca/pr427-scalar-boolean-values-20261012`; the precise BEFORE is under `$HOME/scratch/rumoca/pr427-issue403-cli-before-8a0853ca-20261012/stdio-exact-initial-inputs`.

The separate eval-flat constant-function repair has its own qualification and must be composed to cover translation-time values. This receipt does not claim original-model execution in every export target, final whole-workspace verification, platform CI or coverage. A target-specific idle audit found zero visible references and disclosed 418 denied PIDs; it is not a global idleness claim.
