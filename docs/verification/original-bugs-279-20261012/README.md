# Original bug checkpoint at source279

Source: `279836b2d70ed9f9175a121a7a1d0a9227b1d8d0`.
All four independent gates passed with the source-derived count and zero ignored tests:

| Gate | Tests | Scope |
|---|---:|---|
| `rumoca-compile --lib reported_bug_regressions` | 9 | #423/#426 plus #424 original shape refusal and valid constructor/parameter/Boolean controls |
| `rumoca --test suite_core reported_bug_runtime::issue_ -- --skip issue_425` | 10 | #403 permitted eager profile and explicit-if protection, #405 inputs, #411 exact wide Integer, #418 JAX rendering, #422 ordinary native original U4, #423/#426 simulation |
| `initialization_parameter_conditionals::` | 4 | Immutable opposite arms, retained tunable/cyclic dependencies and compact binder controls |
| `evaluable_parameters::` | 8 | Declaration/evaluation behavior, selected dependency projection, asserted faults and actual x(1)=50 |

The three facade gates used `--no-default-features --features fmu-packaging`.
Full argv, exact test names, test executables, recipe/guardian/source-manifest hashes,
scalar resource measurements and raw log bindings are in [receipts.json](receipts.json).
All tracked source files were checked before and after each gate.
Each invocation retained 600 seconds, aggregate RSS plus swap 4 GiB, host reserve
16 GiB, target cap 8 GiB and scratch reserve 8 GiB, with jobs/test/Rayon threads 4.

The #403 Boolean test records MLS-permitted operand evaluation; it does not require
short-circuit `and`. The original invalid #424 shape is preserved and rejected;
valid reverse-order and parameter/Boolean variants remain separate positive controls.
The #418 test renders the public JAX target, but does not execute JAX. The #422
test executes original U4 with the native backend, but this artifact is not the
separate GNU static-PIE qualifier. Euler38 (#425) was deliberately not repeated.
Earlier independently qualified dedicated proofs retain their original source scopes.
This focused checkpoint neither verifies a later tip nor closes an issue or replaces
mandatory full workflow/browser/wheel/OMC qualification.

Raw evidence: `$HOME/scratch/rumoca/pr427-original-bugs-279-20261012`.
