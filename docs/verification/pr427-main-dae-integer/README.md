# Main-DAE Integer quotient repair

The original #411 model remains unchanged in the public runtime suite. At
`a137a6d6be5eb3739a63fee00fe962f132a70fff`, the retained original runtime
qualification reported Interpreter `mod(9007199254740993+n,7)` at `n=0` as
4 (expected 5), and `div` at `n=2` as 1286742750677284 (expected
1286742750677285). The parent retains the full raw log under the attempt
`pr427-original-bug-runtime-packaging-feature-after-20261011`.

The first divergent producer is `lower/scalar/builtins.rs::quotient`:
Integer operands, including the wide literal and preceding addition, become
scalar Real operations before floor/truncation. The retained PR421 exact
Integer expression owner (24293b9e76c4c84df83d4d2f03b380508fb8927a) serves
computed function-call actuals; it does not dispatch these main-DAE builtins.
The repair reuses that owner's checked expression lowering for the complete
Integer quotient before scalar literal folding. Site identity is the issued
DAE ExprId; captures retain model-coordinate identities and free
`(DomainId, ordinal)` binders. Comprehension-owned binders remain internal.
Unsupported typed vocabulary, including nested function-call operands in this
narrow owner profile, refuses instead of taking a floating partial path.

The evaluator owns scalar ABI admission: Integer inputs must be integral and
inside the checked signed domain, and final Integer outputs must be exactly
representable. This permits exact large values above 2^53 and refuses rounded
odd values and MAX-to-2^63 saturation. Raw typed Int64 transport and explicit
Integer-to-Real conversion retain their typed semantics. Checked output
failures retain owner provenance. The event payload uses the same boundary.
Cranelift host adapters share these helpers; emitted scalar rows enforce the
same bounds. Existing native Integer arithmetic now emits signed-overflow and
result-domain checks with the existing status ABI. No native operator support
is added: Integer Mod/Rem retain their existing native refusal, so public Auto
policy results alone are not proof of native execution.

Governing contracts: MLS §3.7.2 Operators 3.4/3.5/3.6; SPEC_0022 EXPR-024
and the quotient compliance note; SPEC_0007/SPEC_0040 SOLVE-C84;
SPEC_0029/SPEC_0041 (single numeric owner); SPEC_0032 §4 (compact typed
programs); SPEC_0033 §§2–4 (first divergence and strict admission).
SPEC_0036/0037 remain drafts.

## Qualification manifest

All commands below are **UNRUN at this source freeze**. Formatting and
`git diff --check` passed. The parent owns the guarded build lane.
Use the exact same frozen source for the original regressions and controls.

| Command suffix (after `cargo test --locked --offline`) | Expected selected tests |
|---|---:|
| `-p rumoca-eval-solve --lib tests::scalar_integer_payload::` | 3 |
| `-p rumoca-exec-cranelift --lib tests::scalar_integer_payload::` | 5 |
| `-p rumoca --features fmu-packaging --test suite_core reported_bug_runtime::issue_411_` | 2 |
| `-p rumoca --features fmu-packaging --test suite_core reported_bug_runtime::main_dae_integer::` | 4 |

CI's workspace verification discovers both library modules and the registered
`suite_core` child module; no ignore, selector exclusion, or relaxed assertion
is added. The original mod/div tests remain unchanged. New public controls
use Interpreter and Auto with the same compiled model for valid and invalid
inputs. Constructor-backed native tests invoke compiled raw-cell owners and
compiled scalar rows explicitly: MAX+1, MIN negation/abs, multiplication
bounds and asymmetric result domains cannot be masked by an ABI conversion.
The original runtime and new public tests do not claim forced native coverage.

The parent #403 typed-bounds conversion commit bfbf1c550 is a source dependency
(cherry-picked here as ab64c0db1); the scalar call adapter uses its canonical
`From<TypedProgramEvalError>` conversion. No broader #425 or SLAM result is
claimed by this focused repair.
