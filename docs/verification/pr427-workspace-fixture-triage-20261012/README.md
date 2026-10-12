# Workspace fixture triage

The diagnostic workspace census at `ba258da6e325c6a9c682e6bb540d5450c22a22f7`
ran all 10,382 selected tests: 10,376 passed, six failed, none skipped.
It used the ordinary workspace selection with `--no-fail-fast`; Python and
WASM binding crates were excluded by that selection. Test execution took
356.128 seconds; the guarded invocation took 1,139.039 seconds including
preparation/build. Source manifests remained identical and no resource stop
occurred. This is a failed census, not a canonical quick/full pass.

| Failed scope | Earliest observed result | Correction |
|---|---|---|
| Four `clocked_coincident_exchange` tests | ED024 for explicit discrete Real `xs` without a when owner | Remove eight explicit prefixes in their four source constants; preserve equations and numerical assertions. |
| `event_clocks::an_event_clock_ticks_once_while_its_condition_stays_true` | ED024 for explicit discrete Real `off` without a when owner | Remove only the `off`/`dir` prefixes; preserve inferred clocks, `n`'s when owner and every numerical assertion. |
| `main_dae_integer_quotients_refuse_zero_divisors_and_checked_arithmetic_overflow` | The model compiled, contradicting `expect_err`; arithmetic controls were not reached | Exercise the unchanged Integer-input model with valid/zero divisors; retain Real-divisor ED018 and every existing overflow control. |

MLS 3.7 §4.5.3 / SPEC0022 DECL-021 requires an explicitly declared discrete
Real to have a when-clause owner. Synchronous partitioning (§16.8.1) does not
exempt that source declaration. Retained OpenModelica check/instantiate controls
refuse an explicit discrete sampled equation outside when, and accept both
plain clocked Real and explicit discrete Real inside a genuine clocked when.
These test-only corrections preserve the source validator. The intentional
negative clock fixtures and direct checked-DAE capability tests are separate.

Integer expressions with discrete operands change only at events (MLS §§3.8.5,
4.5.3); their exact `div`/`mod`/`rem` arithmetic (§3.7.2 Operators 3.4–3.6)
needs no continuous quotient surface. The checked constructor preserves static
zero refusal and runtime domain/overflow faults. The sealed `5e6f9cb9` profile
has identical quotient-producer bytes to the inspected source: the exact
unbound-input model compiled, but runtime stopped at missing input `n` because
the profile CLI cannot set inputs. Separate explicitly bound input variants
measured `d=7` → `y=5`, `d=0` → checked Integer fault, and Real inputs → ED018.
Those variants do not qualify the corrected exact-input selector. That test
now supplies `d=7` and `d=0` to the same compiled source under both existing
Interpreter and Auto policies; its execution remains pending in this receipt.

The [receipt](receipt.json) binds exact failed selectors, commands, raw logs,
source guards, resource summaries, OMC controls and profile input hashes.
The initial short-revision Nix refusal is retained as preparation-only evidence.
No production change, cache mutation, test waiver or tolerance change was made
for these corrections. A corrected combined workspace and canonical quick/full
qualification remain required.

The corrected original Integer-divisor selector subsequently passed at combined source
`2cf232f490ae1bd76ee2341f51be3a40ad1ff74f`: one test passed, zero failed,
with 1,468 unrelated tests filtered. Both execution policies measured valid
`n=0, d=7` as `y=5`, followed by the checked domain fault at `d=0`.
The same test retains continuous-Real ED018 and all prior overflow controls.
Its 0.034-second execution followed a 234-second test build; the complete
wrapper took 240.592 seconds without a guardian stop and with matching source
seals. The [combined receipt](combined-integer-qualification.json) binds the
actual command and hashes. Broad quick/full and remote CI remain separate.
