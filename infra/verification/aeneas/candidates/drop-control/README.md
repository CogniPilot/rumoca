# Drop-control discriminant switches

Candidate Aeneas prepass. Not adopted; no live pin, flag or proof changes. Composed on the
`method-constraints` candidate chain, so the predecessor is the compiler used for the current DAE
extractions.

## The failure it addresses

After a `match` moves the payload out of one enum variant and leaves the other variants
untouched, rustc's drop elaboration emits, at the end of the function, a switch on the enum's
discriminant. The taken variant's branch is empty; the fallback drops the enum and then continues
exactly as the code after the switch does. `core::result::Result::ok` (`result.rs:709-718`) has
this shape, and so does every function in the fixture that moves a payload.

Aeneas evaluates that switch by reading the whole enum value (`SwitchDiscriminant` at
`InterpStatements.ml:1027-1034`, through `read_place_check` at `InterpExpressions.ml:51-57`).
By then the two match arms have been joined, and joining `Ok(⊥)` against `Err(s)` yields a root
bottom (`InterpMatchCtxs.ml:814`). The read fails with "There should be no bottoms in the value"
and the body is not translated. Under the default `drop_as_no_op = true` (`Config.ml:626`) the
`Drop` the switch routes to evaluates to nothing (`InterpStatements.ml:805-809`), so the switch's
only remaining effect is that read.

## The rewrite

`remove_dead_drop_switches`, registered in `PrePasses.ml` before `remove_useless_joins`. It runs
only when `Config.drop_as_no_op` is already true; it reads the flag and never sets it.

A statement `Switch (SwitchDiscriminant p) { branches = [(_, taken)]; fallback = Some fb }`
followed by `after` is replaced by `taken.statements` when all of the following hold:

1. `p` is a plain local `n`, not a projection.
2. The fallback block is `Drop p :: rest`.
3. `rest` reaches `StorageDead n` through `StorageDead`, `StorageLive`, `Nop` and `PlaceMention`
   statements only, and does not pass a `StorageLive n` first: nothing assigns, returns, branches
   or reinitialises `n` between the removed switch and its discard.
4. `taken.statements @ after` equals `rest` modulo statement ids, block ids and spans.

Otherwise the statement is left unchanged. `StorageDead` is never erased: it is part of the
compared continuation and survives verbatim.

The contract is a claim about a domain, not an equivalence of interpreter checks. The switch is
rustc drop elaboration; its sole consumer is the `Drop` in the fallback, and `drop_as_no_op` never
evaluates that `Drop`. Under that mode the discriminant read has no remaining use, so removing it
changes nothing the mode observes. Condition 3 keeps the surviving `StorageDead n` as the next
access to `n` on both paths, which preserves the path and borrow checks on `n`. It does not
preserve the bottom-value check: `drop_value` deliberately accepts a bottom root that the
discriminant read rejects, and skipping exactly that check on a value that is about to be
discarded is what the pass is for. A body outside rustc drop elaboration that happened to match
the shape would be rewritten on the same terms. What the pass removes is a bottom check on a
value the interpreter is about to discard, and nothing else.

## Fixture

`infra/verification/charon/fixtures/drop-control.rs`. Extracted with `--mir=optimized` because the
`Preset::Aeneas` options set no MIR level and the elaborated drop switch only appears in local
crates at that level; dependency bodies such as `Result::ok` always arrive elaborated.

| function | shape | expected |
| --- | --- | --- |
| `take` | the `Result::ok` shape with generic payloads | rewritten, translates |
| `take_reporting` | same switch, extra `&mut` argument | rewritten, translates |
| `take_early` | the switch nested inside an `if let` arm and again at the tail | both rewritten, translates |
| `take_first` | one tuple half moved; rustc emits a drop flag, so the continuation after the switch begins with `if copy _5` while the fallback's does not | refused, still fails as before |
| `peek` | no move, no switch | unchanged |

Four native tests, `clippy-driver -Dwarnings -Dclippy::all` and `rustfmt --check` pass on it.

## Evidence

Same LLBC, same flags (`-checks -strict-joins -sequential -emit-json`), adopted predecessor
binary versus the candidate. Observed exit codes, input hashes and binary paths are in `run-evidence.json`.

| input | predecessor | candidate |
| --- | --- | --- |
| fixture, five bodies | 4 bottom errors, 4 untranslated | 1 bottom error, 1 untranslated (`take_first`) |
| `OptionBoundarySource.llbc` (`Result::ok`) | exit 1, 1 bottom error | exit 0, `def core.result.Result.ok` transparent |
| fixture with `-eval-drops` | 4 bottom errors | 4 bottom errors, output identical to predecessor |
| DAE all-four root | exit 1, exactly one failure (`Result::ok`) | exit 0, 0 bottom errors, 0 untranslated, 138 transparent bodies |

## Mutations of the pass

Each mutant is the candidate patch plus one change, built the same way.

| mutant | change | result |
| --- | --- | --- |
| `ignore-drop-mode` | remove the `drop_as_no_op` gate | killed: under `-eval-drops` the mutant rewrites and reports 1 bottom error where the candidate reports 4 |
| `ignore-continuation` | compare the continuation with itself instead of with `rest` | killed: rewrites the `take_first` control and translates it, exit 0 where the candidate refuses; generated Lean differs from the candidate by 5 lines |
| `erase-storage-dead` | filter `StorageDead` out of the replacement | killed by the witness: under `-log-debug PrePasses` the candidate logs `[storage_dead(_4) storage_dead(_1) return]` for `take` and 7 `storage_dead` lines across the fixture, the mutant logs `[return]` and 0. The generated Lean is identical, which is why a Lean comparison alone cannot catch this mutant. |

## What this does not establish

This is a syntactic rewrite justified by Aeneas's own `drop_as_no_op` semantics. It proves nothing
about Rust destructors, and it is not a permission to ignore drops in general: with `-eval-drops`
the pass is inert and the failure returns. The translation continues to trust Rust's MIR production
and Aeneas's ADT abstraction. Niche layout and memory validity are outside it.

## Reproduction

```
nix build --impure --no-link --print-out-paths --cores 4 --max-jobs 1 --file infra/verification/aeneas/candidates/drop-control/package.nix
CHARON=/nix/store/v2br63mjn55pkcfdzf6inyg007l36sj7-ocaml5.2.1-aeneas-0.1.0/bin/charon
AENEAS=/nix/store/040z8d8vfdy4ar646s6bk8bbl5b64nwi-ocaml5.2.1-aeneas-0.1.0/bin/aeneas
"$CHARON" rustc --preset=aeneas --mir=optimized --error-on-warnings --dest-file="$OUTPUT/dc.llbc" -- --edition=2024 --crate-name drop_control --crate-type lib infra/verification/charon/fixtures/drop-control.rs
"$AENEAS" -backend lean -checks -strict-joins -sequential -emit-json -dest "$OUTPUT" "$OUTPUT/dc.llbc"
```

Mutant builds compose this `package.nix` with one file from `mutations/` appended to `patches`.
Read the exit status of every command; a successful run is silent. UnitDerivative remains 1/18.

## Witness on the pass output

`-print-llbc` prints the crate at import, before `apply_passes` (`Main.ml:588` versus `:751`), so
it cannot show what the pass emitted. The pass logs its replacement under the `PrePasses` logger
whenever it fires. Running the candidate with `-log-debug PrePasses` on the fixture prints, for each
rewritten switch, the exact statement list that replaced it; for `take` that list is
`storage_dead(_4); storage_dead(carrier); return`. Under the `erase-storage-dead` mutant the same
run prints a replacement with no `storage_dead`. That is the check the mutant fails, independent of
whether later Lean simplification makes the two outputs coincide, and it observes the pass itself
rather than the source of the patch.

## The translated `Result::ok` does not yet Lean-check

Translation succeeds and the definition is transparent, but the generated Lean fails at
`OptionBoundarySource.lean:79:34` and `:80:35`: inside `def core.result.Result.ok`, the unqualified
`ok (some x)` and `ok none` resolve to the definition being introduced rather than to the Aeneas
result constructor. That is an identifier capture in the Lean emitter, outside this candidate and
unrelated to drop semantics; it is recorded as `lean_check_candidate_result_ok` in
`run-evidence.json` with its observed failing exit rather than omitted. Until the emitter
qualifies that constructor, this candidate unblocks extraction of `Result::ok`, not its Lean
admission.
