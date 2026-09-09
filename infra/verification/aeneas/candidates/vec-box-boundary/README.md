# Vec to boxed slice: one sequence-transfer model, two current Rust APIs

Status: frozen for independent review; not accepted or adopted.
Writer Codex; reviewer Claude. Reservation through 2026-09-09T02:00Z.
Anchors: SPEC_0033 §§2/2a/4/6 and experimental SPEC_0037 §§2/6.
Predecessors are frozen Charon candidate4 and parent-clause-names Aeneas32a6f865.
Nothing in this packet is adopted into the live compiler or verification pins.

## Invariant and boundary

For every element type and input vector in Aeneas's existing sequence model,
the original Rust caller of Vec.into_boxed_slice returns the complete ordered
sequence. The current From<Vec<T>> for Box<[T]> API has the same result.
Neither operation clones elements. Native reducers use the default allocator.

This is a trusted typed standard-library contract, not a source proof of Rust's
unsafe allocation implementation. Pinned Rust vec/mod.rs:1723 shrinks capacity
and transfers storage; its From implementation at4553 delegates to that method.
Aeneas already models the latter API as Result.ok v.slice. The change factors
that existing body into the method model; From delegates to it just as Rust does.
The old From result is definitionally unchanged; its existing theorem still
holds after unfolding that delegation. Two current Rust APIs are not a legacy
compatibility path. No second sequence-transfer implementation is introduced.

Rust allocation failure/abort, custom allocator effects, pointer identity,
capacity observations, and destructor effects are outside this sequence
abstraction. The registry keeps the same allocator-parameter filtering as the
existing From model; this does not prove arbitrary allocator behavior.
The pinned translator/toolchain, Lean kernel, and existing Vec/Box storage
correspondence remain trusted. No Rust branding, NonZero range, tensor layout,
production code, or runtime cost changes. Compiler performance is unaffected
by mechanism: only the experimental translator's library bindings change.

Box.from(shared slice) is NOT this operation: pinned boxed/convert.rs:76 calls
Box.clone_from_ref, which uses CloneToUninit. No arbitrary Clone identity or
shared-slice contract is introduced. NonZero.eq also remains open.

## First divergence and rejected design

The unchanged reducer originally emits three entries, including one opaque
Vec.into_boxed_slice; its From<Vec> call already binds to the existing model.
A first attempt to attach both Rust identities to that same Lean declaration
passes the Lean attribute/registry generator but fails Aeneas's strict
name-uniqueness check when both APIs occur in one fixture. The failed extractor
binary and source are preserved under the review artifacts. Its partial output
is not accepted. No name-collision check is weakened.

The final shape keeps one canonical method model and one API-specific
delegation. The official registry generator supplies the binding patch; source
line-number comments move mechanically. No emitted compiler Lean body is
manually edited.

## Files and executed review evidence

- fixture.rs contains original method and From callers plus native edge cases.
- stdlib.patch factors the existing model and registers the missing method.
- binding-registry.patch is produced by the official Lean registry extractor.
- package.nix builds the candidate extractor only; it does not adopt Std patches.
- Laws.lean states universal sequence preservation and agreement of both routes.
- wrong-result.patch changes real Rust to discard the moved sequence and return
  an empty vector's boxed slice; it retains the original method call.
- MutationWitness.lean states the wrong empty result.
- wrong-model.patch loses the sequence in the library model itself; the
  existing From preservation theorem refuses that mutation during compilation.

The [receipt](run-evidence.json) binds terminal commands, source/tool hashes,
failed intermediate attempts, generated artifacts and the production replay.

- Original Rust native tests pass3/3, including move-only and zero-sized values,
  empty input, order, and spare capacity. Strict Clippy and rustfmt pass.
- Both original and payload-loss Rust sources emit two transparent functions,
  no opaque functions, and compile as unedited Lean Types and Funs.
- Three universal original-source laws pass5a4240/0, with precisely propext.
  This foundational axiom already occurs in the unchanged Vec, Slice,
  Usize.max, and old From model66c434/0. No semantic or native-decision axiom
  is introduced. The initial zero-axiom guard was incorrect and failed; the
  corrected guards name the exact existing foundation, not an opaque contract.
- The unchanged laws reject the Rust mutation537c4f/1 on sequence preservation
  and API agreement; the unaffected From law passes. The guarded wrong-outcome
  witness passesd46428/0 and rejectsef5872/1 on the original. Native tests
  reject all three nonempty mutation cases91f872/101.
- The separate library-model mutation fails6ff071/1 at the existing From
  sequence-preservation theorem, not a syntax/import error. It never becomes
  an admitted library. No private Rust allocator implementation was mutated.
- Seven previous Option laws replay unchangedef6599/0 after fresh candidate
  extraction009f15/0 and generated-module compilation649fa6/0.
- Actual selected production emission succeedsa9338b/0 on the identical
  production LLBC. Emitted entries change127 to126; the one removed opaque
  declaration is the newly bound Vec method. Every retained function's
  metadata is identical3d96bf/0, including zero opaque first-party functions.
  Types are byte-identical8d7d8c/0; exactly one generated call line changes
  its allocator argument2d3174/1. NonZero.eq and Box.from(shared slice) remain
  the two opaque operations. Hash storage correspondence, iterator-interface
  warnings, complete production Lean and tool adoption remain open.
- The latest production UnitDerivative tracer380e9b/0 passes5/5.

The emitted opaque count is not a proof percentage: this cut binds a standard
library operation to a typed trusted abstraction. It does not source-prove
Rust's unsafe allocation machinery or finish the DAE-to-Solve theorem.

The official registry changed by one function binding plus source-span comments.
The reviewed package builds binary4b802364 at
/nix/store/yacyw20n96l0sfs0lzv84mjq75ipl18h-ocaml5.2.1-aeneas-0.1.0/bin/aeneas.
Registry patch5235bad5 and Std patchb1e13b70 are separate inputs. The candidate
package only builds the registry; Lean proofs load the compiled Std patch from
an isolated overlay. Unchanged dependencies are read-only symlinks; Vec.olean
is a regular file. The existing expect/string compiled predecessors are linked
explicitly for registry replay. No live .lake or source pin is overwritten.
Review artifacts and rejected direct-binding source are under
dev/vec-box-boundary-2sELs6, outside target/.

Quick/full gates and canonical pilot adoption are not run for this experimental
cut; their existing failures and remaining obligations stay in the roadmaps.
No additional UnitDerivative relation, golden admission, or M1 closure.
