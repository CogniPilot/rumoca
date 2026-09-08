import RumocaPhaseSolve

open Aeneas Aeneas.Std Aeneas.Std.WP
open rumoca_phase_solve.scalar_constant_derivative_refinement

namespace RumocaKernelPilot

set_option maxHeartbeats 1000000

/-- Independent bit-transfer semantics; registers initially have no value. -/
def execute (inputs : Usize → Option U64) :
    List OperationFact → (U32 → Option U64) → Option (List U64)
  | [], _ => some []
  | .Constant destination bits :: rest, registers =>
    execute inputs rest (Function.update registers destination (some bits))
  | .LoadY destination index :: rest, registers => do
    let bits ← inputs index
    execute inputs rest (Function.update registers destination (some bits))
  | .StoreOutput source :: rest, registers => do
    let bits ← registers source
    let outputs ← execute inputs rest registers
    some (bits :: outputs)
  | .Unsupported :: _, _ => none

def canonical (bits : U64) : ScalarBlockFacts 2#usize :=
  .Exact (Array.make 2#usize [.Constant 0#u32 bits, .StoreOutput 0#u32])

def permitted (kernel : ScalarBlockFacts 2#usize) (bits : U64) : Prop :=
  match kernel with
  | .UnsupportedShape => False
  | .Exact operations => operations.val = [.Constant 0#u32 bits, .StoreOutput 0#u32]

def executeKernel (inputs : Usize → Option U64) (kernel : ScalarBlockFacts 2#usize) :
    Option (List U64) :=
  match kernel with
  | .UnsupportedShape => none
  | .Exact operations => execute inputs operations.val (fun _ => none)

theorem canonical_accepted (bits : U64) :
    check_kernel (canonical bits) bits = .ok (.Ok ()) := by
  simp [check_kernel, canonical, Array.index_usize]

private theorem two_operations (operations : Array OperationFact 2#usize) :
    ∃ first second, operations.val = [first, second] := by
  have h : operations.val.length = 2 := by simp
  exact List.length_eq_two.mp h

theorem accepted_iff_permitted (kernel : ScalarBlockFacts 2#usize) (bits : U64) :
    check_kernel kernel bits = .ok (.Ok ()) ↔ permitted kernel bits := by
  cases kernel with
  | UnsupportedShape => simp [check_kernel, permitted]
  | Exact operations =>
    obtain ⟨first, second, hops⟩ := two_operations operations
    cases first <;> cases second
    all_goals simp [check_kernel, permitted, Array.index_usize, hops]
    all_goals split_ifs <;> simp_all
    all_goals scalar_tac

theorem accepted_executes (kernel : ScalarBlockFacts 2#usize) (bits : U64)
    (accepted : check_kernel kernel bits = .ok (.Ok ()))
    (inputs : Usize → Option U64) : executeKernel inputs kernel = some [bits] := by
  have h := (accepted_iff_permitted kernel bits).mp accepted
  cases kernel <;> simp_all [permitted, executeKernel, execute]

theorem checker_total (kernel : ScalarBlockFacts 2#usize) (bits : U64) :
    ∃ result, check_kernel kernel bits = .ok result := by
  cases kernel with
  | UnsupportedShape => simp [check_kernel]
  | Exact operations =>
    obtain ⟨first, second, hops⟩ := two_operations operations
    cases first <;> cases second
    all_goals simp [check_kernel, Array.index_usize, hops]
    all_goals split_ifs <;> simp

/-- Selection preserves the complete sole program, for every element type. -/
theorem selection_sound {T : Type} (width : Usize)
    (programs : Slice (alloc.vec.Vec T)) (operations : Array T width)
    (accepted : exact_program width programs = .ok (some operations)) :
    programs.val.map alloc.vec.Vec.val = [operations.val] := by
  simp [exact_program, Slice.index_usize, alloc.vec.Vec.index,
    core.array.TryFromSharedArraySlice.try_from] at accepted
  split at accepted
  next one =>
    obtain ⟨program, sole⟩ := List.length_eq_one_iff.mp one
    simp [sole] at accepted ⊢
    split at accepted
    next =>
      simp at accepted
      simpa [alloc.vec.Vec.val] using congrArg (fun a : Array T width => a.val) accepted
    next => simp at accepted
  next => simp_all

theorem selection_complete {T : Type} (width : Usize)
    (programs : Slice (alloc.vec.Vec T)) (program : alloc.vec.Vec T)
    (sole : programs.val = [program]) (size : program.val.length = width.val) :
    ∃ operations, exact_program width programs = .ok (some operations) ∧
      operations.val = program.val := by
  simp [exact_program, Slice.index_usize, alloc.vec.Vec.index,
    core.array.TryFromSharedArraySlice.try_from, sole]
  have fits : program.slice.len = width := by
    simp [alloc.vec.Vec.val] at size
    scalar_tac
  simp [fits, alloc.vec.Vec.val]

theorem selection_total {T : Type} (width : Usize)
    (programs : Slice (alloc.vec.Vec T)) :
    ∃ result, exact_program width programs = .ok result := by
  simp [exact_program, Slice.index_usize, alloc.vec.Vec.index,
    core.array.TryFromSharedArraySlice.try_from]
  split
  next one =>
    obtain ⟨program, sole⟩ := List.length_eq_one_iff.mp one
    simp [sole]
    split <;> simp
  next => simp

/-- Composition through an explicit element interpretation. It does not prove
that Rust operation_fact implements that interpretation of LinearOp. -/
theorem selected_kernel_executes {T : Type}
    (programs : Slice (alloc.vec.Vec T)) (operations : Array T 2#usize)
    (classify : T → OperationFact) (bits : U64) (inputs : Usize → Option U64)
    (selected : exact_program 2#usize programs = .ok (some operations))
    (checked : check_kernel
      (.Exact (Array.from (operations.val.map classify) (by simp))) bits = .ok (.Ok ())) :
    execute inputs ((programs.val.flatMap alloc.vec.Vec.val).map classify)
      (fun _ => none) = some [bits] := by
  have complete := selection_sound 2#usize programs operations selected
  have entire : programs.val.flatMap alloc.vec.Vec.val = operations.val := by
    simpa [List.flatMap] using congrArg List.flatten complete
  rw [entire]
  simpa [executeKernel] using accepted_executes _ bits checked inputs

/-- Concrete controls must reduce to refusal, independently of proof tactics. -/
example : exact_program 2#usize
    (Slice.from [alloc.vec.Vec.from [11#u32, 22#u32, 33#u32] (by scalar_tac)]
      (by scalar_tac)) = .ok none := by rfl

example : exact_program 2#usize
    (Slice.from [alloc.vec.Vec.from [11#u32, 22#u32] (by scalar_tac),
      alloc.vec.Vec.from [] (by scalar_tac)] (by scalar_tac)) = .ok none := by rfl

/-- info: 'RumocaKernelPilot.canonical_accepted' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms canonical_accepted
/-- info: 'RumocaKernelPilot.accepted_iff_permitted' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms accepted_iff_permitted
/-- info: 'RumocaKernelPilot.accepted_executes' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms accepted_executes
/-- info: 'RumocaKernelPilot.checker_total' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms checker_total
/-- info: 'RumocaKernelPilot.selection_sound' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms selection_sound
/-- info: 'RumocaKernelPilot.selection_complete' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms selection_complete
/-- info: 'RumocaKernelPilot.selection_total' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms selection_total
/-- info: 'RumocaKernelPilot.selected_kernel_executes' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms selected_kernel_executes

def resultSpec {width : Usize} (actual expected : Array Usize width) :
    core.result.Result Unit OwnerCountMismatch → Prop
  | .Ok () => actual.val = expected.val
  | .Err error =>
      error.index.val < width.val ∧
      actual.val[error.index.val]? = some error.actual ∧
      expected.val[error.index.val]? = some error.expected ∧
      error.actual ≠ error.expected ∧
      ∀ j, j < error.index.val → actual.val[j]? = expected.val[j]?

theorem counts_spec {width : Usize} (actual expected : Array Usize width) :
    check_owner_counts actual expected ⦃ resultSpec actual expected ⦄ := by
  unfold check_owner_counts check_owner_counts_loop
  apply loop.spec_decr_nat
    (fun index => width.val - index.val)
    (fun index => index.val ≤ width.val ∧
      ∀ j, j < index.val → actual.val[j]? = expected.val[j]?)
  · intro index ⟨bound, matchedPrefix⟩
    unfold check_owner_counts_loop.body
    split
    · step as ⟨a, ha⟩
      step as ⟨e, he⟩
      split
      · simp [resultSpec]
        grind
      · step as ⟨next, hnext⟩
        grind
    · simp [resultSpec]
      apply List.ext_getElem?
      intro j
      by_cases hj : j < width.val
      · apply matchedPrefix
        scalar_tac
      · simp_all
  · simp

theorem counts_accepted_iff {width : Usize} (actual expected : Array Usize width) :
    check_owner_counts actual expected = .ok (.Ok ()) ↔ actual.val = expected.val := by
  obtain ⟨result, runs, correct⟩ := spec_imp_exists (counts_spec actual expected)
  cases result with
  | Ok value =>
    cases value
    simp_all [resultSpec]
  | Err error =>
    simp_all [resultSpec]
    grind

theorem counts_total {width : Usize} (actual expected : Array Usize width) :
    ∃ result, check_owner_counts actual expected = .ok result := by
  obtain ⟨result, runs, _⟩ := spec_imp_exists (counts_spec actual expected)
  exact ⟨result, runs⟩

/-- info: 'RumocaKernelPilot.counts_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms counts_spec
/-- info: 'RumocaKernelPilot.counts_accepted_iff' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms counts_accepted_iff
/-- info: 'RumocaKernelPilot.counts_total' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms counts_total

end RumocaKernelPilot
