import RumocaKernelPilot

open Aeneas Aeneas.Std Aeneas.Std.WP
open rumoca_phase_solve.scalar_constant_derivative_refinement
open RumocaKernelPilot

namespace RumocaFactContract

set_option maxHeartbeats 1000000

/-- Exact positive program relations, independent of the production guards. -/
def jacobianPermitted (facts : ScalarBlockFacts 3#usize) (bits : U64) : Prop :=
  match facts with
  | .UnsupportedShape => False
  | .Exact operations => operations.val =
      [.Constant 0#u32 bits, .Constant 1#u32 0#u64, .StoreOutput 1#u32]

def visiblePermitted (facts : ScalarBlockFacts 2#usize) : Prop :=
  match facts with
  | .UnsupportedShape => False
  | .Exact operations => operations.val = [.LoadY 0#u32 0#usize, .StoreOutput 0#u32]

theorem jacobian_accepted_iff (facts : ScalarBlockFacts 3#usize) (bits : U64) :
    check_full_jacobian facts bits = .ok (.Ok ()) ↔ jacobianPermitted facts bits := by
  cases facts with
  | UnsupportedShape => simp [check_full_jacobian, jacobianPermitted]
  | Exact operations =>
    obtain ⟨first, second, third, hops⟩ :=
      List.length_eq_three.mp (show operations.val.length = 3 by simp)
    cases first <;> cases second <;> cases third
    all_goals simp [check_full_jacobian, jacobianPermitted, Array.index_usize, hops]
    all_goals split_ifs <;> simp_all
    all_goals scalar_tac

theorem jacobian_total (facts : ScalarBlockFacts 3#usize) (bits : U64) :
    ∃ result, check_full_jacobian facts bits = .ok result := by
  cases facts with
  | UnsupportedShape => simp [check_full_jacobian]
  | Exact operations =>
    obtain ⟨first, second, third, hops⟩ :=
      List.length_eq_three.mp (show operations.val.length = 3 by simp)
    cases first <;> cases second <;> cases third
    all_goals simp [check_full_jacobian, Array.index_usize, hops]
    all_goals split_ifs <;> simp

/-- The accepted tangent program returns the positive-zero bit pattern,
independently of the primal constant and any solver input. No floating-point
arithmetic or derivative-of-IEEE-operations claim is made. -/
theorem jacobian_executes (facts : ScalarBlockFacts 3#usize) (bits : U64)
    (accepted : check_full_jacobian facts bits = .ok (.Ok ()))
    (inputs : Usize → Option U64) :
    (match facts with
     | .UnsupportedShape => none
     | .Exact operations => execute inputs operations.val (fun _ => none)) =
      some [0#u64] := by
  have h := (jacobian_accepted_iff facts bits).mp accepted
  cases facts <;> simp_all [jacobianPermitted, execute]

theorem visible_accepted_iff (facts : ScalarBlockFacts 2#usize) :
    check_visible_rows facts = .ok (.Ok ()) ↔ visiblePermitted facts := by
  cases facts with
  | UnsupportedShape => simp [check_visible_rows, visiblePermitted]
  | Exact operations =>
    obtain ⟨first, second, hops⟩ :=
      List.length_eq_two.mp (show operations.val.length = 2 by simp)
    cases first <;> cases second
    all_goals simp [check_visible_rows, visiblePermitted, Array.index_usize, hops]
    all_goals split_ifs <;> simp_all
    all_goals scalar_tac

theorem visible_total (facts : ScalarBlockFacts 2#usize) :
    ∃ result, check_visible_rows facts = .ok result := by
  cases facts with
  | UnsupportedShape => simp [check_visible_rows]
  | Exact operations =>
    obtain ⟨first, second, hops⟩ :=
      List.length_eq_two.mp (show operations.val.length = 2 by simp)
    cases first <;> cases second
    all_goals simp [check_visible_rows, Array.index_usize, hops]
    all_goals split_ifs <;> simp

/-- The visible program reads exactly state slot zero, preserving its bits.
Missing input stays missing; no fabricated value enters the semantics. -/
theorem visible_executes (facts : ScalarBlockFacts 2#usize)
    (accepted : check_visible_rows facts = .ok (.Ok ()))
    (inputs : Usize → Option U64) :
    executeKernel inputs facts = (inputs 0#usize).map (fun bits => [bits]) := by
  have h := (visible_accepted_iff facts).mp accepted
  cases facts <;> simp_all [visiblePermitted, executeKernel, execute]
  cases inputs 0#usize <;> rfl

/-- Concrete negative controls pin the state slot and the sign of zero. -/
example : check_visible_rows
    (.Exact (Array.make 2#usize [.LoadY 0#u32 1#usize, .StoreOutput 0#u32])) =
      .ok (.Err .VisibleRowOperations) := by rfl

example : check_full_jacobian
    (.Exact (Array.make 3#usize
      [.Constant 0#u32 7#u64, .Constant 1#u32 9223372036854775808#u64, .StoreOutput 1#u32]))
    7#u64 = .ok (.Err (.FullJacobianTangentBits 9223372036854775808#u64)) := by rfl

/-- info: 'RumocaFactContract.jacobian_accepted_iff' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms jacobian_accepted_iff
/-- info: 'RumocaFactContract.jacobian_total' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms jacobian_total
/-- info: 'RumocaFactContract.jacobian_executes' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms jacobian_executes
/-- info: 'RumocaFactContract.visible_accepted_iff' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms visible_accepted_iff
/-- info: 'RumocaFactContract.visible_total' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms visible_total
/-- info: 'RumocaFactContract.visible_executes' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms visible_executes

end RumocaFactContract
