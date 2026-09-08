import RumocaPhaseSolve

open Aeneas Aeneas.Std Aeneas.Std.WP
open rumoca_phase_solve variable_catalog_refinement
open rumoca_core.ir_primitives

namespace RumocaDimensionCheckContract

set_option autoImplicit false
set_option maxHeartbeats 1000000

/-- Equal rank and the same extent at every ordered axis, including absence. -/
def SameShape (source target : List U32) : Prop :=
  source.length = target.length ∧ ∀ axis : Nat, source[axis]? = target[axis]?

theorem same_shape_iff_equal (source target : List U32) :
    SameShape source target ↔ source = target := by
  constructor
  · intro matched
    apply List.ext_getElem?
    exact matched.2
  · intro equal
    subst target
    simp [SameShape]

def DimensionResult (occurrence : SourceOccurrenceId)
    (source target : alloc.vec.Vec U32)
    (result : core.result.Result Unit VariableCatalogRefinementError) : Prop :=
  match result with
  | .Ok () => SameShape source.val target.val
  | .Err error => ¬ SameShape source.val target.val ∧
      error = .Dimensions occurrence source target

private theorem vector_ne_spec (source target : alloc.vec.Vec U32) :
    alloc.vec.partial_eq.PartialEqVec.ne core.cmp.PartialEqU32 source target
      ⦃ (different : Bool) => different ↔ source.val ≠ target.val ⦄ := by
  simpa [alloc.vec.partial_eq.PartialEqVec.ne, core.slice.cmp.PartialEqSlice.ne,
    Slice.eq_iff, alloc.vec.Vec.val] using
    (core.slice.cmp.PartialEqSlice.ne_homo_spec core.cmp.PartialEqU32
      source.slice target.slice (by intro x y; simp [liftFun2]))

private theorem vector_ne_eq (source target : alloc.vec.Vec U32) :
    alloc.vec.partial_eq.PartialEqVec.ne core.cmp.PartialEqU32 source target =
      .ok (decide (source.val ≠ target.val)) := by
  obtain ⟨different, runs, faithful⟩ := spec_imp_exists (vector_ne_spec source target)
  rw [runs]
  cases different <;> simp_all

private theorem clone_u32_vector (source : alloc.vec.Vec U32) :
    alloc.vec.CloneVec.clone core.clone.CloneU32 source = .ok source := by
  obtain ⟨copied, runs, same⟩ := spec_imp_exists
    (Slice.clone_spec (clone := core.clone.CloneU32.clone) (s := source.slice)
      (h := by intros; rfl))
  rw [← same] at runs
  simp [alloc.vec.CloneVec.clone, runs]

theorem dimension_gate_equation (occurrence : SourceOccurrenceId)
    (source : DaeVariableFact) (target : SolveVariableFact) :
    check_dimensions occurrence source target =
      if source.dimensions.val = target.dimensions.val then .ok (.Ok ())
      else .ok (.Err (.Dimensions occurrence source.dimensions target.dimensions)) := by
  simp only [check_dimensions, vector_ne_eq, bind_tc_ok, clone_u32_vector]
  by_cases equal : target.dimensions.val = source.dimensions.val
  · rw [if_pos equal.symm]
    simp only [equal, ne_self_iff_false, decide_false, Bool.false_eq_true, ↓reduceIte]
  · rw [if_neg (Ne.symm equal)]
    exact if_pos (decide_eq_true equal)

theorem dimensions_spec (occurrence : SourceOccurrenceId)
    (source : DaeVariableFact) (target : SolveVariableFact) :
    check_dimensions occurrence source target
      ⦃ DimensionResult occurrence source.dimensions target.dimensions ⦄ := by
  rw [dimension_gate_equation]
  by_cases equal : source.dimensions.val = target.dimensions.val <;>
    simp [equal, DimensionResult, same_shape_iff_equal]

theorem dimensions_accept_iff (occurrence : SourceOccurrenceId)
    (source : DaeVariableFact) (target : SolveVariableFact) :
    check_dimensions occurrence source target = .ok (.Ok ()) ↔
      SameShape source.dimensions.val target.dimensions.val := by
  rw [dimension_gate_equation]
  by_cases equal : source.dimensions.val = target.dimensions.val <;>
    simp [equal, same_shape_iff_equal]

theorem dimensions_refuse_iff (occurrence : SourceOccurrenceId)
    (source : DaeVariableFact) (target : SolveVariableFact)
    (error : VariableCatalogRefinementError) :
    check_dimensions occurrence source target = .ok (.Err error) ↔
      ¬ SameShape source.dimensions.val target.dimensions.val ∧
        error = .Dimensions occurrence source.dimensions target.dimensions := by
  rw [dimension_gate_equation]
  by_cases equal : source.dimensions.val = target.dimensions.val <;>
    simp [equal, same_shape_iff_equal, eq_comm]

theorem dimensions_total_and_faithful (occurrence : SourceOccurrenceId)
    (source : DaeVariableFact) (target : SolveVariableFact) :
    ∃ result, check_dimensions occurrence source target = .ok result ∧
      DimensionResult occurrence source.dimensions target.dimensions result := by
  exact spec_imp_exists (dimensions_spec occurrence source target)

theorem counts_do_not_affect_dimension_check (occurrence : SourceOccurrenceId)
    (source : DaeVariableFact) (target : SolveVariableFact) (daeCount solveCount : Usize) :
    check_dimensions occurrence { source with scalar_count := daeCount }
        { target with storage := { target.storage with scalar_count := solveCount } } =
      check_dimensions occurrence source target := by
  rfl

def sourceFact (dimensions : alloc.vec.Vec U32) (count : Usize) : DaeVariableFact := {
  occurrence := { value := { value := 41#u32 } }
  role := .State
  fixed := .Fixed
  variability := .Continuous
  is_tunable := false
  causality := .Local
  scalar_type := .Real
  dimensions := dimensions
  scalar_count := count
}

def targetFact (dimensions : alloc.vec.Vec U32) (count : Usize) : SolveVariableFact := {
  occurrence := { value := { value := 41#u32 } }
  fixed := .Fixed
  state_initialization := .Exact
  variability := .Continuous
  tunable := false
  causality := .Local
  dimensions := dimensions
  role := .State
  value_kind := .Real
  storage := { base := .Y 0#usize, scalar_count := count, role := .State, value_kind := .Real }
}

def twoByThree : alloc.vec.Vec U32 :=
  .from [2#u32, 3#u32] (by scalar_tac)

def threeByTwo : alloc.vec.Vec U32 :=
  .from [3#u32, 2#u32] (by scalar_tac)

def properPrefix : alloc.vec.Vec U32 :=
  .from [2#u32] (by scalar_tac)

theorem empty_shape_witness :
    check_dimensions (sourceFact (alloc.vec.Vec.new U32) 1#usize).occurrence
      (sourceFact (alloc.vec.Vec.new U32) 1#usize)
      (targetFact (alloc.vec.Vec.new U32) 1#usize) = .ok (.Ok ()) := by
  simp [dimension_gate_equation, sourceFact, targetFact]

theorem matching_shape_witness :
    check_dimensions (sourceFact twoByThree 6#usize).occurrence
      (sourceFact twoByThree 6#usize) (targetFact twoByThree 6#usize) = .ok (.Ok ()) := by
  simp [dimension_gate_equation, sourceFact, targetFact]

theorem transposed_shape_refusal_witness :
    check_dimensions (sourceFact twoByThree 6#usize).occurrence
      (sourceFact twoByThree 6#usize) (targetFact threeByTwo 6#usize) =
      .ok (.Err (.Dimensions (sourceFact twoByThree 6#usize).occurrence twoByThree threeByTwo)) := by
  simp [dimension_gate_equation, sourceFact, targetFact, twoByThree, threeByTwo]

theorem proper_prefix_refusal_witness :
    check_dimensions (sourceFact properPrefix 2#usize).occurrence
      (sourceFact properPrefix 2#usize) (targetFact twoByThree 6#usize) =
      .ok (.Err (.Dimensions (sourceFact properPrefix 2#usize).occurrence properPrefix twoByThree)) := by
  simp [dimension_gate_equation, sourceFact, targetFact, properPrefix, twoByThree]

/-- info: 'RumocaDimensionCheckContract.same_shape_iff_equal' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in
#print axioms same_shape_iff_equal
/-- info: 'RumocaDimensionCheckContract.dimension_gate_equation' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms dimension_gate_equation
/-- info: 'RumocaDimensionCheckContract.dimensions_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms dimensions_spec
/-- info: 'RumocaDimensionCheckContract.dimensions_accept_iff' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms dimensions_accept_iff
/-- info: 'RumocaDimensionCheckContract.dimensions_refuse_iff' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms dimensions_refuse_iff
/-- info: 'RumocaDimensionCheckContract.dimensions_total_and_faithful' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms dimensions_total_and_faithful
/--
info: 'RumocaDimensionCheckContract.counts_do_not_affect_dimension_check' depends on axioms: [propext,
 Classical.choice,
 Quot.sound]
-/
#guard_msgs in
#print axioms counts_do_not_affect_dimension_check
/-- info: 'RumocaDimensionCheckContract.empty_shape_witness' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms empty_shape_witness
/-- info: 'RumocaDimensionCheckContract.matching_shape_witness' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms matching_shape_witness
/--
info: 'RumocaDimensionCheckContract.transposed_shape_refusal_witness' depends on axioms: [propext,
 Classical.choice,
 Quot.sound]
-/
#guard_msgs in
#print axioms transposed_shape_refusal_witness
/-- info: 'RumocaDimensionCheckContract.proper_prefix_refusal_witness' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms proper_prefix_refusal_witness

end RumocaDimensionCheckContract
