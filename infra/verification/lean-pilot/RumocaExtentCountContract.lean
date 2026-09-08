import RumocaPhaseSolve

open Aeneas Aeneas.Std Aeneas.Std.WP
open rumoca_phase_solve
open rumoca_core.structured_domain rumoca_ir_dae.expression.value_types

namespace RumocaExtentCountContract

set_option autoImplicit false
set_option maxHeartbeats 1000000

/-- Mathematical products of original extent prefixes, including the full shape. -/
def prefixProduct (dimensions : List U32) (length : Nat) : Nat :=
  ((dimensions.take length).map fun extent => extent.val).prod

def ProductResult (dimensions : List U32) : Option Usize → Prop
  | some count => count.val = prefixProduct dimensions dimensions.length ∧
      ∀ length, length ≤ dimensions.length → prefixProduct dimensions length ≤ Usize.max
  | none => ∃ first, first ≤ dimensions.length ∧
      Usize.max < prefixProduct dimensions first ∧
      ∀ length, length < first → prefixProduct dimensions length ≤ Usize.max

theorem prefix_step (dimensions : List U32) (index : Nat) (extent : U32)
    (selected : dimensions[index]? = some extent) :
    prefixProduct dimensions (index + 1) = prefixProduct dimensions index * extent.val := by
  simp [prefixProduct, List.take_add_one, selected, List.map_append, List.prod_append]

theorem extent_product_spec (extents : Slice U32) :
    checked_extent_product extents ⦃ ProductResult extents.val ⦄ := by
  unfold checked_extent_product checked_extent_product_loop
  apply loop.spec_decr_nat
    (fun (_, axis) => extents.val.length - axis.val)
    (fun (product, axis) => axis.val ≤ extents.val.length ∧
      product.val = prefixProduct extents.val axis.val ∧
      ∀ length, length ≤ axis.val → prefixProduct extents.val length ≤ Usize.max)
  · rintro ⟨product, axis⟩ ⟨bound, exactPrefix, safePrefix⟩
    dsimp
    unfold checked_extent_product_loop.body
    dsimp only
    split
    · step as ⟨extent, selected⟩
      step as ⟨wide, widened⟩
      have wideValue : wide.val = extent.val := by scalar_tac
      have nextPrefix := prefix_step extents.val axis.val extent (by grind)
      have multiplication := Usize.checked_mul_bv_spec product wide
      simp only [checked_product, bind_tc_ok]
      cases computed : Usize.checked_mul product wide with
      | none =>
        simp only [computed] at multiplication
        simp only [spec_ok, ProductResult]
        refine ⟨axis.val + 1, by scalar_tac, ?_, ?_⟩
        · simpa [nextPrefix, ← exactPrefix, wideValue] using multiplication
        · intro length earlier
          exact safePrefix length (by omega)
      | some next =>
        simp only [computed] at multiplication
        step as ⟨nextAxis, advanced⟩
        refine ⟨by scalar_tac, ?_, ?_, by scalar_tac⟩
        · simpa [advanced, nextPrefix, ← exactPrefix, wideValue] using multiplication.2.1
        · intro length throughNext
          by_cases atNext : length = nextAxis.val
          · subst length
            simpa [advanced, nextPrefix, ← exactPrefix, wideValue] using multiplication.1
          · exact safePrefix length (by scalar_tac)
    · simp only [spec_ok, ProductResult]
      have finished : axis.val = extents.val.length := by scalar_tac
      simpa [finished] using And.intro exactPrefix safePrefix
  · simp [prefixProduct]
    scalar_tac

def ScalarCountResult (valueType : ValueType) (result : Option Usize) : Prop :=
  match valueType.scalar with
  | .Record => result = none
  | _ => ProductResult valueType.dimensions.val result

theorem scalar_count_spec (valueType : ValueType) :
    ValueType.scalar_count valueType ⦃ ScalarCountResult valueType ⦄ := by
  rcases valueType with ⟨scalar, dimensions, recordName, recordFields⟩
  cases scalarCase : scalar
  all_goals
    simp only [ValueType.scalar_count,
      ScalarType.Insts.CoreCmpPartialEqScalarType.eq,
      ScalarType.read_discriminant, bind_tc_ok, decide_true,
      if_true, ValueType.impl.dimensions,
      alloc.boxed.Box.deref]
  all_goals
    unfold ScalarCountResult
    first | rfl | simpa using extent_product_spec dimensions

theorem extent_product_accepts_iff (extents : Slice U32) (count : Usize) :
    checked_extent_product extents = .ok (some count) ↔
      count.val = prefixProduct extents.val extents.val.length ∧
      ∀ length, length ≤ extents.val.length → prefixProduct extents.val length ≤ Usize.max := by
  obtain ⟨result, runs, correct⟩ := spec_imp_exists (extent_product_spec extents)
  cases result with
  | none =>
    rcases correct with ⟨first, inRange, overflow, _⟩
    constructor
    · intro accepted
      rw [runs] at accepted
      cases accepted
    · rintro ⟨_, fits⟩
      exact False.elim ((Nat.not_le_of_lt overflow) (fits first inRange))
  | some actual =>
    constructor
    · intro accepted
      have same : actual = count := by rw [runs] at accepted; cases accepted; rfl
      simpa [same, ProductResult] using correct
    · intro expected
      have same : actual = count := UScalar.eq_of_val_eq (correct.1.trans expected.1.symm)
      simpa [same] using runs

theorem scalar_count_total_and_faithful (valueType : ValueType) :
    ∃ result, ValueType.scalar_count valueType = .ok result ∧
      ScalarCountResult valueType result := by
  exact spec_imp_exists (scalar_count_spec valueType)

def countFixture (scalar : ScalarType) (dimensions : Slice U32) : ValueType := {
  scalar
  dimensions
  record_name := none
  record_fields := Slice.from [] (by simp)
}

theorem empty_shape_witness :
    ValueType.scalar_count (countFixture .Real (Slice.from [] (by simp))) =
      .ok (some 1#usize) := by
  have product := (extent_product_accepts_iff (Slice.from [] (by simp)) 1#usize).2
    (by simp [prefixProduct]; scalar_tac)
  simpa [ValueType.scalar_count, countFixture, ScalarType.Insts.CoreCmpPartialEqScalarType.eq,
    ScalarType.read_discriminant, ValueType.impl.dimensions, alloc.boxed.Box.deref] using product

def rankThree : Slice U32 := Slice.from [2#u32, 3#u32, 5#u32] (by scalar_tac)

theorem nonempty_shape_witness :
    ValueType.scalar_count (countFixture .Integer rankThree) = .ok (some 30#usize) := by
  have product := (extent_product_accepts_iff rankThree 30#usize).2 (by
    constructor
    · norm_num [prefixProduct, rankThree]
    · intro length bound
      have small : length ≤ 3 := by simpa [rankThree] using bound
      have cases : length = 0 ∨ length = 1 ∨ length = 2 ∨ length = 3 := by omega
      rcases cases with rfl | rfl | rfl | rfl <;>
        norm_num [prefixProduct, rankThree] <;> scalar_tac)
  simpa [ValueType.scalar_count, countFixture, ScalarType.Insts.CoreCmpPartialEqScalarType.eq,
    ScalarType.read_discriminant, ValueType.impl.dimensions, alloc.boxed.Box.deref] using product

def overflowThenZero : Slice U32 :=
  Slice.from [4294967295#u32, 4294967295#u32, 2#u32, 0#u32] (by scalar_tac)

theorem zero_shape_witness :
    ValueType.scalar_count (countFixture .Boolean (Slice.from [0#u32] (by scalar_tac))) =
      .ok (some 0#usize) := by
  have product :=
    (extent_product_accepts_iff (Slice.from [0#u32] (by scalar_tac)) 0#usize).2 (by
      constructor
      · norm_num [prefixProduct]
      · intro length bound
        have cases : length = 0 ∨ length = 1 := by simp at bound; omega
        rcases cases with rfl | rfl
        · norm_num [prefixProduct]
          scalar_tac
        · norm_num [prefixProduct])
  simpa [ValueType.scalar_count, countFixture, ScalarType.Insts.CoreCmpPartialEqScalarType.eq,
    ScalarType.read_discriminant, ValueType.impl.dimensions, alloc.boxed.Box.deref] using product

theorem overflow_before_zero_witness :
    prefixProduct overflowThenZero.val 4 = 0 ∧
      ValueType.scalar_count (countFixture .Real overflowThenZero) = .ok none := by
  constructor
  · norm_num [prefixProduct, overflowThenZero]
  · obtain ⟨result, runs, correct⟩ :=
      scalar_count_total_and_faithful (countFixture .Real overflowThenZero)
    cases result with
    | none => exact runs
    | some count =>
      have fits : ∀ length, length ≤ 4 →
          prefixProduct overflowThenZero.val length ≤ Usize.max := correct.2
      have third := fits 3 (by omega)
      norm_num [prefixProduct, overflowThenZero] at third
      have sizes := Usize.bounds_eq
      scalar_tac

theorem record_refusal_witness :
    ValueType.scalar_count (countFixture .Record rankThree) = .ok none := by
  simp [ValueType.scalar_count, countFixture, ScalarType.Insts.CoreCmpPartialEqScalarType.eq,
    ScalarType.read_discriminant]

/-- info: 'RumocaExtentCountContract.extent_product_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms extent_product_spec
/-- info: 'RumocaExtentCountContract.scalar_count_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms scalar_count_spec
/-- info: 'RumocaExtentCountContract.extent_product_accepts_iff' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms extent_product_accepts_iff
/-- info: 'RumocaExtentCountContract.scalar_count_total_and_faithful' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms scalar_count_total_and_faithful
/-- info: 'RumocaExtentCountContract.empty_shape_witness' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms empty_shape_witness
/-- info: 'RumocaExtentCountContract.nonempty_shape_witness' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms nonempty_shape_witness
/-- info: 'RumocaExtentCountContract.zero_shape_witness' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms zero_shape_witness
/-- info: 'RumocaExtentCountContract.overflow_before_zero_witness' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms overflow_before_zero_witness
/-- info: 'RumocaExtentCountContract.record_refusal_witness' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms record_refusal_witness

end RumocaExtentCountContract
