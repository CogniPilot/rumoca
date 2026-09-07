import ClosedStatic

open Aeneas Aeneas.Std static_region_generics

theorem mutant_returns_zero (value : ClosedOuter) :
    closed_static value = Result.ok 0#u32 := by
  unfold closed_static WRONG_REFERENCE
  rfl

#print axioms mutant_returns_zero
