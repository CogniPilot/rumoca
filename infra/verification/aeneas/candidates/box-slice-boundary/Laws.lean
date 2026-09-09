import BoxSliceBoundary.Funs

open Aeneas Aeneas.Std box_slice_boundary

namespace BoxSliceLaws

theorem dispatch {T : Type} (inst : core.clone.Clone T) (values : Slice T) :
    box_values inst values = Slice.clone inst.clone values := by
  rfl

theorem into_dispatch {T : Type} (inst : core.clone.Clone T) (values : Slice T) :
    into_values inst values = Slice.clone inst.clone values := by
  rfl

theorem identity_when_clone_is_identity {T : Type}
    (inst : core.clone.Clone T) (values : Slice T)
    (h : ∀ x ∈ values.val, inst.clone x = Result.ok x) :
    box_values inst values = Result.ok values := by
  change Slice.clone inst.clone values = Result.ok values
  obtain ⟨_, result, rfl⟩ := WP.spec_imp_exists (Slice.clone_spec h)
  exact result

theorem dimensions_identity (values : Slice U32) :
    u32_values values = Result.ok values := by
  change box_values core.clone.CloneU32 values = Result.ok values
  exact identity_when_clone_is_identity _ _ (by intros; rfl)

theorem empty_does_not_clone {T : Type} (inst : core.clone.Clone T) :
    box_values inst (Slice.new T) = Result.ok (Slice.new T) := by
  rfl

def singleton {T : Type} (value : T) : Slice T :=
  Slice.from [value] (by simp; scalar_tac)

theorem nonidentity_clone (value : Bool) :
    flip_values (singleton value) = Result.ok (singleton (!value)) := by
  cases value <;> rfl

theorem failed_clone_propagates {T : Type} (value : T) (error : Error) :
    box_values { clone := fun _ => Result.fail error } (singleton value) =
      Result.fail error := by
  rfl

theorem diverging_clone_propagates {T : Type} (value : T) :
    box_values { clone := fun _ => Result.div } (singleton value) = Result.div := by
  rfl

/-- info: 'BoxSliceLaws.dispatch' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms dispatch
/-- info: 'BoxSliceLaws.into_dispatch' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms into_dispatch
/-- info: 'BoxSliceLaws.identity_when_clone_is_identity' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms identity_when_clone_is_identity
/-- info: 'BoxSliceLaws.dimensions_identity' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms dimensions_identity
/-- info: 'BoxSliceLaws.empty_does_not_clone' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms empty_does_not_clone
/-- info: 'BoxSliceLaws.nonidentity_clone' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms nonidentity_clone
/-- info: 'BoxSliceLaws.failed_clone_propagates' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms failed_clone_propagates
/-- info: 'BoxSliceLaws.diverging_clone_propagates' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms diverging_clone_propagates

end BoxSliceLaws
