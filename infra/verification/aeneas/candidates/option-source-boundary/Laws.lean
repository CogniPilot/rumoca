import OptionSourceBoundaryClosed

open Aeneas Aeneas.Std
open option_source_boundary

namespace OptionSourceLaws

theorem source_copied {T : Type} (copy : core.marker.Copy T) (value : Option T) :
    core.option.OptionShared0T.copied copy value = Result.ok value := by
  cases value <;> rfl

theorem source_flatten {T : Type} (value : Option (Option T)) :
    core.option.OptionOption.flatten value =
      Result.ok (match value with | none => none | some inner => inner) := by
  cases value <;> rfl

theorem copy_caller {T : Type} (copy : core.marker.Copy T) (value : Option T) :
    copy_value copy value = Result.ok value := by
  cases value <;> rfl

theorem flatten_caller {T : Type} (value : Option (Option T)) :
    flatten_value value =
      Result.ok (match value with | none => none | some inner => inner) := by
  cases value <;> rfl

theorem composed_caller {T : Type} (copy : core.marker.Copy T)
    (value : Option (Option T)) :
    copy_flatten copy value =
      Result.ok (match value with | none => none | some inner => inner) := by
  cases value <;> rfl

theorem clone_none {T : Type} (clone : core.clone.Clone T) :
    core.option.Option.Insts.CoreCloneClone.clone clone none = Result.ok none := by
  rfl

theorem clone_some {T : Type} (clone : core.clone.Clone T) (value : T) :
    core.option.Option.Insts.CoreCloneClone.clone clone (some value) =
      (match clone.clone value with
      | .ok copied => .ok (some copied)
      | .fail error => .fail error
      | .div => .div) := by
  cases outcome : clone.clone value <;>
    simp [core.option.Option.Insts.CoreCloneClone.clone, outcome]

/-- info: 'OptionSourceLaws.source_copied' does not depend on any axioms -/
#guard_msgs in
#print axioms source_copied
/-- info: 'OptionSourceLaws.source_flatten' does not depend on any axioms -/
#guard_msgs in
#print axioms source_flatten
/-- info: 'OptionSourceLaws.copy_caller' does not depend on any axioms -/
#guard_msgs in
#print axioms copy_caller
/-- info: 'OptionSourceLaws.flatten_caller' does not depend on any axioms -/
#guard_msgs in
#print axioms flatten_caller
/-- info: 'OptionSourceLaws.composed_caller' does not depend on any axioms -/
#guard_msgs in
#print axioms composed_caller
/-- info: 'OptionSourceLaws.clone_none' does not depend on any axioms -/
#guard_msgs in
#print axioms clone_none
/-- info: 'OptionSourceLaws.clone_some' depends on axioms: [propext] -/
#guard_msgs in
#print axioms clone_some

end OptionSourceLaws
