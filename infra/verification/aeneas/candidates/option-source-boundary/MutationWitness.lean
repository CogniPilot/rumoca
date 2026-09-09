import OptionSourceBoundaryClosed

open Aeneas Aeneas.Std
open option_source_boundary

namespace OptionSourceMutation

theorem wrong_copy {T : Type} (copy : core.marker.Copy T) (value : Option T) :
    copy_value copy value = Result.ok none := by
  rfl

theorem loses_present_value {T : Type} (copy : core.marker.Copy T) (value : T) :
    copy_value copy (some value) ≠ Result.ok (some value) := by
  simp [copy_value]

/-- info: 'OptionSourceMutation.wrong_copy' does not depend on any axioms -/
#guard_msgs in
#print axioms wrong_copy
/-- info: 'OptionSourceMutation.loses_present_value' depends on axioms: [propext] -/
#guard_msgs in
#print axioms loses_present_value

end OptionSourceMutation
