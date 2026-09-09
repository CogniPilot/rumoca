import OptionBoundarySource

open Aeneas Aeneas.Std
open option_library_boundary

namespace ResultLaws

theorem source_result_ok {T E : Type} (value : core.result.Result T E) :
    core.result.Result.ok value =
      (match value with
      | .Ok value => .ok (some value)
      | .Err _ => .ok none) := by
  cases value <;> rfl

theorem source_result_caller (first : core.result.Result U32 U32)
    (second : Option U32) :
    result_then_option first second =
      (match first, second with
      | .Ok first, some second => .ok (some (first, second))
      | _, _ => .ok none) := by
  cases first <;> cases second <;> rfl

/-- info: 'ResultLaws.source_result_ok' does not depend on any axioms -/
#guard_msgs in
#print axioms source_result_ok

/-- info: 'ResultLaws.source_result_caller' depends on axioms: [propext] -/
#guard_msgs in
#print axioms source_result_caller

end ResultLaws
