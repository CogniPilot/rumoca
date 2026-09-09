import DaeBuiltinBindingSource.Funs

open Aeneas Aeneas.Std

namespace ExpectFailedLaws

theorem expect_id_some (value : dae_builtin_binding.VariableId) :
    dae_builtin_binding.expect_id (.Some value) = Result.ok value := by
  cases value with
  | mk value brand => cases brand; rfl

theorem expect_id_none :
    dae_builtin_binding.expect_id .None = Result.fail Error.panic := by
  rfl

theorem expect_view_some (value : dae_builtin_binding.VariableView) :
    dae_builtin_binding.expect_view (.Some value) = Result.ok value := by
  cases value with
  | mk value brand => cases brand; rfl

theorem expect_view_none :
    dae_builtin_binding.expect_view .None = Result.fail Error.panic := by
  rfl

theorem expect_plain_some (value : U32) :
    dae_builtin_binding.expect_plain (some value) = Result.ok value := by
  rfl

theorem expect_plain_none :
    dae_builtin_binding.expect_plain none = Result.fail Error.panic := by
  rfl

/-- info: 'ExpectFailedLaws.expect_id_some' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms expect_id_some
/-- info: 'ExpectFailedLaws.expect_id_none' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms expect_id_none
/-- info: 'ExpectFailedLaws.expect_view_some' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms expect_view_some
/-- info: 'ExpectFailedLaws.expect_view_none' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms expect_view_none
/-- info: 'ExpectFailedLaws.expect_plain_some' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms expect_plain_some
/-- info: 'ExpectFailedLaws.expect_plain_none' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms expect_plain_none

end ExpectFailedLaws
