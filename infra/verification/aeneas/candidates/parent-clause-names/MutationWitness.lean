import ParentClauseNames

open Aeneas Aeneas.Std parent_clause_names

namespace ParentClauseMutation

theorem swapped_results (left : Std.U32) (right : Bool) :
    concrete left right =
      Result.ok (if right then 1#u32 else 0#u32, left, 101#u32, 202#u32) := by
  cases right <;> rfl

/-- info: 'ParentClauseMutation.swapped_results' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms swapped_results

end ParentClauseMutation
