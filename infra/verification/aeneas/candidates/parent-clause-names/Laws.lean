import ParentClauseNames

open Aeneas Aeneas.Std parent_clause_names

namespace ParentClauseLaws

theorem dispatch {P L R : Type} (inst : Pair P L R) (pair : P) :
    observe inst pair =
      (do
        let left ← inst.left pair
        let leftValue ← inst.ValueInst2.value left
        let right ← inst.right pair
        let rightValue ← inst.ValueInst3.value right
        let firstMethod ← inst.ValueInst pair
        let secondMethod ← inst.ValueInst1 pair
        Result.ok (leftValue, rightValue, firstMethod, secondMethod)) := by
  rfl

theorem concrete_values (left : Std.U32) (right : Bool) :
    concrete left right =
      Result.ok (left, if right then 1#u32 else 0#u32, 101#u32, 202#u32) := by
  cases right <;> rfl

theorem independent_scope {T : Type} (inst : Single T) (value : T) :
    single inst value = inst.ValueInst.value value := by
  rfl

theorem concrete_single (value : Std.U32) :
    single U32.Insts.Parent_clause_namesSingle value = Result.ok value := by
  rfl

/-- info: 'ParentClauseLaws.dispatch' does not depend on any axioms -/
#guard_msgs in
#print axioms dispatch
/-- info: 'ParentClauseLaws.concrete_values' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms concrete_values
/-- info: 'ParentClauseLaws.independent_scope' does not depend on any axioms -/
#guard_msgs in
#print axioms independent_scope
/-- info: 'ParentClauseLaws.concrete_single' does not depend on any axioms -/
#guard_msgs in
#print axioms concrete_single

end ParentClauseLaws
