import IteratorOwnedSource

open Aeneas Aeneas.Std
open iterator_specialization

namespace IteratorLaws

/-- The specialized Rust Option::map returns the callback's updated state. -/
theorem option_map_state {T U F : Type}
    (callback : core.ops.function.FnOnceMut0T0T1T2 F T U)
    (value : Option T) (state : F) :
    core.option.Option.mapT0T1Mut0T2 callback value state =
      (match value with
      | none => .ok (none, state)
      | some value => do
          let (result, state) ← callback.call_once state value
          .ok (some result, state)) := by
  cases value <;> simp [core.option.Option.mapT0T1Mut0T2]

/-- Observable prefix and callback counts, including checked-add failure. -/
def firstTwo (values : List U32) (initial : U32) :
    Result (Option (U32 × U32) × Option (U32 × U32)) := do
  match values with
  | [] => .ok (none, none)
  | first :: rest =>
      let count ← initial + 1#u32
      match rest with
      | [] => .ok (some (first, count), none)
      | second :: _ =>
          let next ← count + 1#u32
          .ok (some (first, count), some (second, next))

/-- The actual Rust caller consumes at most two elements and retains state. -/
theorem mapped_pair_contract (values : Slice U32) (initial : U32) :
    mapped_pair values initial = firstTwo values.val initial := by
  rcases h : values.val with _ | ⟨x, _ | ⟨y, tail⟩⟩
  all_goals
    simp [mapped_pair, firstTwo, h,
      core.slice.Slice.iter, core.slice.iter.IteratorSliceIter.next,
      core.iter.traits.iterator.Iterator.map.default,
      core.iter.adapters.map.Map.new,
      core.iter.adapters.map.Map.Insts.CoreIterTraitsIteratorIterator.next,
      core.option.Option.mapT0T1Mut0T2,
      core.ops.function.FnOnceMut0FAClause0_Clause0_Output.Blanket.call_once,
      mapped_pair.closure.Insts.CoreOpsFunctionFnMutTupleSharedU32PairU32U32.call_mut]
  · intro count _
    change values.val[0]'(by simp [h]) = x
    simp [h]
  · intro count _ next _
    constructor
    · change values.val[0]'(by simp [h]) = x
      simp [h]
    · change values.val[1]'(by simp [h]) = y
      simp [h]

/-- info: 'IteratorLaws.option_map_state' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in
#print axioms option_map_state

/-- info: 'IteratorLaws.mapped_pair_contract' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms mapped_pair_contract

end IteratorLaws
