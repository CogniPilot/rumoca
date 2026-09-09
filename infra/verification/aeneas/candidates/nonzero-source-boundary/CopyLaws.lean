import NonzeroCopyBoundary.Funs

open Aeneas Aeneas.Std nonzero_source_boundary

namespace NonzeroCopyLaws

theorem inner_identity (value : _root_.core.num.niche_types.NonZeroU32Inner) :
    core.num.niche_types.NonZeroU32Inner.Insts.CoreCloneClone.clone value =
      Result.ok value := by
  rfl

theorem outer_identity {T Inner : Type}
    (inst : core.num.nonzero.ZeroablePrimitive T Inner)
    (value : _root_.core.num.nonzero.NonZero T Inner) :
    core.num.nonzero.NonZero.Insts.CoreCloneClone.clone inst value =
      Result.ok value := by
  rfl

theorem generic_dispatch {T : Type} (inst : core.clone.Clone T) (value : T) :
    cloned inst value = inst.clone value := by
  rfl

theorem optional_identity
    (value : Option (_root_.core.num.nonzero.NonZero Std.U32
      _root_.core.num.niche_types.NonZeroU32Inner)) :
    clone_optional value = Result.ok value := by
  cases value <;> rfl

/-- info: 'NonzeroCopyLaws.inner_identity' does not depend on any axioms -/
#guard_msgs in
#print axioms inner_identity
/-- info: 'NonzeroCopyLaws.outer_identity' does not depend on any axioms -/
#guard_msgs in
#print axioms outer_identity
/-- info: 'NonzeroCopyLaws.generic_dispatch' does not depend on any axioms -/
#guard_msgs in
#print axioms generic_dispatch
/-- info: 'NonzeroCopyLaws.optional_identity' does not depend on any axioms -/
#guard_msgs in
#print axioms optional_identity

end NonzeroCopyLaws
