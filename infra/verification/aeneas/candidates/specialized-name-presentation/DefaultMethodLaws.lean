import DefaultMethods

open Aeneas Aeneas.Std

namespace DefaultMethodLaws

theorem default_on_self_preserves_inequality (id : default_methods.Id) (value : U32) :
    default_methods.default_on_self id value = .ok (!decide (id.raw = value)) := by
  by_cases h : id.raw = value <;>
    simp [default_methods.default_on_self,
      default_methods.Compared.differs01IdT0.default,
      default_methods.Id.Insts.Default_methodsComparedU32.matches, h]

theorem default_on_rhs_preserves_inequality (value : U32) (id : default_methods.Id) :
    default_methods.default_on_rhs value id = .ok (!decide (value = id.raw)) := by
  by_cases h : value = id.raw <;>
    simp [default_methods.default_on_rhs,
      default_methods.Compared.differs01T0Id.default,
      default_methods.U32.Insts.Default_methodsComparedId.matches, h]

/-- info: 'DefaultMethodLaws.default_on_self_preserves_inequality' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in
#print axioms default_on_self_preserves_inequality

/-- info: 'DefaultMethodLaws.default_on_rhs_preserves_inequality' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in
#print axioms default_on_rhs_preserves_inequality

end DefaultMethodLaws
