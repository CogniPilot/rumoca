import BrandedEquality

open Aeneas Aeneas.Std branded_equality

namespace EqualityLaws

theorem same_compares_payload (left right : Id) :
    same left right = .ok (decide (left.raw = right.raw)) := by
  by_cases h : left.raw = right.raw
  · simp [same, Id.Insts.CoreCmpPartialEqId.eq,
      core.marker.PhantomDataMut0T0.Insts.CoreCmpPartialEqPhantomDataMut0T0.eq, h]
  · simp [same, Id.Insts.CoreCmpPartialEqId.eq, h]

/-- info: 'EqualityLaws.same_compares_payload' depends on axioms: [propext] -/
#guard_msgs in
#print axioms same_compares_payload

end EqualityLaws
