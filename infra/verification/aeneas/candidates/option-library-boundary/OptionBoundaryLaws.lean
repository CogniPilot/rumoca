import OptionBoundarySource

open Aeneas.Std
open option_library_boundary

namespace OptionBoundaryLaws

/-- Option's branch exposes the exact payload or the uninhabited residual. -/
theorem option_branch {T : Type} (value : Option T) :
    core.option.Option.Insts.CoreOpsTry_traitTry.branch value =
      .ok (match value with
        | none => core.ops.control_flow.ControlFlow.Break none
        | some payload => core.ops.control_flow.ControlFlow.Continue payload) := by
  cases value <;> rfl

/-- No inhabitant can enter the generated panic arm of this residual type. -/
theorem option_residual (T : Type) (residual : Option core.convert.Infallible) :
    core.option.Option.Insts.CoreOpsTry_traitFromResidualOptionInfallible.from_residual
      T residual = .ok none := by
  cases residual with
  | none => rfl
  | some impossible => cases impossible

/-- Marker equality has no premise about the marked type's equality. -/
theorem marker_equality {T : Type} (left right : core.marker.PhantomData T) :
    core.marker.PhantomData.Insts.CoreCmpPartialEqPhantomData.eq left right =
      .ok true := by
  rfl

/-- The fixture's actual Rust equality dispatch reaches that implementation. -/
theorem marker_caller {T : Type} (left right : core.marker.PhantomData T) :
    same_marker left right = .ok true := by
  rfl

/-- info: 'OptionBoundaryLaws.option_branch' does not depend on any axioms -/
#guard_msgs in
#print axioms option_branch

/-- info: 'OptionBoundaryLaws.option_residual' does not depend on any axioms -/
#guard_msgs in
#print axioms option_residual

/-- info: 'OptionBoundaryLaws.marker_equality' does not depend on any axioms -/
#guard_msgs in
#print axioms marker_equality

/-- info: 'OptionBoundaryLaws.marker_caller' does not depend on any axioms -/
#guard_msgs in
#print axioms marker_caller

end OptionBoundaryLaws
