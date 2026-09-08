import RumocaPhaseSolve

open Aeneas Aeneas.Std
open rumoca_phase_solve rumoca_core.ir_primitives rumoca_ir_dae.model
open rumoca_ir_solve.layout rumoca_ir_solve.model rumoca_ir_solve.variable_catalog

namespace RumocaVariableEqualityContract

set_option autoImplicit false
attribute [local instance] Classical.propDecidable

@[simp] theorem fixity_eq (left right : Fixity) :
    Fixity.Insts.CoreCmpPartialEqFixity.eq left right = .ok (decide (left = right)) := by
  cases left <;> cases right <;> simp [Fixity.Insts.CoreCmpPartialEqFixity.eq,
    Fixity.read_discriminant]

@[simp] theorem dae_causality_eq (left right : VariableCausality) :
    VariableCausality.Insts.CoreCmpPartialEqVariableCausality.eq left right = .ok (decide (left = right)) := by
  cases left <;> cases right <;> simp [VariableCausality.Insts.CoreCmpPartialEqVariableCausality.eq,
    VariableCausality.read_discriminant]

@[simp] theorem role_eq (left right : SolveVariableStorageRole) :
    SolveVariableStorageRole.Insts.CoreCmpPartialEqSolveVariableStorageRole.eq left right = .ok (decide (left = right)) := by
  cases left <;> cases right <;> simp [SolveVariableStorageRole.Insts.CoreCmpPartialEqSolveVariableStorageRole.eq,
    SolveVariableStorageRole.read_discriminant]

@[simp] theorem kind_eq (left right : SolveVariableValueKind) :
    SolveVariableValueKind.Insts.CoreCmpPartialEqSolveVariableValueKind.eq left right = .ok (decide (left = right)) := by
  cases left <;> cases right <;> simp [SolveVariableValueKind.Insts.CoreCmpPartialEqSolveVariableValueKind.eq,
    SolveVariableValueKind.read_discriminant]

@[simp] theorem causality_eq (left right : SolveVariableCausality) :
    SolveVariableCausality.Insts.CoreCmpPartialEqSolveVariableCausality.eq left right = .ok (decide (left = right)) := by
  cases left <;> cases right <;> simp [SolveVariableCausality.Insts.CoreCmpPartialEqSolveVariableCausality.eq,
    SolveVariableCausality.read_discriminant]

@[simp] theorem initialization_eq (left right : SolveStateInitialization) :
    SolveStateInitialization.Insts.CoreCmpPartialEqSolveStateInitialization.eq left right = .ok (decide (left = right)) := by
  cases left <;> cases right <;> simp [SolveStateInitialization.Insts.CoreCmpPartialEqSolveStateInitialization.eq,
    SolveStateInitialization.read_discriminant]

@[simp] theorem variability_eq (left right : SolveVariableVariability) :
    SolveVariableVariability.Insts.CoreCmpPartialEqSolveVariableVariability.eq left right = .ok (decide (left = right)) := by
  cases left <;> cases right <;> simp [SolveVariableVariability.Insts.CoreCmpPartialEqSolveVariableVariability.eq,
    SolveVariableVariability.read_discriminant]

@[simp] theorem coordinate_eq (left right : SolveStorageCoordinate) :
    SolveStorageCoordinate.Insts.CoreCmpPartialEqSolveStorageCoordinate.eq left right =
      .ok (decide (left = right)) := by
  cases left <;> cases right <;>
    simp [SolveStorageCoordinate.Insts.CoreCmpPartialEqSolveStorageCoordinate.eq,
      SolveStorageCoordinate.read_discriminant, core.cmp.impls.PartialEqUsize.eq, lift]

@[simp] theorem storage_eq (left right : SolveVariableStorageRun) :
    SolveVariableStorageRun.Insts.CoreCmpPartialEqSolveVariableStorageRun.eq left right =
      .ok (decide (left = right)) := by
  cases left
  cases right
  simp only [SolveVariableStorageRun.Insts.CoreCmpPartialEqSolveVariableStorageRun.eq,
    coordinate_eq, role_eq, kind_eq, bind_tc_ok]
  repeat' first | split | (simp_all [SolveVariableStorageRun.mk.injEq])

/-- info: 'RumocaVariableEqualityContract.fixity_eq' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms fixity_eq
/-- info: 'RumocaVariableEqualityContract.dae_causality_eq' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms dae_causality_eq
/-- info: 'RumocaVariableEqualityContract.role_eq' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms role_eq
/-- info: 'RumocaVariableEqualityContract.kind_eq' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms kind_eq
/-- info: 'RumocaVariableEqualityContract.causality_eq' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms causality_eq
/-- info: 'RumocaVariableEqualityContract.initialization_eq' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms initialization_eq
/-- info: 'RumocaVariableEqualityContract.variability_eq' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms variability_eq
/-- info: 'RumocaVariableEqualityContract.coordinate_eq' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms coordinate_eq
/-- info: 'RumocaVariableEqualityContract.storage_eq' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms storage_eq

end RumocaVariableEqualityContract
