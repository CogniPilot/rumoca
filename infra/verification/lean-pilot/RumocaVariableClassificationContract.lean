import RumocaPhaseSolve

open Aeneas Aeneas.Std Aeneas.Std.WP
open rumoca_phase_solve variable_catalog_refinement
open rumoca_core.ir_primitives
open rumoca_ir_dae.model rumoca_ir_dae.expression.value_types
open rumoca_ir_solve.model rumoca_ir_solve.variable_catalog

namespace RumocaVariableClassificationContract

set_option autoImplicit false
set_option maxHeartbeats 1000000

/-- Each Solve primitive category denotes the corresponding DAE category.
Records have no primitive counterpart in this classification domain. -/
def KindMatches (source : ScalarType) (target : SolveVariableValueKind) : Prop :=
  match target with
  | .Real => source = .Real
  | .Integer => source = .Integer
  | .Enumeration => source = .Enumeration
  | .Boolean => source = .Boolean
  | .String => source = .String

def KindResult (occurrence : SourceOccurrenceId) (source : ScalarType)
    (result : core.result.Result SolveVariableValueKind VariableCatalogRefinementError) : Prop :=
  match result with
  | .Ok target => KindMatches source target
  | .Err error => source = .Record ∧ error = .UnsupportedDaeScalarKind occurrence

def CausalityMatches (source : VariableCausality) (target : SolveVariableCausality) : Prop :=
  match target with
  | .Input => source = .Input
  | .Output => source = .Output
  | .Parameter => source = .Parameter
  | .CalculatedParameter => source = .CalculatedParameter
  | .Independent => source = .Independent
  | .Local => source = .Local

def InitializationMatches (role : VariableRole) (fixed : Fixity)
    (target : SolveStateInitialization) : Prop :=
  match target with
  | .Exact => role = .State ∧ fixed = .Fixed
  | .Approximate => role = .State ∧ fixed = .Free
  | .NotState => role ≠ .State

def VariabilityMatches (source : ExpressionVariability) (tunable : Bool)
    (target : SolveVariableVariability) : Prop :=
  match target with
  | .Constant => source = .Constant
  | .Fixed => source = .Parameter ∧ tunable = false
  | .Tunable => source = .Parameter ∧ tunable = true
  | .Discrete => source = .Discrete
  | .Continuous => source = .Continuous

theorem kind_spec (occurrence : SourceOccurrenceId) (source : ScalarType) :
    expected_kind occurrence source ⦃ KindResult occurrence source ⦄ := by
  cases sourceCase : source <;> simp [expected_kind, KindResult, KindMatches]

theorem kind_accepts_iff (occurrence : SourceOccurrenceId) (source : ScalarType)
    (target : SolveVariableValueKind) :
    expected_kind occurrence source = .ok (.Ok target) ↔ KindMatches source target := by
  cases source <;> cases target <;> simp [expected_kind, KindMatches]

theorem kind_refuses_iff (occurrence : SourceOccurrenceId) (source : ScalarType)
    (error : VariableCatalogRefinementError) :
    expected_kind occurrence source = .ok (.Err error) ↔
      source = .Record ∧ error = .UnsupportedDaeScalarKind occurrence := by
  cases source <;> simp [expected_kind, eq_comm]

theorem causality_spec (source : VariableCausality) :
    expected_causality source ⦃ CausalityMatches source ⦄ := by
  cases sourceCase : source <;> simp [expected_causality, CausalityMatches]

theorem initialization_spec (source : DaeVariableFact) :
    expected_state_initialization source ⦃ InitializationMatches source.role source.fixed ⦄ := by
  cases role : source.role <;> cases fixed : source.fixed <;>
    simp [expected_state_initialization, InitializationMatches, role, fixed]

theorem variability_spec (source : DaeVariableFact) :
    expected_variability source ⦃ VariabilityMatches source.variability source.is_tunable ⦄ := by
  cases variability : source.variability <;> cases tunable : source.is_tunable <;>
    simp [expected_variability, VariabilityMatches, variability, tunable]

/-- All four actual calls terminate with the specified classification or named refusal.
This is not a theorem about joint storage validity or whole-checker acceptance. -/
theorem classifiers_total_and_faithful (source : DaeVariableFact) :
    ∃ kind causality initialization variability,
      expected_kind source.occurrence source.scalar_type = .ok kind ∧
      expected_causality source.causality = .ok causality ∧
      expected_state_initialization source = .ok initialization ∧
      expected_variability source = .ok variability ∧
      KindResult source.occurrence source.scalar_type kind ∧
      CausalityMatches source.causality causality ∧
      InitializationMatches source.role source.fixed initialization ∧
      VariabilityMatches source.variability source.is_tunable variability := by
  obtain ⟨kind, kindRuns, kindCorrect⟩ := spec_imp_exists (kind_spec source.occurrence source.scalar_type)
  obtain ⟨causality, causalityRuns, causalityCorrect⟩ := spec_imp_exists (causality_spec source.causality)
  obtain ⟨initialization, initializationRuns, initializationCorrect⟩ :=
    spec_imp_exists (initialization_spec source)
  obtain ⟨variability, variabilityRuns, variabilityCorrect⟩ := spec_imp_exists (variability_spec source)
  exact ⟨kind, causality, initialization, variability, kindRuns, causalityRuns,
    initializationRuns, variabilityRuns, kindCorrect, causalityCorrect,
    initializationCorrect, variabilityCorrect⟩

theorem storage_shape_does_not_change_classification (source : DaeVariableFact)
    (dimensions : alloc.vec.Vec U32) (count : Usize) :
    expected_state_initialization { source with dimensions := dimensions, scalar_count := count } =
        expected_state_initialization source ∧
      expected_variability { source with dimensions := dimensions, scalar_count := count } =
        expected_variability source := by
  constructor <;> rfl

def fixedState : DaeVariableFact := {
  occurrence := { value := { value := 41#u32 } }
  role := .State
  fixed := .Fixed
  variability := .Continuous
  is_tunable := false
  causality := .Local
  scalar_type := .Real
  dimensions := alloc.vec.Vec.new U32
  scalar_count := 1#usize
}

theorem fixed_state_witness :
    expected_kind fixedState.occurrence fixedState.scalar_type = .ok (.Ok .Real) ∧
      expected_causality fixedState.causality = .ok .Local ∧
      expected_state_initialization fixedState = .ok .Exact ∧
      expected_variability fixedState = .ok .Continuous := by
  simp [fixedState, expected_kind, expected_causality, expected_state_initialization,
    expected_variability]

def tunableParameter : DaeVariableFact := {
  fixedState with
  role := .Parameter
  causality := .Parameter
  variability := .Parameter
  is_tunable := true
}

theorem tunable_parameter_witness :
    expected_causality tunableParameter.causality = .ok .Parameter ∧
      expected_state_initialization tunableParameter = .ok .NotState ∧
      expected_variability tunableParameter = .ok .Tunable := by
  simp [tunableParameter, expected_causality, expected_state_initialization, expected_variability]

theorem record_refusal_witness :
    expected_kind fixedState.occurrence .Record =
      .ok (.Err (.UnsupportedDaeScalarKind fixedState.occurrence)) := by
  rfl

/-- info: 'RumocaVariableClassificationContract.kind_spec' depends on axioms: [propext] -/
#guard_msgs in
#print axioms kind_spec
/-- info: 'RumocaVariableClassificationContract.kind_accepts_iff' depends on axioms: [propext] -/
#guard_msgs in
#print axioms kind_accepts_iff
/-- info: 'RumocaVariableClassificationContract.kind_refuses_iff' depends on axioms: [propext] -/
#guard_msgs in
#print axioms kind_refuses_iff
/-- info: 'RumocaVariableClassificationContract.causality_spec' depends on axioms: [propext] -/
#guard_msgs in
#print axioms causality_spec
/-- info: 'RumocaVariableClassificationContract.initialization_spec' depends on axioms: [propext] -/
#guard_msgs in
#print axioms initialization_spec
/-- info: 'RumocaVariableClassificationContract.variability_spec' depends on axioms: [propext] -/
#guard_msgs in
#print axioms variability_spec
/-- info: 'RumocaVariableClassificationContract.classifiers_total_and_faithful' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in
#print axioms classifiers_total_and_faithful
/-- info: 'RumocaVariableClassificationContract.storage_shape_does_not_change_classification' depends on axioms: [propext] -/
#guard_msgs in
#print axioms storage_shape_does_not_change_classification
/-- info: 'RumocaVariableClassificationContract.fixed_state_witness' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms fixed_state_witness
/--
info: 'RumocaVariableClassificationContract.tunable_parameter_witness' depends on axioms: [propext,
 Classical.choice,
 Quot.sound]
-/
#guard_msgs in
#print axioms tunable_parameter_witness
/-- info: 'RumocaVariableClassificationContract.record_refusal_witness' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms record_refusal_witness

end RumocaVariableClassificationContract
