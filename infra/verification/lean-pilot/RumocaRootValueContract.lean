import RumocaProjectionContract

open Aeneas Aeneas.Std Aeneas.Std.WP
open rumoca_phase_solve
open rumoca_phase_solve.scalar_constant_derivative_refinement
open RumocaKernelPilot RumocaFactContract RumocaCompleteFactContract RumocaProjectionContract

namespace RumocaRootValueContract

set_option maxHeartbeats 1000000
set_option autoImplicit false

/-- Exact stored bits of the first value, or zero for an empty sequence. This
does not introduce an initial value into the IR or claim numeric equality. -/
def HeadBits : List F64 → U64
  | [] => 0#u64
  | value :: _ => value.storageBits

theorem first_value_bits_eq (values : Slice F64) :
    first_value_bits values = .ok (HeadBits values.val) := by
  cases entries : values.val with
  | nil => simp [first_value_bits, Slice.index_usize, HeadBits, entries]
  | cons value rest =>
    simp [first_value_bits, Slice.index_usize, core.f64.F64.to_bits, HeadBits, entries]

/-- Absence and a present empty vector have different presence facts. -/
def StartFields : Option (Slice F64) → Bool × Usize × U64
  | none => (false, 0#usize, 0#u64)
  | some values => (true, Slice.len values, HeadBits values.val)

theorem start_projection_eq (start : Option (Slice F64)) :
    project_start_value_facts start = .ok (StartFields start) := by
  cases start <;> simp [project_start_value_facts, first_value_bits_eq, StartFields]

/-- The first catalog entry's actual start payload. No root-name lookup or
independently supplied start vector can substitute for this field. -/
def CatalogStart (model : rumoca_ir_solve.model.SolveModel) : Option (Slice F64) :=
  match model.variable_catalog.entries.val with
  | [] => none
  | entry :: _ => entry.values.start

/-- This relation selects eight fields from the root. The two additional
values are forwarded premises: their producers and provenance are unproved. -/
def RootValuesRelation (model : rumoca_ir_solve.model.SolveModel)
    (owners : Array Usize 28#usize) (metadata : SolveMetadataFacts) (facts : SolveFacts) : Prop :=
  (facts.catalog_start_present, facts.catalog_start_width, facts.catalog_start_bits) =
      StartFields (CatalogStart model) ∧
    facts.initial_y_width.val = model.initial_y.val.length ∧
    facts.initial_y_bits = HeadBits model.initial_y.val ∧
    project_kernel model.problem.continuous.derivative_rhs = .ok facts.kernel ∧
    project_scalar_block 3#usize model.artifacts.continuous.full_jacobian_v =
      .ok facts.full_jacobian ∧
    project_scalar_block 2#usize model.visible_value_rows = .ok facts.visible_rows ∧
    facts.owners = owners ∧ facts.metadata = metadata

theorem root_projection_eq (model : rumoca_ir_solve.model.SolveModel)
    (owners : Array Usize 28#usize) (metadata : SolveMetadataFacts) :
    project_solve_values_with_owner_premises model owners metadata = (do
      let kernel ← project_kernel model.problem.continuous.derivative_rhs
      let jacobian ← project_scalar_block 3#usize model.artifacts.continuous.full_jacobian_v
      let visible ← project_scalar_block 2#usize model.visible_value_rows
      let start := StartFields (CatalogStart model)
      .ok {
        catalog_start_present := start.1
        catalog_start_width := start.2.1
        catalog_start_bits := start.2.2
        initial_y_width := Slice.len model.initial_y.slice
        initial_y_bits := HeadBits model.initial_y.val
        kernel := kernel
        full_jacobian := jacobian
        visible_rows := visible
        owners := owners
        metadata := metadata
      }) := by
  unfold project_solve_values_with_owner_premises
  simp only [rumoca_ir_solve.model.SolveModel.impl.variable_catalog,
    rumoca_ir_solve.variable_catalog.SolveVariableCatalog.impl.entries,
    rumoca_ir_solve.model.SolveModel.impl.initial_y,
    rumoca_ir_solve.model.SolveModel.impl.problem,
    rumoca_ir_solve.SolveProblem.impl.continuous,
    rumoca_ir_solve.model.ContinuousSolveSystem.impl.derivative_rhs,
    rumoca_ir_solve.model.SolveModel.impl.artifacts,
    rumoca_ir_solve.model.SolveArtifacts.impl.continuous,
    rumoca_ir_solve.model.SolveModel.impl.visible_value_rows,
    alloc.boxed.Box.deref, alloc.vec.Vec.deref, bind_tc_ok]
  cases entries : model.variable_catalog.entries.val with
  | nil =>
    simp [entries, CatalogStart, start_projection_eq, StartFields, first_value_bits_eq,
      alloc.vec.Vec.val]
  | cons entry rest =>
    cases start : entry.values.start <;>
      simp [entries, CatalogStart, Slice.index_usize,
        rumoca_ir_solve.variable_catalog.SolveVariableCatalogEntry.start,
        start, alloc.boxed.Box.deref, lift, start_projection_eq, StartFields,
        first_value_bits_eq, alloc.vec.Vec.val]

theorem root_projection_sound_and_total (model : rumoca_ir_solve.model.SolveModel)
    (owners : Array Usize 28#usize) (metadata : SolveMetadataFacts) :
    ∃ facts, project_solve_values_with_owner_premises model owners metadata = .ok facts ∧
      RootValuesRelation model owners metadata facts := by
  obtain ⟨kernel, kernelRuns⟩ := kernel_total model.problem.continuous.derivative_rhs
  obtain ⟨jacobian, jacobianRuns⟩ :=
    scalar_block_total 3#usize model.artifacts.continuous.full_jacobian_v
  obtain ⟨visible, visibleRuns⟩ := scalar_block_total 2#usize model.visible_value_rows
  simp only [root_projection_eq, kernelRuns, jacobianRuns, visibleRuns, bind_tc_ok]
  refine ⟨_, rfl, ?_⟩
  simp [RootValuesRelation, kernelRuns, jacobianRuns, visibleRuns, alloc.vec.Vec.val]

theorem root_projection_sound (model : rumoca_ir_solve.model.SolveModel)
    (owners : Array Usize 28#usize) (metadata : SolveMetadataFacts) (facts : SolveFacts)
    (projected : project_solve_values_with_owner_premises model owners metadata = .ok facts) :
    RootValuesRelation model owners metadata facts := by
  obtain ⟨actual, runs, relation⟩ := root_projection_sound_and_total model owners metadata
  have same : actual = facts := by simpa [projected] using runs.symm
  simpa [same] using relation

/-- Accepted facts constrain the root's actual starts, not a separately
supplied start vector. Profile admission and both premise producers remain
outside this theorem. -/
theorem accepted_root_start_fields (profile : AdmittedScalarConstantDerivativeProfile)
    (model : rumoca_ir_solve.model.SolveModel) (owners : Array Usize 28#usize)
    (metadata : SolveMetadataFacts) (facts : SolveFacts)
    (projected : project_solve_values_with_owner_premises model owners metadata = .ok facts)
    (accepted : check_scalar_constant_derivative_refinement profile facts = .ok (.Ok ⟨()⟩)) :
    StartFields (CatalogStart model) = (true, 1#usize, profile.start_bits) ∧
      model.initial_y.val.length = 1 ∧ HeadBits model.initial_y.val = profile.start_bits := by
  have relation := root_projection_sound model owners metadata facts projected
  have exactFacts := (whole_contract profile facts).1.mp accepted
  subst facts
  rcases relation with ⟨start, width, bits, _⟩
  exact ⟨start.symm, width.symm, bits.symm⟩

/-- The checker acceptance and root-taking projection compose with the
original-program bit semantics. No owner/metadata producer or DAE admission
claim is inferred from the supplied premises. -/
theorem accepted_root_programs (profile : AdmittedScalarConstantDerivativeProfile)
    (model : rumoca_ir_solve.model.SolveModel) (owners : Array Usize 28#usize)
    (metadata : SolveMetadataFacts) (facts : SolveFacts)
    (projected : project_solve_values_with_owner_premises model owners metadata = .ok facts)
    (accepted : check_scalar_constant_derivative_refinement profile facts = .ok (.Ok ⟨()⟩))
    (inputs : Usize → Option U64) :
    let derivative := model.problem.continuous.derivative_rhs
    let jacobian := model.artifacts.continuous.full_jacobian_v
    let visible := model.visible_value_rows
    ∃ scalar, derivative.nodes.val = [.ScalarPrograms scalar] ∧
      scalar.output_indices.val = [0#usize] ∧
      jacobian.output_indices.val = [0#usize] ∧ visible.output_indices.val = [0#usize] ∧
      scalar.programs.val.map alloc.vec.Vec.val =
        [[.Const 0#u32 ⟨profile.derivative_constant_bits⟩, .StoreOutput 0#u32]] ∧
      jacobian.programs.val.map alloc.vec.Vec.val =
        [[.Const 0#u32 ⟨profile.derivative_constant_bits⟩,
          .Const 1#u32 ⟨0#u64⟩, .StoreOutput 1#u32]] ∧
      visible.programs.val.map alloc.vec.Vec.val = [[.LoadY 0#u32 0#usize, .StoreOutput 0#u32]] ∧
      OriginalProgramExecutes scalar.programs.slice inputs (some [profile.derivative_constant_bits]) ∧
      OriginalProgramExecutes jacobian.programs.slice inputs (some [0#u64]) ∧
      OriginalProgramExecutes visible.programs.slice inputs
        ((inputs 0#usize).map (fun bits => [bits])) := by
  obtain ⟨_, _, _, derivative, jacobian, visible, _⟩ :=
    root_projection_sound model owners metadata facts projected
  exact complete_block_semantics profile facts _ _ _ derivative jacobian visible accepted inputs

/-- info: 'RumocaRootValueContract.root_projection_sound_and_total' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms root_projection_sound_and_total
/-- info: 'RumocaRootValueContract.accepted_root_start_fields' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms accepted_root_start_fields
/-- info: 'RumocaRootValueContract.accepted_root_programs' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms accepted_root_programs

end RumocaRootValueContract
