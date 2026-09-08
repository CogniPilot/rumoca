import RumocaCompleteFactContract

open Aeneas Aeneas.Std Aeneas.Std.WP
open rumoca_phase_solve
open rumoca_phase_solve.scalar_constant_derivative_refinement
open rumoca_ir_solve.linear_op
open RumocaKernelPilot RumocaFactContract RumocaCompleteFactContract

namespace RumocaProjectionContract

set_option maxHeartbeats 1000000
set_option autoImplicit false

/-- Positive original-instruction meaning of a fact. Storage correspondence is
the explicitly trusted external model, not a floating-arithmetic theorem. -/
def Represents (operation : LinearOp) : OperationFact → Prop
  | .Constant destination bits =>
    ∃ value, operation = .Const destination value ∧ value.storageBits = bits
  | .LoadY destination index => operation = .LoadY destination index
  | .StoreOutput source => operation = .StoreOutput source
  | .Unsupported =>
    (∀ destination value, operation ≠ .Const destination value) ∧
    (∀ destination index, operation ≠ .LoadY destination index) ∧
    (∀ source, operation ≠ .StoreOutput source)

theorem classification_sound_and_total (operation : LinearOp) :
    ∃ fact, operation_fact operation = .ok fact ∧ Represents operation fact := by
  cases operation
  all_goals simp [operation_fact, core.f64.F64.to_bits, Represents]

@[step]
theorem classification_spec (operation : LinearOp) :
    operation_fact operation ⦃ Represents operation ⦄ := by
  obtain ⟨fact, runs, faithful⟩ := classification_sound_and_total operation
  simp [runs, faithful]

theorem constant_exact (operation : LinearOp) (destination : U32) (bits : U64) :
    operation_fact operation = .ok (.Constant destination bits) ↔
      operation = .Const destination ⟨bits⟩ := by
  cases operation
  all_goals simp [operation_fact, core.f64.F64.to_bits]
  next dst value => cases value; simp [F64.mk.injEq]

theorem load_exact (operation : LinearOp) (destination : U32) (index : Usize) :
    operation_fact operation = .ok (.LoadY destination index) ↔
      operation = .LoadY destination index := by
  cases operation <;> simp [operation_fact, core.f64.F64.to_bits]

theorem store_exact (operation : LinearOp) (source : U32) :
    operation_fact operation = .ok (.StoreOutput source) ↔
      operation = .StoreOutput source := by
  cases operation <;> simp [operation_fact, core.f64.F64.to_bits]

/-- Equal lengths and a faithful fact at every original position. Unsupported
instructions retain their position; no filtering, truncation or reordering. -/
def RepresentsProgram (operations : List LinearOp) (facts : List OperationFact) : Prop :=
  operations.length = facts.length ∧
  ∀ (index : Nat) operation fact, operations[index]? = some operation →
    facts[index]? = some fact → Represents operation fact

theorem projection_loop_spec {width : Usize} (operations : Array LinearOp width)
    (scratch : Array OperationFact width) :
    project_operations_loop operations scratch 0#usize ⦃ facts =>
      RepresentsProgram operations.val facts.val ⦄ := by
  unfold project_operations_loop
  apply loop.spec_decr_nat
    (fun (_, index) => width.val - index.val)
    (fun (facts, index) => index.val ≤ width.val ∧
      ∀ j, j < index.val → ∀ operation fact,
        operations.val[j]? = some operation → facts.val[j]? = some fact →
          Represents operation fact)
  · rintro ⟨facts, index⟩ ⟨bound, matchedPrefix⟩
    dsimp
    unfold project_operations_loop.body
    split
    · step as ⟨operation, hop⟩
      step with classification_spec as ⟨fact, hfact⟩
      step as ⟨updated, hupdated⟩
      step as ⟨next, hnext⟩
      refine ⟨?_, ?_, ?_⟩
      · scalar_tac
      · intro j hj original classified horiginal hclassified
        subst updated
        simp only [Array.set_val_eq] at hclassified
        by_cases same : j = index.val
        · subst j
          grind
        · have earlier : j < index.val := by scalar_tac
          have previous := matchedPrefix j earlier original classified horiginal
          grind
      · scalar_tac
    · simp only [spec_ok]
      constructor
      · simp
      · intro j operation fact hop hfact
        have inRange : j < operations.val.length := by grind
        apply matchedPrefix j (by scalar_tac) operation fact hop hfact
  · simp

/-- The sole program must have the requested complete width; refusal carries
no prefix. Successful projection relates every fact to its original opcode. -/
def ProjectionRelation (width : Usize)
    (programs : Slice (alloc.vec.Vec LinearOp)) : ScalarBlockFacts width → Prop
  | .UnsupportedShape =>
    ¬ ∃ program, programs.val = [program] ∧ program.val.length = width.val
  | .Exact facts =>
    ∃ program, programs.val = [program] ∧ RepresentsProgram program.val facts.val

theorem projection_spec (width : Usize) (programs : Slice (alloc.vec.Vec LinearOp)) :
    project_operations width programs ⦃ ProjectionRelation width programs ⦄ := by
  obtain ⟨selected, selection⟩ := selection_total width programs
  unfold project_operations
  rw [selection]
  cases selected with
  | none =>
    simp only [bind_tc_ok, spec_ok, ProjectionRelation]
    intro ⟨program, sole, size⟩
    obtain ⟨operations, selected, _⟩ := selection_complete width programs program sole size
    simp_all
  | some operations =>
    simp only [bind_tc_ok]
    step with projection_loop_spec as ⟨facts, faithful⟩
    have complete := selection_sound width programs operations selection
    have one : programs.val.length = 1 := by
      have length := congrArg List.length complete
      simpa using length
    obtain ⟨program, sole⟩ := List.length_eq_one_iff.mp one
    have same : program.val = operations.val := by simpa [sole] using complete
    exact ⟨program, sole, same ▸ faithful⟩

theorem projection_total (width : Usize) (programs : Slice (alloc.vec.Vec LinearOp)) :
    ∃ facts, project_operations width programs = .ok facts := by
  obtain ⟨facts, runs, _⟩ := spec_imp_exists (projection_spec width programs)
  exact ⟨facts, runs⟩

theorem projection_exact_sound {width : Usize}
    (programs : Slice (alloc.vec.Vec LinearOp)) (facts : Array OperationFact width)
    (projected : project_operations width programs = .ok (.Exact facts)) :
    ∃ program, programs.val = [program] ∧ RepresentsProgram program.val facts.val := by
  obtain ⟨result, runs, faithful⟩ := spec_imp_exists (projection_spec width programs)
  have same : result = .Exact facts := by simpa [projected] using runs.symm
  simpa [same, ProjectionRelation] using faithful

theorem projection_refuses_iff (width : Usize)
    (programs : Slice (alloc.vec.Vec LinearOp)) :
    project_operations width programs = .ok .UnsupportedShape ↔
      ¬ ∃ program, programs.val = [program] ∧ program.val.length = width.val := by
  obtain ⟨result, runs, faithful⟩ := spec_imp_exists (projection_spec width programs)
  cases result with
  | UnsupportedShape => simp_all [ProjectionRelation]
  | Exact facts =>
    obtain ⟨program, sole, size, _⟩ := faithful
    have fits : program.val.length = width.val := by simpa using size
    simp [runs, sole, fits]

theorem represents_constant_iff (operation : LinearOp) (destination : U32) (bits : U64) :
    Represents operation (.Constant destination bits) ↔
      operation = .Const destination ⟨bits⟩ := by
  constructor
  · rintro ⟨value, same, storage⟩
    cases value
    simp_all
  · intro same
    exact ⟨⟨bits⟩, same, rfl⟩

/-- Every sole exact-width input produces a complete projection, including
zero width. Soundness cannot be satisfied by making the caller always refuse. -/
theorem projection_complete (width : Usize) (programs : Slice (alloc.vec.Vec LinearOp))
    (program : alloc.vec.Vec LinearOp) (sole : programs.val = [program])
    (size : program.val.length = width.val) :
    ∃ facts, project_operations width programs = .ok (.Exact facts) ∧
      RepresentsProgram program.val facts.val := by
  obtain ⟨result, runs, faithful⟩ := spec_imp_exists (projection_spec width programs)
  cases result with
  | UnsupportedShape => exact False.elim (faithful ⟨program, sole, size⟩)
  | Exact facts =>
    obtain ⟨original, same, represented⟩ := faithful
    have identity : original = program := by simpa [sole] using same.symm
    exact ⟨facts, runs, identity ▸ represented⟩

theorem represented_constant_program (operations : List LinearOp) (bits : U64)
    (faithful : RepresentsProgram operations [.Constant 0#u32 bits, .StoreOutput 0#u32]) :
    operations = [.Const 0#u32 ⟨bits⟩, .StoreOutput 0#u32] := by
  obtain ⟨first, second, shape⟩ := List.length_eq_two.mp faithful.1
  subst operations
  have firstMeaning := faithful.2 0 first (.Constant 0#u32 bits) rfl rfl
  have secondMeaning := faithful.2 1 second (.StoreOutput 0#u32) rfl rfl
  rw [(represents_constant_iff _ _ _).mp firstMeaning]
  simp_all [Represents]

/-- The actual projection and checker identify the entire original program,
not just a matching count or an assumed interpretation of its first slots. -/
theorem projected_kernel_exact (programs : Slice (alloc.vec.Vec LinearOp))
    (facts : ScalarBlockFacts 2#usize) (bits : U64)
    (projected : project_operations 2#usize programs = .ok facts)
    (accepted : check_kernel facts bits = .ok (.Ok ())) :
    programs.val.map alloc.vec.Vec.val = [[.Const 0#u32 ⟨bits⟩, .StoreOutput 0#u32]] := by
  have permitted := (accepted_iff_permitted facts bits).mp accepted
  cases facts with
  | UnsupportedShape => simp [RumocaKernelPilot.permitted] at permitted
  | Exact facts =>
    obtain ⟨program, sole, faithful⟩ := projection_exact_sound programs facts projected
    have original := represented_constant_program program.val bits (permitted ▸ faithful)
    simp [sole, original]

theorem projected_jacobian_exact (programs : Slice (alloc.vec.Vec LinearOp))
    (facts : ScalarBlockFacts 3#usize) (bits : U64)
    (projected : project_operations 3#usize programs = .ok facts)
    (accepted : check_full_jacobian facts bits = .ok (.Ok ())) :
    programs.val.map alloc.vec.Vec.val =
      [[.Const 0#u32 ⟨bits⟩, .Const 1#u32 ⟨0#u64⟩, .StoreOutput 1#u32]] := by
  have permitted := (jacobian_accepted_iff facts bits).mp accepted
  cases facts with
  | UnsupportedShape => simp [jacobianPermitted] at permitted
  | Exact facts =>
    obtain ⟨program, sole, faithful⟩ := projection_exact_sound programs facts projected
    obtain ⟨first, second, third, shape⟩ := List.length_eq_three.mp
      (show program.val.length = 3 by simpa using faithful.1)
    have meaning := faithful.2
    rw [shape, permitted] at meaning
    have firstMeaning := (represents_constant_iff _ _ _).mp
      (meaning 0 first (.Constant 0#u32 bits) rfl rfl)
    have secondMeaning := (represents_constant_iff _ _ _).mp
      (meaning 1 second (.Constant 1#u32 0#u64) rfl rfl)
    have thirdMeaning := meaning 2 third (.StoreOutput 1#u32) rfl rfl
    simp_all [Represents]

theorem projected_visible_exact (programs : Slice (alloc.vec.Vec LinearOp))
    (facts : ScalarBlockFacts 2#usize)
    (projected : project_operations 2#usize programs = .ok facts)
    (accepted : check_visible_rows facts = .ok (.Ok ())) :
    programs.val.map alloc.vec.Vec.val = [[.LoadY 0#u32 0#usize, .StoreOutput 0#u32]] := by
  have permitted := (visible_accepted_iff facts).mp accepted
  cases facts with
  | UnsupportedShape => simp [visiblePermitted] at permitted
  | Exact facts =>
    obtain ⟨program, sole, faithful⟩ := projection_exact_sound programs facts projected
    obtain ⟨first, second, shape⟩ := List.length_eq_two.mp
      (show program.val.length = 2 by simpa using faithful.1)
    have meaning := faithful.2
    rw [shape, permitted] at meaning
    have firstMeaning := meaning 0 first (.LoadY 0#u32 0#usize) rfl rfl
    have secondMeaning := meaning 1 second (.StoreOutput 0#u32) rfl rfl
    simp_all [Represents]

/-- Whole comparison acceptance now identifies all three actual input programs.
The hypotheses bind the actual projector results, not an arbitrary classifier.
Outer ComputeBlock/root selection and other fact projections remain outside. -/
theorem complete_original_programs (profile : AdmittedScalarConstantDerivativeProfile)
    (facts : SolveFacts) (derivative jacobian visible : Slice (alloc.vec.Vec LinearOp))
    (derivativeProjection : project_operations 2#usize derivative = .ok facts.kernel)
    (jacobianProjection : project_operations 3#usize jacobian = .ok facts.full_jacobian)
    (visibleProjection : project_operations 2#usize visible = .ok facts.visible_rows)
    (accepted : check_scalar_constant_derivative_refinement profile facts = .ok (.Ok ⟨()⟩)) :
    derivative.val.map alloc.vec.Vec.val =
      [[.Const 0#u32 ⟨profile.derivative_constant_bits⟩, .StoreOutput 0#u32]] ∧
    jacobian.val.map alloc.vec.Vec.val =
      [[.Const 0#u32 ⟨profile.derivative_constant_bits⟩,
        .Const 1#u32 ⟨0#u64⟩, .StoreOutput 1#u32]] ∧
    visible.val.map alloc.vec.Vec.val = [[.LoadY 0#u32 0#usize, .StoreOutput 0#u32]] := by
  have exactFacts := (whole_contract profile facts).1.mp accepted
  obtain ⟨_, _, _, _, _, kernel, tangent, state, _, _⟩ :=
    (facts_exact profile facts).mpr exactFacts
  exact ⟨projected_kernel_exact derivative facts.kernel _ derivativeProjection
      ((accepted_iff_permitted _ _).mpr kernel),
    projected_jacobian_exact jacobian facts.full_jacobian _ jacobianProjection
      ((jacobian_accepted_iff _ _).mpr tangent),
    projected_visible_exact visible facts.visible_rows visibleProjection
      ((visible_accepted_iff _).mpr state)⟩

/-- Relational reference semantics reuses the existing independent interpreter.
Its witness describes every original operation; this is not a second runtime. -/
def OriginalProgramExecutes (programs : Slice (alloc.vec.Vec LinearOp))
    (inputs : Usize → Option U64) (output : Option (List U64)) : Prop :=
  ∃ program facts, programs.val = [program] ∧ RepresentsProgram program.val facts ∧
    execute inputs facts (fun _ => none) = output

theorem projected_execution_sound {width : Usize}
    (programs : Slice (alloc.vec.Vec LinearOp)) (facts : Array OperationFact width)
    (inputs : Usize → Option U64) (output : Option (List U64))
    (projected : project_operations width programs = .ok (.Exact facts))
    (executed : execute inputs facts.val (fun _ => none) = output) :
    OriginalProgramExecutes programs inputs output := by
  obtain ⟨program, sole, faithful⟩ := projection_exact_sound programs facts projected
  exact ⟨program, facts.val, sole, faithful, executed⟩

theorem complete_projection_executes (profile : AdmittedScalarConstantDerivativeProfile)
    (facts : SolveFacts) (derivative jacobian visible : Slice (alloc.vec.Vec LinearOp))
    (derivativeProjection : project_operations 2#usize derivative = .ok facts.kernel)
    (jacobianProjection : project_operations 3#usize jacobian = .ok facts.full_jacobian)
    (visibleProjection : project_operations 2#usize visible = .ok facts.visible_rows)
    (accepted : check_scalar_constant_derivative_refinement profile facts = .ok (.Ok ⟨()⟩))
    (inputs : Usize → Option U64) :
    OriginalProgramExecutes derivative inputs (some [profile.derivative_constant_bits]) ∧
    OriginalProgramExecutes jacobian inputs (some [0#u64]) ∧
    OriginalProgramExecutes visible inputs ((inputs 0#usize).map (fun bits => [bits])) := by
  have transferred := complete_executes profile facts accepted inputs
  have exactFacts := (whole_contract profile facts).1.mp accepted
  subst facts
  refine ⟨?_, ?_, ?_⟩
  · apply projected_execution_sound derivative _ inputs _ derivativeProjection
    simpa [canonicalFacts, canonical, executeKernel] using transferred.2.2.1
  · apply projected_execution_sound jacobian _ inputs _ jacobianProjection
    simpa [canonicalFacts] using transferred.2.2.2.1
  · apply projected_execution_sound visible _ inputs _ visibleProjection
    simpa [canonicalFacts, executeKernel] using transferred.2.2.2.2

/-- info: 'RumocaProjectionContract.classification_sound_and_total' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in
#print axioms classification_sound_and_total
/-- info: 'RumocaProjectionContract.projection_loop_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms projection_loop_spec
/-- info: 'RumocaProjectionContract.projection_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms projection_spec
/-- info: 'RumocaProjectionContract.projection_refuses_iff' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms projection_refuses_iff
/-- info: 'RumocaProjectionContract.projection_complete' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms projection_complete
/-- info: 'RumocaProjectionContract.complete_original_programs' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms complete_original_programs
/-- info: 'RumocaProjectionContract.complete_projection_executes' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms complete_projection_executes

/-- The actual scalar-block selector observes precisely its stored output map
and programs. All other block fields remain outside this projection relation. -/
theorem scalar_block_projection_eq (width : Usize) (block : rumoca_ir_solve.ScalarProgramBlock) :
    project_scalar_block width block =
      if block.output_indices.val = [0#usize] then
        project_operations width block.programs.slice
      else .ok .UnsupportedShape := by
  by_cases exactOutput : block.output_indices.val = [0#usize]
  · simp [project_scalar_block, rumoca_ir_solve.ScalarProgramBlock.impl.output_indices,
      rumoca_ir_solve.ScalarProgramBlock.impl.programs, alloc.vec.Vec.deref,
      Slice.index_usize, alloc.vec.Vec.val] at exactOutput ⊢
    simp [exactOutput]
  · by_cases one : block.output_indices.val.length = 1
    · obtain ⟨output, sole⟩ := List.length_eq_one_iff.mp one
      have notZero : output ≠ 0#usize := by
        intro zero
        exact exactOutput (by simp [sole, zero])
      have notZeroValue : output.val ≠ 0 := by scalar_tac
      simp [project_scalar_block, rumoca_ir_solve.ScalarProgramBlock.impl.output_indices,
        rumoca_ir_solve.ScalarProgramBlock.impl.programs, alloc.vec.Vec.deref,
        Slice.index_usize, alloc.vec.Vec.val] at sole ⊢
      simp [sole, notZero, notZeroValue]
    · simp [project_scalar_block, rumoca_ir_solve.ScalarProgramBlock.impl.output_indices,
        rumoca_ir_solve.ScalarProgramBlock.impl.programs, alloc.vec.Vec.deref,
        Slice.index_usize, alloc.vec.Vec.val] at one exactOutput ⊢
      simp [one, exactOutput]

/-- Complete node-list selection: no extra scalar or tensor node is discarded. -/
theorem kernel_projection_eq (block : rumoca_ir_solve.tensor.ComputeBlock) :
    project_kernel block =
      match block.nodes.val with
      | [.ScalarPrograms scalar] => project_scalar_block 2#usize scalar
      | _ => .ok .UnsupportedShape := by
  cases nodes : block.nodes.val with
  | nil =>
    simp [project_kernel, alloc.vec.Vec.len, alloc.vec.Vec.val] at nodes ⊢
    simp [nodes]
  | cons first rest =>
    cases rest with
    | nil =>
      cases first
      all_goals simp [project_kernel, alloc.vec.Vec.len, alloc.vec.Vec.index,
        Slice.index_usize, alloc.vec.Vec.val] at nodes ⊢
      all_goals simp [nodes]
    | cons second rest =>
      simp [project_kernel, alloc.vec.Vec.len, alloc.vec.Vec.index,
        Slice.index_usize, alloc.vec.Vec.val] at nodes ⊢
      simp [nodes]

theorem scalar_block_exact_binding {width : Usize}
    (block : rumoca_ir_solve.ScalarProgramBlock) (facts : Array OperationFact width)
    (projected : project_scalar_block width block = .ok (.Exact facts)) :
    block.output_indices.val = [0#usize] ∧
      project_operations width block.programs.slice = .ok (.Exact facts) := by
  rw [scalar_block_projection_eq] at projected
  split at projected
  · exact ⟨by assumption, projected⟩
  · simp at projected

theorem scalar_block_total (width : Usize) (block : rumoca_ir_solve.ScalarProgramBlock) :
    ∃ facts, project_scalar_block width block = .ok facts := by
  rw [scalar_block_projection_eq]
  split
  · exact projection_total width block.programs.slice
  · exact ⟨.UnsupportedShape, rfl⟩

theorem scalar_block_refuses_iff (width : Usize) (block : rumoca_ir_solve.ScalarProgramBlock) :
    project_scalar_block width block = .ok .UnsupportedShape ↔
      ¬ (block.output_indices.val = [0#usize] ∧
        ∃ program, block.programs.val = [program] ∧ program.val.length = width.val) := by
  rw [scalar_block_projection_eq]
  split
  · rename_i exactOutput
    simp only [exactOutput, true_and]
    simpa only [alloc.vec.Vec.val] using projection_refuses_iff width block.programs.slice
  · simp_all

theorem kernel_exact_binding (block : rumoca_ir_solve.tensor.ComputeBlock)
    (facts : Array OperationFact 2#usize)
    (projected : project_kernel block = .ok (.Exact facts)) :
    ∃ scalar, block.nodes.val = [.ScalarPrograms scalar] ∧
      scalar.output_indices.val = [0#usize] ∧
      project_operations 2#usize scalar.programs.slice = .ok (.Exact facts) := by
  rw [kernel_projection_eq] at projected
  split at projected
  · exact ⟨_, by assumption, scalar_block_exact_binding _ _ projected⟩
  · simp at projected

theorem kernel_total (block : rumoca_ir_solve.tensor.ComputeBlock) :
    ∃ facts, project_kernel block = .ok facts := by
  rw [kernel_projection_eq]
  split
  · exact scalar_block_total 2#usize _
  · exact ⟨.UnsupportedShape, rfl⟩

theorem kernel_refuses_iff (block : rumoca_ir_solve.tensor.ComputeBlock) :
    project_kernel block = .ok .UnsupportedShape ↔
      ¬ ∃ scalar, block.nodes.val = [.ScalarPrograms scalar] ∧
        scalar.output_indices.val = [0#usize] ∧
        ∃ program, scalar.programs.val = [program] ∧ program.val.length = 2 := by
  rw [kernel_projection_eq]
  split
  · rename_i scalar sole
    simp [sole, scalar_block_refuses_iff]
  · constructor
    · intro _ ⟨scalar, sole, _⟩
      simp_all
    · intro _
      rfl

/-- The enclosing blocks now supply the exact program slices used by the
accepted fact-level proof. Selection of these blocks from SolveModel and
justification of the admitted DAE profile are still explicit outer obligations. -/
theorem complete_block_semantics (profile : AdmittedScalarConstantDerivativeProfile)
    (facts : SolveFacts) (derivative : rumoca_ir_solve.tensor.ComputeBlock)
    (jacobian visible : rumoca_ir_solve.ScalarProgramBlock)
    (derivativeProjection : project_kernel derivative = .ok facts.kernel)
    (jacobianProjection : project_scalar_block 3#usize jacobian = .ok facts.full_jacobian)
    (visibleProjection : project_scalar_block 2#usize visible = .ok facts.visible_rows)
    (accepted : check_scalar_constant_derivative_refinement profile facts = .ok (.Ok ⟨()⟩))
    (inputs : Usize → Option U64) :
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
  have exactFacts := (whole_contract profile facts).1.mp accepted
  subst facts
  obtain ⟨scalar, sole, output, derivativePrograms⟩ :=
    kernel_exact_binding derivative _ derivativeProjection
  obtain ⟨jacobianOutput, jacobianPrograms⟩ := scalar_block_exact_binding jacobian _ jacobianProjection
  obtain ⟨visibleOutput, visiblePrograms⟩ := scalar_block_exact_binding visible _ visibleProjection
  have originals := complete_original_programs profile (canonicalFacts profile)
    scalar.programs.slice jacobian.programs.slice visible.programs.slice
    derivativePrograms jacobianPrograms visiblePrograms accepted
  have executions := complete_projection_executes profile (canonicalFacts profile)
    scalar.programs.slice jacobian.programs.slice visible.programs.slice
    derivativePrograms jacobianPrograms visiblePrograms accepted inputs
  exact ⟨scalar, sole, output, jacobianOutput, visibleOutput,
    originals.1, originals.2.1, originals.2.2, executions⟩

/-- info: 'RumocaProjectionContract.scalar_block_projection_eq' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms scalar_block_projection_eq
/-- info: 'RumocaProjectionContract.kernel_projection_eq' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms kernel_projection_eq
/-- info: 'RumocaProjectionContract.scalar_block_total' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms scalar_block_total
/-- info: 'RumocaProjectionContract.kernel_total' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms kernel_total
/-- info: 'RumocaProjectionContract.scalar_block_refuses_iff' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms scalar_block_refuses_iff
/-- info: 'RumocaProjectionContract.kernel_refuses_iff' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms kernel_refuses_iff
/-- info: 'RumocaProjectionContract.complete_block_semantics' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms complete_block_semantics

end RumocaProjectionContract
