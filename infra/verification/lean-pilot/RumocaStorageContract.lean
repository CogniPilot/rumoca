import RumocaVariableCheckContract

open Aeneas Aeneas.Std Aeneas.Std.WP
open rumoca_phase_solve variable_catalog_refinement
open rumoca_ir_dae.model rumoca_ir_solve.model rumoca_ir_solve.layout

namespace RumocaStorageContract

set_option autoImplicit false
attribute [local instance] Classical.propDecidable

private theorem dae_role_eq (left right : VariableRole) :
    VariableRole.Insts.CoreCmpPartialEqVariableRole.eq left right =
      .ok (decide (left = right)) := by
  cases left <;> cases right <;>
    simp [VariableRole.Insts.CoreCmpPartialEqVariableRole.eq, VariableRole.read_discriminant]

private theorem zero_add (count : Usize) :
    (0#usize + count : Result Usize) = .ok count := by
  obtain ⟨result, runs, correct⟩ :=
    spec_imp_exists (Usize.add_spec (x := 0#usize) (y := count) (by scalar_tac))
  have same : result = count := by scalar_tac
  simpa only [same] using runs

/-- A translated slice fixture, not a claim that arbitrary facts are Rust roots. -/
def singleton {α : Type} (value : α) : Slice α :=
  Slice.from [value] (by scalar_tac)

theorem initialization_spec (items : Slice DaeVariableFact) :
    derive_storage_loop items (alloc.vec.Vec.new _)
      ⦃ storage => storage.val = List.replicate items.val.length none ⦄ := by
  unfold derive_storage_loop
  apply loop.spec_decr_nat
    (fun storage => items.val.length - storage.val.length)
    (fun storage => storage.val.length ≤ items.val.length ∧
      storage.val = List.replicate storage.val.length none)
  · intro storage ⟨bound, contents⟩
    unfold derive_storage_loop.body
    dsimp only
    split
    · step as ⟨updated, appended⟩
      refine ⟨?_, ?_, ?_⟩
      · clear contents
        simp only [appended, List.length_append, List.length_singleton]
        scalar_tac
      · simp only [appended, List.length_append, List.length_singleton]
        simpa only [List.replicate_add, List.replicate_one] using
          congrArg (· ++ [none]) contents
      · clear contents
        simp only [appended, List.length_append, List.length_singleton]
        scalar_tac
    · simp only [spec_ok]
      have same : storage.val.length = items.val.length := by
        clear contents
        scalar_tac
      simpa [same] using contents
  · simp

private theorem assignment_singleton_done (source : DaeVariableFact)
    (storage : Slice (Option SolveVariableStorageRun)) (role : VariableRole)
    (column : SolveStorageColumn) (base : Usize) :
    assign_storage_role_loop (singleton source) storage role column base 1#usize =
      .ok (.Ok base, storage) := by
  unfold assign_storage_role_loop
  rw [loop]
  simp [assign_storage_role_loop.body, singleton, Slice.len]

private theorem assignment_singleton_skipped (source : DaeVariableFact)
    (storage : Slice (Option SolveVariableStorageRun)) (role : VariableRole)
    (column : SolveStorageColumn) (base : Usize) (other : source.role ≠ role) :
    assign_storage_role (singleton source) storage role column base =
      .ok (.Ok base, storage) := by
  have skipped : assign_storage_role_loop.body (singleton source) role column
      storage base 0#usize = .ok (.cont (storage, base, 1#usize)) := by
    simp [assign_storage_role_loop.body, singleton, Slice.len,
      Slice.index_usize, dae_role_eq, other, zero_add]
  unfold assign_storage_role assign_storage_role_loop
  rw [loop]
  dsimp only
  rw [skipped]
  exact assignment_singleton_done source storage role column base

def stateRun (source : DaeVariableFact) : SolveVariableStorageRun := {
  base := .Y 0#usize
  scalar_count := source.scalar_count
  role := .State
  value_kind := .Real
}

private theorem assignment_singleton_state (source : DaeVariableFact)
    (state : source.role = .State) (localState : source.causality = .Local)
    (real : source.scalar_type = .Real) :
    assign_storage_role (singleton source) (singleton none) .State .Y 0#usize =
      .ok (.Ok source.scalar_count, singleton (some (stateRun source))) := by
  have added : Usize.checked_add 0#usize source.scalar_count = some source.scalar_count := by
    change Option.ofResult (0#usize + source.scalar_count) = _
    rw [zero_add]
    rfl
  have assigned : assign_storage_role_loop.body (singleton source) .State .Y
      (singleton none) 0#usize 0#usize =
      .ok (.cont (singleton (some (stateRun source)), source.scalar_count, 1#usize)) := by
    simp [assign_storage_role_loop.body, singleton, Slice.len, Slice.index_usize,
      dae_role_eq, state, localState, real, expected_kind, expected_role,
      RumocaVariableEqualityContract.dae_causality_eq, SolveStorageCoordinate.new,
      Slice.index_mut_usize, Slice.set, Slice.setAtNat, added, stateRun,
      core.result.Result.Insts.CoreOpsTry.branch, zero_add, lift]
  unfold assign_storage_role assign_storage_role_loop
  rw [loop]
  dsimp only
  rw [assigned]
  exact assignment_singleton_done source (singleton (some (stateRun source)))
    .State .Y source.scalar_count

private theorem column_suffix_skipped (source : DaeVariableFact)
    (storage : Slice (Option SolveVariableStorageRun)) (roles : Slice VariableRole)
    (column : SolveStorageColumn) (base start : Usize)
    (startBound : start.val ≤ roles.val.length)
    (other : ∀ role ∈ roles.val.drop start.val, source.role ≠ role) :
    derive_storage_column_loop (singleton source) storage roles column base start =
      .ok (.Ok (), storage) := by
  suffices run : derive_storage_column_loop (singleton source) storage roles column base start
      ⦃ result => result = (.Ok (), storage) ⦄ by
    obtain ⟨result, runs, same⟩ := spec_imp_exists run
    simpa only [same] using runs
  unfold derive_storage_column_loop
  apply loop.spec_decr_nat
    (fun (_, _, ordinal) => roles.val.length - ordinal.val)
    (fun (current, _, ordinal) => current = storage ∧
      start.val ≤ ordinal.val ∧ ordinal.val ≤ roles.val.length)
  · rintro ⟨current, currentBase, ordinal⟩ invariant
    dsimp only at invariant ⊢
    rcases invariant with ⟨same, lower, upper⟩
    unfold derive_storage_column_loop.body
    dsimp only
    split
    · step as ⟨role, selected⟩
      have differs : source.role ≠ role := other role (by
        apply List.mem_drop_iff_getElem.mpr
        refine ⟨ordinal.val - start.val, by scalar_tac, ?_⟩
        simpa only [Nat.add_sub_of_le lower] using selected.symm)
      rw [assignment_singleton_skipped source current role column currentBase differs]
      simp only [bind_tc_ok, core.result.Result.Insts.CoreOpsTry.branch]
      step as ⟨next, incremented⟩
      refine ⟨same, ?_, ?_, ?_⟩ <;> scalar_tac
    · simpa using same
  · simpa using startBound

private theorem column_skipped (source : DaeVariableFact)
    (storage : Slice (Option SolveVariableStorageRun)) (roles : Slice VariableRole)
    (column : SolveStorageColumn)
    (other : ∀ role ∈ roles.val, source.role ≠ role) :
    derive_storage_column (singleton source) storage roles column = .ok (.Ok (), storage) := by
  exact column_suffix_skipped source storage roles column 0#usize 0#usize
    (by simp) (by simpa using other)

private abbrev yRoles : Slice VariableRole :=
  Slice.from [.State, .Algebraic, .Output] (by scalar_tac)

private theorem state_column (source : DaeVariableFact)
    (state : source.role = .State) (localState : source.causality = .Local)
    (real : source.scalar_type = .Real) :
    derive_storage_column (singleton source) (singleton none) yRoles .Y =
      .ok (.Ok (), singleton (some (stateRun source))) := by
  have assigned : derive_storage_column_loop.body (singleton source) yRoles .Y
      (singleton none) 0#usize 0#usize =
      .ok (.cont (singleton (some (stateRun source)), source.scalar_count, 1#usize)) := by
    simp [derive_storage_column_loop.body, yRoles, Slice.len, Slice.index_usize,
      assignment_singleton_state source state localState real,
      core.result.Result.Insts.CoreOpsTry.branch, zero_add]
  unfold derive_storage_column derive_storage_column_loop
  rw [loop]
  dsimp only
  rw [assigned]
  exact column_suffix_skipped source (singleton (some (stateRun source))) yRoles .Y
    source.scalar_count 1#usize (by simp [yRoles]) (by simp [yRoles, state])

private theorem initialization_singleton (source : DaeVariableFact) :
    derive_storage_loop (singleton source) (alloc.vec.Vec.new _) =
      .ok { slice := singleton none } := by
  obtain ⟨storage, runs, contents⟩ := spec_imp_exists (initialization_spec (singleton source))
  have same : storage = { slice := singleton none } := by
    apply alloc.vec.Vec.ext
    simpa [singleton, alloc.vec.Vec.val] using contents
  simpa only [same] using runs

/-- Actual initialization and both column passes derive the local Real state's
run. Counts are arbitrary translated usize values; DAE-root provenance and
multi-variable prefix overflow are not established by this singleton theorem. -/
theorem derived_singleton_state (source : DaeVariableFact)
    (state : source.role = .State) (localState : source.causality = .Local)
    (real : source.scalar_type = .Real) :
    derive_storage (singleton source) =
      .ok (.Ok { slice := singleton (some (stateRun source)) }) := by
  unfold derive_storage
  simp only [alloc.vec.Vec.with_capacity,
    initialization_singleton, bind_tc_ok, alloc.vec.Vec.deref_mut, lift,
    Array.to_slice, Array.make_val, uncurry_apply_pair]
  rw [state_column source state localState real]
  simp only [bind_tc_ok, core.result.Result.Insts.CoreOpsTry.branch, uncurry_apply_pair]
  rw [column_skipped source _ _ .P (by simp [state])]
  rfl

/-- The per-variable comparison consumes the run returned by actual derivation,
not an independently assumed expected run. Occurrence joining, full-catalog
iteration and the enclosing checked-root construction remain outside this edge. -/
theorem derived_state_fields (source : DaeVariableFact) (target : SolveVariableFact)
    (occurrence : rumoca_core.ir_primitives.SourceOccurrenceId)
    (storage : alloc.vec.Vec (Option SolveVariableStorageRun)) (run : SolveVariableStorageRun)
    (state : source.role = .State) (localState : source.causality = .Local)
    (real : source.scalar_type = .Real)
    (derived : derive_storage (singleton source) = .ok (.Ok storage))
    (present : storage.val = [some run])
    (accepted : check_variable occurrence source target run = .ok (.Ok ())) :
    RumocaVariableCheckContract.FieldsMatch source target (stateRun source) := by
  have sameStorage : storage = { slice := singleton (some (stateRun source)) } := by
    simpa only [derived, Result.ok.injEq, core.result.Result.Ok.injEq] using
      derived_singleton_state source state localState real
  have sameRun : run = stateRun source := by
    simpa [sameStorage, singleton, alloc.vec.Vec.val] using present.symm
  rw [sameRun] at accepted
  exact (RumocaVariableCheckContract.variables_accept_iff occurrence source target
    (stateRun source) (by simp [localState])).mp accepted

/-- The existing nonempty translated fixture satisfies all admission premises.
This is a Lean-domain witness, not evidence of Rust-root constructibility. -/
theorem derived_fixed_state :
    derive_storage (singleton RumocaVariableClassificationContract.fixedState) =
      .ok (.Ok { slice := singleton (some {
        base := .Y 0#usize, scalar_count := 1#usize, role := .State, value_kind := .Real
      }) }) := by
  simpa [stateRun, RumocaVariableClassificationContract.fixedState] using
    derived_singleton_state RumocaVariableClassificationContract.fixedState rfl rfl rfl

/-- info: 'RumocaStorageContract.initialization_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms initialization_spec
/-- info: 'RumocaStorageContract.derived_singleton_state' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms derived_singleton_state
/-- info: 'RumocaStorageContract.derived_state_fields' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms derived_state_fields
/-- info: 'RumocaStorageContract.derived_fixed_state' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms derived_fixed_state

end RumocaStorageContract
