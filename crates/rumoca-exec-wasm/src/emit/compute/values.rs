//! Original tensor value helper for assembly into a complete FMI operation.
//! No refresh, finite-vector validation or public result commit occurs here.

#[cfg(test)]
mod tests;

use super::*;
use crate::native_program::CompiledNativeCallProgramWasm;
use crate::typed_call::program::ProgramHelpers;
use call_program::layout::CallProgramPlan;
use rumoca_ir_solve::{
    SolvePureCallTable,
    fmi::{FmiDerivativeOperationSource, FmiKinematicDerivativeOperation},
};

pub(crate) fn emit_derivative_value_module(
    source: &FmiDerivativeOperationSource<'_>,
) -> Result<CompiledNativeCallProgramWasm, String> {
    emit_values(source.values(), &source.problem().layout, source.calls())
}

/// The same printer over the issued kinematic selection of `source`'s values.
pub(crate) fn emit_kinematic_value_module(
    source: &FmiDerivativeOperationSource<'_>,
    kinematic: &FmiKinematicDerivativeOperation,
) -> Result<CompiledNativeCallProgramWasm, String> {
    emit_values(kinematic.values(), &source.problem().layout, source.calls())
}

struct Ready<'a> {
    plan: KernelPlan<'a>,
    arena: arena::ArenaPlan,
    calls: CallProgramPlan,
    imports: Vec<MathImport>,
    bodies: Vec<Function>,
    faults: Vec<crate::TypedCallFault>,
    scratch_report: crate::ScratchReport,
}

impl<'a> Ready<'a> {
    fn new(
        source: &'a ComputeBlock,
        layout: &VarLayout,
        table: &SolvePureCallTable,
    ) -> Result<Self, String> {
        source
            .validate_shape_contract("FMI derivative tensor values")
            .map_err(|error| error.to_string())?;
        layout
            .validate_shape_contract()
            .map_err(|error| error.to_string())?;
        let plan = KernelPlan::new(source, layout)?;
        conditional::visit_operations(
            &plan.programs,
            crate::exact_assignments::profile::primal_only,
        )?;
        let sites = conditional::call_sites(&plan.programs)?;
        let mut helpers = ProgramHelpers::new(table, &sites).map_err(|error| error.to_string())?;
        let imports = collect_imports(&plan.programs)?
            .into_iter()
            .chain(helpers.math_imports().iter().copied())
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();
        helpers
            .relocate(&imports)
            .map_err(|error| error.to_string())?;
        let (bodies, faults) = helpers
            .bodies(table, crate::outline::FUNCTION_BYTES)
            .map_err(|error| error.to_string())?;
        let arena = arena::ArenaPlan::required(&plan.programs, plan.rank.max(1))?;
        let calls = CallProgramPlan::source_ordered(
            &plan.programs,
            &helpers,
            layout.y_scalars().max(plan.outputs),
            arena.last_integer_local(),
        )?;
        let scratch_report = calls.scratch_report(helpers.owner_scratch(table));
        Ok(Self {
            plan,
            arena,
            calls,
            imports,
            bodies,
            faults,
            scratch_report,
        })
    }
}

fn emit_values(
    source: &ComputeBlock,
    layout: &VarLayout,
    table: &SolvePureCallTable,
) -> Result<CompiledNativeCallProgramWasm, String> {
    let ready = Ready::new(source, layout, table)?;
    let mut module = Module::new();
    let types = call_program::add_types(&mut module);
    let mut catalog = add_import_section(&mut module, &ready.imports, &types);
    catalog.eval_function_index = catalog
        .eval_function_index
        .checked_add(u32::try_from(ready.bodies.len()).map_err(|_| "value helper count overflow")?)
        .ok_or("value entry index overflow")?;
    let mut functions = FunctionSection::new();
    for _ in &ready.bodies {
        functions.function(3);
    }
    functions.function(types.eval_type);
    module.section(&functions);
    ready.arena.add_memory(&mut module);
    let mut exports = ExportSection::new();
    exports.export("memory", ExportKind::Memory, 0);
    exports.export("eval_values", ExportKind::Func, catalog.eval_function_index);
    module.section(&exports);
    let (function, gather_faults, linear_solve_faults) =
        emit_entry(&catalog, &ready, source, layout)?;
    let mut code = CodeSection::new();
    for body in &ready.bodies {
        code.function(body);
    }
    code.function(&function);
    module.section(&code);
    Ok(CompiledNativeCallProgramWasm {
        bytes: module.finish(),
        capacity_policy: crate::CheckedNativeWasmCapacityPolicy::default(),
        scratch_bytes: ready.calls.view().bytes,
        scratch_report: ready.scratch_report,
        faults: ready.faults,
        gather_faults,
        linear_solve_faults,
        math_imports: ready.imports.iter().map(|import| import.symbol()).collect(),
        carried: Default::default(),
    })
}

type Entry = (
    Function,
    Vec<crate::NativeGatherFault>,
    Vec<crate::NativeLinearSolveFault>,
);

fn emit_entry(
    catalog: &ImportCatalog,
    ready: &Ready<'_>,
    source: &ComputeBlock,
    layout: &VarLayout,
) -> Result<Entry, String> {
    let mut locals = ready.arena.locals();
    locals.extend([(2, ValType::I32), (1, ValType::I64)]);
    let mut function = Function::new(locals);
    let mut emitter = BodyEmitter::new(catalog, &mut function);
    emitter.arena = Some(ready.arena.clone());
    emitter.calls = Some(&ready.calls);
    emitter.gather_status_base = Some(
        u32::try_from(ready.faults.len())
            .ok()
            .and_then(|n| n.checked_add(3))
            .ok_or("value fault count overflow")?,
    );
    emitter.begin_exact_schedule(layout)?;
    emitter.push(Instruction::LocalGet(SEED_PTR_PARAM));
    emitter.push(Instruction::LocalSet(OUT_PTR_PARAM));
    let counter = LOCAL_BASE + 2;
    emitter.emit_zero_output(ready.plan.outputs, counter)?;
    for (ordinal, (node, original)) in ready.plan.nodes.iter().zip(&source.nodes).enumerate() {
        emitter.kernel_ordinal = ordinal;
        emitter.program_ordinal = 0;
        emitter.program_span = match original {
            ComputeNode::Map { span, .. }
            | ComputeNode::AffineStencil { span, .. }
            | ComputeNode::MatMul { span, .. }
            | ComputeNode::LinSolve { span, .. } => Some(*span),
            ComputeNode::ScalarPrograms(_) => None,
        };
        match node {
            NodePlan::Scalar { block, outputs } => emitter.emit_mapped_programs(block, outputs)?,
            NodePlan::Affine(plan) => emitter.emit_affine(plan, counter)?,
            NodePlan::Matrix(plan) => emitter.emit_matrix(plan, counter, LOCAL_BASE)?,
        }
    }
    emitter.require_complete_root_programs()?;
    emitter.push(Instruction::I32Const(0));
    let gathers = std::mem::take(&mut emitter.gather_faults);
    let solves = std::mem::take(&mut emitter.linear_solve_faults);
    function.instruction(&Instruction::End);
    Ok((function, gathers, solves))
}
