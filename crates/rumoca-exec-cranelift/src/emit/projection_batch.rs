//! Native execution of the complete construction-issued colored application.
//!
//! Every application prepared from one compiled Jacobian source is emitted
//! into one shared JIT module: host symbols are registered and pure-call
//! imports declared once, and fold kernels and owned conditional helpers are
//! defined once and called by every application whose programs carry them.

use super::*;
use rumoca_ir_solve::{FunctionConditionalOwnerId, ProjectionJacobianApplication};

#[cfg(test)]
mod shared_module_tests;

/// Lazily created JIT module shared by the projection applications of one
/// compiled Jacobian source.
#[derive(Default)]
pub(crate) struct SharedProjectionModule(RefCell<Option<Rc<ProjectionModule>>>);

impl SharedProjectionModule {
    fn module(
        &self,
        pure_calls: Option<&Rc<typed_program::CompiledPureCallTable>>,
    ) -> Result<Rc<ProjectionModule>, CompileError> {
        let mut slot = self.0.borrow_mut();
        if let Some(module) = slot.as_ref() {
            return Ok(module.clone());
        }
        let module = Rc::new(ProjectionModule::new(pure_calls)?);
        *slot = Some(module.clone());
        Ok(module)
    }

    /// Drop the shared module after a failed emission, which can leave
    /// declared but undefined functions behind; the next application starts
    /// a fresh module. Applications already compiled keep theirs alive.
    fn discard(&self) {
        self.0.borrow_mut().take();
    }
}

struct ProjectionModule {
    /// Keeps the pure-call table the module's imports resolve to alive for as
    /// long as any application compiled into the module.
    _pure_calls: Option<Rc<typed_program::CompiledPureCallTable>>,
    state: RefCell<ProjectionModuleState>,
}

struct ProjectionModuleState {
    /// Conditional helpers already defined, per scalar-program source block.
    scopes: Vec<ConditionalScope>,
    applications: usize,
    /// Last field: owns the executable memory every compiled application
    /// points into.
    emitter: CraneliftEmitter,
}

struct ConditionalScope {
    source: ScalarProgramBlock,
    functions: HashMap<FunctionConditionalOwnerId, FuncId>,
}

impl ProjectionModule {
    fn new(
        pure_calls: Option<&Rc<typed_program::CompiledPureCallTable>>,
    ) -> Result<Self, CompileError> {
        Ok(Self {
            _pure_calls: pure_calls.cloned(),
            state: RefCell::new(ProjectionModuleState {
                scopes: Vec::new(),
                applications: 0,
                emitter: CraneliftEmitter::new(pure_calls.map(Rc::as_ref))?,
            }),
        })
    }

    fn compile(
        &self,
        application: &ProjectionJacobianApplication,
    ) -> Result<JacobianRowFn, CompileError> {
        let mut state = self.state.borrow_mut();
        let state = &mut *state;
        let scope = match state
            .scopes
            .iter()
            .position(|scope| scope.source.shares_program_owner(application.source()))
        {
            Some(scope) => scope,
            None => {
                state.scopes.push(ConditionalScope {
                    source: application.source().clone(),
                    functions: HashMap::new(),
                });
                state.scopes.len() - 1
            }
        };
        let emitter = &mut state.emitter;
        emitter.conditional_scope = Some(scope);
        emitter.conditional_functions = std::mem::take(&mut state.scopes[scope].functions);
        let name = format!(
            "rumoca_projection_{}_application_{}",
            application.block_index(),
            state.applications
        );
        state.applications += 1;
        let compiled = emit_projection_application(emitter, application, &name);
        state.scopes[scope].functions = std::mem::take(&mut emitter.conditional_functions);
        compiled
    }
}

fn emit_projection_application(
    emitter: &mut CraneliftEmitter,
    application: &ProjectionJacobianApplication,
    name: &str,
) -> Result<JacobianRowFn, CompileError> {
    for color in application.colors() {
        for program in color.outputs().programs() {
            let row = &application.source().programs()[program.program()];
            emitter.ensure_fold_programs(row, RowKind::JacobianV)?;
            emitter.ensure_conditional_programs(row, RowKind::JacobianV)?;
        }
    }
    let id = emitter.compile_projection_application(application, name)?;
    finalize_jit_module(&mut emitter.module)?;
    finalized_jacobian_fn(&emitter.module, id)
}

pub(super) struct ProjectionBatch {
    function: JacobianRowFn,
    _module: Rc<ProjectionModule>,
}

impl ProjectionBatch {
    pub(super) fn compile(
        application: &ProjectionJacobianApplication,
        pure_calls: Option<&Rc<typed_program::CompiledPureCallTable>>,
        shared: &SharedProjectionModule,
    ) -> Result<Self, CompileError> {
        let module = shared.module(pure_calls)?;
        match module.compile(application) {
            Ok(function) => Ok(Self {
                function,
                _module: module,
            }),
            Err(error) => {
                shared.discard();
                Err(error)
            }
        }
    }

    pub(super) fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &mut [f64],
        output: &mut [f64],
    ) -> Result<(), CompileError> {
        // SAFETY: the prepared application checks all input/output extents and
        // exclusively leases both scratch buffers. The seed pointer refers to
        // writable storage; this entry sets and clears its issued color seeds.
        status::check(unsafe {
            (self.function)(
                y.as_ptr(),
                p.as_ptr(),
                t,
                seed.as_mut_ptr(),
                output.as_mut_ptr(),
            )
        })
    }
}

impl CraneliftEmitter {
    fn compile_projection_application(
        &mut self,
        application: &ProjectionJacobianApplication,
        name: &str,
    ) -> Result<FuncId, CompileError> {
        let pointer = self.module.target_config().pointer_type();
        let mut signature = self.module.make_signature();
        signature.returns.push(AbiParam::new(types::I8));
        for kind in [pointer, pointer, types::F64, pointer, pointer] {
            signature.params.push(AbiParam::new(kind));
        }
        let id = self
            .module
            .declare_function(name, Linkage::Local, &signature)
            .map_err(to_backend_err)?;
        let mut context = self.module.make_context();
        context.func.signature = signature;
        let mut frontend = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut context.func, &mut frontend);
        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);
        let params = builder.block_params(entry).to_vec();
        let mut loaded_y = HashMap::new();
        let mut loaded_p = HashMap::new();
        let mut retained: Vec<Vec<Option<RetainedResult>>> = application
            .source()
            .programs()
            .iter()
            .map(|row| vec![None; row.len()])
            .collect();
        for color in application.colors() {
            set_seeds(&mut builder, params[3], color.seed_indices(), 1.0)?;
            for program in color.outputs().programs() {
                let row = &application.source().programs()[program.program()];
                self.lower_projection_program(
                    &mut builder,
                    &params,
                    row,
                    program.placements(),
                    (&mut loaded_y, &mut loaded_p),
                    ProgramReuse {
                        issued: application.invariant_operations(program.program()),
                        retained: &mut retained[program.program()],
                    },
                )?;
            }
            set_seeds(&mut builder, params[3], color.seed_indices(), 0.0)?;
        }
        status::succeed(&mut builder);
        builder.finalize();
        self.module
            .define_function(id, &mut context)
            .map_err(to_backend_err)?;
        self.module.clear_context(&mut context);
        Ok(id)
    }

    fn lower_projection_program(
        &mut self,
        builder: &mut FunctionBuilder<'_>,
        params: &[cranelift_codegen::ir::Value],
        row: &[LinearOp],
        placements: &[(usize, usize)],
        loads: (
            &mut HashMap<usize, cranelift_codegen::ir::Value>,
            &mut HashMap<usize, cranelift_codegen::ir::Value>,
        ),
        reuse: ProgramReuse<'_>,
    ) -> Result<(), CompileError> {
        let pointer = self.module.target_config().pointer_type();
        let backing_regs_ptr = create_row_register_tape(builder, pointer, row)?;
        let mut regs = HashMap::new();
        let mut lower = RowLowerCtx {
            fb: builder,
            module: &mut self.module,
            math: &mut self.math,
            regs: &mut regs,
            y_ptr: params[0],
            p_ptr: params[1],
            t_value: params[2],
            v_ptr: Some(params[3]),
            backing_regs_ptr,
            flags: MemFlags::new(),
            loaded_y: backing_regs_ptr.is_none().then_some(loads.0),
            loaded_p: backing_regs_ptr.is_none().then_some(loads.1),
            fold_carried: None,
            fold_indices: None,
            fold_index_constants: None,
            fold_captures: None,
            fold_captures_ptr: None,
            conditional_captures: None,
            conditional_captures_ptr: None,
            fold_functions: &self.fold_functions,
            conditional_functions: &self.conditional_functions,
            pure_call_functions: &self.pure_call_functions,
            pure_call_results: HashMap::new(),
            nested_fold_results: HashMap::new(),
            conditional_results: HashMap::new(),
            fold_carried_versions: Vec::new(),
            known_constants: HashMap::new(),
        };
        let mut outputs = Vec::new();
        for (index, operation) in row.iter().cloned().enumerate() {
            if let Some(result) = &reuse.retained[index] {
                result.restore(&mut lower)?;
                continue;
            }
            let retained =
                reuse.issued[index] && operation.dst_register_count() <= MAX_STATIC_MATRIX_WORK;
            let destination = operation.dst_register();
            let count = operation.dst_register_count();
            match operation {
                LinearOp::StoreOutputRange {
                    start,
                    count,
                    stride,
                } => {
                    outputs.extend(lower.lower_output_range(start, count, stride)?);
                }
                operation => outputs.extend(lower.lower_op(operation)?),
            }
            if retained && let Some(start) = destination {
                reuse.retained[index] = Some(RetainedResult::capture(&mut lower, start, count)?);
            }
        }
        for &(offset, target) in placements {
            let byte_offset = stack_element_byte_offset(target, "projection matrix")?;
            lower
                .fb
                .ins()
                .store(lower.flags, outputs[offset], params[4], byte_offset);
        }
        Ok(())
    }
}

fn set_seeds(
    builder: &mut FunctionBuilder<'_>,
    seed: cranelift_codegen::ir::Value,
    indices: &[usize],
    value: f64,
) -> Result<(), CompileError> {
    let value = builder.ins().f64const(value);
    for &index in indices {
        let offset = stack_element_byte_offset(index, "projection seed")?;
        builder.ins().store(MemFlags::new(), value, seed, offset);
    }
    Ok(())
}

struct ProgramReuse<'a> {
    issued: &'a [bool],
    retained: &'a mut [Option<RetainedResult>],
}

#[derive(Clone)]
struct RetainedResult {
    start: u32,
    values: Vec<(cranelift_codegen::ir::Value, Option<f64>)>,
}

impl RetainedResult {
    fn capture(
        lower: &mut RowLowerCtx<'_, '_>,
        start: u32,
        count: usize,
    ) -> Result<Self, CompileError> {
        let values = (0..count)
            .map(|offset| {
                let register = checked_reg_offset(start, offset, "projection reuse")?;
                Ok((
                    lower.lookup(register)?,
                    lower.known_constants.get(&register).copied(),
                ))
            })
            .collect::<Result<Vec<_>, CompileError>>()?;
        Ok(Self { start, values })
    }

    fn restore(&self, lower: &mut RowLowerCtx<'_, '_>) -> Result<(), CompileError> {
        for (offset, &(value, constant)) in self.values.iter().enumerate() {
            let register = checked_reg_offset(self.start, offset, "projection reuse")?;
            lower.insert(register, value)?;
            if let Some(constant) = constant {
                lower.known_constants.insert(register, constant);
            } else {
                lower.known_constants.remove(&register);
            }
        }
        Ok(())
    }
}
