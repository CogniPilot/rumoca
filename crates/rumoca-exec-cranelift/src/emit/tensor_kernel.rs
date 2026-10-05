//! One native loop kernel per affine tensor node (SPEC_0032 §4).
//!
//! A `Map` or `AffineStencil` node compiles to a single function whose loop
//! nest walks the node's compact domain. Each iteration executes the base
//! program with the strided loads, strided constants, and output slot that the
//! node's [`AffineKernelPlan`] defines for that point, so the kernel computes
//! exactly the scalar view's rows without one compiled function per row.

use std::marker::PhantomData;

use super::*;
use rumoca_eval_solve::AffineKernelPlan;

/// Whether the loop kernel owns a node with this base program: straight-line
/// scalar arithmetic with exactly one output. Every other program keeps the
/// per-row path, decided when the block is compiled.
fn tensor_kernel_supported(base_ops: &[LinearOp], kind: RowKind) -> bool {
    let mut outputs = 0usize;
    for op in base_ops {
        match op {
            LinearOp::Const { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::LoadY { .. }
            | LinearOp::LoadP { .. }
            | LinearOp::Move { .. }
            | LinearOp::Unary { .. }
            | LinearOp::Binary { .. }
            | LinearOp::Compare { .. }
            | LinearOp::Select { .. } => {}
            LinearOp::LoadSeed { .. } if kind.has_seed() => {}
            LinearOp::StoreOutput { .. } => outputs += 1,
            _ => return false,
        }
    }
    outputs == 1
}

/// The input lengths a kernel reads: the base program with every strided load
/// at the largest index the plan proved over the domain.
fn tensor_kernel_input_requirements(
    plan: &AffineKernelPlan,
    base_ops: &[LinearOp],
) -> Result<InputRequirements, CompileError> {
    input_requirements_for_linear_ops(&plan.max_index_row(base_ops))
}

/// Per-op role of one base program inside the loop body.
enum PointOp<'a> {
    Load(&'a rumoca_eval_solve::AffineKernelLoad),
    Const(&'a rumoca_eval_solve::AffineKernelConst),
}

impl CraneliftEmitter {
    /// Compile `program` over `plan`'s domain as one loop-nest function with
    /// the row ABI of `K`; the `out` pointer addresses the whole output vector.
    fn compile_tensor_kernel<K: KernelKind>(
        &mut self,
        plan: &AffineKernelPlan,
        program: &KernelProgram<'_, K>,
        name: &str,
    ) -> Result<FuncId, CompileError> {
        let (base_ops, kind) = (program.ops, K::ROW_KIND);
        let pointer_type = self.module.target_config().pointer_type();
        let mut signature = self.module.make_signature();
        signature.returns.push(AbiParam::new(types::I8));
        signature.params.push(AbiParam::new(pointer_type)); // y
        signature.params.push(AbiParam::new(pointer_type)); // p
        signature.params.push(AbiParam::new(types::F64)); // t
        if kind.has_seed() {
            signature.params.push(AbiParam::new(pointer_type)); // v
        }
        signature.params.push(AbiParam::new(pointer_type)); // out
        let func_id = self
            .module
            .declare_function(name, Linkage::Local, &signature)
            .map_err(to_backend_err)?;
        let mut roles = HashMap::new();
        for load in plan.loads() {
            roles.insert(load.op_position(), PointOp::Load(load));
        }
        for constant in plan.consts() {
            roles.insert(constant.op_position(), PointOp::Const(constant));
        }
        let mut context = self.module.make_context();
        context.func.signature = signature;
        let mut fb_ctx = FunctionBuilderContext::new();
        {
            let mut fb = FunctionBuilder::new(&mut context.func, &mut fb_ctx);
            let entry = fb.create_block();
            fb.append_block_params_for_function_params(entry);
            fb.switch_to_block(entry);
            let params = fb.block_params(entry).to_vec();
            let inputs = KernelInputs {
                y_ptr: params[0],
                p_ptr: params[1],
                t_value: params[2],
                v_ptr: kind.has_seed().then(|| params[3]),
                out_ptr: params[if kind.has_seed() { 4 } else { 3 }],
            };
            if plan.point_count() > 0 {
                let ordinals = emit_loop_nest(&mut fb, plan.extents())?;
                let point = KernelPoint {
                    plan,
                    base_ops,
                    roles: &roles,
                    ordinals: &ordinals.values,
                    inputs,
                };
                self.lower_kernel_point(&mut fb, &point)?;
                ordinals.close(&mut fb);
            }
            status::succeed(&mut fb);
            fb.seal_all_blocks();
            fb.finalize();
        }
        self.module
            .define_function(func_id, &mut context)
            .map_err(to_backend_err)?;
        self.module.clear_context(&mut context);
        Ok(func_id)
    }

    fn lower_kernel_point(
        &mut self,
        fb: &mut FunctionBuilder<'_>,
        point: &KernelPoint<'_>,
    ) -> Result<(), CompileError> {
        let mut regs = HashMap::new();
        let mut loaded_y = HashMap::new();
        let mut loaded_p = HashMap::new();
        let mut row = RowLowerCtx {
            fb,
            module: &mut self.module,
            math: &mut self.math,
            regs: &mut regs,
            y_ptr: point.inputs.y_ptr,
            p_ptr: point.inputs.p_ptr,
            t_value: point.inputs.t_value,
            v_ptr: point.inputs.v_ptr,
            backing_regs_ptr: None,
            flags: MemFlags::new(),
            loaded_y: Some(&mut loaded_y),
            loaded_p: Some(&mut loaded_p),
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
        for (position, op) in point.base_ops.iter().cloned().enumerate() {
            match (point.roles.get(&position), op) {
                (Some(PointOp::Load(load)), op) => row.lower_strided_load(load, op, point)?,
                (Some(PointOp::Const(constant)), LinearOp::Const { dst, .. }) => {
                    let value = strided_const(row.fb, constant, point.ordinals);
                    row.insert(dst, value)?;
                }
                (Some(PointOp::Const(_)), _) => {
                    return Err(CompileError::Backend(
                        "strided tensor constant is not a Const".to_string(),
                    ));
                }
                (None, LinearOp::StoreOutput { src }) => {
                    let value = row.lookup(src)?;
                    let index = affine_index(
                        row.fb,
                        point.plan.output_start(),
                        point.plan.output_strides(),
                        point.ordinals,
                    )?;
                    let address = element_address(row.fb, point.inputs.out_ptr, index);
                    row.fb.ins().store(row.flags, value, address, 0);
                }
                (None, op) => {
                    row.lower_op(op)?;
                }
            }
        }
        Ok(())
    }
}

impl RowLowerCtx<'_, '_> {
    fn lower_strided_load(
        &mut self,
        load: &rumoca_eval_solve::AffineKernelLoad,
        op: LinearOp,
        point: &KernelPoint<'_>,
    ) -> Result<(), CompileError> {
        let (dst, base) = match op {
            LinearOp::LoadY { dst, .. } => (dst, self.y_ptr),
            LinearOp::LoadP { dst, .. } => (dst, self.p_ptr),
            LinearOp::LoadSeed { dst, .. } => (
                dst,
                self.v_ptr.ok_or_else(|| {
                    CompileError::Backend(
                        "LoadSeed in tensor kernel without seed input".to_string(),
                    )
                })?,
            ),
            _ => {
                return Err(CompileError::Backend(
                    "strided tensor load is not a Y, P, or seed load".to_string(),
                ));
            }
        };
        let index = affine_index(self.fb, load.base_index(), load.strides(), point.ordinals)?;
        let value = load_f64_at(self.fb, self.flags, base, index);
        self.insert(dst, value)?;
        Ok(())
    }
}

/// `base`, then `value += (k_d as f64) * s_d` for every binder in order: the
/// IEEE operations [`rumoca_eval_solve::AffineKernelConst::value_at`] performs.
fn strided_const(
    fb: &mut FunctionBuilder<'_>,
    constant: &rumoca_eval_solve::AffineKernelConst,
    ordinals: &[cranelift_codegen::ir::Value],
) -> cranelift_codegen::ir::Value {
    let mut value = fb.ins().f64const(constant.base_value());
    for (stride, ordinal) in constant.strides().iter().zip(ordinals) {
        let ordinal = fb.ins().fcvt_from_sint(types::F64, *ordinal);
        let stride = fb.ins().f64const(*stride);
        let term = fb.ins().fmul(ordinal, stride);
        value = fb.ins().fadd(value, term);
    }
    value
}

#[derive(Clone, Copy)]
struct KernelInputs {
    y_ptr: cranelift_codegen::ir::Value,
    p_ptr: cranelift_codegen::ir::Value,
    t_value: cranelift_codegen::ir::Value,
    v_ptr: Option<cranelift_codegen::ir::Value>,
    out_ptr: cranelift_codegen::ir::Value,
}

struct KernelPoint<'a> {
    plan: &'a AffineKernelPlan,
    base_ops: &'a [LinearOp],
    roles: &'a HashMap<usize, PointOp<'a>>,
    ordinals: &'a [cranelift_codegen::ir::Value],
    inputs: KernelInputs,
}

/// The open loop nest of one kernel: `values` are the binder ordinals of the
/// current point; `close` emits the latches and leaves the builder after the
/// nest.
struct LoopNest {
    values: Vec<cranelift_codegen::ir::Value>,
    headers: Vec<cranelift_codegen::ir::Block>,
    exit: cranelift_codegen::ir::Block,
}

fn emit_loop_nest(
    fb: &mut FunctionBuilder<'_>,
    extents: &[usize],
) -> Result<LoopNest, CompileError> {
    let exit = fb.create_block();
    let mut values = Vec::with_capacity(extents.len());
    let mut headers = Vec::with_capacity(extents.len());
    for (dimension, extent) in extents.iter().copied().enumerate() {
        let extent = i64::try_from(extent)
            .map_err(|_| CompileError::Backend("tensor kernel extent exceeds i64".to_string()))?;
        let header = fb.create_block();
        fb.append_block_param(header, types::I64);
        let body = fb.create_block();
        let zero = fb.ins().iconst(types::I64, 0);
        fb.ins().jump(header, &[zero.into()]);
        fb.switch_to_block(header);
        let ordinal = fb.block_params(header)[0];
        let inside = fb.ins().icmp_imm(IntCC::SignedLessThan, ordinal, extent);
        // Leaving binder `d` advances binder `d - 1`; leaving binder 0 ends.
        let done = match dimension.checked_sub(1) {
            Some(_) => fb.create_block(),
            None => exit,
        };
        fb.ins().brif(inside, body, &[], done, &[]);
        if done != exit {
            fb.switch_to_block(done);
            let outer_header: cranelift_codegen::ir::Block = headers[dimension - 1];
            let outer: cranelift_codegen::ir::Value = values[dimension - 1];
            let next = fb.ins().iadd_imm(outer, 1);
            fb.ins().jump(outer_header, &[next.into()]);
        }
        fb.switch_to_block(body);
        values.push(ordinal);
        headers.push(header);
    }
    Ok(LoopNest {
        values,
        headers,
        exit,
    })
}

impl LoopNest {
    fn close(self, fb: &mut FunctionBuilder<'_>) {
        match (self.headers.last(), self.values.last()) {
            (Some(&header), Some(&ordinal)) => {
                let next = fb.ins().iadd_imm(ordinal, 1);
                fb.ins().jump(header, &[next.into()]);
            }
            _ => {
                fb.ins().jump(self.exit, &[]);
            }
        }
        fb.switch_to_block(self.exit);
    }
}

/// `base + sum_d strides[d] * ordinals[d]` in i64. The plan proved the value
/// lies in `[0, max]` at every point, so no intermediate wraps.
fn affine_index(
    fb: &mut FunctionBuilder<'_>,
    base: usize,
    strides: &[i64],
    ordinals: &[cranelift_codegen::ir::Value],
) -> Result<cranelift_codegen::ir::Value, CompileError> {
    let base = i64::try_from(base)
        .map_err(|_| CompileError::Backend("tensor kernel base index exceeds i64".to_string()))?;
    let mut index = fb.ins().iconst(types::I64, base);
    for (stride, ordinal) in strides.iter().copied().zip(ordinals) {
        if stride != 0 {
            let term = fb.ins().imul_imm(*ordinal, stride);
            index = fb.ins().iadd(index, term);
        }
    }
    Ok(index)
}

fn element_address(
    fb: &mut FunctionBuilder<'_>,
    base: cranelift_codegen::ir::Value,
    index: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    let offset = fb.ins().ishl_imm(index, 3);
    fb.ins().iadd(base, offset)
}

fn load_f64_at(
    fb: &mut FunctionBuilder<'_>,
    flags: MemFlags,
    base: cranelift_codegen::ir::Value,
    index: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    let address = element_address(fb, base, index);
    fb.ins().load(types::F64, flags, address, 0)
}

/// The ABI of a set of loop kernels: whether they compute a block's values or
/// its directional derivative along a seed vector. The kind is a type, so a
/// kernel is always called with exactly the inputs its signature declares.
pub(crate) trait KernelKind {
    /// The native function of one kernel.
    type Fn: Copy;
    /// The seed input: none for values, the seed vector for a derivative.
    type Seed<'a>: Copy;
    /// The row ABI the kernels are declared with.
    const ROW_KIND: RowKind;

    fn finalized(module: &JITModule, func_id: FuncId) -> Result<Self::Fn, CompileError>;

    fn seed_slice(seed: Self::Seed<'_>) -> Option<&[f64]>;

    /// Run `kernel` over its whole domain.
    ///
    /// # Safety
    ///
    /// `y`, `p`, `seed`, and `out` satisfy the input requirements and output
    /// length of the kernel set `kernel` belongs to.
    unsafe fn invoke(
        kernel: Self::Fn,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: Self::Seed<'_>,
        out: &mut [f64],
    ) -> u8;
}

/// Loop kernels that compute a block's values.
pub(crate) enum ResidualKernels {}

/// Loop kernels that compute a block's directional derivative.
pub(crate) enum DirectionalKernels {}

impl KernelKind for ResidualKernels {
    type Fn = ResidualRowFn;
    type Seed<'a> = ();
    const ROW_KIND: RowKind = RowKind::Residual;

    fn finalized(module: &JITModule, func_id: FuncId) -> Result<Self::Fn, CompileError> {
        finalized_residual_fn(module, func_id)
    }

    fn seed_slice(_seed: Self::Seed<'_>) -> Option<&[f64]> {
        None
    }

    unsafe fn invoke(
        kernel: Self::Fn,
        y: &[f64],
        p: &[f64],
        t: f64,
        _seed: Self::Seed<'_>,
        out: &mut [f64],
    ) -> u8 {
        // SAFETY: the kernel was declared with the residual row ABI; the
        // caller guarantees the buffers cover every index its plan proved.
        unsafe { kernel(y.as_ptr(), p.as_ptr(), t, out.as_mut_ptr()) }
    }
}

impl KernelKind for DirectionalKernels {
    type Fn = JacobianRowFn;
    type Seed<'a> = &'a [f64];
    const ROW_KIND: RowKind = RowKind::JacobianV;

    fn finalized(module: &JITModule, func_id: FuncId) -> Result<Self::Fn, CompileError> {
        finalized_jacobian_fn(module, func_id)
    }

    fn seed_slice(seed: Self::Seed<'_>) -> Option<&[f64]> {
        Some(seed)
    }

    unsafe fn invoke(
        kernel: Self::Fn,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: Self::Seed<'_>,
        out: &mut [f64],
    ) -> u8 {
        // SAFETY: the kernel was declared with the directional row ABI; the
        // caller guarantees the buffers cover every index its plan proved.
        unsafe { kernel(y.as_ptr(), p.as_ptr(), t, seed.as_ptr(), out.as_mut_ptr()) }
    }
}

/// A base program the loop kernel of `K` owns: straight-line scalar
/// arithmetic with exactly one output ([`tensor_kernel_supported`]), proved
/// once when the program is admitted.
pub(crate) struct KernelProgram<'a, K> {
    ops: &'a [LinearOp],
    kind: PhantomData<K>,
}

impl<'a, K: KernelKind> KernelProgram<'a, K> {
    /// `ops` as a kernel program, or `None` when it keeps the per-row path.
    pub(crate) fn admit(ops: &'a [LinearOp]) -> Option<Self> {
        tensor_kernel_supported(ops, K::ROW_KIND).then_some(Self {
            ops,
            kind: PhantomData,
        })
    }
}

/// One kernel of a [`CompiledTensorKernels`] set, issued by its compilation.
#[derive(Clone, Copy, Debug)]
pub(crate) struct KernelId(usize);

/// Native loop kernels of the affine tensor nodes of one block, sharing one
/// executable-memory arena.
pub(crate) struct CompiledTensorKernels<K: KernelKind> {
    kernels: Vec<K::Fn>,
    input_requirements: InputRequirements,
    output_len: usize,
    _module: OwnedJitModule,
}

impl<K: KernelKind> CompiledTensorKernels<K> {
    /// Compile one loop kernel per `(plan, program)` of `nodes`, issuing the
    /// id of each node's kernel at its position (`None` where a node has no
    /// kernel).
    pub(crate) fn compile(
        nodes: &[Option<(AffineKernelPlan, KernelProgram<'_, K>)>],
    ) -> Result<(Self, Vec<Option<KernelId>>), CompileError> {
        let mut emitter = CraneliftEmitter::new(None)?;
        if emitter.module.target_config().pointer_type() != types::I64 {
            return Err(CompileError::Backend(
                "tensor kernels address memory with 64-bit host pointers".to_string(),
            ));
        }
        let mut func_ids = checked_vec_with_capacity(nodes.len(), "tensor kernels")?;
        let mut input_requirements = InputRequirements::default();
        let mut output_len = 0usize;
        let mut ids = Vec::with_capacity(nodes.len());
        for node in nodes {
            let Some((plan, program)) = node else {
                ids.push(None);
                continue;
            };
            ids.push(Some(KernelId(func_ids.len())));
            let profile_id = NEXT_RESIDUAL_KERNEL_ID.fetch_add(1, Ordering::Relaxed);
            func_ids.push(emitter.compile_tensor_kernel(
                plan,
                program,
                &format!("rumoca_tensor_kernel_{profile_id}"),
            )?);
            input_requirements =
                input_requirements.merge(tensor_kernel_input_requirements(plan, program.ops)?);
            output_len = output_len.max(plan.output_count());
        }
        finalize_jit_module(&mut emitter.module)?;
        let kernels = func_ids
            .into_iter()
            .map(|func_id| K::finalized(&emitter.module, func_id))
            .collect::<Result<Vec<_>, _>>()?;
        Ok((
            Self {
                kernels,
                input_requirements,
                output_len,
                _module: emitter.module,
            },
            ids,
        ))
    }

    /// Check the call's buffers once against every kernel of the set.
    pub(crate) fn frame<'a>(
        &'a self,
        (y, p, t): (&'a [f64], &'a [f64], f64),
        seed: K::Seed<'a>,
        out: &'a mut [f64],
    ) -> Result<KernelFrame<'a, K>, CompileError> {
        validate_output_len(out, self.output_len)?;
        validate_input_requirements(self.input_requirements, y, p, K::seed_slice(seed))?;
        Ok(KernelFrame {
            kernels: self,
            y,
            p,
            t,
            seed,
            out,
        })
    }
}

/// The buffers of one block call, checked against a kernel set's proven
/// requirements when the frame is built, so its kernels run without
/// re-checking.
pub(crate) struct KernelFrame<'a, K: KernelKind> {
    kernels: &'a CompiledTensorKernels<K>,
    pub(crate) y: &'a [f64],
    pub(crate) p: &'a [f64],
    pub(crate) t: f64,
    pub(crate) seed: K::Seed<'a>,
    pub(crate) out: &'a mut [f64],
}

impl<K: KernelKind> KernelFrame<'_, K> {
    /// Run `kernel` over its whole domain, writing its outputs into the
    /// frame's output at the plan's output indices.
    pub(crate) fn run(&mut self, kernel: KernelId) -> Result<(), CompileError> {
        let function = self.kernels.kernels[kernel.0];
        // SAFETY: the frame checked its buffers against the set's input
        // requirements and output length, which the kernel's plan proved
        // cover every index it reads and writes.
        let status = unsafe { K::invoke(function, self.y, self.p, self.t, self.seed, self.out) };
        status::check(status)
    }
}
