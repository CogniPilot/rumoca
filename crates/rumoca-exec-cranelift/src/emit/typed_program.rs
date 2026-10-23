//! Native final-boundary lowering for the checked typed pure-call table.
//!
//! One compiler-issued owner becomes one machine-code helper. Aggregate
//! registers remain compact typed ranges in IR and are materialized as one
//! contiguous 64-bit-cell range only inside this execution adapter.

mod assertions;
mod assigned_state;
#[cfg(test)]
mod assigned_state_tests;
mod control;
mod elementwise;
mod flat_map;
#[cfg(test)]
mod fold_comparison_tests;
mod fusion;
mod inline_call;
mod input_results;
#[cfg(test)]
mod input_reuse_tests;
mod integer;
#[cfg(test)]
mod layout_tests;
mod linear_solve;
mod native;
mod observation;
mod operators;
mod placement;
pub(super) use native::rumoca_host_native;
#[cfg(test)]
mod local_store_tests;
#[cfg(test)]
mod pointwise_abs_tests;
#[cfg(test)]
mod pointwise_power_tests;
#[cfg(test)]
mod reduction_maximum_tests;
mod storage;
mod support;
mod tensor;
#[cfg(test)]
mod tensor_product_tests;

use super::host_runtime::{host_jit_builder, register_math_symbols};
use super::owned_jit_module::{OwnedJitModule, declare_far_call_in_func};
use super::{
    CompileError, MathImports, emit_binary_op, emit_unary_op, finalize_jit_module, to_backend_err,
};
use super::{scratch, status};
use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{AbiParam, InstBuilder, MemFlags, Type, Value, types};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};
use input_results::InputResults;
use observation::{
    ObservationCells, ObservationLowering, declare_observation, observation_cells,
    observation_owner_symbol, publishes_predicates,
};
use operators::{map_binary, map_float_compare, map_int_compare, map_unary};
use rumoca_ir_solve as solve;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use storage::compact_register_tape;
pub use support::{NativeFormDecline, NativeFormSupport, NativeOwnerSupport};

const CELL_BYTES: usize = std::mem::size_of::<u64>();

static NEXT_TABLE_ID: AtomicUsize = AtomicUsize::new(0);

/// One typed pure-call body and the native function it is compiled into.
///
/// `input_count` and `outputs` are the body's declared arities, which fix
/// the tape layout. The tangent-carrying form has
/// its own body, its own native function, and its own input coordinate,
/// so it is compiled as a separate unit from the primal form of the same
/// owner.
#[derive(Clone, Copy)]
struct TypedProgramCompilation<'a> {
    function: FuncId,
    program: &'a solve::TypedProgram,
    input_count: usize,
    outputs: &'a [solve::SolvePureCallOutput],
    coordinate: solve::SolvePureCallInputCoordinate,
    /// The SOLVE-C62 group whose depth-carrying member form this unit is.
    recursion: Option<solve::SolveRecursiveGroup>,
    /// Whether this unit is the assertion-observation form of its owner.
    observation: bool,
    /// The table a call may inline its callee from (primal forms only).
    table: Option<&'a solve::SolvePureCallTable>,
}

/// Member-to-member calls inside one SOLVE-C62 group body: each passes the
/// caller's active depth plus one to the callee's depth-carrying form.
#[derive(Clone, Copy)]
struct RecursiveCalls<'a> {
    group: solve::SolveRecursiveGroup,
    functions: &'a [Option<FuncId>],
    depth: Value,
}

#[derive(Clone)]
pub(super) struct PureCallImport {
    pub(super) function: FuncId,
    /// The helper an assertion observation runs, when the owner has one: its
    /// observation form, or its primal form for an owner that publishes no
    /// predicate.
    pub(super) observation: Option<FuncId>,
    pub(super) inputs: Box<[solve::SolveValueType]>,
    pub(super) outputs: Box<[solve::SolvePureCallOutput]>,
    pub(super) directional: Option<Box<PureCallDirectionalImport>>,
}

#[derive(Clone)]
pub(super) struct PureCallDirectionalImport {
    pub(super) function: FuncId,
    pub(super) inputs: Box<[solve::SolveValueType]>,
    pub(super) outputs: Box<[solve::SolvePureCallOutput]>,
}

struct PureCallSymbol {
    owner: solve::SolvePureCallOwnerId,
    name: String,
    address: *const u8,
    inputs: Box<[solve::SolveValueType]>,
    outputs: Box<[solve::SolvePureCallOutput]>,
}

/// Owns the code memory for one completely compiled checked call table.
pub(crate) struct CompiledPureCallTable {
    scratch: scratch::ScratchOwner,
    symbols: Box<[Option<PureCallSymbol>]>,
    directional_symbols: Box<[Option<PureCallSymbol>]>,
    observation_symbols: Box<[Option<PureCallSymbol>]>,
    support: Box<[NativeOwnerSupport]>,
    _source: solve::SolvePureCallTable,
    _input_results: Vec<InputResults>,
    _module: OwnedJitModule,
}

#[cfg(test)]
thread_local! {
    static IMPORT_DECLARATIONS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// Number of pure-call import declarations made on this thread since the
/// last call.
#[cfg(test)]
pub(super) fn take_import_declarations() -> usize {
    IMPORT_DECLARATIONS.with(|count| count.replace(0))
}

impl CompiledPureCallTable {
    pub(crate) fn compile(table: &solve::SolvePureCallTable) -> Result<Self, CompileError> {
        let table_id = NEXT_TABLE_ID.fetch_add(1, Ordering::Relaxed);
        let support = support::decide(table_id, table)?;
        let mut compiler = TableCompiler::new(table_id, table, Emission::Define, &support)?;
        compiler.compile_all(table, &support)?;
        compiler.finish(table, support)
    }

    pub(crate) fn source_matches(&self, table: &solve::SolvePureCallTable) -> bool {
        self._source.shares_table_owner(table)
    }

    pub(crate) fn owner_support(
        &self,
        owner: solve::SolvePureCallOwnerId,
    ) -> Option<&NativeOwnerSupport> {
        self.support.get(owner.index() as usize)
    }

    /// Whether the native adapter owns the form `site` invokes, as admitted
    /// when the table was built.
    pub(crate) fn executes(&self, site: rumoca_eval_solve::PureCallInvocation<'_>) -> bool {
        self.owner_support(site.owner())
            .is_some_and(|support| match site {
                rumoca_eval_solve::PureCallInvocation::Primal(_) => support.primal_is_native(),
                rumoca_eval_solve::PureCallInvocation::Directional(_) => {
                    support.directional_is_native()
                }
            })
    }

    pub(super) fn register_symbols(&self, builder: &mut JITBuilder) {
        for symbol in self.symbols.iter().flatten() {
            builder.symbol(&symbol.name, symbol.address);
        }
        for symbol in self.directional_symbols.iter().flatten() {
            builder.symbol(&symbol.name, symbol.address);
        }
        for symbol in self.observation_symbols.iter().flatten() {
            builder.symbol(&symbol.name, symbol.address);
        }
    }

    pub(super) fn declare_imports(
        &self,
        module: &mut JITModule,
    ) -> Result<HashMap<solve::SolvePureCallOwnerId, PureCallImport>, CompileError> {
        #[cfg(test)]
        IMPORT_DECLARATIONS.with(|count| count.set(count.get() + 1));
        let pointer_type = module.target_config().pointer_type();
        let mut signature = module.make_signature();
        signature.returns.push(AbiParam::new(types::I8));
        signature.params.push(AbiParam::new(pointer_type));
        signature.params.push(AbiParam::new(pointer_type));
        signature.params.push(AbiParam::new(pointer_type)); // caller-owned scratch
        let mut imports = HashMap::with_capacity(self.symbols.len());
        for symbol in self.symbols.iter().flatten() {
            let function = module
                .declare_function(&symbol.name, Linkage::Import, &signature)
                .map_err(to_backend_err)?;
            let directional = declare_directional_import(
                module,
                &signature,
                self.directional_symbols
                    .get(symbol.owner.index() as usize)
                    .and_then(Option::as_ref),
            )?;
            imports.insert(
                symbol.owner,
                PureCallImport {
                    function,
                    observation: self.declare_observation_import(module, &signature, symbol)?,
                    inputs: symbol.inputs.clone(),
                    outputs: symbol.outputs.clone(),
                    directional,
                },
            );
        }
        Ok(imports)
    }

    pub(crate) fn call_cells(
        &self,
        site: rumoca_eval_solve::PureCallInvocation<'_>,
        input: &[u64],
        output: &mut [u64],
    ) -> Result<(), CompileError> {
        let symbol = match site {
            rumoca_eval_solve::PureCallInvocation::Primal(_) => self
                .symbols
                .get(site.owner().index() as usize)
                .and_then(Option::as_ref),
            rumoca_eval_solve::PureCallInvocation::Directional(_) => self
                .directional_symbols
                .get(site.owner().index() as usize)
                .and_then(Option::as_ref),
        }
        .filter(|symbol| {
            symbol.owner == site.owner()
                && symbol.inputs.as_ref() == site.inputs()
                && symbol.outputs.as_ref() == site.outputs()
        })
        .ok_or_else(|| {
            CompileError::Input(format!(
                "typed call site {} does not match its compiled owner",
                site.owner().index()
            ))
        })?;
        let input_count = scalar_type_count(&symbol.inputs, "typed call input")?;
        let output_count = symbol.outputs.iter().try_fold(0usize, |count, value| {
            count
                .checked_add(value.value_type().scalar_count() as usize)
                .ok_or_else(|| CompileError::Input("typed call output count overflows".into()))
        })?;
        if input.len() != input_count || output.len() != output_count {
            return Err(CompileError::Input(format!(
                "typed call payload has {}/{} cells; expected {input_count}/{output_count}",
                input.len(),
                output.len()
            )));
        }
        type TypedCall =
            unsafe extern "C" fn(*const u64, *mut u64, *mut scratch::NativeScratch) -> u8;
        // SAFETY: `symbol.address` is the finalized address of a Cranelift
        // function constructed with exactly the input/output/scratch-pointer/status `TypedCall` ABI.
        // The checked site/interface and cell extents are proved above, and
        // the owning JIT module remains alive in `self` for this call.
        let call: TypedCall = unsafe { std::mem::transmute(symbol.address) };
        // SAFETY: both slices have the exact checked cell extents required by
        // the compiled owner and remain valid for the duration of the call.
        let status = self
            .scratch
            .invoke(|arena| unsafe { call(input.as_ptr(), output.as_mut_ptr(), arena) });
        status::check(status)
    }
}

fn declare_directional_import(
    module: &mut JITModule,
    signature: &cranelift_codegen::ir::Signature,
    symbol: Option<&PureCallSymbol>,
) -> Result<Option<Box<PureCallDirectionalImport>>, CompileError> {
    let Some(symbol) = symbol else {
        return Ok(None);
    };
    let function = module
        .declare_function(&symbol.name, Linkage::Import, signature)
        .map_err(to_backend_err)?;
    Ok(Some(Box::new(PureCallDirectionalImport {
        function,
        inputs: symbol.inputs.clone(),
        outputs: symbol.outputs.clone(),
    })))
}

fn scalar_type_count(
    types: &[solve::SolveValueType],
    context: &str,
) -> Result<usize, CompileError> {
    types.iter().try_fold(0usize, |count, value_type| {
        count
            .checked_add(value_type.scalar_count() as usize)
            .ok_or_else(|| CompileError::Input(format!("{context} count overflows")))
    })
}

/// Whether lowered bodies are defined in the module or only lowered to find
/// which forms the adapter owns.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Emission {
    Define,
    Probe,
}

struct TableCompiler {
    table_id: usize,
    emission: Emission,
    module: OwnedJitModule,
    math: MathImports,
    functions: Vec<Option<FuncId>>,
    directional_functions: Vec<Option<FuncId>>,
    /// Assertion-observation form of each owner that publishes predicates.
    observation_functions: Vec<Option<FuncId>>,
    /// Depth-carrying form of each SOLVE-C62 group member.
    recursive_functions: Vec<Option<FuncId>>,
    input_results: Vec<InputResults>,
}

impl TableCompiler {
    fn new(
        table_id: usize,
        table: &solve::SolvePureCallTable,
        emission: Emission,
        support: &[NativeOwnerSupport],
    ) -> Result<Self, CompileError> {
        let mut builder = host_jit_builder()?;
        register_math_symbols(&mut builder);
        let mut module = OwnedJitModule::new(JITModule::new(builder));
        let pointer_type = module.target_config().pointer_type();
        let mut signature = module.make_signature();
        signature.returns.push(AbiParam::new(types::I8));
        signature.params.push(AbiParam::new(pointer_type));
        signature.params.push(AbiParam::new(pointer_type));
        signature.params.push(AbiParam::new(pointer_type)); // caller-owned scratch
        let mut recursive_signature = signature.clone();
        recursive_signature
            .params
            .insert(2, AbiParam::new(types::I64));
        let mut functions = Vec::with_capacity(table.owners().len());
        let mut directional_functions = Vec::with_capacity(table.owners().len());
        let mut recursive_functions = Vec::with_capacity(table.owners().len());
        let mut observation_functions = Vec::with_capacity(table.owners().len());
        for (owner, admitted) in table.owners().iter().zip(support) {
            if !admitted.primal_is_native() {
                functions.push(None);
                observation_functions.push(None);
                recursive_functions.push(None);
                directional_functions.push(None);
                continue;
            }
            recursive_functions.push(match table.recursive_group(owner.id()) {
                Some(_) => Some(
                    module
                        .declare_function(
                            &recursive_owner_symbol(table_id, owner.id()),
                            Linkage::Local,
                            &recursive_signature,
                        )
                        .map_err(to_backend_err)?,
                ),
                None => None,
            });
            observation_functions.push(declare_observation(
                &mut module,
                (table_id, owner),
                admitted,
                &signature,
            )?);
            let name = owner_symbol(table_id, owner.id());
            functions.push(Some(
                module
                    .declare_function(&name, Linkage::Export, &signature)
                    .map_err(to_backend_err)?,
            ));
            let directional_function = if admitted.directional_is_native() {
                Some(
                    module
                        .declare_function(
                            &directional_owner_symbol(table_id, owner.id()),
                            Linkage::Export,
                            &signature,
                        )
                        .map_err(to_backend_err)?,
                )
            } else {
                None
            };
            directional_functions.push(directional_function);
        }
        Ok(Self {
            table_id,
            emission,
            module,
            math: MathImports::default(),
            functions,
            directional_functions,
            observation_functions,
            recursive_functions,
            input_results: Vec::new(),
        })
    }

    fn compile_all(
        &mut self,
        table: &solve::SolvePureCallTable,
        support: &[NativeOwnerSupport],
    ) -> Result<(), CompileError> {
        for (owner, admitted) in table.owners().iter().zip(support) {
            if admitted.primal_is_native() {
                self.lower_primal(table, owner)?;
            }
            if admitted.primal_is_native()
                && publishes_predicates(owner)
                && admitted.observation_is_native()
            {
                self.lower_observation(owner)?;
            }
            if admitted.directional_is_native() {
                self.lower_directional(table, owner)?;
            }
        }
        finalize_jit_module(&mut self.module)
    }

    fn lower_primal(
        &mut self,
        table: &solve::SolvePureCallTable,
        owner: &solve::SolvePureCallOwner,
    ) -> Result<(), CompileError> {
        match table.recursive_group(owner.id()) {
            Some(group) => self.compile_recursive_owner(owner, *group),
            None => self.compile_owner(table, owner),
        }
    }

    fn lower_directional(
        &mut self,
        table: &solve::SolvePureCallTable,
        owner: &solve::SolvePureCallOwner,
    ) -> Result<(), CompileError> {
        if table.recursive_group(owner.id()).is_some() {
            return Err(CompileError::Backend(
                "a recursive group has no native directional form".into(),
            ));
        }
        self.compile_directional_owner(owner)
    }

    /// The owner's assertion-observation form: its primal body, stopping at the
    /// first failed Error-level assertion, calling the observation form of every
    /// callee that publishes predicates.
    fn lower_observation(&mut self, owner: &solve::SolvePureCallOwner) -> Result<(), CompileError> {
        let function = self
            .observation_functions
            .get(owner.id().index() as usize)
            .copied()
            .flatten()
            .ok_or_else(|| CompileError::Backend("typed observation function is missing".into()))?;
        let functions: Vec<_> = self
            .observation_functions
            .iter()
            .zip(&self.functions)
            .map(|(observation, primal)| observation.or(*primal))
            .collect();
        self.compile_program(
            TypedProgramCompilation {
                function,
                program: owner.body(),
                input_count: owner.inputs().len(),
                outputs: owner.outputs(),
                coordinate: owner.input_coordinate(),
                recursion: None,
                observation: true,
                table: None,
            },
            &functions,
        )
    }

    fn compile_owner(
        &mut self,
        table: &solve::SolvePureCallTable,
        owner: &solve::SolvePureCallOwner,
    ) -> Result<(), CompileError> {
        let function = self
            .functions
            .get(owner.id().index() as usize)
            .copied()
            .flatten()
            .ok_or_else(|| CompileError::Backend("typed owner function is missing".into()))?;
        let functions = self.functions.clone();
        self.compile_program(
            TypedProgramCompilation {
                function,
                program: owner.body(),
                input_count: owner.inputs().len(),
                outputs: owner.outputs(),
                coordinate: owner.input_coordinate(),
                recursion: None,
                observation: false,
                table: Some(table),
            },
            &functions,
        )
    }

    /// A group member compiles to its depth-carrying form and to the public
    /// owner function, which enters the group at depth 1.
    fn compile_recursive_owner(
        &mut self,
        owner: &solve::SolvePureCallOwner,
        group: solve::SolveRecursiveGroup,
    ) -> Result<(), CompileError> {
        let recursive = self
            .recursive_functions
            .get(owner.id().index() as usize)
            .copied()
            .flatten()
            .ok_or_else(|| CompileError::Backend("typed recursive owner is missing".into()))?;
        let functions = self.functions.clone();
        self.compile_program(
            TypedProgramCompilation {
                function: recursive,
                program: owner.body(),
                input_count: owner.inputs().len(),
                outputs: owner.outputs(),
                coordinate: owner.input_coordinate(),
                recursion: Some(group),
                observation: false,
                table: None,
            },
            &functions,
        )?;
        if self.emission == Emission::Probe {
            return Ok(());
        }
        let public = self
            .functions
            .get(owner.id().index() as usize)
            .copied()
            .flatten()
            .ok_or_else(|| CompileError::Backend("typed owner function is missing".into()))?;
        self.compile_group_entry(public, recursive)
    }

    fn compile_group_entry(
        &mut self,
        public: FuncId,
        recursive: FuncId,
    ) -> Result<(), CompileError> {
        let pointer_type = self.module.target_config().pointer_type();
        let mut context = self.module.make_context();
        context
            .func
            .signature
            .returns
            .push(AbiParam::new(types::I8));
        context
            .func
            .signature
            .params
            .push(AbiParam::new(pointer_type));
        context
            .func
            .signature
            .params
            .push(AbiParam::new(pointer_type));
        context
            .func
            .signature
            .params
            .push(AbiParam::new(pointer_type)); // caller-owned scratch
        let mut builder_context = FunctionBuilderContext::new();
        {
            let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
            let entry = builder.create_block();
            builder.append_block_params_for_function_params(entry);
            builder.switch_to_block(entry);
            builder.seal_block(entry);
            let parameters = builder.block_params(entry).to_vec();
            scratch::enter(&mut builder);
            let depth = builder.ins().iconst(types::I64, 1);
            let local = declare_far_call_in_func(&mut self.module, recursive, builder.func);
            let call = builder
                .ins()
                .call(local, &[parameters[0], parameters[1], depth, parameters[2]]);
            let status = builder.inst_results(call)[0];
            scratch::leave(&mut builder);
            builder.ins().return_(&[status]);
            builder.finalize();
        }
        crate::emit::register_allocation::define_function(&mut *self.module, public, &mut context)
            .map_err(to_backend_err)?;
        self.module.clear_context(&mut context);
        Ok(())
    }

    fn compile_directional_owner(
        &mut self,
        owner: &solve::SolvePureCallOwner,
    ) -> Result<(), CompileError> {
        let directional = owner
            .directional()
            .ok_or_else(|| CompileError::Backend("typed directional owner is missing".into()))?;
        let function = self
            .directional_functions
            .get(owner.id().index() as usize)
            .copied()
            .flatten()
            .ok_or_else(|| CompileError::Backend("typed directional function is missing".into()))?;
        let functions = self.directional_functions.clone();
        self.compile_program(
            TypedProgramCompilation {
                function,
                program: directional.body(),
                input_count: directional.inputs().len(),
                outputs: directional.outputs(),
                coordinate: directional.input_coordinate(),
                recursion: None,
                observation: false,
                table: None,
            },
            &functions,
        )
    }

    fn compile_program(
        &mut self,
        unit: TypedProgramCompilation<'_>,
        functions: &[Option<FuncId>],
    ) -> Result<(), CompileError> {
        let TypedProgramCompilation {
            function,
            program,
            input_count,
            outputs,
            coordinate,
            recursion,
            observation,
            table,
        } = unit;
        let config = self.module.target_config();
        let pointer_type = config.pointer_type();
        let input_results = InputResults::new(coordinate)?;
        let mut context = self.module.make_context();
        program_signature(
            &mut context.func.signature,
            pointer_type,
            recursion.is_some(),
        );
        let mut builder_context = FunctionBuilderContext::new();
        {
            let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
            let entry = builder.create_block();
            builder.append_block_params_for_function_params(entry);
            builder.switch_to_block(entry);
            builder.seal_block(entry);
            let parameters = builder.block_params(entry).to_vec();
            scratch::enter(&mut builder);
            let recursive_calls = recursion.map(|group| {
                enter_group(
                    &mut builder,
                    group,
                    &self.recursive_functions,
                    parameters[2],
                )
            });
            let cache = if observation {
                None
            } else {
                Some(input_results.enter(&mut builder, config, parameters[0], parameters[1])?)
            };
            let ObservationCells {
                cells,
                slots: predicate_slots,
            } = observation_cells(observation, input_count, outputs)?;
            let layout =
                ProgramLayout::new(program, (input_count, outputs.len()), &predicate_slots)?;
            let tape = create_tape(&mut builder, pointer_type, layout.tape_cells)?;
            let mut lowerer = ProgramLowerer {
                builder: &mut builder,
                module: &mut self.module,
                math: &mut self.math,
                pointer_type,
                input: parameters[0],
                output: parameters[1],
                tape,
                layout: &layout,
                outputs,
                functions,
                recursive_calls,
                flags: MemFlags::new(),
                operation: std::ptr::null(),
                observation: observation.then(|| ObservationLowering {
                    owner_output: parameters[1],
                    input_slots: input_count,
                    cells,
                    root: true,
                }),
                input_placement: None,
                output_placement: None,
                // Admission probes compile each owner without inlining, so a
                // declined callee declines its callers by name; the defined
                // forms of admitted owners then only inline admitted callees.
                table: table.filter(|_| !observation && self.emission == Emission::Define),
                inline_depth: 0,
            };
            lowerer.initialize_observation()?;
            lowerer.lower(program)?;
            if let Some(cache) = cache {
                input_results.publish(&mut builder, config, cache, parameters[0], parameters[1])?;
            }
            status::succeed(&mut builder);
            builder.finalize();
        }
        if self.emission == Emission::Define {
            crate::emit::register_allocation::define_function(
                &mut *self.module,
                function,
                &mut context,
            )
            .map_err(to_backend_err)?;
            self.input_results.push(input_results);
        }
        self.module.clear_context(&mut context);
        Ok(())
    }

    fn finish(
        self,
        table: &solve::SolvePureCallTable,
        support: Vec<NativeOwnerSupport>,
    ) -> Result<CompiledPureCallTable, CompileError> {
        let mut symbols = Vec::with_capacity(table.owners().len());
        let mut directional_symbols = Vec::with_capacity(table.owners().len());
        let mut observation_symbols = Vec::with_capacity(table.owners().len());
        for owner in table.owners() {
            let index = owner.id().index() as usize;
            let symbol = self.functions[index]
                .map(|function| {
                    let address = self.module.get_finalized_function(function);
                    validate_finalized_address(address, "primal", owner.id().index())?;
                    Ok(PureCallSymbol {
                        owner: owner.id(),
                        name: owner_symbol(self.table_id, owner.id()),
                        address,
                        inputs: owner.inputs().into(),
                        outputs: owner.outputs().into(),
                    })
                })
                .transpose()?;
            symbols.push(symbol);
            let directional_symbol = match (owner.directional(), self.directional_functions[index])
            {
                (Some(directional), Some(function)) => {
                    let address = self.module.get_finalized_function(function);
                    validate_finalized_address(address, "directional", owner.id().index())?;
                    Some(PureCallSymbol {
                        owner: owner.id(),
                        name: directional_owner_symbol(self.table_id, owner.id()),
                        address,
                        inputs: directional.inputs().into(),
                        outputs: directional.outputs().into(),
                    })
                }
                _ => None,
            };
            directional_symbols.push(directional_symbol);
            observation_symbols.push(
                self.observation_functions[index]
                    .map(|function| {
                        let address = self.module.get_finalized_function(function);
                        validate_finalized_address(address, "observation", owner.id().index())?;
                        Ok(PureCallSymbol {
                            owner: owner.id(),
                            name: observation_owner_symbol(self.table_id, owner.id()),
                            address,
                            inputs: owner.inputs().into(),
                            outputs: owner.outputs().into(),
                        })
                    })
                    .transpose()?,
            );
        }
        Ok(CompiledPureCallTable {
            scratch: Default::default(),
            symbols: symbols.into_boxed_slice(),
            directional_symbols: directional_symbols.into_boxed_slice(),
            observation_symbols: observation_symbols.into_boxed_slice(),
            support: support.into_boxed_slice(),
            _source: table.clone(),
            _input_results: self.input_results,
            _module: self.module,
        })
    }
}

fn owner_symbol(table: usize, owner: solve::SolvePureCallOwnerId) -> String {
    format!("rumoca_typed_pure_call_{table}_{}", owner.index())
}

fn recursive_owner_symbol(table: usize, owner: solve::SolvePureCallOwnerId) -> String {
    format!("rumoca_typed_recursive_call_{table}_{}", owner.index())
}

fn directional_owner_symbol(table: usize, owner: solve::SolvePureCallOwnerId) -> String {
    format!(
        "rumoca_typed_pure_call_directional_{table}_{}",
        owner.index()
    )
}

fn validate_finalized_address(
    address: *const u8,
    kind: &str,
    owner: u32,
) -> Result<(), CompileError> {
    if address.is_null() {
        return Err(CompileError::Backend(format!(
            "Cranelift returned a null typed {kind} owner {owner} function"
        )));
    }
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StorageBase {
    Input,
    Output,
    Tape,
}

#[derive(Clone)]
struct ValueLocation {
    base: StorageBase,
    cell: u32,
    value_type: solve::SolveValueType,
}

struct ProgramLayout {
    slots: Box<[ValueLocation]>,
    registers: Box<[ValueLocation]>,
    assigned_states: Box<[assigned_state::AssignedLocation]>,
    assigned_inputs: Box<[assigned_state::AssignedLocation]>,
    assigned_outputs: Box<[assigned_state::AssignedLocation]>,
    tape_cells: u32,
    /// The operation each fused register defers (see [`fusion`]); such a
    /// register is never stored.
    fused: Box<[Option<solve::SolveOperation>]>,
    /// Last reading operation of each register, extended over fusion.
    last_uses: Box<[Option<usize>]>,
}

impl ProgramLayout {
    fn region(region: &solve::SolveProgramRegion) -> Result<Self, CompileError> {
        Self::new(
            region.body(),
            (region.inputs().len(), region.outputs().len()),
            &[],
        )
    }

    /// `unaliased_slots` are output slots whose cells are written outside the
    /// program's stores, so no register may be placed in them.
    fn new(
        program: &solve::TypedProgram,
        (input_slots, output_slots): (usize, usize),
        unaliased_slots: &[usize],
    ) -> Result<Self, CompileError> {
        let mut input_cell = 0u32;
        let mut output_cell = 0u32;
        let mut tape_cell = 0u32;
        let mut slots = Vec::with_capacity(program.slots().len());
        for (index, slot) in program.slots().iter().enumerate() {
            let count = slot.value_type().scalar_count();
            let (base, cell) = if index < input_slots {
                let cell = input_cell;
                input_cell = checked_cells(input_cell, count, "typed input layout")?;
                (StorageBase::Input, cell)
            } else if index < input_slots.saturating_add(output_slots) {
                let cell = output_cell;
                output_cell = checked_cells(output_cell, count, "typed output layout")?;
                (StorageBase::Output, cell)
            } else {
                let cell = tape_cell;
                tape_cell = checked_cells(tape_cell, count, "typed slot layout")?;
                (StorageBase::Tape, cell)
            };
            slots.push(ValueLocation {
                base,
                cell,
                value_type: slot.value_type().clone(),
            });
        }
        let register_base = tape_cell;
        let mut registers = Vec::with_capacity(program.register_types().len());
        for value_type in program.register_types() {
            let cell = tape_cell;
            tape_cell = checked_cells(
                tape_cell,
                value_type.scalar_count(),
                "typed register layout",
            )?;
            registers.push(ValueLocation {
                base: StorageBase::Tape,
                cell,
                value_type: value_type.clone(),
            });
        }
        let (mut last_uses, definitions) = register_flow(program, registers.len())?;
        let fused = fusion::plan(program, &mut last_uses)?;
        alias_read_only_loads(program, &slots, &mut registers)?;
        alias_final_output_stores(
            program,
            (&slots, unaliased_slots),
            (&last_uses, &definitions),
            &mut registers,
        )?;
        alias_consumed_functional_updates(program, &last_uses, &mut registers)?;
        storage::alias_consumed_fold_carries(program, &last_uses, &mut registers);
        tape_cell = compact_register_tape(register_base, &last_uses, &definitions, &mut registers)?;
        let assigned_states = assigned_state::layout(
            program.assigned_state_types(),
            StorageBase::Tape,
            &mut tape_cell,
        )?;
        let assigned_inputs = assigned_state::layout(
            program.assigned_state_inputs(),
            StorageBase::Input,
            &mut input_cell,
        )?;
        let assigned_outputs = assigned_state::layout(
            program.assigned_state_outputs(),
            StorageBase::Output,
            &mut output_cell,
        )?;
        Ok(Self {
            slots: slots.into_boxed_slice(),
            registers: registers.into_boxed_slice(),
            assigned_states,
            assigned_inputs,
            assigned_outputs,
            tape_cells: tape_cell,
            fused: fused.into_boxed_slice(),
            last_uses: last_uses.into_boxed_slice(),
        })
    }
}

type RegisterFlow = (Vec<Option<usize>>, Vec<Option<usize>>);

fn register_flow(
    program: &solve::TypedProgram,
    register_count: usize,
) -> Result<RegisterFlow, CompileError> {
    let mut last_uses = program.register_last_reads().to_vec();
    last_uses.resize(register_count, None);
    let mut definitions = vec![None; register_count];
    for (operation_index, operation) in program.operations().iter().enumerate() {
        let mut invalid_definition = false;
        operation.operation().visit_output_registers(|register| {
            let Some(definition) = definitions.get_mut(register.index()) else {
                invalid_definition = true;
                return;
            };
            if definition.replace(operation_index).is_some() {
                invalid_definition = true;
            }
        });
        if invalid_definition {
            return Err(CompileError::Backend(
                "checked typed register definition is invalid".into(),
            ));
        }
    }
    Ok((last_uses, definitions))
}

fn alias_read_only_loads(
    program: &solve::TypedProgram,
    slots: &[ValueLocation],
    registers: &mut [ValueLocation],
) -> Result<(), CompileError> {
    for operation in program.operations() {
        let solve::SolveOperation::Load { destination, slot } = operation.operation() else {
            continue;
        };
        let slot_definition = program
            .slots()
            .get(slot.index())
            .ok_or_else(|| CompileError::Backend("typed load slot is missing".into()))?;
        if slot_definition.access() != solve::SolveSlotAccess::ReadOnly {
            continue;
        }
        let source = slots
            .get(slot.index())
            .ok_or_else(|| CompileError::Backend("typed read-only load slot is missing".into()))?
            .clone();
        // Only the call/region input buffer is an immutable external view.
        // Constant slots also have ReadOnly access but live on the mutable
        // tape; aliasing one into a consumed functional update could corrupt a
        // later load from that constant slot.
        if source.base != StorageBase::Input {
            continue;
        }
        let target = registers
            .get_mut(destination.index())
            .ok_or_else(|| CompileError::Backend("typed load destination is missing".into()))?;
        if target.value_type != source.value_type {
            return Err(CompileError::Backend(
                "checked typed load type differs".into(),
            ));
        }
        *target = source;
    }
    Ok(())
}

fn alias_final_output_stores(
    program: &solve::TypedProgram,
    (slots, unaliased_slots): (&[ValueLocation], &[usize]),
    (last_uses, definitions): (&[Option<usize>], &[Option<usize>]),
    registers: &mut [ValueLocation],
) -> Result<(), CompileError> {
    for (operation_index, operation) in program.operations().iter().enumerate() {
        let solve::SolveOperation::Store { slot, source } = operation.operation() else {
            continue;
        };
        if unaliased_slots.contains(&slot.index()) {
            continue;
        }
        if last_uses.get(source.index()) != Some(&Some(operation_index)) {
            continue;
        }
        let destination = slots
            .get(slot.index())
            .ok_or_else(|| CompileError::Backend("typed store slot is missing".into()))?
            .clone();
        if destination.base != StorageBase::Output {
            continue;
        }
        let definition = definitions
            .get(source.index())
            .copied()
            .flatten()
            .ok_or_else(|| {
                CompileError::Backend("typed stored register definition is missing".into())
            })?;
        if output_slot_is_observed(program, *slot, definition, operation_index) {
            continue;
        }
        let source_location = registers
            .get_mut(source.index())
            .ok_or_else(|| CompileError::Backend("typed stored register is missing".into()))?;
        if source_location.value_type != destination.value_type {
            return Err(CompileError::Backend(
                "checked typed output store type differs".into(),
            ));
        }
        *source_location = destination;
    }
    Ok(())
}

fn output_slot_is_observed(
    program: &solve::TypedProgram,
    slot: solve::SolveSlotId,
    definition: usize,
    store: usize,
) -> bool {
    program.operations()[definition..store]
        .iter()
        .any(|operation| {
            matches!(
                operation.operation(),
                solve::SolveOperation::Load { slot: candidate, .. }
                    | solve::SolveOperation::Store { slot: candidate, .. }
                    if *candidate == slot
            )
        })
}

fn alias_consumed_functional_updates(
    program: &solve::TypedProgram,
    last_uses: &[Option<usize>],
    registers: &mut [ValueLocation],
) -> Result<(), CompileError> {
    for (operation_index, operation) in program.operations().iter().enumerate() {
        let (destination, aggregate) = match operation.operation() {
            solve::SolveOperation::UpdateElement {
                destination,
                aggregate,
                ..
            }
            | solve::SolveOperation::UpdateSlice {
                destination,
                aggregate,
                ..
            }
            | solve::SolveOperation::UpdateView {
                destination,
                aggregate,
                ..
            } => (*destination, *aggregate),
            _ => continue,
        };
        if last_uses.get(aggregate.index()) != Some(&Some(operation_index)) {
            continue;
        }
        let source = registers
            .get(aggregate.index())
            .ok_or_else(|| CompileError::Backend("typed update source is missing".into()))?
            .clone();
        let target = registers
            .get_mut(destination.index())
            .ok_or_else(|| CompileError::Backend("typed update destination is missing".into()))?;
        if source.base != StorageBase::Tape || target.base != StorageBase::Tape {
            continue;
        }
        if target.value_type != source.value_type {
            return Err(CompileError::Backend(
                "checked functional update type differs".into(),
            ));
        }
        *target = source;
    }
    Ok(())
}

fn reads_fused(fused: &[Option<solve::SolveOperation>], operation: &solve::SolveOperation) -> bool {
    let mut reads = false;
    operation.visit_input_registers(|register| {
        reads |= fused.get(register.index()).is_some_and(Option::is_some);
    });
    reads
}

fn checked_cells(start: u32, count: u32, context: &str) -> Result<u32, CompileError> {
    start
        .checked_add(count)
        .ok_or_else(|| CompileError::Backend(format!("{context} overflows")))
}

fn create_tape(
    builder: &mut FunctionBuilder<'_>,
    pointer_type: Type,
    cells: u32,
) -> Result<Value, CompileError> {
    let bytes = cells
        .max(1)
        .checked_mul(CELL_BYTES as u32)
        .ok_or_else(|| CompileError::Backend("typed native tape size overflows".into()))?;
    let slot = scratch::create_slot(builder, bytes)?;
    Ok(scratch::stack_addr(builder, pointer_type, slot, 0))
}

fn cell_offset(cell: u32, context: &str) -> Result<i32, CompileError> {
    i32::try_from(
        (cell as usize)
            .checked_mul(CELL_BYTES)
            .ok_or_else(|| CompileError::Backend(format!("{context} offset overflows")))?,
    )
    .map_err(|_| CompileError::Backend(format!("{context} offset exceeds i32")))
}

fn cell_pointer(
    builder: &mut FunctionBuilder<'_>,
    base: Value,
    cell: u32,
    context: &str,
) -> Result<Value, CompileError> {
    let bytes = i64::from(cell)
        .checked_mul(CELL_BYTES as i64)
        .ok_or_else(|| CompileError::Backend(format!("{context} pointer overflows")))?;
    Ok(builder.ins().iadd_imm(base, bytes))
}

struct ProgramLowerer<'a, 'b> {
    builder: &'a mut FunctionBuilder<'b>,
    module: &'a mut JITModule,
    math: &'a mut MathImports,
    pointer_type: Type,
    input: Value,
    output: Value,
    tape: Value,
    layout: &'a ProgramLayout,
    /// The owner form's declared outputs, which fix each assertion's level.
    outputs: &'a [solve::SolvePureCallOutput],
    functions: &'a [Option<FuncId>],
    recursive_calls: Option<RecursiveCalls<'a>>,
    flags: MemFlags,
    /// Present when lowering the assertion-observation form of an owner.
    observation: Option<ObservationLowering>,
    operation: *const solve::SolveSpannedOperation,
    /// The table whose owners a call may be lowered inline from.
    table: Option<&'a solve::SolvePureCallTable>,
    /// Calls lowered inline around this program.
    inline_depth: u32,
    /// Input slots placed on enclosing cells instead of `input` (see
    /// [`placement`]).
    input_placement: Option<Vec<placement::PlacedSlot>>,
    /// Output slots placed on enclosing cells instead of `output`.
    output_placement: Option<Vec<placement::PlacedSlot>>,
}

impl ProgramLowerer<'_, '_> {
    fn lower(&mut self, program: &solve::TypedProgram) -> Result<(), CompileError> {
        for operation in program.operations() {
            self.operation = operation as *const _;
            self.lower_operation(operation.operation())?;
        }
        Ok(())
    }

    // SPEC_0021: exhaustive dispatch over the closed typed operation vocabulary.
    // SPEC_0021: Exception - cohesive exhaustive flow stays contiguous so ordering remains auditable.
    #[allow(clippy::too_many_lines)]
    fn lower_operation(&mut self, operation: &solve::SolveOperation) -> Result<(), CompileError> {
        let mut destination = None;
        operation.visit_output_registers(|register| destination = Some(register));
        if let Some(destination) = destination
            && self
                .layout
                .fused
                .get(destination.index())
                .is_some_and(Option::is_some)
        {
            // Evaluated per element by the operation that reads it.
            return Ok(());
        }
        if let Some(destination) = destination
            && !matches!(
                operation,
                solve::SolveOperation::Reduce { .. } | solve::SolveOperation::MatrixMultiply { .. }
            )
            && (reads_fused(&self.layout.fused, operation)
                || (self.register(destination)?.value_type.scalar_count() >= 4
                    && self.vectorizes(operation)?))
        {
            return self.lower_fused(operation, destination);
        }
        match operation {
            solve::SolveOperation::AssignedState { operation } => {
                self.lower_assigned_state(operation)
            }
            solve::SolveOperation::AssignedConditional { program } => {
                self.lower_assigned_conditional(program)
            }
            solve::SolveOperation::CheckAssertion {
                predicate_output,
                condition,
                captures,
                destinations,
                message,
                ..
            } => self.lower_assertion(
                (*predicate_output, *condition),
                (captures, destinations),
                message,
            ),
            solve::SolveOperation::Constant { destination, value } => {
                self.lower_constant(*destination, value)
            }
            solve::SolveOperation::Load { destination, slot } => {
                let source = self.slot(*slot)?.clone();
                let destination = self.register(*destination)?.clone();
                self.copy(&source, &destination)
            }
            solve::SolveOperation::Store { slot, .. }
                if self
                    .observation
                    .as_ref()
                    .is_some_and(|observation| observation.publishes_slot(*slot)) =>
            {
                Ok(())
            }
            solve::SolveOperation::Store { slot, source } => {
                let source = self.register(*source)?.clone();
                let destination = self.slot(*slot)?.clone();
                self.copy(&source, &destination)
            }
            solve::SolveOperation::Unary {
                destination,
                operator,
                operand,
            } => self.lower_unary(*destination, *operator, *operand),
            solve::SolveOperation::Binary {
                destination,
                operator,
                lhs,
                rhs,
            } => self.lower_binary(*destination, *operator, *lhs, *rhs),
            solve::SolveOperation::Compare {
                destination,
                operator,
                lhs,
                rhs,
            } => self.lower_compare(*destination, *operator, *lhs, *rhs),
            solve::SolveOperation::Convert {
                destination,
                operator,
                operand,
            } => self.lower_convert(*destination, *operator, *operand),
            solve::SolveOperation::Select {
                destination,
                condition,
                if_true,
                if_false,
            } => self.lower_select(*destination, *condition, *if_true, *if_false),
            solve::SolveOperation::Conditional {
                condition,
                captures,
                destinations,
                if_true,
                if_false,
            } => self.lower_conditional(*condition, captures, destinations, if_true, if_false),
            solve::SolveOperation::Map {
                domain,
                captures,
                destination,
                body,
            } => self.lower_map(domain, captures, *destination, body),
            solve::SolveOperation::Fold {
                domain,
                initial,
                captures,
                state_captures,
                destinations,
                transition,
                continuation,
            } => self.lower_fold(
                domain,
                initial,
                captures,
                state_captures,
                destinations,
                (transition, continuation.as_deref()),
            ),
            solve::SolveOperation::MatrixMultiply {
                destination,
                lhs,
                rhs,
            } => self.lower_matrix_multiply(*destination, *lhs, *rhs),
            solve::SolveOperation::LinearSolve {
                destination,
                matrix,
                rhs,
            } => self.lower_linear_solve(*destination, *matrix, *rhs),
            solve::SolveOperation::Native {
                body,
                operands,
                destinations,
            } => self.lower_native(*body, operands, destinations),
            solve::SolveOperation::Scale {
                destination,
                aggregate,
                scalar,
            } => self.lower_scale(*destination, *aggregate, *scalar),
            solve::SolveOperation::BroadcastBinary {
                destination,
                operator,
                aggregate,
                scalar,
                scalar_on_lhs,
            } => self.lower_broadcast_binary(
                *destination,
                *operator,
                *aggregate,
                *scalar,
                *scalar_on_lhs,
            ),
            solve::SolveOperation::Transpose {
                destination,
                operand,
            } => self.lower_transpose(*destination, *operand),
            solve::SolveOperation::Cross {
                destination,
                lhs,
                rhs,
            } => self.lower_cross(*destination, *lhs, *rhs),
            solve::SolveOperation::Reduce {
                destination,
                operator,
                operand,
            } => self.lower_reduce(*destination, *operator, *operand),
            solve::SolveOperation::Identity { destination } => self.lower_identity(*destination),
            solve::SolveOperation::Diagonal {
                destination,
                operand,
            } => self.lower_diagonal(*destination, *operand),
            solve::SolveOperation::Concatenate {
                destination,
                axis,
                operands,
            } => self.lower_concatenate(*destination, *axis, operands),
            solve::SolveOperation::Fill { destination, value } => {
                self.lower_fill(*destination, *value)
            }
            solve::SolveOperation::ConstructAggregate {
                destination,
                elements,
            } => self.lower_construct_aggregate(*destination, elements),
            solve::SolveOperation::ProjectElement {
                destination,
                aggregate,
                indices,
            } => self.lower_project_element(*destination, *aggregate, indices),
            solve::SolveOperation::ProjectElementDynamic {
                destination,
                aggregate,
                indices,
            } => self.lower_project_element_dynamic(*destination, *aggregate, indices),
            solve::SolveOperation::ProjectSlice {
                destination,
                aggregate,
                origin,
            } => self.lower_project_slice(*destination, *aggregate, origin),
            solve::SolveOperation::ProjectView {
                destination,
                aggregate,
                axes,
            } => self.lower_project_view(*destination, *aggregate, axes),
            solve::SolveOperation::SelectElement {
                destination,
                aggregate,
                indices,
                out_of_range,
            } => self.lower_select_element(*destination, *aggregate, indices, *out_of_range),
            solve::SolveOperation::UpdateElement {
                destination,
                aggregate,
                value,
                indices,
            } => self.lower_update_element(*destination, *aggregate, *value, indices),
            solve::SolveOperation::UpdateSlice {
                destination,
                aggregate,
                value,
                origin,
            } => self.lower_update_slice(*destination, *aggregate, *value, origin),
            solve::SolveOperation::UpdateView {
                destination,
                aggregate,
                value,
                axes,
            } => self.lower_update_view(*destination, *aggregate, *value, axes),
            solve::SolveOperation::Call {
                owner,
                arguments,
                destinations,
                assertion_forwarding,
            } => self.lower_call(*owner, arguments, destinations, assertion_forwarding),
        }
    }

    fn slot(&self, slot: solve::SolveSlotId) -> Result<&ValueLocation, CompileError> {
        self.layout
            .slots
            .get(slot.index())
            .ok_or_else(|| CompileError::Backend("typed slot layout is missing".into()))
    }

    fn register(&self, register: solve::SolveRegisterId) -> Result<&ValueLocation, CompileError> {
        self.layout
            .registers
            .get(register.index())
            .ok_or_else(|| CompileError::Backend("typed register layout is missing".into()))
    }

    fn base(&self, location: &ValueLocation) -> Value {
        let (placed, base) = match location.base {
            StorageBase::Input => (self.input_placement.as_deref(), self.input),
            StorageBase::Output => (self.output_placement.as_deref(), self.output),
            StorageBase::Tape => return self.tape,
        };
        placed
            .and_then(|slots| placement::placed_slot(slots, location.cell))
            .map_or(base, |slot| slot.adjusted)
    }

    fn lower_constant(
        &mut self,
        destination: solve::SolveRegisterId,
        value: &solve::SolveValue,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let scalar = self.scalar_constant(value);
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.store_scalar(&destination, zero, scalar)
    }

    pub(super) fn scalar_constant(&mut self, value: &solve::SolveValue) -> Value {
        match value.kind() {
            solve::SolveValueKind::Real32(bits) => {
                self.builder.ins().f32const(f32::from_bits(bits))
            }
            solve::SolveValueKind::Real64(bits) => {
                self.builder.ins().f64const(f64::from_bits(bits))
            }
            solve::SolveValueKind::Integer(value) => self.builder.ins().iconst(types::I64, value),
            solve::SolveValueKind::Boolean(value) => {
                self.builder.ins().iconst(types::I64, i64::from(value))
            }
        }
    }

    fn lower_unary(
        &mut self,
        destination: solve::SolveRegisterId,
        operator: solve::SolveUnaryOperator,
        operand: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let operand = self.register(operand)?.clone();
        self.for_each_element(destination.value_type.scalar_count(), |this, index| {
            let value = this.load_scalar(&operand, index)?;
            let result = this.unary_element(operator, operand.value_type.element_type(), value)?;
            this.store_scalar(&destination, index, result)
        })
    }

    fn lower_binary(
        &mut self,
        destination: solve::SolveRegisterId,
        operator: solve::SolveBinaryOperator,
        lhs: solve::SolveRegisterId,
        rhs: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let lhs = self.register(lhs)?.clone();
        let rhs = self.register(rhs)?.clone();
        self.for_each_element(destination.value_type.scalar_count(), |this, index| {
            let lhs_value = this.load_scalar(&lhs, index)?;
            let rhs_value = this.load_scalar(&rhs, index)?;
            let result = this.binary_element(
                operator,
                destination.value_type.element_type(),
                lhs_value,
                rhs_value,
            )?;
            this.store_scalar(&destination, index, result)
        })
    }

    fn lower_compare(
        &mut self,
        destination: solve::SolveRegisterId,
        operator: solve::SolveCompareOperator,
        lhs: solve::SolveRegisterId,
        rhs: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let lhs = self.register(lhs)?.clone();
        let rhs = self.register(rhs)?.clone();
        self.for_each_element(destination.value_type.scalar_count(), |this, index| {
            let lhs_value = this.load_scalar(&lhs, index)?;
            let rhs_value = this.load_scalar(&rhs, index)?;
            let condition = this.compare_element(
                operator,
                lhs.value_type.element_type(),
                lhs_value,
                rhs_value,
            );
            let result = this.builder.ins().uextend(types::I64, condition);
            this.store_scalar(&destination, index, result)
        })
    }

    fn lower_convert(
        &mut self,
        destination: solve::SolveRegisterId,
        operator: solve::SolveConversionOperator,
        operand: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let operand = self.register(operand)?.clone();
        self.for_each_element(destination.value_type.scalar_count(), |this, index| {
            let value = this.load_scalar(&operand, index)?;
            let result = this.convert_element(
                operator,
                operand.value_type.element_type(),
                &destination.value_type,
                value,
            )?;
            this.store_scalar(&destination, index, result)
        })
    }

    fn lower_select(
        &mut self,
        destination: solve::SolveRegisterId,
        condition: solve::SolveRegisterId,
        if_true: solve::SolveRegisterId,
        if_false: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let condition = self.register(condition)?.clone();
        let if_true = self.register(if_true)?.clone();
        let if_false = self.register(if_false)?.clone();
        let zero = self.builder.ins().iconst(types::I64, 0);
        let condition = self.load_scalar(&condition, zero)?;
        let condition = self.builder.ins().icmp_imm(IntCC::NotEqual, condition, 0);
        self.for_each_element(destination.value_type.scalar_count(), |this, index| {
            let when_true = this.load_scalar(&if_true, index)?;
            let when_false = this.load_scalar(&if_false, index)?;
            let selected = this.builder.ins().select(condition, when_true, when_false);
            this.store_scalar(&destination, index, selected)
        })
    }

    fn lower_call(
        &mut self,
        owner: solve::SolvePureCallOwnerId,
        arguments: &[solve::SolveRegisterId],
        destinations: &[solve::SolveRegisterId],
        forwarding: &[solve::SolveAssertionForwarding],
    ) -> Result<(), CompileError> {
        if let Some(table) = self.table
            && self.observation.is_none()
            && forwarding.is_empty()
            && self.inline_depth < 3
            && self.recursive_calls.is_none()
            && let Some(callee) = inline_call::inline_callee(table, owner, arguments, destinations)
        {
            return self.lower_inline_call(callee, arguments, destinations);
        }
        let output_cells = self.register_cell_count(destinations, "nested typed output")?;
        let output = create_tape(self.builder, self.pointer_type, output_cells)?;
        if self.observation.is_some() {
            self.observe_call(owner, arguments, destinations, forwarding, output)?;
        } else {
            self.call_owner_into(owner, arguments, output)?;
        }
        self.unpack_registers(output, destinations)
    }

    fn call_owner_into(
        &mut self,
        owner: solve::SolvePureCallOwnerId,
        arguments: &[solve::SolveRegisterId],
        output: Value,
    ) -> Result<(), CompileError> {
        let status = self.emit_owner_call(owner, arguments, output)?;
        status::propagate(self.builder, status);
        Ok(())
    }

    /// Call `owner` and return its status, unexamined.
    pub(super) fn emit_owner_call(
        &mut self,
        owner: solve::SolvePureCallOwnerId,
        arguments: &[solve::SolveRegisterId],
        output: Value,
    ) -> Result<Value, CompileError> {
        let input = self.packed_tape(arguments, "nested typed input")?;
        if self.observation.is_some() {
            let width = self.register_cell_count(arguments, "native child invocation")?;
            self.carrier_event(
                super::observation_carrier::CALL_ENTER,
                input,
                width as usize,
                None,
            )?;
        }
        let call = match self
            .recursive_calls
            .filter(|calls| calls.group.contains(owner))
        {
            Some(calls) => {
                let function = calls
                    .functions
                    .get(owner.index() as usize)
                    .copied()
                    .flatten()
                    .ok_or_else(|| {
                        CompileError::Backend("recursive typed call owner is missing".into())
                    })?;
                let local = declare_far_call_in_func(self.module, function, self.builder.func);
                let depth = self.builder.ins().iadd_imm(calls.depth, 1);
                {
                    let arena = scratch::parameter(self.builder);
                    self.builder
                        .ins()
                        .call(local, &[input, output, depth, arena])
                }
            }
            None => {
                let function = self
                    .functions
                    .get(owner.index() as usize)
                    .copied()
                    .flatten()
                    .ok_or_else(|| {
                        CompileError::Backend("nested typed call owner is missing".into())
                    })?;
                let local = declare_far_call_in_func(self.module, function, self.builder.func);
                {
                    let arena = scratch::parameter(self.builder);
                    self.builder.ins().call(local, &[input, output, arena])
                }
            }
        };
        let status = self.builder.inst_results(call)[0];
        self.carrier_event(super::observation_carrier::CALL_EXIT, input, 0, None)?;
        Ok(status)
    }

    fn copy(
        &mut self,
        source: &ValueLocation,
        destination: &ValueLocation,
    ) -> Result<(), CompileError> {
        // A placed region can hold a source and its destination on the same
        // cells under different bases; copying them onto themselves is a no-op.
        if source.value_type == destination.value_type
            && self.physical(source) == self.physical(destination)
        {
            return Ok(());
        }
        self.copy_between_bases(
            source,
            destination,
            self.base(source),
            self.base(destination),
        )
    }

    fn copy_between_bases(
        &mut self,
        source: &ValueLocation,
        destination: &ValueLocation,
        source_base: Value,
        destination_base: Value,
    ) -> Result<(), CompileError> {
        if source.value_type != destination.value_type {
            return Err(CompileError::Backend(
                "typed native copy type mismatch".into(),
            ));
        }
        if source.base == destination.base
            && source.cell == destination.cell
            && source_base == destination_base
        {
            return Ok(());
        }
        let count = source.value_type.scalar_count();
        if count > 16 {
            // Cells are 64-bit whatever the element type, so a value is one
            // contiguous byte range; a block move beats the scalar loop.
            let source_address = cell_pointer(self.builder, source_base, source.cell, "copy")?;
            let destination_address =
                cell_pointer(self.builder, destination_base, destination.cell, "copy")?;
            let bytes = self
                .builder
                .ins()
                .iconst(self.pointer_type, i64::from(count) * CELL_BYTES as i64);
            let config = self.module.target_config();
            self.builder
                .call_memmove(config, destination_address, source_address, bytes);
            return Ok(());
        }
        self.for_each_element(source.value_type.scalar_count(), |this, index| {
            let value = this.load_scalar_from(source, source_base, index)?;
            this.store_scalar_to(destination, destination_base, index, value)
        })
    }

    fn for_each_element(
        &mut self,
        count: u32,
        mut body: impl FnMut(&mut Self, Value) -> Result<(), CompileError>,
    ) -> Result<(), CompileError> {
        if count <= 16 {
            for index in 0..count {
                let index = self.builder.ins().iconst(types::I64, i64::from(index));
                body(self, index)?;
            }
            return Ok(());
        }
        let header = self.builder.create_block();
        let loop_body = self.builder.create_block();
        let exit = self.builder.create_block();
        self.builder.append_block_param(header, types::I64);
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.builder.ins().jump(header, &[zero.into()]);
        self.builder.switch_to_block(header);
        let index = self.builder.block_params(header)[0];
        let in_range =
            self.builder
                .ins()
                .icmp_imm(IntCC::UnsignedLessThan, index, i64::from(count));
        self.builder.ins().brif(in_range, loop_body, &[], exit, &[]);
        self.builder.switch_to_block(loop_body);
        body(self, index)?;
        let next = self.builder.ins().iadd_imm(index, 1);
        self.builder.ins().jump(header, &[next.into()]);
        self.builder.seal_block(loop_body);
        self.builder.seal_block(header);
        self.builder.switch_to_block(exit);
        self.builder.seal_block(exit);
        Ok(())
    }

    fn load_scalar(
        &mut self,
        location: &ValueLocation,
        index: Value,
    ) -> Result<Value, CompileError> {
        self.load_scalar_from(location, self.base(location), index)
    }

    fn load_scalar_from(
        &mut self,
        location: &ValueLocation,
        base: Value,
        index: Value,
    ) -> Result<Value, CompileError> {
        let address = self.cell_address(base, location.cell, index);
        Ok(self.builder.ins().load(
            scalar_cranelift_type(location.value_type.element_type()),
            self.flags,
            address,
            0,
        ))
    }

    fn store_scalar(
        &mut self,
        location: &ValueLocation,
        index: Value,
        value: Value,
    ) -> Result<(), CompileError> {
        self.store_scalar_to(location, self.base(location), index, value)
    }

    fn store_scalar_to(
        &mut self,
        location: &ValueLocation,
        base: Value,
        index: Value,
        value: Value,
    ) -> Result<(), CompileError> {
        let address = self.cell_address(base, location.cell, index);
        self.builder.ins().store(self.flags, value, address, 0);
        Ok(())
    }

    fn cell_address(&mut self, base: Value, start: u32, index: Value) -> Value {
        let index = self.builder.ins().iadd_imm(index, i64::from(start));
        let bytes = self.builder.ins().imul_imm(index, CELL_BYTES as i64);
        self.builder.ins().iadd(base, bytes)
    }
}

fn is_binary64(scalar: solve::SolveScalarType) -> bool {
    matches!(
        scalar,
        solve::SolveScalarType::Real {
            format: solve::SolveRealFormat::Binary64,
            ..
        }
    )
}

fn scalar_cranelift_type(scalar: solve::SolveScalarType) -> Type {
    match scalar {
        solve::SolveScalarType::Real {
            format: solve::SolveRealFormat::Binary32,
            ..
        } => types::F32,
        solve::SolveScalarType::Real {
            format: solve::SolveRealFormat::Binary64,
            ..
        } => types::F64,
        solve::SolveScalarType::Integer(_) | solve::SolveScalarType::Boolean => types::I64,
    }
}

fn promote_real(
    builder: &mut FunctionBuilder<'_>,
    format: solve::SolveRealFormat,
    value: Value,
) -> Value {
    match format {
        solve::SolveRealFormat::Binary32 => builder.ins().fpromote(types::F64, value),
        solve::SolveRealFormat::Binary64 => value,
    }
}

fn demote_real(
    builder: &mut FunctionBuilder<'_>,
    format: solve::SolveRealFormat,
    value: Value,
) -> Value {
    match format {
        solve::SolveRealFormat::Binary32 => builder.ins().fdemote(types::F32, value),
        solve::SolveRealFormat::Binary64 => value,
    }
}

fn promote_conversion_source(
    builder: &mut FunctionBuilder<'_>,
    source: solve::SolveScalarType,
    value: Value,
) -> Result<Value, CompileError> {
    match source {
        solve::SolveScalarType::Real { format, .. } => Ok(promote_real(builder, format, value)),
        _ => Err(CompileError::Backend(
            "invalid Real-to-Integer source".into(),
        )),
    }
}

/// Bound the depth of a SOLVE-C62 group member form on entry, and name the
/// calls it makes to its own group.
fn enter_group<'a>(
    builder: &mut FunctionBuilder<'_>,
    group: solve::SolveRecursiveGroup,
    functions: &'a [Option<FuncId>],
    depth: Value,
) -> RecursiveCalls<'a> {
    let within = builder.ins().icmp_imm(
        IntCC::SignedLessThanOrEqual,
        depth,
        i64::from(group.depth_limit()),
    );
    status::require_recursion_depth(builder, within);
    RecursiveCalls {
        group,
        functions,
        depth,
    }
}

fn program_signature(
    signature: &mut cranelift_codegen::ir::Signature,
    pointer_type: cranelift_codegen::ir::Type,
    recursive: bool,
) {
    signature.returns.push(AbiParam::new(types::I8));
    signature.params.push(AbiParam::new(pointer_type));
    signature.params.push(AbiParam::new(pointer_type));
    if recursive {
        signature.params.push(AbiParam::new(types::I64));
    }
    signature.params.push(AbiParam::new(pointer_type)); // caller-owned scratch
}
