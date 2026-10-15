//! Only the actual accepted compiled invocation may supply an action's message.
use rumoca_exec_wasm::{CompiledRetainedEventWasm, NativeAssertionCells, WasmCompileError};
use rumoca_ir_solve::{
    RetainedEventProgram, SolveEventAction, SolvePureCallTable, SolveScalarType, SolveValueKind,
    VarLayout,
};
use std::cell::RefCell;

pub(super) struct WasmObservations {
    code: CompiledRetainedEventWasm,
    messages: RefCell<Vec<(usize, String)>>,
}

impl WasmObservations {
    pub(super) fn compile(
        program: RetainedEventProgram,
        layout: &VarLayout,
        calls: &SolvePureCallTable,
    ) -> Result<Self, String> {
        if !program
            .source()
            .model()
            .problem
            .layout
            .same_execution_layout(layout)
            || !program.table().shares_table_owner(calls)
        {
            return Err("WASM observation differs from its original execution context".into());
        }
        Ok(Self {
            code: CompiledRetainedEventWasm::compile(program).map_err(|error| error.to_string())?,
            messages: RefCell::default(),
        })
    }
}

impl rumoca_solver::CompiledSolveEventObservation for WasmObservations {
    fn observed_message(&self, action: &SolveEventAction) -> Option<String> {
        let index = self
            .code
            .source()
            .source()
            .model()
            .problem
            .events
            .actions
            .iter()
            .position(|original| std::ptr::eq(original, action))?;
        self.messages
            .borrow()
            .iter()
            .find(|(original, _)| *original == index)
            .map(|(_, message)| message.clone())
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        time: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        self.messages.borrow_mut().clear();
        super::profile::validate_inputs(
            &self.code.source().source().model().problem.layout,
            y.len(),
            p.len(),
            tables.len(),
        )?;
        let mut messages = Vec::new();
        let mut formatting_error = None;
        let result = self
            .code
            .call(y, p, time, out, |action, captures, span, owner| {
                let captures = captures
                    .iter()
                    .map(|(output, cells)| Ok((*output, typed_cells(cells)?)))
                    .collect::<Result<Vec<_>, WasmCompileError>>()?;
                let message =
                    rumoca_eval_solve::render_retained_message(action, &captures, span, owner.id())
                        .map_err(|error| {
                            formatting_error = Some(error);
                            WasmCompileError::Backend(
                                "compiled observation message conversion failed".into(),
                            )
                        })?;
                messages.push((action.action_index(), message));
                Ok(())
            });
        if let Some(error) = formatting_error {
            return Err(error.into());
        }
        result.map_err(rumoca_solver::runtime::native_wasm_error::execution_error)?;
        *self.messages.borrow_mut() = messages;
        Ok(())
    }
}

fn typed_cells(
    cells: &NativeAssertionCells,
) -> Result<rumoca_eval_solve::TypedValue, WasmCompileError> {
    let scalar = cells.value_type().element_type();
    let values = cells
        .bits()
        .iter()
        .map(|bits| match scalar {
            SolveScalarType::Real { .. } => Ok(SolveValueKind::Real64(*bits)),
            SolveScalarType::Integer(_) => Ok(SolveValueKind::Integer(*bits as i64)),
            SolveScalarType::Boolean if *bits <= 1 => Ok(SolveValueKind::Boolean(*bits != 0)),
            SolveScalarType::Boolean => Err(WasmCompileError::Backend(
                "WASM observed Boolean is malformed".into(),
            )),
        })
        .collect::<Result<Vec<_>, _>>()?;
    rumoca_eval_solve::TypedValue::construct(cells.value_type().clone(), values)
        .map_err(|error| WasmCompileError::Backend(error.to_string()))
}
