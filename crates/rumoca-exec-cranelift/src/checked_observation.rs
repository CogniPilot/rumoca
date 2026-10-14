//! Compiled event observations retain their exact issuing source and private outcomes.
use crate::emit::observation_carrier::{Carrier, Record, Storage};
use crate::{CompileError, CompiledExpressionRows, CompiledPureCallTable};
use rumoca_eval_solve::{AssertionReport, TypedValue};
use rumoca_ir_solve as solve;
use std::cell::RefCell;

pub struct CompiledEventObservation {
    source: solve::RetainedEventProgram,
    rows: CompiledExpressionRows,
    _code_input: solve::ScalarProgramBlock,
    state: RefCell<ObservationState>,
}

struct ObservationState {
    storage: Option<Storage>,
    reports: Vec<AssertionReport>,
}

pub fn compile_event_observation(
    source: solve::RetainedEventProgram,
    calls: &CompiledPureCallTable,
) -> Result<CompiledEventObservation, CompileError> {
    if !calls.jit.source_matches(source.table()) {
        return Err(fault(
            "checked observations belong to a different native call table",
        ));
    }
    let block = solve::ScalarProgramBlock::with_program_spans(
        vec![source.program().to_vec()],
        vec![source.span()],
    )
    .map_err(|error| CompileError::Input(error.to_string()))?;
    let rows = crate::compile_expression_scalar_program_block_with_pure_calls(&block, calls)?;
    let storage = Carrier::new(&source, &block.programs()[0])?.into_storage();
    Ok(CompiledEventObservation {
        source,
        rows,
        _code_input: block,
        state: RefCell::new(ObservationState {
            storage: Some(storage),
            reports: Vec::new(),
        }),
    })
}

impl CompiledEventObservation {
    pub fn call(
        &self,
        y: &[f64],
        p: &[f64],
        time: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        let mut state = self.state.borrow_mut();
        state.reports.clear();
        let storage = state
            .storage
            .take()
            .ok_or_else(|| CompileError::Backend("checked observation reentry".into()))?;
        let mut carrier = Carrier::resume(&self.source, storage);
        // The ordinary output transaction and the private report transaction succeed together.
        let mut values = vec![0.0; out.len()];
        let result = carrier.execute(|| {
            self.rows
                .call_with_external_tables(y, p, time, tables, &mut values)
        });
        let reports = result.and_then(|()| {
            let mut reports = Vec::new();
            for record in &carrier.records {
                reports.extend(self.decode_record(record)?);
            }
            Ok(reports)
        });
        state.storage = Some(carrier.into_storage());
        state.reports = reports?;
        out.copy_from_slice(&values);
        Ok(())
    }

    fn decode_record(&self, record: &Record) -> Result<Vec<AssertionReport>, CompileError> {
        let mut reports = Vec::new();
        let invocation = self
            .source
            .invocation_by_index(record.operation)
            .ok_or_else(|| fault("native record lost its original invocation"))?;
        let captures = decode_captures(invocation.owner(), &record.captures)?;
        for action in invocation
            .projections()
            .filter(|action| action.predicate_output() == record.predicate)
        {
            if action.action().span != record.span {
                return Err(fault("native observation source provenance differs"));
            }
            let message = rumoca_eval_solve::render_retained_message(
                &action,
                &captures,
                record.span,
                invocation.owner().id(),
            )
            .map_err(CompileError::MessageFormatting)?;
            reports.push(AssertionReport {
                action_index: action.action_index(),
                kind: action.action().kind,
                message,
                span: record.span,
            });
        }
        Ok(reports)
    }

    /// A borrowed original source action selects a report; detached action copies cannot.
    pub fn message_for_action(&self, action: &solve::SolveEventAction) -> Option<String> {
        let index = self
            .source
            .source()
            .model()
            .problem
            .events
            .actions
            .iter()
            .position(|issued| std::ptr::eq(issued, action))?;
        self.state
            .borrow()
            .reports
            .iter()
            .find(|report| {
                report.action_index == index
                    && report.kind == action.kind
                    && report.span == action.span
            })
            .map(|report| report.message.clone())
    }
}

fn fault(message: &str) -> CompileError {
    CompileError::Backend(message.to_owned())
}

fn decode_captures(
    owner: &solve::SolvePureCallOwner,
    captured: &[(usize, Vec<u64>)],
) -> Result<Vec<(usize, TypedValue)>, CompileError> {
    let mut captures = Vec::new();
    for (output, bits) in captured {
        let value_type = owner
            .outputs()
            .get(*output)
            .ok_or_else(|| fault("native capture output is foreign"))?
            .value_type();
        let elements = bits
            .iter()
            .map(|&bits| match value_type.element_type() {
                solve::SolveScalarType::Real {
                    format: solve::SolveRealFormat::Binary64,
                } => Ok(solve::SolveValueKind::Real64(bits)),
                solve::SolveScalarType::Real {
                    format: solve::SolveRealFormat::Binary32,
                } => Ok(solve::SolveValueKind::Real32(bits as u32)),
                solve::SolveScalarType::Integer(_) => {
                    Ok(solve::SolveValueKind::Integer(bits as i64))
                }
                solve::SolveScalarType::Boolean if bits <= 1 => {
                    Ok(solve::SolveValueKind::Boolean(bits != 0))
                }
                _ => Err(fault("native capture has noncanonical typed bits")),
            })
            .collect::<Result<Vec<_>, _>>()?;
        let value = TypedValue::construct(value_type.clone(), elements)
            .map_err(|error| fault(&error.to_string()))?;
        captures.push((*output, value));
    }
    Ok(captures)
}
