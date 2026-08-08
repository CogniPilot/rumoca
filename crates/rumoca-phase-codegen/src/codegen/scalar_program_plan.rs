//! Target-neutral, lazy template view of scalar Solve programs.
//!
//! The plan preserves Solve-IR operation vocabulary and dense register
//! references. It validates and attaches each `StoreOutput` to its checked
//! output slot, but deliberately contains no target-language text.

use std::sync::Arc;

use minijinja::Value;
use minijinja::value::{Enumerator, Object, ObjectRepr};
use rumoca_ir_solve as solve;

use crate::errors::CodegenError;

#[derive(Debug)]
struct ProgramMetadata {
    output_targets: Vec<Option<usize>>,
    output_count: usize,
    temporary_count: usize,
}

#[derive(Debug)]
pub(super) struct ScalarProgramPlan {
    block: Arc<solve::ScalarProgramBlock>,
    metadata: Arc<Vec<ProgramMetadata>>,
}

impl ScalarProgramPlan {
    pub(super) fn new(block: Arc<solve::ScalarProgramBlock>) -> Result<Self, CodegenError> {
        let metadata = Arc::new(build_metadata(&block)?);
        Ok(Self { block, metadata })
    }
}

impl Object for ScalarProgramPlan {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Map
    }

    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        match key.as_str()? {
            "programs" => Some(Value::from_object(PlanProgramsValue {
                block: self.block.clone(),
                metadata: self.metadata.clone(),
            })),
            "output_count" => Some(Value::from(self.block.output_count())),
            "stored_output_count" => Some(Value::from(self.block.stored_output_count())),
            "uses_linear_solve_component" => {
                Some(Value::from(self.block.uses_linear_solve_component()))
            }
            _ => None,
        }
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Str(&[
            "programs",
            "output_count",
            "stored_output_count",
            "uses_linear_solve_component",
        ])
    }
}

#[derive(Debug)]
struct PlanProgramsValue {
    block: Arc<solve::ScalarProgramBlock>,
    metadata: Arc<Vec<ProgramMetadata>>,
}

impl Object for PlanProgramsValue {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Seq
    }

    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let index = key.as_usize()?;
        (index < self.block.programs().len()).then(|| {
            Value::from_object(PlanProgramValue {
                block: self.block.clone(),
                metadata: self.metadata.clone(),
                index,
            })
        })
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Seq(self.block.programs().len())
    }
}

#[derive(Debug)]
struct PlanProgramValue {
    block: Arc<solve::ScalarProgramBlock>,
    metadata: Arc<Vec<ProgramMetadata>>,
    index: usize,
}

impl Object for PlanProgramValue {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Map
    }

    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let metadata = &self.metadata[self.index];
        match key.as_str()? {
            "ops" => Some(Value::from_object(PlanOpsValue {
                block: self.block.clone(),
                metadata: self.metadata.clone(),
                program_index: self.index,
            })),
            "span" => self
                .block
                .program_span(self.index)
                .map(Value::from_serialize),
            "output_count" => Some(Value::from(metadata.output_count)),
            "temporary_count" => Some(Value::from(metadata.temporary_count)),
            _ => None,
        }
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Str(&["ops", "span", "output_count", "temporary_count"])
    }
}

#[derive(Debug)]
struct PlanOpsValue {
    block: Arc<solve::ScalarProgramBlock>,
    metadata: Arc<Vec<ProgramMetadata>>,
    program_index: usize,
}

impl Object for PlanOpsValue {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Seq
    }

    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let op_index = key.as_usize()?;
        (op_index < self.block.programs()[self.program_index].len()).then(|| {
            Value::from_object(PlanOpValue {
                block: self.block.clone(),
                metadata: self.metadata.clone(),
                program_index: self.program_index,
                op_index,
            })
        })
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Seq(self.block.programs()[self.program_index].len())
    }
}

#[derive(Debug)]
struct PlanOpValue {
    block: Arc<solve::ScalarProgramBlock>,
    metadata: Arc<Vec<ProgramMetadata>>,
    program_index: usize,
    op_index: usize,
}

impl PlanOpValue {
    fn op(&self) -> &solve::LinearOp {
        &self.block.programs()[self.program_index][self.op_index]
    }
}

impl Object for PlanOpValue {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Map
    }

    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        op_field(
            self.op(),
            self.metadata[self.program_index].output_targets[self.op_index],
            key.as_str()?,
        )
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Str(op_keys(self.op()))
    }
}

fn build_metadata(block: &solve::ScalarProgramBlock) -> Result<Vec<ProgramMetadata>, CodegenError> {
    let mut output_ordinal = 0usize;
    let mut metadata = Vec::new();
    reserve_metadata(&mut metadata, block.programs().len())?;
    for program in block.programs() {
        metadata.push(build_program_metadata(
            program,
            block.output_indices(),
            &mut output_ordinal,
        )?);
    }
    validate_output_count(output_ordinal, block.output_indices().len())?;
    Ok(metadata)
}

fn reserve_metadata(
    metadata: &mut Vec<ProgramMetadata>,
    program_count: usize,
) -> Result<(), CodegenError> {
    metadata.try_reserve_exact(program_count).map_err(|_| {
        CodegenError::template("scalar program plan metadata exceeds host memory limits")
    })
}

fn build_program_metadata(
    program: &[solve::LinearOp],
    output_indices: &[usize],
    output_ordinal: &mut usize,
) -> Result<ProgramMetadata, CodegenError> {
    let mut output_targets = Vec::new();
    output_targets
        .try_reserve_exact(program.len())
        .map_err(|_| {
            CodegenError::template("scalar program plan operation metadata exceeds host limits")
        })?;
    let mut output_count = 0usize;
    let mut temporary_count = 0usize;
    for op in program {
        temporary_count = temporary_count.max(temporary_count_after(op)?);
        output_targets.push(take_output_target(
            op,
            output_indices,
            output_ordinal,
            &mut output_count,
        )?);
    }
    Ok(ProgramMetadata {
        output_targets,
        output_count,
        temporary_count,
    })
}

fn temporary_count_after(op: &solve::LinearOp) -> Result<usize, CodegenError> {
    let Some(dst) = op.dst_register() else {
        return Ok(0);
    };
    usize::try_from(dst)
        .ok()
        .and_then(|dst| dst.checked_add(1))
        .ok_or_else(|| {
            CodegenError::template("scalar program plan temporary index exceeds host range")
        })
}

fn take_output_target(
    op: &solve::LinearOp,
    output_indices: &[usize],
    output_ordinal: &mut usize,
    output_count: &mut usize,
) -> Result<Option<usize>, CodegenError> {
    if !matches!(op, solve::LinearOp::StoreOutput { .. }) {
        return no_output_target();
    }
    let target = output_indices
        .get(*output_ordinal)
        .copied()
        .ok_or_else(|| {
            CodegenError::template(format!(
                "scalar program plan is missing output mapping #{}",
                *output_ordinal
            ))
        })?;
    *output_ordinal = output_ordinal
        .checked_add(1)
        .ok_or_else(|| CodegenError::template("scalar program plan output ordinal overflow"))?;
    *output_count = output_count
        .checked_add(1)
        .ok_or_else(|| CodegenError::template("scalar program output count exceeds host range"))?;
    Ok(Some(target))
}

fn no_output_target() -> Result<Option<usize>, CodegenError> {
    Ok(Option::None)
}

fn validate_output_count(store_count: usize, mapping_count: usize) -> Result<(), CodegenError> {
    if store_count == mapping_count {
        Ok(())
    } else {
        Err(CodegenError::template(format!(
            "scalar program plan has {store_count} stores but {mapping_count} output mappings"
        )))
    }
}

fn op_field(op: &solve::LinearOp, output_target: Option<usize>, key: &str) -> Option<Value> {
    match key {
        "kind" => return Some(Value::from(op.kind_name())),
        "dst" => return op.dst_register().map(|value| Value::from(value as usize)),
        _ => {}
    }
    load_field(op, key)
        .or_else(|| stateful_field(op, key))
        .or_else(|| arithmetic_field(op, output_target, key))
}

fn load_field(op: &solve::LinearOp, key: &str) -> Option<Value> {
    use solve::LinearOp;
    match *op {
        LinearOp::Const { value, .. } => match key {
            "value" if value.is_finite() => Some(Value::from(value)),
            "value_class" => Some(Value::from(float_class(value))),
            _ => None,
        },
        LinearOp::LoadY { index, .. }
        | LinearOp::LoadP { index, .. }
        | LinearOp::LoadSeed { index, .. }
            if key == "index" =>
        {
            Some(Value::from(index))
        }
        LinearOp::LoadIndexedP {
            base, count, index, ..
        }
        | LinearOp::LoadIndexedSeed {
            base, count, index, ..
        } => match key {
            "base" => Some(Value::from(base)),
            "count" => Some(Value::from(count)),
            "index_ref" => Some(Value::from(index as usize)),
            _ => None,
        },
        LinearOp::Move { src, .. } if key == "src" => Some(Value::from(src as usize)),
        LinearOp::LinearSolveComponent {
            matrix_start,
            rhs_start,
            n,
            component,
            ..
        } => match key {
            "matrix_start" => Some(Value::from(matrix_start as usize)),
            "rhs_start" => Some(Value::from(rhs_start as usize)),
            "n" => Some(Value::from(n)),
            "component" => Some(Value::from(component)),
            _ => None,
        },
        _ => None,
    }
}

fn float_class(value: f64) -> &'static str {
    match (
        value.is_nan(),
        value.is_infinite(),
        value.is_sign_negative(),
    ) {
        (true, _, _) => "nan",
        (false, true, true) => "negative_infinity",
        (false, true, false) => "positive_infinity",
        (false, false, _) => "finite",
    }
}

fn stateful_field(op: &solve::LinearOp, key: &str) -> Option<Value> {
    use solve::LinearOp;
    match *op {
        LinearOp::TableBounds { table_id, max, .. } => match key {
            "table_id" => Some(Value::from(table_id as usize)),
            "max" => Some(Value::from(max)),
            _ => None,
        },
        LinearOp::TableLookup {
            table_id,
            column,
            input,
            ..
        }
        | LinearOp::TableLookupSlope {
            table_id,
            column,
            input,
            ..
        } => match key {
            "table_id" => Some(Value::from(table_id as usize)),
            "column" => Some(Value::from(column as usize)),
            "input" => Some(Value::from(input as usize)),
            _ => None,
        },
        LinearOp::TableNextEvent { table_id, time, .. } => match key {
            "table_id" => Some(Value::from(table_id as usize)),
            "time" => Some(Value::from(time as usize)),
            _ => None,
        },
        LinearOp::RandomInitialState {
            generator,
            local_seed,
            global_seed,
            state_len,
            state_index,
            ..
        } => match key {
            "generator" => Some(Value::from(random_generator_tag(generator))),
            "local_seed" => Some(Value::from(local_seed as usize)),
            "global_seed" => Some(Value::from(global_seed as usize)),
            "state_len" => Some(Value::from(state_len)),
            "state_index" => Some(Value::from(state_index)),
            _ => None,
        },
        LinearOp::RandomResult {
            generator,
            state_start,
            state_len,
            ..
        } => match key {
            "generator" => Some(Value::from(random_generator_tag(generator))),
            "state_start" => Some(Value::from(state_start as usize)),
            "state_len" => Some(Value::from(state_len)),
            _ => None,
        },
        LinearOp::RandomState {
            generator,
            state_start,
            state_len,
            state_index,
            ..
        } => match key {
            "generator" => Some(Value::from(random_generator_tag(generator))),
            "state_start" => Some(Value::from(state_start as usize)),
            "state_len" => Some(Value::from(state_len)),
            "state_index" => Some(Value::from(state_index)),
            _ => None,
        },
        LinearOp::ImpureRandomInit { seed, .. } if key == "seed" => {
            Some(Value::from(seed as usize))
        }
        LinearOp::ImpureRandom { id, call_site, .. } => match key {
            "id" => Some(Value::from(id as usize)),
            "call_site" => Some(Value::from(call_site)),
            _ => None,
        },
        LinearOp::ImpureRandomInteger {
            id,
            imin,
            imax,
            call_site,
            ..
        } => match key {
            "id" => Some(Value::from(id as usize)),
            "imin" => Some(Value::from(imin as usize)),
            "imax" => Some(Value::from(imax as usize)),
            "call_site" => Some(Value::from(call_site)),
            _ => None,
        },
        _ => None,
    }
}

fn arithmetic_field(
    op: &solve::LinearOp,
    output_target: Option<usize>,
    key: &str,
) -> Option<Value> {
    use solve::LinearOp;
    match *op {
        LinearOp::Unary { op, arg, .. } => match key {
            "operator" => Some(Value::from(op.kind_name())),
            "arg" => Some(Value::from(arg as usize)),
            _ => None,
        },
        LinearOp::Binary { op, lhs, rhs, .. } => match key {
            "operator" => Some(Value::from(op.kind_name())),
            "lhs" => Some(Value::from(lhs as usize)),
            "rhs" => Some(Value::from(rhs as usize)),
            _ => None,
        },
        LinearOp::Compare { op, lhs, rhs, .. } => match key {
            "operator" => Some(Value::from(op.kind_name())),
            "lhs" => Some(Value::from(lhs as usize)),
            "rhs" => Some(Value::from(rhs as usize)),
            _ => None,
        },
        LinearOp::Select {
            cond,
            if_true,
            if_false,
            ..
        } => match key {
            "cond" => Some(Value::from(cond as usize)),
            "if_true" => Some(Value::from(if_true as usize)),
            "if_false" => Some(Value::from(if_false as usize)),
            _ => None,
        },
        LinearOp::StoreOutput { src } => match key {
            "src" => Some(Value::from(src as usize)),
            "output_index" => output_target.map(Value::from),
            _ => None,
        },
        _ => None,
    }
}

fn op_keys(op: &solve::LinearOp) -> &'static [&'static str] {
    use solve::LinearOp;
    match op {
        LinearOp::Const { .. } => &["kind", "dst", "value", "value_class"],
        LinearOp::LoadTime { .. } => &["kind", "dst"],
        LinearOp::LoadY { .. } | LinearOp::LoadP { .. } | LinearOp::LoadSeed { .. } => {
            &["kind", "dst", "index"]
        }
        LinearOp::LoadIndexedP { .. } | LinearOp::LoadIndexedSeed { .. } => {
            &["kind", "dst", "base", "count", "index_ref"]
        }
        LinearOp::Move { .. } => &["kind", "dst", "src"],
        LinearOp::LinearSolveComponent { .. } => {
            &["kind", "dst", "matrix_start", "rhs_start", "n", "component"]
        }
        LinearOp::Unary { .. } => &["kind", "dst", "operator", "arg"],
        LinearOp::Binary { .. } | LinearOp::Compare { .. } => {
            &["kind", "dst", "operator", "lhs", "rhs"]
        }
        LinearOp::Select { .. } => &["kind", "dst", "cond", "if_true", "if_false"],
        LinearOp::StoreOutput { .. } => &["kind", "src", "output_index"],
        LinearOp::TableBounds { .. } => &["kind", "dst", "table_id", "max"],
        LinearOp::TableLookup { .. } | LinearOp::TableLookupSlope { .. } => {
            &["kind", "dst", "table_id", "column", "input"]
        }
        LinearOp::TableNextEvent { .. } => &["kind", "dst", "table_id", "time"],
        LinearOp::RandomInitialState { .. } => &[
            "kind",
            "dst",
            "generator",
            "local_seed",
            "global_seed",
            "state_len",
            "state_index",
        ],
        LinearOp::RandomResult { .. } => &["kind", "dst", "generator", "state_start", "state_len"],
        LinearOp::RandomState { .. } => &[
            "kind",
            "dst",
            "generator",
            "state_start",
            "state_len",
            "state_index",
        ],
        LinearOp::ImpureRandomInit { .. } => &["kind", "dst", "seed"],
        LinearOp::ImpureRandom { .. } => &["kind", "dst", "id", "call_site"],
        LinearOp::ImpureRandomInteger { .. } => &["kind", "dst", "id", "imin", "imax", "call_site"],
    }
}

fn random_generator_tag(generator: solve::RandomGenerator) -> &'static str {
    match generator {
        solve::RandomGenerator::Xorshift64Star => "Xorshift64Star",
        solve::RandomGenerator::Xorshift128Plus => "Xorshift128Plus",
        solve::RandomGenerator::Xorshift1024Star => "Xorshift1024Star",
    }
}
