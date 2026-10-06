//! One demand-built scalar fallback shared by the legacy and plan views.

use super::{scalar_program_plan::ScalarProgramPlan, solve_lazy::SolveProgramsObject};
use crate::errors::{CodegenError, render_err};
use minijinja::{
    Value,
    value::{Enumerator, Object, ObjectRepr},
};
use rumoca_ir_solve as solve;
use std::sync::{Arc, OnceLock};

#[derive(Debug)]
pub(super) struct LazyScalarProjection {
    block: Arc<solve::ComputeBlock>,
    scalar: OnceLock<Result<Arc<solve::ScalarProgramBlock>, CodegenError>>,
    plan: OnceLock<Result<Value, CodegenError>>,
}

impl LazyScalarProjection {
    fn new(block: Arc<solve::ComputeBlock>) -> Self {
        Self {
            block,
            scalar: OnceLock::new(),
            plan: OnceLock::new(),
        }
    }
    fn scalar(&self) -> Result<&Arc<solve::ScalarProgramBlock>, &CodegenError> {
        self.scalar
            .get_or_init(|| {
                rumoca_eval_solve::to_scalar_program_block(&self.block)
                    .map(Arc::new)
                    .map_err(CodegenError::from)
            })
            .as_ref()
    }
    fn plan(&self) -> Result<&Value, &CodegenError> {
        self.plan
            .get_or_init(|| {
                let scalar = self.scalar().map_err(Clone::clone)?;
                ScalarProgramPlan::new(scalar.clone()).map(Value::from_object)
            })
            .as_ref()
    }
    #[cfg(test)]
    pub(super) fn initialized(&self) -> bool {
        self.scalar.get().is_some()
    }
}

fn invalid(error: &CodegenError) -> Value {
    Value::from(render_err(error.to_string()).with_source(error.clone()))
}

impl Object for LazyScalarProjection {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Map
    }
    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Str(&["programs", "program_spans", "output_indices"])
    }
    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let key = key.as_str()?;
        if !matches!(key, "programs" | "program_spans" | "output_indices") {
            return None;
        }
        let scalar = match self.scalar() {
            Ok(scalar) => scalar,
            Err(error) => return Some(invalid(error)),
        };
        match key {
            "programs" => Some(Value::from_object(SolveProgramsObject {
                block: scalar.clone(),
            })),
            "program_spans" => Some(Value::from_serialize(scalar.program_spans())),
            "output_indices" => Some(Value::from_serialize(scalar.output_indices())),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct LazyScalarPlan(Arc<LazyScalarProjection>);
impl Object for LazyScalarPlan {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Map
    }
    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Str(&[
            "programs",
            "output_count",
            "stored_output_count",
            "uses_linear_solve_component",
        ])
    }
    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        if !matches!(
            key.as_str()?,
            "programs" | "output_count" | "stored_output_count" | "uses_linear_solve_component"
        ) {
            return None;
        }
        Some(match self.0.plan() {
            Ok(plan) => plan.get_item(key).unwrap_or_else(Value::from),
            Err(error) => invalid(error),
        })
    }
}

pub(super) fn views(block: Arc<solve::ComputeBlock>) -> (Value, Value) {
    let projection = Arc::new(LazyScalarProjection::new(block));
    (
        Value::from_dyn_object(projection.clone()),
        Value::from_object(LazyScalarPlan(projection)),
    )
}

/// Check compact metadata before rendering without constructing scalar rows.
pub(super) fn validate(block: &solve::ComputeBlock) -> Result<(), CodegenError> {
    block
        .validate_shape_contract("scalarize compute block")
        .map_err(rumoca_eval_solve::ScalarizeError::from)
        .map_err(CodegenError::from)
}
