//! Template view of the regions of a checked function-conditional program.
//!
//! Each arm condition, arm result, and the fallback is its own scalar program
//! with a private register file. A region's `StoreOutput`/`StoreOutputRange`
//! write the region's outputs in order, so each is attached to its local
//! output ordinals, and every region operation is viewed exactly as a block
//! operation is.

use std::sync::Arc;

use minijinja::Value;
use minijinja::value::{Enumerator, Object, ObjectRepr};
use rumoca_ir_solve as solve;

/// The arms of one function-conditional program.
#[derive(Debug)]
pub(super) struct PlanArmsValue {
    pub(super) program: Arc<solve::FunctionConditionalProgram>,
}

impl Object for PlanArmsValue {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Seq
    }

    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let arm = key.as_usize()?;
        (arm < self.program.arms.len()).then(|| {
            Value::from_object(PlanArmValue {
                program: Arc::clone(&self.program),
                arm,
            })
        })
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Seq(self.program.arms.len())
    }
}

#[derive(Debug)]
struct PlanArmValue {
    program: Arc<solve::FunctionConditionalProgram>,
    arm: usize,
}

impl Object for PlanArmValue {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Map
    }

    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let part = match key.as_str()? {
            "condition" => RegionPart::Condition(self.arm),
            "result" => RegionPart::Result(self.arm),
            _ => return None,
        };
        Some(region_value(&self.program, part))
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Str(&["condition", "result"])
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) enum RegionPart {
    Condition(usize),
    Result(usize),
    Fallback,
}

/// The template view of one region of `program`.
pub(super) fn region_value(
    program: &Arc<solve::FunctionConditionalProgram>,
    part: RegionPart,
) -> Value {
    let region = PlanRegionValue {
        program: Arc::clone(program),
        part,
    };
    let targets = local_output_targets(region.ops());
    Value::from_object(region.with_targets(targets))
}

#[derive(Debug)]
struct PlanRegionValue {
    program: Arc<solve::FunctionConditionalProgram>,
    part: RegionPart,
}

impl PlanRegionValue {
    fn ops(&self) -> &[solve::LinearOp] {
        match self.part {
            RegionPart::Condition(arm) => &self.program.arms[arm].condition,
            RegionPart::Result(arm) => &self.program.arms[arm].result,
            RegionPart::Fallback => &self.program.fallback,
        }
    }

    fn register_count(&self) -> usize {
        match self.part {
            RegionPart::Condition(arm) => self.program.arms[arm].condition_register_count,
            RegionPart::Result(arm) => self.program.arms[arm].result_register_count,
            RegionPart::Fallback => self.program.fallback_register_count,
        }
    }

    fn with_targets(self, targets: Vec<Option<Box<[usize]>>>) -> PlanRegionWithTargets {
        PlanRegionWithTargets {
            region: self,
            targets: Arc::new(targets),
        }
    }
}

#[derive(Debug)]
struct PlanRegionWithTargets {
    region: PlanRegionValue,
    targets: Arc<Vec<Option<Box<[usize]>>>>,
}

impl Object for PlanRegionWithTargets {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Map
    }

    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        match key.as_str()? {
            "ops" => Some(Value::from_object(PlanRegionOpsValue {
                region: Arc::clone(self),
            })),
            "register_count" => Some(Value::from(self.region.register_count())),
            _ => None,
        }
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Str(&["ops", "register_count"])
    }
}

#[derive(Debug)]
struct PlanRegionOpsValue {
    region: Arc<PlanRegionWithTargets>,
}

impl Object for PlanRegionOpsValue {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Seq
    }

    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let index = key.as_usize()?;
        (index < self.region.region.ops().len()).then(|| {
            Value::from_object(PlanRegionOpValue {
                region: Arc::clone(&self.region),
                index,
            })
        })
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Seq(self.region.region.ops().len())
    }
}

#[derive(Debug)]
struct PlanRegionOpValue {
    region: Arc<PlanRegionWithTargets>,
    index: usize,
}

impl Object for PlanRegionOpValue {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Map
    }

    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        super::scalar_program_plan::op_field(
            &self.region.region.ops()[self.index],
            self.region.targets[self.index].as_deref(),
            key.as_str()?,
        )
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Str(super::scalar_program_plan::op_keys(
            &self.region.region.ops()[self.index],
        ))
    }
}

/// Each store's region-local output ordinals, in program order.
fn local_output_targets(ops: &[solve::LinearOp]) -> Vec<Option<Box<[usize]>>> {
    let mut next = 0usize;
    ops.iter()
        .map(|op| {
            let count = match op {
                solve::LinearOp::StoreOutput { .. } => 1,
                solve::LinearOp::StoreOutputRange { count, .. } => *count,
                _ => return None,
            };
            let targets = (next..next + count).collect::<Box<[usize]>>();
            next += count;
            Some(targets)
        })
        .collect()
}
