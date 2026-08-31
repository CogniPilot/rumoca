//! Linear register-based ops used by compiled evaluators.

// SPEC_0021 file-size exception - split plan: extract the tensor-node op variants and their accessors into linear_op/tensor.rs, keeping scalar register ops here; tracked as RDD2/GALEC cleanup debt (SPEC_0021 follow-up).

use rumoca_core::StructuredIndexDomain;
use serde::{Deserialize, Serialize};
use std::rc::Rc;
use std::sync::Arc;

use crate::{
    SolvePureCallDirectionalSite, SolvePureCallSite, SolveValueType, deserialize_required_option,
};

/// Register index in a lowered op sequence.
pub type Reg = u32;

/// A strided run of registers read as one tensor operand.
///
/// `stride` counts elements, not stored values: `0` broadcasts a single
/// element across the whole run, `1` is compact, and a larger stride steps
/// over an outer dimension. The per-element value width belongs to the
/// operation, not to the operand, so the two operands of one elementwise op
/// always share it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StridedOperand {
    pub start: Reg,
    pub stride: usize,
}

/// A borrowed view of one function-fold tensor-update store.
///
/// `source_base` and `source_stride` locate the element run the store reads
/// from the carried aggregate, `dimensions` are the extents it walks,
/// `updates` and `nodes` are the subscripted patches and the value graph
/// applied per element, `result` selects the node whose value is stored, and
/// `lanes` is the number of adjacent values stored per element.
#[derive(Clone, Copy, Debug)]
pub struct FoldTensorUpdateStore<'a> {
    pub source_base: usize,
    pub source_stride: usize,
    pub dimensions: &'a [u32],
    pub updates: &'a [FoldTensorUpdate],
    pub nodes: &'a [FoldTensorNode],
    pub result: u32,
    pub lanes: usize,
}

/// The extents and storage width of one matrix product
/// `[rows x columns] = [rows x inner] * [inner x columns]`.
///
/// Storage is element-major with `lanes` adjacent values per element: one
/// lane is a primal evaluation, two lanes are the interleaved primal/tangent
/// representation forward AD uses.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MatrixProductShape {
    pub rows: usize,
    pub inner: usize,
    pub columns: usize,
    pub lanes: usize,
}

/// Checked isolator for one exact scalar view of an implicit output.
///
/// The Solve phase issues this certificate with its continuous refresh owner;
/// evaluators execute it directly and never search the residual program for a
/// target assignment.
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum TargetAssignmentShape {
    Direct {
        target_y_index: usize,
        expr_reg: Reg,
        target_scale: f64,
        expr_eval_len: usize,
    },
    Affine {
        target_y_index: usize,
        offset_reg: Reg,
        #[serde(deserialize_with = "deserialize_required_option")]
        coefficient_reg: Option<Reg>,
        offset_scale: f64,
        coefficient_scale: f64,
        expr_eval_len: usize,
    },
    AffineResidual {
        target_y_index: usize,
        target_reg: Reg,
        residual_reg: Reg,
        coefficient: f64,
        expr_eval_len: usize,
    },
}

impl TargetAssignmentShape {
    #[must_use]
    pub const fn target_y_index(self) -> usize {
        match self {
            Self::Direct { target_y_index, .. }
            | Self::Affine { target_y_index, .. }
            | Self::AffineResidual { target_y_index, .. } => target_y_index,
        }
    }

    #[must_use]
    pub const fn expr_eval_len(self) -> usize {
        match self {
            Self::Direct { expr_eval_len, .. }
            | Self::Affine { expr_eval_len, .. }
            | Self::AffineResidual { expr_eval_len, .. } => expr_eval_len,
        }
    }
}

/// Scalar unary operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum UnaryOp {
    Neg,
    Not,
    Abs,
    Sign,
    Sqrt,
    Floor,
    Ceil,
    Trunc,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Sinh,
    Cosh,
    Tanh,
    Exp,
    Log,
    Log10,
}

impl UnaryOp {
    #[must_use]
    pub fn kind_name(&self) -> &'static str {
        match self {
            Self::Neg => "Neg",
            Self::Not => "Not",
            Self::Abs => "Abs",
            Self::Sign => "Sign",
            Self::Sqrt => "Sqrt",
            Self::Floor => "Floor",
            Self::Ceil => "Ceil",
            Self::Trunc => "Trunc",
            Self::Sin => "Sin",
            Self::Cos => "Cos",
            Self::Tan => "Tan",
            Self::Asin => "Asin",
            Self::Acos => "Acos",
            Self::Atan => "Atan",
            Self::Sinh => "Sinh",
            Self::Cosh => "Cosh",
            Self::Tanh => "Tanh",
            Self::Exp => "Exp",
            Self::Log => "Log",
            Self::Log10 => "Log10",
        }
    }
}

/// Scalar binary operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    And,
    Or,
    Atan2,
    Min,
    Max,
}

impl BinaryOp {
    #[must_use]
    pub fn kind_name(&self) -> &'static str {
        match self {
            Self::Add => "Add",
            Self::Sub => "Sub",
            Self::Mul => "Mul",
            Self::Div => "Div",
            Self::Pow => "Pow",
            Self::And => "And",
            Self::Or => "Or",
            Self::Atan2 => "Atan2",
            Self::Min => "Min",
            Self::Max => "Max",
        }
    }
}

/// Comparison operation that yields Modelica boolean-as-real (`0.0`/`1.0`).
///
/// Equality and inequality are exact IEEE comparisons at Solve-IR row level.
/// Relation event detection is represented separately by signed residual root
/// functions and solver root tolerances, not by tolerant `Eq`/`Ne` rows.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum CompareOp {
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
}

impl CompareOp {
    #[must_use]
    pub fn kind_name(&self) -> &'static str {
        match self {
            Self::Lt => "Lt",
            Self::Le => "Le",
            Self::Gt => "Gt",
            Self::Ge => "Ge",
            Self::Eq => "Eq",
            Self::Ne => "Ne",
        }
    }

    #[must_use]
    pub fn compare(self, lhs: f64, rhs: f64) -> bool {
        match self {
            Self::Lt => lhs < rhs,
            Self::Le => lhs <= rhs,
            Self::Gt => lhs > rhs,
            Self::Ge => lhs >= rhs,
            Self::Eq => lhs == rhs,
            Self::Ne => lhs != rhs,
        }
    }

    #[must_use]
    pub fn compare_as_f64(self, lhs: f64, rhs: f64) -> f64 {
        if self.compare(lhs, rhs) { 1.0 } else { 0.0 }
    }
}

/// Supported deterministic random generators in MSL's `Modelica.Math.Random`
/// package. These are solver-IR op kinds, not Modelica identifiers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RandomGenerator {
    Xorshift64Star,
    Xorshift128Plus,
    Xorshift1024Star,
}

/// One axis of a compact runtime tensor projection.
///
/// Constant coordinates are zero-based IR coordinates. Runtime coordinates
/// are registers containing one-based Modelica integer indices.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TensorIndex {
    Constant(u32),
    Runtime(Reg),
}

/// One axis of a compact tensor update.
///
/// `Whole` preserves that axis in the update value. `Index` removes it and
/// selects one zero-based constant or one-based runtime Modelica coordinate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TensorSubscript {
    Whole,
    Index(TensorIndex),
}

/// One axis of a compact ordinary tensor update.
///
/// A slice remains a packed register range of one-based Modelica indices; it
/// is never enumerated into an extent-sized select graph in Solve IR.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TensorUpdateSubscript {
    Whole,
    Index(TensorIndex),
    Slice { start: Reg, dimensions: Box<[u32]> },
}

/// One ordered patch in a compact tensor-valued fold transition.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FoldTensorUpdate {
    pub subscripts: Box<[TensorSubscript]>,
    #[serde(deserialize_with = "deserialize_required_option")]
    pub condition: Option<Reg>,
    pub value_start: Reg,
    pub value_stride: usize,
}

/// One compact tensor-expression node. Node id zero is the carried source;
/// stored nodes have ids `index + 1` and may reference only earlier ids.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum FoldTensorNode {
    Update {
        base: u32,
        update: u32,
    },
    Select {
        condition: Reg,
        if_true: u32,
        if_false: u32,
    },
}

/// Compact source for a nested fold's initial carried tuple.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum FoldInitialSource {
    Registers { start: Reg, count: usize },
    ParentCarried { base: usize, count: usize },
}

/// Compact checked body of a DAE function fold.
///
/// `update` describes exactly one symbolic domain iteration. It reads the
/// current tuple through `LoadFoldCarried`, reads binder coordinates through
/// `LoadFoldIndex`, reads lexically enclosing loop values through
/// `LoadFoldCapture`, and stores exactly `carried_count` next values. Execution
/// owns domain traversal; no compiler phase enumerates the points.
///
/// Invariant-bearing fields cannot be assembled outside the checked
/// constructor:
///
/// ```compile_fail
/// use rumoca_ir_solve::{FunctionFoldProgram, LinearOp};
///
/// fn never<T>() -> T { loop {} }
///
/// let _ = FunctionFoldProgram {
///     domain: never(),
///     domain_scalar_count: never(),
///     carried_count: never(),
///     capture_count: never(),
///     register_count: never(),
///     update: never::<Vec<LinearOp>>(),
/// };
/// ```
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct FunctionFoldProgram {
    domain: StructuredIndexDomain,
    domain_scalar_count: usize,
    carried_count: usize,
    capture_count: usize,
    register_count: usize,
    update: Vec<LinearOp>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct FunctionFoldProgramWire {
    domain: StructuredIndexDomain,
    domain_scalar_count: usize,
    carried_count: usize,
    capture_count: usize,
    register_count: usize,
    update: Vec<LinearOp>,
}

impl<'de> Deserialize<'de> for FunctionFoldProgram {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let wire = FunctionFoldProgramWire::deserialize(deserializer)?;
        let stored_register_count = wire.register_count;
        let stored_domain_scalar_count = wire.domain_scalar_count;
        let program = Self::checked(
            wire.domain,
            wire.carried_count,
            wire.capture_count,
            wire.update,
        )
        .map_err(serde::de::Error::custom)?;
        if program.register_count != stored_register_count {
            return Err(serde::de::Error::custom(
                ScalarProgramRegisterError::InvalidFunctionFold {
                    op_index: 0,
                    reason: "stored update register count does not match its body",
                },
            ));
        }
        if program.domain_scalar_count != stored_domain_scalar_count {
            return Err(serde::de::Error::custom(
                ScalarProgramRegisterError::InvalidFunctionFold {
                    op_index: 0,
                    reason: "stored domain cardinality does not match its structured domain",
                },
            ));
        }
        Ok(program)
    }
}

impl FunctionFoldProgram {
    pub fn checked(
        domain: StructuredIndexDomain,
        carried_count: usize,
        capture_count: usize,
        update: Vec<LinearOp>,
    ) -> Result<Self, ScalarProgramRegisterError> {
        let domain_scalar_count = validate_function_fold_domain(&domain)?;
        let register_count = ScalarProgramRegisterFlow::derive_inner(
            &update,
            Some((carried_count, domain.binders.len(), capture_count)),
            Some(&domain),
            None,
        )?
        .register_count();
        let output_count = fold_output_count(&update, 0)?;
        if output_count != carried_count {
            return Err(ScalarProgramRegisterError::InvalidFunctionFold {
                op_index: 0,
                reason: "update output count does not match carried tuple",
            });
        }
        Ok(Self {
            domain,
            domain_scalar_count,
            carried_count,
            capture_count,
            register_count,
            update,
        })
    }

    #[must_use]
    pub const fn domain(&self) -> &StructuredIndexDomain {
        &self.domain
    }

    /// Number of points the checked structured domain traverses.
    ///
    /// The cardinality is issued by [`FunctionFoldProgram::checked`] from the
    /// same domain validation that admits the program, so a consumer reads a
    /// proved count instead of recomputing a fallible one.
    #[must_use]
    pub const fn domain_scalar_count(&self) -> usize {
        self.domain_scalar_count
    }

    #[must_use]
    pub const fn carried_count(&self) -> usize {
        self.carried_count
    }

    #[must_use]
    pub const fn capture_count(&self) -> usize {
        self.capture_count
    }

    #[must_use]
    pub const fn register_count(&self) -> usize {
        self.register_count
    }

    #[must_use]
    pub fn update(&self) -> &[LinearOp] {
        &self.update
    }
}

fn validate_function_fold_domain(
    domain: &StructuredIndexDomain,
) -> Result<usize, ScalarProgramRegisterError> {
    domain
        .validate()
        .map_err(|_| ScalarProgramRegisterError::InvalidFunctionFold {
            op_index: 0,
            reason: "function fold has an invalid structured domain",
        })
}

/// One checked condition and its lazily selected correlated result region.
///
/// Arm register capacities and bodies are issued only by the enclosing
/// [`FunctionConditionalProgram`] constructor:
///
/// ```compile_fail
/// use rumoca_ir_solve::{FunctionConditionalArmProgram, LinearOp};
///
/// fn never<T>() -> T { loop {} }
///
/// let _ = FunctionConditionalArmProgram {
///     condition_register_count: never(),
///     result_register_count: never(),
///     condition: never::<Vec<LinearOp>>(),
///     result: never::<Vec<LinearOp>>(),
/// };
/// ```
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct FunctionConditionalArmProgram {
    condition_register_count: usize,
    result_register_count: usize,
    condition: Vec<LinearOp>,
    result: Vec<LinearOp>,
}

impl FunctionConditionalArmProgram {
    #[must_use]
    pub const fn condition_register_count(&self) -> usize {
        self.condition_register_count
    }

    #[must_use]
    pub const fn result_register_count(&self) -> usize {
        self.result_register_count
    }

    #[must_use]
    pub fn condition(&self) -> &[LinearOp] {
        &self.condition
    }

    #[must_use]
    pub fn result(&self) -> &[LinearOp] {
        &self.result
    }
}

/// Compact execution owner for one DAE multi-target function conditional.
///
/// The complete ordered branch correlation cannot be forged fieldwise:
///
/// ```compile_fail
/// use rumoca_ir_solve::{FunctionConditionalArmProgram, FunctionConditionalProgram, LinearOp};
///
/// fn never<T>() -> T { loop {} }
///
/// let _ = FunctionConditionalProgram {
///     owner: never(),
///     capture_count: never(),
///     target_widths: never::<Box<[usize]>>(),
///     result_count: never(),
///     arms: never::<Box<[FunctionConditionalArmProgram]>>(),
///     fallback_register_count: never(),
///     fallback: never::<Vec<LinearOp>>(),
/// };
/// ```
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct FunctionConditionalProgram {
    owner: Option<FunctionConditionalOwnerId>,
    capture_count: usize,
    target_widths: Box<[usize]>,
    result_count: usize,
    arms: Box<[FunctionConditionalArmProgram]>,
    fallback_register_count: usize,
    fallback: Vec<LinearOp>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct FunctionConditionalArmProgramWire {
    condition_register_count: usize,
    result_register_count: usize,
    condition: Vec<LinearOp>,
    result: Vec<LinearOp>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct FunctionConditionalProgramWire {
    #[serde(deserialize_with = "deserialize_required_option")]
    owner: Option<FunctionConditionalOwnerId>,
    capture_count: usize,
    target_widths: Box<[usize]>,
    result_count: usize,
    arms: Box<[FunctionConditionalArmProgramWire]>,
    fallback_register_count: usize,
    fallback: Vec<LinearOp>,
}

impl<'de> Deserialize<'de> for FunctionConditionalProgram {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let wire = FunctionConditionalProgramWire::deserialize(deserializer)?;
        let stored_result_count = wire.result_count;
        let stored_fallback_register_count = wire.fallback_register_count;
        let stored_arm_register_counts = wire
            .arms
            .iter()
            .map(|arm| (arm.condition_register_count, arm.result_register_count))
            .collect::<Box<[_]>>();
        let arms = wire
            .arms
            .into_vec()
            .into_iter()
            .map(|arm| (arm.condition, arm.result));
        let program = Self::checked_with_owner(
            wire.owner,
            wire.capture_count,
            wire.target_widths,
            arms,
            wire.fallback,
        )
        .map_err(serde::de::Error::custom)?;
        if program.result_count != stored_result_count {
            return Err(serde::de::Error::custom(
                ScalarProgramRegisterError::InvalidFunctionConditional {
                    op_index: 0,
                    reason: "stored result width does not match the target tuple",
                },
            ));
        }
        if program.fallback_register_count != stored_fallback_register_count
            || program
                .arms
                .iter()
                .zip(stored_arm_register_counts)
                .any(|(arm, stored)| {
                    (arm.condition_register_count, arm.result_register_count) != stored
                })
        {
            return Err(serde::de::Error::custom(
                ScalarProgramRegisterError::InvalidFunctionConditional {
                    op_index: 0,
                    reason: "stored region register capacity does not match its body",
                },
            ));
        }
        Ok(program)
    }
}

/// Block-local identity issued only for an exact reusable function call frame.
///
/// Zero is not an identity. The wire mirror is the bare `u64` payload, and every
/// decoded value is routed through [`FunctionConditionalOwnerId::checked`], so a
/// private zero cannot reach conditional construction through an owned wire.
///
/// The payload is equally unreachable positionally:
///
/// ```compile_fail
/// use rumoca_ir_solve::FunctionConditionalOwnerId;
///
/// let _ = FunctionConditionalOwnerId(0);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
#[serde(transparent)]
pub struct FunctionConditionalOwnerId(u64);

impl<'de> Deserialize<'de> for FunctionConditionalOwnerId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let raw = u64::deserialize(deserializer)?;
        Self::checked(raw).ok_or_else(|| {
            serde::de::Error::custom(ScalarProgramRegisterError::InvalidFunctionConditional {
                op_index: 0,
                reason: "owner identity is zero",
            })
        })
    }
}

impl FunctionConditionalOwnerId {
    pub const fn checked(raw: u64) -> Option<Self> {
        if raw == 0 { None } else { Some(Self(raw)) }
    }

    pub const fn get(self) -> u64 {
        self.0
    }
}

impl FunctionConditionalProgram {
    pub fn checked(
        capture_count: usize,
        target_widths: impl Into<Box<[usize]>>,
        arms: impl IntoIterator<Item = (Vec<LinearOp>, Vec<LinearOp>)>,
        fallback: Vec<LinearOp>,
    ) -> Result<Self, ScalarProgramRegisterError> {
        Self::checked_with_owner(None, capture_count, target_widths, arms, fallback)
    }

    pub fn checked_owned(
        owner: FunctionConditionalOwnerId,
        capture_count: usize,
        target_widths: impl Into<Box<[usize]>>,
        arms: impl IntoIterator<Item = (Vec<LinearOp>, Vec<LinearOp>)>,
        fallback: Vec<LinearOp>,
    ) -> Result<Self, ScalarProgramRegisterError> {
        Self::checked_with_owner(Some(owner), capture_count, target_widths, arms, fallback)
    }

    fn checked_with_owner(
        owner: Option<FunctionConditionalOwnerId>,
        capture_count: usize,
        target_widths: impl Into<Box<[usize]>>,
        arms: impl IntoIterator<Item = (Vec<LinearOp>, Vec<LinearOp>)>,
        fallback: Vec<LinearOp>,
    ) -> Result<Self, ScalarProgramRegisterError> {
        let target_widths = target_widths.into();
        if target_widths.is_empty() || target_widths.contains(&0) {
            return Err(ScalarProgramRegisterError::InvalidFunctionConditional {
                op_index: 0,
                reason: "target tuple is empty or contains an empty value",
            });
        }
        let result_count = target_widths.iter().try_fold(0usize, |count, width| {
            count.checked_add(*width).ok_or(
                ScalarProgramRegisterError::InvalidFunctionConditional {
                    op_index: 0,
                    reason: "target tuple width overflows",
                },
            )
        })?;
        let arms = arms
            .into_iter()
            .map(|(condition, result)| {
                let condition_register_count = ScalarProgramRegisterFlow::derive_inner(
                    &condition,
                    None,
                    None,
                    Some(capture_count),
                )?
                .register_count();
                require_conditional_output_count(&condition, 1, "condition")?;
                let result_register_count = ScalarProgramRegisterFlow::derive_inner(
                    &result,
                    None,
                    None,
                    Some(capture_count),
                )?
                .register_count();
                require_conditional_output_count(&result, result_count, "branch result")?;
                Ok(FunctionConditionalArmProgram {
                    condition_register_count,
                    result_register_count,
                    condition,
                    result,
                })
            })
            .collect::<Result<Vec<_>, ScalarProgramRegisterError>>()?
            .into_boxed_slice();
        if arms.is_empty() {
            return Err(ScalarProgramRegisterError::InvalidFunctionConditional {
                op_index: 0,
                reason: "ordered condition list is empty",
            });
        }
        let fallback_register_count =
            ScalarProgramRegisterFlow::derive_inner(&fallback, None, None, Some(capture_count))?
                .register_count();
        require_conditional_output_count(&fallback, result_count, "fallback result")?;
        Ok(Self {
            owner,
            capture_count,
            target_widths,
            result_count,
            arms,
            fallback_register_count,
            fallback,
        })
    }

    #[must_use]
    pub const fn owner(&self) -> Option<FunctionConditionalOwnerId> {
        self.owner
    }

    #[must_use]
    pub const fn capture_count(&self) -> usize {
        self.capture_count
    }

    #[must_use]
    pub fn target_widths(&self) -> &[usize] {
        &self.target_widths
    }

    #[must_use]
    pub const fn result_count(&self) -> usize {
        self.result_count
    }

    #[must_use]
    pub fn arms(&self) -> &[FunctionConditionalArmProgram] {
        &self.arms
    }

    #[must_use]
    pub const fn fallback_register_count(&self) -> usize {
        self.fallback_register_count
    }

    #[must_use]
    pub fn fallback(&self) -> &[LinearOp] {
        &self.fallback
    }
}

fn require_conditional_output_count(
    program: &[LinearOp],
    expected: usize,
    region: &'static str,
) -> Result<(), ScalarProgramRegisterError> {
    let actual = fold_output_count(program, 0)?;
    if actual != expected {
        return Err(ScalarProgramRegisterError::InvalidFunctionConditional {
            op_index: 0,
            reason: match region {
                "condition" => "condition region must produce exactly one value",
                "branch result" => "branch region does not completely define the result tuple",
                _ => "fallback region does not completely define the result tuple",
            },
        });
    }
    Ok(())
}

fn fold_output_count(
    update: &[LinearOp],
    op_index: usize,
) -> Result<usize, ScalarProgramRegisterError> {
    update.iter().try_fold(0usize, |count, op| {
        let arity = match op {
            LinearOp::StoreOutput { .. } => 1,
            LinearOp::StoreOutputFoldTensorUpdate {
                source_base,
                dimensions,
                lanes,
                ..
            } => {
                if *source_base != count {
                    return Err(ScalarProgramRegisterError::InvalidFunctionFold {
                        op_index,
                        reason: "aggregate update source must be its matching carried output",
                    });
                }
                tensor_scalar_count(op_index, dimensions.as_ref())?
                    .checked_mul(*lanes)
                    .ok_or(ScalarProgramRegisterError::InvalidFunctionFold {
                        op_index,
                        reason: "aggregate update output count overflows",
                    })?
            }
            LinearOp::StoreOutputFunctionFold { count: output, .. } => *output,
            LinearOp::StoreOutputRange { count, .. } => *count,
            _ => 0,
        };
        count
            .checked_add(arity)
            .ok_or(ScalarProgramRegisterError::InvalidFunctionFold {
                op_index,
                reason: "update output count overflows",
            })
    })
}

/// Linear register-based operation stream.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TensorConcatenateSource {
    pub start: Reg,
    pub dimensions: Box<[u32]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TensorInputKind {
    Y,
    P,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LinearOp {
    Const {
        dst: Reg,
        value: f64,
    },
    LoadTime {
        dst: Reg,
    },
    LoadY {
        dst: Reg,
        index: usize,
    },
    LoadP {
        dst: Reg,
        index: usize,
    },
    /// Project one scalar from a packed tensor register range.
    ///
    /// The tensor remains one compact owner: `dimensions` and `indices` carry
    /// rank-sized affine addressing metadata, never an extent-sized candidate
    /// or select list. Backends lower this operation directly at their final
    /// execution/rendering boundary.
    LoadIndexedRegister {
        dst: Reg,
        base: Reg,
        stride: usize,
        dimensions: Box<[u32]>,
        indices: Box<[TensorIndex]>,
    },
    /// Project one scalar directly from the current aggregate carried by a
    /// [`FunctionFoldProgram`], without scalar packing or candidate expansion.
    LoadIndexedFoldCarried {
        dst: Reg,
        base: usize,
        stride: usize,
        dimensions: Box<[u32]>,
        indices: Box<[TensorIndex]>,
    },
    /// Project one scalar directly from the immutable capture tuple of the
    /// current [`FunctionFoldProgram`]. The capture range stays compact and is
    /// never repacked into intermediate scalar registers.
    LoadIndexedFoldCapture {
        dst: Reg,
        base: usize,
        stride: usize,
        dimensions: Box<[u32]>,
        indices: Box<[TensorIndex]>,
    },
    /// Load AD seed for a state/algebraic/output scalar from `v[]`.
    LoadSeed {
        dst: Reg,
        index: usize,
    },
    /// Load one current carried value inside a [`FunctionFoldProgram`] update.
    LoadFoldCarried {
        dst: Reg,
        index: usize,
    },
    /// Load one Modelica binder coordinate inside a function-fold update.
    LoadFoldIndex {
        dst: Reg,
        dimension: usize,
    },
    /// Load one immutable value captured from an enclosing function-fold body.
    LoadFoldCapture {
        dst: Reg,
        index: usize,
    },
    /// Load one immutable value from the enclosing function-conditional ABI.
    LoadFunctionConditionalCapture {
        dst: Reg,
        index: usize,
    },
    /// Load one compact consecutive range from the enclosing
    /// function-conditional ABI.
    LoadFunctionConditionalCaptureRange {
        dst_start: Reg,
        index_start: usize,
        count: usize,
    },
    /// Copy a register value. This keeps packed register ranges explicit
    /// without introducing expression-level aliases into solver IR.
    Move {
        dst: Reg,
        src: Reg,
    },
    /// Solve one component of a dense linear system `A * x = b`.
    ///
    /// `matrix_start..matrix_start+n*n` stores row-major `A`;
    /// `rhs_start..rhs_start+n` stores `b`; `component` selects `x[component]`.
    LinearSolveComponent {
        dst: Reg,
        matrix_start: Reg,
        rhs_start: Reg,
        n: usize,
        component: usize,
    },
    /// One strided dot product over two packed register ranges.
    DotProduct {
        dst: Reg,
        lhs_start: Reg,
        rhs_start: Reg,
        count: usize,
        lhs_stride: usize,
        rhs_stride: usize,
    },
    /// Multiply two dense row-major tensors as one aggregate owner.
    ///
    /// Its extents and storage width are the fields of
    /// [`MatrixProductShape`]. The operation count is independent of the
    /// matrix extents.
    MatrixMultiply {
        dst_start: Reg,
        lhs_start: Reg,
        rhs_start: Reg,
        rows: usize,
        inner: usize,
        columns: usize,
        lanes: usize,
    },
    /// Apply one elementwise binary operator to compact tensor ranges.
    /// A zero source stride broadcasts one scalar across the output range.
    TensorBinary {
        dst_start: Reg,
        op: BinaryOp,
        lhs_start: Reg,
        rhs_start: Reg,
        count: usize,
        lhs_stride: usize,
        rhs_stride: usize,
        lanes: usize,
    },
    /// Compute the cross product of two length-three vectors as one owner.
    /// Storage is element-major with one primal or two interleaved AD lanes.
    TensorCross {
        dst_start: Reg,
        lhs_start: Reg,
        rhs_start: Reg,
        lanes: usize,
    },
    /// Transpose the first two dimensions of one dense row-major tensor.
    ///
    /// `rows` and `columns` describe the first two dimensions of the output,
    /// `element_width` is the product of all untouched trailing dimensions,
    /// and `lanes` independently describes primal or interleaved dual values.
    TensorTranspose {
        dst_start: Reg,
        src_start: Reg,
        rows: usize,
        columns: usize,
        element_width: usize,
        lanes: usize,
    },
    /// Concatenate dense row-major tensors along one axis as one aggregate owner.
    TensorConcatenate {
        dst_start: Reg,
        sources: Box<[TensorConcatenateSource]>,
        dimensions: Box<[u32]>,
        axis: usize,
        lanes: usize,
    },
    /// Apply one shape-preserving array patch without enumerating coordinates.
    TensorUpdate {
        dst_start: Reg,
        base_start: Reg,
        value_start: Reg,
        dimensions: Box<[u32]>,
        subscripts: Box<[TensorUpdateSubscript]>,
        lanes: usize,
    },
    /// Replicate one scalar (or one interleaved dual scalar) across a tensor.
    TensorFill {
        dst_start: Reg,
        value_start: Reg,
        count: usize,
        lanes: usize,
    },
    /// Construct a square identity tensor, including zero tangent lanes.
    TensorIdentity {
        dst_start: Reg,
        size: usize,
        lanes: usize,
    },
    /// Load one contiguous runtime tensor, optionally interleaving AD seeds.
    TensorLoad {
        dst_start: Reg,
        input: TensorInputKind,
        input_start: usize,
        count: usize,
        #[serde(deserialize_with = "deserialize_required_option")]
        seed_start: Option<usize>,
        lanes: usize,
    },
    /// Host-backed table bound lookup (`*_Tmin`, `*_Tmax`, `*_AbscissaUmin`, `*_AbscissaUmax`).
    TableBounds {
        dst: Reg,
        table_id: Reg,
        max: bool,
    },
    /// Host-backed table lookup (`getTimeTableValue*`, `getTable1DValue*`).
    TableLookup {
        dst: Reg,
        table_id: Reg,
        column: Reg,
        input: Reg,
    },
    /// Host-backed table lookup slope d(lookup)/d(input) for AD rows.
    TableLookupSlope {
        dst: Reg,
        table_id: Reg,
        column: Reg,
        input: Reg,
    },
    /// Host-backed table next-event lookup (`getNextTimeEvent`).
    TableNextEvent {
        dst: Reg,
        table_id: Reg,
        time: Reg,
    },
    /// Deterministic random state initialization for MSL Xorshift generators.
    RandomInitialState {
        dst: Reg,
        generator: RandomGenerator,
        local_seed: Reg,
        global_seed: Reg,
        state_len: usize,
        state_index: usize,
    },
    /// Deterministic random sample in `(0, 1]` from an input state vector.
    RandomResult {
        dst: Reg,
        generator: RandomGenerator,
        state_start: Reg,
        state_len: usize,
    },
    /// Deterministic random output state component from an input state vector.
    RandomState {
        dst: Reg,
        generator: RandomGenerator,
        state_start: Reg,
        state_len: usize,
        state_index: usize,
    },
    /// Initialize an MSL impure random stream and return its stream id.
    ImpureRandomInit {
        dst: Reg,
        seed: Reg,
    },
    /// Draw one MSL impure random sample in `(0, 1]`.
    ImpureRandom {
        dst: Reg,
        id: Reg,
        call_site: u64,
    },
    /// Draw one MSL impure random integer sample in `[imin, imax]`.
    ImpureRandomInteger {
        dst: Reg,
        id: Reg,
        imin: Reg,
        imax: Reg,
        call_site: u64,
    },
    Unary {
        dst: Reg,
        op: UnaryOp,
        arg: Reg,
    },
    Binary {
        dst: Reg,
        op: BinaryOp,
        lhs: Reg,
        rhs: Reg,
    },
    Compare {
        dst: Reg,
        op: CompareOp,
        lhs: Reg,
        rhs: Reg,
    },
    Select {
        dst: Reg,
        cond: Reg,
        if_true: Reg,
        if_false: Reg,
    },
    /// Execute one compact finite-domain fold and write a consecutive carried tuple.
    FunctionFold {
        dst_start: Reg,
        initial_start: Reg,
        capture_start: Reg,
        program: Arc<FunctionFoldProgram>,
    },
    /// Execute one compact finite-domain fold only when `activation` is true.
    /// The inactive result is the initial carried tuple, preserving lazy
    /// Modelica algorithm-branch semantics without scalarizing either arm.
    GuardedFunctionFold {
        dst_start: Reg,
        initial_start: Reg,
        capture_start: Reg,
        activation: Reg,
        program: Arc<FunctionFoldProgram>,
    },
    /// Evaluate ordered conditions and only the first matching correlated result region.
    FunctionConditional {
        dst_start: Reg,
        capture_start: Reg,
        program: Arc<FunctionConditionalProgram>,
    },
    /// Invoke one issued model-level typed pure-call owner.
    ///
    /// Each entry of `input_starts` names one compact register range whose
    /// width comes from the matching typed input in `site`. Ordered value
    /// results and assertion predicates are written consecutively from
    /// `dst_start`. No function body or scalar-coordinate catalog is embedded.
    PureCall {
        dst_start: Reg,
        input_starts: Box<[Reg]>,
        site: SolvePureCallSite,
    },
    /// Invoke the checked compact directional relation of one issued typed
    /// pure-call owner. Inputs and outputs use its aggregate primal/tangent
    /// ABI; no function body or tensor coordinate is embedded here.
    PureCallDirectional {
        dst_start: Reg,
        input_starts: Box<[Reg]>,
        site: SolvePureCallDirectionalSite,
    },
    /// Produce one aggregate carried output by applying a compact tensor
    /// update to the current carried tuple.
    ///
    /// Values use element-major lane layout. This emits
    /// `product(dimensions) * lanes` consecutive fold outputs while its IR
    /// footprint remains rank-sized. Ordinary lowering uses one lane; forward
    /// AD uses interleaved real/tangent lanes.
    StoreOutputFoldTensorUpdate {
        source_base: usize,
        source_stride: usize,
        dimensions: Box<[u32]>,
        /// Ordered aggregate patches. Each patch is rank-sized regardless of
        /// the carried tensor extent.
        updates: Box<[FoldTensorUpdate]>,
        nodes: Box<[FoldTensorNode]>,
        result: u32,
        lanes: usize,
    },
    /// Execute a nested fold from compact register/parent-carried initial
    /// sources and project one consecutive carried value directly as this
    /// fold's aggregate output.
    StoreOutputFunctionFold {
        initial: Box<[FoldInitialSource]>,
        capture_start: Reg,
        program: Arc<FunctionFoldProgram>,
        result_base: usize,
        count: usize,
        #[serde(deserialize_with = "deserialize_required_option")]
        condition: Option<Reg>,
        nested_when_true: bool,
    },
    /// Project a compact affine register range as consecutive program outputs.
    ///
    /// `count` outputs read `start + ordinal * stride`. This is the canonical
    /// result boundary for tensor/record values; scalar target instructions
    /// are materialized only by a final execution or rendering backend.
    StoreOutputRange {
        start: Reg,
        count: usize,
        stride: usize,
    },
    /// Marks final row value for residual output.
    StoreOutput {
        src: Reg,
    },
}

impl LinearOp {
    #[must_use]
    pub fn kind_name(&self) -> &'static str {
        match self {
            Self::Const { .. } => "Const",
            Self::LoadTime { .. } => "LoadTime",
            Self::LoadY { .. } => "LoadY",
            Self::LoadP { .. } => "LoadP",
            Self::LoadIndexedRegister { .. } => "LoadIndexedRegister",
            Self::LoadIndexedFoldCarried { .. } => "LoadIndexedFoldCarried",
            Self::LoadIndexedFoldCapture { .. } => "LoadIndexedFoldCapture",
            Self::LoadSeed { .. } => "LoadSeed",
            Self::LoadFoldCarried { .. } => "LoadFoldCarried",
            Self::LoadFoldIndex { .. } => "LoadFoldIndex",
            Self::LoadFoldCapture { .. } => "LoadFoldCapture",
            Self::LoadFunctionConditionalCapture { .. } => "LoadFunctionConditionalCapture",
            Self::LoadFunctionConditionalCaptureRange { .. } => {
                "LoadFunctionConditionalCaptureRange"
            }
            Self::Move { .. } => "Move",
            Self::LinearSolveComponent { .. } => "LinearSolveComponent",
            Self::DotProduct { .. } => "DotProduct",
            Self::MatrixMultiply { .. } => "MatrixMultiply",
            Self::TensorBinary { .. } => "TensorBinary",
            Self::TensorCross { .. } => "TensorCross",
            Self::TensorTranspose { .. } => "TensorTranspose",
            Self::TensorConcatenate { .. } => "TensorConcatenate",
            Self::TensorUpdate { .. } => "TensorUpdate",
            Self::TensorFill { .. } => "TensorFill",
            Self::TensorIdentity { .. } => "TensorIdentity",
            Self::TensorLoad { .. } => "TensorLoad",
            Self::TableBounds { .. } => "TableBounds",
            Self::TableLookup { .. } => "TableLookup",
            Self::TableLookupSlope { .. } => "TableLookupSlope",
            Self::TableNextEvent { .. } => "TableNextEvent",
            Self::RandomInitialState { .. } => "RandomInitialState",
            Self::RandomResult { .. } => "RandomResult",
            Self::RandomState { .. } => "RandomState",
            Self::ImpureRandomInit { .. } => "ImpureRandomInit",
            Self::ImpureRandom { .. } => "ImpureRandom",
            Self::ImpureRandomInteger { .. } => "ImpureRandomInteger",
            Self::Unary { .. } => "Unary",
            Self::Binary { .. } => "Binary",
            Self::Compare { .. } => "Compare",
            Self::Select { .. } => "Select",
            Self::FunctionFold { .. } => "FunctionFold",
            Self::GuardedFunctionFold { .. } => "GuardedFunctionFold",
            Self::FunctionConditional { .. } => "FunctionConditional",
            Self::PureCall { .. } => "PureCall",
            Self::PureCallDirectional { .. } => "PureCallDirectional",
            Self::StoreOutputFoldTensorUpdate { .. } => "StoreOutputFoldTensorUpdate",
            Self::StoreOutputFunctionFold { .. } => "StoreOutputFunctionFold",
            Self::StoreOutputRange { .. } => "StoreOutputRange",
            Self::StoreOutput { .. } => "StoreOutput",
        }
    }

    pub fn dst_register(&self) -> Option<Reg> {
        match self {
            Self::Const { dst, .. }
            | Self::LoadTime { dst }
            | Self::LoadY { dst, .. }
            | Self::LoadP { dst, .. }
            | Self::LoadIndexedRegister { dst, .. }
            | Self::LoadIndexedFoldCarried { dst, .. }
            | Self::LoadIndexedFoldCapture { dst, .. }
            | Self::LoadSeed { dst, .. }
            | Self::LoadFoldCarried { dst, .. }
            | Self::LoadFoldIndex { dst, .. }
            | Self::LoadFoldCapture { dst, .. }
            | Self::LoadFunctionConditionalCapture { dst, .. }
            | Self::Move { dst, .. }
            | Self::LinearSolveComponent { dst, .. }
            | Self::DotProduct { dst, .. }
            | Self::TableBounds { dst, .. }
            | Self::TableLookup { dst, .. }
            | Self::TableLookupSlope { dst, .. }
            | Self::TableNextEvent { dst, .. }
            | Self::RandomInitialState { dst, .. }
            | Self::RandomResult { dst, .. }
            | Self::RandomState { dst, .. }
            | Self::ImpureRandomInit { dst, .. }
            | Self::ImpureRandom { dst, .. }
            | Self::ImpureRandomInteger { dst, .. }
            | Self::Unary { dst, .. }
            | Self::Binary { dst, .. }
            | Self::Compare { dst, .. }
            | Self::Select { dst, .. } => Some(*dst),
            Self::LoadFunctionConditionalCaptureRange { dst_start, .. }
            | Self::FunctionFold { dst_start, .. }
            | Self::GuardedFunctionFold { dst_start, .. }
            | Self::FunctionConditional { dst_start, .. }
            | Self::PureCall { dst_start, .. }
            | Self::PureCallDirectional { dst_start, .. }
            | Self::MatrixMultiply { dst_start, .. }
            | Self::TensorBinary { dst_start, .. }
            | Self::TensorCross { dst_start, .. }
            | Self::TensorTranspose { dst_start, .. } => Some(*dst_start),
            Self::TensorConcatenate { dst_start, .. }
            | Self::TensorUpdate { dst_start, .. }
            | Self::TensorFill { dst_start, .. }
            | Self::TensorIdentity { dst_start, .. }
            | Self::TensorLoad { dst_start, .. } => Some(*dst_start),
            Self::StoreOutputFoldTensorUpdate { .. }
            | Self::StoreOutputFunctionFold { .. }
            | Self::StoreOutputRange { .. }
            | Self::StoreOutput { .. } => None,
        }
    }

    /// Number of contiguous registers written beginning at [`Self::dst_register`].
    ///
    /// Aggregate operations retain their compact shape in one operation, so
    /// consumers must not assume that every operation writes one scalar.
    #[must_use]
    pub fn dst_register_count(&self) -> usize {
        match self {
            Self::LoadFunctionConditionalCaptureRange { count, .. } => *count,
            Self::FunctionFold { program, .. } | Self::GuardedFunctionFold { program, .. } => {
                program.carried_count
            }
            Self::FunctionConditional { program, .. } => program.result_count,
            Self::PureCall { site, .. } => site.output_scalar_count().unwrap_or(usize::MAX),
            Self::PureCallDirectional { site, .. } => {
                site.output_scalar_count().unwrap_or(usize::MAX)
            }
            Self::MatrixMultiply {
                rows,
                columns,
                lanes,
                ..
            } => rows.saturating_mul(*columns).saturating_mul(*lanes),
            Self::TensorTranspose {
                rows,
                columns,
                element_width,
                lanes,
                ..
            } => rows
                .saturating_mul(*columns)
                .saturating_mul(*element_width)
                .saturating_mul(*lanes),
            Self::TensorBinary { count, lanes, .. }
            | Self::TensorFill { count, lanes, .. }
            | Self::TensorLoad { count, lanes, .. } => count.saturating_mul(*lanes),
            Self::TensorCross { lanes, .. } => 3usize.saturating_mul(*lanes),
            Self::TensorConcatenate {
                dimensions, lanes, ..
            }
            | Self::TensorUpdate {
                dimensions, lanes, ..
            } => dimensions.iter().fold(*lanes, |count, extent| {
                count.saturating_mul(*extent as usize)
            }),
            Self::TensorIdentity { size, lanes, .. } => {
                size.saturating_mul(*size).saturating_mul(*lanes)
            }
            Self::StoreOutputFoldTensorUpdate { .. }
            | Self::StoreOutputFunctionFold { .. }
            | Self::StoreOutputRange { .. }
            | Self::StoreOutput { .. } => 0,
            _ => 1,
        }
    }
}

/// Constructor proof that every register read in one scalar program is
/// dominated by an earlier write.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScalarProgramRegisterFlow {
    register_count: usize,
}

impl ScalarProgramRegisterFlow {
    pub fn derive(program: &[LinearOp]) -> Result<Self, ScalarProgramRegisterError> {
        Self::derive_inner(program, None, None, None)
    }

    fn derive_inner(
        program: &[LinearOp],
        fold_context: Option<(usize, usize, usize)>,
        fold_domain: Option<&StructuredIndexDomain>,
        conditional_capture_count: Option<usize>,
    ) -> Result<Self, ScalarProgramRegisterError> {
        let mut derivation = RegisterFlowDerivation::default();
        for (op_index, op) in program.iter().enumerate() {
            derivation.observe_sources(op, op_index, fold_context, conditional_capture_count)?;
            validate_runtime_index_evidence(op, op_index, &derivation.index_evidence)?;
            derivation.issue_destination(op, op_index, fold_domain)?;
        }
        Ok(Self {
            register_count: checked_register_count(derivation.max_register)?,
        })
    }

    pub const fn register_count(self) -> usize {
        self.register_count
    }
}

/// Working state of one scalar program's register-flow derivation.
///
/// The three facts advance together for every operation: which registers are
/// already written, the exact-integer domain each register currently carries,
/// and the highest register the program names.
#[derive(Default)]
struct RegisterFlowDerivation {
    initialized: Vec<bool>,
    index_evidence: Vec<Option<ExactIntegerEvidence>>,
    max_register: Option<Reg>,
}

impl RegisterFlowDerivation {
    /// Check that one operation reads only registers an earlier write
    /// dominates, and record the highest register it reads.
    fn observe_sources(
        &mut self,
        op: &LinearOp,
        op_index: usize,
        fold_context: Option<(usize, usize, usize)>,
        conditional_capture_count: Option<usize>,
    ) -> Result<(), ScalarProgramRegisterError> {
        if let Some(register) = validate_op_sources(
            op,
            op_index,
            &self.initialized,
            fold_context,
            conditional_capture_count,
        )? {
            self.observe_register(register);
        }
        Ok(())
    }

    /// Claim one operation's destination range and issue the exact-integer
    /// domain that range now carries.
    fn issue_destination(
        &mut self,
        op: &LinearOp,
        op_index: usize,
        fold_domain: Option<&StructuredIndexDomain>,
    ) -> Result<(), ScalarProgramRegisterError> {
        let Some(dst) = op.dst_register() else {
            return Ok(());
        };
        let dst_count = op.dst_register_count();
        let last = register_range_last(op_index, op.kind_name(), dst, dst_count)?;
        require_register_range_uninitialized(
            op_index,
            op.kind_name(),
            dst,
            dst_count,
            &self.initialized,
        )?;
        let exact_integer = (dst_count == 1).then(|| {
            derive_exact_integer_evidence(op, op_index, &self.index_evidence, fold_domain)
        });
        mark_register_range_initialized(&mut self.initialized, dst, dst_count);
        mark_exact_integer_evidence_unknown(&mut self.index_evidence, dst, dst_count);
        if let Some(exact_integer) = exact_integer {
            self.index_evidence[dst as usize] = exact_integer;
        }
        self.observe_register(last);
        Ok(())
    }

    fn observe_register(&mut self, register: Reg) {
        self.max_register = Some(
            self.max_register
                .map_or(register, |max: Reg| max.max(register)),
        );
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScalarProgramRegisterError {
    UndefinedRegister {
        op_index: usize,
        operation: &'static str,
        register: Reg,
    },
    EmptyRegisterRange {
        op_index: usize,
        operation: &'static str,
    },
    RegisterRangeOverflow {
        op_index: usize,
        operation: &'static str,
        start: Reg,
        len: usize,
    },
    DestinationRegisterAlreadyDefined {
        op_index: usize,
        operation: &'static str,
        register: Reg,
    },
    InvalidProjection {
        op_index: usize,
        operation: &'static str,
        projection: usize,
        len: usize,
    },
    RegisterCountOverflow {
        register: Reg,
    },
    InvalidFunctionFold {
        op_index: usize,
        reason: &'static str,
    },
    InvalidFunctionConditional {
        op_index: usize,
        reason: &'static str,
    },
    InvalidPureCall {
        op_index: usize,
        reason: &'static str,
    },
    InvalidTensorProjection {
        op_index: usize,
        reason: &'static str,
    },
}

/// Closed interval of exactly representable integer values.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct ExactIntegerInterval {
    lower: i64,
    upper: i64,
}

impl ExactIntegerInterval {
    const MAX_EXACT_F64_INTEGER: i64 = 1_i64 << 53;

    fn exact(value: f64) -> Option<Self> {
        let integer = value as i64;
        (value.is_finite()
            && value.fract() == 0.0
            && value.abs() <= Self::MAX_EXACT_F64_INTEGER as f64
            && integer as f64 == value)
            .then_some(Self {
                lower: integer,
                upper: integer,
            })
    }

    fn checked(lower: i64, upper: i64) -> Option<Self> {
        (lower <= upper
            && lower >= -Self::MAX_EXACT_F64_INTEGER
            && upper <= Self::MAX_EXACT_F64_INTEGER)
            .then_some(Self { lower, upper })
    }

    fn join(self, other: Self) -> Option<Self> {
        Self::checked(self.lower.min(other.lower), self.upper.max(other.upper))
    }

    fn neg(self) -> Option<Self> {
        Self::checked(self.upper.checked_neg()?, self.lower.checked_neg()?)
    }

    fn abs(self) -> Option<Self> {
        let upper = self.lower.unsigned_abs().max(self.upper.unsigned_abs());
        let upper = i64::try_from(upper).ok()?;
        Self::checked(
            if self.lower <= 0 && self.upper >= 0 {
                0
            } else {
                self.lower.abs().min(self.upper.abs())
            },
            upper,
        )
    }

    fn add(self, other: Self) -> Option<Self> {
        Self::checked(
            self.lower.checked_add(other.lower)?,
            self.upper.checked_add(other.upper)?,
        )
    }

    fn sub(self, other: Self) -> Option<Self> {
        Self::checked(
            self.lower.checked_sub(other.upper)?,
            self.upper.checked_sub(other.lower)?,
        )
    }

    fn mul(self, other: Self) -> Option<Self> {
        let products = [
            self.lower.checked_mul(other.lower)?,
            self.lower.checked_mul(other.upper)?,
            self.upper.checked_mul(other.lower)?,
            self.upper.checked_mul(other.upper)?,
        ];
        Self::checked(*products.iter().min()?, *products.iter().max()?)
    }
}

/// Identity of the operation that most recently produced one register.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct RegisterProducer {
    operation: usize,
    register: Reg,
}

/// Persistent construction-local citation chain for exact-integer evidence.
///
/// Each derived node retains the exact producer nodes it consumed. Since only
/// this module constructs a node, checking its direct parents proves the full
/// chain inductively without rescanning it for every indexed operation.
#[derive(Clone, Debug)]
struct ExactIntegerProvenance {
    producer: RegisterProducer,
    origin: ExactIntegerOrigin,
}

#[derive(Clone, Debug)]
enum ExactIntegerOrigin {
    ExactConstant,
    CheckedFoldBinder,
    IntegerPreserving {
        sources: Box<[Rc<ExactIntegerProvenance>]>,
    },
}

impl ExactIntegerProvenance {
    fn exact_constant(producer: RegisterProducer) -> Rc<Self> {
        Rc::new(Self {
            producer,
            origin: ExactIntegerOrigin::ExactConstant,
        })
    }

    fn checked_fold_binder(producer: RegisterProducer) -> Rc<Self> {
        Rc::new(Self {
            producer,
            origin: ExactIntegerOrigin::CheckedFoldBinder,
        })
    }

    fn derived(
        producer: RegisterProducer,
        sources: impl IntoIterator<Item = Rc<Self>>,
    ) -> Option<Rc<Self>> {
        let sources = sources.into_iter().collect::<Box<[_]>>();
        if sources.is_empty()
            || sources
                .iter()
                .any(|source| source.producer.operation >= producer.operation)
        {
            return None;
        }
        Some(Rc::new(Self {
            producer,
            origin: ExactIntegerOrigin::IntegerPreserving { sources },
        }))
    }

    fn cites(&self, producer: RegisterProducer) -> bool {
        if self.producer != producer {
            return false;
        }
        match &self.origin {
            ExactIntegerOrigin::ExactConstant | ExactIntegerOrigin::CheckedFoldBinder => true,
            ExactIntegerOrigin::IntegerPreserving { sources } => !sources.is_empty(),
        }
    }
}

/// Construction-local proof that one register is always an exactly
/// representable integer in one closed interval, citing its current producer
/// and the complete persistent producer chain.
#[derive(Clone, Debug)]
struct ExactIntegerEvidence {
    interval: ExactIntegerInterval,
    provenance: Rc<ExactIntegerProvenance>,
}

impl ExactIntegerEvidence {
    fn exact_constant(interval: ExactIntegerInterval, producer: RegisterProducer) -> Self {
        Self {
            interval,
            provenance: ExactIntegerProvenance::exact_constant(producer),
        }
    }

    fn checked_fold_binder(interval: ExactIntegerInterval, producer: RegisterProducer) -> Self {
        Self {
            interval,
            provenance: ExactIntegerProvenance::checked_fold_binder(producer),
        }
    }

    fn derived(
        interval: ExactIntegerInterval,
        producer: RegisterProducer,
        sources: impl IntoIterator<Item = ExactIntegerEvidence>,
    ) -> Option<Self> {
        let provenance = ExactIntegerProvenance::derived(
            producer,
            sources.into_iter().map(|source| source.provenance),
        )?;
        Some(Self {
            interval,
            provenance,
        })
    }

    fn cited_for(&self, register: Reg) -> bool {
        self.provenance.cites(RegisterProducer {
            operation: self.provenance.producer.operation,
            register,
        })
    }
}

/// Branded evidence for a one-based Modelica tensor coordinate.
struct OneBasedTensorCoordinateEvidence;

impl OneBasedTensorCoordinateEvidence {
    fn issue(evidence: ExactIntegerEvidence, register: Reg, extent: u32) -> Option<Self> {
        (evidence.cited_for(register)
            && evidence.interval.lower >= 1
            && evidence.interval.upper <= i64::from(extent))
        .then_some(Self)
    }
}

fn mark_exact_integer_evidence_unknown(
    evidence: &mut Vec<Option<ExactIntegerEvidence>>,
    start: Reg,
    count: usize,
) {
    let end = start as usize + count;
    if evidence.len() < end {
        evidence.resize(end, None);
    }
    evidence[start as usize..end].fill(None);
}

/// Exact-integer evidence currently carried by each register, readable only
/// through the citation check that keeps a stale entry from being consumed.
#[derive(Clone, Copy)]
struct ExactIntegerSources<'a> {
    evidence: &'a [Option<ExactIntegerEvidence>],
}

impl ExactIntegerSources<'_> {
    fn get(self, register: Reg) -> Option<ExactIntegerEvidence> {
        self.evidence
            .get(register as usize)
            .cloned()
            .flatten()
            .filter(|source| source.cited_for(register))
    }
}

fn derive_exact_integer_evidence(
    op: &LinearOp,
    op_index: usize,
    evidence: &[Option<ExactIntegerEvidence>],
    fold_domain: Option<&StructuredIndexDomain>,
) -> Option<ExactIntegerEvidence> {
    let dst = op.dst_register()?;
    let producer = RegisterProducer {
        operation: op_index,
        register: dst,
    };
    let sources = ExactIntegerSources { evidence };
    match *op {
        LinearOp::Const { value, .. } => Some(ExactIntegerEvidence::exact_constant(
            ExactIntegerInterval::exact(value)?,
            producer,
        )),
        LinearOp::LoadFoldIndex { dimension, .. } => {
            fold_binder_evidence(fold_domain?, dimension, producer)
        }
        LinearOp::Move { src, .. } => {
            let source = sources.get(src)?;
            ExactIntegerEvidence::derived(source.interval, producer, [source])
        }
        LinearOp::Unary { op, arg, .. } => unary_evidence(op, sources.get(arg)?, producer),
        LinearOp::Binary { op, lhs, rhs, .. } => {
            binary_evidence(op, sources.get(lhs)?, sources.get(rhs)?, producer)
        }
        LinearOp::Compare { .. } => None,
        LinearOp::Select {
            if_true, if_false, ..
        } => {
            let if_true = sources.get(if_true)?;
            let if_false = sources.get(if_false)?;
            ExactIntegerEvidence::derived(
                if_true.interval.join(if_false.interval)?,
                producer,
                [if_true, if_false],
            )
        }
        _ => None,
    }
}

/// Exact-integer domain of one fold binder read: the binder's own closed
/// value range, oriented low to high.
fn fold_binder_evidence(
    domain: &StructuredIndexDomain,
    dimension: usize,
    producer: RegisterProducer,
) -> Option<ExactIntegerEvidence> {
    let binder = domain.binders.get(dimension)?;
    Some(ExactIntegerEvidence::checked_fold_binder(
        ExactIntegerInterval::checked(
            binder.lower.min(binder.upper),
            binder.lower.max(binder.upper),
        )?,
        producer,
    ))
}

/// Exact-integer domain one unary operation preserves. Operations that leave
/// the integers, and the boolean negation, carry no domain forward.
fn unary_evidence(
    op: UnaryOp,
    source: ExactIntegerEvidence,
    producer: RegisterProducer,
) -> Option<ExactIntegerEvidence> {
    let interval = match op {
        UnaryOp::Neg => source.interval.neg()?,
        UnaryOp::Abs => source.interval.abs()?,
        UnaryOp::Sign => ExactIntegerInterval::checked(-1, 1)?,
        UnaryOp::Floor | UnaryOp::Ceil | UnaryOp::Trunc => source.interval,
        UnaryOp::Not
        | UnaryOp::Sqrt
        | UnaryOp::Sin
        | UnaryOp::Cos
        | UnaryOp::Tan
        | UnaryOp::Asin
        | UnaryOp::Acos
        | UnaryOp::Atan
        | UnaryOp::Sinh
        | UnaryOp::Cosh
        | UnaryOp::Tanh
        | UnaryOp::Exp
        | UnaryOp::Log
        | UnaryOp::Log10 => return None,
    };
    ExactIntegerEvidence::derived(interval, producer, [source])
}

/// Exact-integer domain one binary operation preserves. Division, power,
/// `atan2`, and the boolean connectives carry no domain forward.
fn binary_evidence(
    op: BinaryOp,
    lhs: ExactIntegerEvidence,
    rhs: ExactIntegerEvidence,
    producer: RegisterProducer,
) -> Option<ExactIntegerEvidence> {
    let interval = match op {
        BinaryOp::Add => lhs.interval.add(rhs.interval)?,
        BinaryOp::Sub => lhs.interval.sub(rhs.interval)?,
        BinaryOp::Mul => lhs.interval.mul(rhs.interval)?,
        BinaryOp::Min => ExactIntegerInterval::checked(
            lhs.interval.lower.min(rhs.interval.lower),
            lhs.interval.upper.min(rhs.interval.upper),
        )?,
        BinaryOp::Max => ExactIntegerInterval::checked(
            lhs.interval.lower.max(rhs.interval.lower),
            lhs.interval.upper.max(rhs.interval.upper),
        )?,
        BinaryOp::And | BinaryOp::Or | BinaryOp::Div | BinaryOp::Pow | BinaryOp::Atan2 => {
            return None;
        }
    };
    ExactIntegerEvidence::derived(interval, producer, [lhs, rhs])
}

fn validate_runtime_index_evidence(
    op: &LinearOp,
    op_index: usize,
    evidence: &[Option<ExactIntegerEvidence>],
) -> Result<(), ScalarProgramRegisterError> {
    let validate = |index: TensorIndex, extent: u32| match index {
        TensorIndex::Constant(_) => Ok(()),
        TensorIndex::Runtime(register) => {
            let exact_integer = evidence.get(register as usize).cloned().flatten().ok_or(
                ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "runtime tensor index lacks a construction-issued exact integer domain",
                },
            )?;
            OneBasedTensorCoordinateEvidence::issue(exact_integer, register, extent).ok_or(
                ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "runtime tensor index domain escapes its selected axis",
                },
            )?;
            Ok(())
        }
    };
    let validate_projection = |dimensions: &[u32], indices: &[TensorIndex]| {
        for (&extent, &index) in dimensions.iter().zip(indices) {
            validate(index, extent)?;
        }
        Ok(())
    };
    match op {
        LinearOp::LoadIndexedRegister {
            dimensions,
            indices,
            ..
        }
        | LinearOp::LoadIndexedFoldCarried {
            dimensions,
            indices,
            ..
        }
        | LinearOp::LoadIndexedFoldCapture {
            dimensions,
            indices,
            ..
        } => validate_projection(dimensions, indices),
        LinearOp::TensorUpdate {
            dimensions,
            subscripts,
            ..
        } => validate_tensor_update_index_domains(op_index, dimensions, subscripts, validate),
        LinearOp::StoreOutputFoldTensorUpdate {
            dimensions,
            updates,
            ..
        } => validate_fold_tensor_patch_index_domains(dimensions, updates, validate),
        _ => Ok(()),
    }
}

/// Runtime index domains read by the aggregate patches of one tensor-valued
/// fold transition. Whole-axis subscripts name no register.
fn validate_fold_tensor_patch_index_domains(
    dimensions: &[u32],
    updates: &[FoldTensorUpdate],
    validate: impl Fn(TensorIndex, u32) -> Result<(), ScalarProgramRegisterError>,
) -> Result<(), ScalarProgramRegisterError> {
    for update in updates {
        for (extent, index) in patch_runtime_indices(dimensions, &update.subscripts) {
            validate(index, extent)?;
        }
    }
    Ok(())
}

/// Extent-paired indices of one aggregate patch, skipping its whole axes.
fn patch_runtime_indices<'a>(
    dimensions: &'a [u32],
    subscripts: &'a [TensorSubscript],
) -> impl Iterator<Item = (u32, TensorIndex)> + 'a {
    dimensions
        .iter()
        .zip(subscripts)
        .filter_map(|(&extent, subscript)| match *subscript {
            TensorSubscript::Index(index) => Some((extent, index)),
            TensorSubscript::Whole => None,
        })
}

fn validate_tensor_update_index_domains(
    op_index: usize,
    dimensions: &[u32],
    subscripts: &[TensorUpdateSubscript],
    validate: impl Fn(TensorIndex, u32) -> Result<(), ScalarProgramRegisterError>,
) -> Result<(), ScalarProgramRegisterError> {
    for (&extent, subscript) in dimensions.iter().zip(subscripts) {
        match subscript {
            TensorUpdateSubscript::Whole => {}
            TensorUpdateSubscript::Index(index) => validate(*index, extent)?,
            TensorUpdateSubscript::Slice { start, dimensions } => {
                validate_tensor_update_slice_index_domains(
                    op_index, extent, *start, dimensions, &validate,
                )?;
            }
        }
    }
    Ok(())
}

/// Runtime index domains of one contiguous slice subscript: every coordinate
/// register the slice names must be a one-based coordinate of its axis.
fn validate_tensor_update_slice_index_domains(
    op_index: usize,
    extent: u32,
    start: Reg,
    dimensions: &[u32],
    validate: &impl Fn(TensorIndex, u32) -> Result<(), ScalarProgramRegisterError>,
) -> Result<(), ScalarProgramRegisterError> {
    let count = dimensions
        .iter()
        .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
        .ok_or(ScalarProgramRegisterError::InvalidTensorProjection {
            op_index,
            reason: "tensor update slice coordinate count overflows",
        })?;
    for offset in 0..count {
        let register = tensor_update_slice_register(op_index, start, offset)?;
        validate(TensorIndex::Runtime(register), extent)?;
    }
    Ok(())
}

/// One coordinate register of a tensor-update slice's contiguous range.
fn tensor_update_slice_register(
    op_index: usize,
    start: Reg,
    offset: usize,
) -> Result<Reg, ScalarProgramRegisterError> {
    let offset =
        Reg::try_from(offset).map_err(|_| ScalarProgramRegisterError::InvalidTensorProjection {
            op_index,
            reason: "tensor update slice register range exceeds register identity",
        })?;
    start
        .checked_add(offset)
        .ok_or(ScalarProgramRegisterError::InvalidTensorProjection {
            op_index,
            reason: "tensor update slice register range overflows",
        })
}

impl std::fmt::Display for ScalarProgramRegisterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UndefinedRegister {
                op_index,
                operation,
                register,
            } => write!(
                f,
                "{operation} op {op_index} reads undefined register r{register}"
            ),
            Self::EmptyRegisterRange {
                op_index,
                operation,
            } => write!(f, "{operation} op {op_index} reads an empty register range"),
            Self::RegisterRangeOverflow {
                op_index,
                operation,
                start,
                len,
            } => write!(
                f,
                "{operation} op {op_index} register range r{start}..+{len} overflows"
            ),
            Self::DestinationRegisterAlreadyDefined {
                op_index,
                operation,
                register,
            } => write!(
                f,
                "{operation} op {op_index} redefines destination register r{register}"
            ),
            Self::InvalidProjection {
                op_index,
                operation,
                projection,
                len,
            } => write!(
                f,
                "{operation} op {op_index} projects element {projection} from range length {len}"
            ),
            Self::RegisterCountOverflow { register } => {
                write!(f, "register r{register} overflows the host register count")
            }
            Self::InvalidFunctionFold { op_index, reason } => {
                write!(f, "FunctionFold op {op_index} is invalid: {reason}")
            }
            Self::InvalidFunctionConditional { op_index, reason } => {
                write!(f, "FunctionConditional op {op_index} is invalid: {reason}")
            }
            Self::InvalidPureCall { op_index, reason } => {
                write!(f, "PureCall op {op_index} is invalid: {reason}")
            }
            Self::InvalidTensorProjection { op_index, reason } => {
                write!(f, "tensor projection op {op_index} is invalid: {reason}")
            }
        }
    }
}

fn mark_register_initialized(initialized: &mut Vec<bool>, register: Reg) {
    let index = register as usize;
    if initialized.len() <= index {
        initialized.resize(index + 1, false);
    }
    initialized[index] = true;
}

/// Mark one compact destination range as written by the operation at `dst`.
fn mark_register_range_initialized(initialized: &mut Vec<bool>, dst: Reg, count: usize) {
    for offset in 0..count {
        mark_register_initialized(initialized, dst + offset as Reg);
    }
}

/// Prove that one operation issues fresh SSA definitions for its entire
/// destination range. The range end was already checked by
/// [`register_range_last`], so this reports the first overlapping definition
/// rather than manufacturing a second value for the same register identity.
fn require_register_range_uninitialized(
    op_index: usize,
    operation: &'static str,
    dst: Reg,
    count: usize,
    initialized: &[bool],
) -> Result<(), ScalarProgramRegisterError> {
    for offset in 0..count {
        let register = dst + offset as Reg;
        if initialized.get(register as usize).copied().unwrap_or(false) {
            return Err(
                ScalarProgramRegisterError::DestinationRegisterAlreadyDefined {
                    op_index,
                    operation,
                    register,
                },
            );
        }
    }
    Ok(())
}

/// Read-only context for one operation's source-register proof.
///
/// Every extracted check needs the same row context, so bundling it keeps the
/// per-operation helpers below the workspace argument budget and keeps one
/// operation-name spelling (`LinearOp::kind_name`) in every diagnostic.
#[derive(Clone, Copy)]
struct OpSources<'a> {
    op: &'a LinearOp,
    op_index: usize,
    initialized: &'a [bool],
    fold_context: Option<(usize, usize, usize)>,
    conditional_capture_count: Option<usize>,
}

impl OpSources<'_> {
    /// Stable operation name carried by every register-flow diagnostic.
    fn operation(self) -> &'static str {
        self.op.kind_name()
    }

    /// Prove one scalar source register is dominated by an earlier write.
    fn require(self, register: Reg) -> Result<(), ScalarProgramRegisterError> {
        require_register(self.op_index, self.operation(), register, self.initialized)
    }

    /// Prove one compact consecutive source range and return its last register.
    fn require_range(self, start: Reg, len: usize) -> Result<Reg, ScalarProgramRegisterError> {
        require_register_range(
            self.op_index,
            self.operation(),
            start,
            len,
            self.initialized,
        )
    }

    /// Last register of one compact consecutive range.
    fn range_last(self, start: Reg, len: usize) -> Result<Reg, ScalarProgramRegisterError> {
        register_range_last(self.op_index, self.operation(), start, len)
    }

    /// Prove one projection selects an element inside its checked tuple.
    fn projection(self, projection: usize, len: usize) -> Result<(), ScalarProgramRegisterError> {
        validate_projection(self.op_index, self.operation(), projection, len)
    }

    fn fold_error(self, reason: &'static str) -> ScalarProgramRegisterError {
        ScalarProgramRegisterError::InvalidFunctionFold {
            op_index: self.op_index,
            reason,
        }
    }

    fn conditional_error(self, reason: &'static str) -> ScalarProgramRegisterError {
        ScalarProgramRegisterError::InvalidFunctionConditional {
            op_index: self.op_index,
            reason,
        }
    }

    fn pure_call_error(self, reason: &'static str) -> ScalarProgramRegisterError {
        ScalarProgramRegisterError::InvalidPureCall {
            op_index: self.op_index,
            reason,
        }
    }

    fn tensor_error(self, reason: &'static str) -> ScalarProgramRegisterError {
        ScalarProgramRegisterError::InvalidTensorProjection {
            op_index: self.op_index,
            reason,
        }
    }
}

/// Which compact tuple of the enclosing function-fold ABI a scalar load reads.
#[derive(Clone, Copy)]
enum FoldSlot {
    Carried,
    Binder,
    Capture,
}

impl FoldSlot {
    /// Width of this tuple in the enclosing update body's ABI.
    const fn count(self, fold_context: (usize, usize, usize)) -> usize {
        let (carried_count, dimension_count, capture_count) = fold_context;
        match self {
            Self::Carried => carried_count,
            Self::Binder => dimension_count,
            Self::Capture => capture_count,
        }
    }

    /// Diagnostic reported when the load escaped its function-fold update body.
    const fn escaped_reason(self) -> &'static str {
        match self {
            Self::Carried => "carried load escaped its update body",
            Self::Binder => "binder load escaped its update body",
            Self::Capture => "capture load escaped its update body",
        }
    }
}

/// Source-register proof for one checked [`LinearOp`].
///
/// This is the sole dispatch table of the proof: every arm names its variant
/// and delegates to one checked helper, so the table carries the operation
/// vocabulary and no validation logic. The match stays exhaustive with no
/// catch-all arm, so a new `LinearOp` variant cannot reach an evaluator until
/// its sources are proved here.
///
/// The table is laid out by hand (`rustfmt::skip`) so one arm reads as one
/// entry. Automatic formatting expands every multi-field pattern to one field
/// per line, which triples the table without telling a reader anything the
/// single-line form does not.
#[rustfmt::skip]
fn validate_op_sources(
    op: &LinearOp,
    op_index: usize,
    initialized: &[bool],
    fold_context: Option<(usize, usize, usize)>,
    conditional_capture_count: Option<usize>,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    let cx = OpSources { op, op_index, initialized, fold_context, conditional_capture_count };
    match *op {
        LinearOp::Const { .. } | LinearOp::LoadTime { .. } | LinearOp::LoadY { .. }
        | LinearOp::LoadP { .. } | LinearOp::LoadSeed { .. } => Ok(None),
        LinearOp::LoadFoldCarried { index, .. } => fold_load(cx, index, FoldSlot::Carried),
        LinearOp::LoadFoldIndex { dimension, .. } => fold_load(cx, dimension, FoldSlot::Binder),
        LinearOp::LoadFoldCapture { index, .. } => fold_load(cx, index, FoldSlot::Capture),
        LinearOp::LoadFunctionConditionalCapture { index, .. } => conditional_capture(cx, index),
        LinearOp::LoadFunctionConditionalCaptureRange { index_start, count, .. } =>
            conditional_capture_range(cx, index_start, count),
        LinearOp::Move { src, .. } | LinearOp::Unary { arg: src, .. }
        | LinearOp::StoreOutput { src } => single_source(cx, src),
        LinearOp::StoreOutputRange { start, count, stride } =>
            output_range(cx, start, count, stride),
        LinearOp::LoadIndexedRegister { base, stride, ref dimensions, ref indices, .. } =>
            indexed_register(cx, base, stride, dimensions, indices),
        LinearOp::LoadIndexedFoldCarried { base, stride, ref dimensions, ref indices, .. } =>
            indexed_fold_carried(cx, base, stride, dimensions, indices),
        LinearOp::LoadIndexedFoldCapture { base, stride, ref dimensions, ref indices, .. } =>
            indexed_fold_capture(cx, base, stride, dimensions, indices),
        LinearOp::StoreOutputFoldTensorUpdate {
            source_base, source_stride, ref dimensions, ref updates, ref nodes, result, lanes,
        } => {
            fold_tensor_source(cx, source_base, source_stride, dimensions, updates, lanes)?;
            let max_register = fold_tensor_patches(cx, dimensions, updates, lanes)?;
            fold_tensor_nodes(cx, nodes, updates.len(), result, max_register)
        }
        LinearOp::StoreOutputFunctionFold {
            ref initial, capture_start, ref program, result_base, count, condition, ..
        } => nested_fold(cx, initial, capture_start, program, result_base, count, condition),
        LinearOp::FunctionConditional { capture_start, ref program, .. } =>
            conditional_program(cx, capture_start, program),
        LinearOp::PureCall { ref input_starts, ref site, .. } => pure_call(cx, input_starts, site),
        LinearOp::PureCallDirectional { ref input_starts, ref site, .. } =>
            directional_call(cx, input_starts, site),
        LinearOp::Binary { lhs, rhs, .. } | LinearOp::Compare { lhs, rhs, .. } =>
            two_sources(cx, lhs, rhs),
        LinearOp::Select { cond, if_true, if_false, .. } =>
            three_sources(cx, cond, if_true, if_false),
        LinearOp::LinearSolveComponent { matrix_start, rhs_start, n, component, .. } =>
            linear_solve_sources(cx, matrix_start, rhs_start, n, component).map(Some),
        LinearOp::DotProduct { lhs_start, rhs_start, count, lhs_stride, rhs_stride, .. } =>
            dot_product_sources(cx, lhs_start, rhs_start, count, lhs_stride, rhs_stride).map(Some),
        LinearOp::MatrixMultiply { lhs_start, rhs_start, rows, inner, columns, lanes, .. } =>
            matrix_multiply(cx, lhs_start, rhs_start, rows, inner, columns, lanes),
        LinearOp::TensorBinary {
            op, lhs_start, rhs_start, count, lhs_stride, rhs_stride, lanes, ..
        } => tensor_binary(cx, op, (lhs_start, lhs_stride), (rhs_start, rhs_stride), count, lanes),
        LinearOp::TensorCross { lhs_start, rhs_start, lanes, .. } =>
            tensor_cross(cx, lhs_start, rhs_start, lanes),
        LinearOp::TensorTranspose { src_start, rows, columns, element_width, lanes, .. } =>
            tensor_transpose(cx, src_start, rows, columns, element_width, lanes),
        LinearOp::TensorConcatenate { ref sources, ref dimensions, axis, lanes, .. } =>
            tensor_concatenate(cx, sources, dimensions, axis, lanes),
        LinearOp::TensorUpdate {
            base_start, value_start, ref dimensions, ref subscripts, lanes, ..
        } => tensor_update(cx, base_start, value_start, dimensions, subscripts, lanes),
        LinearOp::TensorFill { value_start, count, lanes, .. } =>
            tensor_fill(cx, value_start, count, lanes),
        LinearOp::TensorIdentity { size, lanes, .. } => tensor_identity(cx, size, lanes),
        LinearOp::TensorLoad { count, seed_start, lanes, .. } =>
            tensor_load(cx, count, seed_start, lanes),
        LinearOp::TableBounds { table_id, .. } => single_source(cx, table_id),
        LinearOp::TableLookup { table_id, column, input, .. }
        | LinearOp::TableLookupSlope { table_id, column, input, .. } =>
            three_sources(cx, table_id, column, input),
        LinearOp::TableNextEvent { table_id, time, .. } => two_sources(cx, table_id, time),
        LinearOp::RandomInitialState { .. } | LinearOp::RandomResult { .. }
        | LinearOp::RandomState { .. } | LinearOp::ImpureRandomInit { .. }
        | LinearOp::ImpureRandom { .. } | LinearOp::ImpureRandomInteger { .. } =>
            random_sources(cx),
        LinearOp::FunctionFold { initial_start, capture_start, ref program, .. } =>
            function_fold_sources(cx, initial_start, capture_start, None, program),
        LinearOp::GuardedFunctionFold {
            initial_start, capture_start, activation, ref program, ..
        } => function_fold_sources(cx, initial_start, capture_start, Some(activation), program),
    }
}

/// One scalar load from a compact function-fold tuple.
fn fold_load(
    cx: OpSources<'_>,
    index: usize,
    slot: FoldSlot,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    let Some(fold_context) = cx.fold_context else {
        return Err(cx.fold_error(slot.escaped_reason()));
    };
    cx.projection(index, slot.count(fold_context))?;
    Ok(None)
}

/// One scalar load from the enclosing function-conditional capture ABI.
fn conditional_capture(
    cx: OpSources<'_>,
    index: usize,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    let Some(capture_count) = cx.conditional_capture_count else {
        return Err(cx.conditional_error("capture load escaped its conditional region"));
    };
    cx.projection(index, capture_count)?;
    Ok(None)
}

/// One compact consecutive range of the function-conditional capture ABI.
fn conditional_capture_range(
    cx: OpSources<'_>,
    index_start: usize,
    count: usize,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    let Some(capture_count) = cx.conditional_capture_count else {
        return Err(cx.conditional_error("capture range load escaped its conditional region"));
    };
    if count == 0
        || index_start
            .checked_add(count)
            .is_none_or(|end| end > capture_count)
    {
        return Err(cx.conditional_error(
            "capture range load is empty, overflows, or exceeds the capture ABI",
        ));
    }
    Ok(None)
}

/// One operation whose only source is a single scalar register.
fn single_source(cx: OpSources<'_>, src: Reg) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    cx.require(src)?;
    Ok(Some(src))
}

/// One operation reading exactly two scalar source registers.
fn two_sources(
    cx: OpSources<'_>,
    first: Reg,
    second: Reg,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    cx.require(first)?;
    cx.require(second)?;
    Ok(Some(first.max(second)))
}

/// One operation reading exactly three scalar source registers.
fn three_sources(
    cx: OpSources<'_>,
    first: Reg,
    second: Reg,
    third: Reg,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    cx.require(first)?;
    cx.require(second)?;
    cx.require(third)?;
    Ok(Some(first.max(second).max(third)))
}

/// One compact affine register range projected as consecutive outputs.
fn output_range(
    cx: OpSources<'_>,
    start: Reg,
    count: usize,
    stride: usize,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    require_strided_register_range(
        cx.op_index,
        cx.operation(),
        start,
        count,
        stride,
        cx.initialized,
    )
    .map(Some)
}

/// One scalar projected from a packed tensor register range.
fn indexed_register(
    cx: OpSources<'_>,
    base: Reg,
    stride: usize,
    dimensions: &[u32],
    indices: &[TensorIndex],
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    if dimensions.is_empty() || dimensions.len() != indices.len() {
        return Err(cx.tensor_error("tensor projection rank does not match its checked dimensions"));
    }
    let count = dimensions.iter().try_fold(1usize, |count, &extent| {
        (extent != 0)
            .then(|| count.checked_mul(extent as usize))
            .flatten()
    });
    let Some(count) = count else {
        return Err(cx.tensor_error("tensor projection dimensions are empty or overflow"));
    };
    if stride == 0 {
        return Err(cx.tensor_error("tensor projection register stride is zero"));
    }
    let last_offset = (count - 1)
        .checked_mul(stride)
        .ok_or_else(|| cx.tensor_error("tensor projection register stride overflows"))?;
    let last_offset = Reg::try_from(last_offset)
        .map_err(|_| cx.tensor_error("tensor projection register stride overflows"))?;
    let base_last = base
        .checked_add(last_offset)
        .ok_or_else(|| cx.tensor_error("tensor projection register range overflows"))?;
    for offset in 0..count {
        cx.require(base + (offset * stride) as Reg)?;
    }
    let mut source_last = base_last;
    for (&extent, index) in dimensions.iter().zip(indices.iter()) {
        match *index {
            TensorIndex::Constant(coordinate) if coordinate >= extent => {
                return Err(cx.tensor_error("constant tensor coordinate is out of range"));
            }
            TensorIndex::Constant(_) => {}
            TensorIndex::Runtime(register) => {
                cx.require(register)?;
                source_last = source_last.max(register);
            }
        }
    }
    Ok(Some(source_last))
}

/// One scalar projected from the current aggregate carried tuple.
fn indexed_fold_carried(
    cx: OpSources<'_>,
    base: usize,
    stride: usize,
    dimensions: &[u32],
    indices: &[TensorIndex],
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    let Some((carried_count, _, _)) = cx.fold_context else {
        return Err(cx.fold_error("indexed carried load escaped its update body"));
    };
    validate_tensor_projection_shape(cx.op_index, dimensions, indices)?;
    let count = tensor_scalar_count(cx.op_index, dimensions)?;
    let last = base
        .checked_add(
            (count - 1)
                .checked_mul(stride)
                .ok_or_else(|| cx.tensor_error("indexed carried projection stride overflows"))?,
        )
        .ok_or_else(|| cx.tensor_error("indexed carried projection range overflows"))?;
    if stride == 0 || last >= carried_count {
        return Err(cx.tensor_error("indexed carried projection is outside its tuple"));
    }
    indexed_fold_runtime_indices(cx, indices)
}

/// One scalar projected from the immutable capture tuple of a function fold.
fn indexed_fold_capture(
    cx: OpSources<'_>,
    base: usize,
    stride: usize,
    dimensions: &[u32],
    indices: &[TensorIndex],
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    let Some((_, _, capture_count)) = cx.fold_context else {
        return Err(cx.fold_error("indexed capture load escaped its update body"));
    };
    validate_tensor_projection_shape(cx.op_index, dimensions, indices)?;
    let count = tensor_scalar_count(cx.op_index, dimensions)?;
    let last = base
        .checked_add(
            (count - 1)
                .checked_mul(stride)
                .ok_or_else(|| cx.tensor_error("indexed capture projection stride overflows"))?,
        )
        .ok_or_else(|| cx.tensor_error("indexed capture projection range overflows"))?;
    if stride == 0 || last >= capture_count {
        return Err(cx.tensor_error("indexed capture projection is outside its tuple"));
    }
    indexed_fold_runtime_indices(cx, indices)
}

/// Runtime coordinate registers read by one indexed function-fold projection.
fn indexed_fold_runtime_indices(
    cx: OpSources<'_>,
    indices: &[TensorIndex],
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    let mut source_last = None;
    for index in indices {
        if let TensorIndex::Runtime(register) = *index {
            cx.require(register)?;
            source_last = Some(source_last.map_or(register, |last: Reg| last.max(register)));
        }
    }
    Ok(source_last)
}

/// Carried source range read by one compact tensor-valued fold transition.
fn fold_tensor_source(
    cx: OpSources<'_>,
    source_base: usize,
    source_stride: usize,
    dimensions: &[u32],
    updates: &[FoldTensorUpdate],
    lanes: usize,
) -> Result<(), ScalarProgramRegisterError> {
    let Some((carried_count, _, _)) = cx.fold_context else {
        return Err(cx.fold_error("aggregate output escaped its function-fold update body"));
    };
    if dimensions.is_empty() || dimensions.contains(&0) || updates.is_empty() {
        return Err(cx.tensor_error("tensor update has no checked rank or patches"));
    }
    if lanes == 0 || source_stride != lanes {
        return Err(cx.tensor_error("tensor update storage is not element-major by lane"));
    }
    let count = tensor_scalar_count(cx.op_index, dimensions)?;
    let source_last = source_base
        .checked_add(
            (count - 1)
                .checked_mul(source_stride)
                .ok_or_else(|| cx.tensor_error("tensor update carried stride overflows"))?,
        )
        .and_then(|last| last.checked_add(lanes - 1))
        .ok_or_else(|| cx.tensor_error("tensor update carried range overflows"))?;
    if source_last >= carried_count {
        return Err(cx.tensor_error("tensor update source is outside its carried tuple"));
    }
    Ok(())
}

/// Ordered aggregate patches of one compact tensor-valued fold transition.
fn fold_tensor_patches(
    cx: OpSources<'_>,
    dimensions: &[u32],
    updates: &[FoldTensorUpdate],
    lanes: usize,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    let mut max_register = None;
    for update in updates {
        if dimensions.len() != update.subscripts.len() || update.value_stride != lanes {
            return Err(cx.tensor_error("tensor patch shape or lane layout is invalid"));
        }
        let mut value_count = 1usize;
        if let Some(condition) = update.condition {
            cx.require(condition)?;
            max_register = Some(max_register.map_or(condition, |last: Reg| last.max(condition)));
        }
        for (&extent, subscript) in dimensions.iter().zip(update.subscripts.iter()) {
            value_count =
                fold_tensor_subscript(cx, extent, *subscript, value_count, &mut max_register)?;
        }
        let value_last_offset = (value_count - 1)
            .checked_mul(update.value_stride)
            .and_then(|offset| offset.checked_add(lanes - 1))
            .ok_or_else(|| cx.tensor_error("tensor update value range overflows"))?;
        let value_last_offset = Reg::try_from(value_last_offset)
            .map_err(|_| cx.tensor_error("tensor update value range exceeds register identity"))?;
        let value_last = update
            .value_start
            .checked_add(value_last_offset)
            .ok_or_else(|| cx.tensor_error("tensor update value register range overflows"))?;
        fold_tensor_values(cx, update, value_count, lanes)?;
        max_register = Some(max_register.map_or(value_last, |last| last.max(value_last)));
    }
    Ok(max_register)
}

/// One axis of one aggregate patch in a tensor-valued fold transition.
fn fold_tensor_subscript(
    cx: OpSources<'_>,
    extent: u32,
    subscript: TensorSubscript,
    value_count: usize,
    max_register: &mut Option<Reg>,
) -> Result<usize, ScalarProgramRegisterError> {
    match subscript {
        TensorSubscript::Whole => value_count
            .checked_mul(extent as usize)
            .ok_or_else(|| cx.tensor_error("tensor update value extent overflows")),
        TensorSubscript::Index(TensorIndex::Constant(coordinate)) => {
            if coordinate >= extent {
                return Err(cx.tensor_error("constant tensor update coordinate is out of range"));
            }
            Ok(value_count)
        }
        TensorSubscript::Index(TensorIndex::Runtime(register)) => {
            cx.require(register)?;
            *max_register = Some(max_register.map_or(register, |last: Reg| last.max(register)));
            Ok(value_count)
        }
    }
}

/// Element-major lane values read by one aggregate tensor patch.
fn fold_tensor_values(
    cx: OpSources<'_>,
    update: &FoldTensorUpdate,
    value_count: usize,
    lanes: usize,
) -> Result<(), ScalarProgramRegisterError> {
    for element in 0..value_count {
        for lane in 0..lanes {
            cx.require(update.value_start + (element * update.value_stride + lane) as Reg)?;
        }
    }
    Ok(())
}

/// Topological node list and result selector of one tensor fold transition.
fn fold_tensor_nodes(
    cx: OpSources<'_>,
    nodes: &[FoldTensorNode],
    update_count: usize,
    result: u32,
    max_register: Option<Reg>,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    let mut max_register = max_register;
    for (index, node) in nodes.iter().enumerate() {
        let current = u32::try_from(index + 1)
            .map_err(|_| cx.tensor_error("tensor expression node identity overflows"))?;
        fold_tensor_node(cx, node, current, update_count, &mut max_register)?;
    }
    if result == 0 || result as usize > nodes.len() {
        return Err(cx.tensor_error("tensor expression result does not name a stored node"));
    }
    Ok(max_register)
}

/// One compact tensor-expression node of a fold transition.
fn fold_tensor_node(
    cx: OpSources<'_>,
    node: &FoldTensorNode,
    current: u32,
    update_count: usize,
    max_register: &mut Option<Reg>,
) -> Result<(), ScalarProgramRegisterError> {
    match *node {
        FoldTensorNode::Update { base, update } => {
            if base >= current || update as usize >= update_count {
                return Err(cx.tensor_error("tensor update node is not topological"));
            }
        }
        FoldTensorNode::Select {
            condition,
            if_true,
            if_false,
        } => {
            if if_true >= current || if_false >= current {
                return Err(cx.tensor_error("tensor select node is not topological"));
            }
            cx.require(condition)?;
            *max_register = Some(max_register.map_or(condition, |last| last.max(condition)));
        }
    }
    Ok(())
}

/// One nested fold projected directly as this fold's aggregate output.
fn nested_fold(
    cx: OpSources<'_>,
    initial: &[FoldInitialSource],
    capture_start: Reg,
    program: &FunctionFoldProgram,
    result_base: usize,
    count: usize,
    condition: Option<Reg>,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    let Some((parent_carried, _, _)) = cx.fold_context else {
        return Err(cx.fold_error("nested aggregate fold escaped its parent update body"));
    };
    let mut initial_count = 0usize;
    let mut max_register = None;
    for source in initial {
        let source_count = nested_fold_initial(cx, source, parent_carried, &mut max_register)?;
        initial_count = initial_count
            .checked_add(source_count)
            .ok_or_else(|| cx.fold_error("nested fold initial source count overflows"))?;
    }
    if initial_count != program.carried_count
        || count == 0
        || result_base
            .checked_add(count)
            .is_none_or(|end| end > program.carried_count)
    {
        return Err(cx.fold_error("nested fold initial/result layout is invalid"));
    }
    if program.capture_count != 0 {
        let last = cx.range_last(capture_start, program.capture_count)?;
        for offset in 0..program.capture_count {
            cx.require(capture_start + offset as Reg)?;
        }
        max_register = Some(max_register.map_or(last, |current: Reg| current.max(last)));
    }
    if let Some(condition) = condition {
        cx.require(condition)?;
        max_register = Some(max_register.map_or(condition, |current: Reg| current.max(condition)));
    }
    Ok(max_register)
}

/// One compact source of a nested fold's initial carried tuple.
fn nested_fold_initial(
    cx: OpSources<'_>,
    source: &FoldInitialSource,
    parent_carried: usize,
    max_register: &mut Option<Reg>,
) -> Result<usize, ScalarProgramRegisterError> {
    match *source {
        FoldInitialSource::Registers { start, count } => {
            if count != 0 {
                let last = cx.range_last(start, count)?;
                for offset in 0..count {
                    cx.require(start + offset as Reg)?;
                }
                *max_register = Some(max_register.map_or(last, |current: Reg| current.max(last)));
            }
            Ok(count)
        }
        FoldInitialSource::ParentCarried { base, count } => {
            if base
                .checked_add(count)
                .is_none_or(|end| end > parent_carried)
            {
                return Err(cx.fold_error("nested fold parent-carried initial source is invalid"));
            }
            Ok(count)
        }
    }
}

/// One issued multi-target function conditional and its capture ABI.
fn conditional_program(
    cx: OpSources<'_>,
    capture_start: Reg,
    program: &FunctionConditionalProgram,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    if program.capture_count == 0 {
        return Ok(None);
    }
    cx.require_range(capture_start, program.capture_count)
        .map(Some)
}

/// One issued model-level typed pure-call owner.
fn pure_call(
    cx: OpSources<'_>,
    input_starts: &[Reg],
    site: &SolvePureCallSite,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    typed_call_sources(
        cx,
        input_starts,
        site.inputs(),
        site.output_scalar_count(),
        "typed call interface has invalid input or output width",
    )
}

/// The checked compact directional relation of one typed pure-call owner.
fn directional_call(
    cx: OpSources<'_>,
    input_starts: &[Reg],
    site: &SolvePureCallDirectionalSite,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    typed_call_sources(
        cx,
        input_starts,
        site.inputs(),
        site.output_scalar_count(),
        "typed directional call interface has invalid input or output width",
    )
}

/// Compact input ranges of one typed call, sized by its declared value types.
fn typed_call_sources(
    cx: OpSources<'_>,
    input_starts: &[Reg],
    inputs: &[SolveValueType],
    output_scalar_count: Option<usize>,
    reason: &'static str,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    if input_starts.len() != inputs.len() || output_scalar_count.is_none_or(|count| count == 0) {
        return Err(cx.pure_call_error(reason));
    }
    let mut last = None;
    for (&start, value_type) in input_starts.iter().zip(inputs) {
        let count = value_type.scalar_count() as usize;
        cx.require_range(start, count)?;
        let range_last = cx.range_last(start, count)?;
        last = Some(last.map_or(range_last, |current: Reg| current.max(range_last)));
    }
    Ok(last)
}

/// Two dense row-major tensor operands of one aggregate matrix multiply.
fn matrix_multiply(
    cx: OpSources<'_>,
    lhs_start: Reg,
    rhs_start: Reg,
    rows: usize,
    inner: usize,
    columns: usize,
    lanes: usize,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    if rows == 0 || inner == 0 || columns == 0 || lanes == 0 || lanes > 2 {
        return Err(cx.tensor_error("matrix multiply has an invalid shape or lane count"));
    }
    let lhs_count = rows
        .checked_mul(inner)
        .and_then(|count| count.checked_mul(lanes))
        .ok_or_else(|| cx.tensor_error("matrix multiply lhs range overflows"))?;
    let rhs_count = inner
        .checked_mul(columns)
        .and_then(|count| count.checked_mul(lanes))
        .ok_or_else(|| cx.tensor_error("matrix multiply rhs range overflows"))?;
    cx.require_range(lhs_start, lhs_count)?;
    cx.require_range(rhs_start, rhs_count)?;
    Ok(Some(
        cx.range_last(lhs_start, lhs_count)?
            .max(cx.range_last(rhs_start, rhs_count)?),
    ))
}

/// Two strided operand ranges of one elementwise tensor binary operation.
///
/// `lhs` and `rhs` are each one `(start, stride)` source range; a zero stride
/// broadcasts one scalar across the output range.
fn tensor_binary(
    cx: OpSources<'_>,
    op: BinaryOp,
    lhs: (Reg, usize),
    rhs: (Reg, usize),
    count: usize,
    lanes: usize,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    if count == 0
        || lanes == 0
        || lanes > 2
        || !matches!(
            op,
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div
        )
    {
        return Err(cx.tensor_error("tensor binary has an invalid operator, extent, or lane count"));
    }
    let (lhs_start, lhs_stride) = lhs;
    let (rhs_start, rhs_stride) = rhs;
    let source_count = |stride: usize| {
        count
            .saturating_sub(1)
            .checked_mul(stride)
            .and_then(|last| last.checked_add(1))
            .and_then(|elements| elements.checked_mul(lanes))
    };
    let lhs_count = source_count(lhs_stride)
        .ok_or_else(|| cx.tensor_error("tensor binary lhs range overflows"))?;
    let rhs_count = source_count(rhs_stride)
        .ok_or_else(|| cx.tensor_error("tensor binary rhs range overflows"))?;
    cx.require_range(lhs_start, lhs_count)?;
    cx.require_range(rhs_start, rhs_count)?;
    Ok(Some(
        cx.range_last(lhs_start, lhs_count)?
            .max(cx.range_last(rhs_start, rhs_count)?),
    ))
}

/// Two length-three vector operands of one aggregate cross product.
fn tensor_cross(
    cx: OpSources<'_>,
    lhs_start: Reg,
    rhs_start: Reg,
    lanes: usize,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    if lanes == 0 || lanes > 2 {
        return Err(cx.tensor_error("tensor cross product has an invalid lane count"));
    }
    let count = 3usize
        .checked_mul(lanes)
        .ok_or_else(|| cx.tensor_error("tensor cross product input range overflows"))?;
    cx.require_range(lhs_start, count)?;
    cx.require_range(rhs_start, count)?;
    Ok(Some(
        cx.range_last(lhs_start, count)?
            .max(cx.range_last(rhs_start, count)?),
    ))
}

/// The dense row-major source of one aggregate tensor transpose.
fn tensor_transpose(
    cx: OpSources<'_>,
    src_start: Reg,
    rows: usize,
    columns: usize,
    element_width: usize,
    lanes: usize,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    let count = rows
        .checked_mul(columns)
        .and_then(|count| count.checked_mul(element_width))
        .and_then(|count| count.checked_mul(lanes))
        .ok_or_else(|| cx.tensor_error("tensor transpose range overflows"))?;
    if rows == 0 || columns == 0 || element_width == 0 || lanes == 0 || lanes > 2 {
        return Err(
            cx.tensor_error("tensor transpose has an invalid shape, element width, or lane count")
        );
    }
    cx.require_range(src_start, count)?;
    cx.range_last(src_start, count).map(Some)
}

/// Every dense row-major source of one aggregate tensor concatenation.
fn tensor_concatenate(
    cx: OpSources<'_>,
    sources: &[TensorConcatenateSource],
    dimensions: &[u32],
    axis: usize,
    lanes: usize,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    if sources.is_empty()
        || dimensions.is_empty()
        || axis >= dimensions.len()
        || lanes == 0
        || lanes > 2
    {
        return Err(cx.tensor_error("tensor concatenate has an invalid shape, axis, or lane count"));
    }
    let mut axis_extent = 0u32;
    let mut last = 0;
    for source in sources {
        if source.dimensions.len() != dimensions.len()
            || source
                .dimensions
                .iter()
                .zip(dimensions.iter())
                .enumerate()
                .any(|(dimension, (source, result))| dimension != axis && source != result)
        {
            return Err(cx.tensor_error("tensor concatenate source shape is incompatible"));
        }
        axis_extent = axis_extent
            .checked_add(source.dimensions[axis])
            .ok_or_else(|| cx.tensor_error("tensor concatenate axis extent overflows"))?;
        let count = source
            .dimensions
            .iter()
            .try_fold(lanes, |count, extent| count.checked_mul(*extent as usize))
            .ok_or_else(|| cx.tensor_error("tensor concatenate source extent overflows"))?;
        cx.require_range(source.start, count)?;
        last = last.max(cx.range_last(source.start, count)?);
    }
    if axis_extent != dimensions[axis] {
        return Err(cx.tensor_error("tensor concatenate sources do not cover the result axis"));
    }
    Ok(Some(last))
}

/// The base range, projection, and value range of one shape-preserving patch.
fn tensor_update(
    cx: OpSources<'_>,
    base_start: Reg,
    value_start: Reg,
    dimensions: &[u32],
    subscripts: &[TensorUpdateSubscript],
    lanes: usize,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    if dimensions.is_empty()
        || dimensions.len() != subscripts.len()
        || lanes == 0
        || lanes > 2
        || dimensions.contains(&0)
    {
        return Err(
            cx.tensor_error("tensor update has an invalid shape, projection, or lane count")
        );
    }
    let base_count = dimensions
        .iter()
        .try_fold(lanes, |count, extent| count.checked_mul(*extent as usize))
        .ok_or_else(|| cx.tensor_error("tensor update base extent overflows"))?;
    cx.require_range(base_start, base_count)?;
    let mut last = cx.range_last(base_start, base_count)?;
    let mut value_count = lanes;
    for (&extent, subscript) in dimensions.iter().zip(subscripts.iter()) {
        value_count = tensor_update_subscript(cx, extent, subscript, value_count, &mut last)?;
    }
    cx.require_range(value_start, value_count)?;
    last = last.max(cx.range_last(value_start, value_count)?);
    Ok(Some(last))
}

/// One axis of a compact ordinary tensor update.
fn tensor_update_subscript(
    cx: OpSources<'_>,
    extent: u32,
    subscript: &TensorUpdateSubscript,
    value_count: usize,
    last: &mut Reg,
) -> Result<usize, ScalarProgramRegisterError> {
    match subscript {
        TensorUpdateSubscript::Whole => value_count
            .checked_mul(extent as usize)
            .ok_or_else(|| cx.tensor_error("tensor update value extent overflows")),
        TensorUpdateSubscript::Index(TensorIndex::Constant(coordinate)) => {
            if *coordinate >= extent {
                return Err(cx.tensor_error("tensor update constant index is out of range"));
            }
            Ok(value_count)
        }
        TensorUpdateSubscript::Index(TensorIndex::Runtime(register)) => {
            cx.require(*register)?;
            *last = (*last).max(*register);
            Ok(value_count)
        }
        TensorUpdateSubscript::Slice { start, dimensions } => {
            if dimensions.is_empty() || dimensions.contains(&0) {
                return Err(cx.tensor_error("tensor update slice has an invalid shape"));
            }
            let count = dimensions
                .iter()
                .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
                .ok_or_else(|| cx.tensor_error("tensor update slice extent overflows"))?;
            cx.require_range(*start, count)?;
            *last = (*last).max(cx.range_last(*start, count)?);
            value_count
                .checked_mul(count)
                .ok_or_else(|| cx.tensor_error("tensor update sliced value extent overflows"))
        }
    }
}

/// The replicated scalar (or interleaved dual scalar) of one tensor fill.
fn tensor_fill(
    cx: OpSources<'_>,
    value_start: Reg,
    count: usize,
    lanes: usize,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    if count == 0 || lanes == 0 || lanes > 2 {
        return Err(cx.tensor_error("tensor fill has an invalid extent or lane count"));
    }
    cx.require_range(value_start, lanes)?;
    cx.range_last(value_start, lanes).map(Some)
}

/// The checked extent of one square identity tensor.
fn tensor_identity(
    cx: OpSources<'_>,
    size: usize,
    lanes: usize,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    if size == 0 || lanes == 0 || lanes > 2 || size.checked_mul(size).is_none() {
        return Err(cx.tensor_error("tensor identity has an invalid extent or lane count"));
    }
    Ok(None)
}

/// The checked extent and seed interleaving of one runtime tensor load.
fn tensor_load(
    cx: OpSources<'_>,
    count: usize,
    seed_start: Option<usize>,
    lanes: usize,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    if count == 0 || lanes == 0 || lanes > 2 || (lanes == 1 && seed_start.is_some()) {
        return Err(cx.tensor_error("tensor load has an invalid extent, seed, or lane count"));
    }
    Ok(None)
}

/// The compact call ABI of one issued finite-domain function fold.
fn function_fold_sources(
    cx: OpSources<'_>,
    initial_start: Reg,
    capture_start: Reg,
    activation: Option<Reg>,
    program: &FunctionFoldProgram,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    if program.carried_count == 0 {
        return Err(cx.fold_error("carried tuple is empty"));
    }
    cx.require_range(initial_start, program.carried_count)?;
    if program.capture_count != 0 {
        cx.require_range(capture_start, program.capture_count)?;
    }
    if let Some(activation) = activation {
        cx.require(activation)?;
    }
    // `FunctionFoldProgram::checked` (including checked wire replay) owns
    // body/domain/output validation. A reference from an outer program proves
    // only its compact call ABI; recursively re-proving the immutable Arc here
    // would turn a shared program DAG back into an extent-sized traversal tree.
    let initial_last = cx.range_last(initial_start, program.carried_count)?;
    let mut source_last = if program.capture_count == 0 {
        initial_last
    } else {
        initial_last.max(cx.range_last(capture_start, program.capture_count)?)
    };
    if let Some(activation) = activation {
        source_last = source_last.max(activation);
    }
    Ok(Some(source_last))
}

fn register_range_last(
    op_index: usize,
    operation: &'static str,
    start: Reg,
    len: usize,
) -> Result<Reg, ScalarProgramRegisterError> {
    if len == 0 {
        return Err(ScalarProgramRegisterError::EmptyRegisterRange {
            op_index,
            operation,
        });
    }
    let offset =
        Reg::try_from(len - 1).map_err(|_| ScalarProgramRegisterError::RegisterRangeOverflow {
            op_index,
            operation,
            start,
            len,
        })?;
    start
        .checked_add(offset)
        .ok_or(ScalarProgramRegisterError::RegisterRangeOverflow {
            op_index,
            operation,
            start,
            len,
        })
}

fn validate_tensor_projection_shape(
    op_index: usize,
    dimensions: &[u32],
    indices: &[TensorIndex],
) -> Result<(), ScalarProgramRegisterError> {
    if dimensions.is_empty() || dimensions.len() != indices.len() {
        return Err(ScalarProgramRegisterError::InvalidTensorProjection {
            op_index,
            reason: "tensor projection rank does not match its checked dimensions",
        });
    }
    for (&extent, index) in dimensions.iter().zip(indices) {
        if extent == 0 {
            return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                op_index,
                reason: "tensor projection has an empty dimension",
            });
        }
        if matches!(*index, TensorIndex::Constant(coordinate) if coordinate >= extent) {
            return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                op_index,
                reason: "constant tensor coordinate is out of range",
            });
        }
    }
    Ok(())
}

fn tensor_scalar_count(
    op_index: usize,
    dimensions: &[u32],
) -> Result<usize, ScalarProgramRegisterError> {
    dimensions
        .iter()
        .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
        .ok_or(ScalarProgramRegisterError::InvalidTensorProjection {
            op_index,
            reason: "tensor projection dimensions overflow",
        })
}

/// The input state and seed registers of one deterministic random operation.
fn random_sources(cx: OpSources<'_>) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    match *cx.op {
        LinearOp::RandomInitialState {
            local_seed,
            global_seed,
            state_len,
            state_index,
            ..
        } => {
            cx.projection(state_index, state_len)?;
            two_sources(cx, local_seed, global_seed)
        }
        LinearOp::RandomResult {
            state_start,
            state_len,
            ..
        } => cx.require_range(state_start, state_len).map(Some),
        LinearOp::RandomState {
            state_start,
            state_len,
            state_index,
            ..
        } => {
            cx.projection(state_index, state_len)?;
            cx.require_range(state_start, state_len).map(Some)
        }
        LinearOp::ImpureRandomInit { seed, .. } => single_source(cx, seed),
        LinearOp::ImpureRandom { id, .. } => single_source(cx, id),
        LinearOp::ImpureRandomInteger { id, imin, imax, .. } => three_sources(cx, id, imin, imax),
        _ => unreachable!("random source validation requires a random operation"),
    }
}

/// The dense matrix and right-hand side of one linear-system component solve.
fn linear_solve_sources(
    cx: OpSources<'_>,
    matrix_start: Reg,
    rhs_start: Reg,
    n: usize,
    component: usize,
) -> Result<Reg, ScalarProgramRegisterError> {
    cx.projection(component, n)?;
    let matrix_len = n
        .checked_mul(n)
        .ok_or(ScalarProgramRegisterError::RegisterRangeOverflow {
            op_index: cx.op_index,
            operation: "LinearSolveComponent",
            start: matrix_start,
            len: n,
        })?;
    let matrix_end = cx.require_range(matrix_start, matrix_len)?;
    let rhs_end = cx.require_range(rhs_start, n)?;
    Ok(matrix_end.max(rhs_end))
}

/// The two strided operand ranges of one scalar dot product.
fn dot_product_sources(
    cx: OpSources<'_>,
    lhs_start: Reg,
    rhs_start: Reg,
    count: usize,
    lhs_stride: usize,
    rhs_stride: usize,
) -> Result<Reg, ScalarProgramRegisterError> {
    let lhs_end = require_strided_registers(
        cx.op_index,
        cx.operation(),
        lhs_start,
        count,
        lhs_stride,
        cx.initialized,
    )?;
    let rhs_end = require_strided_registers(
        cx.op_index,
        cx.operation(),
        rhs_start,
        count,
        rhs_stride,
        cx.initialized,
    )?;
    Ok(lhs_end.max(rhs_end))
}

fn require_strided_registers(
    op_index: usize,
    operation: &'static str,
    start: Reg,
    count: usize,
    stride: usize,
    initialized: &[bool],
) -> Result<Reg, ScalarProgramRegisterError> {
    let Some(last_term) = count.checked_sub(1) else {
        return Err(ScalarProgramRegisterError::EmptyRegisterRange {
            op_index,
            operation,
        });
    };
    let last = last_term
        .checked_mul(stride)
        .and_then(|offset| (start as usize).checked_add(offset))
        .and_then(|register| Reg::try_from(register).ok())
        .ok_or(ScalarProgramRegisterError::RegisterRangeOverflow {
            op_index,
            operation,
            start,
            len: count,
        })?;
    for term in 0..count {
        let register = start as usize + term * stride;
        require_register(op_index, operation, register as Reg, initialized)?;
    }
    Ok(last)
}

fn validate_projection(
    op_index: usize,
    operation: &'static str,
    projection: usize,
    len: usize,
) -> Result<(), ScalarProgramRegisterError> {
    if projection < len {
        return Ok(());
    }
    Err(ScalarProgramRegisterError::InvalidProjection {
        op_index,
        operation,
        projection,
        len,
    })
}

fn require_register(
    op_index: usize,
    operation: &'static str,
    register: Reg,
    initialized: &[bool],
) -> Result<(), ScalarProgramRegisterError> {
    if initialized.get(register as usize).copied().unwrap_or(false) {
        return Ok(());
    }
    Err(ScalarProgramRegisterError::UndefinedRegister {
        op_index,
        operation,
        register,
    })
}

fn require_register_range(
    op_index: usize,
    operation: &'static str,
    start: Reg,
    len: usize,
    initialized: &[bool],
) -> Result<Reg, ScalarProgramRegisterError> {
    let Some(last_offset) = len.checked_sub(1) else {
        return Err(ScalarProgramRegisterError::EmptyRegisterRange {
            op_index,
            operation,
        });
    };
    let last_offset = Reg::try_from(last_offset).map_err(|_| {
        ScalarProgramRegisterError::RegisterRangeOverflow {
            op_index,
            operation,
            start,
            len,
        }
    })?;
    let end = start.checked_add(last_offset).ok_or(
        ScalarProgramRegisterError::RegisterRangeOverflow {
            op_index,
            operation,
            start,
            len,
        },
    )?;
    for register in start..=end {
        if !initialized.get(register as usize).copied().unwrap_or(false) {
            return Err(ScalarProgramRegisterError::UndefinedRegister {
                op_index,
                operation,
                register,
            });
        }
    }
    Ok(end)
}

fn require_strided_register_range(
    op_index: usize,
    operation: &'static str,
    start: Reg,
    count: usize,
    stride: usize,
    initialized: &[bool],
) -> Result<Reg, ScalarProgramRegisterError> {
    let Some(last_ordinal) = count.checked_sub(1) else {
        return Err(ScalarProgramRegisterError::EmptyRegisterRange {
            op_index,
            operation,
        });
    };
    if stride == 0 {
        return Err(ScalarProgramRegisterError::InvalidFunctionConditional {
            op_index,
            reason: "output range stride is zero",
        });
    }
    let last_offset = last_ordinal.checked_mul(stride).ok_or(
        ScalarProgramRegisterError::RegisterRangeOverflow {
            op_index,
            operation,
            start,
            len: count,
        },
    )?;
    let last_offset = Reg::try_from(last_offset).map_err(|_| {
        ScalarProgramRegisterError::RegisterRangeOverflow {
            op_index,
            operation,
            start,
            len: count,
        }
    })?;
    let end = start.checked_add(last_offset).ok_or(
        ScalarProgramRegisterError::RegisterRangeOverflow {
            op_index,
            operation,
            start,
            len: count,
        },
    )?;
    for ordinal in 0..count {
        let register = start + Reg::try_from(ordinal * stride).expect("checked output range");
        require_register(op_index, operation, register, initialized)?;
    }
    Ok(end)
}

fn checked_register_count(max_register: Option<Reg>) -> Result<usize, ScalarProgramRegisterError> {
    let Some(register) = max_register else {
        return Ok(0);
    };
    usize::try_from(register)
        .ok()
        .and_then(|register| register.checked_add(1))
        .ok_or(ScalarProgramRegisterError::RegisterCountOverflow { register })
}

#[cfg(test)]
mod tests {
    use super::{
        BinaryOp, CompareOp, FoldInitialSource, FoldTensorUpdate, FunctionConditionalProgram,
        FunctionFoldProgram, LinearOp, TargetAssignmentShape, TensorInputKind, TensorSubscript,
        UnaryOp,
    };
    use rumoca_core::StructuredIndexDomain;
    use serde::de::DeserializeOwned;
    use std::sync::Arc;

    fn assert_missing_option_key_rejected<T>(
        mut value: serde_json::Value,
        object_pointer: &str,
        field: &str,
    ) where
        T: DeserializeOwned + std::fmt::Debug,
    {
        assert!(
            value
                .pointer_mut(object_pointer)
                .expect("current wire fixture object")
                .as_object_mut()
                .expect("current wire fixture is an object")
                .remove(field)
                .is_some(),
            "fixture must carry `{field}`"
        );
        let error = serde_json::from_value::<T>(value)
            .expect_err("deleting a current optional key must fail");
        assert!(
            error
                .to_string()
                .contains(&format!("missing field `{field}`")),
            "unexpected omission error for {field}: {error}"
        );
    }

    #[test]
    fn compare_op_equality_is_exact_not_epsilon_based() {
        let near_zero = f64::MIN_POSITIVE;

        assert!(!CompareOp::Eq.compare(0.0, near_zero));
        assert!(CompareOp::Ne.compare(0.0, near_zero));
        assert_eq!(CompareOp::Eq.compare_as_f64(0.0, near_zero), 0.0);
        assert_eq!(CompareOp::Ne.compare_as_f64(0.0, near_zero), 1.0);
    }

    #[test]
    fn linear_op_kind_name_reports_stable_variant_name() {
        let op = LinearOp::TableNextEvent {
            dst: 0,
            table_id: 1,
            time: 2,
        };

        assert_eq!(op.kind_name(), "TableNextEvent");
    }

    #[test]
    fn binary_op_kind_name_reports_stable_variant_name() {
        assert_eq!(BinaryOp::Atan2.kind_name(), "Atan2");
    }

    #[test]
    fn unary_op_kind_name_reports_stable_variant_name() {
        assert_eq!(UnaryOp::Log10.kind_name(), "Log10");
    }

    #[test]
    fn compare_op_kind_name_reports_stable_variant_name() {
        assert_eq!(CompareOp::Ne.kind_name(), "Ne");
    }

    #[test]
    fn linear_option_wire_keys_require_explicit_null() {
        let assignment = TargetAssignmentShape::Affine {
            target_y_index: 0,
            offset_reg: 1,
            coefficient_reg: None,
            offset_scale: 1.0,
            coefficient_scale: 1.0,
            expr_eval_len: 2,
        };
        assert_missing_option_key_rejected::<TargetAssignmentShape>(
            serde_json::to_value(assignment).expect("serialize affine assignment"),
            "/Affine",
            "coefficient_reg",
        );

        let update = FoldTensorUpdate {
            subscripts: vec![TensorSubscript::Whole].into(),
            condition: None,
            value_start: 0,
            value_stride: 1,
        };
        assert_missing_option_key_rejected::<FoldTensorUpdate>(
            serde_json::to_value(update).expect("serialize fold update"),
            "",
            "condition",
        );

        let region = || {
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ]
        };
        let conditional =
            FunctionConditionalProgram::checked(0, [1], [(region(), region())], region())
                .expect("the conditional wire fixture must satisfy its checked body contract");
        assert_missing_option_key_rejected::<FunctionConditionalProgram>(
            serde_json::to_value(conditional).expect("serialize conditional program"),
            "",
            "owner",
        );

        let tensor_load = LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 1,
            seed_start: None,
            lanes: 1,
        };
        assert_missing_option_key_rejected::<LinearOp>(
            serde_json::to_value(tensor_load).expect("serialize tensor load"),
            "/TensorLoad",
            "seed_start",
        );

        let fold = FunctionFoldProgram::checked(
            StructuredIndexDomain {
                binders: Vec::new(),
            },
            1,
            0,
            vec![
                LinearOp::LoadFoldCarried { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
        )
        .expect("the fold fixture must satisfy the checked carried-output contract");
        let nested_fold = LinearOp::StoreOutputFunctionFold {
            initial: vec![FoldInitialSource::Registers { start: 0, count: 1 }].into(),
            capture_start: 0,
            program: Arc::new(fold),
            result_base: 0,
            count: 1,
            condition: None,
            nested_when_true: false,
        };
        assert_missing_option_key_rejected::<LinearOp>(
            serde_json::to_value(nested_fold).expect("serialize nested fold"),
            "/StoreOutputFunctionFold",
            "condition",
        );
    }
}
