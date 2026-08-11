//! Linear register-based ops used by compiled evaluators.

use rumoca_core::StructuredIndexDomain;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::sync::Arc;

use crate::{SolvePureCallDirectionalSite, SolvePureCallSite};

/// Register index in a lowered op sequence.
pub type Reg = u32;

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
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionFoldProgram {
    pub domain: StructuredIndexDomain,
    pub carried_count: usize,
    pub capture_count: usize,
    pub register_count: usize,
    pub update: Vec<LinearOp>,
}

impl FunctionFoldProgram {
    pub fn checked(
        domain: StructuredIndexDomain,
        carried_count: usize,
        capture_count: usize,
        update: Vec<LinearOp>,
    ) -> Result<Self, ScalarProgramRegisterError> {
        let register_count = ScalarProgramRegisterFlow::derive_inner(
            &update,
            Some((carried_count, domain.binders.len(), capture_count)),
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
            carried_count,
            capture_count,
            register_count,
            update,
        })
    }

    pub fn register_flow(&self) -> Result<ScalarProgramRegisterFlow, ScalarProgramRegisterError> {
        let flow = ScalarProgramRegisterFlow::derive_inner(
            &self.update,
            Some((
                self.carried_count,
                self.domain.binders.len(),
                self.capture_count,
            )),
            None,
        )?;
        if flow.register_count() != self.register_count {
            return Err(ScalarProgramRegisterError::InvalidFunctionFold {
                op_index: 0,
                reason: "stored update register count does not match its body",
            });
        }
        Ok(flow)
    }
}

/// One checked condition and its lazily selected correlated result region.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionConditionalArmProgram {
    pub condition_register_count: usize,
    pub result_register_count: usize,
    pub condition: Vec<LinearOp>,
    pub result: Vec<LinearOp>,
}

/// Compact execution owner for one DAE multi-target function conditional.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionConditionalProgram {
    pub owner: Option<FunctionConditionalOwnerId>,
    pub capture_count: usize,
    pub target_widths: Box<[usize]>,
    pub result_count: usize,
    pub arms: Box<[FunctionConditionalArmProgram]>,
    pub fallback_register_count: usize,
    pub fallback: Vec<LinearOp>,
}

/// Block-local identity issued only for an exact reusable function call frame.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct FunctionConditionalOwnerId(u64);

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
        let mut validation = ScalarProgramValidationCache::default();
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
                let condition_register_count = ScalarProgramRegisterFlow::derive_inner_with_cache(
                    &condition,
                    None,
                    Some(capture_count),
                    &mut validation,
                )?
                .register_count();
                require_conditional_output_count(&condition, 1, "condition")?;
                let result_register_count = ScalarProgramRegisterFlow::derive_inner_with_cache(
                    &result,
                    None,
                    Some(capture_count),
                    &mut validation,
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
        let fallback_register_count = ScalarProgramRegisterFlow::derive_inner_with_cache(
            &fallback,
            None,
            Some(capture_count),
            &mut validation,
        )?
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

    pub fn validate(&self) -> Result<(), ScalarProgramRegisterError> {
        self.validate_with_cache(&mut ScalarProgramValidationCache::default())
    }

    fn validate_with_cache(
        &self,
        validation: &mut ScalarProgramValidationCache,
    ) -> Result<(), ScalarProgramRegisterError> {
        if !validation.begin_conditional(self) {
            return Ok(());
        }
        if self.target_widths.is_empty() || self.target_widths.contains(&0) {
            return Err(ScalarProgramRegisterError::InvalidFunctionConditional {
                op_index: 0,
                reason: "target tuple is empty or contains an empty value",
            });
        }
        let result_count = self.target_widths.iter().try_fold(0usize, |count, width| {
            count.checked_add(*width).ok_or(
                ScalarProgramRegisterError::InvalidFunctionConditional {
                    op_index: 0,
                    reason: "target tuple width overflows",
                },
            )
        })?;
        if result_count != self.result_count {
            return Err(ScalarProgramRegisterError::InvalidFunctionConditional {
                op_index: 0,
                reason: "stored result width does not match the target tuple",
            });
        }
        if self.arms.is_empty() {
            return Err(ScalarProgramRegisterError::InvalidFunctionConditional {
                op_index: 0,
                reason: "ordered condition list is empty",
            });
        }
        for arm in &self.arms {
            let condition_register_count = ScalarProgramRegisterFlow::derive_inner_with_cache(
                &arm.condition,
                None,
                Some(self.capture_count),
                validation,
            )?
            .register_count();
            require_conditional_output_count(&arm.condition, 1, "condition")?;
            let result_register_count = ScalarProgramRegisterFlow::derive_inner_with_cache(
                &arm.result,
                None,
                Some(self.capture_count),
                validation,
            )?
            .register_count();
            require_conditional_output_count(&arm.result, self.result_count, "branch result")?;
            if condition_register_count != arm.condition_register_count
                || result_register_count != arm.result_register_count
            {
                return Err(ScalarProgramRegisterError::InvalidFunctionConditional {
                    op_index: 0,
                    reason: "stored branch register capacity does not match its region",
                });
            }
        }
        let fallback_register_count = ScalarProgramRegisterFlow::derive_inner_with_cache(
            &self.fallback,
            None,
            Some(self.capture_count),
            validation,
        )?
        .register_count();
        require_conditional_output_count(&self.fallback, self.result_count, "fallback result")?;
        if fallback_register_count != self.fallback_register_count {
            return Err(ScalarProgramRegisterError::InvalidFunctionConditional {
                op_index: 0,
                reason: "stored fallback register capacity does not match its region",
            });
        }
        Ok(())
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
    /// Runtime-indexed parameter load: `p[base + clamp(round(index), 0, count-1)]`.
    ///
    /// Lowering emits this in place of an N-deep `(idx==k ? p[slot_k] : prev)`
    /// select chain when a dynamic array subscript resolves to a contiguous,
    /// row-major run of parameter slots (`base..base+count`). `index` is a
    /// register holding the 0-based flat offset; it is rounded and clamped at
    /// evaluation so an in-range model index is exact and out-of-range is
    /// saturated rather than silently zero.
    LoadIndexedP {
        dst: Reg,
        base: usize,
        count: usize,
        index: Reg,
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
    /// Runtime-indexed AD seed load: `seed[base + clamp(round(index), 0, count-1)]`.
    ///
    /// Forward-mode dual of [`LinearOp::LoadIndexedP`] under parameter-seed AD
    /// (`SeedMode::SolverYAndP`): the loaded parameter's tangent is the seed at
    /// the same runtime offset, shifted into the seed region.
    LoadIndexedSeed {
        dst: Reg,
        base: usize,
        count: usize,
        index: Reg,
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
    /// Storage is element-major with `lanes` adjacent values per element.
    /// One lane is primal evaluation; two lanes are the interleaved
    /// primal/tangent representation used by forward AD. The operation count
    /// is independent of the matrix extents.
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

/// Resolve a runtime flat offset register value to an absolute slot in a
/// contiguous `[base, base+count)` run, with the round-then-clamp semantics
/// shared by [`LinearOp::LoadIndexedP`] / [`LinearOp::LoadIndexedSeed`] across
/// the interpreter, JIT, and every codegen backend. `count == 0` is degenerate
/// and saturates to `base`.
#[must_use]
pub fn resolve_indexed_slot(index_value: f64, base: usize, count: usize) -> usize {
    if count == 0 {
        return base;
    }
    let rounded = index_value.round();
    let clamped = if rounded < 0.0 {
        0
    } else if rounded as usize >= count {
        count - 1
    } else {
        rounded as usize
    };
    base + clamped
}

impl LinearOp {
    #[must_use]
    pub fn kind_name(&self) -> &'static str {
        match self {
            Self::Const { .. } => "Const",
            Self::LoadTime { .. } => "LoadTime",
            Self::LoadY { .. } => "LoadY",
            Self::LoadP { .. } => "LoadP",
            Self::LoadIndexedP { .. } => "LoadIndexedP",
            Self::LoadIndexedRegister { .. } => "LoadIndexedRegister",
            Self::LoadIndexedFoldCarried { .. } => "LoadIndexedFoldCarried",
            Self::LoadIndexedFoldCapture { .. } => "LoadIndexedFoldCapture",
            Self::LoadSeed { .. } => "LoadSeed",
            Self::LoadIndexedSeed { .. } => "LoadIndexedSeed",
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
            | Self::LoadIndexedP { dst, .. }
            | Self::LoadIndexedRegister { dst, .. }
            | Self::LoadIndexedFoldCarried { dst, .. }
            | Self::LoadIndexedFoldCapture { dst, .. }
            | Self::LoadSeed { dst, .. }
            | Self::LoadIndexedSeed { dst, .. }
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

/// Construction-local proof context for immutable conditional program owners.
///
/// Pointer identity is sufficient here: lowering shares exact owners through
/// `Arc`, and checked wire replay reconstructs and validates each stored body.
/// Semantic owner-id uniqueness is proved separately by the enclosing block.
#[derive(Default)]
pub(crate) struct ScalarProgramValidationCache {
    conditional_programs: HashSet<ConditionalValidationKey>,
    use_owner_ids: bool,
}

impl ScalarProgramValidationCache {
    pub(crate) fn for_checked_owner_table() -> Self {
        Self {
            conditional_programs: HashSet::new(),
            use_owner_ids: true,
        }
    }

    fn begin_conditional(&mut self, program: &FunctionConditionalProgram) -> bool {
        let key = if self.use_owner_ids {
            program.owner.map_or_else(
                || ConditionalValidationKey::Pointer(std::ptr::from_ref(program) as usize),
                ConditionalValidationKey::Owner,
            )
        } else {
            ConditionalValidationKey::Pointer(std::ptr::from_ref(program) as usize)
        };
        self.conditional_programs.insert(key)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ConditionalValidationKey {
    Owner(FunctionConditionalOwnerId),
    Pointer(usize),
}

impl ScalarProgramRegisterFlow {
    pub fn derive(program: &[LinearOp]) -> Result<Self, ScalarProgramRegisterError> {
        Self::derive_inner_with_cache(
            program,
            None,
            None,
            &mut ScalarProgramValidationCache::default(),
        )
    }

    fn derive_inner(
        program: &[LinearOp],
        fold_context: Option<(usize, usize, usize)>,
        conditional_capture_count: Option<usize>,
    ) -> Result<Self, ScalarProgramRegisterError> {
        Self::derive_inner_with_cache(
            program,
            fold_context,
            conditional_capture_count,
            &mut ScalarProgramValidationCache::default(),
        )
    }

    pub(crate) fn derive_with_cache(
        program: &[LinearOp],
        validation: &mut ScalarProgramValidationCache,
    ) -> Result<Self, ScalarProgramRegisterError> {
        Self::derive_inner_with_cache(program, None, None, validation)
    }

    fn derive_inner_with_cache(
        program: &[LinearOp],
        fold_context: Option<(usize, usize, usize)>,
        conditional_capture_count: Option<usize>,
        validation: &mut ScalarProgramValidationCache,
    ) -> Result<Self, ScalarProgramRegisterError> {
        let mut initialized = Vec::new();
        let mut max_register = None;
        for (op_index, op) in program.iter().enumerate() {
            if let Some(register) = validate_op_sources(
                op,
                op_index,
                &initialized,
                fold_context,
                conditional_capture_count,
                validation,
            )? {
                max_register = Some(max_register.map_or(register, |max: Reg| max.max(register)));
            }
            let dst_count = op.dst_register_count();
            if let Some(dst) = op.dst_register() {
                let last = register_range_last(op_index, op.kind_name(), dst, dst_count)?;
                for offset in 0..dst_count {
                    mark_register_initialized(&mut initialized, dst + offset as Reg);
                }
                max_register = Some(max_register.map_or(last, |max: Reg| max.max(last)));
            }
        }
        Ok(Self {
            register_count: checked_register_count(max_register)?,
        })
    }

    pub const fn register_count(self) -> usize {
        self.register_count
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

fn validate_op_sources(
    op: &LinearOp,
    op_index: usize,
    initialized: &[bool],
    fold_context: Option<(usize, usize, usize)>,
    conditional_capture_count: Option<usize>,
    validation: &mut ScalarProgramValidationCache,
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    match *op {
        LinearOp::Const { .. }
        | LinearOp::LoadTime { .. }
        | LinearOp::LoadY { .. }
        | LinearOp::LoadP { .. }
        | LinearOp::LoadSeed { .. } => Ok(None),
        LinearOp::LoadFoldCarried { index, .. } => {
            let Some((carried_count, _, _)) = fold_context else {
                return Err(ScalarProgramRegisterError::InvalidFunctionFold {
                    op_index,
                    reason: "carried load escaped its update body",
                });
            };
            validate_projection(op_index, op.kind_name(), index, carried_count)?;
            Ok(None)
        }
        LinearOp::LoadFoldIndex { dimension, .. } => {
            let Some((_, dimension_count, _)) = fold_context else {
                return Err(ScalarProgramRegisterError::InvalidFunctionFold {
                    op_index,
                    reason: "binder load escaped its update body",
                });
            };
            validate_projection(op_index, op.kind_name(), dimension, dimension_count)?;
            Ok(None)
        }
        LinearOp::LoadFoldCapture { index, .. } => {
            let Some((_, _, capture_count)) = fold_context else {
                return Err(ScalarProgramRegisterError::InvalidFunctionFold {
                    op_index,
                    reason: "capture load escaped its update body",
                });
            };
            validate_projection(op_index, op.kind_name(), index, capture_count)?;
            Ok(None)
        }
        LinearOp::LoadFunctionConditionalCapture { index, .. } => {
            let Some(capture_count) = conditional_capture_count else {
                return Err(ScalarProgramRegisterError::InvalidFunctionConditional {
                    op_index,
                    reason: "capture load escaped its conditional region",
                });
            };
            validate_projection(op_index, op.kind_name(), index, capture_count)?;
            Ok(None)
        }
        LinearOp::LoadFunctionConditionalCaptureRange {
            index_start, count, ..
        } => {
            let Some(capture_count) = conditional_capture_count else {
                return Err(ScalarProgramRegisterError::InvalidFunctionConditional {
                    op_index,
                    reason: "capture range load escaped its conditional region",
                });
            };
            if count == 0
                || index_start
                    .checked_add(count)
                    .is_none_or(|end| end > capture_count)
            {
                return Err(ScalarProgramRegisterError::InvalidFunctionConditional {
                    op_index,
                    reason: "capture range load is empty, overflows, or exceeds the capture ABI",
                });
            }
            Ok(None)
        }
        LinearOp::Move { src, .. }
        | LinearOp::Unary { arg: src, .. }
        | LinearOp::LoadIndexedP { index: src, .. }
        | LinearOp::LoadIndexedSeed { index: src, .. }
        | LinearOp::StoreOutput { src } => {
            require_register(op_index, op.kind_name(), src, initialized)?;
            Ok(Some(src))
        }
        LinearOp::StoreOutputRange {
            start,
            count,
            stride,
        } => require_strided_register_range(
            op_index,
            op.kind_name(),
            start,
            count,
            stride,
            initialized,
        )
        .map(Some),
        LinearOp::LoadIndexedRegister {
            base,
            stride,
            ref dimensions,
            ref indices,
            ..
        } => {
            if dimensions.is_empty() || dimensions.len() != indices.len() {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor projection rank does not match its checked dimensions",
                });
            }
            let count = dimensions.iter().try_fold(1usize, |count, &extent| {
                (extent != 0)
                    .then(|| count.checked_mul(extent as usize))
                    .flatten()
            });
            let Some(count) = count else {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor projection dimensions are empty or overflow",
                });
            };
            if stride == 0 {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor projection register stride is zero",
                });
            }
            let last_offset = (count - 1).checked_mul(stride).ok_or(
                ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor projection register stride overflows",
                },
            )?;
            let last_offset = Reg::try_from(last_offset).map_err(|_| {
                ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor projection register stride overflows",
                }
            })?;
            let base_last = base.checked_add(last_offset).ok_or(
                ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor projection register range overflows",
                },
            )?;
            for offset in 0..count {
                require_register(
                    op_index,
                    "LoadIndexedRegister",
                    base + (offset * stride) as Reg,
                    initialized,
                )?;
            }
            let mut source_last = base_last;
            for (&extent, index) in dimensions.iter().zip(indices.iter()) {
                match *index {
                    TensorIndex::Constant(coordinate) if coordinate >= extent => {
                        return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                            op_index,
                            reason: "constant tensor coordinate is out of range",
                        });
                    }
                    TensorIndex::Constant(_) => {}
                    TensorIndex::Runtime(register) => {
                        require_register(op_index, "LoadIndexedRegister", register, initialized)?;
                        source_last = source_last.max(register);
                    }
                }
            }
            Ok(Some(source_last))
        }
        LinearOp::LoadIndexedFoldCarried {
            base,
            stride,
            ref dimensions,
            ref indices,
            ..
        } => {
            let Some((carried_count, _, _)) = fold_context else {
                return Err(ScalarProgramRegisterError::InvalidFunctionFold {
                    op_index,
                    reason: "indexed carried load escaped its update body",
                });
            };
            validate_tensor_projection_shape(op_index, dimensions.as_ref(), indices.as_ref())?;
            let count = tensor_scalar_count(op_index, dimensions.as_ref())?;
            let last = base
                .checked_add((count - 1).checked_mul(stride).ok_or(
                    ScalarProgramRegisterError::InvalidTensorProjection {
                        op_index,
                        reason: "indexed carried projection stride overflows",
                    },
                )?)
                .ok_or(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "indexed carried projection range overflows",
                })?;
            if stride == 0 || last >= carried_count {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "indexed carried projection is outside its tuple",
                });
            }
            let mut source_last = None;
            for index in indices.iter() {
                if let TensorIndex::Runtime(register) = *index {
                    require_register(op_index, "LoadIndexedFoldCarried", register, initialized)?;
                    source_last =
                        Some(source_last.map_or(register, |last: Reg| last.max(register)));
                }
            }
            Ok(source_last)
        }
        LinearOp::LoadIndexedFoldCapture {
            base,
            stride,
            ref dimensions,
            ref indices,
            ..
        } => {
            let Some((_, _, capture_count)) = fold_context else {
                return Err(ScalarProgramRegisterError::InvalidFunctionFold {
                    op_index,
                    reason: "indexed capture load escaped its update body",
                });
            };
            validate_tensor_projection_shape(op_index, dimensions.as_ref(), indices.as_ref())?;
            let count = tensor_scalar_count(op_index, dimensions.as_ref())?;
            let last = base
                .checked_add((count - 1).checked_mul(stride).ok_or(
                    ScalarProgramRegisterError::InvalidTensorProjection {
                        op_index,
                        reason: "indexed capture projection stride overflows",
                    },
                )?)
                .ok_or(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "indexed capture projection range overflows",
                })?;
            if stride == 0 || last >= capture_count {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "indexed capture projection is outside its tuple",
                });
            }
            let mut source_last = None;
            for index in indices.iter() {
                if let TensorIndex::Runtime(register) = *index {
                    require_register(op_index, "LoadIndexedFoldCapture", register, initialized)?;
                    source_last =
                        Some(source_last.map_or(register, |last: Reg| last.max(register)));
                }
            }
            Ok(source_last)
        }
        LinearOp::StoreOutputFoldTensorUpdate {
            source_base,
            source_stride,
            ref dimensions,
            ref updates,
            ref nodes,
            result,
            lanes,
        } => {
            let Some((carried_count, _, _)) = fold_context else {
                return Err(ScalarProgramRegisterError::InvalidFunctionFold {
                    op_index,
                    reason: "aggregate output escaped its function-fold update body",
                });
            };
            if dimensions.is_empty() || updates.is_empty() {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor update has no checked rank or patches",
                });
            }
            if lanes == 0 || source_stride != lanes {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor update storage is not element-major by lane",
                });
            }
            let count = tensor_scalar_count(op_index, dimensions.as_ref())?;
            let source_last = source_base
                .checked_add((count - 1).checked_mul(source_stride).ok_or(
                    ScalarProgramRegisterError::InvalidTensorProjection {
                        op_index,
                        reason: "tensor update carried stride overflows",
                    },
                )?)
                .and_then(|last| last.checked_add(lanes - 1))
                .ok_or(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor update carried range overflows",
                })?;
            if source_last >= carried_count {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor update source is outside its carried tuple",
                });
            }
            let mut max_register = None;
            for update in updates {
                if dimensions.len() != update.subscripts.len() || update.value_stride != lanes {
                    return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                        op_index,
                        reason: "tensor patch shape or lane layout is invalid",
                    });
                }
                let mut value_count = 1usize;
                if let Some(condition) = update.condition {
                    require_register(
                        op_index,
                        "StoreOutputFoldTensorUpdate",
                        condition,
                        initialized,
                    )?;
                    max_register =
                        Some(max_register.map_or(condition, |last: Reg| last.max(condition)));
                }
                for (&extent, subscript) in dimensions.iter().zip(update.subscripts.iter()) {
                    match *subscript {
                        TensorSubscript::Whole => {
                            value_count = value_count.checked_mul(extent as usize).ok_or(
                                ScalarProgramRegisterError::InvalidTensorProjection {
                                    op_index,
                                    reason: "tensor update value extent overflows",
                                },
                            )?;
                        }
                        TensorSubscript::Index(TensorIndex::Constant(coordinate)) => {
                            if coordinate >= extent {
                                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                                    op_index,
                                    reason: "constant tensor update coordinate is out of range",
                                });
                            }
                        }
                        TensorSubscript::Index(TensorIndex::Runtime(register)) => {
                            require_register(
                                op_index,
                                "StoreOutputFoldTensorUpdate",
                                register,
                                initialized,
                            )?;
                            max_register =
                                Some(max_register.map_or(register, |last: Reg| last.max(register)));
                        }
                    }
                }
                let value_last_offset = (value_count - 1)
                    .checked_mul(update.value_stride)
                    .and_then(|offset| offset.checked_add(lanes - 1))
                    .ok_or(ScalarProgramRegisterError::InvalidTensorProjection {
                        op_index,
                        reason: "tensor update value range overflows",
                    })?;
                let value_last_offset = Reg::try_from(value_last_offset).map_err(|_| {
                    ScalarProgramRegisterError::InvalidTensorProjection {
                        op_index,
                        reason: "tensor update value range exceeds register identity",
                    }
                })?;
                let value_last = update.value_start.checked_add(value_last_offset).ok_or(
                    ScalarProgramRegisterError::InvalidTensorProjection {
                        op_index,
                        reason: "tensor update value register range overflows",
                    },
                )?;
                for element in 0..value_count {
                    for lane in 0..lanes {
                        require_register(
                            op_index,
                            "StoreOutputFoldTensorUpdate",
                            update.value_start + (element * update.value_stride + lane) as Reg,
                            initialized,
                        )?;
                    }
                }
                max_register = Some(max_register.map_or(value_last, |last| last.max(value_last)));
            }
            for (index, node) in nodes.iter().enumerate() {
                let current = u32::try_from(index + 1).map_err(|_| {
                    ScalarProgramRegisterError::InvalidTensorProjection {
                        op_index,
                        reason: "tensor expression node identity overflows",
                    }
                })?;
                match *node {
                    FoldTensorNode::Update { base, update } => {
                        if base >= current || update as usize >= updates.len() {
                            return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                                op_index,
                                reason: "tensor update node is not topological",
                            });
                        }
                    }
                    FoldTensorNode::Select {
                        condition,
                        if_true,
                        if_false,
                    } => {
                        if if_true >= current || if_false >= current {
                            return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                                op_index,
                                reason: "tensor select node is not topological",
                            });
                        }
                        require_register(
                            op_index,
                            "StoreOutputFoldTensorUpdate",
                            condition,
                            initialized,
                        )?;
                        max_register =
                            Some(max_register.map_or(condition, |last| last.max(condition)));
                    }
                }
            }
            if result == 0 || result as usize > nodes.len() {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor expression result does not name a stored node",
                });
            }
            Ok(max_register)
        }
        LinearOp::StoreOutputFunctionFold {
            ref initial,
            capture_start,
            ref program,
            result_base,
            count,
            condition,
            ..
        } => {
            let Some((parent_carried, _, _)) = fold_context else {
                return Err(ScalarProgramRegisterError::InvalidFunctionFold {
                    op_index,
                    reason: "nested aggregate fold escaped its parent update body",
                });
            };
            let mut initial_count = 0usize;
            let mut max_register = None;
            for source in initial {
                let source_count = match *source {
                    FoldInitialSource::Registers { start, count } => {
                        if count != 0 {
                            let last = register_range_last(
                                op_index,
                                "StoreOutputFunctionFold",
                                start,
                                count,
                            )?;
                            for offset in 0..count {
                                require_register(
                                    op_index,
                                    "StoreOutputFunctionFold",
                                    start + offset as Reg,
                                    initialized,
                                )?;
                            }
                            max_register =
                                Some(max_register.map_or(last, |current: Reg| current.max(last)));
                        }
                        count
                    }
                    FoldInitialSource::ParentCarried { base, count } => {
                        if base
                            .checked_add(count)
                            .is_none_or(|end| end > parent_carried)
                        {
                            return Err(ScalarProgramRegisterError::InvalidFunctionFold {
                                op_index,
                                reason: "nested fold parent-carried initial source is invalid",
                            });
                        }
                        count
                    }
                };
                initial_count = initial_count.checked_add(source_count).ok_or(
                    ScalarProgramRegisterError::InvalidFunctionFold {
                        op_index,
                        reason: "nested fold initial source count overflows",
                    },
                )?;
            }
            if initial_count != program.carried_count
                || count == 0
                || result_base
                    .checked_add(count)
                    .is_none_or(|end| end > program.carried_count)
            {
                return Err(ScalarProgramRegisterError::InvalidFunctionFold {
                    op_index,
                    reason: "nested fold initial/result layout is invalid",
                });
            }
            if program.capture_count != 0 {
                let last = register_range_last(
                    op_index,
                    "StoreOutputFunctionFold",
                    capture_start,
                    program.capture_count,
                )?;
                for offset in 0..program.capture_count {
                    require_register(
                        op_index,
                        "StoreOutputFunctionFold",
                        capture_start + offset as Reg,
                        initialized,
                    )?;
                }
                max_register = Some(max_register.map_or(last, |current: Reg| current.max(last)));
            }
            if let Some(condition) = condition {
                require_register(op_index, "StoreOutputFunctionFold", condition, initialized)?;
                max_register =
                    Some(max_register.map_or(condition, |current: Reg| current.max(condition)));
            }
            Ok(max_register)
        }
        LinearOp::FunctionConditional {
            capture_start,
            ref program,
            ..
        } => {
            program
                .validate_with_cache(validation)
                .map_err(|error| match error {
                    ScalarProgramRegisterError::InvalidFunctionConditional { reason, .. } => {
                        ScalarProgramRegisterError::InvalidFunctionConditional { op_index, reason }
                    }
                    other => other,
                })?;
            if program.capture_count == 0 {
                return Ok(None);
            }
            require_register_range(
                op_index,
                "FunctionConditional",
                capture_start,
                program.capture_count,
                initialized,
            )
            .map(Some)
        }
        LinearOp::PureCall {
            ref input_starts,
            ref site,
            ..
        } => {
            if input_starts.len() != site.inputs().len()
                || site.output_scalar_count().is_none_or(|count| count == 0)
            {
                return Err(ScalarProgramRegisterError::InvalidPureCall {
                    op_index,
                    reason: "typed call interface has invalid input or output width",
                });
            }
            let mut last = None;
            for (&start, value_type) in input_starts.iter().zip(site.inputs()) {
                let count = value_type.scalar_count() as usize;
                require_register_range(op_index, "PureCall", start, count, initialized)?;
                let range_last = register_range_last(op_index, "PureCall", start, count)?;
                last = Some(last.map_or(range_last, |current: Reg| current.max(range_last)));
            }
            Ok(last)
        }
        LinearOp::PureCallDirectional {
            ref input_starts,
            ref site,
            ..
        } => {
            if input_starts.len() != site.inputs().len()
                || site.output_scalar_count().is_none_or(|count| count == 0)
            {
                return Err(ScalarProgramRegisterError::InvalidPureCall {
                    op_index,
                    reason: "typed directional call interface has invalid input or output width",
                });
            }
            let mut last = None;
            for (&start, value_type) in input_starts.iter().zip(site.inputs()) {
                let count = value_type.scalar_count() as usize;
                require_register_range(op_index, "PureCallDirectional", start, count, initialized)?;
                let range_last =
                    register_range_last(op_index, "PureCallDirectional", start, count)?;
                last = Some(last.map_or(range_last, |current: Reg| current.max(range_last)));
            }
            Ok(last)
        }
        LinearOp::Binary { lhs, rhs, .. } | LinearOp::Compare { lhs, rhs, .. } => {
            require_register(op_index, op.kind_name(), lhs, initialized)?;
            require_register(op_index, op.kind_name(), rhs, initialized)?;
            Ok(Some(lhs.max(rhs)))
        }
        LinearOp::Select {
            cond,
            if_true,
            if_false,
            ..
        } => {
            require_register(op_index, op.kind_name(), cond, initialized)?;
            require_register(op_index, op.kind_name(), if_true, initialized)?;
            require_register(op_index, op.kind_name(), if_false, initialized)?;
            Ok(Some(cond.max(if_true).max(if_false)))
        }
        LinearOp::LinearSolveComponent {
            matrix_start,
            rhs_start,
            n,
            component,
            ..
        } => linear_solve_sources(op_index, matrix_start, rhs_start, n, component, initialized)
            .map(Some),
        LinearOp::DotProduct {
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            ..
        } => dot_product_sources(
            op_index,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            initialized,
        )
        .map(Some),
        LinearOp::MatrixMultiply {
            lhs_start,
            rhs_start,
            rows,
            inner,
            columns,
            lanes,
            ..
        } => {
            if rows == 0 || inner == 0 || columns == 0 || lanes == 0 || lanes > 2 {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "matrix multiply has an invalid shape or lane count",
                });
            }
            let lhs_count = rows
                .checked_mul(inner)
                .and_then(|count| count.checked_mul(lanes))
                .ok_or(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "matrix multiply lhs range overflows",
                })?;
            let rhs_count = inner
                .checked_mul(columns)
                .and_then(|count| count.checked_mul(lanes))
                .ok_or(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "matrix multiply rhs range overflows",
                })?;
            require_register_range(
                op_index,
                "MatrixMultiply",
                lhs_start,
                lhs_count,
                initialized,
            )?;
            require_register_range(
                op_index,
                "MatrixMultiply",
                rhs_start,
                rhs_count,
                initialized,
            )?;
            Ok(Some(
                register_range_last(op_index, "MatrixMultiply", lhs_start, lhs_count)?.max(
                    register_range_last(op_index, "MatrixMultiply", rhs_start, rhs_count)?,
                ),
            ))
        }
        LinearOp::TensorBinary {
            op,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes,
            ..
        } => {
            if count == 0
                || lanes == 0
                || lanes > 2
                || !matches!(
                    op,
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div
                )
            {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor binary has an invalid operator, extent, or lane count",
                });
            }
            let source_count = |stride: usize| {
                count
                    .saturating_sub(1)
                    .checked_mul(stride)
                    .and_then(|last| last.checked_add(1))
                    .and_then(|elements| elements.checked_mul(lanes))
            };
            let lhs_count = source_count(lhs_stride).ok_or(
                ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor binary lhs range overflows",
                },
            )?;
            let rhs_count = source_count(rhs_stride).ok_or(
                ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor binary rhs range overflows",
                },
            )?;
            require_register_range(op_index, "TensorBinary", lhs_start, lhs_count, initialized)?;
            require_register_range(op_index, "TensorBinary", rhs_start, rhs_count, initialized)?;
            Ok(Some(
                register_range_last(op_index, "TensorBinary", lhs_start, lhs_count)?.max(
                    register_range_last(op_index, "TensorBinary", rhs_start, rhs_count)?,
                ),
            ))
        }
        LinearOp::TensorCross {
            lhs_start,
            rhs_start,
            lanes,
            ..
        } => {
            if lanes == 0 || lanes > 2 {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor cross product has an invalid lane count",
                });
            }
            let count = 3usize.checked_mul(lanes).ok_or(
                ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor cross product input range overflows",
                },
            )?;
            require_register_range(op_index, "TensorCross", lhs_start, count, initialized)?;
            require_register_range(op_index, "TensorCross", rhs_start, count, initialized)?;
            Ok(Some(
                register_range_last(op_index, "TensorCross", lhs_start, count)?.max(
                    register_range_last(op_index, "TensorCross", rhs_start, count)?,
                ),
            ))
        }
        LinearOp::TensorTranspose {
            src_start,
            rows,
            columns,
            element_width,
            lanes,
            ..
        } => {
            let count = rows
                .checked_mul(columns)
                .and_then(|count| count.checked_mul(element_width))
                .and_then(|count| count.checked_mul(lanes))
                .ok_or(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor transpose range overflows",
                })?;
            if rows == 0 || columns == 0 || element_width == 0 || lanes == 0 || lanes > 2 {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor transpose has an invalid shape, element width, or lane count",
                });
            }
            require_register_range(op_index, "TensorTranspose", src_start, count, initialized)?;
            register_range_last(op_index, "TensorTranspose", src_start, count).map(Some)
        }
        LinearOp::TensorConcatenate {
            ref sources,
            ref dimensions,
            axis,
            lanes,
            ..
        } => {
            if sources.is_empty()
                || dimensions.is_empty()
                || axis >= dimensions.len()
                || lanes == 0
                || lanes > 2
            {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor concatenate has an invalid shape, axis, or lane count",
                });
            }
            let mut axis_extent = 0u32;
            let mut last = 0;
            for source in sources.iter() {
                if source.dimensions.len() != dimensions.len()
                    || source
                        .dimensions
                        .iter()
                        .zip(dimensions.iter())
                        .enumerate()
                        .any(|(dimension, (source, result))| dimension != axis && source != result)
                {
                    return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                        op_index,
                        reason: "tensor concatenate source shape is incompatible",
                    });
                }
                axis_extent = axis_extent.checked_add(source.dimensions[axis]).ok_or(
                    ScalarProgramRegisterError::InvalidTensorProjection {
                        op_index,
                        reason: "tensor concatenate axis extent overflows",
                    },
                )?;
                let count = source
                    .dimensions
                    .iter()
                    .try_fold(lanes, |count, extent| count.checked_mul(*extent as usize))
                    .ok_or(ScalarProgramRegisterError::InvalidTensorProjection {
                        op_index,
                        reason: "tensor concatenate source extent overflows",
                    })?;
                require_register_range(
                    op_index,
                    "TensorConcatenate",
                    source.start,
                    count,
                    initialized,
                )?;
                last = last.max(register_range_last(
                    op_index,
                    "TensorConcatenate",
                    source.start,
                    count,
                )?);
            }
            if axis_extent != dimensions[axis] {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor concatenate sources do not cover the result axis",
                });
            }
            Ok(Some(last))
        }
        LinearOp::TensorUpdate {
            base_start,
            value_start,
            ref dimensions,
            ref subscripts,
            lanes,
            ..
        } => {
            if dimensions.is_empty()
                || dimensions.len() != subscripts.len()
                || lanes == 0
                || lanes > 2
                || dimensions.contains(&0)
            {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor update has an invalid shape, projection, or lane count",
                });
            }
            let base_count = dimensions
                .iter()
                .try_fold(lanes, |count, extent| count.checked_mul(*extent as usize))
                .ok_or(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor update base extent overflows",
                })?;
            require_register_range(
                op_index,
                "TensorUpdate",
                base_start,
                base_count,
                initialized,
            )?;
            let mut last = register_range_last(op_index, "TensorUpdate", base_start, base_count)?;
            let mut value_count = lanes;
            for (&extent, subscript) in dimensions.iter().zip(subscripts.iter()) {
                match subscript {
                    TensorUpdateSubscript::Whole => {
                        value_count = value_count.checked_mul(extent as usize).ok_or(
                            ScalarProgramRegisterError::InvalidTensorProjection {
                                op_index,
                                reason: "tensor update value extent overflows",
                            },
                        )?;
                    }
                    TensorUpdateSubscript::Index(TensorIndex::Constant(coordinate)) => {
                        if *coordinate >= extent {
                            return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                                op_index,
                                reason: "tensor update constant index is out of range",
                            });
                        }
                    }
                    TensorUpdateSubscript::Index(TensorIndex::Runtime(register)) => {
                        require_register(op_index, "TensorUpdate", *register, initialized)?;
                        last = last.max(*register);
                    }
                    TensorUpdateSubscript::Slice { start, dimensions } => {
                        if dimensions.is_empty() || dimensions.contains(&0) {
                            return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                                op_index,
                                reason: "tensor update slice has an invalid shape",
                            });
                        }
                        let count = dimensions
                            .iter()
                            .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
                            .ok_or(ScalarProgramRegisterError::InvalidTensorProjection {
                                op_index,
                                reason: "tensor update slice extent overflows",
                            })?;
                        require_register_range(
                            op_index,
                            "TensorUpdate",
                            *start,
                            count,
                            initialized,
                        )?;
                        last = last.max(register_range_last(
                            op_index,
                            "TensorUpdate",
                            *start,
                            count,
                        )?);
                        value_count = value_count.checked_mul(count).ok_or(
                            ScalarProgramRegisterError::InvalidTensorProjection {
                                op_index,
                                reason: "tensor update sliced value extent overflows",
                            },
                        )?;
                    }
                }
            }
            require_register_range(
                op_index,
                "TensorUpdate",
                value_start,
                value_count,
                initialized,
            )?;
            last = last.max(register_range_last(
                op_index,
                "TensorUpdate",
                value_start,
                value_count,
            )?);
            Ok(Some(last))
        }
        LinearOp::TensorFill {
            value_start,
            count,
            lanes,
            ..
        } => {
            if count == 0 || lanes == 0 || lanes > 2 {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor fill has an invalid extent or lane count",
                });
            }
            require_register_range(op_index, "TensorFill", value_start, lanes, initialized)?;
            register_range_last(op_index, "TensorFill", value_start, lanes).map(Some)
        }
        LinearOp::TensorIdentity { size, lanes, .. } => {
            if size == 0 || lanes == 0 || lanes > 2 || size.checked_mul(size).is_none() {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor identity has an invalid extent or lane count",
                });
            }
            Ok(None)
        }
        LinearOp::TensorLoad {
            count,
            seed_start,
            lanes,
            ..
        } => {
            if count == 0 || lanes == 0 || lanes > 2 || (lanes == 1 && seed_start.is_some()) {
                return Err(ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index,
                    reason: "tensor load has an invalid extent, seed, or lane count",
                });
            }
            Ok(None)
        }
        LinearOp::TableBounds { table_id, .. } => {
            require_register(op_index, op.kind_name(), table_id, initialized)?;
            Ok(Some(table_id))
        }
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
        } => {
            require_register(op_index, op.kind_name(), table_id, initialized)?;
            require_register(op_index, op.kind_name(), column, initialized)?;
            require_register(op_index, op.kind_name(), input, initialized)?;
            Ok(Some(table_id.max(column).max(input)))
        }
        LinearOp::TableNextEvent { table_id, time, .. } => {
            require_register(op_index, op.kind_name(), table_id, initialized)?;
            require_register(op_index, op.kind_name(), time, initialized)?;
            Ok(Some(table_id.max(time)))
        }
        LinearOp::RandomInitialState { .. }
        | LinearOp::RandomResult { .. }
        | LinearOp::RandomState { .. }
        | LinearOp::ImpureRandomInit { .. }
        | LinearOp::ImpureRandom { .. }
        | LinearOp::ImpureRandomInteger { .. } => {
            validate_random_sources(op, op_index, initialized)
        }
        LinearOp::FunctionFold {
            initial_start,
            capture_start,
            ref program,
            ..
        } => validate_function_fold_sources(
            op_index,
            "FunctionFold",
            initial_start,
            capture_start,
            None,
            program,
            initialized,
        ),
        LinearOp::GuardedFunctionFold {
            initial_start,
            capture_start,
            activation,
            ref program,
            ..
        } => validate_function_fold_sources(
            op_index,
            "GuardedFunctionFold",
            initial_start,
            capture_start,
            Some(activation),
            program,
            initialized,
        ),
    }
}

fn validate_function_fold_sources(
    op_index: usize,
    operation: &'static str,
    initial_start: Reg,
    capture_start: Reg,
    activation: Option<Reg>,
    program: &FunctionFoldProgram,
    initialized: &[bool],
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    if program.carried_count == 0 {
        return Err(ScalarProgramRegisterError::InvalidFunctionFold {
            op_index,
            reason: "carried tuple is empty",
        });
    }
    require_register_range(
        op_index,
        operation,
        initial_start,
        program.carried_count,
        initialized,
    )?;
    if program.capture_count != 0 {
        require_register_range(
            op_index,
            operation,
            capture_start,
            program.capture_count,
            initialized,
        )?;
    }
    if let Some(activation) = activation {
        require_register(op_index, operation, activation, initialized)?;
    }
    // `FunctionFoldProgram::checked` (including checked wire replay) owns
    // body/domain/output validation. A reference from an outer program proves
    // only its compact call ABI; recursively re-proving the immutable Arc here
    // would turn a shared program DAG back into an extent-sized traversal tree.
    let initial_last =
        register_range_last(op_index, operation, initial_start, program.carried_count)?;
    let mut source_last = if program.capture_count == 0 {
        initial_last
    } else {
        initial_last.max(register_range_last(
            op_index,
            operation,
            capture_start,
            program.capture_count,
        )?)
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

fn validate_random_sources(
    op: &LinearOp,
    op_index: usize,
    initialized: &[bool],
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    match *op {
        LinearOp::RandomInitialState {
            local_seed,
            global_seed,
            state_len,
            state_index,
            ..
        } => {
            validate_projection(op_index, op.kind_name(), state_index, state_len)?;
            require_register(op_index, op.kind_name(), local_seed, initialized)?;
            require_register(op_index, op.kind_name(), global_seed, initialized)?;
            Ok(Some(local_seed.max(global_seed)))
        }
        LinearOp::RandomResult {
            state_start,
            state_len,
            ..
        } => require_register_range(
            op_index,
            op.kind_name(),
            state_start,
            state_len,
            initialized,
        )
        .map(Some),
        LinearOp::RandomState {
            state_start,
            state_len,
            state_index,
            ..
        } => {
            validate_projection(op_index, op.kind_name(), state_index, state_len)?;
            require_register_range(
                op_index,
                op.kind_name(),
                state_start,
                state_len,
                initialized,
            )
            .map(Some)
        }
        LinearOp::ImpureRandomInit { seed, .. } => {
            require_register(op_index, op.kind_name(), seed, initialized)?;
            Ok(Some(seed))
        }
        LinearOp::ImpureRandom { id, .. } => {
            require_register(op_index, op.kind_name(), id, initialized)?;
            Ok(Some(id))
        }
        LinearOp::ImpureRandomInteger { id, imin, imax, .. } => {
            require_register(op_index, op.kind_name(), id, initialized)?;
            require_register(op_index, op.kind_name(), imin, initialized)?;
            require_register(op_index, op.kind_name(), imax, initialized)?;
            Ok(Some(id.max(imin).max(imax)))
        }
        _ => unreachable!("random source validation requires a random operation"),
    }
}

fn linear_solve_sources(
    op_index: usize,
    matrix_start: Reg,
    rhs_start: Reg,
    n: usize,
    component: usize,
    initialized: &[bool],
) -> Result<Reg, ScalarProgramRegisterError> {
    validate_projection(op_index, "LinearSolveComponent", component, n)?;
    let matrix_len = n
        .checked_mul(n)
        .ok_or(ScalarProgramRegisterError::RegisterRangeOverflow {
            op_index,
            operation: "LinearSolveComponent",
            start: matrix_start,
            len: n,
        })?;
    let matrix_end = require_register_range(
        op_index,
        "LinearSolveComponent",
        matrix_start,
        matrix_len,
        initialized,
    )?;
    let rhs_end =
        require_register_range(op_index, "LinearSolveComponent", rhs_start, n, initialized)?;
    Ok(matrix_end.max(rhs_end))
}

#[allow(clippy::too_many_arguments)]
fn dot_product_sources(
    op_index: usize,
    lhs_start: Reg,
    rhs_start: Reg,
    count: usize,
    lhs_stride: usize,
    rhs_stride: usize,
    initialized: &[bool],
) -> Result<Reg, ScalarProgramRegisterError> {
    let lhs_end = require_strided_registers(
        op_index,
        "DotProduct",
        lhs_start,
        count,
        lhs_stride,
        initialized,
    )?;
    let rhs_end = require_strided_registers(
        op_index,
        "DotProduct",
        rhs_start,
        count,
        rhs_stride,
        initialized,
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
    use super::{BinaryOp, CompareOp, LinearOp, UnaryOp};

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
}
