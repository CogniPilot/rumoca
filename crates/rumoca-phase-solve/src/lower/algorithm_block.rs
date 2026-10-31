//! Refine a checked GALEC block into a [`SolveAlgorithmBlock`] (#483).
//!
//! The GALEC targets compile DAE → GALEC → Solve IR → C. This module is the
//! GALEC → Solve step: it reads the checked [`AlgorithmCodePackage`] and builds
//! the block's slot table and its three lifecycle methods as typed programs.
//! A construct this refinement does not yet represent is refused with a coded,
//! source-bearing diagnostic before any part of the block exists.
//!
//! Solve regions are value-only: a `conditional` region sees its captured
//! inputs and writes its outputs, never the method's slots. A GALEC `if`
//! statement that assigns state therefore lowers SSA-style. Its branches carry
//! every variable assigned in either branch as a region output, passing the
//! incoming value through where a branch leaves one unassigned, and the method
//! stores the outputs after the conditional.
//!
//! The semantics mirrored here come from the GALEC oracle
//! (`rumoca-eval-galec`) and the embedded C target:
//! - declared ranges saturate at every method boundary (low bound first, NaN
//!   passes through): on entry to `Recalibrate` and `DoStep` and on every
//!   return. At `Startup`'s return only the variables `Startup` definitely
//!   wrote are saturated, and inputs are saturated too;
//! - each method clears the error-signal status on entry, and a Real
//!   relational comparison on a NaN operand evaluates to false and raises NAN.

use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

#[cfg(test)]
use check::check_statement;
use check::{
    Builtin, check_expression, check_statements, check_subscripts, integer_conversion_limits,
    loop_range, may_fault, static_int,
};
use definedness::{require_defined_function, require_defined_reads};
use expressions::{expression_type, lower_expression, lower_lazy_select, may_overflow, raise};
use faults::{Summary, function_summary, require_fault_visible_state};
use functions::{
    Functions, check_call_statement, check_user_call, lower_functions, lower_user_call,
};

use rumoca_core::{IndexBinderProvenance, Span, StructuredIndexBinder, StructuredIndexDomain};
use rumoca_ir_galec::ast::{
    BinaryOp, Block, BlockMethod, Condition, Dimension, Expression, ForLoop, FunctionCall,
    IfBranch, IfExpression, IfStatement, InterfaceKind, LimitTarget, PredefinedSignal,
    ProtectedKind, RefPart, Reference, ScalarType, Spanned, Statement, TypeRef,
    VariableDeclaration,
};
use rumoca_ir_galec::package::AlgorithmCodePackage;
use rumoca_ir_solve::{
    ProgramRegister, ProgramSlot, SolveAlgorithmBlock, SolveAlgorithmBlockConstructionError,
    SolveAlgorithmCodeEntityKind, SolveAlgorithmCodeRef, SolveArithmeticProfile,
    SolveBinaryOperator, SolveBlockFunctions, SolveBlockMethodKind, SolveBlockSlotDeclaration,
    SolveCompareOperator, SolveConversionOperator, SolveProgramConstructionError, SolveRealFormat,
    SolveScalarType, SolveSlotAccess, SolveStorageClass, SolveUnaryOperator, SolveValue,
    SolveValueType, TypedProgramBuilder,
};

use crate::diagnostic_codes::EL007_UNSUPPORTED_ALGORITHM_CODE;

/// Why a checked GALEC block was not refined into a Solve block.
#[derive(Debug, Clone, PartialEq)]
pub enum AlgorithmBlockLowerError {
    /// A construct this refinement does not represent yet (code EL007).
    Unsupported { construct: &'static str, span: Span },
    /// A store the block's lifecycle does not allow, such as `DoStep` writing
    /// a tunable parameter (code EL007).
    LifecycleStore { name: String, span: Span },
    /// A declared range bound the target profile cannot express (code
    /// EL007); `feature` is the stable `unsupported-feature:` tag.
    UnrepresentableBound {
        feature: &'static str,
        reason: &'static str,
        span: Span,
    },
    /// The Solve block constructor refused the refined block.
    Construction(SolveAlgorithmBlockConstructionError),
    /// A user function's body did not build as a pure-call owner.
    Function {
        name: String,
        error: SolveProgramConstructionError,
    },
    /// A construct with no exact source span anywhere in it, so no Solve
    /// provenance can name it (SPEC_0008).
    MissingProvenance { what: String },
}

impl AlgorithmBlockLowerError {
    #[must_use]
    pub const fn code(&self) -> &'static str {
        match self {
            Self::Unsupported { .. }
            | Self::LifecycleStore { .. }
            | Self::UnrepresentableBound { .. } => EL007_UNSUPPORTED_ALGORITHM_CODE,
            Self::Construction(_) | Self::Function { .. } | Self::MissingProvenance { .. } => {
                crate::diagnostic_codes::EL005_INVALID_SOLVE_CONTRACT
            }
        }
    }
}

impl std::fmt::Display for AlgorithmBlockLowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unsupported { construct, .. } => write!(
                f,
                "{}: GALEC {construct} is not yet refined into Solve IR",
                self.code()
            ),
            Self::LifecycleStore { name, .. } => write!(
                f,
                "{}: `{name}` is stored outside the methods its lifecycle allows",
                self.code()
            ),
            Self::UnrepresentableBound {
                feature, reason, ..
            } => write!(
                f,
                "{}: unsupported-feature:{feature}: {reason}",
                self.code()
            ),
            Self::Construction(error) => write!(f, "{}: {error}", self.code()),
            Self::Function { name, error } => {
                write!(f, "{}: function `{name}`: {error}", self.code())
            }
            Self::MissingProvenance { what } => {
                write!(f, "{}: {what} carries no source span", self.code())
            }
        }
    }
}

impl std::error::Error for AlgorithmBlockLowerError {}

/// The predefined error signals in status-mask order (INVALID_ARGUMENT is
/// bit 0, NAN is bit 2), one Boolean status slot each.
const OVERFLOW_SIGNAL: usize = 1;

/// The index into `SIGNALS` of a predefined signal name.
fn predefined_signal(name: &str) -> Option<usize> {
    SIGNALS.iter().position(|(_, signal)| *signal == name)
}

const SIGNALS: [(PredefinedSignal, &str); 6] = [
    (PredefinedSignal::InvalidArgument, "INVALID_ARGUMENT"),
    (PredefinedSignal::Overflow, "OVERFLOW"),
    (PredefinedSignal::Nan, "NAN"),
    (
        PredefinedSignal::SolveLinearEquationsFailed,
        "SOLVE_LINEAR_EQUATIONS_FAILED",
    ),
    (PredefinedSignal::NoSolutionFound, "NO_SOLUTION_FOUND"),
    (PredefinedSignal::UnspecifiedError, "UNSPECIFIED_ERROR"),
];
const NAN_SIGNAL: usize = 2;

/// Which methods may store a block variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Lifecycle {
    /// Written only by the boundary saturation (inputs).
    BoundaryOnly,
    /// Written by `Startup` only (tunable parameters, constants).
    StartupOnly,
    /// Written by `Startup` and `Recalibrate` (dependent parameters).
    StartupAndRecalibrate,
    /// Written by any method (outputs, states, signal status).
    Any,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Method {
    Startup,
    Recalibrate,
    DoStep,
    /// A user function body.
    Function,
}

/// A declared bound, already decided in the profile's domain.
#[derive(Debug, Clone, Copy, PartialEq)]
enum Bound {
    Real(f64),
    Integer(i64),
}

#[derive(Debug, Clone)]
struct Variable {
    name: String,
    value_type: SolveValueType,
    /// Array extents, empty for a scalar.
    dims: Vec<u32>,
    storage: SolveStorageClass,
    lifecycle: Lifecycle,
    low: Option<Bound>,
    high: Option<Bound>,
    span: Span,
}

impl Variable {
    /// Whether the slot holds a value before any store: the builder starts
    /// outputs and method locals uninitialized.
    fn starts_initialized(&self) -> bool {
        !matches!(
            self.storage,
            SolveStorageClass::Output | SolveStorageClass::MethodLocal
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum VarId {
    Block(usize),
    Local(usize),
    /// The iterator of the `for` loop at this nesting depth.
    Loop(usize),
}

/// The block's variables and the arithmetic they are typed under.
struct BlockScope {
    arithmetic: SolveArithmeticProfile,
    block: Vec<Variable>,
    by_name: BTreeMap<String, usize>,
    status: [usize; 6],
    functions: Rc<Functions>,
    /// The block's own span, else its first exact span: what derived
    /// declarations (status bits, loop iterators) are located at.
    provenance: Span,
}

/// One method's lowering: its locals and which method it is.
struct MethodScope<'a> {
    scope: &'a BlockScope,
    method: Method,
    locals: Vec<Variable>,
    local_names: BTreeMap<String, usize>,
    /// The iterators of the `for` loops being analysed or lowered, outermost
    /// first; a name resolves to the innermost loop that binds it.
    loops: RefCell<Vec<String>>,
    /// What every loop iterator is: an unbounded Integer scalar no slot holds.
    iterator: Variable,
    /// Indices into `SIGNALS` of the signals the lowered body may raise.
    raised: RefCell<BTreeSet<usize>>,
}

impl MethodScope<'_> {
    fn variable(&self, id: VarId) -> &Variable {
        match id {
            VarId::Block(index) => &self.scope.block[index],
            VarId::Local(index) => &self.locals[index],
            VarId::Loop(_) => &self.iterator,
        }
    }

    fn value_type(&self, id: VarId) -> SolveValueType {
        match id {
            VarId::Loop(_) => SolveValueType::scalar(SolveScalarType::Integer(
                self.scope.arithmetic.integer_domain(),
            )),
            _ => self.variable(id).value_type.clone(),
        }
    }

    /// Whether `id` holds a value when the method starts. Recalibrate and
    /// DoStep see every block variable Startup defined.
    fn starts_initialized(&self, id: VarId) -> bool {
        match id {
            VarId::Loop(_) => false,
            VarId::Block(_) if !matches!(self.method, Method::Startup | Method::Function) => true,
            _ => self.variable(id).starts_initialized(),
        }
    }

    /// A whole-variable reference.
    fn resolve(&self, reference: &Reference) -> Result<VarId, AlgorithmBlockLowerError> {
        let (id, subscripts) = self.resolve_place(reference)?;
        if subscripts.is_empty() {
            Ok(id)
        } else {
            Err(unsupported_reference("subscripted reference", reference))
        }
    }

    /// A reference to a variable or one of its elements.
    fn resolve_place<'r>(
        &self,
        reference: &'r Reference,
    ) -> Result<(VarId, &'r [Expression]), AlgorithmBlockLowerError> {
        match reference {
            Reference::Local(part) => {
                if let Some(iterator) = self.resolve_iterator(part) {
                    return iterator;
                }
                let id = self
                    .local_names
                    .get(part.name.lexeme())
                    .map(|&index| VarId::Local(index))
                    .ok_or_else(|| unsupported("unresolved local reference", part.span))?;
                Ok((id, &part.subscripts))
            }
            Reference::State(parts) => {
                let [part] = parts.as_slice() else {
                    return Err(unsupported_reference(
                        "compartment member reference",
                        reference,
                    ));
                };
                let id = self
                    .scope
                    .by_name
                    .get(part.name.lexeme())
                    .map(|&index| VarId::Block(index))
                    .ok_or_else(|| unsupported("unresolved state reference", part.span))?;
                Ok((id, &part.subscripts))
            }
        }
    }

    /// `part` as the innermost loop iterator of that name, if one is bound.
    #[allow(clippy::type_complexity)]
    fn resolve_iterator<'r>(
        &self,
        part: &'r RefPart,
    ) -> Option<Result<(VarId, &'r [Expression]), AlgorithmBlockLowerError>> {
        let name = part.name.lexeme();
        let depth = self
            .loops
            .borrow()
            .iter()
            .rposition(|loop_| loop_ == name)?;
        Some(if part.subscripts.is_empty() {
            Ok((VarId::Loop(depth), &[]))
        } else {
            Err(unsupported("subscripted loop iterator", part.span))
        })
    }

    /// Run `body` with `iterator` bound as the innermost loop variable.
    fn with_loop<T>(&self, iterator: &str, body: impl FnOnce(usize) -> T) -> T {
        let depth = {
            let mut loops = self.loops.borrow_mut();
            loops.push(iterator.to_owned());
            loops.len() - 1
        };
        let result = body(depth);
        self.loops.borrow_mut().pop();
        result
    }

    fn check_store(&self, id: VarId, span: Span) -> Result<(), AlgorithmBlockLowerError> {
        let index = match id {
            VarId::Block(index) => index,
            VarId::Local(_) => return Ok(()),
            VarId::Loop(_) => return Err(unsupported("assignment to a loop iterator", span)),
        };
        let variable = &self.scope.block[index];
        let allowed = match variable.lifecycle {
            Lifecycle::Any => true,
            Lifecycle::BoundaryOnly => false,
            Lifecycle::StartupOnly => self.method == Method::Startup,
            Lifecycle::StartupAndRecalibrate => {
                matches!(self.method, Method::Startup | Method::Recalibrate)
            }
        };
        if allowed {
            Ok(())
        } else {
            Err(AlgorithmBlockLowerError::LifecycleStore {
                name: variable.name.clone(),
                span,
            })
        }
    }
}

fn unsupported(construct: &'static str, span: Span) -> AlgorithmBlockLowerError {
    AlgorithmBlockLowerError::Unsupported { construct, span }
}

fn unrepresentable_bound(
    feature: &'static str,
    reason: &'static str,
    span: Span,
) -> AlgorithmBlockLowerError {
    AlgorithmBlockLowerError::UnrepresentableBound {
        feature,
        reason,
        span,
    }
}

/// `construct` is not refined, located at the reference's first part. A
/// state reference with no part has no span to cite.
fn unsupported_reference(
    construct: &'static str,
    reference: &Reference,
) -> AlgorithmBlockLowerError {
    let first = match reference {
        Reference::Local(part) => Some(part),
        Reference::State(parts) => parts.first(),
    };
    match first {
        Some(part) => unsupported(construct, part.span),
        None => AlgorithmBlockLowerError::MissingProvenance {
            what: format!("GALEC {construct}"),
        },
    }
}

fn reference_subscripts(reference: &Reference) -> bool {
    match reference {
        Reference::Local(part) => !part.subscripts.is_empty(),
        Reference::State(parts) => parts.iter().any(|part| !part.subscripts.is_empty()),
    }
}

/// Refine `package`'s checked block into a Solve block typed under `arithmetic`.
pub fn lower_algorithm_block(
    package: &AlgorithmCodePackage,
    arithmetic: SolveArithmeticProfile,
) -> Result<SolveAlgorithmBlock, AlgorithmBlockLowerError> {
    let block = package.block();
    require_supported_block(block)?;
    let scope = block_scope(block, arithmetic)?;
    // Refuse every unsupported construct before any program is built, so the
    // builder closures below meet only construction errors.
    let checked = |method, body: &BlockMethod| {
        let method_scope = method_scope(&scope, method, body)?;
        check_statements(&method_scope, &body.statements)?;
        Ok::<_, AlgorithmBlockLowerError>(method_scope)
    };
    let startup = checked(Method::Startup, &block.startup)?;
    let recalibrate = checked(Method::Recalibrate, &block.recalibrate)?;
    let do_step = checked(Method::DoStep, &block.do_step)?;
    require_startup_defines_state(&startup, &block.startup.statements)?;
    let slots = scope
        .block
        .iter()
        .map(|variable| {
            SolveBlockSlotDeclaration::construct(
                variable.value_type.clone(),
                variable.storage,
                block_access(variable),
                SolveAlgorithmCodeRef::construct(
                    block_source_kind(variable),
                    variable.name.clone(),
                    variable.span,
                ),
            )
        })
        .collect::<Vec<_>>();
    let (functions, function_sources, status_bindings) = lower_functions(block, &scope)?;
    for (scope, body) in [
        (&startup, &block.startup),
        (&recalibrate, &block.recalibrate),
        (&do_step, &block.do_step),
    ] {
        require_fault_visible_state(scope, &body.statements)?;
        require_defined_reads(scope, &body.statements)?;
    }
    let mut refined = SolveAlgorithmBlock::construct_with_functions(
        arithmetic,
        slots,
        SolveBlockFunctions {
            table: functions,
            sources: function_sources,
            status_bindings,
        },
        |builder, slots| lower_method(builder, slots, &startup, &block.startup),
        |builder, slots| lower_method(builder, slots, &recalibrate, &block.recalibrate),
        |builder, slots| lower_method(builder, slots, &do_step, &block.do_step),
    )
    .map_err(AlgorithmBlockLowerError::Construction)?;
    for (scope, body, kind) in [
        (&startup, &block.startup, SolveBlockMethodKind::Startup),
        (
            &recalibrate,
            &block.recalibrate,
            SolveBlockMethodKind::Recalibrate,
        ),
        (&do_step, &block.do_step, SolveBlockMethodKind::DoStep),
    ] {
        let declared = declared_signals(scope, body)?;
        refined = refined
            .with_method_escapes(
                kind,
                declared.iter().map(|&signal| scope.scope.status[signal]),
            )
            .map_err(AlgorithmBlockLowerError::Construction)?;
    }
    Ok(refined)
}

/// A method's `signals` clause is its declared escape set. With no
/// signal checks in this slice every raise escapes, so each signal the
/// lowered body may raise must be declared.
fn declared_signals(
    scope: &MethodScope<'_>,
    body: &BlockMethod,
) -> Result<BTreeSet<usize>, AlgorithmBlockLowerError> {
    let declared = body
        .signals
        .iter()
        .map(|signal| usize::from(signal.bit()))
        .collect::<BTreeSet<_>>();
    if !scope.raised.borrow().is_subset(&declared) {
        return Err(unsupported(
            "raised signal missing from the method's signals clause",
            body.span,
        ));
    }
    Ok(declared)
}

fn require_supported_block(block: &Block) -> Result<(), AlgorithmBlockLowerError> {
    if let Some(compartment) = block.compartments.first() {
        return Err(unsupported("state compartment", compartment.name.span()));
    }
    if let Some(signal) = block.error_signals.first() {
        let _ = signal;
        return Err(unsupported("user error signal", block.span));
    }
    Ok(())
}

/// Recalibrate and DoStep assume every block slot initialized, so a
/// successful Startup must define every slot the caller does not: inputs and
/// tunables are external, and each method clears its own status bits.
fn require_startup_defines_state(
    startup: &MethodScope<'_>,
    statements: &[Spanned<Statement>],
) -> Result<(), AlgorithmBlockLowerError> {
    let written = definite_writes(startup, statements);
    let undefined = startup
        .scope
        .block
        .iter()
        .enumerate()
        .find(|(index, variable)| {
            !matches!(
                variable.storage,
                SolveStorageClass::Input
                    | SolveStorageClass::TunableParameter
                    | SolveStorageClass::SignalStatus
            ) && !written.contains(&VarId::Block(*index))
        });
    match undefined {
        Some((_, variable)) => Err(unsupported(
            "block state not definitely assigned by Startup",
            variable.span,
        )),
        None => Ok(()),
    }
}

/// The block's own span, else the first exact span inside it.
fn block_provenance(block: &Block) -> Option<Span> {
    let declarations = block
        .interface
        .iter()
        .map(|variable| variable.decl.span)
        .chain(block.protected.iter().map(|entity| entity.decl.span));
    let statements = [&block.startup, &block.recalibrate, &block.do_step]
        .into_iter()
        .flat_map(|method| method.statements.iter().map(|statement| statement.span));
    std::iter::once(block.span)
        .chain(declarations)
        .chain(statements)
        .find(|span| !span.is_dummy())
}

fn block_scope(
    block: &Block,
    arithmetic: SolveArithmeticProfile,
) -> Result<BlockScope, AlgorithmBlockLowerError> {
    let mut variables = Vec::new();
    for interface in &block.interface {
        let (storage, lifecycle) = match interface.kind {
            InterfaceKind::Input => (SolveStorageClass::Input, Lifecycle::BoundaryOnly),
            InterfaceKind::Output => (SolveStorageClass::Output, Lifecycle::Any),
            InterfaceKind::TunableParameter => {
                (SolveStorageClass::TunableParameter, Lifecycle::StartupOnly)
            }
        };
        variables.push(variable(&interface.decl, storage, lifecycle, arithmetic)?);
    }
    for entity in &block.protected {
        let (storage, lifecycle) = match entity.kind {
            ProtectedKind::DependentParameter => (
                SolveStorageClass::CalculatedParameter,
                Lifecycle::StartupAndRecalibrate,
            ),
            // Assigned once by Startup and immutable afterwards: a
            // writable slot whose only store site is Startup.
            ProtectedKind::Constant => (
                SolveStorageClass::CalculatedParameter,
                Lifecycle::StartupOnly,
            ),
            ProtectedKind::State => (SolveStorageClass::PersistentState, Lifecycle::Any),
        };
        variables.push(variable(&entity.decl, storage, lifecycle, arithmetic)?);
    }
    // Generated blocks leave the block and method spans empty; the derived
    // status bits take the block's first exact span.
    let provenance = block_provenance(block)
        .ok_or_else(|| unsupported("block without source provenance", block.span))?;
    let mut status = [0; 6];
    for (index, (_, name)) in SIGNALS.iter().enumerate() {
        status[index] = variables.len();
        variables.push(Variable {
            name: format!("ErrorSignalStatus.{name}"),
            value_type: SolveValueType::scalar(SolveScalarType::Boolean),
            dims: Vec::new(),
            storage: SolveStorageClass::SignalStatus,
            lifecycle: Lifecycle::Any,
            low: None,
            high: None,
            span: provenance,
        });
    }
    let by_name = variables
        .iter()
        .enumerate()
        .map(|(index, variable)| (variable.name.clone(), index))
        .collect();
    Ok(BlockScope {
        arithmetic,
        block: variables,
        by_name,
        status,
        functions: Rc::new(Functions::declare(block, arithmetic)?),
        provenance,
    })
}

/// The declaration every `for` iterator of `scope` shares.
fn iterator_declaration(scope: &BlockScope) -> Variable {
    Variable {
        name: "for-loop iterator".to_owned(),
        value_type: SolveValueType::scalar(SolveScalarType::Integer(
            scope.arithmetic.integer_domain(),
        )),
        dims: Vec::new(),
        storage: SolveStorageClass::MethodLocal,
        lifecycle: Lifecycle::Any,
        low: None,
        high: None,
        span: scope.provenance,
    }
}

fn method_scope<'a>(
    scope: &'a BlockScope,
    method: Method,
    body: &BlockMethod,
) -> Result<MethodScope<'a>, AlgorithmBlockLowerError> {
    let mut locals = Vec::with_capacity(body.locals.len());
    for local in &body.locals {
        locals.push(variable(
            local,
            SolveStorageClass::MethodLocal,
            Lifecycle::Any,
            scope.arithmetic,
        )?);
    }
    let local_names = locals
        .iter()
        .enumerate()
        .map(|(index, local)| (local.name.clone(), index))
        .collect();
    Ok(MethodScope {
        scope,
        method,
        locals,
        local_names,
        loops: RefCell::new(Vec::new()),
        iterator: iterator_declaration(scope),
        raised: RefCell::new(BTreeSet::new()),
    })
}

fn variable(
    decl: &VariableDeclaration,
    storage: SolveStorageClass,
    lifecycle: Lifecycle,
    arithmetic: SolveArithmeticProfile,
) -> Result<Variable, AlgorithmBlockLowerError> {
    let mut dims = Vec::with_capacity(decl.dimensions.len());
    for dimension in &decl.dimensions {
        let Dimension::Expr(Expression::Integer(extent)) = dimension else {
            return Err(unsupported(
                "array dimension that is not a literal",
                decl.span,
            ));
        };
        dims.push(
            u32::try_from(*extent)
                .ok()
                .filter(|&extent| extent > 0)
                .ok_or_else(|| unsupported("array extent", decl.span))?,
        );
    }
    let scalar = match &decl.ty {
        TypeRef::Primitive(ScalarType::Real) => SolveScalarType::Real {
            format: arithmetic.real_format(),
        },
        TypeRef::Primitive(ScalarType::Integer) => {
            SolveScalarType::Integer(arithmetic.integer_domain())
        }
        TypeRef::Primitive(ScalarType::Boolean) => SolveScalarType::Boolean,
        TypeRef::Compartment(_) => {
            return Err(unsupported("compartment-typed variable", decl.span));
        }
    };
    let low = decl
        .range
        .min
        .as_ref()
        .map(|bound| decide_bound(bound, scalar, arithmetic, BoundSide::Low, decl.span))
        .transpose()?
        .flatten();
    let high = decl
        .range
        .max
        .as_ref()
        .map(|bound| decide_bound(bound, scalar, arithmetic, BoundSide::High, decl.span))
        .transpose()?
        .flatten();
    let value_type = if dims.is_empty() {
        SolveValueType::scalar(scalar)
    } else {
        SolveValueType::tensor(scalar, dims.clone())
            .map_err(|_| unsupported("array extent", decl.span))?
    };
    Ok(Variable {
        name: decl.name.lexeme().to_owned(),
        value_type,
        dims,
        storage,
        lifecycle,
        low,
        high,
        span: decl.span,
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BoundSide {
    Low,
    High,
}

/// Decide a declared bound in the profile's domain: `None` when no
/// value of the domain can violate it (the clamp is elided), the bound when
/// it is representable, and a refusal when every value would violate it.
fn decide_bound(
    bound: &Expression,
    scalar: SolveScalarType,
    arithmetic: SolveArithmeticProfile,
    side: BoundSide,
    span: Span,
) -> Result<Option<Bound>, AlgorithmBlockLowerError> {
    let literal =
        literal_number(bound).ok_or_else(|| unsupported("non-literal range bound", span))?;
    match scalar {
        SolveScalarType::Real { format } => {
            let literal = literal.real();
            if literal.is_nan() {
                return Err(unrepresentable_bound(
                    "target-real-range",
                    "NaN is not a saturation bound",
                    span,
                ));
            }
            let largest = match format {
                SolveRealFormat::Binary32 => f64::from(f32::MAX),
                SolveRealFormat::Binary64 => f64::MAX,
            };
            let unviolable = match side {
                BoundSide::Low => literal < -largest,
                BoundSide::High => literal > largest,
            };
            if unviolable {
                return Ok(None);
            }
            if literal.abs() > largest {
                return Err(unrepresentable_bound(
                    "target-real-range",
                    "range bound outside the target Real format",
                    span,
                ));
            }
            let _ = arithmetic;
            Ok(Some(Bound::Real(literal)))
        }
        SolveScalarType::Integer(domain) => {
            // Decided in Integer space: an f64 round trip would move bounds
            // beyond 2^53 and clamp legal values.
            let literal = literal
                .integer()
                .ok_or_else(|| unsupported("non-integral Integer range bound", span))?;
            // Unlike Real, a bound outside the domain declares a variable
            // the target cannot execute (SPEC_0034 GAL-028): refused, while
            // a bound at the domain edge is unviolable and elided.
            if literal < domain.minimum() || literal > domain.maximum() {
                return Err(unrepresentable_bound(
                    "target-integer-range",
                    "range bound outside the target Integer domain",
                    span,
                ));
            }
            let unviolable = match side {
                BoundSide::Low => literal == domain.minimum(),
                BoundSide::High => literal == domain.maximum(),
            };
            Ok((!unviolable).then_some(Bound::Integer(literal)))
        }
        SolveScalarType::Boolean => Err(unsupported("Boolean range bound", span)),
    }
}

/// A range-bound literal, kept in its own domain until the bound is decided.
#[derive(Debug, Clone, Copy)]
enum Literal {
    Real(f64),
    Integer(i64),
}

impl Literal {
    fn real(self) -> f64 {
        match self {
            Self::Real(value) => value,
            Self::Integer(value) => value as f64,
        }
    }

    /// The exact Integer value, or `None` for a fractional or out-of-range
    /// Real literal.
    fn integer(self) -> Option<i64> {
        const TWO_POW_63: f64 = 9_223_372_036_854_775_808.0;
        match self {
            Self::Integer(value) => Some(value),
            Self::Real(value)
                if value.fract() == 0.0 && (-TWO_POW_63..TWO_POW_63).contains(&value) =>
            {
                Some(value as i64)
            }
            Self::Real(_) => None,
        }
    }
}

fn literal_number(expression: &Expression) -> Option<Literal> {
    match expression {
        Expression::Real(value) => Some(Literal::Real(*value)),
        Expression::Integer(value) => Some(Literal::Integer(*value)),
        Expression::Paren(inner) => literal_number(inner),
        _ => None,
    }
}

fn block_access(variable: &Variable) -> SolveSlotAccess {
    // Every block slot has some store site: inputs are saturated at method
    // boundaries and parameters are written by Startup. The
    // lifecycle check above is what limits the store sites.
    let _ = variable;
    SolveSlotAccess::ReadWrite
}

fn block_source_kind(variable: &Variable) -> SolveAlgorithmCodeEntityKind {
    match variable.storage {
        SolveStorageClass::CalculatedParameter | SolveStorageClass::PersistentState => {
            SolveAlgorithmCodeEntityKind::Protected
        }
        SolveStorageClass::SignalStatus => SolveAlgorithmCodeEntityKind::ErrorSignalStatus,
        _ => SolveAlgorithmCodeEntityKind::Interface,
    }
}
// ---------------------------------------------------------------------------
// Lowering.

/// Current values of the variables a program region has seen.
struct Env<'p> {
    values: BTreeMap<VarId, ProgramRegister<'p>>,
    /// The method's slots, at a method's top level only: block slots then
    /// locals. A region has none and reads only captured values.
    slots: Option<Vec<ProgramSlot<'p>>>,
    /// Variables definitely written on every path so far (Startup's
    /// boundary saturation reads this).
    definite: BTreeSet<VarId>,
}

impl<'p> Env<'p> {
    fn slot(&self, scope: &MethodScope<'_>, id: VarId) -> Option<ProgramSlot<'p>> {
        let slots = self.slots.as_ref()?;
        Some(match id {
            VarId::Block(index) => slots[index],
            VarId::Local(index) => slots[scope.scope.block.len() + index],
            VarId::Loop(_) => return None,
        })
    }

    /// Whether `id` has a value here: a register, or an initialized slot.
    fn available(&self, scope: &MethodScope<'_>, id: VarId) -> bool {
        self.values.contains_key(&id) || (self.slots.is_some() && scope.starts_initialized(id))
    }

    fn read(
        &mut self,
        builder: &mut TypedProgramBuilder<'p>,
        scope: &MethodScope<'_>,
        id: VarId,
        span: Span,
    ) -> Result<ProgramRegister<'p>, SolveProgramConstructionError> {
        if let Some(&value) = self.values.get(&id) {
            return Ok(value);
        }
        let slot = self
            .slot(scope, id)
            .ok_or(SolveProgramConstructionError::UninitializedSlot { provenance: span })?;
        let value = builder.load(slot, span)?;
        self.values.insert(id, value);
        Ok(value)
    }

    fn write(
        &mut self,
        builder: &mut TypedProgramBuilder<'p>,
        scope: &MethodScope<'_>,
        id: VarId,
        value: ProgramRegister<'p>,
        span: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        if let Some(slot) = self.slot(scope, id) {
            builder.store(slot, value, span)?;
        }
        self.values.insert(id, value);
        self.definite.insert(id);
        Ok(())
    }
}

fn lower_method<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    block_slots: &[ProgramSlot<'p>],
    scope: &MethodScope<'_>,
    body: &BlockMethod,
) -> Result<Vec<SolveAlgorithmCodeRef>, SolveProgramConstructionError> {
    // A generated method may leave its span empty; it then takes the
    // block's provenance.
    let method_span = if body.span.is_dummy() {
        scope.scope.provenance
    } else {
        body.span
    };
    let mut slots = block_slots.to_vec();
    let mut sources = Vec::with_capacity(scope.locals.len());
    for local in &scope.locals {
        slots.push(builder.declare_slot(
            local.value_type.clone(),
            SolveStorageClass::MethodLocal,
            SolveSlotAccess::ReadWrite,
            local.span,
        )?);
        sources.push(SolveAlgorithmCodeRef::construct(
            SolveAlgorithmCodeEntityKind::MethodLocal,
            format!("{}.{}", method_name(scope.method), local.name),
            local.span,
        ));
    }
    let mut env = Env {
        values: BTreeMap::new(),
        slots: Some(slots),
        definite: BTreeSet::new(),
    };
    // The error-signal status is cleared exactly once on entry.
    let clear = builder.constant(SolveValue::boolean(false), method_span)?;
    for &status in &scope.scope.status {
        env.write(builder, scope, VarId::Block(status), clear, method_span)?;
    }
    zero_initialize(
        builder,
        scope,
        &mut env,
        (0..scope.locals.len()).map(VarId::Local),
        method_span,
    )?;
    let status_definite = env.definite.clone();
    // Recalibrate and DoStep also saturate on entry, after the clear: the
    // caller may have written an out-of-range input or parameter since the
    // last return (`rumoca-eval-galec` `invoke`, `limit_at_entry`).
    if scope.method != Method::Startup {
        lower_limit_target(
            builder,
            scope,
            &mut env,
            &LimitTarget::SelfState,
            method_span,
        )?;
    }
    lower_statements(builder, scope, &mut env, &body.statements)?;
    // Implicit `limit self` at the method's return.
    let limited = (0..scope.scope.block.len())
        .map(VarId::Block)
        .filter(|&id| {
            let variable = scope.variable(id);
            // The interpreter is the reference. At Startup's
            // return it saturates every initialized slot: the inputs and
            // tunable parameters the caller set before Startup, and what
            // Startup itself definitely wrote.
            let written = scope.method != Method::Startup
                || matches!(
                    variable.storage,
                    SolveStorageClass::Input | SolveStorageClass::TunableParameter
                )
                || (env.definite.contains(&id) && !status_definite.contains(&id));
            (variable.low.is_some() || variable.high.is_some())
                && written
                && env.available(scope, id)
        })
        .collect::<Vec<_>>();
    for id in limited {
        saturate(builder, scope, &mut env, id, method_span)?;
    }
    Ok(sources)
}

const fn method_name(method: Method) -> &'static str {
    match method {
        Method::Startup => "Startup",
        Method::Recalibrate => "Recalibrate",
        Method::DoStep => "DoStep",
        Method::Function => "function",
    }
}

fn lower_statements<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    statements: &[Spanned<Statement>],
) -> Result<(), SolveProgramConstructionError> {
    for statement in statements {
        lower_statement(builder, scope, env, &statement.node, statement.span)?;
    }
    Ok(())
}

fn lower_statement<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    statement: &Statement,
    span: Span,
) -> Result<(), SolveProgramConstructionError> {
    match statement {
        Statement::Assignment { target, value } => {
            lower_assignment(builder, scope, env, target, value, span)
        }
        Statement::For(for_loop) => lower_for(builder, scope, env, for_loop, span),
        // At a method's top level an `if` splits into one region per
        // statement, so each statement's stores are visible before a later
        // statement can fault.
        Statement::If(if_statement) if env.slots.is_some() && scope.method != Method::Function => {
            lower_if_split(builder, scope, env, if_statement, None, span)?;
            env.definite.extend(
                Body::Chain(&if_statement.branches, if_statement.else_body.as_deref())
                    .definite(scope),
            );
            Ok(())
        }
        Statement::If(if_statement) => lower_if(
            builder,
            scope,
            env,
            &if_statement.branches,
            if_statement.else_body.as_deref(),
            span,
        ),
        Statement::Limit(targets) => {
            for target in targets {
                lower_limit_target(builder, scope, env, target, span)?;
            }
            Ok(())
        }
        Statement::MultiAssignment { targets, call } => {
            let results = lower_user_call(builder, scope, env, call, span)?;
            for (target, value) in targets.iter().zip(results) {
                store_place(builder, scope, env, target, value, span)?;
            }
            Ok(())
        }
        Statement::Call(call) => lower_user_call(builder, scope, env, call, span).map(|_| ()),
        Statement::Signal(signals) => {
            for signal in signals {
                let signal = predefined_signal(signal.as_str())
                    .ok_or(SolveProgramConstructionError::InvalidRegion { provenance: span })?;
                let raised = builder.constant(SolveValue::boolean(true), span)?;
                raise(builder, scope, env, signal, raised, span)?;
            }
            Ok(())
        }
    }
}

/// Saturate one `limit` target: `self` is every ranged block variable that
/// holds a value here, a reference is that variable alone.
fn lower_limit_target<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    target: &LimitTarget,
    span: Span,
) -> Result<(), SolveProgramConstructionError> {
    let ids = match target {
        LimitTarget::SelfState => (0..scope.scope.block.len())
            .map(VarId::Block)
            .filter(|&id| is_ranged(scope.variable(id)) && env.available(scope, id))
            .collect::<Vec<_>>(),
        LimitTarget::Reference(reference) => vec![resolved(scope, reference, span)?],
    };
    for id in ids {
        saturate(builder, scope, env, id, span)?;
    }
    Ok(())
}

fn is_ranged(variable: &Variable) -> bool {
    variable.low.is_some() || variable.high.is_some()
}

fn lower_assignment<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    target: &Reference,
    value: &Expression,
    span: Span,
) -> Result<(), SolveProgramConstructionError> {
    let (id, _) = scope
        .resolve_place(target)
        .map_err(|_| SolveProgramConstructionError::InvalidRegion { provenance: span })?;
    let value = if matches!(value, Expression::Array(_)) {
        lower_array_literal(builder, scope, env, value, &scope.value_type(id), span)?
    } else {
        lower_expression(builder, scope, env, value, span)?
    };
    store_place(builder, scope, env, target, value, span)
}

/// Store `value` to a variable or one of its elements.
fn store_place<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    target: &Reference,
    value: ProgramRegister<'p>,
    span: Span,
) -> Result<(), SolveProgramConstructionError> {
    let (id, subscripts) = scope
        .resolve_place(target)
        .map_err(|_| SolveProgramConstructionError::InvalidRegion { provenance: span })?;
    if subscripts.is_empty() {
        return env.write(builder, scope, id, value, span);
    }
    let indices = lower_indices(builder, scope, env, subscripts, span)?;
    let current = env.read(builder, scope, id, span)?;
    let updated = builder.update_element(current, value, &indices, span)?;
    env.write(builder, scope, id, updated, span)
}

fn lower_indices<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    subscripts: &[Expression],
    span: Span,
) -> Result<Vec<ProgramRegister<'p>>, SolveProgramConstructionError> {
    subscripts
        .iter()
        .map(|subscript| lower_expression(builder, scope, env, subscript, span))
        .collect()
}

/// Build an array literal for a target of `target_type`: a filled tensor
/// with each element written in row-major order.
/// A tensor of `value_type` holding zero (false) everywhere.
fn zero_filled<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    arithmetic: SolveArithmeticProfile,
    value_type: &SolveValueType,
    span: Span,
) -> Result<ProgramRegister<'p>, SolveProgramConstructionError> {
    let zero = match value_type.element_type() {
        SolveScalarType::Real { .. } => SolveValue::real(arithmetic, 0.0),
        SolveScalarType::Integer(_) => SolveValue::integer(arithmetic, 0)
            .map_err(|_| SolveProgramConstructionError::TypeMismatch { provenance: span })?,
        SolveScalarType::Boolean => SolveValue::boolean(false),
    };
    let zero = builder.constant(zero, span)?;
    if value_type.dimensions().is_empty() {
        return Ok(zero);
    }
    builder.fill(zero, value_type.dimensions().to_vec(), span)
}

/// Generated Algorithm Code writes array outputs and locals element by
/// element before any whole definition, and defines scalar locals under a
/// guard that a later read repeats; GALEC storage exists from the start, but
/// Solve requires a definition on every path. Outputs and locals therefore
/// start at zero (false), which only an invalid read of an unassigned value
/// could observe; GALEC definite assignment rules that out.
fn zero_initialize<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    ids: impl IntoIterator<Item = VarId>,
    span: Span,
) -> Result<(), SolveProgramConstructionError> {
    for id in ids {
        let value_type = scope.value_type(id);
        let zero = zero_filled(builder, scope.scope.arithmetic, &value_type, span)?;
        env.write(builder, scope, id, zero, span)?;
    }
    Ok(())
}

fn lower_array_literal<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    literal: &Expression,
    target_type: &SolveValueType,
    span: Span,
) -> Result<ProgramRegister<'p>, SolveProgramConstructionError> {
    let mut tensor = zero_filled(builder, scope.scope.arithmetic, target_type, span)?;
    let mut position = Vec::new();
    fill_literal(
        builder,
        scope,
        env,
        literal,
        &mut position,
        &mut tensor,
        span,
    )?;
    Ok(tensor)
}

fn fill_literal<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    literal: &Expression,
    position: &mut Vec<i64>,
    tensor: &mut ProgramRegister<'p>,
    span: Span,
) -> Result<(), SolveProgramConstructionError> {
    let Expression::Array(items) = literal else {
        let value = lower_expression(builder, scope, env, literal, span)?;
        let mut indices = Vec::with_capacity(position.len());
        for &index in position.iter() {
            let index = SolveValue::integer(scope.scope.arithmetic, index)
                .map_err(|_| SolveProgramConstructionError::TypeMismatch { provenance: span })?;
            indices.push(builder.constant(index, span)?);
        }
        *tensor = builder.update_element(*tensor, value, &indices, span)?;
        return Ok(());
    };
    for (offset, item) in (1..).zip(items) {
        position.push(offset);
        fill_literal(builder, scope, env, item, position, tensor, span)?;
        position.pop();
    }
    Ok(())
}

/// Lower a static-range `for` loop to a fold carrying every variable the
/// body assigns that holds a value on entry. A variable the body
/// assigns before reading, with no value on entry, stays local to each pass.
fn lower_for<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    for_loop: &ForLoop,
    span: Span,
) -> Result<(), SolveProgramConstructionError> {
    let invalid = SolveProgramConstructionError::InvalidRegion { provenance: span };
    let iterator = for_loop.iterator.as_ref().ok_or(invalid.clone())?;
    let (start, stop, step) = loop_range(scope, for_loop).ok_or(invalid)?;
    scope.with_loop(iterator.lexeme(), |depth| {
        let mut assigned = BTreeSet::new();
        let mut read = BTreeSet::new();
        statement_effects(scope, &for_loop.body, &mut assigned, &mut read);
        assigned.extend(scope.scope.status.iter().map(|&index| VarId::Block(index)));
        read.extend(scope.scope.status.iter().map(|&index| VarId::Block(index)));
        read.remove(&VarId::Loop(depth));
        let carried = assigned
            .iter()
            .copied()
            .filter(|&id| env.available(scope, id))
            .collect::<Vec<_>>();
        let captures = read
            .iter()
            .copied()
            .filter(|id| !assigned.contains(id) && env.available(scope, *id))
            .collect::<Vec<_>>();
        let mut initial = Vec::with_capacity(carried.len());
        for &id in &carried {
            initial.push(env.read(builder, scope, id, span)?);
        }
        let mut capture_values = Vec::with_capacity(captures.len());
        for &id in &captures {
            capture_values.push(env.read(builder, scope, id, span)?);
        }
        let domain = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                provenance: IndexBinderProvenance::Generated,
                id: depth,
                display_name: iterator.lexeme().to_owned(),
                lower: start,
                upper: stop,
                step,
            }],
        };
        let results = builder.fold(
            domain,
            &initial,
            &capture_values,
            span,
            |region, carried_slots, capture_slots, binder_slots, outputs| {
                let mut body_env = Env {
                    values: BTreeMap::new(),
                    slots: None,
                    definite: BTreeSet::new(),
                };
                for (&id, &slot) in carried.iter().zip(carried_slots) {
                    body_env.values.insert(id, region.load(slot, span)?);
                }
                for (&id, &slot) in captures.iter().zip(capture_slots) {
                    body_env.values.insert(id, region.load(slot, span)?);
                }
                body_env
                    .values
                    .insert(VarId::Loop(depth), region.load(binder_slots[0], span)?);
                lower_statements(region, scope, &mut body_env, &for_loop.body)?;
                for (&id, &output) in carried.iter().zip(outputs) {
                    let value = body_env.values.get(&id).copied().ok_or(
                        SolveProgramConstructionError::UninitializedSlot { provenance: span },
                    )?;
                    region.store(output, value, span)?;
                }
                Ok(())
            },
        )?;
        for (&id, value) in carried.iter().zip(results) {
            if let Some(slot) = env.slot(scope, id) {
                builder.store(slot, value, span)?;
            }
            env.values.insert(id, value);
        }
        Ok(())
    })
}

fn resolved(
    scope: &MethodScope<'_>,
    reference: &Reference,
    span: Span,
) -> Result<VarId, SolveProgramConstructionError> {
    scope
        .resolve(reference)
        .map_err(|_| SolveProgramConstructionError::InvalidRegion { provenance: span })
}

/// Saturate `id` to its declared range: low bound first; NaN passes through.
fn saturate<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    id: VarId,
    span: Span,
) -> Result<(), SolveProgramConstructionError> {
    let variable = scope.variable(id).clone();
    if !variable.dims.is_empty() {
        return saturate_elements(builder, scope, env, id, &variable, span);
    }
    let mut value = env.read(builder, scope, id, span)?;
    if let Some(low) = variable.low {
        let bound = bound_constant(builder, scope, low, span)?;
        let below = builder.compare(SolveCompareOperator::Less, value, bound, span)?;
        value = builder.select(below, bound, value, span)?;
    }
    if let Some(high) = variable.high {
        let bound = bound_constant(builder, scope, high, span)?;
        let above = builder.compare(SolveCompareOperator::Greater, value, bound, span)?;
        value = builder.select(above, bound, value, span)?;
    }
    env.write(builder, scope, id, value, span)
}

/// Saturate every element of an array variable. Solve's `select` takes a
/// scalar condition and `Min`/`Max` discard NaN, so a fold visits each
/// element and applies the scalar clamp.
fn saturate_elements<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    id: VarId,
    variable: &Variable,
    span: Span,
) -> Result<(), SolveProgramConstructionError> {
    let value = env.read(builder, scope, id, span)?;
    let domain = StructuredIndexDomain {
        binders: variable
            .dims
            .iter()
            .enumerate()
            .map(|(axis, &extent)| StructuredIndexBinder {
                provenance: IndexBinderProvenance::Generated,
                id: axis,
                display_name: format!("{}[{axis}]", variable.name),
                lower: 1,
                upper: i64::from(extent),
                step: 1,
            })
            .collect(),
    };
    let results = builder.fold(
        domain,
        &[value],
        &[],
        span,
        |region, carried, _, binders, outputs| {
            let tensor = region.load(carried[0], span)?;
            let indices = binders
                .iter()
                .map(|&binder| region.load(binder, span))
                .collect::<Result<Vec<_>, _>>()?;
            let mut element = region.project_element_dynamic(tensor, &indices, span)?;
            if let Some(low) = variable.low {
                let bound = bound_constant(region, scope, low, span)?;
                let below = region.compare(SolveCompareOperator::Less, element, bound, span)?;
                element = region.select(below, bound, element, span)?;
            }
            if let Some(high) = variable.high {
                let bound = bound_constant(region, scope, high, span)?;
                let above = region.compare(SolveCompareOperator::Greater, element, bound, span)?;
                element = region.select(above, bound, element, span)?;
            }
            let updated = region.update_element(tensor, element, &indices, span)?;
            region.store(outputs[0], updated, span)
        },
    )?;
    env.write(builder, scope, id, results[0], span)
}

fn bound_constant<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    bound: Bound,
    span: Span,
) -> Result<ProgramRegister<'p>, SolveProgramConstructionError> {
    let value = match bound {
        Bound::Real(value) => SolveValue::real(scope.scope.arithmetic, value),
        Bound::Integer(value) => SolveValue::integer(scope.scope.arithmetic, value)
            .map_err(|_| SolveProgramConstructionError::TypeMismatch { provenance: span })?,
    };
    builder.constant(value, span)
}

/// Lower an `if`/`elseif`/`else` chain SSA-style.
fn lower_if<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    branches: &[IfBranch],
    else_body: Option<&[Spanned<Statement>]>,
    span: Span,
) -> Result<(), SolveProgramConstructionError> {
    let Some((first, rest)) = branches.split_first() else {
        return match else_body {
            Some(body) => lower_statements(builder, scope, env, body),
            None => Ok(()),
        };
    };
    let Condition::Expression(condition) = &first.condition else {
        return Err(SolveProgramConstructionError::InvalidRegion { provenance: span });
    };
    let condition = lower_expression(builder, scope, env, condition, span)?;
    let true_body = Body::Statements(&first.body);
    let false_body = if rest.is_empty() {
        else_body.map_or(Body::Empty, Body::Statements)
    } else {
        Body::Chain(rest, else_body)
    };
    lower_conditional(
        builder,
        scope,
        env,
        condition,
        &true_body,
        &false_body,
        span,
    )
}

/// One `conditional` region pair over an evaluated `condition`.
fn lower_conditional<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    condition: ProgramRegister<'p>,
    true_body: &Body<'_>,
    false_body: &Body<'_>,
    span: Span,
) -> Result<(), SolveProgramConstructionError> {
    // Status bits are written by comparisons inside either branch, so they
    // are always carried.
    let mut assigned = BTreeSet::new();
    let mut read = BTreeSet::new();
    true_body.effects(scope, &mut assigned, &mut read);
    false_body.effects(scope, &mut assigned, &mut read);
    assigned.extend(scope.scope.status.iter().map(|&index| VarId::Block(index)));
    read.extend(scope.scope.status.iter().map(|&index| VarId::Block(index)));
    let captures = read
        .union(&assigned)
        .copied()
        .filter(|&id| env.available(scope, id))
        .collect::<Vec<_>>();
    let mut capture_values = Vec::with_capacity(captures.len());
    for &id in &captures {
        capture_values.push(env.read(builder, scope, id, span)?);
    }
    let outputs = assigned.iter().copied().collect::<Vec<_>>();
    let output_types = outputs
        .iter()
        .map(|&id| scope.variable(id).value_type.clone())
        .collect::<Vec<_>>();
    let definite_true = true_body.definite(scope);
    let definite_false = false_body.definite(scope);
    let results = builder.conditional(
        condition,
        &capture_values,
        output_types,
        span,
        |region, inputs, region_outputs| {
            lower_branch(
                region,
                scope,
                &captures,
                &outputs,
                inputs,
                region_outputs,
                true_body,
                span,
            )
        },
        |region, inputs, region_outputs| {
            lower_branch(
                region,
                scope,
                &captures,
                &outputs,
                inputs,
                region_outputs,
                false_body,
                span,
            )
        },
    )?;
    for (&id, value) in outputs.iter().zip(results) {
        if let Some(slot) = env.slot(scope, id) {
            builder.store(slot, value, span)?;
        }
        env.values.insert(id, value);
    }
    env.definite
        .extend(definite_true.intersection(&definite_false).copied());
    Ok(())
}

/// A top-level `if` chain under an optional enclosing `guard`: each condition
/// is evaluated once, only where no earlier branch was taken, and each body
/// statement runs in its own region guarded by its branch.
fn lower_if_split<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    if_statement: &IfStatement,
    guard: Option<ProgramRegister<'p>>,
    span: Span,
) -> Result<(), SolveProgramConstructionError> {
    // Where no earlier branch was taken (within `guard`).
    let mut remaining = guard;
    for branch in &if_statement.branches {
        let Condition::Expression(condition) = &branch.condition else {
            return Err(SolveProgramConstructionError::InvalidRegion { provenance: span });
        };
        let value = match remaining {
            None => lower_expression(builder, scope, env, condition, span)?,
            Some(remaining) => lower_lazy_select(
                builder,
                scope,
                env,
                remaining,
                condition,
                &[],
                &Expression::Bool(false),
                span,
            )?,
        };
        let taken = match remaining {
            None => value,
            Some(remaining) => builder.binary(SolveBinaryOperator::And, remaining, value, span)?,
        };
        lower_guarded_statements(builder, scope, env, taken, &branch.body)?;
        let not_taken = builder.unary(SolveUnaryOperator::Not, value, span)?;
        remaining = Some(match remaining {
            None => not_taken,
            Some(remaining) => {
                builder.binary(SolveBinaryOperator::And, remaining, not_taken, span)?
            }
        });
    }
    if let (Some(body), Some(remaining)) = (&if_statement.else_body, remaining) {
        lower_guarded_statements(builder, scope, env, remaining, body)?;
    }
    Ok(())
}

fn lower_guarded_statements<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    guard: ProgramRegister<'p>,
    statements: &[Spanned<Statement>],
) -> Result<(), SolveProgramConstructionError> {
    for statement in statements {
        if let Statement::If(nested) = &statement.node {
            lower_if_split(builder, scope, env, nested, Some(guard), statement.span)?;
            continue;
        }
        let body = Body::Statements(std::slice::from_ref(statement));
        lower_conditional(
            builder,
            scope,
            env,
            guard,
            &body,
            &Body::Empty,
            statement.span,
        )?;
    }
    Ok(())
}

// SPEC_0021: Exception - one branch region needs the parent scope, carried variables, captures and status channels together.
#[allow(clippy::too_many_arguments)]
fn lower_branch<'r>(
    region: &mut TypedProgramBuilder<'r>,
    scope: &MethodScope<'_>,
    captures: &[VarId],
    outputs: &[VarId],
    inputs: &[ProgramSlot<'r>],
    region_outputs: &[ProgramSlot<'r>],
    body: &Body<'_>,
    span: Span,
) -> Result<(), SolveProgramConstructionError> {
    let mut env = Env {
        values: BTreeMap::new(),
        slots: None,
        definite: BTreeSet::new(),
    };
    for (&id, &input) in captures.iter().zip(inputs) {
        let value = region.load(input, span)?;
        env.values.insert(id, value);
    }
    match body {
        Body::Empty => {}
        Body::Statements(statements) => lower_statements(region, scope, &mut env, statements)?,
        Body::Chain(branches, else_body) => {
            lower_if(region, scope, &mut env, branches, *else_body, span)?;
        }
    }
    for (&id, &output) in outputs.iter().zip(region_outputs) {
        let value = env
            .values
            .get(&id)
            .copied()
            .ok_or(SolveProgramConstructionError::UninitializedSlot { provenance: span })?;
        region.store(output, value, span)?;
    }
    Ok(())
}

/// One arm of a lowered `if` chain.
enum Body<'a> {
    Empty,
    Statements(&'a [Spanned<Statement>]),
    Chain(&'a [IfBranch], Option<&'a [Spanned<Statement>]>),
}

impl Body<'_> {
    /// Collect the variables this arm assigns and reads.
    fn effects(
        &self,
        scope: &MethodScope<'_>,
        assigned: &mut BTreeSet<VarId>,
        read: &mut BTreeSet<VarId>,
    ) {
        match self {
            Self::Empty => {}
            Self::Statements(statements) => statement_effects(scope, statements, assigned, read),
            Self::Chain(branches, else_body) => {
                chain_effects(scope, branches, *else_body, assigned, read);
            }
        }
    }

    /// The variables this arm writes on every path through it.
    fn definite(&self, scope: &MethodScope<'_>) -> BTreeSet<VarId> {
        match self {
            Self::Empty => BTreeSet::new(),
            Self::Statements(statements) => definite_writes(scope, statements),
            Self::Chain(branches, else_body) => {
                let Some(else_body) = else_body else {
                    return BTreeSet::new();
                };
                let mut sets = branches
                    .iter()
                    .map(|branch| definite_writes(scope, &branch.body))
                    .collect::<Vec<_>>();
                sets.push(definite_writes(scope, else_body));
                intersect(sets)
            }
        }
    }
}

fn intersect(sets: Vec<BTreeSet<VarId>>) -> BTreeSet<VarId> {
    let mut sets = sets.into_iter();
    let Some(first) = sets.next() else {
        return BTreeSet::new();
    };
    sets.fold(first, |acc, set| acc.intersection(&set).copied().collect())
}

fn definite_writes(scope: &MethodScope<'_>, statements: &[Spanned<Statement>]) -> BTreeSet<VarId> {
    let mut written = BTreeSet::new();
    for statement in statements {
        match &statement.node {
            // An element write leaves the rest of the array as it was.
            Statement::Assignment { target, .. } => {
                if let Ok(id) = scope.resolve(target) {
                    written.insert(id);
                }
            }
            Statement::MultiAssignment { targets, .. } => {
                written.extend(
                    targets
                        .iter()
                        .filter_map(|target| scope.resolve(target).ok()),
                );
            }
            Statement::If(if_statement) => {
                written.extend(
                    Body::Chain(&if_statement.branches, if_statement.else_body.as_deref())
                        .definite(scope),
                );
            }
            _ => {}
        }
    }
    written
}

fn statement_effects(
    scope: &MethodScope<'_>,
    statements: &[Spanned<Statement>],
    assigned: &mut BTreeSet<VarId>,
    read: &mut BTreeSet<VarId>,
) {
    for statement in statements {
        match &statement.node {
            Statement::Assignment { target, value } => {
                expression_reads(scope, value, read);
                place_effects(scope, target, assigned, read);
            }
            Statement::MultiAssignment { targets, call } => {
                for argument in &call.arguments {
                    expression_reads(scope, argument, read);
                }
                for target in targets {
                    place_effects(scope, target, assigned, read);
                }
            }
            Statement::Call(call) => {
                for argument in &call.arguments {
                    expression_reads(scope, argument, read);
                }
            }
            Statement::If(if_statement) => {
                Body::Chain(&if_statement.branches, if_statement.else_body.as_deref())
                    .effects(scope, assigned, read);
            }
            Statement::For(for_loop) => loop_effects(scope, for_loop, assigned, read),
            Statement::Limit(targets) => {
                for target in targets {
                    limit_effects(scope, target, assigned, read);
                }
            }
            _ => {}
        }
    }
}

/// An assignment target: an element write also reads the array and its
/// subscripts.
fn place_effects(
    scope: &MethodScope<'_>,
    target: &Reference,
    assigned: &mut BTreeSet<VarId>,
    read: &mut BTreeSet<VarId>,
) {
    let Ok((id, subscripts)) = scope.resolve_place(target) else {
        return;
    };
    assigned.insert(id);
    if !subscripts.is_empty() {
        read.insert(id);
    }
    for subscript in subscripts {
        expression_reads(scope, subscript, read);
    }
}

/// A loop's body effects, without its own iterator.
fn loop_effects(
    scope: &MethodScope<'_>,
    for_loop: &ForLoop,
    assigned: &mut BTreeSet<VarId>,
    read: &mut BTreeSet<VarId>,
) {
    let Some(iterator) = &for_loop.iterator else {
        return;
    };
    scope.with_loop(iterator.lexeme(), |depth| {
        statement_effects(scope, &for_loop.body, assigned, read);
        read.remove(&VarId::Loop(depth));
    });
}

fn chain_effects(
    scope: &MethodScope<'_>,
    branches: &[IfBranch],
    else_body: Option<&[Spanned<Statement>]>,
    assigned: &mut BTreeSet<VarId>,
    read: &mut BTreeSet<VarId>,
) {
    for branch in branches {
        if let Condition::Expression(condition) = &branch.condition {
            expression_reads(scope, condition, read);
        }
        statement_effects(scope, &branch.body, assigned, read);
    }
    if let Some(body) = else_body {
        statement_effects(scope, body, assigned, read);
    }
}

/// A `limit` both reads and writes each variable it saturates.
fn limit_effects(
    scope: &MethodScope<'_>,
    target: &LimitTarget,
    assigned: &mut BTreeSet<VarId>,
    read: &mut BTreeSet<VarId>,
) {
    let ids = match target {
        LimitTarget::SelfState => (0..scope.scope.block.len())
            .map(VarId::Block)
            .filter(|&id| is_ranged(scope.variable(id)))
            .collect::<Vec<_>>(),
        LimitTarget::Reference(reference) => scope.resolve(reference).into_iter().collect(),
    };
    assigned.extend(ids.iter().copied());
    read.extend(ids);
}

fn expression_reads(scope: &MethodScope<'_>, expression: &Expression, read: &mut BTreeSet<VarId>) {
    match expression {
        Expression::Ref(reference) | Expression::Neg(reference) => {
            if let Ok((id, subscripts)) = scope.resolve_place(reference) {
                read.insert(id);
                for subscript in subscripts {
                    expression_reads(scope, subscript, read);
                }
            }
        }
        Expression::Array(items) => {
            for item in items {
                expression_reads(scope, item, read);
            }
        }
        Expression::Paren(inner) | Expression::Not(inner) => expression_reads(scope, inner, read),
        Expression::Binary { lhs, rhs, .. } => {
            expression_reads(scope, lhs, read);
            expression_reads(scope, rhs, read);
        }
        Expression::If(if_expression) => {
            for (condition, value) in &if_expression.branches {
                expression_reads(scope, condition, read);
                expression_reads(scope, value, read);
            }
            expression_reads(scope, &if_expression.else_value, read);
        }
        Expression::Call(call) => {
            for argument in &call.arguments {
                expression_reads(scope, argument, read);
            }
        }
        _ => {}
    }
}

mod check;
mod definedness;
mod expressions;
mod faults;
mod functions;

#[cfg(test)]
mod tests;
