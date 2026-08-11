//! `initial equation` → `Startup` lowering (GAL-028, D14).
//!
//! The DAE initialization partition carries two populations:
//!
//! - **fixed-start rows** synthesized by the DAE phase (MLS §8.6, explicit
//!   `lhs`, origin `"fixed start initialization for <name>"`) — these repeat
//!   the variable's `start` attribute, which the literal `Startup` mirroring
//!   already emits, so they are skipped here;
//! - **source `initial equation` rows** in residual form (`lhs == None`,
//!   `rhs == a - b` where the source equation was `a = b`) — these orient to
//!   an assignment, dependency-sort, and lower into `Startup` after the
//!   literal mirroring and the inlined `Recalibrate` recomputation.
//!
//! Ordering inside `Startup` (D14): literal mirroring first, then the
//! dependency-sorted computed statements overwriting the computed subset,
//! then `'previous(x)'` re-seeding for initialized variables with kept pre
//! slots (the pre slot's snapshot start is stale once the base variable is
//! computed).
//!
//! The manifest `start` of every computed variable (and its pre slot) is
//! the projection-time constant evaluation of the computation under default
//! parameter values, recorded as a [`ConstEnv`] override before manifest
//! building — GAL-020 "start mirrors Startup" holds by construction.
//!
//! Rejections (stable `unsupported-feature:` ids, GAL-007):
//!
//! - `implicit-initial-equation` — residual that is not `variable - expr`
//!   (or `expr - variable`) for a known writable variable;
//! - `partial-initial-equation` — element-indexed target (the manifest
//!   `start` override needs the whole variable);
//! - `duplicate-initial-equation` — two computations for one variable;
//! - `initial-equation-reads-input` — control inputs are not valid before
//!   the first tick (GAL-028);
//! - `initial-equation-target` — target is a parameter/constant/input;
//! - `initialization-cycle` — cyclic dependencies among computed variables.

use std::collections::{HashMap, HashSet};

use rumoca_core::{Expression, OpBinary, Span};
use rumoca_ir_dae::{Dae, DaeSymbolTable, Equation};
use rumoca_ir_galec::ast::{self as gast, Statement};

use crate::classify::{Classification, ClassifiedVariable, VariableClass};
use crate::diagnostic::GalecTargetError;
use crate::lower::conditions::ConditionTable;
use crate::lower::expr::ExprLowerer;
use crate::lower::methods;
use crate::manifest_vars::const_eval::{ConstEnv, StartShape};

/// The origin prefix the DAE phase stamps on synthesized MLS §8.6 rows
/// (`rumoca-phase-dae/src/initial.rs`); such rows repeat the `start`
/// attribute the literal mirroring already emits.
const FIXED_START_ORIGIN_PREFIX: &str = "fixed start initialization for ";

/// One oriented source initial equation: `target := value` at `Startup`.
pub(crate) struct OrientedInitial<'a> {
    /// DAE name of the assigned variable.
    pub target: String,
    /// The defining DAE expression.
    pub value: &'a Expression,
    /// Originating Modelica span (D11).
    pub span: Span,
}

/// Orient the initialization partition into `Startup` assignments,
/// dependency-sorted; collect-all diagnostics.
pub(crate) fn orient_initial_equations<'a>(
    dae: &'a Dae,
    classification: &Classification<'_>,
) -> Result<Vec<OrientedInitial<'a>>, Vec<GalecTargetError>> {
    let mut oriented = Vec::new();
    let mut errors = Vec::new();
    let mut seen = HashSet::new();
    for equation in &dae.initialization.equations {
        match orient_one(equation, classification) {
            Ok(None) => {}
            Ok(Some(initial)) => {
                if !seen.insert(initial.target.clone()) {
                    errors.push(unsupported(
                        "duplicate-initial-equation",
                        format!(
                            "variable `{}` is initialized more than once",
                            initial.target
                        ),
                        initial.span,
                    ));
                    continue;
                }
                if let Some(input) = reads_input(initial.value, classification) {
                    errors.push(unsupported(
                        "initial-equation-reads-input",
                        format!(
                            "initial equation for `{}` reads input `{input}`; control \
                             inputs are not valid before the first tick",
                            initial.target
                        ),
                        initial.span,
                    ));
                    continue;
                }
                oriented.push(initial);
            }
            Err(error) => errors.push(error),
        }
    }
    if !errors.is_empty() {
        return Err(errors);
    }
    order_by_initialization_dependencies(oriented).map_err(|error| vec![error])
}

fn orient_one<'a>(
    equation: &'a Equation,
    classification: &Classification<'_>,
) -> Result<Option<OrientedInitial<'a>>, GalecTargetError> {
    if let Some(lhs) = &equation.lhs {
        if equation.origin.starts_with(FIXED_START_ORIGIN_PREFIX) {
            // MLS §8.6 fixed-start row: repeats the `start` attribute the
            // literal mirroring already emits.
            return Ok(None);
        }
        let target = resolve_writable_target(lhs.as_str(), classification, equation.span)?;
        return Ok(Some(OrientedInitial {
            target,
            value: &equation.rhs,
            span: equation.span,
        }));
    }
    // Residual form `0 = a - b` from a source `a = b`.
    if let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &equation.rhs
    {
        if let Some(target) = plain_target_name(lhs, classification) {
            let target = resolve_writable_target(&target, classification, equation.span)?;
            return Ok(Some(OrientedInitial {
                target,
                value: rhs,
                span: equation.span,
            }));
        }
        if let Some(target) = plain_target_name(rhs, classification) {
            let target = resolve_writable_target(&target, classification, equation.span)?;
            return Ok(Some(OrientedInitial {
                target,
                value: lhs,
                span: equation.span,
            }));
        }
    }
    Err(unsupported(
        "implicit-initial-equation",
        format!(
            "initial equation does not have the explicit form `variable = expression` \
             (origin: {})",
            equation.origin
        ),
        equation.span,
    ))
}

/// The unsubscripted variable name a residual side names, when it is a plain
/// reference to a classified variable.
fn plain_target_name(expr: &Expression, classification: &Classification<'_>) -> Option<String> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    classification
        .find(name.as_str())
        .map(|classified| classified.variable.name.as_str().to_owned())
}

/// A target must be a whole (unsubscripted) state or output variable.
fn resolve_writable_target(
    name: &str,
    classification: &Classification<'_>,
    span: Span,
) -> Result<String, GalecTargetError> {
    let Some(classified) = classification.find(name) else {
        // A trailing element index means a scalarized element row.
        if rumoca_core::component_path_trailing_index(name)
            .and_then(|(base, _)| classification.find(&base))
            .is_some()
        {
            return Err(unsupported(
                "partial-initial-equation",
                format!(
                    "initial equation targets element `{name}`; only whole-variable \
                     initialization lowers (the manifest start mirrors the whole value)"
                ),
                span,
            ));
        }
        return Err(GalecTargetError::UnknownVariableReference {
            name: name.to_owned(),
            span: (!span.is_dummy()).then_some(span),
        });
    };
    match classified.class {
        VariableClass::State | VariableClass::Output => Ok(name.to_owned()),
        VariableClass::Input
        | VariableClass::TunableParameter
        | VariableClass::DependentParameter
        | VariableClass::Constant => Err(unsupported(
            "initial-equation-target",
            format!(
                "initial equation targets `{name}` ({:?}); only states and outputs \
                 take computed initialization",
                classified.class
            ),
            span,
        )),
    }
}

/// The first input variable an expression reads, if any (GAL-028).
fn reads_input(expr: &Expression, classification: &Classification<'_>) -> Option<String> {
    methods::referenced_names(expr).into_iter().find(|name| {
        classification
            .find(name)
            .is_some_and(|classified| classified.class == VariableClass::Input)
    })
}

/// Stable topological order over the computed variables (reads-before-writes
/// among the initialized set); cycles are a stable diagnostic.
fn order_by_initialization_dependencies(
    oriented: Vec<OrientedInitial<'_>>,
) -> Result<Vec<OrientedInitial<'_>>, GalecTargetError> {
    let index_by_target: HashMap<&str, usize> = oriented
        .iter()
        .enumerate()
        .map(|(index, initial)| (initial.target.as_str(), index))
        .collect();
    let mut ordered = Vec::with_capacity(oriented.len());
    let mut state = vec![VisitState::Unvisited; oriented.len()];
    for index in 0..oriented.len() {
        visit(index, &oriented, &index_by_target, &mut state, &mut ordered)?;
    }
    drop(index_by_target);
    let mut slots: Vec<Option<OrientedInitial<'_>>> = oriented.into_iter().map(Some).collect();
    Ok(ordered
        .into_iter()
        .filter_map(|index| slots[index].take())
        .collect())
}

#[derive(Clone, Copy, PartialEq)]
enum VisitState {
    Unvisited,
    Visiting,
    Done,
}

fn visit(
    index: usize,
    oriented: &[OrientedInitial<'_>],
    index_by_target: &HashMap<&str, usize>,
    state: &mut [VisitState],
    ordered: &mut Vec<usize>,
) -> Result<(), GalecTargetError> {
    match state[index] {
        VisitState::Done => return Ok(()),
        VisitState::Visiting => {
            return Err(unsupported(
                "initialization-cycle",
                format!(
                    "initial equations form a dependency cycle through `{}`",
                    oriented[index].target
                ),
                oriented[index].span,
            ));
        }
        VisitState::Unvisited => {}
    }
    state[index] = VisitState::Visiting;
    for name in methods::referenced_names(oriented[index].value) {
        if let Some(&dependency) = index_by_target.get(name.as_str()) {
            visit(dependency, oriented, index_by_target, state, ordered)?;
        }
    }
    state[index] = VisitState::Done;
    ordered.push(index);
    Ok(())
}

/// Record manifest `start` overrides: the projection-time evaluation of each
/// computation (in dependency order) under default parameter values, plus
/// the same shape for the variable's pre slot (its snapshot start is stale).
pub(crate) fn record_start_overrides(
    oriented: &[OrientedInitial<'_>],
    classification: &Classification<'_>,
    env: &mut ConstEnv<'_>,
) -> Result<(), Vec<GalecTargetError>> {
    let mut errors = Vec::new();
    for initial in oriented {
        let Some(classified) = classification.find(&initial.target) else {
            continue;
        };
        match env.evaluate_start_shape(initial.value) {
            Ok(shape) => {
                if let StartShape::Scalar(value) = &shape {
                    env.insert_computed(initial.target.clone(), *value);
                }
                if let Some(pre_slot) = pre_slot_name(classification, &initial.target) {
                    env.insert_start_override(pre_slot, shape.clone());
                }
                env.insert_start_override(initial.target.clone(), shape);
            }
            Err(failure) => errors.push(failure.into_error(classified.variable, "start")),
        }
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// The DAE name of the kept pre slot whose base is `target`, if any.
fn pre_slot_name(classification: &Classification<'_>, target: &str) -> Option<String> {
    classification
        .variables
        .iter()
        .find(|classified| classified.pre_base.as_deref() == Some(target))
        .map(|classified| classified.variable.name.as_str().to_owned())
}

/// Lower the oriented computations into `Startup` statements (already
/// dependency-ordered), then re-seed `'previous(x)'` for computed variables
/// with kept pre slots.
pub(crate) fn lower_into_startup(
    oriented: &[OrientedInitial<'_>],
    classification: &Classification<'_>,
    conditions: &ConditionTable<'_>,
    functions: &DaeSymbolTable,
) -> Result<Vec<gast::Spanned<Statement>>, Vec<GalecTargetError>> {
    let mut lowerer = ExprLowerer::new(classification, conditions, functions);
    let mut statements = Vec::new();
    let mut seeds = Vec::new();
    let mut errors = Vec::new();
    for initial in oriented {
        let Some(classified) = classification.find(&initial.target) else {
            continue;
        };
        match lower_one(initial, classified, &mut lowerer) {
            Ok(statement) => {
                statements.push(statement);
                if let Some(seed) = pre_seed(classification, classified, &initial.target) {
                    seeds.push(seed);
                }
            }
            Err(error) => errors.push(error),
        }
    }
    if !errors.is_empty() {
        return Err(errors);
    }
    statements.extend(seeds);
    Ok(statements)
}

fn lower_one(
    initial: &OrientedInitial<'_>,
    classified: &ClassifiedVariable<'_>,
    lowerer: &mut ExprLowerer<'_>,
) -> Result<gast::Spanned<Statement>, GalecTargetError> {
    let typed = lowerer.lower(initial.value)?;
    let value = methods::coerce_to(typed, classified.scalar_type, &initial.target)?;
    Ok(gast::Spanned::new(
        Statement::Assignment {
            target: state_target(classified.galec_name.clone()),
            value,
        },
        initial.span,
    ))
}

/// `'previous(x)' := x` re-seed after the computed assignment (D14).
fn pre_seed(
    classification: &Classification<'_>,
    computed: &ClassifiedVariable<'_>,
    target: &str,
) -> Option<gast::Spanned<Statement>> {
    let pre_name = pre_slot_name(classification, target)?;
    let pre = classification.find(&pre_name)?;
    Some(gast::Spanned::dummy(Statement::Assignment {
        target: state_target(pre.galec_name.clone()),
        value: crate::lower::expr::state_ref(computed.galec_name.clone(), Vec::new()),
    }))
}

fn state_target(name: gast::Name) -> gast::Reference {
    gast::Reference::State(vec![gast::RefPart {
        name,
        subscripts: Vec::new(),
        span: Span::DUMMY,
    }])
}

fn unsupported(feature: &str, detail: String, span: Span) -> GalecTargetError {
    GalecTargetError::UnsupportedFeature {
        feature: feature.to_owned(),
        detail,
        span: (!span.is_dummy()).then_some(span),
    }
}
