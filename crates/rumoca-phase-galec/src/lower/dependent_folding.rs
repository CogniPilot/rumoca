//! Dependent parameters bound to Modelica function calls.
//!
//! # Why a fold rather than a call
//!
//! SPEC_0034 GAL-017 lets `Startup` call builtins only, and a function that
//! only `Startup` calls is unreachable from `DoStep`. A dependent parameter
//! bound to a function call therefore has no admissible emission as a call:
//! the flight allocation matrix `wrenchToRotorThrust = quadrotorWrenchToThrust(
//! geometry...)` refuses with EG028 and EG027 together.
//!
//! When every input the binding reads is frozen at generation time, the call
//! has exactly one value and that value is a literal. Folding it is not an
//! optimization: it is the only emission that satisfies GAL-017 while
//! preserving the model's meaning. `Recalibrate` keeps running the same
//! statement, which stays correct precisely because a frozen input cannot
//! change between `Startup` and `Recalibrate`. That is what `first_unfrozen`
//! proves, and why a binding that reads a tunable parameter or an input is
//! refused rather than folded.
//!
//! # Which evaluation produces the value
//!
//! The value is the one `build_projected_variable` already derived for this
//! variable through `rumoca_eval_dae::NumericEvaluator`, the single owner of
//! generation-time numeric evaluation in this projection: every `start`,
//! `min`, `max`, and `nominal` in the emitted block comes from it. The fold
//! introduces no second evaluator and no second traversal. It emits the value
//! the projection had already proven for this exact expression.
//!
//! Evaluating the Modelica function rather than its GALEC lowering also
//! discharges the function's own preconditions at generation time:
//! `NumericEvaluator` executes `assert` statements inside a called function
//! and fails the compilation when one does not hold. A geometry that admits no
//! allocation inverse is therefore rejected while the code is generated,
//! instead of raising an eFMI error signal on the vehicle.

use std::collections::{HashMap, HashSet};

use rumoca_ir_dae as dae;
use rumoca_ir_galec::ast as gast;
use rumoca_ir_galec::package::ConstantFoldedParameter;

use super::{ClassifiedVariable, ProjectionParts, VariableClass, state_reference};
use crate::diagnostic::GalecTargetError;

/// Largest dependent parameter, in scalars, the projection will fold.
///
/// A folded value is emitted as a literal in both `Startup` and `Recalibrate`,
/// so its scalars are paid for twice in the generated code's text. The cap
/// keeps a large table from silently trading an embedded target's whole text
/// budget for a matrix its designer expected to stay a call.
const FOLD_SCALAR_CAP: usize = 1024;

/// One value a binding reads that is not fixed when the code is generated.
///
/// A dependent parameter that is itself unfrozen names the value that made it
/// unfrozen instead of naming itself, so a diagnostic reports the root the
/// author can act on rather than the nearest link in the chain.
#[derive(Clone)]
struct Unfrozen {
    name: String,
    kind: &'static str,
    remedy: &'static str,
}

/// Lower every dependent parameter in `order` into `Startup`/`Recalibrate`.
///
/// `order` is the topological order over dependent parameters, so a parameter
/// is reached only after everything it reads has been decided. That order is
/// what makes the frozen-input proof transitive: a dependent parameter is
/// frozen exactly when every parameter it reads is already known frozen.
pub(super) fn append_dependent_parameters<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
    order: &[u32],
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
    parts: &mut ProjectionParts,
) -> Result<(), GalecTargetError> {
    let mut frozen = HashSet::new();
    let mut unfrozen_roots: HashMap<u32, Unfrozen> = HashMap::new();
    for id in order {
        let classified = by_id
            .get(id)
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: format!("dependent parameter variable #{id} is absent from classification"),
            })?;
        let unfrozen = first_unfrozen(view, classified, by_id, &frozen, &unfrozen_roots);
        match &unfrozen {
            None => {
                frozen.insert(*id);
            }
            Some(root) => {
                unfrozen_roots.insert(*id, root.clone());
            }
        }
        if let Some(statement) = fold(view, classified, unfrozen, parts)? {
            parts.startup.push(statement.clone());
            parts.recalibrate.push(statement);
            continue;
        }
        let lowered = super::dependent_assignment(view, definitions, classified, by_id, pre_names)?;
        parts.startup.extend(lowered.statements.iter().cloned());
        parts.recalibrate.extend(lowered.statements);
        parts.startup_locals.extend(lowered.locals);
        parts
            .startup_called_user_functions
            .extend(lowered.called_user_functions);
    }
    Ok(())
}

/// Emit one dependent parameter as its generation-time value, or `None` when
/// the binding calls no function and keeps its ordinary lowering.
fn fold<'dae>(
    view: dae::DaeView<'dae>,
    classified: &ClassifiedVariable<'dae>,
    unfrozen: Option<Unfrozen>,
    parts: &mut ProjectionParts,
) -> Result<Option<gast::Spanned<gast::Statement>>, GalecTargetError> {
    let Some(expression) = defining_expression(classified) else {
        return Ok(None);
    };
    let Some(function) = called_function(view, expression) else {
        return Ok(None);
    };
    let span = classified.variable.declaration().span();
    let refuse = |reason: String| GalecTargetError::DependentParameterNotFoldable {
        variable: classified.variable.name().to_string(),
        function: function.clone(),
        reason,
        span: (!span.is_dummy()).then_some(span),
    };
    if let Some(unfrozen) = unfrozen {
        return Err(refuse(format!(
            "it reads `{}`, which is {} and therefore carries no value when the code \
             is generated{}",
            unfrozen.name, unfrozen.kind, unfrozen.remedy
        )));
    }
    let scalars = classified.variable.scalar_count();
    if scalars > FOLD_SCALAR_CAP {
        return Err(refuse(format!(
            "the value carries {scalars} scalars, above the {FOLD_SCALAR_CAP}-scalar \
             generation-time folding limit"
        )));
    }
    let value = parts
        .dependent_starts
        .remove(&classified.id.index())
        .ok_or_else(|| GalecTargetError::LoweringInternal {
            detail: format!(
                "dependent parameter `{}` has no projected generation-time value",
                classified.variable.name()
            ),
        })?;
    parts.constant_folded.push(ConstantFoldedParameter {
        variable: classified.name.lexeme().to_owned(),
        folded_from: function,
        scalars,
    });
    Ok(Some(gast::Spanned::new(
        gast::Statement::Assignment {
            target: state_reference(classified.name.clone(), span),
            value,
        },
        span,
    )))
}

/// Report the first value `classified`'s binding reads that is not fixed when
/// the code is generated.
///
/// A constant qualifies outright. A dependent parameter qualifies only when it
/// is itself already proven frozen, which the topological order guarantees has
/// been decided. Everything else (a tunable parameter, an input, a state, a
/// causal local) can change after the code is generated, so a value folded
/// from it would be wrong on the very run that changed it.
fn first_unfrozen<'dae>(
    view: dae::DaeView<'dae>,
    classified: &ClassifiedVariable<'dae>,
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    frozen: &HashSet<u32>,
    unfrozen_roots: &HashMap<u32, Unfrozen>,
) -> Option<Unfrozen> {
    let expression = defining_expression(classified)?;
    let mut witness = None;
    dae::for_each_expression(view, expression, |_, node| {
        if witness.is_some() {
            return;
        }
        let Some(reference) = node.variable_coordinate() else {
            return;
        };
        let index = reference.index();
        let read = by_id.get(&index);
        match read.map(|read| read.class) {
            Some(VariableClass::Constant) => {}
            Some(VariableClass::DependentParameter) if frozen.contains(&index) => {}
            // The chain's root is what an author can change, so a dependent
            // parameter reports the value that made it unfrozen.
            Some(VariableClass::DependentParameter) => {
                witness = Some(
                    unfrozen_roots
                        .get(&index)
                        .cloned()
                        .unwrap_or_else(|| named(read, index, VariableClass::DependentParameter)),
                );
            }
            other => {
                witness = Some(named(
                    read,
                    index,
                    other.unwrap_or(VariableClass::TunableParameter),
                ));
            }
        }
    });
    witness
}

/// Describe one read variable for a refusal message.
fn named(read: Option<&ClassifiedVariable<'_>>, index: u32, class: VariableClass) -> Unfrozen {
    Unfrozen {
        name: read.map_or_else(
            || format!("#{index}"),
            |read| read.variable.name().to_string(),
        ),
        kind: class_name(class),
        remedy: class_remedy(class),
    }
}

/// The expression that defines a dependent parameter, matching the source
/// `build_projected_variable` evaluates for a parameter's projected value.
fn defining_expression<'dae>(classified: &ClassifiedVariable<'dae>) -> Option<dae::ExprId<'dae>> {
    classified
        .variable
        .binding()
        .or(classified.variable.start())
}

/// Name the Modelica function a dependent-parameter binding calls, if any.
///
/// A call is exactly the construct GAL-017 forbids `Startup` from emitting, so
/// its presence is what selects the fold. Builtins are a separate DAE
/// operation and stay callable from `Startup`.
fn called_function<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<String> {
    let mut called = None;
    dae::for_each_expression(view, expression, |_, node| {
        if called.is_some() {
            return;
        }
        if let dae::ExpressionOperation::Call { function, .. } = node.operation() {
            called = view
                .function(function)
                .map(|function| function.name().to_string());
        }
    });
    called
}

/// How a diagnostic names one classified variable.
const fn class_name(class: VariableClass) -> &'static str {
    match class {
        VariableClass::Input => "a block input",
        VariableClass::Output => "a block output",
        VariableClass::Local => "a step-local value",
        VariableClass::TunableParameter => "a tunable parameter",
        VariableClass::DependentParameter => "a dependent parameter that is itself not frozen",
        VariableClass::Constant => "a constant",
        VariableClass::State => "a block state",
    }
}

/// What an author can change to make one unfrozen read frozen.
///
/// A tunable parameter has a direct remedy in the model: MLS §18.3 makes
/// `annotation(Evaluate = true)` the declaration that a parameter is evaluated
/// at translation time, which is exactly the property the fold needs, and it
/// is what removes the parameter from the block's tunable interface. The other
/// classes vary while the block runs and have no such remedy.
const fn class_remedy(class: VariableClass) -> &'static str {
    match class {
        VariableClass::TunableParameter => {
            ". Declaring that parameter `annotation(Evaluate = true)` evaluates it at \
             translation time and makes this binding foldable"
        }
        VariableClass::Input
        | VariableClass::Output
        | VariableClass::Local
        | VariableClass::DependentParameter
        | VariableClass::Constant
        | VariableClass::State => "",
    }
}
