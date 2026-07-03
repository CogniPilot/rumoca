//! DAE → [`AlgorithmCodePackage`] lowering (the projection's back half).
//!
//! Pipeline (GAL-004: checks over the untouched DAE, output re-validated):
//!
//! 1. [`check_admissibility`] over the untouched canonical DAE;
//! 2. [`classify_variables`] (GAL-020);
//! 3. index the condition surface (`conditions` module) and lower every
//!    `f_z`/`f_m` row: unwrap the sample-tick when-edge guard (`guard`
//!    module), lower the body (`expr` module) into a flat `DoStep`
//!    assignment, then topologically order the assignments by their
//!    current-tick reads (`schedule` module — MLS B.1b rows are
//!    simultaneous, so DAE order is not causal; discrete algebraic loops
//!    are rejected, never emitted in a silently wrong order);
//! 4. drop generated `__pre__.` slots that only served dropped hold
//!    branches, then build the manifest variables over the kept set;
//! 5. wire the manifest `<Clock>` to the sample-period constant (`clock`
//!    module);
//! 6. build the block: interface/protected sections, `Startup` (all
//!    variables, literal starts mirroring the manifest, dependent
//!    parameters recomputed symbolically), `Recalibrate` (dependents only,
//!    emitted even when empty), `DoStep` (flat assignments + `'previous(x)'`
//!    commits at the end, trap T2);
//! 7. post-validate: `rumoca_galec::validate` over the block and the typed
//!    Algorithm Code manifest validation — any failure here is a bug in
//!    this projection and reported as an internal error (ET018), never
//!    shipped.
//!
//! # D8 (Real relationals and escape sets, trap T9)
//!
//! Per SPEC_0034 D8, slice 1 lowers Real relational operators with empty
//! escape-set accounting (matching the `rumoca-galec` validator's
//! documented NAN deferral), while rejecting every construct whose escape
//! set would have to be non-empty to conform: Real→Integer narrowing
//! (`unsupported-feature:real-to-integer-conversion`, which would also need
//! the floor-vs-truncate rewrite of trap T8) and anything requiring the
//! signaling linear-solver builtins. Full NAN accounting (T9) is tracked
//! for slice 2, at which point the relational stance is revisited together
//! with the validator.

use rumoca_galec::ast::{self as gast, Block, Statement};
use rumoca_ir_dae::Equation;

use crate::admissibility::check_admissibility;
use crate::classify::{Classification, classify_variables};
use crate::diagnostic::GalecTargetError;
use crate::input::{GalecInput, GalecOptions};
use crate::manifest_vars::build_manifest_variables;
use crate::package::{AlgorithmCodePackage, ManifestFragment};

pub(crate) mod clock;
pub(crate) mod conditions;
pub(crate) mod expr;
pub(crate) mod guard;
pub(crate) mod methods;
pub(crate) mod schedule;

use conditions::ConditionTable;
use expr::ExprLowerer;

pub use expr::emittable_builtin_targets;

/// Lower a canonical DAE to an eFMI Algorithm Code package.
///
/// # Errors
///
/// Returns ALL collected diagnostics: admissibility rejections (ET001–
/// ET009), classification/manifest failures (ET010–ET016), and lowering
/// rejections (ET017–ET020). The input DAE is never mutated (GAL-002).
pub fn lower_to_algorithm_code(
    input: &GalecInput<'_>,
    options: &GalecOptions,
) -> Result<AlgorithmCodePackage, Vec<GalecTargetError>> {
    let clock = check_admissibility(input)?;
    let classification = classify_variables(input)?;
    let conditions = ConditionTable::build(input.dae).map_err(|error| vec![error])?;
    let sample_index = conditions.sample_index().map_err(|error| vec![error])?;

    // 3. DoStep rows: unwrap guards, lower bodies, record read pre slots.
    let mut lowerer = ExprLowerer::new(&classification, &conditions);
    let mut do_step = Vec::new();
    let mut errors = Vec::new();
    let updates = input
        .dae
        .discrete
        .real_updates
        .iter()
        .chain(input.dae.discrete.valued_updates.iter());
    for equation in updates {
        match lower_update_row(
            equation,
            &classification,
            &conditions,
            sample_index,
            &mut lowerer,
        ) {
            Ok(statement) => do_step.push(statement),
            Err(error) => errors.push(error),
        }
    }
    if !errors.is_empty() {
        return Err(errors);
    }
    // MLS B.1b rows are simultaneous; order the sequential DoStep by
    // current-tick reads (discrete algebraic loops are rejected).
    let mut do_step = schedule::order_by_dependencies(do_step).map_err(|error| vec![error])?;

    // 4. Keep only the pre slots the emitted code reads; unread slots
    // existed solely for the dropped keep-previous hold branches.
    let referenced = lowerer.into_referenced_pre();
    let kept = Classification::new(
        classification
            .variables
            .iter()
            .filter(|classified| {
                classified.pre_base.is_none()
                    || referenced.contains(classified.variable.name.as_str())
            })
            .cloned()
            .collect(),
    );
    let mut manifest = build_manifest_variables(&kept)?;
    let starts = methods::manifest_by_dae_name(&manifest);

    // 6. Block sections + methods (borrowing `starts` before clock wiring
    // may extend the manifest).
    let (interface, mut protected) =
        methods::build_sections(&kept, &starts).map_err(|error| vec![error])?;
    let mut startup = methods::build_startup(&kept, &conditions, &starts)?;
    let recalibrate = methods::build_recalibrate(&kept, &conditions)?;
    let commits = methods::build_pre_commits(&kept, &referenced)?;
    do_step.extend(commits);

    // 5. Clock wiring (may synthesize a period constant).
    let wired = clock::wire_clock(
        &clock,
        conditions.sample_period_expr(),
        &kept,
        &mut manifest,
    )
    .map_err(|error| vec![error])?;
    if let Some(constant) = &wired.synthesized {
        protected.push(clock::synthesized_declaration(constant));
        startup.push(clock::synthesized_startup_statement(constant));
    }

    let block_name = block_name(input, options).map_err(|error| vec![error])?;
    let alg_file_name = format!("{}.alg", crate::mangle::manifest_name(&block_name));
    let mut block = Block::new(block_name);
    block.interface = interface;
    block.protected = protected;
    block.startup.statements = startup;
    block.recalibrate.statements = recalibrate;
    block.do_step.statements = do_step;

    let package = AlgorithmCodePackage {
        block,
        manifest: ManifestFragment {
            variables: manifest.variables,
            clock_variable_ref_id: wired.variable_ref_id,
            // Slice 1 emits nothing that can signal, and Real relationals
            // lower with empty escape accounting (module docs, D8).
            startup_signals: Vec::new(),
            recalibrate_signals: Vec::new(),
            do_step_signals: Vec::new(),
        },
        alg_file_name,
    };
    validate_package(&package)?;
    Ok(package)
}

/// One guarded `f_z`/`f_m` row → one flat `DoStep` assignment.
fn lower_update_row(
    equation: &Equation,
    classification: &Classification<'_>,
    conditions: &ConditionTable<'_>,
    sample_index: Option<usize>,
    lowerer: &mut ExprLowerer<'_>,
) -> Result<Statement, GalecTargetError> {
    let Some(lhs) = &equation.lhs else {
        return Err(GalecTargetError::UnsupportedFeature {
            feature: "implicit-discrete-update".to_owned(),
            detail: format!(
                "discrete update without an explicit target (origin: {})",
                equation.origin
            ),
            span: (!equation.span.is_dummy()).then_some(equation.span),
        });
    };
    let target_name = lhs.as_str();
    let (classified, subscripts) = resolve_target(classification, target_name)?;
    let body = guard::unwrap_guarded_update(equation, target_name, conditions, sample_index)?;
    let typed = lowerer.lower(body)?;
    let value = methods::coerce_to(typed, classified.scalar_type, target_name)?;
    Ok(Statement::Assignment {
        target: gast::Reference::State(vec![gast::RefPart {
            name: classified.galec_name.clone(),
            subscripts,
        }]),
        value,
    })
}

/// Resolve an update target to its classified variable, carrying a rendered
/// element index (`x[2]`) as a literal subscript.
fn resolve_target<'c, 'a>(
    classification: &'c Classification<'a>,
    target_name: &str,
) -> Result<
    (
        &'c crate::classify::ClassifiedVariable<'a>,
        Vec<gast::Expression>,
    ),
    GalecTargetError,
> {
    if let Some(classified) = classification.find(target_name) {
        return Ok((classified, Vec::new()));
    }
    if let Some((base, index)) = conditions::split_indexed_name(target_name)
        && let Some(classified) = classification.find(&base)
    {
        let index = i64::try_from(index).map_err(|_| GalecTargetError::LoweringInternal {
            detail: format!("index of `{target_name}` exceeds i64"),
        })?;
        return Ok((classified, vec![gast::Expression::Integer(index)]));
    }
    Err(GalecTargetError::UnknownVariableReference {
        name: target_name.to_owned(),
        span: None,
    })
}

/// The emitted block name: the override, or the mangled model name.
fn block_name(
    input: &GalecInput<'_>,
    options: &GalecOptions,
) -> Result<gast::Name, GalecTargetError> {
    let source = options.block_name.as_deref().unwrap_or(input.model_name);
    crate::mangle::galec_variable_name(source)
}

/// GAL-004 post-validation: the emitted block must pass the full GALEC
/// validator, and the manifest fragment must assemble into a valid typed
/// Algorithm Code manifest. Failures are projection bugs (ET018).
fn validate_package(package: &AlgorithmCodePackage) -> Result<(), Vec<GalecTargetError>> {
    let mut errors = Vec::new();
    if let Err(diagnostics) = rumoca_galec::validate(&package.block) {
        errors.extend(diagnostics.into_iter().map(|diagnostic| {
            GalecTargetError::LoweringInternal {
                detail: format!("lowering produced an invalid GALEC block: {diagnostic}"),
            }
        }));
    }
    match crate::emit::render_block(&package.block) {
        Ok(alg_text) => {
            if let Err(error) = crate::emit::assemble_manifest(package, alg_text.as_bytes()) {
                errors.push(GalecTargetError::LoweringInternal {
                    detail: format!("lowering produced an invalid manifest fragment: {error}"),
                });
            }
        }
        Err(error) => errors.push(GalecTargetError::LoweringInternal {
            detail: format!("lowering produced an unprintable GALEC block: {error}"),
        }),
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
