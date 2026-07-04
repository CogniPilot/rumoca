//! Block sections, `Startup`, `Recalibrate`, and the end-of-`DoStep`
//! `'previous(x)'` commits.
//!
//! - **Sections** (GAL-020): inputs, then outputs, then tunable parameters
//!   before `protected` (the validator enforces this interface order);
//!   dependent parameters, constants, and states (including the kept
//!   `'previous(x)'` slots) after `protected`. Declarations carry **no**
//!   `(min=, max=)` range attributes: GALEC ranges saturate (trap T3),
//!   which is the opposite of Modelica min/max semantics — bounds stay in
//!   the manifest, where they are informational.
//! - **Startup** (GAL-017) initializes ALL writable block variables,
//!   builtins only: literal start values exactly mirroring the manifest
//!   `start`s (both are produced from the same evaluation), then dependent
//!   parameters recomputed **symbolically** from tunables in dependency
//!   order — never folded to literals (parameters stay tunable, GAL-020).
//! - **Recalibrate** recomputes exactly the dependent-parameter slice and
//!   is emitted even when empty (GAL-017).
//! - **Commits**: `DoStep` ends with `'previous(x)' := x` for exactly the
//!   referenced pre slots (trap T2), in classification order.

use std::collections::{HashMap, HashSet};

use crate::manifest_context::algorithm_code_manifest::{StartValue, Variable as ManifestVariable};
use rumoca_ir_galec::ast::{
    self as gast, Dimension, InterfaceKind, InterfaceVariable, ProtectedEntity, ProtectedKind,
    RefPart, Reference, ScalarType, Statement, VariableDeclaration,
};

use crate::classify::{Classification, ClassifiedVariable, VariableClass};
use crate::diagnostic::GalecTargetError;
use crate::lower::conditions::ConditionTable;
use crate::lower::expr::{ExprLowerer, state_ref, widen_to_real};
use crate::manifest_vars::ManifestVariables;

/// Lookup from DAE variable name to its built manifest variable.
pub(crate) fn manifest_by_dae_name(
    manifest: &ManifestVariables,
) -> HashMap<&str, &ManifestVariable> {
    let by_id: HashMap<&str, &ManifestVariable> = manifest
        .variables
        .iter()
        .map(|variable| (variable.common().id.as_str(), variable))
        .collect();
    manifest
        .ids_by_dae_name
        .iter()
        .filter_map(|(dae_name, id)| {
            by_id
                .get(id.as_str())
                .map(|variable| (dae_name.as_str(), *variable))
        })
        .collect()
}

/// Build the interface and protected sections from the filtered
/// classification, with `start` fields mirroring the manifest values.
pub(crate) fn build_sections(
    classification: &Classification<'_>,
    starts: &HashMap<&str, &ManifestVariable>,
) -> Result<(Vec<InterfaceVariable>, Vec<ProtectedEntity>), GalecTargetError> {
    let mut inputs = Vec::new();
    let mut outputs = Vec::new();
    let mut tunables = Vec::new();
    let mut protected = Vec::new();
    for classified in listed(classification) {
        let decl = declaration(classified)?;
        let start = match starts.get(classified.variable.name.as_str()) {
            Some(variable) => Some(start_literal(variable)?),
            None => None,
        };
        match classified.class {
            VariableClass::Input => inputs.push(interface(InterfaceKind::Input, decl, start)),
            VariableClass::Output => outputs.push(interface(InterfaceKind::Output, decl, start)),
            VariableClass::TunableParameter => {
                tunables.push(interface(InterfaceKind::TunableParameter, decl, start));
            }
            VariableClass::DependentParameter => protected.push(ProtectedEntity {
                kind: ProtectedKind::DependentParameter,
                decl,
                start,
            }),
            VariableClass::Constant => protected.push(ProtectedEntity {
                kind: ProtectedKind::Constant,
                decl,
                start,
            }),
            VariableClass::State => protected.push(ProtectedEntity {
                kind: ProtectedKind::State,
                decl,
                start,
            }),
        }
    }
    let mut interface_vars = inputs;
    interface_vars.append(&mut outputs);
    interface_vars.append(&mut tunables);
    Ok((interface_vars, protected))
}

fn interface(
    kind: InterfaceKind,
    decl: VariableDeclaration,
    start: Option<gast::Expression>,
) -> InterfaceVariable {
    InterfaceVariable { kind, decl, start }
}

fn declaration(
    classified: &ClassifiedVariable<'_>,
) -> Result<VariableDeclaration, GalecTargetError> {
    let mut decl =
        VariableDeclaration::scalar(classified.scalar_type, classified.galec_name.clone());
    for size in &classified.variable.dims {
        if *size < 1 {
            return Err(GalecTargetError::LoweringInternal {
                detail: format!(
                    "non-positive dimension on `{}` survived admissibility",
                    classified.variable.name.as_str()
                ),
            });
        }
        decl.dimensions
            .push(Dimension::Expr(gast::Expression::Integer(*size)));
    }
    Ok(decl)
}

/// `Startup` statements: literal initialization of every writable
/// manifest-listed variable (GAL-017), then the symbolic
/// dependent-parameter recomputation.
///
/// Control inputs are excluded per GAL-017: GALEC inputs are read-only
/// inside the block (the side-effect analysis rejects writes to them,
/// EG033) — the environment provides them; their manifest `start` remains
/// the documented default.
pub(crate) fn build_startup(
    classification: &Classification<'_>,
    conditions: &ConditionTable<'_>,
    starts: &HashMap<&str, &ManifestVariable>,
) -> Result<Vec<gast::Spanned<Statement>>, Vec<GalecTargetError>> {
    let mut statements = Vec::new();
    let mut errors = Vec::new();
    for classified in listed(classification) {
        if matches!(
            classified.class,
            VariableClass::DependentParameter | VariableClass::Input
        ) {
            continue;
        }
        let Some(manifest_variable) = starts.get(classified.variable.name.as_str()) else {
            errors.push(GalecTargetError::LoweringInternal {
                detail: format!(
                    "variable `{}` has no manifest entry to mirror in Startup",
                    classified.variable.name.as_str()
                ),
            });
            continue;
        };
        match start_literal(manifest_variable) {
            Ok(value) => statements.push(gast::Spanned::new(
                Statement::Assignment {
                    target: plain_state_target(classified),
                    value,
                },
                init_origin(classified),
            )),
            Err(error) => errors.push(error),
        }
    }
    match build_recalibrate(classification, conditions) {
        Ok(mut dependents) => statements.append(&mut dependents),
        Err(mut dependent_errors) => errors.append(&mut dependent_errors),
    }
    if errors.is_empty() {
        Ok(statements)
    } else {
        Err(errors)
    }
}

/// `Recalibrate` statements: dependent parameters recomputed symbolically
/// from their preserved defining expressions, in dependency order.
pub(crate) fn build_recalibrate(
    classification: &Classification<'_>,
    conditions: &ConditionTable<'_>,
) -> Result<Vec<gast::Spanned<Statement>>, Vec<GalecTargetError>> {
    let dependents: Vec<&ClassifiedVariable<'_>> = listed(classification)
        .filter(|classified| classified.class == VariableClass::DependentParameter)
        .collect();
    let ordered = dependency_order(&dependents).map_err(|error| vec![error])?;
    let mut lowerer = ExprLowerer::new(classification, conditions);
    let mut statements = Vec::new();
    let mut errors = Vec::new();
    for classified in ordered {
        match dependent_assignment(classified, &mut lowerer) {
            Ok(statement) => statements.push(statement),
            Err(error) => errors.push(error),
        }
    }
    if errors.is_empty() {
        Ok(statements)
    } else {
        Err(errors)
    }
}

fn dependent_assignment(
    classified: &ClassifiedVariable<'_>,
    lowerer: &mut ExprLowerer<'_>,
) -> Result<gast::Spanned<Statement>, GalecTargetError> {
    let name = classified.variable.name.as_str();
    let Some(defining) = &classified.variable.start else {
        return Err(GalecTargetError::AttributeNotEvaluable {
            variable: name.to_owned(),
            attribute: "start",
            reason: "dependent parameter has no preserved defining expression".to_owned(),
            span: None,
        });
    };
    let typed = lowerer.lower(defining)?;
    let value = coerce_to(typed, classified.scalar_type, name)?;
    Ok(gast::Spanned::new(
        Statement::Assignment {
            target: plain_state_target(classified),
            value,
        },
        init_origin(classified),
    ))
}

/// Topological order of dependent parameters by start-expression reads.
fn dependency_order<'c, 'a>(
    dependents: &[&'c ClassifiedVariable<'a>],
) -> Result<Vec<&'c ClassifiedVariable<'a>>, GalecTargetError> {
    let names: HashSet<&str> = dependents
        .iter()
        .map(|classified| classified.variable.name.as_str())
        .collect();
    let mut ordered = Vec::with_capacity(dependents.len());
    let mut done: HashSet<&str> = HashSet::new();
    let mut visiting: Vec<&str> = Vec::new();
    for classified in dependents {
        visit(
            classified,
            dependents,
            &names,
            &mut done,
            &mut visiting,
            &mut ordered,
        )?;
    }
    Ok(ordered)
}

fn visit<'c, 'a>(
    classified: &'c ClassifiedVariable<'a>,
    dependents: &[&'c ClassifiedVariable<'a>],
    names: &HashSet<&str>,
    done: &mut HashSet<&'c str>,
    visiting: &mut Vec<&'c str>,
    ordered: &mut Vec<&'c ClassifiedVariable<'a>>,
) -> Result<(), GalecTargetError> {
    let name = classified.variable.name.as_str();
    if done.contains(name) {
        return Ok(());
    }
    if visiting.contains(&name) {
        return Err(GalecTargetError::StartDependencyCycle {
            through: name.to_owned(),
        });
    }
    visiting.push(name);
    if let Some(defining) = &classified.variable.start {
        for read in referenced_names(defining) {
            if names.contains(read.as_str())
                && let Some(dependency) = dependents
                    .iter()
                    .find(|candidate| candidate.variable.name.as_str() == read)
            {
                visit(dependency, dependents, names, done, visiting, ordered)?;
            }
        }
    }
    visiting.pop();
    done.insert(name);
    ordered.push(classified);
    Ok(())
}

/// All variable names read by an expression (for dependency ordering only).
fn referenced_names(expr: &rumoca_core::Expression) -> Vec<String> {
    struct Collector(Vec<String>);
    impl rumoca_core::ExpressionVisitor for Collector {
        fn visit_var_ref(
            &mut self,
            name: &rumoca_core::Reference,
            subscripts: &[rumoca_core::Subscript],
        ) {
            self.0.push(name.as_str().to_owned());
            for subscript in subscripts {
                self.visit_subscript(subscript);
            }
        }
    }
    let mut collector = Collector(Vec::new());
    rumoca_core::ExpressionVisitor::visit_expression(&mut collector, expr);
    collector.0
}

/// End-of-`DoStep` commits: `'previous(x)' := x` for exactly the referenced
/// pre slots, in classification order (trap T2).
pub(crate) fn build_pre_commits(
    classification: &Classification<'_>,
    referenced: &crate::lower::expr::ReferencedPre,
) -> Result<Vec<gast::Spanned<Statement>>, Vec<GalecTargetError>> {
    let mut statements = Vec::new();
    let mut errors = Vec::new();
    for classified in &classification.variables {
        let Some(base) = &classified.pre_base else {
            continue;
        };
        if !referenced.contains(classified.variable.name.as_str()) {
            continue;
        }
        let Some(base_classified) = classification.find(base) else {
            errors.push(GalecTargetError::UnknownVariableReference {
                name: base.clone(),
                span: None,
            });
            continue;
        };
        // A `'previous(x)' := x` commit is synthesized bookkeeping (trap T2)
        // with no originating Modelica construct, so it carries no span (D11).
        statements.push(gast::Spanned::dummy(Statement::Assignment {
            target: plain_state_target(classified),
            value: state_ref(base_classified.galec_name.clone(), Vec::new()),
        }));
    }
    if errors.is_empty() {
        Ok(statements)
    } else {
        Err(errors)
    }
}

/// The originating Modelica span for a generated `Startup`/`Recalibrate`
/// statement over `classified` (D11): its `start=`/binding span when the
/// binding was explicit, else its declaration span. `Span::DUMMY` when neither
/// is real (e.g. a projection-generated variable with no Modelica source).
fn init_origin(classified: &ClassifiedVariable<'_>) -> rumoca_core::Span {
    classified
        .variable
        .start_span
        .unwrap_or(classified.variable.source_span)
}

/// Coerce a lowered value to the target's scalar type: identity, or an
/// explicit `real()` widening; anything else is a diagnostic.
pub(crate) fn coerce_to(
    typed: crate::lower::expr::Typed,
    target: ScalarType,
    target_name: &str,
) -> Result<gast::Expression, GalecTargetError> {
    if typed.ty == target {
        return Ok(typed.expr);
    }
    if target == ScalarType::Real && typed.ty == ScalarType::Integer {
        return widen_to_real(typed, "assignment value", None);
    }
    if target == ScalarType::Integer && typed.ty == ScalarType::Real {
        // The canonical DAE normalizes numeric literals to Real, so an
        // Integer variable's value arrives from the real pipeline as e.g.
        // `Real(10.0)` while Flat-side provenance types the variable
        // Integer. Retyping an exactly-integral Real *literal* is an exact
        // compile-time value conversion — not the rejected runtime
        // `integer()` rewrite, which stays an escape-set problem.
        if let Some(value) = integral_real_literal(&typed.expr) {
            return Ok(gast::Expression::Integer(value));
        }
        return Err(GalecTargetError::UnsupportedFeature {
            feature: "real-to-integer-conversion".to_owned(),
            detail: format!(
                "assignment to Integer `{target_name}` from a Real expression \
                 (GALEC `integer()` truncates toward zero and signals, while \
                 Modelica conversion floors; the rewrite needs escape-set \
                 accounting)"
            ),
            span: None,
        });
    }
    Err(GalecTargetError::LoweringTypeMismatch {
        context: format!("assignment to `{target_name}`"),
        expected: target.keyword(),
        found: typed.ty.keyword(),
        span: None,
    })
}

/// The exact `i64` value of an exactly-integral, exactly-representable
/// Real literal (finite, zero fraction, |value| ≤ 2^53); `None` for
/// anything else — the caller keeps its type-mismatch diagnostic, values
/// are never rounded (SPEC_0008).
fn integral_real_literal(expr: &gast::Expression) -> Option<i64> {
    /// Largest f64 magnitude whose integers are all exactly representable.
    const EXACT_INTEGER_BOUND: f64 = 9_007_199_254_740_992.0; // 2^53
    let gast::Expression::Real(value) = expr else {
        return None;
    };
    (value.is_finite() && value.fract() == 0.0 && value.abs() <= EXACT_INTEGER_BOUND)
        .then_some(*value as i64)
}

/// `self.<galec name>` assignment target of a classified variable.
pub(crate) fn plain_state_target(classified: &ClassifiedVariable<'_>) -> Reference {
    Reference::State(vec![RefPart::plain(classified.galec_name.clone())])
}

/// Manifest-listed classified variables, in classification order.
fn listed<'c, 'a>(
    classification: &'c Classification<'a>,
) -> impl Iterator<Item = &'c ClassifiedVariable<'a>> {
    classification
        .variables
        .iter()
        .filter(|classified| !classified.projection_internal)
}

// ---------------------------------------------------------------------------
// Start literals (mirroring the manifest values)
// ---------------------------------------------------------------------------

/// The GALEC literal for a manifest variable's `start`, row-major nested
/// for arrays with scalar broadcast.
///
/// # Errors
///
/// `ET018` when the manifest-validated shape invariants (every dimension a
/// positive `usize`, flat element count = `product(dims)`) do not hold —
/// admissibility and the manifest layer enforce them upstream, so a breach
/// here is a projection bug and must never fabricate a wrong-shape literal
/// (SPEC_0008: fail early, no silent defaults).
pub(crate) fn start_literal(
    variable: &ManifestVariable,
) -> Result<gast::Expression, GalecTargetError> {
    let dims = &variable.common().dimensions;
    match variable {
        ManifestVariable::Real(real) => {
            nested_literal(dims, &real.start, |v| gast::Expression::Real(*v))
        }
        ManifestVariable::Integer(integer) => nested_literal(dims, &integer.start, |v| {
            gast::Expression::Integer(i64::from(*v))
        }),
        ManifestVariable::Boolean(boolean) => {
            nested_literal(dims, &boolean.start, |v| gast::Expression::Bool(*v))
        }
    }
}

fn nested_literal<T>(
    dims: &[u64],
    start: &StartValue<T>,
    to_expr: impl Fn(&T) -> gast::Expression + Copy,
) -> Result<gast::Expression, GalecTargetError> {
    match start {
        StartValue::Scalar(value) => broadcast_literal(dims, &to_expr(value)),
        StartValue::Array(values) => {
            let exprs: Vec<gast::Expression> = values.iter().map(to_expr).collect();
            nest_row_major(dims, &exprs)
        }
    }
}

/// A manifest dimension as a positive `usize`; `ET018` otherwise (validated
/// upstream — see [`start_literal`]).
fn literal_dimension(size: u64) -> Result<usize, GalecTargetError> {
    usize::try_from(size)
        .ok()
        .filter(|size| *size >= 1)
        .ok_or_else(|| GalecTargetError::LoweringInternal {
            detail: format!(
                "manifest dimension {size} survived validation as a start-nesting size"
            ),
        })
}

fn broadcast_literal(
    dims: &[u64],
    value: &gast::Expression,
) -> Result<gast::Expression, GalecTargetError> {
    let Some((first, rest)) = dims.split_first() else {
        return Ok(value.clone());
    };
    let element = broadcast_literal(rest, value)?;
    Ok(gast::Expression::Array(vec![
        element;
        literal_dimension(*first)?
    ]))
}

/// Nest a row-major flat element list per the dimension sizes, re-checking
/// the `values.len() == product(dims)` invariant the manifest layer
/// enforced (`ET018` on breach — see [`start_literal`]).
fn nest_row_major(
    dims: &[u64],
    values: &[gast::Expression],
) -> Result<gast::Expression, GalecTargetError> {
    match dims.split_first() {
        None => match values {
            [single] => Ok(single.clone()),
            _ => Err(GalecTargetError::LoweringInternal {
                detail: format!(
                    "row-major start nesting expected 1 element at scalar depth, found {}",
                    values.len()
                ),
            }),
        },
        Some((first, rest)) => {
            let rows = literal_dimension(*first)?;
            if !values.len().is_multiple_of(rows) {
                return Err(GalecTargetError::LoweringInternal {
                    detail: format!(
                        "row-major start nesting cannot split {} element(s) into {rows} row(s)",
                        values.len()
                    ),
                });
            }
            let chunk = values.len() / rows;
            (0..rows)
                .map(|row| nest_row_major(rest, &values[row * chunk..(row + 1) * chunk]))
                .collect::<Result<Vec<_>, _>>()
                .map(gast::Expression::Array)
        }
    }
}

#[cfg(test)]
mod tests {
    //! The shape invariants (dims >= 1, element count = product(dims)) are
    //! enforced upstream by admissibility and the manifest layer; these
    //! helpers must FAIL on a breach (ET018), never fabricate a wrong-shape
    //! Startup literal (SPEC_0008: no silent defaults).

    use super::{
        ScalarType, broadcast_literal, coerce_to, gast, integral_real_literal, nest_row_major,
    };
    use crate::diagnostic::GalecTargetError;
    use crate::lower::expr::Typed;

    fn reals(count: usize) -> Vec<gast::Expression> {
        (0..count)
            .map(|index| gast::Expression::Real(index as f64))
            .collect()
    }

    #[test]
    fn nest_row_major_nests_valid_shapes() {
        let nested = nest_row_major(&[2, 2], &reals(4)).expect("valid shape");
        let gast::Expression::Array(rows) = nested else {
            panic!("expected outer array");
        };
        assert_eq!(rows.len(), 2);
        assert!(matches!(&rows[0], gast::Expression::Array(row) if row.len() == 2));
    }

    #[test]
    fn nest_row_major_fails_early_on_broken_invariants() {
        // Element count not divisible by the leading dimension.
        assert!(nest_row_major(&[2], &reals(3)).is_err());
        // Leftover elements at scalar depth.
        assert!(nest_row_major(&[2], &reals(4)).is_err());
        // Empty flat list where the dims require elements.
        assert!(nest_row_major(&[2], &[]).is_err());
        // Scalar depth with no element at all.
        assert!(nest_row_major(&[], &[]).is_err());
        // Zero dimension must never clamp to 1.
        assert!(nest_row_major(&[0], &reals(1)).is_err());
    }

    #[test]
    fn broadcast_literal_fails_early_on_non_positive_dimension() {
        let value = gast::Expression::Real(1.0);
        assert!(broadcast_literal(&[0], &value).is_err());
        let broadcast = broadcast_literal(&[3], &value).expect("valid dims");
        assert!(matches!(broadcast, gast::Expression::Array(items) if items.len() == 3));
    }

    /// Exact-or-`None` contract of the Real→Integer literal retyping
    /// (SPEC_0008: values are never rounded). 2^53 is the largest f64
    /// magnitude below which every integer is exactly representable.
    #[test]
    fn integral_real_literal_accepts_exactly_representable_integers_only() {
        const TWO_POW_53: i64 = 9_007_199_254_740_992;
        let real = |value: f64| gast::Expression::Real(value);

        assert_eq!(integral_real_literal(&real(0.0)), Some(0));
        assert_eq!(integral_real_literal(&real(-3.0)), Some(-3));
        assert_eq!(
            integral_real_literal(&real(TWO_POW_53 as f64)),
            Some(TWO_POW_53),
            "the 2^53 boundary itself is exactly representable"
        );
        assert_eq!(
            integral_real_literal(&real(-(TWO_POW_53 as f64))),
            Some(-TWO_POW_53)
        );

        // Non-integral, non-finite, or beyond exact representability: None.
        assert_eq!(integral_real_literal(&real(0.5)), None);
        assert_eq!(integral_real_literal(&real(f64::NAN)), None);
        assert_eq!(integral_real_literal(&real(f64::INFINITY)), None);
        assert_eq!(integral_real_literal(&real(f64::NEG_INFINITY)), None);
        // 2^53 + 2 is representable but past the exactness bound.
        assert_eq!(integral_real_literal(&real((TWO_POW_53 + 2) as f64)), None);
        // Only Real literals qualify — never composite expressions.
        assert_eq!(integral_real_literal(&gast::Expression::Integer(1)), None);
    }

    /// The guard branch behind the literal retyping: a Real value that is
    /// NOT an exactly-integral literal keeps the stable
    /// `unsupported-feature` diagnostic instead of being converted.
    #[test]
    fn coerce_to_integer_rejects_non_integral_real_with_diagnostic() {
        let typed = |expr| Typed {
            expr,
            ty: ScalarType::Real,
        };

        let error = coerce_to(typed(gast::Expression::Real(0.5)), ScalarType::Integer, "n")
            .expect_err("non-integral Real must not convert");
        assert!(
            matches!(
                &error,
                GalecTargetError::UnsupportedFeature { feature, .. }
                    if feature == "real-to-integer-conversion"
            ),
            "expected the real-to-integer-conversion diagnostic, got: {error}"
        );

        // A non-literal Real expression is never folded, even if it would
        // evaluate to an integer at runtime.
        let reference = super::state_ref(gast::Name::ident("x"), Vec::new());
        assert!(matches!(
            coerce_to(typed(reference), ScalarType::Integer, "n"),
            Err(GalecTargetError::UnsupportedFeature { .. })
        ));

        // The exact-integral literal path still converts.
        let converted = coerce_to(typed(gast::Expression::Real(4.0)), ScalarType::Integer, "n")
            .expect("exactly-integral Real literal retypes");
        assert_eq!(converted, gast::Expression::Integer(4));
    }
}
