//! Manifest `<Clock>` wiring (GAL-016, D6).
//!
//! The single admitted `ClockSchedule` must be exposed as a manifest
//! `<Clock>` referencing a Real `constant` block variable holding the
//! sample period in seconds (the unit stays implicit for slice 1 — no
//! manifest `Units` are invented). The defining constant is found
//! **structurally**: the sample-tick `f_c` right-hand side is the lowered
//! `sample(start, period)` call, and its period argument names the source
//! constant. When the period argument is not a usable named constant (a
//! literal, an expression, or a variable whose manifest start does not
//! equal the admitted period), a constant is synthesized instead — value
//! heuristics over variable names are never used.
//!
//! A non-zero clock phase cannot be represented by the Beta-1 `<Clock>`
//! element and is rejected as unsupported.

use rumoca_core::Expression;
use rumoca_efmi::Identifier;
use rumoca_efmi::algorithm_code_manifest::{
    BlockCausality, RealVariable, StartValue, Variable as ManifestVariable, VariableCommon,
};
use rumoca_ir_galec::ast::{self as gast, Name};

use crate::admissibility::AdmittedClock;
use crate::classify::{Classification, VariableClass};
use crate::diagnostic::GalecTargetError;
use crate::manifest_vars::ManifestVariables;

/// Result of clock wiring: the manifest variable the `<Clock>` references,
/// and the synthesized constant (when the DAE had no usable named one)
/// that the block/method builders must also declare and initialize.
pub(crate) struct WiredClock {
    pub variable_ref_id: Identifier,
    pub synthesized: Option<SynthesizedPeriodConstant>,
}

/// A projection-synthesized sample-period constant.
pub(crate) struct SynthesizedPeriodConstant {
    pub galec_name: Name,
    pub period_seconds: f64,
}

/// Wire the admitted clock to a Real-constant manifest variable.
pub(crate) fn wire_clock(
    clock: &AdmittedClock,
    period_expr: Option<&Expression>,
    classification: &Classification<'_>,
    manifest: &mut ManifestVariables,
) -> Result<WiredClock, GalecTargetError> {
    if clock.phase_seconds != 0.0 {
        return Err(GalecTargetError::UnsupportedFeature {
            feature: "clock-phase".to_owned(),
            detail: format!(
                "clock phase offset {} s (the eFMI Beta-1 Clock element carries \
                 only a period constant)",
                clock.phase_seconds
            ),
            span: None,
        });
    }
    if let Some(id) = named_period_constant(clock, period_expr, classification, manifest)? {
        return Ok(WiredClock {
            variable_ref_id: id,
            synthesized: None,
        });
    }
    synthesize_period_constant(clock, classification, manifest)
}

/// The manifest id of the period constant named by the sample call, when
/// it is a manifest-listed constant whose start equals the admitted period.
fn named_period_constant(
    clock: &AdmittedClock,
    period_expr: Option<&Expression>,
    classification: &Classification<'_>,
    manifest: &ManifestVariables,
) -> Result<Option<Identifier>, GalecTargetError> {
    let Some(Expression::VarRef {
        name, subscripts, ..
    }) = period_expr
    else {
        return Ok(None);
    };
    if !subscripts.is_empty() {
        return Ok(None);
    }
    let Some(classified) = classification.find(name.as_str()) else {
        return Ok(None);
    };
    if classified.class != VariableClass::Constant {
        return Ok(None);
    }
    let Some(id) = manifest.ids_by_dae_name.get(name.as_str()) else {
        return Ok(None);
    };
    let starts_match = manifest.variables.iter().any(|variable| {
        variable.common().id.as_str() == id
            && matches!(
                variable,
                ManifestVariable::Real(real)
                    if real.start == StartValue::Scalar(clock.period_seconds)
            )
    });
    if !starts_match {
        // Defensive: the schedule is authoritative; a mismatched named
        // constant is bypassed in favor of a synthesized one.
        return Ok(None);
    }
    Identifier::new(id.clone())
        .map(Some)
        .map_err(GalecTargetError::from)
}

fn synthesize_period_constant(
    clock: &AdmittedClock,
    classification: &Classification<'_>,
    manifest: &mut ManifestVariables,
) -> Result<WiredClock, GalecTargetError> {
    let name = free_constant_name(classification, manifest);
    let id = Identifier::new(format!("V{}", manifest.variables.len() + 1))?;
    manifest
        .variables
        .push(ManifestVariable::Real(RealVariable {
            common: VariableCommon {
                id: id.clone(),
                name: rumoca_efmi::NormalizedText::new(name.clone())?,
                description: Some(rumoca_efmi::NormalizedText::new(
                    "sample period of the block clock (seconds), synthesized by the \
                     Rumoca GALEC projection",
                )?),
                block_causality: BlockCausality::Constant,
                dimensions: Vec::new(),
                annotations: Vec::new(),
            },
            start: StartValue::Scalar(clock.period_seconds),
            unit_ref_id: None,
            relative_quantity: false,
            min: None,
            max: None,
            nominal: None,
        }));
    Ok(WiredClock {
        variable_ref_id: id,
        synthesized: Some(SynthesizedPeriodConstant {
            galec_name: Name::ident(name),
            period_seconds: clock.period_seconds,
        }),
    })
}

/// A block/manifest name that collides with nothing already emitted.
fn free_constant_name(classification: &Classification<'_>, manifest: &ManifestVariables) -> String {
    let taken = |candidate: &str| {
        manifest
            .variables
            .iter()
            .any(|variable| variable.common().name.as_str() == candidate)
            || classification
                .variables
                .iter()
                .any(|classified| crate::mangle::manifest_name(&classified.galec_name) == candidate)
    };
    let mut candidate = "samplePeriod".to_owned();
    let mut counter = 0usize;
    while taken(&candidate) || rumoca_ir_galec::builtins::is_reserved_name(&candidate) {
        counter += 1;
        candidate = format!("clockSamplePeriod{counter}");
    }
    candidate
}

/// The Startup assignment of a synthesized period constant.
pub(crate) fn synthesized_startup_statement(
    constant: &SynthesizedPeriodConstant,
) -> gast::Statement {
    gast::Statement::Assignment {
        target: gast::Reference::State(vec![gast::RefPart::plain(constant.galec_name.clone())]),
        value: gast::Expression::Real(constant.period_seconds),
    }
}

/// The protected declaration of a synthesized period constant.
pub(crate) fn synthesized_declaration(
    constant: &SynthesizedPeriodConstant,
) -> gast::ProtectedEntity {
    gast::ProtectedEntity {
        kind: gast::ProtectedKind::Constant,
        decl: gast::VariableDeclaration::scalar(
            gast::ScalarType::Real,
            constant.galec_name.clone(),
        ),
        start: Some(gast::Expression::Real(constant.period_seconds)),
    }
}
