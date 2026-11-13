//! How a generated Co-Simulation component advances one communication step
//! (SPEC_0044 ME-LSW-001).
//!
//! The plan is constructed with the component, so every generated
//! Co-Simulation executor (the FMI 2 and 3 C profiles and fmi-ls-wasm, which
//! compiles the same C) integrates by the same rule instead of each template
//! choosing its own method, substep, and error control.
//!
//! # References
//!
//! The method is the embedded pair of J. R. Dormand and P. J. Prince, "A
//! family of embedded Runge-Kutta formulae", Journal of Computational and
//! Applied Mathematics 6(1):19-26, 1980, the tableau `rumoca-solver-rk45`
//! integrates Model Exchange with. The step-size controller is the elementary
//! one of E. Hairer, S. P. Norsett and G. Wanner, "Solving Ordinary
//! Differential Equations I: Nonstiff Problems", 2nd rev. ed., Springer 1993,
//! section II.4 (equations II.4.12 and II.4.13), with the same constants.

use serde::{Deserialize, Serialize};

/// The explicit integration method of a Co-Simulation substep.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub enum CoSimulationMethod {
    /// Dormand-Prince 5(4): the fifth-order solution advances, the embedded
    /// fourth-order solution estimates the local error, and the last stage is
    /// the next substep's first (first same as last).
    DormandPrince54,
    /// The fixed-step method of `order` (1 explicit Euler, 2 Heun, 4 classical
    /// Runge-Kutta) the fixed-step backend implements; that backend checks
    /// the order when the component is constructed.
    ExplicitRungeKutta { order: u32 },
    /// The fixed-step backend's partitioned Euler step over the compiler-issued
    /// kinematic states (order 1).
    SemiImplicitEuler,
}

/// How a communication step is divided into substeps.
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub enum CoSimulationSubstep {
    /// Adaptive substeps whose local error estimate stays within the
    /// component tolerance, the last one ending exactly at the communication
    /// point; a step the controller cannot complete within its budget is
    /// discarded and rolled back.
    ErrorControlled,
    /// Substeps of `step` seconds, the last one shortened to end exactly at
    /// the communication point; no error estimate and no rejection.
    Fixed { step: f64 },
}

/// The step-size controller of an error-controlled substep rule. A fixed
/// substep rule reads only its default tolerance.
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CoSimulationController {
    /// Relative and absolute tolerance of the local error when the importer
    /// defines none (the FMI setup tolerance replaces it otherwise); equal to
    /// the native simulation's default tolerance. With a plan's absolute
    /// tolerance, the relative one only.
    default_tolerance: f64,
    /// `safety` of `factor = safety * err^exponent`.
    safety: f64,
    /// `-1 / (p - 1)` for the fifth-order pair.
    exponent: f64,
    /// Smallest and largest factor one substep may change the step size by.
    min_factor: f64,
    max_factor: f64,
    /// Attempted substeps one communication step may take before it is
    /// discarded.
    max_substeps: u64,
}

/// The Co-Simulation step rule of one FMI component.
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CoSimulationStepPlan {
    method: CoSimulationMethod,
    substep: CoSimulationSubstep,
    controller: CoSimulationController,
    /// The absolute tolerance of the component's sessions (event location and
    /// any error control). Absent, the relative tolerance serves as both.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    absolute_tolerance: Option<f64>,
}

/// A step plan whose values cannot describe a step rule.
#[derive(Clone, Debug, PartialEq, thiserror::Error)]
pub enum CoSimulationPlanError {
    #[error("a fixed substep must be finite and positive, got {0}")]
    Step(f64),
    #[error("a {name} tolerance must be finite and positive, got {value}")]
    Tolerance { name: &'static str, value: f64 },
    #[error(
        "the step-size controller differs from the standard one; only its default tolerance \
         is a plan choice"
    )]
    Controller,
    #[error("{method:?} has no {substep:?} rule")]
    Pairing {
        method: CoSimulationMethod,
        substep: CoSimulationSubstep,
    },
}

impl CoSimulationStepPlan {
    #[must_use]
    pub const fn default_tolerance(&self) -> f64 {
        self.controller.default_tolerance
    }

    #[must_use]
    pub const fn method(&self) -> CoSimulationMethod {
        self.method
    }

    #[must_use]
    pub const fn substep(&self) -> CoSimulationSubstep {
        self.substep
    }

    #[must_use]
    pub const fn absolute_tolerance(&self) -> Option<f64> {
        self.absolute_tolerance
    }

    /// A fixed-step plan: `method` advances substeps of `step` seconds, and
    /// the component's sessions use `relative_tolerance` (when the importer
    /// defines none) and `absolute_tolerance`.
    pub fn fixed_step(
        method: CoSimulationMethod,
        step: f64,
        relative_tolerance: f64,
        absolute_tolerance: f64,
    ) -> Result<Self, CoSimulationPlanError> {
        Self {
            method,
            substep: CoSimulationSubstep::Fixed { step },
            controller: CoSimulationController {
                default_tolerance: relative_tolerance,
                ..Self::STANDARD.controller
            },
            absolute_tolerance: Some(absolute_tolerance),
        }
        .checked()
    }

    /// This plan when its method and substep rule pair up, every value is
    /// finite and positive, and its controller is the standard one but for
    /// the default tolerance: the plugins executing a plan read no other
    /// controller field, so a plan naming other coefficients or another
    /// substep budget is refused rather than carried and ignored. A replayed
    /// or attached plan is checked here too.
    pub fn checked(self) -> Result<Self, CoSimulationPlanError> {
        let standard = CoSimulationController {
            default_tolerance: self.controller.default_tolerance,
            ..Self::STANDARD.controller
        };
        if self.controller != standard {
            return Err(CoSimulationPlanError::Controller);
        }
        let fixed = matches!(
            self.method,
            CoSimulationMethod::ExplicitRungeKutta { .. } | CoSimulationMethod::SemiImplicitEuler
        );
        match self.substep {
            CoSimulationSubstep::Fixed { step } if !(step.is_finite() && step > 0.0) => {
                return Err(CoSimulationPlanError::Step(step));
            }
            CoSimulationSubstep::Fixed { .. } if fixed => {}
            CoSimulationSubstep::ErrorControlled if !fixed => {}
            substep => {
                return Err(CoSimulationPlanError::Pairing {
                    method: self.method,
                    substep,
                });
            }
        }
        let tolerances = [
            ("relative", Some(self.controller.default_tolerance)),
            ("absolute", self.absolute_tolerance),
        ];
        for (name, value) in tolerances {
            if let Some(value) = value.filter(|value| !(value.is_finite() && *value > 0.0)) {
                return Err(CoSimulationPlanError::Tolerance { name, value });
            }
        }
        Ok(self)
    }
    /// The plan every component is constructed with. The accuracy of a
    /// communication step follows the component tolerance, not the importer's
    /// communication step size, so a stiff block the importer steps coarsely
    /// takes as many substeps as its fastest mode needs.
    pub const STANDARD: Self = Self {
        method: CoSimulationMethod::DormandPrince54,
        substep: CoSimulationSubstep::ErrorControlled,
        controller: CoSimulationController {
            default_tolerance: 1.0e-6,
            safety: 0.9,
            exponent: -0.2,
            min_factor: 0.2,
            max_factor: 5.0,
            max_substeps: 100_000,
        },
        absolute_tolerance: None,
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Templates select their substep integrator and read its controller by
    /// these names, so the serialized plan is part of the rendering contract.
    #[test]
    fn the_standard_plan_serializes_the_names_templates_read() {
        let plan = serde_json::to_value(CoSimulationStepPlan::STANDARD).expect("serializes");
        assert_eq!(
            plan,
            serde_json::json!({
                "method": "DormandPrince54",
                "substep": "ErrorControlled",
                "controller": {
                    "default_tolerance": 1.0e-6,
                    "safety": 0.9,
                    "exponent": -0.2,
                    "min_factor": 0.2,
                    "max_factor": 5.0,
                    "max_substeps": 100_000,
                },
            })
        );
    }

    #[test]
    fn a_fixed_plan_pairs_a_fixed_method_with_positive_finite_values() {
        let heun = CoSimulationMethod::ExplicitRungeKutta { order: 2 };
        let plan = CoSimulationStepPlan::fixed_step(heun, 1.0e-3, 1.0e-6, 1.0e-8).unwrap();
        assert_eq!(plan.substep(), CoSimulationSubstep::Fixed { step: 1.0e-3 });
        assert_eq!(plan.default_tolerance(), 1.0e-6);
        assert_eq!(plan.absolute_tolerance(), Some(1.0e-8));
        let json = serde_json::to_value(plan).unwrap();
        assert_eq!(json["absolute_tolerance"], serde_json::json!(1.0e-8));
        let replayed: CoSimulationStepPlan = serde_json::from_value(json).unwrap();
        assert_eq!(replayed.checked().unwrap(), plan);

        for step in [0.0, -1.0e-3, f64::NAN, f64::INFINITY] {
            assert!(matches!(
                CoSimulationStepPlan::fixed_step(heun, step, 1.0e-6, 1.0e-8),
                Err(CoSimulationPlanError::Step(_))
            ));
        }
        for (relative, absolute) in [(0.0, 1.0e-8), (1.0e-6, f64::NAN)] {
            assert!(matches!(
                CoSimulationStepPlan::fixed_step(heun, 1.0e-3, relative, absolute),
                Err(CoSimulationPlanError::Tolerance { .. })
            ));
        }
        assert!(matches!(
            CoSimulationStepPlan::fixed_step(
                CoSimulationMethod::DormandPrince54,
                1.0e-3,
                1.0e-6,
                1.0e-8
            ),
            Err(CoSimulationPlanError::Pairing { .. })
        ));
        let error_controlled_heun = CoSimulationStepPlan {
            method: heun,
            ..CoSimulationStepPlan::STANDARD
        };
        assert!(matches!(
            error_controlled_heun.checked(),
            Err(CoSimulationPlanError::Pairing { .. })
        ));
        assert_eq!(
            CoSimulationStepPlan::STANDARD.checked().unwrap(),
            CoSimulationStepPlan::STANDARD
        );
    }

    /// A replayed plan whose controller coefficients or substep budget differ
    /// from the standard ones is refused: no plugin reads them.
    #[test]
    fn a_plan_with_other_controller_fields_is_refused() {
        let mut json = serde_json::to_value(CoSimulationStepPlan::STANDARD).unwrap();
        json["controller"]["max_substeps"] = serde_json::json!(1);
        let budget: CoSimulationStepPlan = serde_json::from_value(json).unwrap();
        assert!(matches!(
            budget.checked(),
            Err(CoSimulationPlanError::Controller)
        ));
        let mut json = serde_json::to_value(CoSimulationStepPlan::STANDARD).unwrap();
        json["controller"]["safety"] = serde_json::json!(0.5);
        let safety: CoSimulationStepPlan = serde_json::from_value(json).unwrap();
        assert!(matches!(
            safety.checked(),
            Err(CoSimulationPlanError::Controller)
        ));
        let mut json = serde_json::to_value(CoSimulationStepPlan::STANDARD).unwrap();
        json["controller"]["default_tolerance"] = serde_json::json!(1.0e-4);
        let tolerance: CoSimulationStepPlan = serde_json::from_value(json).unwrap();
        assert_eq!(tolerance.checked().unwrap().default_tolerance(), 1.0e-4);
    }
}
