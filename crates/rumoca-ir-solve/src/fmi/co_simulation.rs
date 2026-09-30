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

use serde::Serialize;

/// The explicit integration method of a Co-Simulation substep.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum CoSimulationMethod {
    /// Dormand-Prince 5(4): the fifth-order solution advances, the embedded
    /// fourth-order solution estimates the local error, and the last stage is
    /// the next substep's first (first same as last).
    DormandPrince54,
}

/// How a communication step is divided into substeps.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum CoSimulationSubstep {
    /// Adaptive substeps whose local error estimate stays within the
    /// component tolerance, the last one ending exactly at the communication
    /// point; a step the controller cannot complete within its budget is
    /// discarded and rolled back.
    ErrorControlled,
}

/// The step-size controller of an error-controlled substep rule.
#[derive(Clone, Copy, Debug, PartialEq, Serialize)]
pub struct CoSimulationController {
    /// Relative and absolute tolerance of the local error when the importer
    /// defines none (the FMI setup tolerance replaces it otherwise); equal to
    /// the native simulation's default tolerance.
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
#[derive(Clone, Copy, Debug, PartialEq, Serialize)]
pub struct CoSimulationStepPlan {
    method: CoSimulationMethod,
    substep: CoSimulationSubstep,
    controller: CoSimulationController,
}

impl CoSimulationStepPlan {
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
}
