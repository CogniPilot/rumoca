//! Tiered Model Tests for the Rumoca Compiler
//!
//! This module tests Modelica models in increasing complexity tiers.
//! Each tier builds on the previous, allowing us to identify issues early
//! and track feature support progress.
//!
//! ## Tier Overview
//!
//! | Tier | Name | Description |
//! |------|------|-------------|
//! | 0 | Minimal | Empty models, single variables |
//! | 1 | Basic Equations | Simple algebraic equations |
//! | 2 | ODEs | Derivatives, state variables |
//! | 3 | Parameters | Parameters, constants, modifications |
//! | 4 | Arrays | Array declarations and indexing |
//! | 5 | Conditionals | If-expressions, if-equations, when |
//! | 6 | Functions | Built-in and user-defined functions |
//! | 7 | Components | Component instantiation, connectors |
//! | 8 | Inheritance | Extends, modifications, redeclarations |
//! | 9 | Advanced | Algorithms, external functions |

use rumoca_compile::{Session, SessionConfig};
use rumoca_ir_dae::Dae;

/// Check if a Expression contains an If expression anywhere in its tree.
fn contains_if_expr(expr: &rumoca_core::Expression) -> bool {
    match expr {
        rumoca_core::Expression::If { .. } => true,
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            contains_if_expr(lhs) || contains_if_expr(rhs)
        }
        rumoca_core::Expression::Unary { rhs, .. } => contains_if_expr(rhs),
        _ => false,
    }
}

/// Check if an expression references a lowered pre() parameter.
fn contains_pre_param_ref(expr: &rumoca_core::Expression) -> bool {
    match expr {
        rumoca_core::Expression::VarRef { name, .. } => name.as_str().starts_with("__pre__."),
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            contains_pre_param_ref(lhs) || contains_pre_param_ref(rhs)
        }
        rumoca_core::Expression::Unary { rhs, .. } => contains_pre_param_ref(rhs),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(cond, then_expr)| {
                contains_pre_param_ref(cond) || contains_pre_param_ref(then_expr)
            }) || contains_pre_param_ref(else_branch)
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. } => {
            args.iter().any(contains_pre_param_ref)
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            elements.iter().any(contains_pre_param_ref)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            contains_pre_param_ref(start)
                || step.as_ref().is_some_and(|s| contains_pre_param_ref(s))
                || contains_pre_param_ref(end)
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            contains_pre_param_ref(base)
                || subscripts.iter().any(|sub| match sub {
                    rumoca_core::Subscript::Expr { expr: e, .. } => contains_pre_param_ref(e),
                    _ => false,
                })
        }
        rumoca_core::Expression::FieldAccess { base, .. } => contains_pre_param_ref(base),
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            indices
                .iter()
                .any(|index| contains_pre_param_ref(&index.range))
                || contains_pre_param_ref(expr)
                || filter
                    .as_ref()
                    .is_some_and(|cond| contains_pre_param_ref(cond))
        }
        rumoca_core::Expression::Literal { value: _, .. }
        | rumoca_core::Expression::Empty { .. } => false,
    }
}

/// Result of compiling a model with diagnostic information.
#[derive(Debug)]
struct CompileResult {
    dae: Dae,
    states: usize,
    algebraics: usize,
    parameters: usize,
    constants: usize,
    discrete_reals: usize,
    discrete_valued: usize,
    inputs: usize,
    outputs: usize,
    f_x_count: usize,
    balance: i64,
}

impl CompileResult {
    fn is_balanced(&self) -> bool {
        self.balance == 0
    }
}

/// Compile a model using Session and return detailed results.
fn compile(source: &str, model_name: &str) -> Result<CompileResult, String> {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .map_err(|e| format!("Parse/Resolve/Typecheck: {:?}", e))?;

    let result = session
        .compile_model(model_name)
        .map_err(|e| format!("Instantiate/Flatten/ToDae: {:?}", e))?;

    let dae = result.dae;
    Ok(CompileResult {
        states: dae.variables.states.len(),
        algebraics: dae.variables.algebraics.len(),
        parameters: dae.variables.parameters.len(),
        constants: dae.variables.constants.len(),
        discrete_reals: dae.variables.discrete_reals.len(),
        discrete_valued: dae.variables.discrete_valued.len(),
        inputs: dae.variables.inputs.len(),
        outputs: dae.variables.outputs.len(),
        f_x_count: dae.continuous.equations.len(),
        balance: rumoca_phase_dae::balance::balance(&dae).expect("valid DAE balance fixture"),
        dae,
    })
}

/// Assert compilation succeeds.
fn assert_compiles(source: &str, model_name: &str) -> CompileResult {
    match compile(source, model_name) {
        Ok(result) => {
            println!(
                "{}: states={}, alg={}, params={}, f_x={}, balance={}",
                model_name,
                result.states,
                result.algebraics,
                result.parameters,
                result.f_x_count,
                result.balance
            );
            result
        }
        Err(e) => panic!("Model {} failed to compile: {}", model_name, e),
    }
}

/// Assert compilation fails in the expected phase.
fn assert_fails(source: &str, model_name: &str, expected_phase: &str) {
    match compile(source, model_name) {
        Ok(_) => panic!(
            "Model {} should have failed at {} phase",
            model_name, expected_phase
        ),
        Err(e) => {
            assert!(
                e.contains(expected_phase),
                "Expected {} error, got: {}",
                expected_phase,
                e
            );
        }
    }
}

// =============================================================================
// TIER 0: Minimal Models
// =============================================================================
// Goal: Verify the basic pipeline works with trivial models.

mod tier_cases;
