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

use rumoca_ir_dae::{self as dae, Dae};
use rumoca_compile::{Session, SessionConfig};

/// Check if a Expression contains an If expression anywhere in its tree.
fn contains_if_expr(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::If { .. } => true,
        dae::Expression::Binary { lhs, rhs, .. } => contains_if_expr(lhs) || contains_if_expr(rhs),
        dae::Expression::Unary { rhs, .. } => contains_if_expr(rhs),
        _ => false,
    }
}

/// Check if a Expression contains a pre() builtin call anywhere in its tree.
fn contains_pre_expr(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::BuiltinCall {
            function: rumoca_ir_dae::BuiltinFunction::Pre,
            ..
        } => true,
        dae::Expression::Binary { lhs, rhs, .. } => {
            contains_pre_expr(lhs) || contains_pre_expr(rhs)
        }
        dae::Expression::Unary { rhs, .. } => contains_pre_expr(rhs),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches
                .iter()
                .any(|(cond, then_expr)| contains_pre_expr(cond) || contains_pre_expr(then_expr))
                || contains_pre_expr(else_branch)
        }
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            args.iter().any(contains_pre_expr)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(contains_pre_expr)
        }
        dae::Expression::Range { start, step, end } => {
            contains_pre_expr(start)
                || step.as_ref().is_some_and(|s| contains_pre_expr(s))
                || contains_pre_expr(end)
        }
        dae::Expression::Index { base, subscripts } => {
            contains_pre_expr(base)
                || subscripts.iter().any(|sub| match sub {
                    rumoca_ir_dae::Subscript::Expr(e) => contains_pre_expr(e),
                    _ => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => contains_pre_expr(base),
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            indices.iter().any(|index| contains_pre_expr(&index.range))
                || contains_pre_expr(expr)
                || filter.as_ref().is_some_and(|cond| contains_pre_expr(cond))
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
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
        states: dae.states.len(),
        algebraics: dae.algebraics.len(),
        parameters: dae.parameters.len(),
        constants: dae.constants.len(),
        discrete_reals: dae.discrete_reals.len(),
        discrete_valued: dae.discrete_valued.len(),
        inputs: dae.inputs.len(),
        outputs: dae.outputs.len(),
        f_x_count: dae.f_x.len(),
        balance: rumoca_eval_dae::analysis::balance(&dae),
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
