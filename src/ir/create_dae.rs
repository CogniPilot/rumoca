//! This module provides functionality for working with the `Dae` structure,
//! which is part of the Abstract Syntax Tree (AST) representation in the
//! Differential-Algebraic Equation (DAE) domain. It is used to model and
//! manipulate DAE-related constructs within the application.
use crate::dae::ast::Dae;
use crate::ir::ast::{
    Causality, ClassDefinition, Component, Equation, Expression, Name, Statement, Token,
    Variability,
};
use crate::ir::constants::BUILTIN_REINIT;
use crate::ir::error::IrError;
use crate::ir::visitor::Visitable;
use crate::ir::visitors::condition_finder::ConditionFinder;
use crate::ir::visitors::state_finder::StateFinder;
use git_version::git_version;

use anyhow::Result;

const GIT_VERSION: &str = git_version!(fallback = "unknown");

/// Creates a DAE (Differential-Algebraic Equation) representation from a flattened class definition.
///
/// This function transforms a flattened Modelica class into a structured DAE representation suitable
/// for numerical solving. It performs the following transformations:
///
/// - Identifies state variables (those appearing in `der()` calls) and their derivatives
/// - Classifies components by variability (parameters, constants, discrete, continuous)
/// - Classifies components by causality (inputs, outputs, algebraic variables)
/// - Finds and extracts conditions from when/if clauses
/// - Processes `reinit` statements in when clauses
/// - Creates previous value variables for discrete and state variables
/// - Collects all equations into the appropriate DAE categories
///
/// # Arguments
///
/// * `fclass` - A mutable reference to a flattened class definition (output from the `flatten` function)
///
/// # Returns
///
/// * `Result<Dae>` - The DAE representation on success, or an error if:
///   - Connection equations are not yet expanded (not implemented)
///   - Invalid reinit function calls are encountered
///
/// # Errors
///
/// Returns an error if connection equations are encountered (they should be expanded during
/// flattening but this feature is not yet implemented).
pub fn create_dae(fclass: &mut ClassDefinition) -> Result<Dae> {
    // create default Dae struct
    let mut dae = Dae {
        rumoca_version: env!("CARGO_PKG_VERSION").to_string(),
        git_version: GIT_VERSION.to_string(),
        t: Component {
            name: "t".to_string(),
            type_name: Name {
                name: vec![Token {
                    text: "Real".to_string(),
                    ..Default::default()
                }],
            },
            ..Default::default()
        },
        ..Default::default()
    };

    // run statefinder to find states and replace
    // derivative references
    let mut state_finder = StateFinder::default();
    fclass.accept(&mut state_finder);

    // find conditions
    let mut condition_finder = ConditionFinder::default();
    fclass.accept(&mut condition_finder);

    // handle components
    for (_, comp) in &fclass.components {
        match comp.variability {
            Variability::Parameter(..) => {
                dae.p.insert(comp.name.clone(), comp.clone());
            }
            Variability::Constant(..) => {
                dae.cp.insert(comp.name.clone(), comp.clone());
            }
            Variability::Discrete(..) => {
                dae.m.insert(comp.name.clone(), comp.clone());
            }
            Variability::Empty => {
                if state_finder.states.contains(&comp.name) {
                    // Add state variable only - derivatives remain as der() calls in equations
                    dae.x.insert(comp.name.clone(), comp.clone());
                } else {
                    match comp.causality {
                        Causality::Input(..) => {
                            dae.u.insert(comp.name.clone(), comp.clone());
                        }
                        Causality::Output(..) => {
                            dae.y.insert(comp.name.clone(), comp.clone());
                        }
                        Causality::Empty => {
                            dae.y.insert(comp.name.clone(), comp.clone());
                        }
                    }
                }
            }
        }
    }

    // handle conditions and relations
    dae.c = condition_finder.conditions.clone();
    dae.fc = condition_finder.expressions.clone();

    // Apply BLT transformation to reorder and normalize equations
    let transformed_equations = crate::ir::blt::blt_transform(fclass.equations.clone());

    // handle equations
    for eq in &transformed_equations {
        match &eq {
            Equation::Simple { .. } => {
                dae.fx.push(eq.clone());
            }
            Equation::If { .. } => {
                dae.fx.push(eq.clone());
            }
            Equation::For { .. } => {
                // For equations are passed through directly - they will be
                // either expanded by the backend or serialized as-is
                dae.fx.push(eq.clone());
            }
            Equation::Connect { .. } => {
                return Err(IrError::UnexpandedConnectionEquation.into());
            }
            Equation::When(blocks) => {
                for block in blocks {
                    for eq in &block.eqs {
                        match eq {
                            Equation::FunctionCall { comp, args } => {
                                let name = comp.to_string();
                                if name == BUILTIN_REINIT {
                                    let cond_name = match &block.cond {
                                        Expression::ComponentReference(cref) => cref.to_string(),
                                        other => {
                                            let loc = other
                                                .get_location()
                                                .map(|l| {
                                                    format!(
                                                        " at {}:{}:{}",
                                                        l.file_name, l.start_line, l.start_column
                                                    )
                                                })
                                                .unwrap_or_default();
                                            anyhow::bail!(
                                                "Unsupported condition type in 'when' block{}. \
                                                 Expected a component reference.",
                                                loc
                                            )
                                        }
                                    };
                                    if args.len() != 2 {
                                        return Err(
                                            IrError::InvalidReinitArgCount(args.len()).into()
                                        );
                                    }
                                    match &args[0] {
                                        Expression::ComponentReference(cref) => {
                                            dae.fr.insert(
                                                cond_name,
                                                Statement::Assignment {
                                                    comp: cref.clone(),
                                                    value: args[1].clone(),
                                                },
                                            );
                                        }
                                        _ => {
                                            return Err(IrError::InvalidReinitFirstArg(format!(
                                                "{:?}",
                                                args[0]
                                            ))
                                            .into());
                                        }
                                    }
                                }
                            }
                            other => {
                                let loc = other
                                    .get_location()
                                    .map(|l| {
                                        format!(
                                            " at {}:{}:{}",
                                            l.file_name, l.start_line, l.start_column
                                        )
                                    })
                                    .unwrap_or_default();
                                anyhow::bail!(
                                    "Unsupported equation type in 'when' block{}. \
                                     Only 'reinit' function calls are currently supported.",
                                    loc
                                )
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    // Handle initial equations
    for eq in &fclass.initial_equations {
        match eq {
            Equation::Simple { .. } | Equation::For { .. } | Equation::If { .. } => {
                dae.fx_init.push(eq.clone());
            }
            _ => {
                // Other equation types in initial section are less common
                // but we'll pass them through
                dae.fx_init.push(eq.clone());
            }
        }
    }

    Ok(dae)
}
