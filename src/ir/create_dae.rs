//! This module provides functionality for working with the `Dae` structure,
//! which is part of the Abstract Syntax Tree (AST) representation in the
//! Differential-Algebraic Equation (DAE) domain. It is used to model and
//! manipulate DAE-related constructs within the application.
use crate::dae::ast::Dae;
use crate::ir::ast::{
    Causality, ClassDefinition, Component, Equation, Expression, Name, Statement, Token,
    Variability,
};
use crate::ir::visitor::Visitable;
use crate::ir::visitors::condition_finder::ConditionFinder;
use crate::ir::visitors::state_finder::StateFinder;
use git_version::git_version;

use anyhow::Result;
use indexmap::IndexMap;

use super::visitors::pre_finder::PreFinder;

const GIT_VERSION: &str = git_version!();

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
                    dae.x.insert(comp.name.clone(), comp.clone());
                    let mut der_comp = comp.clone();
                    der_comp.name = format!("der_{}", comp.name);
                    dae.x_dot.insert(der_comp.name.clone(), der_comp);
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

    // handle pre
    let mut pre_finder = PreFinder::default();
    fclass.accept(&mut pre_finder);
    add_pre_components(&dae.x, &mut dae.pre_x);
    add_pre_components(&dae.m, &mut dae.pre_m);
    add_pre_components(&dae.z, &mut dae.pre_z);

    // handle conditions and relations
    dae.c = condition_finder.conditions.clone();
    dae.fc = condition_finder.expressions.clone();

    // handle equations
    for eq in &fclass.equations {
        match &eq {
            Equation::Simple { .. } => {
                dae.fx.push(eq.clone());
            }
            Equation::If { .. } => {
                dae.fx.push(eq.clone());
            }
            Equation::Connect { .. } => {
                panic!("connection equations should already by expanded in flatten")
            }
            Equation::When(blocks) => {
                for block in blocks {
                    for eq in &block.eqs {
                        match eq {
                            Equation::FunctionCall { comp, args } => {
                                let name = comp.to_string();
                                if name == "reinit" {
                                    let cond_name = match &block.cond {
                                        Expression::ComponentReference(cref) => cref.to_string(),
                                        _ => todo!("handle other condition types"),
                                    };
                                    if args.len() != 2 {
                                        panic!("reinit function call must have two arguments");
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
                                        _ => panic!(
                                            "first argument of reinit must be a component reference"
                                        ),
                                    }
                                }
                            }
                            _ => todo!("handle other equation types"),
                        }
                    }
                }
            }
            _ => {}
        }
    }
    Ok(dae)
}

fn add_pre_components(
    source: &IndexMap<String, Component>,
    target: &mut IndexMap<String, Component>,
) {
    for comp in source.values() {
        let mut pre_comp = comp.clone();
        pre_comp.name = format!("pre_{}", comp.name);
        target.insert(pre_comp.name.clone(), pre_comp);
    }
}
