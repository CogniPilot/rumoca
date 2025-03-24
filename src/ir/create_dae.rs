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

use anyhow::Result;

use super::visitors::pre_finder::PreFinder;

pub fn create_dae(fclass: &mut ClassDefinition) -> Result<Dae> {
    // create default Dae struct
    let mut dae = Dae {
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
                dae.p.push(comp.clone());
            }
            Variability::Constant(..) => {
                dae.cp.push(comp.clone());
            }
            Variability::Discrete(..) => {
                dae.m.push(comp.clone());
            }
            Variability::Empty => {
                if state_finder.states.contains(&comp.name) {
                    dae.x.push(comp.clone());
                    let mut der_comp = comp.clone();
                    der_comp.name = format!("der_{}", comp.name);
                    dae.x_dot.push(der_comp);
                } else {
                    match comp.causality {
                        Causality::Input(..) => {
                            dae.u.push(comp.clone());
                        }
                        Causality::Output(..) => {
                            dae.y.push(comp.clone());
                        }
                        Causality::Empty => {
                            dae.y.push(comp.clone());
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

    // handle equations
    for eq in &fclass.equations {
        match &eq {
            Equation::Simple { .. } => {
                dae.fx.push(eq.clone());
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
                                            dae.fr.insert(cond_name, Statement::Assignment {
                                                comp: cref.clone(),
                                                value: args[1].clone(),
                                            });
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

fn add_pre_components(source: &Vec<Component>, target: &mut Vec<Component>) {
    for comp in source {
        let mut pre_comp = comp.clone();
        pre_comp.name = format!("pre_{}", comp.name);
        target.push(pre_comp);
    }
}
