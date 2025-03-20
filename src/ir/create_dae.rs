//! This module provides functionality for working with the `Dae` structure,
//! which is part of the Abstract Syntax Tree (AST) representation in the
//! Differential-Algebraic Equation (DAE) domain. It is used to model and
//! manipulate DAE-related constructs within the application.
use crate::dae::ast::Dae;
use crate::ir::ast::{Causality, ClassDefinition, Component, Name, Token, Variability, Equation};
use crate::ir::visitor::Visitable;
use crate::ir::visitors::state_finder::StateFinder;
use crate::ir::visitors::condition_finder::ConditionFinder;

use anyhow::Result;


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

    // handle equations
    for eq in &fclass.equations {
        match &eq {
            Equation::Simple {..} => {
                dae.fx.push(eq.clone());
            }
            Equation::When(blocks) => {
                for block in blocks {
                    dae.c.push(block.cond.clone());
                    dae.fc.push(block.eqs.clone());
                }
            }
            _ => {}
        }
    }
    Ok(dae)
}
