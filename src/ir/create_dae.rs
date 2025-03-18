use crate::dae::ast::Dae;
use crate::ir::ast::{Causality, ClassDefinition, Component, Name, Token, Variability};
use crate::ir::visitor::Visitable;
use crate::ir::visitors::state_finder::StateFinder;
use anyhow::Result;

pub fn create_dae(fclass: &mut ClassDefinition) -> Result<Dae> {
    let mut dae = Dae {
        t: Component {
            name: "time".to_string(),
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
    let mut state_finder = StateFinder::default();
    fclass.accept(&mut state_finder);

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

    for eq in &fclass.equations {
        dae.fx.push(eq.clone());
    }
    Ok(dae)
}
