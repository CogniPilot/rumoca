use crate::s1_parser::ast as parse_ast;
use crate::s2_analyzer::ast;
use std::collections::HashMap;
use std::collections::HashSet;

pub fn flatten(
    def: &parse_ast::StoredDefinition,
) -> Result<Vec<ast::Class>, Box<dyn std::error::Error>> {
    let mut class_order = Vec::new();
    let mut classes = HashMap::new();

    for class in &def.classes {
        let mut fclass: ast::Class = Default::default();
        let mut states = HashSet::new();

        fclass.name = class.name.clone();
        fclass.class_type = class.class_type.clone();
        fclass.description = class.description.clone();

        for composition in &class.compositions {
            // ================================================================
            // Element List
            // ================================================================
            if let parse_ast::Composition::ElementList {
                visibility: _,
                elements,
            } = composition
            {
                for comp in elements {
                    let flat_comp = ast::Component {
                        name: comp.name.clone(),
                        start: comp.modification.expression.clone(),
                        array_subscripts: comp.array_subscripts.clone(),
                    };
                    match comp.variability {
                        parse_ast::Variability::Constant => {
                            fclass.c.push(flat_comp);
                        }

                        parse_ast::Variability::Continuous => {
                            if states.contains(&comp.name) {
                                fclass.x.push(flat_comp);
                            } else if comp.causality == parse_ast::Causality::Input {
                                fclass.u.push(flat_comp);
                            } else if comp.causality == parse_ast::Causality::Output {
                                fclass.y.push(flat_comp);
                            } else {
                                fclass.w.push(flat_comp);
                            }
                        }
                        parse_ast::Variability::Discrete => {
                            fclass.z.push(flat_comp);
                        }
                        parse_ast::Variability::Parameter => {
                            fclass.p.push(flat_comp);
                        }
                    }
                }
            }
            // ================================================================
            // Equation Section
            // ================================================================
            else if let parse_ast::Composition::EquationSection {
                initial: _,
                equations,
            } = composition
            {
                for eq in equations {
                    // find all states in the class by searching
                    // for component references that are taken the derivative of
                    if let parse_ast::Equation::Der { comp, rhs } = eq {
                        states.insert(comp.name.clone());
                        fclass.ode.push(*rhs.clone());
                    } else {
                        panic!("unhandled equation");
                    }
                }
            // ================================================================
            // Algorithm Section
            // ================================================================
            } else if let parse_ast::Composition::AlgorithmSection {
                initial: _,
                statements,
            } = composition
            {
                for stmt in statements {
                    fclass.alg.push(stmt.clone());
                }
            } else {
                panic!("unhandled composition section");
            }
        }

        classes.insert(fclass.name.to_string(), fclass.clone());
        class_order.push(fclass.name.to_string());
    }

    Ok(class_order
        .iter()
        .map(|name| classes[name].clone())
        .collect::<Vec<ast::Class>>())
}
