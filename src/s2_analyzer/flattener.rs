use crate::s1_parser::ast as parse_ast;
use crate::s2_analyzer::ast;
use ordermap::OrderMap;

pub fn flatten(
    def: &parse_ast::StoredDefinition,
) -> Result<OrderMap<String, ast::Class>, Box<dyn std::error::Error>> {
    let mut classes = OrderMap::new();

    for class in &def.classes {
        let mut fclass: ast::Class = Default::default();

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

                    fclass
                        .components
                        .insert(flat_comp.name.clone(), flat_comp.clone());

                    match comp.variability {
                        parse_ast::Variability::Constant => {
                            fclass.c.insert(flat_comp.name.to_string());
                        }

                        parse_ast::Variability::Continuous => {
                            if comp.causality == parse_ast::Causality::Input {
                                fclass.u.insert(flat_comp.name.to_string());
                            } else if comp.causality == parse_ast::Causality::Output {
                                fclass.y.insert(flat_comp.name.to_string());
                            } else {
                                fclass.w.insert(flat_comp.name.to_string());
                            }
                        }
                        parse_ast::Variability::Discrete => {
                            fclass.z.insert(flat_comp.name.to_string());
                        }
                        parse_ast::Variability::Parameter => {
                            fclass.p.insert(flat_comp.name.to_string());
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
                        // check internal variables for state
                        if fclass.w.contains(&comp.name) {
                            fclass.x.insert(fclass.w.remove_full(&comp.name).unwrap().1);
                        } else if fclass.y.contains(&comp.name) {
                            fclass.x.insert(fclass.y.remove_full(&comp.name).unwrap().1);
                        }
                        fclass.ode.insert(comp.name.clone(), *rhs.clone());
                    } else {
                        panic!("unhandled equation {:?}", eq);
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
    }

    Ok(classes)
}

