use crate::s1_parser::ast as parse_ast;
use crate::s2_analyzer::ast;

pub fn flatten(def: &parse_ast::StoredDefinition) -> Result<ast::Def, Box<dyn std::error::Error>> {
    let mut flat_def = ast::Def {
        model_md5: def.model_md5.clone(),
        rumoca_git_hash: def.rumoca_git_hash.clone(),
        rumoca_version: env!("CARGO_PKG_VERSION").to_string(),
        template_md5: "".to_string(),
        ..Default::default()
    };

    for class in &def.classes {
        let mut fclass = ast::Class {
            name: class.name.clone(),
            class_type: class.class_type.clone(),
            description: class.description.clone(),
            ..Default::default()
        };

        for composition in &class.compositions {
            flatten_composition(composition, &mut fclass)
        }

        flat_def
            .classes
            .insert(fclass.name.to_string(), fclass.clone());
    }

    Ok(flat_def)
}

pub fn flatten_composition(composition: &parse_ast::Composition, class: &mut ast::Class) {
    match composition {
        parse_ast::Composition::ElementList {
            visibility: _,
            elements,
        } => {
            for comp in elements {
                flatten_component(comp, class);
            }
        }
        parse_ast::Composition::EquationSection {
            initial: _,
            equations,
        } => {
            for eq in equations {
                flatten_equation(eq, class);
            }
        }
        parse_ast::Composition::AlgorithmSection {
            initial: _,
            statements,
        } => {
            for stmt in statements {
                flatten_statement(stmt, class);
            }
        }
    }
}

pub fn flatten_component(comp: &parse_ast::ComponentDeclaration, class: &mut ast::Class) {
    let flat_comp = ast::Component {
        name: comp.name.clone(),
        start: comp.modification.expression.clone(),
        array_subscripts: comp.array_subscripts.clone(),
    };

    class
        .components
        .insert(flat_comp.name.clone(), flat_comp.clone());

    match comp.variability {
        parse_ast::Variability::Constant => {
            class.c.insert(flat_comp.name.to_string());
        }
        parse_ast::Variability::Continuous => match comp.causality {
            parse_ast::Causality::Input => {
                class.u.insert(flat_comp.name.to_string());
            }
            parse_ast::Causality::Output => {
                class.y.insert(flat_comp.name.to_string());
            }
            parse_ast::Causality::None => {
                class.w.insert(flat_comp.name.to_string());
            }
        },
        parse_ast::Variability::Discrete => {
            class.z.insert(flat_comp.name.to_string());
        }
        parse_ast::Variability::Parameter => {
            class.p.insert(flat_comp.name.to_string());
        }
    }
}

pub fn flatten_equation(eq: &parse_ast::Equation, class: &mut ast::Class) {
    // find all states in the class by searching
    // for component references that are taken the derivative of
    match eq {
        parse_ast::Equation::Der { comp, rhs } => {
            if class.w.contains(&comp.name) {
                class.x.insert(class.w.remove_full(&comp.name).unwrap().1);
            } else if class.y.contains(&comp.name) {
                class.x.insert(comp.name.clone());
            } else {
                panic!("derivative state not declared {:?}", comp.name);
            }
            class.ode.insert(comp.name.clone(), rhs.clone());
        }
        parse_ast::Equation::Simple { lhs: _, rhs: _ } => {
            class.algebraic.push(eq.clone());
        }
        parse_ast::Equation::If {
            if_cond: _,
            if_eqs: _,
            else_if_blocks: _,
            else_eqs: _,
        } => {
            class.algebraic.push(eq.clone());
        }
    }
}

pub fn flatten_statement(_stmt: &parse_ast::Statement, _class: &mut ast::Class) {}
