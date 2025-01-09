use crate::s1_parser::ast as parse_ast;
use crate::s2_analyzer::ast;
use std::collections::HashMap;
use std::error::Error;

pub fn evaluate(class: &ast::Class, expr: &parse_ast::Expression) -> Result<f64, Box<dyn Error>> {
    match expr {
        parse_ast::Expression::Add { lhs, rhs } => {
            Ok(evaluate(class, lhs)? + evaluate(class, rhs)?)
        }
        parse_ast::Expression::Sub { lhs, rhs } => {
            Ok(evaluate(class, lhs)? - evaluate(class, rhs)?)
        }
        parse_ast::Expression::Mul { lhs, rhs } => {
            Ok(evaluate(class, lhs)? * evaluate(class, rhs)?)
        }
        parse_ast::Expression::Div { lhs, rhs } => {
            Ok(evaluate(class, lhs)? / evaluate(class, rhs)?)
        }
        parse_ast::Expression::ElemAdd { lhs, rhs } => {
            Ok(evaluate(class, lhs)? + evaluate(class, rhs)?)
        }
        parse_ast::Expression::ElemSub { lhs, rhs } => {
            Ok(evaluate(class, lhs)? - evaluate(class, rhs)?)
        }
        parse_ast::Expression::ElemMul { lhs, rhs } => {
            Ok(evaluate(class, lhs)? * evaluate(class, rhs)?)
        }
        parse_ast::Expression::ElemDiv { lhs, rhs } => {
            Ok(evaluate(class, lhs)? / evaluate(class, rhs)?)
        }
        parse_ast::Expression::Exp { lhs, rhs } => {
            Ok(evaluate(class, lhs)?.powf(evaluate(class, rhs)?))
        }
        parse_ast::Expression::Parenthesis { rhs } => Ok(evaluate(class, rhs)?),
        parse_ast::Expression::UnsignedReal(v) => Ok(*v),
        parse_ast::Expression::UnsignedInteger(v) => Ok(*v as f64),
        parse_ast::Expression::Ref { comp } => {
            Ok(evaluate(class, &class.components[&comp.name].start)?)
        }
        parse_ast::Expression::ArrayArguments { args } => {
            println!("calling eval on array arguments: {:?}", args);
            Ok(0.0)
        }
        _ => {
            todo!("{:?}", expr)
        }
    }
}

pub fn flatten(def: &parse_ast::StoredDefinition) -> Result<ast::Def, Box<dyn std::error::Error>> {
    let mut flat_def = ast::Def {
        model_md5: def.model_md5.clone(),
        rumoca_git_hash: def.rumoca_git_hash.clone(),
        rumoca_version: env!("CARGO_PKG_VERSION").to_string(),
        template_md5: "".to_string(),
        ..Default::default()
    };

    for class in &def.classes {
        flatten_class(class, &mut flat_def);
    }

    let mut start_vals = HashMap::new();
    for (_, class) in &flat_def.classes {
        evaluate_expressions(class, &mut start_vals);
    }

    for (_, class) in &mut flat_def.classes {
        set_start_expressions(class, &start_vals);
    }

    Ok(flat_def)
}

pub fn evaluate_expressions(class: &ast::Class, start_vals: &mut HashMap<String, f64>) {
    for (name, comp) in &class.components {
        start_vals.insert(name.clone(), evaluate(class, &comp.start).unwrap());
    }
}

pub fn set_start_expressions(class: &mut ast::Class, start_vals: &HashMap<String, f64>) {
    for (name, comp) in &mut class.components {
        comp.start_value = start_vals[name];
    }
}

pub fn flatten_class(class: &parse_ast::ClassDefinition, def: &mut ast::Def) {
    let mut fclass = ast::Class {
        name: class.name.clone(),
        class_type: class.class_type.clone(),
        description: class.description.clone(),
        ..Default::default()
    };

    for composition in &class.compositions {
        flatten_composition(composition, &mut fclass)
    }

    def.classes.insert(fclass.name.to_string(), fclass.clone());
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
        start_value: 0.0,
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
