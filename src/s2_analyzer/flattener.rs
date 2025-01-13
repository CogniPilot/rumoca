use crate::s2_analyzer::ast;
use ndarray::{ArrayBase, ArrayD, IxDyn, OwnedRepr};
use rumoca_parser::s1_parser::ast::{
    self as parse_ast, Causality, Element, Expression, ModExpr, Modification, Statement, Subscript,
    Variability,
};
use std::collections::HashMap;
use std::error::Error;

#[macro_export]
macro_rules! red_eprintln {
    ($($arg:tt)*) => {{
        eprintln!("\x1b[31m{}\x1b[0m", format!($($arg)*));
    }};
}

pub fn evaluate(
    class: &ast::Class,
    expr: &Expression,
) -> Result<ArrayBase<OwnedRepr<f64>, IxDyn>, Box<dyn Error>> {
    match expr {
        Expression::Add { lhs, rhs } => Ok(evaluate(class, lhs)? + evaluate(class, rhs)?),
        Expression::Sub { lhs, rhs } => Ok(evaluate(class, lhs)? - evaluate(class, rhs)?),
        Expression::Mul { lhs, rhs } => {
            // matrix multiplication
            let a = evaluate(class, lhs)?;
            let b = evaluate(class, rhs)?;
            let res = a * b;
            Ok(res)
        }
        Expression::Div { lhs, rhs } => Ok(evaluate(class, lhs)? / evaluate(class, rhs)?),
        Expression::ElemAdd { lhs, rhs } => Ok(evaluate(class, lhs)? + evaluate(class, rhs)?),
        Expression::ElemSub { lhs, rhs } => Ok(evaluate(class, lhs)? - evaluate(class, rhs)?),
        Expression::ElemMul { lhs, rhs } => Ok(evaluate(class, lhs)? * evaluate(class, rhs)?),
        Expression::ElemDiv { lhs, rhs } => Ok(evaluate(class, lhs)? / evaluate(class, rhs)?),
        Expression::Exp { lhs, rhs } => {
            let base = evaluate(class, lhs)?;
            let exp = evaluate(class, rhs)?;
            if base.shape() != [1] {
                panic!("exp called with non-scalar base")
            }
            if exp.shape() != [1] {
                panic!("exp called with non-scalar exponent")
            }
            let shape = IxDyn(&[1]);
            let values = vec![base[0].powf(exp[0])];
            Ok(ArrayD::from_shape_vec(shape, values).unwrap())
        }
        Expression::Parenthesis { rhs } => Ok(evaluate(class, rhs)?),
        Expression::UnsignedReal(v) => {
            let shape = IxDyn(&[1]);
            let values = vec![*v];
            Ok(ArrayD::from_shape_vec(shape, values).unwrap())
        }
        Expression::UnsignedInteger(v) => {
            let shape = IxDyn(&[1]);
            let values = vec![*v as f64];
            Ok(ArrayD::from_shape_vec(shape, values).unwrap())
        }
        Expression::Ref { comp } => match &class.components[&compref_to_string(comp)].start {
            Some(m) => match m {
                Modification::Expression { expr } => match expr {
                    ModExpr::Expression { expr } => Ok(evaluate(class, expr)?),
                    ModExpr::Break => Err("Break modification flattening not yet implemented"
                        .to_string()
                        .into()),
                },
                Modification::Class { .. } => {
                    Err("Class modification flattening not yet implemented"
                        .to_string()
                        .into())
                }
            },
            None => {
                panic!("no start value defined for {:?}", comp);
            }
        },
        Expression::ArrayArguments { args } => {
            let shape = IxDyn(&[args.len()]);
            let mut values = Vec::new();
            for arg in args {
                let arg_val = evaluate(class, arg)?;
                if arg_val.shape() != [1] {
                    panic!("array arguments called with non-scalar argument")
                }
                values.push(arg_val[0])
            }
            Ok(ArrayD::from_shape_vec(shape, values).unwrap())
        }
        Expression::Negative { rhs } => Ok(-evaluate(class, rhs)?),
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

pub fn evaluate_expressions(
    class: &ast::Class,
    start_vals: &mut HashMap<String, ArrayBase<OwnedRepr<f64>, IxDyn>>,
) {
    for (name, comp) in &class.components {
        if let Some(m) = &comp.start {
            match m {
                Modification::Expression { expr } => match expr {
                    ModExpr::Expression { expr: e } => {
                        start_vals.insert(name.clone(), evaluate(class, e).unwrap());
                    }
                    ModExpr::Break => {
                        red_eprintln!("Break modification flattening not yet implemented");
                    }
                },
                Modification::Class { .. } => {
                    red_eprintln!("Class modification flattening not yet implemented");
                }
            }
        }
    }
}

pub fn set_start_expressions(
    class: &mut ast::Class,
    start_vals: &HashMap<String, ArrayBase<OwnedRepr<f64>, IxDyn>>,
) {
    for (name, comp) in &mut class.components {
        if start_vals.contains_key(name) {
            comp.start_value = start_vals[name].clone();
        }
    }
}

pub fn flatten_class(class: &parse_ast::ClassDefinition, def: &mut ast::Def) {
    let mut fclass = ast::Class {
        class_type: class.class_prefixes.class_type.clone(),
        ..Default::default()
    };

    match &class.class_specifier {
        parse_ast::ClassSpecifier::Long {
            name,
            description,
            composition,
            ..
        } => {
            fclass.name = name.clone();
            fclass.description = description.clone();
            for composition_part in composition.iter() {
                flatten_composition_part(composition_part, &mut fclass)
            }
        }
        parse_ast::ClassSpecifier::Extends { .. } => {
            red_eprintln!("Class extends flattening not yet implemented");
        }
    }

    def.classes.insert(fclass.name.to_string(), fclass.clone());
}
pub fn flatten_composition_part(composition: &parse_ast::CompositionPart, class: &mut ast::Class) {
    match composition {
        parse_ast::CompositionPart::ElementList { elements, .. } => {
            for elem in elements {
                flatten_element(elem, class);
            }
        }
        parse_ast::CompositionPart::EquationSection { equations, .. } => {
            for eq in equations {
                flatten_equation(eq, class);
            }
        }
        parse_ast::CompositionPart::AlgorithmSection { statements, .. } => {
            for stmt in statements {
                flatten_statement(stmt, class);
            }
        }
    }
}

pub fn flatten_element(elem: &Element, class: &mut ast::Class) {
    match elem {
        Element::ComponentClause { flags: _, clause } => {
            for comp in clause.components.iter() {
                // determine array subscripts
                let comp_sub = match &comp.declaration.array_subscripts {
                    Some(sub) => {
                        // already has array subscripts
                        sub.clone()
                    }
                    None => match &clause.array_subscripts {
                        Some(clause_sub) => {
                            // take array subscripts from clause
                            clause_sub.clone()
                        }
                        None => {
                            // scalar, no subscripts
                            Vec::<Subscript>::new()
                        }
                    },
                };

                let flat_comp = ast::Component {
                    name: comp.declaration.name.clone(),
                    start: comp.declaration.modification.clone(),
                    start_value: ArrayD::zeros(vec![1]),
                    array_subscripts: comp_sub,
                };

                class
                    .components
                    .insert(flat_comp.name.clone(), flat_comp.clone());

                match clause.type_prefix.variability {
                    Variability::Constant => {
                        class.c.insert(flat_comp.name.to_string());
                    }
                    Variability::Continuous => match clause.type_prefix.causality {
                        Causality::Input => {
                            class.u.insert(flat_comp.name.to_string());
                        }
                        Causality::Output => {
                            class.y.insert(flat_comp.name.to_string());
                        }
                        Causality::None => {
                            class.w.insert(flat_comp.name.to_string());
                        }
                    },
                    Variability::Discrete => {
                        class.z.insert(flat_comp.name.to_string());
                    }
                    Variability::Parameter => {
                        class.p.insert(flat_comp.name.to_string());
                    }
                }
            }
        }
        Element::ClassDefinition { .. } => {
            red_eprintln!("Class definition element not yet implemented");
        }
        Element::ImportClause { .. } => {
            red_eprintln!("Import element not yet implemented");
        }
        Element::ExtendsClause { .. } => {
            red_eprintln!("Extends clause element not yet implemented");
        }
    }
}

pub fn compref_to_string(comp: &parse_ast::ComponentReference) -> String {
    let mut s: String = "".to_string();
    for (index, part) in comp.parts.iter().enumerate() {
        if index != 0 || comp.local {
            s += ".";
        }
        s += &part.name;
    }
    s
}

pub fn flatten_equation(eq: &parse_ast::Equation, class: &mut ast::Class) {
    match eq {
        parse_ast::Equation::Simple { lhs, rhs, .. } => {
            class.algebraic.push(eq.clone());
            flatten_expression(lhs, class);
            flatten_expression(rhs, class);
        }
        parse_ast::Equation::If { .. } => {
            red_eprintln!("If equation flattening not yet implemented");
        }
        parse_ast::Equation::For { .. } => {
            red_eprintln!("For equation flattening not yet implemented");
        }
        parse_ast::Equation::Connect { .. } => {
            red_eprintln!("Connect equation flattening not yet implemented");
        }
    }
}

pub fn flatten_expression(expr: &Expression, class: &mut ast::Class) {
    match expr {
        Expression::Der { args } => {
            for arg in args {
                if let Expression::Ref { comp } = arg {
                    let comp_key = compref_to_string(comp);
                    if class.w.contains(&comp_key) {
                        class.x.insert(class.w.remove_full(&comp_key).unwrap().1);
                    } else if class.y.contains(&comp_key) {
                        class.x.insert(comp_key.clone());
                    } else {
                        red_eprintln!("derivative state not declared {:?}", comp_key);
                    }
                    // TODO, need to solve for derivatives from equations
                    // setting derivatives to zero for now
                    class
                        .ode
                        .insert(comp_key.clone(), Expression::UnsignedInteger(0));
                }
            }
        }
        Expression::Add { lhs, rhs } => {
            flatten_expression(lhs, class);
            flatten_expression(rhs, class);
        }
        Expression::Sub { lhs, rhs } => {
            flatten_expression(lhs, class);
            flatten_expression(rhs, class);
        }
        Expression::Mul { lhs, rhs } => {
            flatten_expression(lhs, class);
            flatten_expression(rhs, class);
        }
        Expression::Div { lhs, rhs } => {
            flatten_expression(lhs, class);
            flatten_expression(rhs, class);
        }
        _ => {}
    }
}

pub fn flatten_statement(stmt: &parse_ast::Statement, class: &mut ast::Class) {
    match &stmt {
        Statement::Assignment { .. } => {
            class.algorithm.push(stmt.clone());
        }
        Statement::If { .. } => {
            red_eprintln!("If statement flattening not yet implemented");
        }
        parse_ast::Statement::For { .. } => {
            red_eprintln!("For statement flattening not yet implemented");
        }
        parse_ast::Statement::While { .. } => {
            red_eprintln!("While statement flattening not yet implemented");
        }
        parse_ast::Statement::Break { .. } => {
            red_eprintln!("Break statement flattening not yet implemented");
        }
        parse_ast::Statement::Return { .. } => {
            red_eprintln!("Return statement flattening not yet implemented");
        }
    }
}
