use crate::s1_parser::ast as parse_ast;
use crate::s2_analyzer::ast;
use ndarray::{ArrayBase, ArrayD, IxDyn, OwnedRepr};
use std::collections::HashMap;
use std::error::Error;

pub fn evaluate(
    class: &ast::Class,
    expr: &parse_ast::Expression,
) -> Result<ArrayBase<OwnedRepr<f64>, IxDyn>, Box<dyn Error>> {
    match expr {
        parse_ast::Expression::Add { lhs, rhs } => {
            Ok(evaluate(class, lhs)? + evaluate(class, rhs)?)
        }
        parse_ast::Expression::Sub { lhs, rhs } => {
            Ok(evaluate(class, lhs)? - evaluate(class, rhs)?)
        }
        parse_ast::Expression::Mul { lhs, rhs } => {
            // matrix multiplication
            let a = evaluate(class, lhs)?;
            let b = evaluate(class, rhs)?;
            let res = a * b;
            Ok(res)
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
        parse_ast::Expression::Parenthesis { rhs } => Ok(evaluate(class, rhs)?),
        parse_ast::Expression::UnsignedReal(v) => {
            let shape = IxDyn(&[1]);
            let values = vec![*v];
            Ok(ArrayD::from_shape_vec(shape, values).unwrap())
        }
        parse_ast::Expression::UnsignedInteger(v) => {
            let shape = IxDyn(&[1]);
            let values = vec![*v as f64];
            Ok(ArrayD::from_shape_vec(shape, values).unwrap())
        }
        parse_ast::Expression::Ref { comp } => {
            match &class.components[&compref_to_string(comp)].start {
                Some(m) => Ok(evaluate(class, &m.expression)?),
                None => {
                    panic!("no start value defined for {:?}", comp);
                }
            }
        }
        parse_ast::Expression::ArrayArguments { args } => {
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
        parse_ast::Expression::Negative { rhs } => Ok(-evaluate(class, rhs)?),
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
            start_vals.insert(name.clone(), evaluate(class, &m.expression).unwrap());
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
        name: class.class_specifier.name.clone(),
        class_type: class.class_prefixes.class_type.clone(),
        description: class.class_specifier.description.clone(),
        ..Default::default()
    };

    for composition_part in &class.class_specifier.composition {
        flatten_composition_part(composition_part, &mut fclass)
    }

    def.classes.insert(fclass.name.to_string(), fclass.clone());
}
pub fn flatten_composition_part(composition: &parse_ast::CompositionPart, class: &mut ast::Class) {
    match composition {
        parse_ast::CompositionPart::ElementList {
            visibility: _,
            elements,
        } => {
            for comp in elements {
                flatten_component(comp, class);
            }
        }
        parse_ast::CompositionPart::EquationSection {
            initial: _,
            equations,
        } => {
            for eq in equations {
                flatten_equation(eq, class);
            }
        }
        parse_ast::CompositionPart::AlgorithmSection {
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
        name: comp.declaration.name.clone(),
        start: comp.declaration.modification.clone(),
        start_value: ArrayD::zeros(vec![1, 1]),
        array_subscripts: comp.declaration.array_subscripts.clone(),
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

pub fn compref_to_string(comp: &parse_ast::ComponentReference) -> String {
    let mut s: String = "".to_string();
    for (index, part) in comp.parts.iter().enumerate() {
        if index != 0 || (index == 0 && comp.local) {
            s += ".";
        }
        s += &part.name;
    }
    s
}

pub fn flatten_equation(eq: &parse_ast::Equation, class: &mut ast::Class) {
    // find all states in the class by searching
    // for component references that are taken the derivative of
    match eq {
        parse_ast::Equation::Der { comp, rhs } => {
            let comp_key = compref_to_string(&comp);
            if class.w.contains(&comp_key) {
                class.x.insert(class.w.remove_full(&comp_key).unwrap().1);
            } else if class.y.contains(&comp_key) {
                class.x.insert(comp_key.clone());
            } else {
                panic!("derivative state not declared {:?}", comp_key);
            }
            class.ode.insert(comp_key.clone(), rhs.clone());
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
