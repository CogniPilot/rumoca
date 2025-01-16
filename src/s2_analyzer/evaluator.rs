// use crate::s2_analyzer::ast;
// use ndarray::{ArrayBase, ArrayD, IxDyn, OwnedRepr};
// use rumoca_parser::{Walker, Visitor};
// use rumoca_parser::s1_parser::ast::{
//     self as parse_ast, Causality, Element, Expression, ModExpr, Modification, Statement, Subscript,
//     Variability, BinaryOp, UnaryOp
// };
// use std::collections::HashMap;
// use std::error::Error;

// pub fn evaluate(
//     class: &ast::Class,
//     expr: &Expression,
// ) -> Result<ArrayBase<OwnedRepr<f64>, IxDyn>, Box<dyn Error>> {
//     match expr {
//         Expression::Binary { op, lhs, rhs } => {
//             let lval = evaluate(class, lhs)?;
//             let rval = evaluate(class, rhs)?;
//             Ok(match op {
//                 BinaryOp::Add => lval + rval,
//                 BinaryOp::Sub => lval - rval,
//                 BinaryOp::Mul => lval * rval,
//                 BinaryOp::Div => lval + rval,
//                 BinaryOp::ElemAdd => todo!(),
//                 BinaryOp::ElemSub => todo!(),
//                 BinaryOp::ElemMul => todo!(),
//                 BinaryOp::ElemDiv => todo!(),
//                 BinaryOp::Exp => {
//                     let base = &lval;
//                     let exp = &rval;
//                     if base.shape() != [1] {
//                         panic!("exp called with non-scalar base")
//                     }
//                     if exp.shape() != [1] {
//                         panic!("exp called with non-scalar exponent")
//                     }
//                     let shape = IxDyn(&[1]);
//                     let values = vec![base[0].powf(exp[0])];
//                     ArrayD::from_shape_vec(shape, values).unwrap()
//                 }
//                 BinaryOp::ElemExp => todo!(),
//                 BinaryOp::Or => lval || rval,
//                 BinaryOp::And => lval && rval,
//                 BinaryOp::LessThan => lval < rval,
//                 BinaryOp::LessThanOrEqual => lval <= rval,
//                 BinaryOp::GreaterThan => lval > rval,
//                 BinaryOp::GreaterThanOrEqual => lval >= rval,
//                 BinaryOp::Equal => lval == rval,
//                 BinaryOp::NotEqual => lval != rval,
//                 BinaryOp::Range => todo!(),
//             })
//         }
//         Expression::Unary { op, rhs } => {
//             let rval = evaluate(class, rhs)?;
//             Ok(match op {
//                 UnaryOp::Negative => -rval,
//                 UnaryOp::Paren => rval,
//                 UnaryOp::Not => !rval,
//             })
//         }
//         Expression::UnsignedReal(v) => {
//             let shape = IxDyn(&[1]);
//             let values = vec![*v];
//             Ok(ArrayD::from_shape_vec(shape, values).unwrap())
//         }
//         Expression::UnsignedInteger(v) => {
//             let shape = IxDyn(&[1]);
//             let values = vec![*v as f64];
//             Ok(ArrayD::from_shape_vec(shape, values).unwrap())
//         }
//         Expression::Ref { comp } => match &class.components[&compref_to_string(comp)].start {
//             Some(m) => match m {
//                 Modification::Expression { expr } => match expr {
//                     ModExpr::Expression { expr } => Ok(evaluate(class, expr)?),
//                     ModExpr::Break => Err("Break modification flattening not yet implemented"
//                         .to_string()
//                         .into()),
//                 },
//                 Modification::Class { .. } => {
//                     Err("Class modification flattening not yet implemented"
//                         .to_string()
//                         .into())
//                 }
//             },
//             None => {
//                 panic!("no start value defined for {:?}", comp);
//             }
//         },
//         Expression::ArrayArguments { args } => {
//             let shape = IxDyn(&[args.len()]);
//             let mut values = Vec::new();
//             for arg in args {
//                 let arg_val = evaluate(class, arg)?;
//                 if arg_val.shape() != [1] {
//                     panic!("array arguments called with non-scalar argument")
//                 }
//                 values.push(arg_val[0])
//             }
//             Ok(ArrayD::from_shape_vec(shape, values).unwrap())
//         }
//         Expression::Negative { rhs } => Ok(-evaluate(class, rhs)?),
//         _ => {
//             todo!("{:?}", expr)
//         }
//     }
// }

//     for class in &def.classes {
//         flatten_class(class, &mut flat_def);
//     }

//     let mut start_vals = HashMap::new();
//     for (_, class) in &flat_def.classes {
//         evaluate_expressions(class, &mut start_vals);
//     }

//     for (_, class) in &mut flat_def.classes {
//         set_start_expressions(class, &start_vals);
//     }

//     Ok(flat_def)
// }

// pub fn evaluate_expressions(
//     class: &ast::Class,
//     start_vals: &mut HashMap<String, ArrayBase<OwnedRepr<f64>, IxDyn>>,
// ) {
//     for (name, comp) in &class.components {
//         if let Some(m) = &comp.start {
//             match m {
//                 Modification::Expression { expr } => match expr {
//                     ModExpr::Expression { expr: e } => {
//                         start_vals.insert(name.clone(), evaluate(class, e).unwrap());
//                     }
//                     ModExpr::Break => {
//                         red_eprintln!("Break modification flattening not yet implemented");
//                     }
//                 },
//                 Modification::Class { .. } => {
//                     red_eprintln!("Class modification flattening not yet implemented");
//                 }
//             }
//         }
//     }
// }

// pub fn set_start_expressions(
//     class: &mut ast::Class,
//     start_vals: &HashMap<String, ArrayBase<OwnedRepr<f64>, IxDyn>>,
// ) {
//     for (name, comp) in &mut class.components {
//         if start_vals.contains_key(name) {
//             comp.start_value = start_vals[name].clone();
//         }
//     }
// }

// pub fn flatten_class(class: &parse_ast::ClassDefinition, def: &mut ast::Def) {
//     let mut fclass = ast::Class {
//         class_type: class.class_prefixes.class_type.clone(),
//         ..Default::default()
//     };

//     match &class.class_specifier {
//         parse_ast::ClassSpecifier::Long {
//             name,
//             description,
//             composition,
//             ..
//         } => {
//             fclass.name = name.clone();
//             fclass.description = description.clone();
//             for composition_part in composition.iter() {
//                 flatten_composition_part(composition_part, &mut fclass)
//             }
//         }
//         parse_ast::ClassSpecifier::Extends { .. } => {
//             red_eprintln!("Class extends flattening not yet implemented");
//         }
//     }

//     def.classes.insert(fclass.name.to_string(), fclass.clone());
// }
// pub fn flatten_composition_part(composition: &parse_ast::CompositionPart, class: &mut ast::Class) {
//     match composition {
//         parse_ast::CompositionPart::ElementList { elements, .. } => {
//             for elem in elements {
//                 flatten_element(elem, class);
//             }
//         }
//         parse_ast::CompositionPart::EquationSection { equations, .. } => {
//             for eq in equations {
//                 flatten_equation(eq, class);
//             }
//         }
//         parse_ast::CompositionPart::AlgorithmSection { statements, .. } => {
//             for stmt in statements {
//                 flatten_statement(stmt, class);
//             }
//         }
//     }
// }

// pub fn flatten_element(elem: &Element, class: &mut ast::Class) {
//     match elem {
//         Element::ComponentClause { flags: _, clause } => {
//             for comp in clause.components.iter() {
//                 // determine array subscripts
//                 let comp_sub = match &comp.declaration.array_subscripts {
//                     Some(sub) => {
//                         // already has array subscripts
//                         sub.clone()
//                     }
//                     None => match &clause.array_subscripts {
//                         Some(clause_sub) => {
//                             // take array subscripts from clause
//                             clause_sub.clone()
//                         }
//                         None => {
//                             // scalar, no subscripts
//                             Vec::<Subscript>::new()
//                         }
//                     },
//                 };

//                 let flat_comp = ast::Component {
//                     name: comp.declaration.name.clone(),
//                     start: comp.declaration.modification.clone(),
//                     start_value: ArrayD::zeros(vec![1]),
//                     array_subscripts: comp_sub,
//                 };

//                 class
//                     .components
//                     .insert(flat_comp.name.clone(), flat_comp.clone());

//                 match clause.type_prefix.variability {
//                     Variability::Constant => {
//                         class.c.insert(flat_comp.name.to_string());
//                     }
//                     Variability::Continuous => match clause.type_prefix.causality {
//                         Causality::Input => {
//                             class.u.insert(flat_comp.name.to_string());
//                         }
//                         Causality::Output => {
//                             class.y.insert(flat_comp.name.to_string());
//                         }
//                         Causality::None => {
//                             class.w.insert(flat_comp.name.to_string());
//                         }
//                     },
//                     Variability::Discrete => {
//                         class.z.insert(flat_comp.name.to_string());
//                     }
//                     Variability::Parameter => {
//                         class.p.insert(flat_comp.name.to_string());
//                     }
//                 }
//             }
//         }
//         Element::ClassDefinition { .. } => {
//             red_eprintln!("Class definition element not yet implemented");
//         }
//         Element::ImportClause { .. } => {
//             red_eprintln!("Import element not yet implemented");
//         }
//         Element::ExtendsClause { .. } => {
//             red_eprintln!("Extends clause element not yet implemented");
//         }
//     }
// }

// pub fn compref_to_string(comp: &parse_ast::ComponentReference) -> String {
//     let mut s: String = "".to_string();
//     for (index, part) in comp.parts.iter().enumerate() {
//         if index != 0 || comp.local {
//             s += ".";
//         }
//         s += &part.name;
//     }
//     s
// }

// pub fn flatten_equation(eq: &parse_ast::Equation, class: &mut ast::Class) {
//     match eq {
//         parse_ast::Equation::Simple { lhs, rhs, .. } => {
//             class.algebraic.push(eq.clone());
//             flatten_expression(lhs, class);
//             flatten_expression(rhs, class);
//         }
//         parse_ast::Equation::If { .. } => {
//             red_eprintln!("If equation flattening not yet implemented");
//         }
//         parse_ast::Equation::For { .. } => {
//             red_eprintln!("For equation flattening not yet implemented");
//         }
//         parse_ast::Equation::Connect { .. } => {
//             red_eprintln!("Connect equation flattening not yet implemented");
//         }
//     }
// }

// pub fn flatten_expression(expr: &Expression, class: &mut ast::Class) {
//     match expr {
//         Expression::Der { args } => {
//             for arg in args {
//                 if let Expression::Ref { comp } = arg {
//                     let comp_key = compref_to_string(comp);
//                     if class.w.contains(&comp_key) {
//                         class.x.insert(class.w.remove_full(&comp_key).unwrap().1);
//                     } else if class.y.contains(&comp_key) {
//                         class.x.insert(comp_key.clone());
//                     } else {
//                         red_eprintln!("derivative state not declared {:?}", comp_key);
//                     }
//                     // TODO, need to solve for derivatives from equations
//                     // setting derivatives to zero for now
//                     class
//                         .ode
//                         .insert(comp_key.clone(), Expression::UnsignedInteger(0));
//                 }
//             }
//         }
//         Expression::UnsignedInteger(_) => todo!(),
//         Expression::UnsignedReal(_) => todo!(),
//         Expression::Boolean(_) => todo!(),
//         Expression::Ref { comp } => todo!(),
//         Expression::Unary { op, rhs } => todo!(),
//         Expression::Binary { op, lhs, rhs } => todo!(),
//         Expression::If { if_blocks, else_expr } => todo!(),
//         Expression::ArrayArguments { args } => todo!(),
//         Expression::FunctionCall { comp, args } => todo!(),
//     }
// }

// pub fn flatten_statement(stmt: &parse_ast::Statement, class: &mut ast::Class) {
//     match stmt {
//         Statement::Assignment { .. } => {
//             class.algorithm.push(stmt.clone());
//         }
//         Statement::If { .. } => todo!(),
//         Statement::For { indices, stmts, description } => todo!(),
//         Statement::While { cond, stmts, description } => todo!(),
//         Statement::Break { description } => todo!(),
//         Statement::Return { description } => todo!(),
//     }
// }
