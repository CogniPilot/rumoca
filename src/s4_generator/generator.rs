use crate::s2_analyzer::ast;
use crate::s1_parser::ast as parse_ast;
use minijinja::{context, Environment, Error};
use minijinja::value::ViaDeserialize;

pub fn panic(msg: &str) {
    panic!("{:?}", msg);
}

pub fn eval(class: ViaDeserialize<ast::Class>, expr: ViaDeserialize<parse_ast::Expression>) -> Result<f64, Error> {
    evaluate(&class.0, &expr.0)
}

pub fn evaluate(class: &ast::Class, expr: &parse_ast::Expression) -> Result<f64, Error> {
    match expr
    {
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
        parse_ast::Expression::UnsignedReal(v) => {
            Ok(*v)
        }
        parse_ast::Expression::UnsignedInteger(v) => {
            Ok(*v as f64)
        }
        parse_ast::Expression::Parenthesis { rhs } => {
            Ok(evaluate(class, rhs)?)
        }
        parse_ast::Expression::Ref { comp } => {
            Ok(evaluate(class, &class.components[&comp.name].start)?)
        }
        _ => {
            panic!("unhandled expr {:?}", expr);
        }
    }
}

pub fn generate(
    def: &mut ast::Def,
    template_file: &str,
) -> Result<String, Box<dyn std::error::Error>> {
    let template_txt = std::fs::read_to_string(template_file)?;

    let digest = md5::compute(&template_txt);
    def.template_md5 = format!("{:x}", digest);
 
    let mut env = Environment::new();
    env.add_function("panic", panic);
    env.add_function("eval", eval);
    env.add_template("template", &template_txt)?;
    let tmpl = env.get_template("template")?;
    let txt = tmpl.render(context!(def => def)).unwrap();
    Ok(txt)
}
