use core::{iter::Iterator, result::Result};
use std::collections::HashSet;

use serde::Serialize;

use crate::ir::{
    GalecAlgorithmPackage, GalecDecl, GalecExpr, GalecMethodKind, GalecModel, GalecStmt, GalecType,
    GalecVariable, GalecVariableRole,
};

#[derive(Debug, Clone, Serialize)]
pub struct GalecTemplateContext {
    pub package: GalecAlgorithmPackage,
}

pub fn template_context(model: &GalecModel) -> GalecTemplateContext {
    GalecTemplateContext {
        package: model.algorithm_package(),
    }
}

#[derive(Debug, thiserror::Error)]
pub enum GalecCodegenError {
    #[error("GALEC rendering failed: {0}")]
    Render(String),
}

pub fn render_galec(model: &GalecModel) -> Result<String, GalecCodegenError> {
    let rendered = RenderedGalecModel::from_model(model)?;
    let mut env = minijinja::Environment::new();
    env.set_undefined_behavior(minijinja::UndefinedBehavior::Strict);
    env.add_template("galec.alg.jinja", GALEC_TEMPLATE)
        .map_err(render_error)?;
    let template = env.get_template("galec.alg.jinja").map_err(render_error)?;

    template
        .render(minijinja::context! { model => rendered })
        .map_err(render_error)
}

fn render_error(error: minijinja::Error) -> GalecCodegenError {
    GalecCodegenError::Render(error.to_string())
}

const GALEC_TEMPLATE: &str = r#"block {{ model.name }}
{% for decl in model.public_declarations %}    {{ decl }}
{% endfor %}{% if model.protected_declarations %}
protected
{% for decl in model.protected_declarations %}    {{ decl }}
{% endfor %}{% endif %}
public
{% for method in model.methods %}
    method {{ method.name }}
{% if method.protected_declarations %}    protected
{% for decl in method.protected_declarations %}        {{ decl }}
{% endfor %}{% endif %}    algorithm
{% for statement in method.statements %}        {{ statement }}
{% endfor %}    end {{ method.name }};
{% if not loop.last %}
{% endif %}{% endfor %}end {{ model.name }};
"#;

#[derive(Debug, Serialize)]
struct RenderedGalecModel {
    name: String,
    public_declarations: Vec<String>,
    protected_declarations: Vec<String>,
    methods: Vec<RenderedGalecMethod>,
}

#[derive(Debug, Serialize)]
struct RenderedGalecMethod {
    name: String,
    protected_declarations: Vec<String>,
    statements: Vec<String>,
}

impl RenderedGalecModel {
    fn from_model(model: &GalecModel) -> Result<Self, GalecCodegenError> {
        let block_variables = block_variable_names(model);
        let public_declarations = public_declarations(model)?;
        let protected_declarations = protected_declarations(model)?;
        let methods = model
            .methods()
            .into_iter()
            .map(|method| {
                let protected_declarations = if method.kind == GalecMethodKind::DoStep {
                    local_declarations(&model.declarations)?
                } else {
                    Vec::new()
                };
                let statements = method
                    .block
                    .statements
                    .iter()
                    .map(|statement| {
                        render_stmt(statement, &block_variables)
                            .map(|statement| indent_multiline_statement(&statement))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(RenderedGalecMethod {
                    name: method.name,
                    protected_declarations,
                    statements,
                })
            })
            .collect::<Result<Vec<_>, GalecCodegenError>>()?;

        Ok(Self {
            name: model.name.clone(),
            public_declarations,
            protected_declarations,
            methods,
        })
    }
}

fn indent_multiline_statement(statement: &str) -> String {
    statement.replace('\n', "\n        ")
}

fn block_variable_names(model: &GalecModel) -> HashSet<String> {
    model
        .interface
        .inputs
        .iter()
        .chain(model.interface.outputs.iter())
        .chain(model.interface.parameters.iter())
        .chain(model.interface.states.iter())
        .map(|variable| variable.name.clone())
        .collect()
}

fn public_declarations(model: &GalecModel) -> Result<Vec<String>, GalecCodegenError> {
    model
        .interface
        .inputs
        .iter()
        .chain(model.interface.outputs.iter())
        .chain(
            model
                .interface
                .parameters
                .iter()
                .filter(|variable| !matches!(variable.role, GalecVariableRole::DependentParameter)),
        )
        .map(render_variable_decl)
        .collect()
}

fn protected_declarations(model: &GalecModel) -> Result<Vec<String>, GalecCodegenError> {
    model
        .interface
        .parameters
        .iter()
        .filter(|variable| matches!(variable.role, GalecVariableRole::DependentParameter))
        .chain(model.interface.states.iter())
        .map(render_variable_decl)
        .collect()
}

fn local_declarations(declarations: &[GalecDecl]) -> Result<Vec<String>, GalecCodegenError> {
    declarations
        .iter()
        .map(|decl| render_variable_decl(&decl.variable))
        .collect()
}

fn render_variable_decl(variable: &GalecVariable) -> Result<String, GalecCodegenError> {
    let role = match variable.role {
        GalecVariableRole::Input => "input ",
        GalecVariableRole::Output => "output ",
        GalecVariableRole::TunableParameter | GalecVariableRole::DependentParameter => "parameter ",
        GalecVariableRole::Constant => "constant ",
        GalecVariableRole::State | GalecVariableRole::Local => "",
    };
    let (ty, dims) = render_type(&variable.ty)?;
    let attrs = render_attributes(variable)?;
    Ok(format!("{role}{ty} {}{dims}{attrs};", variable.name))
}

fn render_attributes(variable: &GalecVariable) -> Result<String, GalecCodegenError> {
    let mut attrs = Vec::new();
    if let Some(min) = &variable.min {
        attrs.push(format!("min = {}", render_expr(min, &HashSet::new())?));
    }
    if let Some(max) = &variable.max {
        attrs.push(format!("max = {}", render_expr(max, &HashSet::new())?));
    }
    if let Some(nominal) = &variable.nominal {
        attrs.push(format!(
            "nominal = {}",
            render_expr(nominal, &HashSet::new())?
        ));
    }
    if attrs.is_empty() {
        Ok(String::new())
    } else {
        Ok(format!("({})", attrs.join(", ")))
    }
}

fn render_type(ty: &GalecType) -> Result<(String, String), GalecCodegenError> {
    match ty {
        GalecType::Real => Ok(("Real".to_string(), String::new())),
        GalecType::Integer => Ok(("Integer".to_string(), String::new())),
        GalecType::Boolean => Ok(("Boolean".to_string(), String::new())),
        GalecType::Array { element, dims } => {
            let (element, nested_dims) = render_type(element)?;
            let dims = dims
                .iter()
                .map(|dim| dim.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            Ok((element, format!("{nested_dims}[{dims}]")))
        }
    }
}

fn render_stmt(
    statement: &GalecStmt,
    block_variables: &HashSet<String>,
) -> Result<String, GalecCodegenError> {
    match statement {
        GalecStmt::Assign { lhs, rhs } => Ok(format!(
            "{} := {};",
            render_name(lhs, block_variables),
            render_expr(rhs, block_variables)?
        )),
        GalecStmt::If {
            branches,
            else_branch,
        } => render_if_stmt(branches, else_branch, block_variables),
    }
}

fn render_if_stmt(
    branches: &[(GalecExpr, Vec<GalecStmt>)],
    else_branch: &[GalecStmt],
    block_variables: &HashSet<String>,
) -> Result<String, GalecCodegenError> {
    let mut rendered = String::new();

    for (index, (condition, statements)) in branches.iter().enumerate() {
        if index == 0 {
            rendered.push_str(&format!(
                "if {} then\n",
                render_expr(condition, block_variables)?
            ));
        } else {
            rendered.push_str(&format!(
                "elseif {} then\n",
                render_expr(condition, block_variables)?
            ));
        }

        for statement in statements {
            rendered.push_str("   ");
            rendered.push_str(&render_stmt(statement, block_variables)?);
            rendered.push('\n');
        }
    }

    if !else_branch.is_empty() {
        rendered.push_str("else\n");
        for statement in else_branch {
            rendered.push_str("   ");
            rendered.push_str(&render_stmt(statement, block_variables)?);
            rendered.push('\n');
        }
    }

    rendered.push_str("end if;");
    Ok(rendered)
}

fn render_expr(
    expr: &GalecExpr,
    block_variables: &HashSet<String>,
) -> Result<String, GalecCodegenError> {
    match expr {
        GalecExpr::RealLiteral(value) => Ok(render_real(*value)),
        GalecExpr::IntegerLiteral(value) => Ok(value.to_string()),
        GalecExpr::BooleanLiteral(value) => Ok(value.to_string()),
        GalecExpr::Variable(name) => Ok(render_name(name, block_variables)),
        GalecExpr::Add(lhs, rhs) => render_binary("+", lhs, rhs, block_variables),
        GalecExpr::Sub(lhs, rhs) => render_binary("-", lhs, rhs, block_variables),
        GalecExpr::Mul(lhs, rhs) => render_binary("*", lhs, rhs, block_variables),
        GalecExpr::Div(lhs, rhs) => render_binary("/", lhs, rhs, block_variables),
        GalecExpr::Eq(lhs, rhs) => render_binary("==", lhs, rhs, block_variables),
        GalecExpr::Neq(lhs, rhs) => render_binary("<>", lhs, rhs, block_variables),
        GalecExpr::Lt(lhs, rhs) => render_binary("<", lhs, rhs, block_variables),
        GalecExpr::Le(lhs, rhs) => render_binary("<=", lhs, rhs, block_variables),
        GalecExpr::Gt(lhs, rhs) => render_binary(">", lhs, rhs, block_variables),
        GalecExpr::Ge(lhs, rhs) => render_binary(">=", lhs, rhs, block_variables),
        GalecExpr::And(lhs, rhs) => render_binary("and", lhs, rhs, block_variables),
        GalecExpr::Or(lhs, rhs) => render_binary("or", lhs, rhs, block_variables),
        GalecExpr::Neg(rhs) => Ok(format!("(-{})", render_expr(rhs, block_variables)?)),
        GalecExpr::Not(rhs) => Ok(format!("(not {})", render_expr(rhs, block_variables)?)),
        GalecExpr::If {
            branches,
            else_expr,
        } => render_if_expr(branches, else_expr, block_variables),
    }
}

fn render_binary(
    op: &str,
    lhs: &GalecExpr,
    rhs: &GalecExpr,
    block_variables: &HashSet<String>,
) -> Result<String, GalecCodegenError> {
    Ok(format!(
        "({} {op} {})",
        render_expr(lhs, block_variables)?,
        render_expr(rhs, block_variables)?,
    ))
}

fn render_if_expr(
    branches: &[(GalecExpr, GalecExpr)],
    else_expr: &GalecExpr,
    block_variables: &HashSet<String>,
) -> Result<String, GalecCodegenError> {
    let mut rendered = String::from("(if ");

    for (index, (condition, value)) in branches.iter().enumerate() {
        if index > 0 {
            rendered.push_str(" elseif ");
        }
        rendered.push_str(&render_expr(condition, block_variables)?);
        rendered.push_str(" then ");
        rendered.push_str(&render_expr(value, block_variables)?);
    }

    rendered.push_str(" else ");
    rendered.push_str(&render_expr(else_expr, block_variables)?);
    rendered.push(')');

    Ok(rendered)
}

fn render_name(name: &str, block_variables: &HashSet<String>) -> String {
    if block_variables.contains(name) {
        format!("self.{name}")
    } else {
        name.to_string()
    }
}

fn render_real(value: f64) -> String {
    let rendered = value.to_string();
    if rendered.contains('.') || rendered.contains('e') || rendered.contains('E') {
        rendered
    } else {
        format!("{rendered}.0")
    }
}
