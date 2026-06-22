use std::collections::{HashMap, HashSet};

use serde::Serialize;

use crate::ir::{
    GalecBlock, GalecDecl, GalecDeclRole, GalecExpr, GalecModel, GalecStmt, GalecType,
    GalecVariable,
};

#[derive(Debug, Clone, Serialize)]
pub struct GalecCContext {
    pub model_name: String,
    pub struct_fields: String,
    pub has_recalibrate: bool,
    pub methods: GalecCMethods,
    /// Sample period in seconds (f64), serialized as a JSON number.
    pub period_s: f64,
    /// Sample period in nanoseconds (truncated), for RTOS timer config.
    pub period_ns: u64,
}

#[derive(Debug, Clone, Serialize)]
pub struct GalecCMethods {
    pub init: GalecCMethod,
    pub recalibrate: GalecCMethod,
    pub step: GalecCMethod,
}

#[derive(Debug, Clone, Serialize)]
pub struct GalecCMethod {
    pub body: String,
}

pub fn galec_c_template_context(model: &GalecModel) -> GalecCContext {
    let model_name = sanitize_name(&model.name);
    let struct_fields = build_struct_fields(model);
    let has_recalibrate = !model.recalibrate.statements.is_empty();
    let block_vars = block_variable_names(model);
    let type_map = build_type_map(model);

    let period_s = model.sample_time.period_seconds;
    let period_ns = (period_s * 1_000_000_000.0).round() as u64;

    GalecCContext {
        model_name,
        struct_fields,
        has_recalibrate,
        methods: GalecCMethods {
            init: render_c_method(&model.init, &block_vars, &type_map, "    "),
            recalibrate: render_c_method(&model.recalibrate, &block_vars, &type_map, "    "),
            step: render_c_method_with_locals(
                &model.step,
                &model.declarations,
                &block_vars,
                &type_map,
                "    ",
            ),
        },
        period_s,
        period_ns,
    }
}

fn render_c_method(
    block: &GalecBlock,
    block_vars: &HashSet<String>,
    type_map: &HashMap<String, &GalecType>,
    indent: &str,
) -> GalecCMethod {
    let body = block
        .statements
        .iter()
        .map(|stmt| render_stmt_c(stmt, block_vars, type_map, indent))
        .collect::<Vec<_>>()
        .join("\n");
    GalecCMethod { body }
}

fn render_c_method_with_locals(
    block: &GalecBlock,
    declarations: &[GalecDecl],
    block_vars: &HashSet<String>,
    type_map: &HashMap<String, &GalecType>,
    indent: &str,
) -> GalecCMethod {
    let mut lines = Vec::new();

    for decl in declarations {
        if matches!(decl.role, GalecDeclRole::Local | GalecDeclRole::Temporary) {
            lines.push(format!(
                "{indent}{};",
                render_var_decl_c(&decl.variable)
            ));
        }
    }

    for stmt in &block.statements {
        lines.push(render_stmt_c(stmt, block_vars, type_map, indent));
    }

    GalecCMethod {
        body: lines.join("\n"),
    }
}

fn sanitize_name(name: &str) -> String {
    let mut result = String::with_capacity(name.len());
    for ch in name.chars() {
        if ch.is_alphanumeric() || ch == '_' {
            result.push(ch);
        } else if ch == ']' {
        } else {
            result.push('_');
        }
    }
    while result.ends_with('_') {
        result.pop();
    }
    if result.is_empty() {
        result.push_str("_unnamed");
    }
    result
}

fn render_type_parts(ty: &GalecType) -> (String, String) {
    match ty {
        GalecType::Real => ("real_t".to_string(), String::new()),
        GalecType::Integer => ("int".to_string(), String::new()),
        GalecType::Boolean => ("bool".to_string(), String::new()),
        GalecType::Array { element, dims } => {
            let (base, nested_dims) = render_type_parts(element);
            let dim_str = dims
                .iter()
                .map(|d| format!("[{d}]"))
                .collect::<Vec<_>>()
                .join("");
            (base, format!("{nested_dims}{dim_str}"))
        }
    }
}

fn render_var_decl_c(var: &GalecVariable) -> String {
    let (ty, dims) = render_type_parts(&var.ty);
    let name = sanitize_name(&var.name);
    format!("{ty} {name}{dims}")
}

fn render_struct_field(var: &GalecVariable) -> String {
    let (ty, dims) = render_type_parts(&var.ty);
    let name = sanitize_name(&var.name);
    format!("    {ty} {name}{dims};")
}

fn build_type_map(model: &GalecModel) -> HashMap<String, &GalecType> {
    let mut map = HashMap::new();
    for var in &model.interface.inputs {
        map.insert(var.name.clone(), &var.ty);
    }
    for var in &model.interface.outputs {
        map.insert(var.name.clone(), &var.ty);
    }
    for var in &model.interface.parameters {
        map.insert(var.name.clone(), &var.ty);
    }
    for var in &model.interface.states {
        map.insert(var.name.clone(), &var.ty);
    }
    for decl in &model.declarations {
        map.insert(decl.variable.name.clone(), &decl.variable.ty);
    }
    map
}

fn render_array_assign(
    lhs: &str,
    rhs: &GalecExpr,
    block_vars: &HashSet<String>,
    indent: &str,
) -> Vec<String> {
    match rhs {
        GalecExpr::Array(elements) => {
            let mut stmts = Vec::new();
            for (i, elem) in elements.iter().enumerate() {
                let indexed_lhs = format!("{}[{}]", lhs, i);
                match elem {
                    GalecExpr::Array(_) => {
                        stmts.extend(render_array_assign(
                            &indexed_lhs,
                            elem,
                            block_vars,
                            indent,
                        ));
                    }
                    _ => {
                        let rhs_str = render_expr_c(elem, block_vars);
                        stmts.push(format!("{indent}{indexed_lhs} = {rhs_str};"));
                    }
                }
            }
            stmts
        }
        _ => {
            let rhs_str = render_expr_c(rhs, block_vars);
            vec![format!("{indent}{lhs} = {rhs_str};")]
        }
    }
}

fn render_scalar_to_array_assign(
    lhs: &str,
    rhs: &GalecExpr,
    ty: &GalecType,
    block_vars: &HashSet<String>,
    indent: &str,
) -> Vec<String> {
    match ty {
        GalecType::Array { dims, element } if dims.len() == 1 => {
            let n = dims[0];
            let rhs_str = render_expr_c(rhs, block_vars);
            (0..n)
                .map(|i| format!("{indent}{lhs}[{i}] = {rhs_str};"))
                .collect()
        }
        GalecType::Array { dims, element } => {
            let inner = GalecType::Array {
                element: element.clone(),
                dims: dims[1..].to_vec(),
            };
            let mut stmts = Vec::new();
            for i in 0..dims[0] {
                let indexed = format!("{}[{}]", lhs, i);
                stmts.extend(render_scalar_to_array_assign(
                    &indexed, rhs, &inner, block_vars, indent,
                ));
            }
            stmts
        }
        _ => vec![],
    }
}

fn block_variable_names(model: &GalecModel) -> HashSet<String> {
    model
        .interface
        .inputs
        .iter()
        .chain(model.interface.outputs.iter())
        .chain(model.interface.parameters.iter())
        .chain(model.interface.states.iter())
        .map(|v| v.name.clone())
        .collect()
}

fn build_struct_fields(model: &GalecModel) -> String {
    let mut fields = Vec::new();

    fields.push("    real_t time;".to_string());

    for var in &model.interface.inputs {
        fields.push(render_struct_field(var));
    }
    for var in &model.interface.outputs {
        fields.push(render_struct_field(var));
    }
    for var in &model.interface.parameters {
        fields.push(render_struct_field(var));
    }
    for var in &model.interface.states {
        fields.push(render_struct_field(var));
    }

    fields.join("\n")
}

fn render_name_c(name: &str, block_vars: &HashSet<String>) -> String {
    let safe = sanitize_name(name);
    if block_vars.contains(name) {
        format!("m->{safe}")
    } else {
        safe
    }
}

fn render_expr_c(expr: &GalecExpr, block_vars: &HashSet<String>) -> String {
    match expr {
        GalecExpr::RealLiteral(v) => render_real_c(*v),
        GalecExpr::IntegerLiteral(v) => v.to_string(),
        GalecExpr::BooleanLiteral(v) => {
            if *v {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        GalecExpr::Variable(n) => render_name_c(n, block_vars),
        GalecExpr::Add(l, r) => render_binary_c("+", l, r, block_vars),
        GalecExpr::Sub(l, r) => render_binary_c("-", l, r, block_vars),
        GalecExpr::Mul(l, r) => render_binary_c("*", l, r, block_vars),
        GalecExpr::Div(l, r) => render_binary_c("/", l, r, block_vars),
        GalecExpr::Pow(l, r) => {
            format!(
                "pow({}, {})",
                render_expr_c(l, block_vars),
                render_expr_c(r, block_vars)
            )
        }
        GalecExpr::Eq(l, r) => render_binary_c("==", l, r, block_vars),
        GalecExpr::Neq(l, r) => render_binary_c("!=", l, r, block_vars),
        GalecExpr::Lt(l, r) => render_binary_c("<", l, r, block_vars),
        GalecExpr::Le(l, r) => render_binary_c("<=", l, r, block_vars),
        GalecExpr::Gt(l, r) => render_binary_c(">", l, r, block_vars),
        GalecExpr::Ge(l, r) => render_binary_c(">=", l, r, block_vars),
        GalecExpr::And(l, r) => render_binary_c("&&", l, r, block_vars),
        GalecExpr::Or(l, r) => render_binary_c("||", l, r, block_vars),
        GalecExpr::Neg(v) => format!("(-{})", render_expr_c(v, block_vars)),
        GalecExpr::Not(v) => format!("(!{})", render_expr_c(v, block_vars)),
        GalecExpr::If {
            branches,
            else_expr,
        } => render_if_expr_c(branches, else_expr, block_vars),
        GalecExpr::BuiltinCall { function, args } => {
            let rendered_args: Vec<String> = args
                .iter()
                .map(|a| render_expr_c(a, block_vars))
                .collect();
            let args = rendered_args.join(", ");
            let c_name = match function.as_str() {
                "index" if rendered_args.len() == 2 => {
                    return format!(
                        "({})[((int)({})-1)]",
                        rendered_args[0], rendered_args[1]
                    );
                }
                "min" => "fmin",
                "max" => "fmax",
                "abs" => "fabs",
                other => other,
            };
            format!("{c_name}({args})")
        }
        GalecExpr::Array(elements) => {
            let elems = elements
                .iter()
                .map(|e| render_expr_c(e, block_vars))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{elems}}}")
        }
    }
}

fn render_binary_c(
    op: &str,
    lhs: &GalecExpr,
    rhs: &GalecExpr,
    block_vars: &HashSet<String>,
) -> String {
    format!(
        "({} {op} {})",
        render_expr_c(lhs, block_vars),
        render_expr_c(rhs, block_vars)
    )
}

fn render_if_expr_c(
    branches: &[(GalecExpr, GalecExpr)],
    else_expr: &GalecExpr,
    block_vars: &HashSet<String>,
) -> String {
    if let Some((cond, val)) = branches.first() {
        let rest: Vec<(GalecExpr, GalecExpr)> = branches[1..].to_vec();
        let inner = if rest.is_empty() {
            render_expr_c(else_expr, block_vars)
        } else {
            render_if_expr_c(&rest, else_expr, block_vars)
        };
        format!(
            "({} ? {} : {})",
            render_expr_c(cond, block_vars),
            render_expr_c(val, block_vars),
            inner
        )
    } else {
        render_expr_c(else_expr, block_vars)
    }
}

fn render_stmt_c(
    stmt: &GalecStmt,
    block_vars: &HashSet<String>,
    type_map: &HashMap<String, &GalecType>,
    indent: &str,
) -> String {
    match stmt {
        GalecStmt::Assign { lhs, rhs } => {
            let lhs_name = render_name_c(lhs, block_vars);
            let is_array_lhs = type_map
                .get(lhs)
                .is_some_and(|ty| matches!(ty, GalecType::Array { .. }));
            match rhs {
                GalecExpr::Array(_) => {
                    let stmts = render_array_assign(&lhs_name, rhs, block_vars, indent);
                    stmts.join("\n")
                }
                _ if is_array_lhs => {
                    let ty = type_map.get(lhs).unwrap();
                    let stmts =
                        render_scalar_to_array_assign(&lhs_name, rhs, ty, block_vars, indent);
                    stmts.join("\n")
                }
                _ => {
                    let rhs_expr = render_expr_c(rhs, block_vars);
                    format!("{indent}{lhs_name} = {rhs_expr};")
                }
            }
        }
        GalecStmt::If {
            branches,
            else_branch,
        } => render_if_stmt_c(branches, else_branch, block_vars, type_map, indent),
    }
}

fn render_if_stmt_c(
    branches: &[(GalecExpr, Vec<GalecStmt>)],
    else_branch: &[GalecStmt],
    block_vars: &HashSet<String>,
    type_map: &HashMap<String, &GalecType>,
    indent: &str,
) -> String {
    let mut out = String::new();
    let inner_indent = format!("{indent}    ");

    for (idx, (cond, stmts)) in branches.iter().enumerate() {
        if idx == 0 {
            out.push_str(&format!(
                "{indent}if ({}) {{\n",
                render_expr_c(cond, block_vars)
            ));
        } else {
            out.push_str(&format!(
                "{indent}}} else if ({}) {{\n",
                render_expr_c(cond, block_vars)
            ));
        }
        for s in stmts {
            out.push_str(&render_stmt_c(s, block_vars, type_map, &inner_indent));
            out.push('\n');
        }
    }

    if !else_branch.is_empty() {
        out.push_str(&format!("{indent}}} else {{\n"));
        for s in else_branch {
            out.push_str(&render_stmt_c(s, block_vars, type_map, &inner_indent));
            out.push('\n');
        }
        out.push_str(&format!("{indent}}}"));
    } else {
        out.push_str(&format!("{indent}}}"));
    }

    out
}

fn render_real_c(value: f64) -> String {
    let rendered = value.to_string();
    if rendered.contains('.') || rendered.contains('e') || rendered.contains('E') {
        rendered
    } else {
        format!("{rendered}.0")
    }
}
