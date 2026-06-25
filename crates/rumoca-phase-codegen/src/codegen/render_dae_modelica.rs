//! DAE Modelica presentation helpers.
//!
//! These helpers are for human-readable DAE dumps. They preserve scalar DAE
//! semantics while rendering structured equation families as array equations
//! when the scalar-view rows form a compact, obvious array slice.

use super::render_expr::{
    get_field, is_variant, render_expression, render_serialized_name, render_subscript,
};
use super::render_stmt::render_equation;
use super::{ExprConfig, RenderResult, render_vec_with_capacity};
use crate::errors::render_err;
use minijinja::Value;
use std::collections::HashMap;

pub(crate) fn render_dae_equations_function(
    dae: Value,
    partition: String,
    config: Value,
) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);
    let equations = get_field(&dae, &partition)
        .map_err(|err| render_err(format!("DAE partition `{partition}` is missing: {err}")))?;
    let family_field = structured_family_field(&partition);
    let families = family_field.and_then(|field| get_field(&dae, field).ok());
    render_partition_equations(&equations, families.as_ref(), &cfg)
}

fn structured_family_field(partition: &str) -> Option<&'static str> {
    match partition {
        "f_x" => Some("structured_equations"),
        "initial_equations" => Some("initial_structured_equations"),
        _ => None,
    }
}

fn optional_render_miss<T>() -> Result<Option<T>, minijinja::Error> {
    Ok(Option::None)
}

fn render_partition_equations(
    equations: &Value,
    families: Option<&Value>,
    cfg: &ExprConfig,
) -> RenderResult {
    let equation_count = sequence_len(equations, "DAE equations")?;
    let families_by_start = structured_families_by_start(families)?;
    let mut lines = Vec::new();
    let mut row = 0usize;
    while row < equation_count {
        if let Some(family) = families_by_start.get(&row)
            && let Some(rendered) = render_structured_family(equations, family, cfg)?
        {
            lines.extend(rendered.into_iter().map(|line| format!("  {line};")));
            row = family.end_equation_index();
            continue;
        }
        lines.push(format!(
            "  {};",
            render_equation(&sequence_item(equations, row, "DAE equation")?, cfg)?
        ));
        row += 1;
    }
    Ok(lines.join("\n"))
}

#[derive(Clone, Debug)]
struct StructuredFamily {
    first_equation_index: usize,
    equation_count: usize,
    iteration_count: usize,
    domain: Domain,
    /// The family's canonical comprehension body (one residual expression per
    /// template equation, with symbolic binder indices). A regular elementwise
    /// family always carries this, so the RHS renders directly from the template —
    /// exact, not reconstructed from a corner cell. One entry per `equation_count`;
    /// indexed by equation position.
    template: Option<Vec<Value>>,
}

impl StructuredFamily {
    fn end_equation_index(&self) -> usize {
        self.first_equation_index + self.equation_count * self.iteration_count
    }
}

#[derive(Clone, Debug)]
struct Domain {
    binders: Vec<DomainBinder>,
}

impl Domain {
    fn shape(&self) -> Result<Vec<usize>, minijinja::Error> {
        let mut shape =
            render_vec_with_capacity(self.binders.len(), "structured domain shape rank")?;
        for binder in &self.binders {
            shape.push(binder.values.len());
        }
        Ok(shape)
    }

    fn tuples(&self) -> Result<Vec<Vec<i64>>, minijinja::Error> {
        let mut tuples =
            render_vec_with_capacity(self.iteration_count()?, "structured domain tuple count")?;
        let mut current =
            render_vec_with_capacity(self.binders.len(), "structured domain tuple rank")?;
        self.push_tuples(0, &mut current, &mut tuples)?;
        Ok(tuples)
    }

    fn iteration_count(&self) -> Result<usize, minijinja::Error> {
        self.binders.iter().try_fold(1usize, |count, binder| {
            count.checked_mul(binder.values.len()).ok_or_else(|| {
                render_err("structured domain iteration count overflows host index range")
            })
        })
    }

    fn push_tuples(
        &self,
        dimension: usize,
        current: &mut Vec<i64>,
        tuples: &mut Vec<Vec<i64>>,
    ) -> Result<(), minijinja::Error> {
        if dimension == self.binders.len() {
            let mut tuple =
                render_vec_with_capacity(current.len(), "structured domain tuple value count")?;
            tuple.extend(current.iter().copied());
            tuples.push(tuple);
            return Ok(());
        }
        for value in &self.binders[dimension].values {
            current.push(*value);
            self.push_tuples(dimension + 1, current, tuples)?;
            current.pop();
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
struct DomainBinder {
    /// Source binder name (`i`, `j`), used for the `for`-comprehension clauses and
    /// to match the symbolic binder references in a family's comprehension template.
    display_name: String,
    lower: i64,
    upper: i64,
    step: i64,
    values: Vec<i64>,
}

fn structured_families_by_start(
    families: Option<&Value>,
) -> Result<HashMap<usize, StructuredFamily>, minijinja::Error> {
    let Some(families) = families else {
        return Ok(HashMap::new());
    };
    let mut by_start = HashMap::new();
    for index in 0..sequence_len(families, "structured equation families")? {
        let family_value = sequence_item(families, index, "structured equation family")?;
        if let Some(family) = parse_structured_family(&family_value)? {
            by_start.insert(family.first_equation_index, family);
        }
    }
    Ok(by_start)
}

fn parse_structured_family(family: &Value) -> Result<Option<StructuredFamily>, minijinja::Error> {
    let counts = usize_sequence_field(family, "equation_counts", "structured family")?;
    let Some((&first_count, rest)) = counts.split_first() else {
        return optional_render_miss();
    };
    if first_count == 0 || rest.iter().any(|count| *count != first_count) {
        return optional_render_miss();
    }
    let domain = parse_domain(&required_field(family, "domain", "structured family")?)?;
    if domain.iteration_count()? != counts.len() {
        return optional_render_miss();
    }
    Ok(Some(StructuredFamily {
        first_equation_index: usize_field(family, "first_equation_index", "structured family")?,
        equation_count: first_count,
        iteration_count: counts.len(),
        domain,
        template: parse_family_template(family, first_count)?,
    }))
}

/// Parse a family's comprehension template body (the per-equation residual
/// expressions) from the serialized DAE. `None` when the family carries no
/// template (non-elementwise families). Errors — never silently drops —
/// when a template is present but its `body` length does not match the family's
/// `equation_count`, since that would render the wrong kernel for a position.
fn parse_family_template(
    family: &Value,
    equation_count: usize,
) -> Result<Option<Vec<Value>>, minijinja::Error> {
    let Ok(template) = get_field(family, "template") else {
        return optional_render_miss();
    };
    if template.is_none() || template.is_undefined() {
        return optional_render_miss();
    }
    let body = required_field(&template, "body", "comprehension template")?;
    let len = sequence_len(&body, "comprehension template body")?;
    if len != equation_count {
        return Err(render_err(
            "comprehension template body length does not match family equation count",
        ));
    }
    let mut residuals = render_vec_with_capacity(len, "comprehension template body count")?;
    for index in 0..len {
        residuals.push(sequence_item(
            &body,
            index,
            "comprehension template residual",
        )?);
    }
    Ok(Some(residuals))
}

fn parse_domain(domain: &Value) -> Result<Domain, minijinja::Error> {
    let binders = required_field(domain, "binders", "structured domain")?;
    let binder_count = sequence_len(&binders, "structured domain binders")?;
    let mut parsed = render_vec_with_capacity(binder_count, "structured domain binder count")?;
    for index in 0..binder_count {
        let binder = sequence_item(&binders, index, "structured domain binder")?;
        let display_name = super::value_to_string(&required_field(
            &binder,
            "display_name",
            "structured domain binder",
        )?);
        let lower = i64_field(&binder, "lower", "structured domain binder")?;
        let upper = i64_field(&binder, "upper", "structured domain binder")?;
        let step = i64_field(&binder, "step", "structured domain binder")?;
        let Some(values) = stepped_values(lower, upper, step)? else {
            return Err(render_err("structured domain binder has invalid zero step"));
        };
        parsed.push(DomainBinder {
            display_name,
            lower,
            upper,
            step,
            values,
        });
    }
    Ok(Domain { binders: parsed })
}

fn stepped_values(lower: i64, upper: i64, step: i64) -> Result<Option<Vec<i64>>, minijinja::Error> {
    if step == 0 {
        return optional_render_miss();
    }
    let count = stepped_value_count(lower, upper, step).ok_or_else(|| {
        render_err("structured domain binder value count overflows host index range")
    })?;
    let mut values = render_vec_with_capacity(count, "structured domain binder value count")?;
    let mut value = lower;
    if step > 0 {
        while value <= upper {
            values.push(value);
            value = value
                .checked_add(step)
                .ok_or_else(|| render_err("structured domain binder value overflows i64 range"))?;
        }
    } else {
        while value >= upper {
            values.push(value);
            value = value
                .checked_add(step)
                .ok_or_else(|| render_err("structured domain binder value overflows i64 range"))?;
        }
    }
    Ok(Some(values))
}

fn stepped_value_count(lower: i64, upper: i64, step: i64) -> Option<usize> {
    if step > 0 && lower > upper || step < 0 && lower < upper {
        return Some(0);
    }
    let distance = i128::from(upper) - i128::from(lower);
    let step = i128::from(step);
    let count = distance.checked_div(step)?.checked_add(1)?;
    usize::try_from(count).ok()
}

fn render_structured_family(
    equations: &Value,
    family: &StructuredFamily,
    cfg: &ExprConfig,
) -> Result<Option<Vec<String>>, minijinja::Error> {
    let tuples = family.domain.tuples()?;
    let mut lines = render_vec_with_capacity(
        family.equation_count,
        "structured DAE equation render count",
    )?;
    for equation_position in 0..family.equation_count {
        let equation_refs =
            family_equation_refs(equations, family, equation_position, tuples.len())?;
        let template_residual = family
            .template
            .as_ref()
            .map(|body| &body[equation_position]);
        let Some(line) = render_structured_equation_position(
            &equation_refs,
            &family.domain,
            &tuples,
            template_residual,
            cfg,
        )?
        else {
            return optional_render_miss();
        };
        lines.push(line);
    }
    Ok(Some(lines))
}

fn family_equation_refs(
    equations: &Value,
    family: &StructuredFamily,
    equation_position: usize,
    iteration_count: usize,
) -> Result<Vec<Value>, minijinja::Error> {
    let mut refs =
        render_vec_with_capacity(iteration_count, "structured DAE equation reference count")?;
    for iteration in 0..iteration_count {
        let index =
            family.first_equation_index + iteration * family.equation_count + equation_position;
        refs.push(sequence_item(equations, index, "structured DAE equation")?);
    }
    Ok(refs)
}

fn render_structured_equation_position(
    equations: &[Value],
    domain: &Domain,
    tuples: &[Vec<i64>],
    template_residual: Option<&Value>,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let mut lhs_exprs =
        render_vec_with_capacity(equations.len(), "structured DAE lhs expression count")?;
    let mut rhs_exprs =
        render_vec_with_capacity(equations.len(), "structured DAE rhs expression count")?;
    for equation in equations {
        let Some((lhs, rhs)) = equation_sides(equation)? else {
            return optional_render_miss();
        };
        lhs_exprs.push(lhs);
        rhs_exprs.push(rhs);
    }
    let Some(lhs) = render_access_slice(&lhs_exprs, domain, tuples, cfg)? else {
        return optional_render_miss();
    };
    // A regular elementwise family carries its comprehension template, so the RHS is
    // rendered as a `for`-comprehension directly from the captured body (exact). A
    // pure array slice on the RHS is handled first; anything else falls back to the
    // spelled-out literal.
    let template_comprehension = match template_residual {
        Some(residual) => render_template_comprehension_rhs(residual, domain, cfg)?,
        None => None,
    };
    let rhs = if let Some(slice) = render_access_slice(&rhs_exprs, domain, tuples, cfg)? {
        slice
    } else if let Some(comprehension) = template_comprehension {
        comprehension
    } else {
        render_array_literal(&rhs_exprs, &domain.shape()?, cfg)?
    };
    Ok(Some(format!("{lhs} = {rhs}")))
}

/// Render a family's RHS as a `for`-comprehension directly from its captured
/// comprehension template -- exact, not reconstructed from a materialized corner.
/// The template residual is `lhs - rhs` with symbolic binder indices; its `rhs`
/// is the per-cell kernel and the domain binders supply the `for` clauses, e.g.
/// `{(...stencil(i, j)...) for i in 2:29, j in 2:17}`. Yields `optional_render_miss`
/// only when the residual is not a `lhs - rhs` shape (the caller then falls through);
/// it never silently hides a malformed template.
fn render_template_comprehension_rhs(
    template_residual: &Value,
    domain: &Domain,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    // A template body is always a `lhs - rhs` residual (built by flatten's
    // `make_residual`), so a non-residual here is corruption, not a fallback case.
    let Some((_lhs, rhs)) = residual_sides(template_residual)? else {
        return Err(render_err(
            "comprehension template body is not a `lhs - rhs` residual",
        ));
    };
    let kernel = render_expression(&rhs, cfg)?;
    let clauses = domain
        .binders
        .iter()
        .map(|binder| format!("{} in {}", binder.display_name, render_domain_slice(binder)))
        .collect::<Vec<_>>();
    Ok(Some(format!("{{{kernel} for {}}}", clauses.join(", "))))
}

fn equation_sides(equation: &Value) -> Result<Option<(Value, Value)>, minijinja::Error> {
    if let Ok(lhs) = equation.get_attr("lhs")
        && !lhs.is_none()
        && !lhs.is_undefined()
    {
        let rhs = required_field(equation, "rhs", "explicit DAE equation")?;
        return Ok(Some((lhs, rhs)));
    }
    let rhs = required_field(equation, "rhs", "DAE equation")?;
    residual_sides(&rhs)
}

/// Split a residual expression `lhs - rhs` (a `Binary { Sub, .. }`) back into its
/// `(lhs, rhs)` sides. Yields `optional_render_miss` for any other expression shape,
/// so callers fall back to rendering the residual as-is. Shared by `equation_sides`
/// and the comprehension-template renderer.
fn residual_sides(residual: &Value) -> Result<Option<(Value, Value)>, minijinja::Error> {
    let Ok(binary) = get_field(residual, "Binary") else {
        return optional_render_miss();
    };
    if !binary_is_sub(&binary) {
        return optional_render_miss();
    }
    Ok(Some((
        required_field(&binary, "lhs", "DAE residual")?,
        required_field(&binary, "rhs", "DAE residual")?,
    )))
}

fn binary_is_sub(binary: &Value) -> bool {
    get_field(binary, "op")
        .ok()
        .is_some_and(|op| is_variant(&op, "Sub") || is_variant(&op, "SubElem"))
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Access {
    wrapper: AccessWrapper,
    name: String,
    subscripts: Vec<SubscriptToken>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum AccessWrapper {
    Plain,
    Der,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct SubscriptToken {
    rendered: String,
    index: Option<i64>,
}

fn render_access_slice(
    exprs: &[Value],
    domain: &Domain,
    tuples: &[Vec<i64>],
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let mut accesses = render_vec_with_capacity(exprs.len(), "structured access count")?;
    for expr in exprs {
        let Some(access) = expression_access(expr, cfg)? else {
            return optional_render_miss();
        };
        accesses.push(access);
    }
    let Some(first) = accesses.first() else {
        return optional_render_miss();
    };
    if accesses
        .iter()
        .any(|access| access.wrapper != first.wrapper || access.name != first.name)
    {
        return optional_render_miss();
    }
    let Some(slice) = access_slice(first, &accesses, domain, tuples)? else {
        return optional_render_miss();
    };
    let rendered = match first.wrapper {
        AccessWrapper::Plain => slice,
        AccessWrapper::Der => format!("der({slice})"),
    };
    Ok(Some(rendered))
}

fn expression_access(expr: &Value, cfg: &ExprConfig) -> Result<Option<Access>, minijinja::Error> {
    if let Ok(var_ref) = get_field(expr, "VarRef") {
        return var_ref_access(&var_ref, AccessWrapper::Plain, cfg).map(Some);
    }
    if let Ok(builtin) = get_field(expr, "BuiltinCall")
        && let Ok(function) = get_field(&builtin, "function")
        && is_variant(&function, "Der")
    {
        let args = required_field(&builtin, "args", "der call")?;
        if sequence_len(&args, "der arguments")? != 1 {
            return optional_render_miss();
        }
        let arg = sequence_item(&args, 0, "der argument")?;
        if let Ok(var_ref) = get_field(&arg, "VarRef") {
            return var_ref_access(&var_ref, AccessWrapper::Der, cfg).map(Some);
        }
    }
    optional_render_miss()
}

fn var_ref_access(
    var_ref: &Value,
    wrapper: AccessWrapper,
    cfg: &ExprConfig,
) -> Result<Access, minijinja::Error> {
    let name = render_serialized_name(&required_field(var_ref, "name", "VarRef")?);
    let subscripts = required_field(var_ref, "subscripts", "VarRef")?;
    let subscript_count = sequence_len(&subscripts, "VarRef subscripts")?;
    let mut rendered =
        render_vec_with_capacity(subscript_count, "VarRef rendered subscript count")?;
    for index in 0..subscript_count {
        let subscript = sequence_item(&subscripts, index, "VarRef subscript")?;
        rendered.push(subscript_token(&subscript, cfg)?);
    }
    Ok(Access {
        wrapper,
        name,
        subscripts: rendered,
    })
}

fn subscript_token(
    subscript: &Value,
    cfg: &ExprConfig,
) -> Result<SubscriptToken, minijinja::Error> {
    let index = get_field(subscript, "Index")
        .ok()
        .and_then(|index| super::render_expr::subscript_index_value(&index).ok());
    Ok(SubscriptToken {
        rendered: render_subscript(subscript, cfg)?,
        index,
    })
}

fn access_slice(
    first: &Access,
    accesses: &[Access],
    domain: &Domain,
    tuples: &[Vec<i64>],
) -> Result<Option<String>, minijinja::Error> {
    let mut used_dimensions = render_vec_with_capacity(
        domain.binders.len(),
        "structured access used dimension count",
    )?;
    used_dimensions.extend(std::iter::repeat_n(false, domain.binders.len()));
    let mut rendered_subscripts = render_vec_with_capacity(
        first.subscripts.len(),
        "structured access rendered subscript count",
    )?;
    for subscript_index in 0..first.subscripts.len() {
        if let Some(dimension) =
            matching_domain_dimension(accesses, tuples, subscript_index, &used_dimensions)
        {
            used_dimensions[dimension] = true;
            rendered_subscripts.push(render_domain_slice(&domain.binders[dimension]));
        } else if fixed_subscript(accesses, subscript_index) {
            rendered_subscripts.push(first.subscripts[subscript_index].rendered.clone());
        } else {
            return optional_render_miss();
        }
    }
    if used_dimensions.iter().any(|used| !*used) {
        return optional_render_miss();
    }
    Ok(Some(format!(
        "{}[{}]",
        first.name,
        rendered_subscripts.join(", ")
    )))
}

fn matching_domain_dimension(
    accesses: &[Access],
    tuples: &[Vec<i64>],
    subscript_index: usize,
    used_dimensions: &[bool],
) -> Option<usize> {
    (0..used_dimensions.len()).find(|dimension| {
        !used_dimensions[*dimension]
            && accesses.iter().zip(tuples).all(|(access, tuple)| {
                access
                    .subscripts
                    .get(subscript_index)
                    .and_then(|subscript| subscript.index)
                    == tuple.get(*dimension).copied()
            })
    })
}

fn fixed_subscript(accesses: &[Access], subscript_index: usize) -> bool {
    let Some(first) = accesses
        .first()
        .and_then(|access| access.subscripts.get(subscript_index))
    else {
        return false;
    };
    accesses.iter().all(|access| {
        access
            .subscripts
            .get(subscript_index)
            .is_some_and(|subscript| subscript == first)
    })
}

fn render_domain_slice(binder: &DomainBinder) -> String {
    if binder.step == 1 {
        format!("{}:{}", binder.lower, binder.upper)
    } else {
        format!("{}:{}:{}", binder.lower, binder.step, binder.upper)
    }
}

fn render_array_literal(
    exprs: &[Value],
    shape: &[usize],
    cfg: &ExprConfig,
) -> Result<String, minijinja::Error> {
    let mut rendered = render_vec_with_capacity(exprs.len(), "DAE array literal element count")?;
    for expr in exprs {
        rendered.push(render_expression(expr, cfg)?);
    }
    nested_array_literal(&rendered, shape)
}

fn nested_array_literal(values: &[String], shape: &[usize]) -> Result<String, minijinja::Error> {
    if shape.len() <= 1 {
        return Ok(format!("{{{}}}", values.join(", ")));
    }
    let chunk_len = shape[1..].iter().try_fold(1usize, |acc, dim| {
        acc.checked_mul(*dim)
            .ok_or_else(|| render_err("DAE array literal nested shape overflows host index range"))
    })?;
    if chunk_len == 0 {
        return Err(render_err(
            "DAE array literal nested shape contains zero-width chunk",
        ));
    }
    let mut chunks = render_vec_with_capacity(
        values.len().div_ceil(chunk_len),
        "DAE nested array literal chunk count",
    )?;
    for chunk in values.chunks(chunk_len) {
        chunks.push(nested_array_literal(chunk, &shape[1..])?);
    }
    Ok(format!("{{{}}}", chunks.join(", ")))
}

fn required_field(value: &Value, field: &str, context: &str) -> Result<Value, minijinja::Error> {
    get_field(value, field).map_err(|err| render_err(format!("{context} missing `{field}`: {err}")))
}

fn sequence_len(value: &Value, context: &str) -> Result<usize, minijinja::Error> {
    value
        .len()
        .ok_or_else(|| render_err(format!("{context} is not a sequence")))
}

fn sequence_item(value: &Value, index: usize, context: &str) -> Result<Value, minijinja::Error> {
    value
        .get_item(&Value::from(index))
        .map_err(|err| render_err(format!("{context} {index} is inaccessible: {err}")))
        .and_then(|item| {
            if item.is_undefined() || item.is_none() {
                Err(render_err(format!("{context} {index} is missing")))
            } else {
                Ok(item)
            }
        })
}

fn usize_sequence_field(
    value: &Value,
    field: &str,
    context: &str,
) -> Result<Vec<usize>, minijinja::Error> {
    let sequence = required_field(value, field, context)?;
    let value_count = sequence_len(&sequence, field)?;
    let mut values = render_vec_with_capacity(value_count, "usize sequence field count")?;
    for index in 0..value_count {
        let value = sequence_item(&sequence, index, field)?;
        values.push(value.as_usize().ok_or_else(|| {
            render_err(format!("{context} `{field}` entry {index} is not a usize"))
        })?);
    }
    Ok(values)
}

fn usize_field(value: &Value, field: &str, context: &str) -> Result<usize, minijinja::Error> {
    required_field(value, field, context)?
        .as_usize()
        .ok_or_else(|| render_err(format!("{context} `{field}` is not a usize")))
}

fn i64_field(value: &Value, field: &str, context: &str) -> Result<i64, minijinja::Error> {
    required_field(value, field, context)?
        .as_i64()
        .ok_or_else(|| render_err(format!("{context} `{field}` is not an integer")))
}
