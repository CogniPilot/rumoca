use std::collections::HashMap;

use rumoca_ir_dae as dae;

use crate::projection_maps::{
    build_component_index_projection_map, build_function_output_projection_map, output_scalar_count,
};

mod shape;
use shape::*;

type Dae = dae::Dae;
type Equation = dae::Equation;
type Expression = rumoca_core::Expression;
type Literal = rumoca_core::Literal;
type OpBinary = rumoca_core::OpBinary;
type OpUnary = rumoca_core::OpUnary;
type Reference = rumoca_core::Reference;
type Subscript = rumoca_core::Subscript;
type VarName = rumoca_core::VarName;
type StructuralError = crate::StructuralError;

/// Build output variable names in solver-vector order (states, algebraics, outputs).
///
/// Array variables are expanded with their DAE shape metadata, so vectors use
/// `name[1]`, `name[2]`, etc. and matrices use `name[1,1]`, `name[1,2]`, etc.
pub fn build_output_names(dae: &Dae) -> Result<Vec<String>, StructuralError> {
    let mut names = Vec::new();
    for (name, var) in dae
        .variables
        .states
        .iter()
        .chain(dae.variables.algebraics.iter())
        .chain(dae.variables.outputs.iter())
    {
        let sz = output_scalar_count(&var.dims, var.source_span)?;
        if sz <= 1 {
            names.push(name.as_str().to_string());
        } else {
            for i in 1..=sz {
                names.push(format_scalar_ref(
                    name.as_str(),
                    &var.dims,
                    var.source_span,
                    i,
                )?);
            }
        }
    }
    Ok(names)
}

/// Collect variable dimensions from all variable categories.
pub fn build_var_dims_map(dae: &Dae) -> HashMap<String, Vec<i64>> {
    let mut map = HashMap::new();
    for (name, var) in dae
        .variables
        .states
        .iter()
        .chain(dae.variables.algebraics.iter())
        .chain(dae.variables.outputs.iter())
        .chain(dae.variables.parameters.iter())
        .chain(dae.variables.constants.iter())
        .chain(dae.variables.inputs.iter())
    {
        if !var.dims.is_empty() {
            map.insert(name.as_str().to_string(), var.dims.clone());
        }
    }
    map
}

fn build_structural_int_map(
    dae: &Dae,
    var_dims: &HashMap<String, Vec<i64>>,
) -> HashMap<String, i64> {
    let mut values = HashMap::new();
    for _ in 0..dae
        .variables
        .parameters
        .len()
        .max(dae.variables.constants.len())
        .max(1)
    {
        let before = values.len();
        for (name, var) in dae
            .variables
            .constants
            .iter()
            .chain(dae.variables.parameters.iter())
        {
            if !dae.variables.constants.contains_key(name) && var.is_tunable {
                continue;
            }
            let Some(start) = var.start.as_ref() else {
                continue;
            };
            if let Some(value) = eval_structural_int_expr(start, &values, var_dims) {
                values.insert(name.as_str().to_string(), value);
            }
        }
        if values.len() == before {
            break;
        }
    }
    values
}

fn eval_structural_int_expr(
    expr: &Expression,
    values: &HashMap<String, i64>,
    var_dims: &HashMap<String, Vec<i64>>,
) -> Option<i64> {
    match expr {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(*value),
        Expression::Literal {
            value: Literal::Real(value),
            ..
        } if value.is_finite() && value.fract() == 0.0 => Some(*value as i64),
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => values.get(name.as_str()).copied(),
        Expression::Unary { op, rhs, .. } => {
            let value = eval_structural_int_expr(rhs, values, var_dims)?;
            match op {
                OpUnary::Plus | OpUnary::DotPlus => Some(value),
                OpUnary::Minus | OpUnary::DotMinus => Some(-value),
                _ => None,
            }
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = eval_structural_int_expr(lhs, values, var_dims)?;
            let rhs = eval_structural_int_expr(rhs, values, var_dims)?;
            match op {
                OpBinary::Add | OpBinary::AddElem => lhs.checked_add(rhs),
                OpBinary::Sub | OpBinary::SubElem => lhs.checked_sub(rhs),
                OpBinary::Mul | OpBinary::MulElem => lhs.checked_mul(rhs),
                OpBinary::Div | OpBinary::DivElem if rhs != 0 && lhs % rhs == 0 => Some(lhs / rhs),
                _ => None,
            }
        }
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args,
            ..
        } => {
            let Expression::VarRef {
                name, subscripts, ..
            } = args.first()?
            else {
                return None;
            };
            if !subscripts.is_empty() {
                return Some(1);
            }
            let dims = var_dims.get(name.as_str())?;
            let Some(dim_expr) = args.get(1) else {
                return (dims.len() == 1).then_some(dims[0]);
            };
            let dim = eval_structural_int_expr(dim_expr, values, var_dims)?;
            dims.get(usize::try_from(dim).ok()?.checked_sub(1)?)
                .copied()
        }
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Min,
            args,
            ..
        } => args
            .iter()
            .map(|arg| eval_structural_int_expr(arg, values, var_dims))
            .collect::<Option<Vec<_>>>()
            .and_then(|values| values.into_iter().min()),
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Max,
            args,
            ..
        } => args
            .iter()
            .map(|arg| eval_structural_int_expr(arg, values, var_dims))
            .collect::<Option<Vec<_>>>()
            .and_then(|values| values.into_iter().max()),
        _ => None,
    }
}

/// Build a map from a Complex-record base name to concrete `base.re` / `base.im`
/// scalar variable names present in the DAE.
pub fn build_complex_field_map(dae: &Dae) -> HashMap<String, [Option<String>; 2]> {
    let mut map: HashMap<String, [Option<String>; 2]> = HashMap::new();
    for (name, _) in dae
        .variables
        .states
        .iter()
        .chain(dae.variables.algebraics.iter())
        .chain(dae.variables.outputs.iter())
        .chain(dae.variables.parameters.iter())
        .chain(dae.variables.constants.iter())
        .chain(dae.variables.inputs.iter())
    {
        let raw = name.as_str();
        let Some((base, field)) = name.scope_split() else {
            continue;
        };
        let slot = match field {
            "re" => 0,
            "im" => 1,
            _ => continue,
        };
        map.entry(base.to_string()).or_insert([None, None])[slot] = Some(raw.to_string());
    }
    map
}

/// Recursively index into an expression tree at 1-based Modelica index `i`.
///
/// - `Array { elements }` → return `elements[i-1]`
/// - `VarRef { name, subscripts: [] }` for array vars → add `Subscript::generated_index(i, rumoca_core::Span::DUMMY)`
/// - `FunctionCall { is_constructor: true }` → project positional constructor arg `i`
/// - `Binary/Unary/BuiltinCall/If/FunctionCall/Index` → recurse into children
/// - Scalars (Literal, etc.) → broadcast unchanged
pub struct IndexProjectionContext<'a> {
    i: usize,
    var_dims: &'a HashMap<String, Vec<i64>>,
    structural_values: &'a HashMap<String, i64>,
    complex_fields: &'a HashMap<String, [Option<String>; 2]>,
    component_index_map: &'a HashMap<String, HashMap<usize, String>>,
    function_output_index_map: &'a HashMap<String, HashMap<usize, String>>,
}

impl<'a> IndexProjectionContext<'a> {
    fn with_index(&self, i: usize) -> IndexProjectionContext<'a> {
        IndexProjectionContext {
            i,
            var_dims: self.var_dims,
            structural_values: self.structural_values,
            complex_fields: self.complex_fields,
            component_index_map: self.component_index_map,
            function_output_index_map: self.function_output_index_map,
        }
    }

    fn project_at(&self, expr: &Expression, i: usize) -> Result<Expression, StructuralError> {
        self.with_index(i).project(expr)
    }

    fn map_exprs(&self, exprs: &[Expression]) -> Result<Vec<Expression>, StructuralError> {
        exprs.iter().map(|expr| self.project(expr)).collect()
    }

    fn project_var_ref(
        &self,
        name: &Reference,
        subscripts: &[Subscript],
        fallback: &Expression,
    ) -> Result<Expression, StructuralError> {
        if let Some(dims) = self.var_dims.get(name.as_str()) {
            return project_dimmed_var_ref(
                name,
                dims,
                subscripts,
                self.i,
                fallback,
                self.structural_values,
            );
        }

        if !subscripts.is_empty() {
            return Ok(fallback.clone());
        }

        if let Some(fields) = self.complex_fields.get(name.as_str()) {
            let projected = match self.i {
                1 => fields[0].as_ref(),
                2 => fields[1].as_ref(),
                _ => None,
            };
            if let Some(projected_name) = projected {
                return Ok(Expression::VarRef {
                    name: rumoca_core::Reference::new(projected_name.clone()),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                });
            }
        }

        if let Some(by_index) = self.component_index_map.get(name.as_str())
            && let Some(projected_name) = by_index.get(&self.i)
        {
            return Ok(Expression::VarRef {
                name: rumoca_core::Reference::new(projected_name.clone()),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            });
        }

        Ok(fallback.clone())
    }

    fn expression_shape(&self, expr: &Expression) -> ExpressionShape {
        match expr {
            Expression::Literal { value: _, .. } => ExpressionShape::Scalar,
            Expression::VarRef {
                name, subscripts, ..
            } => self.var_ref_shape(name, subscripts),
            Expression::Array {
                elements,
                is_matrix,
                ..
            } => array_literal_shape(elements, *is_matrix),
            Expression::Unary { rhs, .. } => self.expression_shape(rhs),
            Expression::Binary { op, lhs, rhs, .. } => {
                let lhs_shape = self.expression_shape(lhs);
                let rhs_shape = self.expression_shape(rhs);
                if matches!(op, OpBinary::Mul) {
                    combine_matrix_mul_shapes(lhs_shape, rhs_shape)
                } else if matches!(op, OpBinary::MulElem) {
                    combine_elementwise_shapes(lhs_shape, rhs_shape)
                } else if matches!(
                    op,
                    OpBinary::Add | OpBinary::AddElem | OpBinary::Sub | OpBinary::SubElem
                ) {
                    combine_additive_shapes(lhs_shape, rhs_shape)
                } else if matches!(op, OpBinary::Div | OpBinary::DivElem) {
                    combine_division_shapes(lhs_shape, rhs_shape)
                } else {
                    ExpressionShape::Scalar
                }
            }
            Expression::If { else_branch, .. } => self.expression_shape(else_branch),
            Expression::BuiltinCall { function, args, .. } => self.builtin_shape(*function, args),
            Expression::FunctionCall {
                args,
                is_constructor,
                ..
            } if *is_constructor => {
                if args.len() > 1 {
                    ExpressionShape::Vector(args.len())
                } else {
                    ExpressionShape::Scalar
                }
            }
            Expression::Index {
                base, subscripts, ..
            } => self
                .expression_dims(base)
                .and_then(|dims| {
                    apply_subscripts_to_dims(&dims, subscripts, self.structural_values)
                })
                .map(|dims| shape_from_dims(&dims))
                .unwrap_or(ExpressionShape::Other),
            _ => ExpressionShape::Other,
        }
    }

    fn expression_dims(&self, expr: &Expression) -> Option<Vec<i64>> {
        match self.expression_shape(expr) {
            ExpressionShape::Scalar => Some(Vec::new()),
            ExpressionShape::Vector(n) => Some(vec![n as i64]),
            ExpressionShape::Matrix(r, c) => Some(vec![r as i64, c as i64]),
            ExpressionShape::Other => None,
        }
    }

    fn var_ref_shape(&self, name: &Reference, subscripts: &[Subscript]) -> ExpressionShape {
        if let Some(dims) = self.var_dims.get(name.as_str()) {
            return apply_subscripts_to_dims(dims, subscripts, self.structural_values)
                .map(|dims| shape_from_dims(&dims))
                .unwrap_or(ExpressionShape::Other);
        }
        if self.complex_fields.contains_key(name.as_str()) {
            return ExpressionShape::Vector(2);
        }
        if let Some(by_index) = self.component_index_map.get(name.as_str()) {
            return ExpressionShape::Vector(by_index.len());
        }
        ExpressionShape::Scalar
    }

    fn builtin_shape(
        &self,
        function: rumoca_core::BuiltinFunction,
        args: &[Expression],
    ) -> ExpressionShape {
        match function {
            rumoca_core::BuiltinFunction::Der
            | rumoca_core::BuiltinFunction::Pre
            | rumoca_core::BuiltinFunction::NoEvent => args
                .first()
                .map(|arg| self.expression_shape(arg))
                .unwrap_or(ExpressionShape::Scalar),
            rumoca_core::BuiltinFunction::Transpose => {
                match args.first().map(|arg| self.expression_shape(arg)) {
                    Some(ExpressionShape::Matrix(r, c)) => ExpressionShape::Matrix(c, r),
                    Some(ExpressionShape::Vector(n)) => ExpressionShape::Vector(n),
                    Some(shape) => shape,
                    None => ExpressionShape::Other,
                }
            }
            rumoca_core::BuiltinFunction::Cross => ExpressionShape::Vector(3),
            rumoca_core::BuiltinFunction::Skew => ExpressionShape::Matrix(3, 3),
            rumoca_core::BuiltinFunction::Identity => args
                .first()
                .and_then(integer_literal_value)
                .and_then(|n| (n > 0).then_some(ExpressionShape::Matrix(n as usize, n as usize)))
                .unwrap_or(ExpressionShape::Other),
            rumoca_core::BuiltinFunction::Diagonal => {
                match args.first().map(|arg| self.expression_shape(arg)) {
                    Some(ExpressionShape::Vector(n)) => ExpressionShape::Matrix(n, n),
                    _ => ExpressionShape::Other,
                }
            }
            rumoca_core::BuiltinFunction::Vector => args
                .first()
                .map(|arg| match self.expression_shape(arg) {
                    ExpressionShape::Scalar => ExpressionShape::Vector(1),
                    ExpressionShape::Vector(n) => ExpressionShape::Vector(n),
                    ExpressionShape::Matrix(r, c) => ExpressionShape::Vector(r * c),
                    ExpressionShape::Other => ExpressionShape::Other,
                })
                .unwrap_or(ExpressionShape::Other),
            rumoca_core::BuiltinFunction::Matrix => args
                .first()
                .map(|arg| match self.expression_shape(arg) {
                    ExpressionShape::Scalar => ExpressionShape::Matrix(1, 1),
                    ExpressionShape::Vector(n) => ExpressionShape::Matrix(n, 1),
                    shape => shape,
                })
                .unwrap_or(ExpressionShape::Other),
            rumoca_core::BuiltinFunction::Scalar
            | rumoca_core::BuiltinFunction::Sum
            | rumoca_core::BuiltinFunction::Product
            | rumoca_core::BuiltinFunction::Size
            | rumoca_core::BuiltinFunction::Ndims => ExpressionShape::Scalar,
            _ => ExpressionShape::Scalar,
        }
    }

    fn project_matrix_mul(
        &self,
        lhs: &Expression,
        rhs: &Expression,
    ) -> Result<Option<Expression>, StructuralError> {
        let lhs_shape = self.expression_shape(lhs);
        let rhs_shape = self.expression_shape(rhs);
        match (lhs_shape, rhs_shape) {
            (ExpressionShape::Vector(n), ExpressionShape::Vector(m)) if n == m => {
                let mut terms = Vec::with_capacity(n);
                for k in 1..=n {
                    terms.push(mul_expr(self.project_at(lhs, k)?, self.project_at(rhs, k)?));
                }
                Ok(Some(sum_terms(terms)))
            }
            (ExpressionShape::Matrix(rows, cols), ExpressionShape::Vector(n)) if cols == n => {
                if self.i < 1 || self.i > rows {
                    return Ok(None);
                }
                let row = self.i;
                let mut terms = Vec::with_capacity(cols);
                for k in 1..=cols {
                    terms.push(mul_expr(
                        self.project_at(lhs, matrix_linear_index(row, k, cols))?,
                        self.project_at(rhs, k)?,
                    ));
                }
                Ok(Some(sum_terms(terms)))
            }
            (ExpressionShape::Vector(n), ExpressionShape::Matrix(rows, cols)) if n == rows => {
                if self.i < 1 || self.i > cols {
                    return Ok(None);
                }
                let col = self.i;
                let mut terms = Vec::with_capacity(n);
                for k in 1..=n {
                    terms.push(mul_expr(
                        self.project_at(lhs, k)?,
                        self.project_at(rhs, matrix_linear_index(k, col, cols))?,
                    ));
                }
                Ok(Some(sum_terms(terms)))
            }
            (
                ExpressionShape::Matrix(lhs_rows, lhs_cols),
                ExpressionShape::Matrix(rhs_rows, rhs_cols),
            ) if lhs_cols == rhs_rows => {
                let result_count = lhs_rows * rhs_cols;
                if self.i < 1 || self.i > result_count {
                    return Ok(None);
                }
                let (row, col) = row_major_subscripts_2d(self.i, rhs_cols);
                let mut terms = Vec::with_capacity(lhs_cols);
                for k in 1..=lhs_cols {
                    terms.push(mul_expr(
                        self.project_at(lhs, matrix_linear_index(row, k, lhs_cols))?,
                        self.project_at(rhs, matrix_linear_index(k, col, rhs_cols))?,
                    ));
                }
                Ok(Some(sum_terms(terms)))
            }
            _ => Ok(None),
        }
    }

    fn project_transpose(
        &self,
        args: &[Expression],
    ) -> Result<Option<Expression>, StructuralError> {
        let Some(arg) = args.first() else {
            return Ok(None);
        };
        match self.expression_shape(arg) {
            ExpressionShape::Matrix(rows, cols) => {
                let result_count = rows * cols;
                if self.i < 1 || self.i > result_count {
                    return Ok(None);
                }
                let (row, col) = row_major_subscripts_2d(self.i, rows);
                Ok(Some(
                    self.project_at(arg, matrix_linear_index(col, row, cols))?,
                ))
            }
            ExpressionShape::Vector(n) if self.i >= 1 && self.i <= n => {
                Ok(Some(self.project_at(arg, self.i)?))
            }
            ExpressionShape::Scalar if self.i == 1 => Ok(Some(self.project_at(arg, 1)?)),
            _ => Ok(None),
        }
    }

    fn project_cross(&self, args: &[Expression]) -> Result<Option<Expression>, StructuralError> {
        let Some(lhs) = args.first() else {
            return Ok(None);
        };
        let Some(rhs) = args.get(1) else {
            return Ok(None);
        };
        if self.expression_shape(lhs) != ExpressionShape::Vector(3)
            || self.expression_shape(rhs) != ExpressionShape::Vector(3)
        {
            return Ok(None);
        }

        let component = match self.i {
            1 => sub_expr(
                mul_expr(self.project_at(lhs, 2)?, self.project_at(rhs, 3)?),
                mul_expr(self.project_at(lhs, 3)?, self.project_at(rhs, 2)?),
            ),
            2 => sub_expr(
                mul_expr(self.project_at(lhs, 3)?, self.project_at(rhs, 1)?),
                mul_expr(self.project_at(lhs, 1)?, self.project_at(rhs, 3)?),
            ),
            3 => sub_expr(
                mul_expr(self.project_at(lhs, 1)?, self.project_at(rhs, 2)?),
                mul_expr(self.project_at(lhs, 2)?, self.project_at(rhs, 1)?),
            ),
            _ => return Ok(None),
        };
        Ok(Some(component))
    }

    // SPEC_0021: Exception - cohesive lowering of scalarized linear-algebra Expression forms.
    #[allow(clippy::too_many_lines)]
    fn lower_scalar_linear_algebra(
        &self,
        expr: &Expression,
    ) -> Result<Expression, StructuralError> {
        match expr {
            Expression::Binary { op, lhs, rhs, .. } => {
                let lowered_lhs = self.lower_scalar_linear_algebra(lhs)?;
                let lowered_rhs = self.lower_scalar_linear_algebra(rhs)?;
                if matches!(op, OpBinary::Mul)
                    && self.expression_shape(expr) == ExpressionShape::Scalar
                    && let Some(projected) = self
                        .with_index(1)
                        .project_matrix_mul(&lowered_lhs, &lowered_rhs)?
                {
                    return Ok(projected);
                }
                Ok(Expression::Binary {
                    op: op.clone(),
                    lhs: Box::new(lowered_lhs),
                    rhs: Box::new(lowered_rhs),
                    span: rumoca_core::Span::DUMMY,
                })
            }
            Expression::Unary { op, rhs, .. } => Ok(Expression::Unary {
                op: op.clone(),
                rhs: Box::new(self.lower_scalar_linear_algebra(rhs)?),
                span: rumoca_core::Span::DUMMY,
            }),
            Expression::BuiltinCall { function, args, .. } => Ok(Expression::BuiltinCall {
                function: *function,
                args: args
                    .iter()
                    .map(|arg| self.lower_scalar_linear_algebra(arg))
                    .collect::<Result<Vec<_>, _>>()?,
                span: rumoca_core::Span::DUMMY,
            }),
            Expression::If {
                branches,
                else_branch,
                ..
            } => Ok(Expression::If {
                branches: branches
                    .iter()
                    .map(|(condition, value)| {
                        Ok((condition.clone(), self.lower_scalar_linear_algebra(value)?))
                    })
                    .collect::<Result<Vec<_>, StructuralError>>()?,
                else_branch: Box::new(self.lower_scalar_linear_algebra(else_branch)?),
                span: rumoca_core::Span::DUMMY,
            }),
            Expression::FunctionCall {
                name,
                args,
                is_constructor,
                ..
            } => Ok(Expression::FunctionCall {
                name: name.clone(),
                args: args
                    .iter()
                    .map(|arg| self.lower_scalar_linear_algebra(arg))
                    .collect::<Result<Vec<_>, _>>()?,
                is_constructor: *is_constructor,
                span: rumoca_core::Span::DUMMY,
            }),
            Expression::Array {
                elements,
                is_matrix,
                ..
            } => Ok(Expression::Array {
                elements: elements
                    .iter()
                    .map(|element| self.lower_scalar_linear_algebra(element))
                    .collect::<Result<Vec<_>, _>>()?,
                is_matrix: *is_matrix,
                span: rumoca_core::Span::DUMMY,
            }),
            Expression::Tuple { elements, .. } => Ok(Expression::Tuple {
                elements: elements
                    .iter()
                    .map(|element| self.lower_scalar_linear_algebra(element))
                    .collect::<Result<Vec<_>, _>>()?,
                span: rumoca_core::Span::DUMMY,
            }),
            Expression::Index {
                base, subscripts, ..
            } => {
                if let Some(dims) = self.expression_dims(base)
                    && apply_subscripts_to_dims(&dims, subscripts, self.structural_values)
                        .is_some_and(|dims| dims.is_empty())
                    && let Some(index) = linear_index_for_static_subscripts(
                        &dims,
                        subscripts,
                        self.structural_values,
                    )
                {
                    return self.project_at(base, index);
                }
                Ok(Expression::Index {
                    base: Box::new(self.lower_scalar_linear_algebra(base)?),
                    subscripts: subscripts.clone(),
                    span: rumoca_core::Span::DUMMY,
                })
            }
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                ..
            } => Ok(Expression::ArrayComprehension {
                expr: Box::new(self.lower_scalar_linear_algebra(expr)?),
                indices: indices.clone(),
                filter: filter
                    .as_ref()
                    .map(|value| self.lower_scalar_linear_algebra(value).map(Box::new))
                    .transpose()?,
                span: rumoca_core::Span::DUMMY,
            }),
            _ => Ok(expr.clone()),
        }
    }

    fn project(&self, expr: &Expression) -> Result<Expression, StructuralError> {
        match expr {
            Expression::Array {
                elements,
                is_matrix,
                ..
            } => Ok(project_array_literal_scalar(elements, *is_matrix, self.i)
                .unwrap_or_else(|| expr.clone())),
            Expression::VarRef {
                name, subscripts, ..
            } => self.project_var_ref(name, subscripts, expr),
            Expression::Binary { op, lhs, rhs, .. } => {
                if matches!(op, OpBinary::Mul)
                    && let Some(projected) = self.project_matrix_mul(lhs, rhs)?
                {
                    return Ok(projected);
                }
                Ok(Expression::Binary {
                    op: op.clone(),
                    lhs: Box::new(self.project(lhs)?),
                    rhs: Box::new(self.project(rhs)?),
                    span: rumoca_core::Span::DUMMY,
                })
            }
            Expression::Unary { op, rhs, .. } => Ok(Expression::Unary {
                op: op.clone(),
                rhs: Box::new(self.project(rhs)?),
                span: rumoca_core::Span::DUMMY,
            }),
            Expression::BuiltinCall { function, args, .. } => {
                if matches!(function, rumoca_core::BuiltinFunction::Transpose)
                    && let Some(projected) = self.project_transpose(args)?
                {
                    return Ok(projected);
                }
                if matches!(function, rumoca_core::BuiltinFunction::Cross)
                    && let Some(projected) = self.project_cross(args)?
                {
                    return Ok(projected);
                }
                Ok(Expression::BuiltinCall {
                    function: *function,
                    args: self.map_exprs(args)?,
                    span: rumoca_core::Span::DUMMY,
                })
            }
            Expression::If {
                branches,
                else_branch,
                ..
            } => Ok(Expression::If {
                branches: branches
                    .iter()
                    .map(|(cond, val)| Ok((cond.clone(), self.project(val)?)))
                    .collect::<Result<Vec<_>, StructuralError>>()?,
                else_branch: Box::new(self.project(else_branch)?),
                span: rumoca_core::Span::DUMMY,
            }),
            Expression::FunctionCall {
                name,
                args,
                is_constructor,
                ..
            } => self.project_function_call(name, args, *is_constructor),
            Expression::Index {
                base, subscripts, ..
            } => Ok(Expression::Index {
                base: Box::new(self.project(base)?),
                subscripts: subscripts.clone(),
                span: rumoca_core::Span::DUMMY,
            }),
            Expression::FieldAccess { base, field, .. } => {
                if let Some(projected) = self.project_record_array_member_slice(base, field) {
                    return Ok(projected);
                }
                Ok(expr.clone())
            }
            _ => Ok(expr.clone()),
        }
    }

    fn project_function_call(
        &self,
        name: &Reference,
        args: &[Expression],
        is_constructor: bool,
    ) -> Result<Expression, StructuralError> {
        if is_constructor && self.i >= 1 && self.i <= args.len() {
            return self.project(&args[self.i - 1]);
        }
        if let Some(by_index) = self.function_output_index_map.get(name.as_str())
            && let Some(projected_output) = by_index.get(&self.i)
        {
            return Ok(Expression::FunctionCall {
                name: rumoca_core::Reference::new(format!(
                    "{}.{}",
                    name.as_str(),
                    projected_output
                )),
                args: args.to_vec(),
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            });
        }
        Ok(Expression::FunctionCall {
            name: name.clone(),
            args: self.map_exprs(args)?,
            is_constructor,
            span: rumoca_core::Span::DUMMY,
        })
    }

    /// Projects element `i` of a record-array member slice such as
    /// `ac.pin[:].v` into the scalarized component variable `ac.pin[i].v`.
    /// Declines when the selection is not a full one-dimensional colon slice
    /// over a structured base or the element variable is unknown.
    fn project_record_array_member_slice(
        &self,
        base: &Expression,
        field: &str,
    ) -> Option<Expression> {
        let Expression::Index {
            base: inner,
            subscripts,
            span,
        } = base
        else {
            return None;
        };
        let Expression::VarRef {
            name,
            subscripts: ref_subscripts,
            ..
        } = inner.as_ref()
        else {
            return None;
        };
        if !ref_subscripts.is_empty()
            || subscripts.len() != 1
            || !matches!(subscripts[0], Subscript::Colon { .. })
            || self.i == 0
        {
            return None;
        }
        let component_ref = name.component_ref()?;
        let mut element_ref = component_ref.clone();
        element_ref.parts.last_mut()?.subs = vec![Subscript::generated_index(self.i as i64, *span)];
        element_ref.parts.push(rumoca_core::ComponentRefPart {
            ident: field.to_string(),
            span: *span,
            subs: Vec::new(),
        });
        // Existence of the element variable is enforced by the Solve
        // reference resolver, which fails loudly on unknown references.
        let reference = rumoca_core::Reference::from_component_reference(element_ref);
        Some(Expression::VarRef {
            name: reference,
            subscripts: vec![],
            span: *span,
        })
    }
}

fn project_dimmed_var_ref(
    name: &Reference,
    dims: &[i64],
    subscripts: &[Subscript],
    i: usize,
    fallback: &Expression,
    structural_values: &HashMap<String, i64>,
) -> Result<Expression, StructuralError> {
    if !subscripts.is_empty() {
        return Ok(project_subscripted_dims(
            dims,
            subscripts,
            i,
            fallback.span().unwrap_or(rumoca_core::Span::DUMMY),
            structural_values,
        )?
        .map(|projected_subscripts| Expression::VarRef {
            name: name.clone(),
            subscripts: projected_subscripts,
            span: rumoca_core::Span::DUMMY,
        })
        .unwrap_or_else(|| fallback.clone()));
    }

    let scalar_count =
        output_scalar_count(dims, fallback.span().unwrap_or(rumoca_core::Span::DUMMY))?;
    if scalar_count > 1 && i <= scalar_count {
        return Ok(Expression::VarRef {
            name: name.clone(),
            subscripts: linear_subscripts_for_dims(dims, i),
            span: rumoca_core::Span::DUMMY,
        });
    }

    Ok(fallback.clone())
}

fn integer_literal_value(expr: &Expression) -> Option<i64> {
    match expr {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(*value),
        Expression::Literal {
            value: Literal::Real(value),
            ..
        } if value.is_finite() && value.fract() == 0.0 => Some(*value as i64),
        _ => None,
    }
}

fn project_subscripted_dims(
    dims: &[i64],
    subscripts: &[Subscript],
    linear_index: usize,
    span: rumoca_core::Span,
    structural_values: &HashMap<String, i64>,
) -> Result<Option<Vec<Subscript>>, StructuralError> {
    if linear_index == 0 {
        return Ok(None);
    }
    // MLS §10.6: array equations are equivalent to scalar equations over each
    // selected element. Preserve written fixed subscripts and replace only
    // projected slice/trailing dimensions with scalar indices.
    let Some(projected_dims) = apply_subscripts_to_dims(dims, subscripts, structural_values) else {
        return Ok(None);
    };
    let scalar_count = output_scalar_count(&projected_dims, span)?;
    if scalar_count == 0
        || linear_index > scalar_count
        || (scalar_count == 1 && projected_dims.is_empty())
    {
        return Ok(None);
    }

    let projection = linear_subscripts_for_dims(&projected_dims, linear_index);
    let mut projection_iter = projection.into_iter();
    let mut projected_subscripts = Vec::new();
    let mut dim_idx = 0usize;

    for subscript in subscripts {
        if dim_idx >= dims.len() {
            projected_subscripts.push(subscript.clone());
            continue;
        }
        match subscript {
            Subscript::Expr { expr, .. }
                if range_subscript_indices(expr, structural_values).is_some() =>
            {
                let Subscript::Index {
                    value: projected_index,
                    ..
                } = projection_iter.next().ok_or_else(|| {
                    StructuralError::ContractViolation {
                        reason: format!(
                            "scalarization projection for dimensions {projected_dims:?} ended before subscript projection completed"
                        ),
                        span: rumoca_core::Span::DUMMY,
                    }
                })?
                else {
                    return Ok(None);
                };
                let Some(selected_index) = usize::try_from(projected_index)
                    .ok()
                    .and_then(|index| index.checked_sub(1))
                else {
                    return Ok(None);
                };
                let Some(selected) = range_subscript_indices(expr, structural_values)
                    .and_then(|indices| indices.get(selected_index).copied())
                else {
                    return Ok(None);
                };
                projected_subscripts.push(Subscript::generated_index(
                    selected,
                    rumoca_core::Span::DUMMY,
                ));
                dim_idx += 1;
            }
            Subscript::Index { .. } | Subscript::Expr { expr: _, .. } => {
                projected_subscripts.push(subscript.clone());
                dim_idx += 1;
            }
            Subscript::Colon { .. } => {
                let Some(projected) = projection_iter.next() else {
                    return Ok(None);
                };
                projected_subscripts.push(projected);
                dim_idx += 1;
            }
        }
    }

    while dim_idx < dims.len() {
        let Some(projected) = projection_iter.next() else {
            return Ok(None);
        };
        projected_subscripts.push(projected);
        dim_idx += 1;
    }

    Ok(Some(projected_subscripts))
}

fn format_scalar_ref(
    name: &str,
    dims: &[i64],
    span: rumoca_core::Span,
    linear_index: usize,
) -> Result<String, StructuralError> {
    if dims.is_empty() || output_scalar_count(dims, span)? <= 1 {
        return Ok(name.to_string());
    }
    Ok(dae::scalar_name_text_for_flat_index(
        name,
        dims,
        linear_index.saturating_sub(1),
    ))
}

fn binary_expr(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn add_expr(lhs: Expression, rhs: Expression) -> Expression {
    binary_expr(OpBinary::Add, lhs, rhs)
}

fn sub_expr(lhs: Expression, rhs: Expression) -> Expression {
    binary_expr(OpBinary::Sub, lhs, rhs)
}

fn mul_expr(lhs: Expression, rhs: Expression) -> Expression {
    binary_expr(OpBinary::Mul, lhs, rhs)
}

fn sum_terms(terms: impl IntoIterator<Item = Expression>) -> Expression {
    let mut iter = terms.into_iter();
    let Some(first) = iter.next() else {
        return Expression::Literal {
            value: Literal::Real(0.0),
            span: rumoca_core::Span::DUMMY,
        };
    };
    iter.fold(first, add_expr)
}

fn project_array_literal_scalar(
    elements: &[Expression],
    is_matrix: bool,
    scalar_index: usize,
) -> Option<Expression> {
    if scalar_index == 0 {
        return None;
    }

    let first = elements.first()?;
    let Expression::Array {
        elements: first_row,
        ..
    } = first
    else {
        if !is_matrix {
            return elements.get(scalar_index - 1).cloned();
        }
        // MLS §10.4: a single-row matrix literal is encoded as `is_matrix=true`
        // with scalar elements. Preserve the written row order on the compiled
        // scalarization path so it matches interpreted array evaluation.
        return elements.get(scalar_index - 1).cloned();
    };
    let cols = first_row.len();
    if cols == 0 {
        return None;
    }

    let flat = scalar_index - 1;
    let row = flat / cols;
    let col = flat % cols;
    let Expression::Array {
        elements: row_elements,
        ..
    } = elements.get(row)?
    else {
        return None;
    };
    row_elements.get(col).cloned()
}

pub fn index_into_expr(
    expr: &Expression,
    i: usize,
    var_dims: &HashMap<String, Vec<i64>>,
    complex_fields: &HashMap<String, [Option<String>; 2]>,
    component_index_map: &HashMap<String, HashMap<usize, String>>,
    function_output_index_map: &HashMap<String, HashMap<usize, String>>,
) -> Result<Expression, StructuralError> {
    let structural_values = HashMap::new();
    IndexProjectionContext {
        i,
        var_dims,
        structural_values: &structural_values,
        complex_fields,
        component_index_map,
        function_output_index_map,
    }
    .project(expr)
}

pub struct ExpressionScalarizationContext {
    var_dims: HashMap<String, Vec<i64>>,
    structural_values: HashMap<String, i64>,
    complex_fields: HashMap<String, [Option<String>; 2]>,
    component_index_map: HashMap<String, HashMap<usize, String>>,
    function_output_index_map: HashMap<String, HashMap<usize, String>>,
}

pub fn build_expression_scalarization_context(
    dae: &Dae,
) -> Result<ExpressionScalarizationContext, StructuralError> {
    let var_dims = build_var_dims_map(dae);
    Ok(ExpressionScalarizationContext {
        structural_values: build_structural_int_map(dae, &var_dims),
        var_dims,
        complex_fields: build_complex_field_map(dae),
        component_index_map: build_component_index_projection_map(dae),
        function_output_index_map: build_function_output_projection_map(dae)?,
    })
}

pub fn scalarize_expression_rows(
    expr: &Expression,
    output_len: usize,
    ctx: &ExpressionScalarizationContext,
) -> Result<Vec<Expression>, StructuralError> {
    if output_len <= 1 {
        return Ok(vec![expr.clone()]);
    }

    (1..=output_len)
        .map(|index| {
            IndexProjectionContext {
                i: index,
                var_dims: &ctx.var_dims,
                structural_values: &ctx.structural_values,
                complex_fields: &ctx.complex_fields,
                component_index_map: &ctx.component_index_map,
                function_output_index_map: &ctx.function_output_index_map,
            }
            .project(expr)
        })
        .collect::<Result<Vec<_>, _>>()
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScalarizedLhsTarget {
    name: String,
    expr: Expression,
    array_selector: Option<usize>,
    field_selector: Option<usize>,
}

impl ScalarizedLhsTarget {
    fn from_projected_var_ref(
        name: &Reference,
        subscripts: Vec<Subscript>,
        index: usize,
    ) -> Option<Self> {
        Some(Self {
            name: scalarization_var_ref_name(name, &subscripts)?,
            expr: var_ref_expr(name, &subscripts),
            array_selector: Some(index),
            field_selector: None,
        })
    }

    fn from_flattened_name(lhs: &str, target: &str) -> Option<Self> {
        let (array_selector, field_selector) = parse_scalar_target_projection(lhs, target)?;
        Some(Self {
            name: target.to_string(),
            expr: target_var_ref_expr(target),
            array_selector,
            field_selector,
        })
    }
}

pub fn scalar_targets_for_lhs(
    lhs: &str,
    span: rumoca_core::Span,
    scalar_names: &[String],
    var_dims: &HashMap<String, Vec<i64>>,
) -> Result<Vec<ScalarizedLhsTarget>, StructuralError> {
    if let Some(dims) = var_dims.get(lhs) {
        let scalar_count = output_scalar_count(dims, span)?;
        if scalar_count > 1 {
            let lhs_name = Reference::new(lhs);
            return Ok((1..=scalar_count)
                .filter_map(|index| {
                    let subscripts = linear_subscripts_for_dims(dims, index);
                    ScalarizedLhsTarget::from_projected_var_ref(&lhs_name, subscripts, index)
                })
                .collect());
        }
    }

    let dotted_prefix = format!("{lhs}.");
    let indexed_prefix = format!("{lhs}[");
    Ok(scalar_names
        .iter()
        .filter(|name| {
            let raw = name.as_str();
            raw == lhs || raw.starts_with(&dotted_prefix) || raw.starts_with(&indexed_prefix)
        })
        .filter_map(|target| ScalarizedLhsTarget::from_flattened_name(lhs, target))
        .collect())
}

pub fn scalarization_subscript_text(sub: &Subscript) -> Option<String> {
    match sub {
        Subscript::Index { value: i, .. } => Some(i.to_string()),
        Subscript::Expr { expr, .. } => match expr.as_ref() {
            Expression::Literal {
                value: Literal::Integer(i),
                ..
            } => Some(i.to_string()),
            Expression::Literal {
                value: Literal::Real(v),
                ..
            } if v.is_finite() && v.fract() == 0.0 => Some((*v as i64).to_string()),
            _ => None,
        },
        _ => None,
    }
}

pub trait ScalarizationReferenceName {
    fn scalarization_name(&self) -> &str;
}

impl ScalarizationReferenceName for VarName {
    fn scalarization_name(&self) -> &str {
        self.as_str()
    }
}

impl ScalarizationReferenceName for Reference {
    fn scalarization_name(&self) -> &str {
        self.as_str()
    }
}

pub fn scalarization_var_ref_name(
    name: &impl ScalarizationReferenceName,
    subscripts: &[Subscript],
) -> Option<String> {
    if subscripts.is_empty() {
        return Some(name.scalarization_name().to_string());
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        indices.push(scalarization_subscript_text(sub)?);
    }
    Some(format!(
        "{}[{}]",
        name.scalarization_name(),
        indices.join(",")
    ))
}

pub fn residual_lhs_target_name(expr: &Expression) -> Option<String> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        ..
    } = expr
    else {
        return None;
    };
    if let Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
    {
        return scalarization_var_ref_name(name, subscripts);
    }
    None
}

fn residual_lhs_var_ref(expr: &Expression) -> Option<(&Reference, &[Subscript])> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        ..
    } = expr
    else {
        return None;
    };
    if let Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
    {
        return Some((name, subscripts));
    }
    None
}

fn residual_lhs_scalar_targets(
    expr: &Expression,
    span: rumoca_core::Span,
    var_dims: &HashMap<String, Vec<i64>>,
    structural_values: &HashMap<String, i64>,
) -> Result<Vec<ScalarizedLhsTarget>, StructuralError> {
    let Some((name, subscripts)) = residual_lhs_var_ref(expr) else {
        return Ok(Vec::new());
    };
    let Some(dims) = var_dims.get(name.as_str()) else {
        return Ok(Vec::new());
    };
    if subscripts.is_empty() {
        return Ok(Vec::new());
    }

    let Some(projected_dims) = apply_subscripts_to_dims(dims, subscripts, structural_values) else {
        return Ok(Vec::new());
    };
    let scalar_count = output_scalar_count(&projected_dims, span)?;

    let mut targets = Vec::new();
    for idx in 1..=scalar_count {
        let Some(projected_subscripts) =
            project_subscripted_dims(dims, subscripts, idx, span, structural_values)?
        else {
            continue;
        };
        if let Some(target) =
            ScalarizedLhsTarget::from_projected_var_ref(name, projected_subscripts, idx)
        {
            targets.push(target);
        }
    }
    Ok(targets)
}

pub fn parse_one_based_index(text: &str) -> Option<usize> {
    let idx = text.trim().parse::<usize>().ok()?;
    (idx > 0).then_some(idx)
}

pub fn parse_complex_field_selector(fragment: &str) -> Option<(usize, Option<usize>)> {
    let (field_name, array_selector) =
        if let Some(scalar) = rumoca_core::parse_scalar_name(fragment) {
            let index = match scalar.indices.as_slice() {
                [index] => usize::try_from(*index).ok().filter(|index| *index > 0)?,
                _ => return None,
            };
            (scalar.base.trim(), Some(index))
        } else {
            (fragment.trim(), None)
        };

    let field_selector = match field_name {
        "re" => 1,
        "im" => 2,
        _ => return None,
    };
    Some((field_selector, array_selector))
}

fn scalar_target_index_for_base(base: &str, target: &str) -> Option<usize> {
    let scalar = rumoca_core::parse_scalar_name(target)?;
    if scalar.base != base {
        return None;
    }
    match scalar.indices.as_slice() {
        [index] => usize::try_from(*index).ok().filter(|index| *index > 0),
        _ => None,
    }
}

pub fn parse_scalar_target_projection(
    lhs: &str,
    target: &str,
) -> Option<(Option<usize>, Option<usize>)> {
    if target == lhs {
        return Some((None, None));
    }

    if let Some(index) = scalar_target_index_for_base(lhs, target) {
        return Some((Some(index), None));
    }

    let target_path = rumoca_core::ComponentPath::from_flat_path(target);
    let (field_part, base_parts) = target_path.parts().split_last()?;
    let base_path = rumoca_core::ComponentPath::from_parts(base_parts.iter().cloned());
    let base_part = base_path.as_str();
    if base_part == lhs {
        let (field_selector, array_selector) = parse_complex_field_selector(field_part)?;
        return Some((array_selector, Some(field_selector)));
    }
    if let Some(index) = scalar_target_index_for_base(lhs, base_part) {
        let (field_selector, nested_array_selector) = parse_complex_field_selector(field_part)?;
        return Some((nested_array_selector.or(Some(index)), Some(field_selector)));
    }

    None
}

pub fn target_var_ref_expr(target: &str) -> Expression {
    if let Some(scalar) = rumoca_core::parse_scalar_name(target) {
        let indices = scalar
            .indices
            .into_iter()
            .map(|index| usize::try_from(index).ok().filter(|index| *index > 0))
            .collect::<Option<Vec<_>>>();
        if let Some(indices) = indices {
            return Expression::VarRef {
                name: rumoca_core::Reference::new(scalar.base.to_string()),
                subscripts: indices
                    .into_iter()
                    .map(|idx| Subscript::generated_index(idx as i64, rumoca_core::Span::DUMMY))
                    .collect(),
                span: rumoca_core::Span::DUMMY,
            };
        }
    }

    Expression::VarRef {
        name: rumoca_core::Reference::new(target.to_string()),
        subscripts: Vec::new(),
        span: rumoca_core::Span::DUMMY,
    }
}

pub struct ScalarProjectionContext<'a> {
    var_dims: &'a HashMap<String, Vec<i64>>,
    structural_values: &'a HashMap<String, i64>,
    complex_fields: &'a HashMap<String, [Option<String>; 2]>,
    component_index_map: &'a HashMap<String, HashMap<usize, String>>,
    function_output_index_map: &'a HashMap<String, HashMap<usize, String>>,
}

impl ScalarProjectionContext<'_> {
    fn index_context(&self, scalar_idx: usize) -> IndexProjectionContext<'_> {
        IndexProjectionContext {
            i: scalar_idx,
            var_dims: self.var_dims,
            structural_values: self.structural_values,
            complex_fields: self.complex_fields,
            component_index_map: self.component_index_map,
            function_output_index_map: self.function_output_index_map,
        }
    }

    fn project_index(
        &self,
        expr: &Expression,
        scalar_idx: usize,
    ) -> Result<Expression, StructuralError> {
        self.index_context(scalar_idx).project(expr)
    }

    fn lower_scalar_linear_algebra(
        &self,
        expr: &Expression,
    ) -> Result<Expression, StructuralError> {
        self.index_context(1).lower_scalar_linear_algebra(expr)
    }

    fn expression_shape(&self, expr: &Expression) -> ExpressionShape {
        self.index_context(1).expression_shape(expr)
    }
}

fn complex_zero() -> Expression {
    Expression::Literal {
        value: Literal::Real(0.0),
        span: rumoca_core::Span::DUMMY,
    }
}

fn complex_add(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn complex_sub(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn complex_mul(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn complex_div(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Div,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn var_ref_expr(name: &Reference, subscripts: &[Subscript]) -> Expression {
    Expression::VarRef {
        name: name.clone(),
        subscripts: subscripts.to_vec(),
        span: rumoca_core::Span::DUMMY,
    }
}

fn project_complex_var_ref(
    name: &Reference,
    subscripts: &[Subscript],
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Expression {
    if is_complex_field_scalar_name(name.as_str(), "re") {
        return if field_idx == 1 {
            var_ref_expr(name, subscripts)
        } else {
            complex_zero()
        };
    }
    if is_complex_field_scalar_name(name.as_str(), "im") {
        return if field_idx == 2 {
            var_ref_expr(name, subscripts)
        } else {
            complex_zero()
        };
    }
    if let Some(fields) = projection.complex_fields.get(name.as_str()) {
        let projected_name = match field_idx {
            1 => fields[0].as_ref(),
            2 => fields[1].as_ref(),
            _ => None,
        };
        if let Some(projected_name) = projected_name {
            return Expression::VarRef {
                name: rumoca_core::Reference::new(projected_name.clone()),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            };
        }
    }
    if field_idx == 1 {
        var_ref_expr(name, subscripts)
    } else {
        complex_zero()
    }
}

fn is_complex_field_scalar_name(name: &str, field: &str) -> bool {
    rumoca_core::top_level_path_ends_with(name, field)
}

fn project_complex_unary(
    expr: &Expression,
    op: &OpUnary,
    rhs: &Expression,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Result<Expression, StructuralError> {
    if matches!(op, OpUnary::Minus | OpUnary::DotMinus) {
        return Ok(Expression::Unary {
            op: op.clone(),
            rhs: Box::new(project_complex_component(rhs, field_idx, projection)?),
            span: rumoca_core::Span::DUMMY,
        });
    }
    if field_idx == 1 {
        Ok(expr.clone())
    } else {
        Ok(complex_zero())
    }
}

fn project_complex_mul_or_div(
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Result<Expression, StructuralError> {
    let lhs_re = project_complex_component(lhs, 1, projection)?;
    let lhs_im = project_complex_component(lhs, 2, projection)?;
    let rhs_re = project_complex_component(rhs, 1, projection)?;
    let rhs_im = project_complex_component(rhs, 2, projection)?;

    if matches!(op, OpBinary::Mul | OpBinary::MulElem) {
        return Ok(if field_idx == 1 {
            complex_sub(complex_mul(lhs_re, rhs_re), complex_mul(lhs_im, rhs_im))
        } else {
            complex_add(complex_mul(lhs_re, rhs_im), complex_mul(lhs_im, rhs_re))
        });
    }

    let denom = complex_add(
        complex_mul(rhs_re.clone(), rhs_re.clone()),
        complex_mul(rhs_im.clone(), rhs_im.clone()),
    );
    if field_idx == 1 {
        Ok(complex_div(
            complex_add(complex_mul(lhs_re, rhs_re), complex_mul(lhs_im, rhs_im)),
            denom,
        ))
    } else {
        Ok(complex_div(
            complex_sub(complex_mul(lhs_im, rhs_re), complex_mul(lhs_re, rhs_im)),
            denom,
        ))
    }
}

fn project_complex_binary(
    expr: &Expression,
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Result<Expression, StructuralError> {
    if matches!(
        op,
        OpBinary::Add | OpBinary::AddElem | OpBinary::Sub | OpBinary::SubElem
    ) {
        return Ok(Expression::Binary {
            op: op.clone(),
            lhs: Box::new(project_complex_component(lhs, field_idx, projection)?),
            rhs: Box::new(project_complex_component(rhs, field_idx, projection)?),
            span: rumoca_core::Span::DUMMY,
        });
    }
    if matches!(
        op,
        OpBinary::Mul | OpBinary::MulElem | OpBinary::Div | OpBinary::DivElem
    ) {
        return project_complex_mul_or_div(op, lhs, rhs, field_idx, projection);
    }
    if field_idx == 1 {
        Ok(expr.clone())
    } else {
        Ok(complex_zero())
    }
}

fn project_constructor_component(
    expr: &Expression,
    name: &Reference,
    args: &[Expression],
    field_idx: usize,
) -> Expression {
    if let Some(arg) = args.get(field_idx.saturating_sub(1)) {
        return arg.clone();
    }
    if field_idx == 2
        && args.len() == 1
        && rumoca_core::qualified_type_name_matches(name.as_str(), "Complex")
    {
        return complex_zero();
    }
    expr.clone()
}

fn project_function_call_component(
    expr: &Expression,
    name: &Reference,
    args: &[Expression],
    is_constructor: bool,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Expression {
    if is_constructor {
        return project_constructor_component(expr, name, args, field_idx);
    }
    if let Some(by_index) = projection.function_output_index_map.get(name.as_str())
        && let Some(projected_output) = by_index.get(&field_idx)
    {
        return Expression::FunctionCall {
            name: rumoca_core::Reference::new(format!("{}.{}", name.as_str(), projected_output)),
            args: args.to_vec(),
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        };
    }
    if field_idx == 1 {
        expr.clone()
    } else {
        complex_zero()
    }
}

fn project_if_component(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Result<Expression, StructuralError> {
    Ok(Expression::If {
        branches: branches
            .iter()
            .map(|(cond, val)| {
                Ok((
                    cond.clone(),
                    project_complex_component(val, field_idx, projection)?,
                ))
            })
            .collect::<Result<Vec<_>, StructuralError>>()?,
        else_branch: Box::new(project_complex_component(
            else_branch,
            field_idx,
            projection,
        )?),
        span: rumoca_core::Span::DUMMY,
    })
}

fn project_array_component(
    expr: &Expression,
    elements: &[Expression],
    is_matrix: bool,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Result<Expression, StructuralError> {
    if elements.len() == 1 {
        return project_complex_component(&elements[0], field_idx, projection);
    }
    if elements.is_empty() {
        return Ok(expr.clone());
    }
    Ok(Expression::Array {
        elements: elements
            .iter()
            .map(|element| project_complex_component(element, field_idx, projection))
            .collect::<Result<Vec<_>, _>>()?,
        is_matrix,
        span: rumoca_core::Span::DUMMY,
    })
}

fn project_complex_component(
    expr: &Expression,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Result<Expression, StructuralError> {
    match expr {
        Expression::Literal { value: _, .. } => {
            if field_idx == 1 {
                Ok(expr.clone())
            } else {
                Ok(complex_zero())
            }
        }
        Expression::VarRef {
            name, subscripts, ..
        } => Ok(project_complex_var_ref(
            name, subscripts, field_idx, projection,
        )),
        Expression::Unary { op, rhs, .. } => {
            project_complex_unary(expr, op, rhs, field_idx, projection)
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            project_complex_binary(expr, op, lhs, rhs, field_idx, projection)
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => project_if_component(branches, else_branch, field_idx, projection),
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } => Ok(project_function_call_component(
            expr,
            name,
            args,
            *is_constructor,
            field_idx,
            projection,
        )),
        Expression::Array {
            elements,
            is_matrix,
            ..
        } => project_array_component(expr, elements, *is_matrix, field_idx, projection),
        _ => projection.project_index(expr, field_idx),
    }
}

pub fn project_rhs_for_scalar_target(
    rhs: &Expression,
    scalar_idx: usize,
    lhs_target: Option<&str>,
    target: Option<&ScalarizedLhsTarget>,
    projection: &ScalarProjectionContext<'_>,
) -> Result<Expression, StructuralError> {
    if let (Some(lhs_name), Some(target)) = (lhs_target, target)
        && (target.array_selector.is_some() || target.field_selector.is_some())
    {
        if let Expression::Binary {
            op,
            lhs,
            rhs: row_rhs,
            ..
        } = rhs
            && matches!(op, OpBinary::Sub)
            && let Expression::VarRef {
                name, subscripts, ..
            } = lhs.as_ref()
            && scalarization_var_ref_name(name, subscripts)
                .as_deref()
                .is_some_and(|lhs_row_name| lhs_row_name == lhs_name)
        {
            let mut projected_rhs = (*row_rhs.clone()).clone();
            if let Some(idx) = target.array_selector {
                projected_rhs = projection.project_index(&projected_rhs, idx)?;
            }
            if let Some(field_idx) = target.field_selector {
                projected_rhs = project_complex_component(&projected_rhs, field_idx, projection)?;
            }
            return Ok(Expression::Binary {
                op: op.clone(),
                lhs: Box::new(target.expr.clone()),
                rhs: Box::new(projected_rhs),
                span: rumoca_core::Span::DUMMY,
            });
        }

        let mut projected = rhs.clone();
        if let Some(idx) = target.array_selector {
            projected = projection.project_index(&projected, idx)?;
        }
        if let Some(field_idx) = target.field_selector {
            projected = project_complex_component(&projected, field_idx, projection)?;
        }
        return Ok(projected);
    }

    projection.project_index(rhs, scalar_idx)
}

pub fn scalarized_equation_lhs(
    eq: &Equation,
    target: Option<&ScalarizedLhsTarget>,
    scalar_idx: usize,
) -> Option<rumoca_core::Reference> {
    let lhs = eq.lhs.as_ref()?;
    if let Some(name) = target {
        return Some(structured_scalar_target(&rumoca_core::VarName::new(
            name.name.clone(),
        )));
    }
    Some(lhs.with_appended_index(scalar_idx as i64))
}

/// Structured reference for a rendered scalar target (the target names come
/// from the scalarized output table, so their subscripts are static).
fn structured_scalar_target(name: &rumoca_core::VarName) -> rumoca_core::Reference {
    match rumoca_core::component_reference_from_flat_name(name, rumoca_core::Span::DUMMY) {
        Some(component_ref) => {
            rumoca_core::Reference::with_component_reference(name.as_str(), component_ref)
        }
        None => rumoca_core::Reference::from_var_name(name.clone()),
    }
}

fn lower_scalar_linear_algebra_exprs(
    exprs: &mut [Expression],
    projection: &ScalarProjectionContext<'_>,
) -> Result<(), StructuralError> {
    for expr in exprs {
        *expr = projection.lower_scalar_linear_algebra(expr)?;
    }
    Ok(())
}

/// Expand array equations (scalar_count > 1) into individual scalar equations.
///
/// After this pass every element of `dae.continuous.equations` has `scalar_count == 1`,
/// which is required by solvers that expect one equation per unknown.
pub fn scalarize_equations(dae: &mut Dae) -> Result<(), StructuralError> {
    let var_dims = build_var_dims_map(dae);
    let structural_values = build_structural_int_map(dae, &var_dims);
    let complex_fields = build_complex_field_map(dae);
    let component_index_map = build_component_index_projection_map(dae);
    let function_output_index_map = build_function_output_projection_map(dae)?;
    let projection = ScalarProjectionContext {
        var_dims: &var_dims,
        structural_values: &structural_values,
        complex_fields: &complex_fields,
        component_index_map: &component_index_map,
        function_output_index_map: &function_output_index_map,
    };
    let scalar_names = build_output_names(dae)?;
    let mut expanded = Vec::new();
    for eq in &dae.continuous.equations {
        let scalarization_target = eq
            .lhs
            .as_ref()
            .map(|lhs| lhs.as_str().to_string())
            .or_else(|| residual_lhs_target_name(&eq.rhs));
        let residual_lhs_targets =
            residual_lhs_scalar_targets(&eq.rhs, eq.span, &var_dims, &structural_values)?;
        let has_residual_lhs_targets = !residual_lhs_targets.is_empty();
        let lhs_targets = if has_residual_lhs_targets {
            residual_lhs_targets
        } else {
            scalarization_target
                .as_deref()
                .map(|lhs| scalar_targets_for_lhs(lhs, eq.span, &scalar_names, &var_dims))
                .transpose()?
                .unwrap_or_default()
        };
        let rhs_shape_count = shape_scalar_count(projection.expression_shape(&eq.rhs));
        let scalar_count = if has_residual_lhs_targets {
            lhs_targets.len().max(1)
        } else if let Some(rhs_count) = rhs_shape_count.filter(|count| *count > 1) {
            // MLS §10.6 / SPEC_0019: array equations represent one scalar
            // equation per array element. Prefer the expression IR shape over
            // stale scalar_count metadata for residuals such as
            // `J * der(omega) - M_body`.
            rhs_count.max(lhs_targets.len())
        } else {
            eq.scalar_count.max(lhs_targets.len()).max(1)
        };
        if scalar_count <= 1 {
            let mut lowered = eq.clone();
            let rhs = if projection
                .expression_shape(&lowered.rhs)
                .is_singleton_array()
            {
                projection.project_index(&lowered.rhs, 1)?
            } else {
                lowered.rhs.clone()
            };
            lowered.rhs = projection.lower_scalar_linear_algebra(&rhs)?;
            expanded.push(lowered);
        } else {
            for i in 1..=scalar_count {
                let target = lhs_targets.get(i - 1);
                expanded.push(Equation {
                    lhs: scalarized_equation_lhs(eq, target, i),
                    rhs: project_rhs_for_scalar_target(
                        &eq.rhs,
                        i,
                        scalarization_target.as_deref(),
                        target,
                        &projection,
                    )?,
                    span: eq.span,
                    origin: eq.origin.clone(),
                    scalar_count: 1,
                });
            }
        }
    }
    dae.continuous.equations = expanded;

    lower_scalar_linear_algebra_exprs(&mut dae.conditions.relations, &projection)?;
    lower_scalar_linear_algebra_exprs(&mut dae.events.synthetic_root_conditions, &projection)?;
    lower_scalar_linear_algebra_exprs(&mut dae.clocks.triggered_conditions, &projection)?;
    lower_scalar_linear_algebra_exprs(&mut dae.clocks.constructor_exprs, &projection)?;
    Ok(())
}

#[cfg(test)]
mod tests;
