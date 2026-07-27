//! Linear-algebra semantics for `*` on array operands (dot, matrix-vector,
//! vector-matrix, and matrix-matrix products).

use rumoca_core::Span;

use super::errors::EvalError;
use super::operators::eval_add;
use super::value::Value;

enum ArrayShape<'a> {
    Vector(&'a [Value]),
    Matrix(Vec<&'a [Value]>),
}

pub(super) fn eval_matrix_mul(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    let lhs_shape = classify_array_shape(lhs, span)?;
    let rhs_shape = classify_array_shape(rhs, span)?;
    match (lhs_shape, rhs_shape) {
        // Modelica vector * vector is dot-product.
        (ArrayShape::Vector(lhs_vec), ArrayShape::Vector(rhs_vec)) => {
            eval_dot_product(lhs_vec, rhs_vec, span)
        }
        // Matrix * vector => vector.
        (ArrayShape::Matrix(lhs_rows), ArrayShape::Vector(rhs_vec)) => {
            eval_matrix_vector_mul(&lhs_rows, rhs_vec, span)
        }
        // Vector * matrix => vector.
        (ArrayShape::Vector(lhs_vec), ArrayShape::Matrix(rhs_rows)) => {
            eval_vector_matrix_mul(lhs_vec, &rhs_rows, span)
        }
        // Matrix * matrix => matrix.
        (ArrayShape::Matrix(lhs_rows), ArrayShape::Matrix(rhs_rows)) => {
            eval_matrix_matrix_mul(&lhs_rows, &rhs_rows, span)
        }
    }
}

fn classify_array_shape<'a>(value: &'a Value, span: Span) -> Result<ArrayShape<'a>, EvalError> {
    let Value::Array(elements) = value else {
        return Err(EvalError::type_mismatch("Array", value.type_name(), span));
    };

    if elements.iter().all(|item| !matches!(item, Value::Array(_))) {
        return Ok(ArrayShape::Vector(elements.as_slice()));
    }

    if elements.iter().any(|item| !matches!(item, Value::Array(_))) {
        return Err(EvalError::function_error(
            "mixed-rank arrays are not valid matrix operands".to_string(),
            span,
        ));
    }

    let mut rows = Vec::with_capacity(elements.len());
    let mut n_cols: Option<usize> = None;
    for row in elements {
        let Value::Array(row_elems) = row else {
            return Err(EvalError::function_error(
                "mixed-rank arrays are not valid matrix operands".to_string(),
                span,
            ));
        };
        if row_elems.iter().any(|item| matches!(item, Value::Array(_))) {
            return Err(EvalError::function_error(
                "rank > 2 arrays are not supported in matrix multiplication".to_string(),
                span,
            ));
        }
        match n_cols {
            Some(expected) if expected != row_elems.len() => {
                return Err(EvalError::function_error(
                    "matrix rows must have the same length".to_string(),
                    span,
                ));
            }
            None => {
                n_cols = Some(row_elems.len());
            }
            _ => {}
        }
        rows.push(row_elems.as_slice());
    }

    Ok(ArrayShape::Matrix(rows))
}

fn eval_dot_product(lhs_vec: &[Value], rhs_vec: &[Value], span: Span) -> Result<Value, EvalError> {
    if lhs_vec.len() != rhs_vec.len() {
        return Err(EvalError::function_error(
            format!(
                "vector dot-product size mismatch: {} vs {}",
                lhs_vec.len(),
                rhs_vec.len()
            ),
            span,
        ));
    }

    let mut acc = Value::Integer(0);
    for (lhs, rhs) in lhs_vec.iter().zip(rhs_vec.iter()) {
        let product = eval_numeric_mul(lhs, rhs, span)?;
        acc = eval_add(&acc, &product, span)?;
    }
    Ok(acc)
}

fn eval_matrix_vector_mul(
    lhs_rows: &[&[Value]],
    rhs_vec: &[Value],
    span: Span,
) -> Result<Value, EvalError> {
    let lhs_cols = lhs_rows.first().map_or(0, |row| row.len());
    if lhs_cols != rhs_vec.len() {
        return Err(EvalError::function_error(
            format!(
                "matrix-vector size mismatch: left cols {} vs right size {}",
                lhs_cols,
                rhs_vec.len()
            ),
            span,
        ));
    }

    let mut result = Vec::with_capacity(lhs_rows.len());
    for row in lhs_rows {
        result.push(eval_dot_product(row, rhs_vec, span)?);
    }
    Ok(Value::Array(result))
}

fn eval_vector_matrix_mul(
    lhs_vec: &[Value],
    rhs_rows: &[&[Value]],
    span: Span,
) -> Result<Value, EvalError> {
    if rhs_rows.is_empty() {
        return Ok(Value::Array(Vec::new()));
    }

    if lhs_vec.len() != rhs_rows.len() {
        return Err(EvalError::function_error(
            format!(
                "vector-matrix size mismatch: left size {} vs right rows {}",
                lhs_vec.len(),
                rhs_rows.len()
            ),
            span,
        ));
    }

    let rhs_cols = rhs_rows[0].len();
    let mut out = Vec::with_capacity(rhs_cols);
    for (col, _) in rhs_rows[0].iter().enumerate() {
        let mut acc = Value::Integer(0);
        for (lhs_val, rhs_row) in lhs_vec.iter().zip(rhs_rows.iter()) {
            let product = eval_numeric_mul(lhs_val, &rhs_row[col], span)?;
            acc = eval_add(&acc, &product, span)?;
        }
        out.push(acc);
    }
    Ok(Value::Array(out))
}

fn eval_matrix_matrix_mul(
    lhs_rows: &[&[Value]],
    rhs_rows: &[&[Value]],
    span: Span,
) -> Result<Value, EvalError> {
    if rhs_rows.is_empty() {
        return Ok(Value::Array(Vec::new()));
    }

    let lhs_cols = lhs_rows.first().map_or(0, |row| row.len());
    let rhs_rows_count = rhs_rows.len();
    let rhs_cols = rhs_rows[0].len();

    if lhs_cols != rhs_rows_count {
        return Err(EvalError::function_error(
            format!(
                "matrix-matrix size mismatch: left cols {} vs right rows {}",
                lhs_cols, rhs_rows_count
            ),
            span,
        ));
    }

    let mut result_rows = Vec::with_capacity(lhs_rows.len());
    for lhs_row in lhs_rows {
        let mut out_row = Vec::with_capacity(rhs_cols);
        for (col, _) in rhs_rows[0].iter().enumerate() {
            let mut acc = Value::Integer(0);
            for (lhs_val, rhs_row) in lhs_row.iter().zip(rhs_rows.iter()) {
                let product = eval_numeric_mul(lhs_val, &rhs_row[col], span)?;
                acc = eval_add(&acc, &product, span)?;
            }
            out_row.push(acc);
        }
        result_rows.push(Value::Array(out_row));
    }
    Ok(Value::Array(result_rows))
}

fn eval_numeric_mul(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a * b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 * b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a * *b as f64)),
        _ => Err(EvalError::type_mismatch(
            "numeric scalar",
            format!("{} * {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}
