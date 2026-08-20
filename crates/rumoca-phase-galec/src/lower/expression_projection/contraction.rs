//! Tensor-contraction shape and scalar-index projection helpers.

use super::*;

pub(super) struct TensorContraction {
    pub(super) extent: u32,
    pub(super) lhs_outer: Vec<gast::Expression>,
    pub(super) rhs_outer: Vec<gast::Expression>,
    pub(super) lhs_matrix: bool,
    pub(super) rhs_matrix: bool,
}

pub(super) fn tensor_contraction(
    lhs_dimensions: &[u32],
    rhs_dimensions: &[u32],
    indices: &[gast::Expression],
) -> Option<TensorContraction> {
    match (lhs_dimensions, rhs_dimensions, indices) {
        ([_rows, inner], [rhs_inner], [row]) if inner == rhs_inner => Some(TensorContraction {
            extent: *inner,
            lhs_outer: vec![row.clone()],
            rhs_outer: Vec::new(),
            lhs_matrix: true,
            rhs_matrix: false,
        }),
        ([inner], [rhs_inner, _columns], [column]) if inner == rhs_inner => {
            Some(TensorContraction {
                extent: *inner,
                lhs_outer: Vec::new(),
                rhs_outer: vec![column.clone()],
                lhs_matrix: false,
                rhs_matrix: true,
            })
        }
        ([_rows, inner], [rhs_inner, _columns], [row, column]) if inner == rhs_inner => {
            Some(TensorContraction {
                extent: *inner,
                lhs_outer: vec![row.clone()],
                rhs_outer: vec![column.clone()],
                lhs_matrix: true,
                rhs_matrix: true,
            })
        }
        _ => None,
    }
}

pub(super) fn contraction_indices(
    contraction: &TensorContraction,
    contracted: gast::Expression,
) -> (Vec<gast::Expression>, Vec<gast::Expression>) {
    let lhs = if contraction.lhs_matrix {
        vec![contraction.lhs_outer[0].clone(), contracted.clone()]
    } else {
        vec![contracted.clone()]
    };
    let rhs = if contraction.rhs_matrix {
        let outer = contraction
            .rhs_outer
            .first()
            .expect("matrix contraction has a result column");
        vec![contracted, outer.clone()]
    } else {
        vec![contracted]
    };
    (lhs, rhs)
}

pub(super) fn sum_terms(
    terms: Vec<gast::Expression>,
    feature: &str,
    detail: &str,
    span: Span,
) -> Result<gast::Expression, GalecTargetError> {
    terms
        .into_iter()
        .reduce(|lhs, rhs| gast::Expression::binary(gast::BinaryOp::Add, lhs, rhs))
        .ok_or_else(|| unsupported(feature, detail.to_owned(), span))
}
