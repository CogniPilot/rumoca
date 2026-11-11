//! Only an alias row that selects one fixed binder point keeps scalar row
//! owners; every other binder conditional keeps its compact owner.

use super::*;
use crate::construction::analysis::loop_compaction::preservation_corpus::{
    binary, element, integer, real, span, var,
};

fn domain() -> rumoca_core::StructuredIndexDomain {
    rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "j".to_string(),
            lower: 1,
            upper: 3,
            step: 1,
        }],
    }
}

fn select(condition: Expression, selected: Expression, otherwise: Expression) -> Expression {
    Expression::If {
        branches: vec![(condition, selected)],
        else_branch: Box::new(otherwise),
        span: span(),
    }
}

/// `k` is a parameter; `z` changes at runtime.
fn selects_alias_row(residual: &Expression) -> bool {
    let domain = domain();
    let fixed = |expression: &Expression| {
        let mut reads = Vec::new();
        expression.collect_var_refs(&mut reads);
        reads.iter().all(|name| name.as_str() == "k")
    };
    PointSelection {
        domain: &domain,
        fixed: &fixed,
    }
    .selects_alias_row(residual)
}

fn residual(lhs: Expression, rhs: Expression) -> Expression {
    binary(OpBinary::Sub, lhs, rhs)
}

#[test]
fn a_fixed_point_selection_alias_row_keeps_scalar_rows() {
    let plug = residual(
        element("plug_i", var("j")),
        select(
            binary(OpBinary::Eq, var("j"), var("k")),
            var("pin_i"),
            real(0.0),
        ),
    );
    assert!(selects_alias_row(&plug));
    let reversed = residual(
        select(
            binary(OpBinary::Neq, var("k"), var("j")),
            integer(0),
            var("pin_i"),
        ),
        element("plug_i", var("j")),
    );
    assert!(selects_alias_row(&reversed));
}

#[test]
fn other_binder_conditionals_keep_their_compact_owner() {
    let point = |selector: Expression, selected: Expression, otherwise: Expression| {
        select(
            binary(OpBinary::Eq, var("j"), selector),
            selected,
            otherwise,
        )
    };
    let boundary_arm = residual(
        element("x", var("j")),
        point(
            integer(1),
            real(0.0),
            binary(
                OpBinary::Sub,
                element("x", binary(OpBinary::Sub, var("j"), integer(1))),
                element("x", var("j")),
            ),
        ),
    );
    assert!(!selects_alias_row(&boundary_arm));
    let boundary_term = residual(
        element("dx", var("j")),
        binary(
            OpBinary::Add,
            element("x", var("j")),
            point(integer(1), var("u"), real(0.0)),
        ),
    );
    assert!(!selects_alias_row(&boundary_term));
    let runtime_selector = residual(element("y", var("j")), point(var("z"), var("w"), real(0.0)));
    assert!(!selects_alias_row(&runtime_selector));
    let range = residual(
        element("y", var("j")),
        select(
            binary(OpBinary::Gt, var("j"), integer(1)),
            var("w"),
            real(0.0),
        ),
    );
    assert!(!selects_alias_row(&range));
}
