//! Coverage for the compact-family expansion Phase B depends on.
//!
//! Before this expansion existed, `eliminate_via_blt` matched
//! `BltBlock::StructuredScalar` with an empty arm and a comment claiming the
//! variant could never reach it. It can: any DAE whose rows are all
//! `scalar_count == 1` takes the `uses_scalar_view == false` branch and gets its
//! blocks handed back, structured families included. Dropping the arm's blocks
//! silently left the family's rows in the DAE, so these tests pin that the rows
//! survive as scalar blocks instead.

use super::*;
use rumoca_ir_dae as dae;

use crate::incidence::corners::tests::test_span;

fn named(name: &str) -> UnknownId {
    UnknownId::Variable(rumoca_core::VarName::from(name))
}

/// A 1-D family covering rows `first..first + cells`, cell `p` solving unknown
/// `first + p`.
fn family_block(first: usize, cells: usize) -> StructuredScalarBlock {
    StructuredScalarBlock {
        span: test_span(),
        first_equation_index: first,
        equations_per_point: 1,
        point_count: cells,
        extents: vec![cells],
        cell_strides: vec![1],
        base_unknowns: vec![first],
        unknown_steps: vec![vec![1]],
    }
}

fn matching(rows: usize) -> Vec<(EquationRef, UnknownId)> {
    (0..rows)
        .map(|row| (EquationRef(row), named(&format!("v{row}"))))
        .collect()
}

#[test]
fn a_compact_family_becomes_one_scalar_block_per_row_in_domain_order() {
    const CELLS: usize = 5;
    let blocks = vec![
        BltBlock::Scalar {
            equation: EquationRef(0),
            unknown: named("v0"),
        },
        BltBlock::StructuredScalar(family_block(1, CELLS)),
    ];

    let expanded = expand_compact_family_blocks(blocks, &matching(CELLS + 1))
        .expect("the family's rows are all in the matching");

    let rows: Vec<(usize, String)> = expanded
        .iter()
        .filter_map(|block| match block {
            BltBlock::Scalar { equation, unknown } => Some((equation.0, unknown.to_string())),
            _ => None,
        })
        .collect();
    assert_eq!(
        rows.len(),
        expanded.len(),
        "every emitted block must be scalar, got {expanded:?}"
    );
    let reference: Vec<(usize, String)> = (0..=CELLS).map(|row| (row, format!("v{row}"))).collect();
    assert_eq!(rows, reference, "rows keep their domain order");
}

/// The block list is handed back untouched — same allocation, no map lookup
/// built — when no family block is present, which is every array-free model.
#[test]
fn a_block_list_without_families_is_returned_unchanged() {
    let blocks = vec![
        BltBlock::Scalar {
            equation: EquationRef(0),
            unknown: named("v0"),
        },
        BltBlock::AlgebraicLoop {
            equations: vec![EquationRef(1), EquationRef(2)],
            unknowns: vec![named("v1"), named("v2")],
        },
    ];

    let expanded =
        expand_compact_family_blocks(blocks, &[]).expect("no family means nothing to resolve");

    assert_eq!(expanded.len(), 2);
    assert!(matches!(expanded[1], BltBlock::AlgebraicLoop { .. }));
}

/// A family row missing from the matching is a spanned contract violation, not
/// a dropped row: dropping it would silently shrink the elimination.
#[test]
fn a_family_row_absent_from_the_matching_is_reported() {
    let blocks = vec![BltBlock::StructuredScalar(family_block(0, 4))];

    let err = expand_compact_family_blocks(blocks, &matching(2))
        .expect_err("rows 2 and 3 have no matched unknown");

    assert!(matches!(err, StructuralError::ContractViolation { .. }));
    assert_eq!(err.source_span(), Some(test_span()));
}

/// A DAE of `cells` states, one ODE row each, carrying one non-materialized
/// regular family over the rows.
///
/// Every equation is `scalar_count == 1`, so `prepare_blt_elimination` takes the
/// `uses_scalar_view == false` branch and hands its blocks back — the branch in
/// which a compact family block reaches Phase B.
fn whole_array_ode_dae(cells: usize) -> dae::Dae {
    let mut system = dae::Dae::new();
    for index in 0..cells {
        let name = rumoca_core::VarName::new(format!("x{index}").as_str());
        system
            .variables
            .states
            .insert(name.clone(), dae::Variable::new(name, test_span()));
    }
    for index in 0..cells {
        let body = rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Der,
                args: vec![state_ref(index)],
                span: test_span(),
            }),
            rhs: Box::new(state_ref(index)),
            span: test_span(),
        };
        system.continuous.equations.push(dae::Equation {
            lhs: None,
            rhs: body,
            span: test_span(),
            origin: format!("whole_array_ode[{index}]"),
            scalar_count: 1,
        });
    }
    let mut family = crate::incidence::corners::tests::one_dim_family(
        i64::try_from(cells).expect("cell count fits i64"),
    );
    family.regular = Some(rumoca_core::RegularForFamily {
        binders: vec!["i".to_string()],
        accesses: Vec::new(),
    });
    family.template = Some(rumoca_core::ComprehensionTemplate {
        body: vec![system.continuous.equations[0].rhs.clone()],
        scalar_view: rumoca_core::ComprehensionScalarView::RowMajorProjection,
    });
    family.interiors_materialized = false;
    system.continuous.structured_equations = vec![family];
    system
}

fn state_ref(index: usize) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(format!("x{index}").as_str()),
        subscripts: vec![],
        span: test_span(),
    }
}

/// End-to-end pin for the arm that used to be a silent no-op: the sort really
/// does produce a compact family block for this DAE, and preparation really
/// does hand Phase B one scalar block per row instead.
#[test]
fn blt_preparation_never_hands_a_compact_family_block_to_elimination() {
    const CELLS: usize = 16;
    let dae = whole_array_ode_dae(CELLS);

    // The sort compacts, so the arm under test is genuinely live.
    let sorted = crate::sort_dae(&dae).expect("the whole-array ODE is structurally regular");
    assert_eq!(
        sorted.blocks.len(),
        1,
        "the fixture must reach BLT as one compact family block"
    );
    assert!(matches!(
        sorted.blocks[0],
        BltBlock::StructuredScalar { .. }
    ));

    let prepared = crate::eliminate::prepare_blt_elimination(&dae, false, false)
        .expect("preparing a regular scalar system succeeds");
    let blocks = prepared
        .blocks
        .expect("an all-scalar system hands its blocks back");

    assert_eq!(blocks.len(), CELLS, "one scalar block per family row");
    assert!(
        blocks
            .iter()
            .all(|block| matches!(block, BltBlock::Scalar { .. })),
        "no compact block may survive into Phase B, got {blocks:?}"
    );
}

/// And the guard behind it: handing `eliminate_via_blt` a compact block anyway
/// is a spanned contract violation, not a skipped family.
#[test]
fn blt_elimination_rejects_an_unexpanded_compact_family_block() {
    let mut dae = whole_array_ode_dae(4);
    let blocks = vec![BltBlock::StructuredScalar(family_block(0, 4))];

    let err = crate::eliminate::eliminate_via_blt(&mut dae, &blocks, &[])
        .expect_err("an unexpanded compact block must be reported");

    assert!(matches!(err, StructuralError::ContractViolation { .. }));
    assert_eq!(err.source_span(), Some(test_span()));
}
