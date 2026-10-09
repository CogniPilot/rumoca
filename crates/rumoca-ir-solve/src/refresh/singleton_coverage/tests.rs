use super::*;
use crate::{AlgebraicProjectionBlock, AlgebraicProjectionPlan};

fn row(equation: usize, target: usize, owner: usize) -> AlgebraicRefreshRow {
    let mut row = super::super::tests::row(target);
    row.equation_index = equation;
    row.owner_id = RefreshRowOwnerId::checked(owner).unwrap();
    row
}

fn block(equation: usize, target: usize) -> AlgebraicProjectionBlock {
    AlgebraicProjectionBlock {
        rows: vec![equation],
        y_indices: vec![target],
        tearing: None,
        alternate_charts: Vec::new(),
    }
}

// The relation must be a bijection between singleton blocks and row owners.
fn relation_oracle(plan: &RefreshPlan) -> Option<BTreeSet<RefreshRowOwnerId>> {
    if plan.rows.len() != plan.simultaneous_plan.blocks.len() {
        return None;
    }
    let owners = plan
        .rows
        .iter()
        .map(|row| row.owner_id)
        .collect::<BTreeSet<_>>();
    if owners.len() != plan.rows.len() {
        return None;
    }
    let mut matched = vec![0usize; plan.rows.len()];
    for block in &plan.simultaneous_plan.blocks {
        let ([equation], [target]) = (block.rows.as_slice(), block.y_indices.as_slice()) else {
            return None;
        };
        let candidates = plan
            .rows
            .iter()
            .enumerate()
            .filter(|(_, row)| (row.equation_index, row.target_index) == (*equation, *target))
            .map(|(position, _)| position)
            .collect::<Vec<_>>();
        let [position] = candidates.as_slice() else {
            return None;
        };
        matched[*position] += 1;
    }
    matched.iter().all(|&count| count == 1).then_some(owners)
}

#[test]
fn singleton_index_matches_bijection_oracle_for_all_small_row_and_block_inventories() {
    for row_code in 0..64 {
        for block_code in 0..125 {
            let plan = small_plan(row_code, block_code);
            assert_eq!(exact_singleton_row_owners(&plan), relation_oracle(&plan));
        }
    }
}

fn small_plan(mut row_code: usize, mut block_code: usize) -> RefreshPlan {
    let mut rows = Vec::new();
    let mut blocks = Vec::new();
    for _ in 0..3 {
        match row_code % 4 {
            1 => rows.push(row(0, 0, 0)),
            2 => rows.push(row(1, 1, 1)),
            3 => rows.push(row(0, 1, 1)),
            _ => {}
        }
        match block_code % 5 {
            1 => blocks.push(block(0, 0)),
            2 => blocks.push(block(1, 1)),
            3 => blocks.push(block(0, 1)),
            4 => {
                let mut invalid = block(0, 0);
                invalid.rows.push(1);
                blocks.push(invalid);
            }
            _ => {}
        }
        row_code /= 4;
        block_code /= 5;
    }
    RefreshPlan {
        rows,
        simultaneous_plan: AlgebraicProjectionPlan { blocks },
        ..Default::default()
    }
}

#[test]
fn singleton_index_preserves_duplicate_pair_owner_and_missing_coordinate_refusals() {
    let mut plan = RefreshPlan {
        rows: vec![row(0, 0, 0), row(1, 1, 1)],
        simultaneous_plan: AlgebraicProjectionPlan {
            blocks: vec![block(1, 1), block(0, 0)],
        },
        ..Default::default()
    };
    assert!(exact_singleton_row_owners(&plan).is_some());
    plan.rows.reverse();
    assert!(exact_singleton_row_owners(&plan).is_some());
    plan.rows[1] = row(1, 1, 0);
    assert_eq!(exact_singleton_row_owners(&plan), None);
    plan.rows[1] = row(0, 0, 1);
    assert_eq!(exact_singleton_row_owners(&plan), None);
    plan.rows[0].owner_id = RefreshRowOwnerId::checked(1).unwrap();
    assert_eq!(exact_singleton_row_owners(&plan), None);
    plan.rows[0] = row(usize::MAX, 1, 1);
    assert_eq!(exact_singleton_row_owners(&plan), None);
}

#[test]
fn large_reversed_singleton_catalog_uses_one_borrowed_index_without_row_clones() {
    let count = 20_000;
    let plan = RefreshPlan {
        rows: (0..count)
            .rev()
            .map(|index| row(index, index, index))
            .collect(),
        simultaneous_plan: AlgebraicProjectionPlan {
            blocks: (0..count).map(|index| block(index, index)).collect(),
        },
        ..Default::default()
    };
    let index = SingletonRows::new(&plan.rows).unwrap();
    assert_eq!(index.0.len(), count);
    for row in &plan.rows {
        let issued = index.0[row.equation_index];
        assert!(std::ptr::eq(issued, row));
        assert_eq!(
            index.unique_owner(row.equation_index, row.target_index),
            Some(row.owner_id)
        );
    }
    assert_eq!(exact_singleton_row_owners(&plan).unwrap().len(), count);
}
