use super::*;
use crate::TensorInputKind;

fn compare(source: &[LinearOp], requests: &[(usize, Reg)]) {
    let mut queries = ScalarProgramYDependencyQueries::new(source);
    for &(len, register) in requests {
        let expected = source
            .get(..len)
            .and_then(|prefix| ScalarProgramYDependency::new(prefix).footprint([register]));
        assert_eq!(
            queries.footprint(len, [register]),
            expected,
            "{len}/{register}"
        );
    }
}

#[test]
fn many_store_prefixes_of_seven_million_registers_share_one_inventory() {
    let mut source = vec![LinearOp::TensorLoad {
        dst_start: 0,
        input: TensorInputKind::Y,
        input_start: 100,
        count: 7_000_000,
        seed_start: None,
        lanes: 1,
    }];
    source.extend((0..32).map(|offset| LinearOp::StoreOutputRange {
        start: offset * 14400,
        count: 14400,
        stride: 1,
    }));
    let mut queries = ScalarProgramYDependencyQueries::new(&source);
    for len in 1..source.len() {
        let register = ((len - 1) * 14400) as Reg;
        assert_eq!(
            queries.footprint(len, [register]),
            Some(IndexIntervals::singleton(register as usize + 100))
        );
    }
    assert_eq!(queries.complete_builds, 1);
    assert_eq!(queries.prefix_builds, 0);
    assert!(queries.prefix.is_none());
}

#[test]
fn nonmonotone_unique_ranges_do_not_expose_future_registers() {
    let source = [
        LinearOp::LoadY { dst: 10, index: 5 },
        LinearOp::StoreOutput { src: 10 },
        LinearOp::TensorLoad {
            dst_start: 2,
            input: TensorInputKind::Y,
            input_start: 7,
            count: 3,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 2,
            count: 3,
            stride: 1,
        },
    ];
    compare(
        &source,
        &[
            (1, 10),
            (1, 2),
            (3, 2),
            (3, 4),
            (1, 4),
            (0, 10),
            (3, 7),
            (5, 2),
        ],
    );
    let mut queries = ScalarProgramYDependencyQueries::new(&source);
    assert_eq!(queries.footprint(1, [2]), None);
    assert_eq!(
        queries.footprint(3, [2]),
        Some(IndexIntervals::singleton(7))
    );
    assert_eq!(queries.complete_builds, 1);
    assert_eq!(queries.prefix_builds, 0);
}

#[test]
fn overwrites_and_refused_suffixes_keep_exact_prefix_answers() {
    for suffix in [
        LinearOp::Const { dst: 0, value: 4.0 },
        LinearOp::LoadFoldCarried { dst: 1, index: 0 },
        LinearOp::Move { dst: 1, src: 99 },
    ] {
        let source = [
            LinearOp::LoadY { dst: 0, index: 5 },
            LinearOp::StoreOutput { src: 0 },
            suffix,
            LinearOp::StoreOutput { src: 0 },
        ];
        compare(&source, &[(1, 0), (3, 0), (1, 0), (2, 0), (3, 1)]);
        let mut queries = ScalarProgramYDependencyQueries::new(&source);
        assert_eq!(
            queries.footprint(1, [0]),
            Some(IndexIntervals::singleton(5))
        );
        queries.footprint(3, [0]);
        assert!(queries.complete.get().unwrap().is_none());
        assert_eq!(queries.complete_builds, 1);
        assert_eq!(queries.prefix_builds, 2);
        assert_eq!(queries.prefix.as_ref().unwrap().0, 3);
    }
}

#[test]
fn register_only_walk_checks_huge_zero_stride_outputs_without_retaining_them() {
    for register in [0, 1] {
        let source = [
            LinearOp::LoadY { dst: 0, index: 5 },
            LinearOp::StoreOutputRange {
                start: register,
                count: usize::MAX,
                stride: 0,
            },
        ];
        let result = ScalarProgramYDependency::complete(&source);
        assert_eq!(result.is_some(), register == 0);
        let mut queries = ScalarProgramYDependencyQueries::new(&source);
        assert_eq!(
            queries.footprint(1, [0]),
            Some(IndexIntervals::singleton(5))
        );
        assert_eq!(queries.complete.get().unwrap().is_some(), register == 0);
    }
}
