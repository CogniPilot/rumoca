use super::super::random_programs::{Stream, random_defined_program};
use super::*;

/// `program` with every load index moved by its own random offset.
fn shifted(stream: &mut Stream, program: &[LinearOp]) -> Vec<LinearOp> {
    let mut copy = program.to_vec();
    for op in &mut copy {
        if let LinearOp::LoadY { index, .. } | LinearOp::LoadP { index, .. } = op {
            *index += 1 + stream.next(50);
        }
    }
    copy
}

fn loads(program: &[LinearOp]) -> usize {
    program
        .iter()
        .filter(|op| matches!(op, LinearOp::LoadY { .. } | LinearOp::LoadP { .. }))
        .count()
}

/// Shifted copies of a random program form one class with it, and the
/// representative with each member's indices put back is that member.
#[test]
fn shifted_copies_share_and_rebuild_every_member() {
    let mut stream = Stream(0x1f83_d9ab_fb41_bd6b);
    let mut shared = 0;
    for _ in 0..1000 {
        let program = random_defined_program(&mut stream);
        let copies = 1 + stream.next(4);
        let mut programs = vec![program.clone()];
        for _ in 0..copies {
            programs.push(shifted(&mut stream, &program));
        }
        let classes = load_index_classes(&programs);
        if loads(&program) == 0 {
            // Equal programs without loads share too.
            assert_eq!(classes.len(), 1, "{program:?}");
        }
        assert_eq!(classes.len(), 1, "{programs:?}");
        let class = &classes[0];
        assert_eq!(class.members, (0..programs.len()).collect::<Vec<_>>());
        let (shape, _) = index_shape(&programs[class.members[0]]).expect("modelled");
        for (&member, indices) in class.members.iter().zip(&class.indices) {
            assert_eq!(indices.len(), loads(&programs[member]));
            let rebuilt = instantiate(&shape, indices).expect("indices fit");
            assert!(same_ops(&rebuilt, &programs[member]));
        }
        shared += class.members.len();
    }
    assert!(shared > 2000, "{shared}");
}

fn row(constant: f64, register: Reg, index: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index },
        LinearOp::Const {
            dst: register,
            value: constant,
        },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: register,
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

/// Rows that differ in a constant, even only in its sign bit, or in a
/// register, are different classes; rows that differ in their load index
/// alone share.
#[test]
fn rows_differing_in_a_constant_or_a_register_do_not_share() {
    let programs = vec![
        row(0.5, 1, 0),
        row(0.5, 1, 7),
        row(0.25, 1, 3),
        row(0.0, 1, 4),
        row(-0.0, 1, 5),
        row(0.5, 3, 6),
    ];
    let classes = load_index_classes(&programs);
    assert_eq!(
        classes,
        vec![LoadIndexClass {
            members: vec![0, 1],
            indices: vec![vec![0], vec![7]],
        }]
    );
}

/// A program holding an operation the classes do not model takes part in
/// none.
#[test]
fn an_unmodelled_operation_shares_nothing() {
    let mut program = row(0.5, 1, 0);
    program.insert(
        0,
        LinearOp::TensorIdentity {
            dst_start: 10,
            size: 1,
            lanes: 1,
        },
    );
    assert!(load_index_classes(&[program.clone(), program]).is_empty());
}
