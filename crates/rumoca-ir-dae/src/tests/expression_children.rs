use rumoca_core::{DefId, SourceId, Span};

use crate::expression::{
    Coordinate, ExprNode, ExpressionArenaStorage, OperandRange, PackedSubscript,
    PackedSubscriptKind,
};
use crate::{BinaryOperator, DaeLiteral, DaeProvenance, PureBuiltin, UnaryOperator};

#[test]
// SPEC_0021: Exception - exhaustive explicit coverage of every raw ExprNode variant.
#[allow(clippy::too_many_lines)]
fn raw_children_cover_every_variant_in_syntactic_order() {
    let provenance = DaeProvenance::source(Span::from_offsets(
        SourceId::from_source_name("raw_children.mo"),
        0,
        1,
    ))
    .unwrap();
    let operands = OperandRange { start: 0, len: 2 };
    let subscripts = OperandRange { start: 0, len: 3 };
    let expressions = ExpressionArenaStorage {
        operands: vec![20, 21],
        subscripts: vec![
            PackedSubscript {
                kind: PackedSubscriptKind::Index(22),
                provenance,
            },
            PackedSubscript {
                kind: PackedSubscriptKind::Whole,
                provenance,
            },
            PackedSubscript {
                kind: PackedSubscriptKind::Slice(23),
                provenance,
            },
        ],
        ..ExpressionArenaStorage::default()
    };
    let cases = vec![
        (ExprNode::Literal(DaeLiteral::Integer(1)), vec![]),
        (ExprNode::Coordinate(Coordinate::Time), vec![]),
        (
            ExprNode::Unary {
                operator: UnaryOperator::Plus,
                operand: 1,
            },
            vec![1],
        ),
        (
            ExprNode::Binary {
                operator: BinaryOperator::Add,
                lhs: 1,
                rhs: 2,
            },
            vec![1, 2],
        ),
        (ExprNode::Conditional { operands }, vec![20, 21]),
        (ExprNode::Array { operands }, vec![20, 21]),
        (ExprNode::Record { operands }, vec![20, 21]),
        (ExprNode::Field { base: 1, field: 0 }, vec![1]),
        (
            ExprNode::Range {
                start: 1,
                explicit_step: Some(2),
                stop: 3,
            },
            vec![1, 2, 3],
        ),
        (ExprNode::Comprehension { domain: 0, body: 1 }, vec![1]),
        (
            ExprNode::Index {
                base: 1,
                subscripts,
            },
            vec![1, 22, 23],
        ),
        (
            ExprNode::ArrayUpdate {
                base: 1,
                value: 2,
                subscripts,
            },
            vec![1, 2, 22, 23],
        ),
        (
            ExprNode::Builtin {
                builtin: PureBuiltin::Abs,
                operands,
            },
            vec![20, 21],
        ),
        (
            ExprNode::Call {
                function: 0,
                output: 0,
                operands,
            },
            vec![20, 21],
        ),
        (
            ExprNode::StringConversion {
                declaration: DefId::new(1),
                value: 1,
                minimum_length: Some(2),
                left_justified: Some(3),
                significant_digits: Some(4),
                format: Some(5),
            },
            vec![1, 2, 3, 4, 5],
        ),
        (
            ExprNode::FunctionValue {
                function: 0,
                value: 0,
                definition_ordinal: 0,
            },
            vec![],
        ),
        (
            ExprNode::FunctionFoldParameter {
                function: 0,
                fold: 0,
                carried: 0,
                definition_ordinal: 0,
            },
            vec![],
        ),
        (
            ExprNode::FunctionFoldOutput {
                function: 0,
                fold: 0,
                carried: 0,
                definition_ordinal: 0,
            },
            vec![],
        ),
    ];

    for (node, expected) in cases {
        let mut actual = Vec::new();
        node.for_each_child(&expressions, |child| actual.push(child));
        assert_eq!(actual, expected, "unexpected children for {node:?}");
    }
}
