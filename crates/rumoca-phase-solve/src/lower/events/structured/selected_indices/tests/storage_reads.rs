use super::*;
use rumoca_core::{IndexBinderProvenance, StructuredIndexBinder, StructuredIndexDomain};
use solve::{CompareOp, LinearOp as Op, UnaryOp};

fn conditional(
    captures: usize,
    condition: Vec<Op>,
    result: Vec<Op>,
    fallback: Vec<Op>,
) -> solve::FunctionConditionalProgram {
    solve::FunctionConditionalProgram::checked(
        captures,
        vec![1],
        vec![(condition, result)],
        fallback,
    )
    .unwrap()
}

fn constant(value: f64) -> Vec<Op> {
    vec![Op::Const { dst: 0, value }, Op::StoreOutput { src: 0 }]
}

fn relation(index: usize) -> Vec<Op> {
    let wrapper = conditional(
        1,
        vec![Op::LoadP { dst: 0, index: 0 }, Op::StoreOutput { src: 0 }],
        vec![
            Op::LoadY { dst: 0, index },
            Op::Unary {
                dst: 1,
                op: UnaryOp::Sqrt,
                arg: 0,
            },
            Op::LoadFunctionConditionalCapture { dst: 2, index: 0 },
            Op::Compare {
                dst: 3,
                op: CompareOp::Lt,
                lhs: 1,
                rhs: 2,
            },
            Op::StoreOutput { src: 3 },
        ],
        vec![
            Op::LoadP {
                dst: 0,
                index: index + 1,
            },
            Op::StoreOutput { src: 0 },
        ],
    );
    let outer = conditional(
        1,
        constant(1.0),
        vec![
            Op::LoadFunctionConditionalCapture { dst: 0, index: 0 },
            Op::FunctionConditional {
                dst_start: 1,
                capture_start: 0,
                program: Arc::new(wrapper),
            },
            Op::StoreOutput { src: 1 },
        ],
        constant(99.0),
    );
    vec![
        Op::Const { dst: 0, value: 1.0 },
        Op::FunctionConditional {
            dst_start: 1,
            capture_start: 0,
            program: Arc::new(outer),
        },
        Op::StoreOutput { src: 1 },
    ]
}

fn domain() -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: 3,
            step: 1,
        }],
    }
}

fn certificate(programs: &[Vec<Op>]) -> Result<AffineProgramCertificate, LowerError> {
    let domain = domain();
    let points = domain.index_tuples().unwrap();
    derive_affine_program_certificate(&domain, &points[0], &points, programs, source_span())
}

#[test]
fn nested_relation_reads_keep_captures_lazy_arithmetic_and_full_affine_proof() {
    let original = (0..3).map(relation).collect::<Vec<_>>();
    assert!(certificate(&original).is_err());
    let mut exposed = original.clone();
    expose_selected_indices(&mut exposed, source_span()).unwrap();
    let (base_ops, load_strides, const_strides) = certificate(&exposed).unwrap();
    assert_eq!(load_strides.len(), 2);
    let block = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::Map {
            output_map: solve::TensorOutputMap::dense_contiguous(0, &domain()).unwrap(),
            domain: domain(),
            base_ops,
            load_strides,
            const_strides,
            metadata: solve::TensorNodeMetadata::default(),
            span: source_span(),
        }],
    };
    let scalar = rumoca_eval_solve::to_scalar_program_block(&block).unwrap();
    for (y, p, expected) in [
        (
            vec![0.25, 4.0, 0.0],
            vec![1.0, 0.0, 1.0, 0.0],
            vec![1.0, 0.0, 1.0],
        ),
        (
            vec![-1.0, -4.0, -9.0],
            vec![0.0, 1.0, 0.0, 1.0],
            vec![1.0, 0.0, 1.0],
        ),
    ] {
        for (point, original) in original.iter().enumerate() {
            assert_eq!(
                rumoca_eval_solve::eval_row(original, &y, &p, 0.0, None).unwrap(),
                expected[point]
            );
            assert_eq!(
                rumoca_eval_solve::eval_row(&exposed[point], &y, &p, 0.0, None).unwrap(),
                expected[point]
            );
            assert_eq!(
                rumoca_eval_solve::eval_row(&scalar.programs()[point], &y, &p, 0.0, None).unwrap(),
                expected[point]
            );
        }
    }
}

#[test]
fn exposing_nested_reads_does_not_certify_non_affine_storage_indices() {
    let mut programs = [0, 1, 3].into_iter().map(relation).collect::<Vec<_>>();
    expose_selected_indices(&mut programs, source_span()).unwrap();
    assert!(certificate(&programs).is_err());
}

#[test]
fn nested_storage_kind_and_destination_changes_are_not_capture_substitutions() {
    let span = source_span();
    for changed in [
        Op::LoadP { dst: 0, index: 1 },
        Op::LoadY { dst: 1, index: 1 },
    ] {
        let base = Op::LoadY { dst: 0, index: 0 };
        let mut region = Region::new(2, 2, Some(0), span).unwrap();
        assert!(storage(&[&base, &changed], &mut region, &0).is_err());
        assert!(region.family.programs.iter().all(Vec::is_empty));
    }
}
