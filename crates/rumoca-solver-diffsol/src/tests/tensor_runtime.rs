use rumoca_ir_solve as solve;

use crate::OdeModel;

#[test]
fn ode_model_evaluates_tensor_jacobian_vector_product() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string()];
    model.problem.continuous.implicit_row_targets = vec![Some(solve::scalar_slot_y(0))];
    model.artifacts.continuous.implicit_jacobian_v = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::MatMul {
            lhs_ops: vec![
                solve::LinearOp::Const { dst: 0, value: 2.0 },
                solve::LinearOp::Const {
                    dst: 1,
                    value: -1.0,
                },
                solve::LinearOp::Const { dst: 2, value: 3.0 },
                solve::LinearOp::Const { dst: 3, value: 4.0 },
            ],
            lhs_start: 0,
            rhs_ops: vec![
                solve::LinearOp::LoadSeed { dst: 4, index: 0 },
                solve::LinearOp::LoadSeed { dst: 5, index: 1 },
            ],
            rhs_start: 4,
            m: 2,
            k: 2,
            n: 1,
            lhs_sparsity: solve::SparsityPattern::Dense,
            rhs_sparsity: solve::SparsityPattern::Dense,
            metadata: solve::TensorNodeMetadata::default(),
            span: solve::SolveVariableMeta::empty_with_span(fixture_span!()).source_span,
        }],
    };

    let ode = OdeModel::new(&model).expect("ODE model should prepare tensor artifacts");
    let mut out = [0.0; 2];
    ode.eval_jacobian_v(&[], &[], 0.0, &[5.0, 7.0], &mut out)
        .expect("tensor Jacobian-vector product should evaluate");

    assert_eq!(out, [3.0, 43.0]);
}
