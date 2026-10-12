use super::*;
use crate::SolveBinaryOperator;

#[test]
fn integer_tensor_directional_owners_preserve_primal_without_real_tangents() {
    let scalar = SolveScalarType::integer(profile());
    for operation in ["matrix", "cross", "scale", "broadcast"] {
        let dimensions = if operation == "matrix" {
            vec![2, 2]
        } else {
            vec![3]
        };
        let tensor = SolveValueType::tensor(scalar, dimensions).unwrap();
        let rhs = if matches!(operation, "scale" | "broadcast") {
            SolveValueType::scalar(scalar)
        } else {
            tensor.clone()
        };
        let table = SolvePureCallTable::construct(profile(), |table| {
            table.add_owner(
                identity(1),
                vec![tensor.clone(), rhs.clone()],
                vec![SolvePureCallOutput::result(tensor.clone())],
                span(0),
                |builder, inputs, outputs| {
                    let lhs = builder.load(inputs[0], span(1))?;
                    let rhs = builder.load(inputs[1], span(2))?;
                    let value = match operation {
                        "matrix" => builder.matrix_multiply(lhs, rhs, span(3))?,
                        "cross" => builder.cross(lhs, rhs, span(3))?,
                        "scale" => builder.scale(lhs, rhs, span(3))?,
                        _ => builder.broadcast_binary(
                            SolveBinaryOperator::Add,
                            lhs,
                            rhs,
                            false,
                            span(3),
                        )?,
                    };
                    builder.store(outputs[0], value, span(4))
                },
            )?;
            Ok(())
        })
        .unwrap_or_else(|error| panic!("{operation}: {error}"));
        let directional = table.owners()[0]
            .directional()
            .expect("Integer owner is retained");
        assert_eq!(directional.inputs(), &[tensor.clone(), rhs]);
        assert_eq!(directional.outputs().len(), 1);
        assert_eq!(directional.outputs()[0].value_type(), &tensor);
        assert!(
            directional
                .body()
                .register_types()
                .iter()
                .all(|ty| ty.element_type() == scalar)
        );
    }
}
