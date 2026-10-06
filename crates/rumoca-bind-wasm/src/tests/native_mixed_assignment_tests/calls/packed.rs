use super::*;
use sha2::{Digest, Sha256};

fn source() -> &'static str {
    "function TupleValue
       input Real patch[2]; output Real value;
       algorithm value := patch[1]-patch[2]; end TupleValue;
     model PackedCalls
       input Real x[5] = {1,2,3,4,5}; output Real y[4];
       equation for i in 1:4 loop
         if i > 1 and i < 4 then y[i] = TupleValue(x[i:i+1]);
         else y[i] = 0.0; end if;
       end for;
     end PackedCalls;"
}

fn artifact(source: &str) -> serde_json::Value {
    let artifact = crate::native_assignment_api::with_prepared_native_model(
        source,
        "PackedCalls",
        |model, source, name| {
            assert!(
                model
                    .problem
                    .continuous
                    .implicit_rhs
                    .nodes
                    .iter()
                    .any(guarded_source_calls)
            );
            crate::native_program_api::model_artifact(model, source, name)
        },
    )
    .unwrap();
    let artifact: serde_json::Value = serde_json::from_str(&artifact).unwrap();
    assert_eq!(artifact["profile"], "native-direct-program-f64-v3");
    assert_eq!(artifact["abi"]["transactional_y"], true);
    assert_eq!(
        artifact["source_sha256"],
        format!("{:x}", Sha256::digest(source.as_bytes()))
    );
    artifact
}

fn guarded_source_calls(node: &rumoca_ir_solve::ComputeNode) -> bool {
    let rumoca_ir_solve::ComputeNode::ScalarPrograms(block) = node else {
        return false;
    };
    // Compact conditional capture exposes the original scalar rows directly,
    // instead of the former Move-packed residual tuple. Both interior calls
    // must still use one exact source-issued owner and their full patch input.
    if block.output_indices() != [0, 1, 2, 3] || block.programs().len() != 4 {
        return false;
    }
    let mut owner = None;
    block.programs().iter().enumerate().all(|(row, ops)| {
        if !matches!(
            ops.last(),
            Some(rumoca_ir_solve::LinearOp::StoreOutput { .. })
        ) {
            return false;
        }
        let calls = ops
            .iter()
            .filter_map(|op| match op {
                rumoca_ir_solve::LinearOp::PureCall { site, .. } => Some(site),
                _ => None,
            })
            .collect::<Vec<_>>();
        if row == 0 || row == 3 {
            return calls.is_empty();
        }
        let [site] = calls.as_slice() else {
            return false;
        };
        if owner.is_some_and(|owner| owner != site.owner()) {
            return false;
        }
        owner = Some(site.owner());
        site.inputs().len() == 1
            && site.inputs()[0].dimensions() == [2]
            && site.outputs().len() == 1
            && site.outputs()[0].value_type().scalar_count() == 1
    })
}

fn check_frames(artifact: &serde_json::Value, edited: bool) {
    let mut runner = CallExecution::new(artifact);
    let mut p = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|v| v.as_f64().unwrap())
        .collect::<Vec<_>>();
    for input in [
        [1., 2., 4., 8., 16.],
        [9., -0., 0., -2., 3.],
        [0., f64::INFINITY, 2., 4., 0.],
    ] {
        for (i, &value) in input.iter().enumerate() {
            p[slot(artifact, &format!("x[{}]", i + 1), "P")] = value;
        }
        let output = runner.run(&p);
        let expected = [
            0.,
            if edited {
                input[1] + input[2]
            } else {
                input[1] - input[2]
            },
            if edited {
                input[2] + input[3]
            } else {
                input[2] - input[3]
            },
            0.,
        ];
        for (i, value) in expected.into_iter().enumerate() {
            assert_eq!(
                output[slot(artifact, &format!("y[{}]", i + 1), "Y")].to_bits(),
                value.to_bits()
            );
        }
    }
}

#[test]
fn original_move_packed_source_tuple_admits_native_v3_and_executes_source_edit() {
    let _lock = session_test_guard();
    for (source, edited) in [
        (source().to_owned(), false),
        (
            source().replace("patch[1]-patch[2]", "patch[1]+patch[2]"),
            true,
        ),
    ] {
        let artifact = artifact(&source);
        check_frames(&artifact, edited);
    }
}
