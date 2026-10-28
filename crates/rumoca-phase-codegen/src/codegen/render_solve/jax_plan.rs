//! Demand-built, node-ordered compact JAX families plus checked scalar fallback.
use super::jax_family;
use super::*;
use crate::errors::CodegenError;
use std::sync::OnceLock;

#[derive(Debug)]
struct JaxPlan {
    block: Arc<solve::ComputeBlock>,
    steps: OnceLock<Result<Vec<Value>, CodegenError>>,
}
#[derive(Debug)]
struct Steps(Vec<Value>);
impl Object for Steps {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Seq
    }
    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Seq(self.0.len())
    }
    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        self.0.get(key.as_usize()?).cloned()
    }
}
impl Object for JaxPlan {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Map
    }
    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Str(&["steps"])
    }
    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        if key.as_str()? != "steps" {
            return None;
        }
        Some(
            match self.steps.get_or_init(|| execution_steps(&self.block)) {
                Ok(steps) => Value::from_object(Steps(steps.clone())),
                Err(error) => Value::from(render_err(error.to_string()).with_source(error.clone())),
            },
        )
    }
}

pub(in crate::codegen) fn jax_plan_value(block: Arc<solve::ComputeBlock>) -> Value {
    Value::from_object(JaxPlan {
        block,
        steps: OnceLock::new(),
    })
}

fn native_coverage(
    family: &RenderNativeAffineFamily,
    coverage: &mut Vec<(usize, usize)>,
) -> Result<usize, CodegenError> {
    let count = family
        .output_map
        .output_count(&family.domain)
        .map_err(|e| CodegenError::template(format!("JAX output map: {e:?}")))?;
    // Derive contiguous spans from compact stride geometry. Rectangular
    // interiors need one range per row, never one tuple/program per cell.
    coverage.extend(
        jax_family::coverage_ranges(family).map_err(|e| CodegenError::template(e.to_string()))?,
    );
    Ok(count)
}

fn scalar_step(
    node: &solve::ComputeNode,
    cursor: usize,
) -> Result<(Value, Vec<usize>), CodegenError> {
    // Scalarize only this unsupported node, preserving source spans and
    // original global placement. Other native nodes remain compact owners.
    let scalar = rumoca_eval_solve::to_scalar_program_block(&solve::ComputeBlock {
        nodes: vec![node.clone()],
    })?;
    let indices = match node {
        solve::ComputeNode::ScalarPrograms(source) => {
            rumoca_eval_solve::scalar_program_output_indices(source, cursor, "JAX scalar fallback")?
        }
        solve::ComputeNode::MatMul { .. } | solve::ComputeNode::LinSolve { .. } => scalar
            .output_indices()
            .iter()
            .map(|index| {
                cursor
                    .checked_add(*index)
                    .ok_or_else(|| CodegenError::template("JAX fallback output overflow"))
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => scalar.output_indices().to_vec(),
    };
    let scalar = solve::ScalarProgramBlock::with_output_indices(
        scalar.programs().to_vec(),
        scalar.program_spans().to_vec(),
        indices.clone(),
    )
    .map_err(|e| CodegenError::template(e.to_string()))?;
    let plan = super::super::scalar_program_plan::ScalarProgramPlan::new(Arc::new(scalar))?;
    Ok((
        minijinja::context! { native => false, scalar_plan => Value::from_object(plan) },
        indices,
    ))
}

fn native_family(
    node: &solve::ComputeNode,
) -> Result<Option<RenderNativeAffineFamily>, CodegenError> {
    if !matches!(
        node,
        solve::ComputeNode::Map { .. } | solve::ComputeNode::AffineStencil { .. }
    ) {
        return Ok(None);
    }
    let mut partition = native_family_template_partition(&solve::ComputeBlock {
        nodes: vec![node.clone()],
    })?;
    if partition.families.len() != 1 || !jax_family::supports(&partition.families[0]) {
        return Ok(None);
    }
    Ok(Some(partition.families.remove(0)))
}

fn execution_steps(block: &solve::ComputeBlock) -> Result<Vec<Value>, CodegenError> {
    let mut steps = Vec::new();
    let mut cursor = 0usize;
    let mut coverage = Vec::new();
    for node in &block.nodes {
        if let Some(family) = native_family(node)? {
            if family.count == 0 {
                continue;
            }
            cursor = cursor.max(native_coverage(&family, &mut coverage)?);
            steps.push(minijinja::context! {
                native => true,
                families => Value::from_object(SolveNativeFamiliesValue::new(vec![family])),
            });
            continue;
        }
        let (step, indices) = scalar_step(node, cursor)?;
        for index in indices {
            let end = index
                .checked_add(1)
                .ok_or_else(|| CodegenError::template("JAX output range overflow"))?;
            coverage.push((index, end));
            cursor = cursor.max(end);
        }
        steps.push(step);
    }
    coverage.sort_unstable();
    let mut covered = 0usize;
    for (start, end) in coverage {
        if start > covered {
            return Err(CodegenError::template("JAX derivative output coverage gap"));
        }
        covered = covered.max(end);
    }
    if covered != block.len()? {
        return Err(CodegenError::template(
            "JAX derivative output extent mismatch",
        ));
    }
    Ok(steps)
}

#[cfg(test)]
mod tests {
    use super::*;
    fn span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("jax-plan.mo"),
            1,
            2,
        )
    }
    fn map_node(operator: solve::BinaryOp) -> solve::ComputeNode {
        let domain = rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                provenance: rumoca_core::IndexBinderProvenance::Generated,
                id: 0,
                display_name: "i".into(),
                lower: 1,
                upper: 128,
                step: 1,
            }],
        };
        solve::ComputeNode::Map {
            output_map: solve::TensorOutputMap::dense_contiguous(0, &domain).unwrap(),
            domain,
            base_ops: vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::Const { dst: 1, value: 2.0 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: operator,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ],
            load_strides: vec![solve::AffineStencilLoadStride {
                op_position: 0,
                terms: vec![solve::AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 1,
                }],
            }],
            const_strides: vec![],
            metadata: Default::default(),
            span: span(),
        }
    }
    #[test]
    fn native_family_then_scalar_boundary_keeps_global_output_slot() {
        let boundary = solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 7.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            span().require_provenance("boundary").unwrap(),
        )
        .unwrap();
        let steps = execution_steps(&solve::ComputeBlock {
            nodes: vec![
                map_node(solve::BinaryOp::Mul),
                solve::ComputeNode::ScalarPrograms(boundary),
            ],
        })
        .unwrap();
        assert_eq!(steps.len(), 2);
        assert_eq!(
            steps[0].get_attr("native").unwrap(),
            minijinja::Value::from(true)
        );
        assert_eq!(
            steps[1].get_attr("native").unwrap(),
            minijinja::Value::from(false)
        );
        let programs = steps[1]
            .get_attr("scalar_plan")
            .unwrap()
            .get_attr("programs")
            .unwrap();
        let program = programs.get_item(&Value::from(0)).unwrap();
        let indices = program.get_attr("output_indices").unwrap();
        assert_eq!(
            indices.get_item(&Value::from(0)).unwrap().as_usize(),
            Some(128)
        );
    }
    #[test]
    fn dense_fallback_after_native_family_keeps_its_global_offset() {
        let provenance =
            solve::PatternProvenance::derived(solve::PatternDerivation::TensorOperand, span())
                .unwrap();
        let pattern =
            solve::StructuralPattern::from_row_dependencies(1, 1, &[vec![0]], provenance).unwrap();
        let dense = solve::ComputeNode::MatMul {
            lhs_ops: vec![solve::LinearOp::Const { dst: 0, value: 2.0 }],
            lhs_start: 0,
            rhs_ops: vec![solve::LinearOp::Const { dst: 1, value: 3.0 }],
            rhs_start: 1,
            m: 1,
            k: 1,
            n: 1,
            lhs_pattern: pattern.clone(),
            rhs_pattern: pattern,
            metadata: Default::default(),
            span: span(),
        };
        let steps = execution_steps(&solve::ComputeBlock {
            nodes: vec![map_node(solve::BinaryOp::Mul), dense],
        })
        .unwrap();
        assert_eq!(steps.len(), 2);
        assert_eq!(
            steps[0].get_attr("native").unwrap(),
            minijinja::Value::from(true)
        );
        let programs = steps[1]
            .get_attr("scalar_plan")
            .unwrap()
            .get_attr("programs")
            .unwrap();
        let program = programs.get_item(&Value::from(0)).unwrap();
        let indices = program.get_attr("output_indices").unwrap();
        assert_eq!(
            indices.get_item(&Value::from(0)).unwrap().as_usize(),
            Some(128)
        );
    }

    #[test]
    fn output_gaps_refuse_instead_of_returning_initialized_placeholders() {
        let block = solve::ScalarProgramBlock::with_output_indices(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            vec![span()],
            vec![2],
        )
        .unwrap();
        let result = execution_steps(&solve::ComputeBlock {
            nodes: vec![solve::ComputeNode::ScalarPrograms(block)],
        });
        assert!(result.is_err());
        assert!(
            execution_steps(&solve::ComputeBlock::default())
                .unwrap()
                .is_empty()
        );
    }

    #[test]
    fn strided_two_dimensional_interior_keeps_native_body_and_boundary_hole() {
        let mut interior = map_node(solve::BinaryOp::Mul);
        if let solve::ComputeNode::Map {
            domain, output_map, ..
        } = &mut interior
        {
            domain.binders[0].upper = 2;
            let mut inner = domain.binders[0].clone();
            inner.id = 1;
            inner.display_name = "j".into();
            domain.binders.push(inner);
            *output_map = solve::TensorOutputMap {
                start: 0,
                strides: vec![
                    solve::AffineStencilIndexStrideTerm {
                        dimension: 0,
                        stride: 3,
                    },
                    solve::AffineStencilIndexStrideTerm {
                        dimension: 1,
                        stride: 1,
                    },
                ],
            };
        }
        let partition = native_family_template_partition(&solve::ComputeBlock {
            nodes: vec![interior.clone()],
        })
        .unwrap();
        let mut coverage = Vec::new();
        assert_eq!(
            native_coverage(&partition.families[0], &mut coverage).unwrap(),
            5
        );
        assert_eq!(coverage, vec![(0, 2), (3, 5)]);
        // Interior stores0,1,3,4. A scalar boundary fills2; treating the
        // family's bounding box as dense would silently miss this hole.
        let boundary = solve::ScalarProgramBlock::with_output_indices(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 7.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            vec![span()],
            vec![2],
        )
        .unwrap();
        assert!(
            execution_steps(&solve::ComputeBlock {
                nodes: vec![interior.clone()]
            })
            .is_err()
        );
        let steps = execution_steps(&solve::ComputeBlock {
            nodes: vec![interior, solve::ComputeNode::ScalarPrograms(boundary)],
        })
        .unwrap();
        assert_eq!(steps.len(), 2);
        assert_eq!(
            steps[0].get_attr("native").unwrap(),
            minijinja::Value::from(true)
        );
        assert_eq!(
            steps[1].get_attr("native").unwrap(),
            minijinja::Value::from(false)
        );
    }

    #[test]
    fn malformed_jax_plan_refuses_through_actual_template_iteration() {
        let scalar = solve::ScalarProgramBlock::with_output_indices(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            vec![span()],
            vec![2],
        )
        .unwrap();
        let plan = jax_plan_value(Arc::new(solve::ComputeBlock {
            nodes: vec![solve::ComputeNode::ScalarPrograms(scalar)],
        }));
        let mut environment = minijinja::Environment::new();
        environment
            .add_template(
                "probe",
                "{% for step in plan.steps %}unexpected{% endfor %}",
            )
            .unwrap();
        // Exercise the lazy error Value at the real template consumer boundary,
        // including its cached second access; an empty loop must never succeed.
        for _ in 0..2 {
            let error = environment
                .get_template("probe")
                .unwrap()
                .render(minijinja::context! { plan => plan.clone() })
                .expect_err("malformed plan cannot render as an empty derivative");
            assert!(format!("{error:#}").contains("coverage gap"), "{error:#}");
        }
    }

    #[test]
    fn unsupported_family_preserves_checked_scalar_fallback() {
        let steps = execution_steps(&solve::ComputeBlock {
            nodes: vec![map_node(solve::BinaryOp::Min)],
        })
        .unwrap();
        assert_eq!(steps.len(), 1);
        assert_eq!(
            steps[0].get_attr("native").unwrap(),
            minijinja::Value::from(false)
        );
        let programs = steps[0]
            .get_attr("scalar_plan")
            .unwrap()
            .get_attr("programs")
            .unwrap();
        assert_eq!(programs.len(), Some(128));
    }
}
