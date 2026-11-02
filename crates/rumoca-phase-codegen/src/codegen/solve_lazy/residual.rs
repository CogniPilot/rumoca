//! Demand-built scalar residuals retain the original block and pure-call owner.

use super::{
    Arc, CodegenError, Enumerator, Object, ObjectRepr, OnceLock, SolveRenderHandle, Value,
};

struct ContinuousBlocks {
    ordinary: Value,
    handle: SolveRenderHandle,
    residual: OnceLock<Result<Value, CodegenError>>,
}

impl std::fmt::Debug for ContinuousBlocks {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("ContinuousBlocks")
            .field("residual_ready", &self.residual.get().is_some())
            .finish_non_exhaustive()
    }
}

impl ContinuousBlocks {
    fn residual(&self) -> Value {
        let result = self.residual.get_or_init(|| {
            match super::scalar_lowered_block(
                &self.handle.problem().continuous.residual,
                self.handle.inlined_pure_calls(),
            )? {
                Some(block) => super::super::solve_template_compute_block_json(&block),
                None => Ok(Value::from(())),
            }
        });
        match result {
            Ok(value) => value.clone(),
            Err(error) => {
                Value::from(crate::errors::render_err(error.to_string()).with_source(error.clone()))
            }
        }
    }
}

impl Object for ContinuousBlocks {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Map
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Str(&[
            "implicit_rhs",
            "residual",
            "residual_scalar",
            "derivative_rhs",
            "derivative_rhs_scalar",
            "algebraic_assignments",
        ])
    }

    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        if key.as_str()? == "residual_scalar" {
            Some(self.residual())
        } else {
            self.ordinary.get_item(key).ok()
        }
    }
}

pub(in crate::codegen) fn continuous_blocks(ordinary: Value, handle: SolveRenderHandle) -> Value {
    Value::from_object(ContinuousBlocks {
        ordinary,
        handle,
        residual: OnceLock::new(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_solve as solve;

    fn fixture(ops: Vec<solve::LinearOp>) -> Value {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("scalar_residual.mo"),
            1,
            2,
        );
        let rows = solve::ScalarProgramBlock::with_output_indices(vec![ops], vec![span], vec![2])
            .expect("source-backed sparse output row");
        let mut problem = solve::SolveProblem::default();
        problem.continuous.residual = solve::ComputeBlock::from_scalar_program_block(rows);
        continuous_blocks(
            minijinja::context! { residual => "original", derivative_rhs => "derivative" },
            SolveRenderHandle::standalone(
                Arc::new(problem),
                Arc::new(solve::SolveArtifacts::default()),
            ),
        )
    }

    #[test]
    fn projection_is_demanded_once_and_preserves_sparse_outputs() {
        let value = fixture(vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]);
        let owner = value.downcast_object_ref::<ContinuousBlocks>().unwrap();
        assert!(format!("{owner:?}").contains("residual_ready: false"));
        let environment = minijinja::Environment::new();
        assert_eq!(
            environment
                .render_str(
                    "{{ c is mapping }}|{{ c | list | join(',') }}",
                    minijinja::context! { c => value.clone() }
                )
                .unwrap(),
            "true|implicit_rhs,residual,residual_scalar,derivative_rhs,derivative_rhs_scalar,algebraic_assignments"
        );
        assert_eq!(
            value.get_attr("residual").unwrap().as_str(),
            Some("original")
        );
        assert_eq!(
            value.get_attr("derivative_rhs").unwrap().as_str(),
            Some("derivative")
        );
        assert!(owner.residual.get().is_none());
        let scalar = value.get_attr("residual_scalar").unwrap();
        let first = owner.residual.get().unwrap() as *const _;
        let template = "{{ b.output_count }}|{{ b.scalar_plan.output_count }}|{{ b.scalar_plan.stored_output_count }}|{% for p in b.scalar_plan.programs %}{% for op in p.ops %}{% if op.kind == 'StoreOutput' %}{{ op.output_index }}{% endif %}{% endfor %}{% endfor %}";
        assert_eq!(
            environment
                .render_str(template, minijinja::context! { b => scalar })
                .unwrap(),
            "3|3|1|2"
        );
        value.get_attr("residual_scalar").unwrap();
        assert_eq!(owner.residual.get().unwrap() as *const _, first);
        assert!(format!("{owner:?}").contains("residual_ready: true"));
    }

    #[test]
    fn unsupported_scalar_projection_is_none_without_hiding_original() {
        let value = fixture(vec![
            solve::LinearOp::TensorLoad {
                dst_start: 0,
                input: solve::TensorInputKind::Y,
                input_start: 0,
                count: 1,
                seed_start: Some(0),
                lanes: 2,
            },
            solve::LinearOp::StoreOutput { src: 0 },
        ]);
        assert!(value.get_attr("residual_scalar").unwrap().is_none());
        assert_eq!(
            value.get_attr("residual").unwrap().as_str(),
            Some("original")
        );
        assert!(value.get_attr("no_such_block").unwrap().is_undefined());
    }
}
