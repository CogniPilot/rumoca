//! Cranelift backend for compiled residual and Jacobian-vector evaluation.

mod emit;

use crate::compiled::VarLayout;
use crate::compiled::ad::lower_residual_ad;
use crate::compiled::lower::{
    LowerError, lower_discrete_rhs, lower_expression_rows_from_expressions, lower_initial_residual,
    lower_residual, lower_root_conditions,
};
#[cfg(feature = "wasm")]
use crate::compiled::wasm;
use rumoca_ir_dae as dae;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Backend {
    Cranelift,
    #[cfg(feature = "wasm")]
    Wasm,
}

#[derive(Debug)]
pub enum CompileError {
    Lower(LowerError),
    Backend(String),
    Input(String),
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lower(err) => write!(f, "{err}"),
            Self::Backend(msg) => write!(f, "cranelift backend error: {msg}"),
            Self::Input(msg) => write!(f, "invalid input: {msg}"),
        }
    }
}

impl std::error::Error for CompileError {}

impl From<LowerError> for CompileError {
    fn from(value: LowerError) -> Self {
        Self::Lower(value)
    }
}

pub struct CompiledResidual {
    backend: ResidualBackend,
}

enum ResidualBackend {
    Cranelift(Box<emit::CompiledResidualRows>),
    #[cfg(feature = "wasm")]
    Wasm(wasm::CompiledResidualWasm),
}

impl CompiledResidual {
    pub fn call(&self, y: &[f64], p: &[f64], t: f64, out: &mut [f64]) -> Result<(), CompileError> {
        match &self.backend {
            ResidualBackend::Cranelift(jit) => jit.call(y, p, t, out),
            #[cfg(feature = "wasm")]
            ResidualBackend::Wasm(_) => Err(CompileError::Input(
                "WASM residual artifact does not support direct native call()".to_string(),
            )),
        }
    }

    pub fn rows(&self) -> usize {
        match &self.backend {
            ResidualBackend::Cranelift(jit) => jit.rows(),
            #[cfg(feature = "wasm")]
            ResidualBackend::Wasm(wasm) => wasm.rows(),
        }
    }

    #[cfg(feature = "wasm")]
    pub fn wasm_module_bytes(&self) -> Option<&[u8]> {
        match &self.backend {
            ResidualBackend::Cranelift(_) => None,
            ResidualBackend::Wasm(wasm) => Some(wasm.module_bytes()),
        }
    }
}

pub struct CompiledJacobianV {
    jit: emit::CompiledJacobianRows,
}

impl CompiledJacobianV {
    pub fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        self.jit.call(y, p, t, v, out)
    }

    pub fn rows(&self) -> usize {
        self.jit.rows()
    }
}

pub struct CompiledExpressionRows {
    jit: emit::CompiledResidualRows,
}

impl CompiledExpressionRows {
    pub fn call(&self, y: &[f64], p: &[f64], t: f64, out: &mut [f64]) -> Result<(), CompileError> {
        self.jit.call(y, p, t, out)
    }

    pub fn rows(&self) -> usize {
        self.jit.rows()
    }
}

pub fn compile_residual(
    dae_model: &dae::Dae,
    backend: Backend,
) -> Result<CompiledResidual, CompileError> {
    match backend {
        Backend::Cranelift => {
            let layout = VarLayout::from_dae(dae_model);
            let rows = lower_residual(dae_model, &layout)?;
            let jit = emit::compile_residual_rows(&rows)?;
            Ok(CompiledResidual {
                backend: ResidualBackend::Cranelift(Box::new(jit)),
            })
        }
        #[cfg(feature = "wasm")]
        Backend::Wasm => {
            let artifact = wasm::compile_residual_wasm(dae_model)
                .map_err(|err| CompileError::Backend(err.to_string()))?;
            Ok(CompiledResidual {
                backend: ResidualBackend::Wasm(artifact),
            })
        }
    }
}

pub fn compile_jacobian_v(
    dae_model: &dae::Dae,
    backend: Backend,
) -> Result<CompiledJacobianV, CompileError> {
    match backend {
        Backend::Cranelift => {
            let layout = VarLayout::from_dae(dae_model);
            let rows = lower_residual_ad(dae_model, &layout)?;
            let jit = emit::compile_jacobian_rows(&rows)?;
            Ok(CompiledJacobianV { jit })
        }
        #[cfg(feature = "wasm")]
        Backend::Wasm => Err(CompileError::Input(
            "WASM backend does not yet implement Jacobian-vector compilation".to_string(),
        )),
    }
}

fn compile_expression_rows(
    rows: Vec<Vec<crate::compiled::linear_op::LinearOp>>,
    backend: Backend,
    kernel_name: &str,
) -> Result<CompiledExpressionRows, CompileError> {
    #[cfg(not(feature = "wasm"))]
    let _ = kernel_name;
    match backend {
        Backend::Cranelift => {
            let jit = emit::compile_residual_rows(&rows)?;
            Ok(CompiledExpressionRows { jit })
        }
        #[cfg(feature = "wasm")]
        Backend::Wasm => Err(CompileError::Input(format!(
            "WASM backend does not yet implement {kernel_name} compilation"
        ))),
    }
}

pub fn compile_root_conditions(
    dae_model: &dae::Dae,
    backend: Backend,
) -> Result<CompiledExpressionRows, CompileError> {
    let layout = VarLayout::from_dae(dae_model);
    let rows = lower_root_conditions(dae_model, &layout)?;
    compile_expression_rows(rows, backend, "root-condition kernel")
}

pub fn compile_expressions(
    dae_model: &dae::Dae,
    expressions: &[dae::Expression],
    backend: Backend,
) -> Result<CompiledExpressionRows, CompileError> {
    let layout = VarLayout::from_dae(dae_model);
    let rows = lower_expression_rows_from_expressions(expressions, &layout, &dae_model.functions)?;
    compile_expression_rows(rows, backend, "expression kernel")
}

pub fn compile_discrete_rhs(
    dae_model: &dae::Dae,
    backend: Backend,
) -> Result<CompiledExpressionRows, CompileError> {
    let layout = VarLayout::from_dae(dae_model);
    let rows = lower_discrete_rhs(dae_model, &layout)?;
    compile_expression_rows(rows, backend, "discrete RHS kernel")
}

pub fn compile_initial_residual(
    dae_model: &dae::Dae,
    backend: Backend,
) -> Result<CompiledExpressionRows, CompileError> {
    let layout = VarLayout::from_dae(dae_model);
    let rows = lower_initial_residual(dae_model, &layout)?;
    compile_expression_rows(rows, backend, "initial residual kernel")
}

#[cfg(test)]
mod tests {
    use super::{Backend, compile_jacobian_v, compile_residual, compile_root_conditions};
    use crate::runtime::dual::Dual;
    use crate::runtime::eval::{
        VarEnv, build_env, eval_condition_as_root_dae as eval_condition_as_root,
        eval_expr_dae as eval_expr, lift_env, map_var_to_env,
    };
    use rumoca_ir_dae as dae;

    fn scalar_var(name: &str) -> dae::Variable {
        dae::Variable::new(dae::VarName::new(name))
    }

    fn expr_var(name: &str) -> dae::Expression {
        dae::Expression::VarRef {
            name: dae::VarName::new(name),
            subscripts: vec![],
        }
    }

    fn seed_duals_from_v(dae_model: &dae::Dae, env_dual: &mut VarEnv<Dual>, v: &[f64]) {
        let mut seed_env = VarEnv::<f64>::new();
        let mut idx = 0usize;
        for (name, var) in dae_model
            .states
            .iter()
            .chain(dae_model.algebraics.iter())
            .chain(dae_model.outputs.iter())
        {
            map_var_to_env(&mut seed_env, name.as_str(), var, v, &mut idx);
        }
        for (name, du) in seed_env.vars {
            if let Some(entry) = env_dual.vars.get_mut(&name) {
                entry.du = du;
            }
        }
    }

    #[test]
    fn compile_residual_matches_interpreted_rows() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .states
            .insert(dae::VarName::new("x"), scalar_var("x"));
        dae_model
            .algebraics
            .insert(dae::VarName::new("z"), scalar_var("z"));
        dae_model
            .parameters
            .insert(dae::VarName::new("p"), scalar_var("p"));

        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Add(Default::default()),
                lhs: Box::new(expr_var("x")),
                rhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Sin,
                    args: vec![expr_var("z")],
                }),
            },
            Default::default(),
            "row0",
        ));
        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Exp,
                    args: vec![expr_var("x")],
                }),
                rhs: Box::new(dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Mul(Default::default()),
                    lhs: Box::new(expr_var("p")),
                    rhs: Box::new(expr_var("z")),
                }),
            },
            Default::default(),
            "row1",
        ));

        let compiled = compile_residual(&dae_model, Backend::Cranelift).expect("compile residual");
        let y = vec![0.25, 0.5];
        let p = vec![2.0];
        let t = 0.2;
        let mut out = vec![0.0; dae_model.f_x.len()];
        compiled
            .call(&y, &p, t, &mut out)
            .expect("call compiled residual");

        let env = build_env(&dae_model, &y, &p, t);
        let expected0 = -eval_expr::<f64>(&dae_model.f_x[0].rhs, &env);
        let expected1 = eval_expr::<f64>(&dae_model.f_x[1].rhs, &env);
        assert!((out[0] - expected0).abs() <= 1e-12);
        assert!((out[1] - expected1).abs() <= 1e-12);
    }

    #[test]
    fn compile_jacobian_v_matches_dual_reference() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .states
            .insert(dae::VarName::new("x"), scalar_var("x"));
        dae_model
            .algebraics
            .insert(dae::VarName::new("z"), scalar_var("z"));

        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Add(Default::default()),
                lhs: Box::new(expr_var("x")),
                rhs: Box::new(expr_var("z")),
            },
            Default::default(),
            "row0",
        ));
        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Add(Default::default()),
                lhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Cos,
                    args: vec![expr_var("x")],
                }),
                rhs: Box::new(dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Mul(Default::default()),
                    lhs: Box::new(expr_var("z")),
                    rhs: Box::new(expr_var("z")),
                }),
            },
            Default::default(),
            "row1",
        ));

        let compiled = compile_jacobian_v(&dae_model, Backend::Cranelift).expect("compile jv");
        let y = vec![0.5, 1.2];
        let p = vec![];
        let v = vec![1.4, -0.3];
        let t = 0.0;
        let mut out = vec![0.0; dae_model.f_x.len()];
        compiled
            .call(&y, &p, t, &v, &mut out)
            .expect("call compiled jv");

        let env_f64 = build_env(&dae_model, &y, &p, t);
        let mut env_dual = lift_env::<Dual>(&env_f64);
        seed_duals_from_v(&dae_model, &mut env_dual, &v);
        let expected0 = -eval_expr::<Dual>(&dae_model.f_x[0].rhs, &env_dual).du;
        let expected1 = eval_expr::<Dual>(&dae_model.f_x[1].rhs, &env_dual).du;
        assert!((out[0] - expected0).abs() <= 1e-12);
        assert!((out[1] - expected1).abs() <= 1e-12);
    }

    #[test]
    fn compile_root_conditions_matches_runtime_reference() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .states
            .insert(dae::VarName::new("x"), scalar_var("x"));
        dae_model
            .algebraics
            .insert(dae::VarName::new("z"), scalar_var("z"));

        dae_model.relation.push(dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Lt(Default::default()),
            lhs: Box::new(expr_var("x")),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
        });
        dae_model
            .synthetic_root_conditions
            .push(dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Eq(Default::default()),
                lhs: Box::new(expr_var("z")),
                rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
            });

        let compiled =
            compile_root_conditions(&dae_model, Backend::Cranelift).expect("compile roots");
        let y = vec![-0.2, 0.0];
        let p = vec![];
        let t = 0.1;
        let mut out = vec![0.0; compiled.rows()];
        compiled
            .call(&y, &p, t, &mut out)
            .expect("call compiled roots");

        let env = build_env(&dae_model, &y, &p, t);
        let mut expected = Vec::new();
        expected.extend(
            dae_model
                .relation
                .iter()
                .map(|expr| eval_condition_as_root(expr, &env)),
        );
        expected.extend(
            dae_model
                .synthetic_root_conditions
                .iter()
                .map(|expr| eval_condition_as_root(expr, &env)),
        );
        assert_eq!(out.len(), expected.len());
        for (actual, expected) in out.into_iter().zip(expected) {
            assert!((actual - expected).abs() <= 1e-12);
        }
    }

    #[test]
    fn compile_root_conditions_disables_runtime_sample_condition_rows() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .states
            .insert(dae::VarName::new("x"), scalar_var("x"));
        dae_model.relation.push(dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![
                dae::Expression::Literal(dae::Literal::Real(0.0)),
                dae::Expression::Literal(dae::Literal::Real(0.1)),
            ],
        });

        let compiled =
            compile_root_conditions(&dae_model, Backend::Cranelift).expect("compile roots");
        let mut out = vec![0.0; compiled.rows()];
        compiled
            .call(&[0.0], &[], 0.0, &mut out)
            .expect("call compiled roots");
        assert_eq!(out, vec![1.0]);
    }

    #[test]
    fn compile_root_conditions_disables_runtime_discrete_binding_rows() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .discrete_valued
            .insert(dae::VarName::new("count"), scalar_var("count"));
        dae_model
            .parameters
            .insert(dae::VarName::new("nperiod"), scalar_var("nperiod"));
        dae_model
            .synthetic_root_conditions
            .push(dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Ge(Default::default()),
                lhs: Box::new(expr_var("count")),
                rhs: Box::new(expr_var("nperiod")),
            });

        let compiled =
            compile_root_conditions(&dae_model, Backend::Cranelift).expect("compile roots");
        let mut out = vec![0.0; compiled.rows()];
        compiled
            .call(&[2.0], &[3.0], 0.0, &mut out)
            .expect("call compiled roots");
        assert_eq!(out, vec![1.0]);
    }

    #[cfg(feature = "wasm")]
    #[test]
    fn compile_residual_wasm_backend_produces_module_bytes() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .states
            .insert(dae::VarName::new("x"), scalar_var("x"));
        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Sin,
                args: vec![expr_var("x")],
            },
            Default::default(),
            "row0",
        ));

        let compiled = compile_residual(&dae_model, Backend::Wasm).expect("compile wasm residual");
        let module = compiled
            .wasm_module_bytes()
            .expect("extract wasm module bytes");
        assert!(!module.is_empty());
        let err = compiled.call(&[0.0], &[], 0.0, &mut [0.0]);
        assert!(err.is_err());
    }
}
