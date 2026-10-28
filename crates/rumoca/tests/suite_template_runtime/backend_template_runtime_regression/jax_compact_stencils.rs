//! Public source-size and numerical/AD regressions for #477.
use super::*;

fn heat_source(count: usize) -> String {
    format!(
        r#"
model Heat
  parameter Integer N = {count};
  parameter Real k = 1.0;
  Real u[N](each start = 1.0);
equation
  der(u[1]) = -k*u[1];
  for i in 2:N-1 loop
    der(u[i]) = k*(u[i-1] - 2*u[i] + u[i+1]);
  end for;
  der(u[N]) = -k*u[N];
end Heat;
"#
    )
}

fn heat_rhs(count: usize) -> String {
    let source = heat_source(count);
    let compiled = Compiler::new()
        .model("Heat")
        .compile_str(&source, "Heat.mo")
        .expect("reported Heat model compiles through checked Solve");
    let files = rumoca::render_target_files(&compiled, "Heat", "jax-ode", None)
        .expect("public JAX target renders Heat");
    let file = files
        .iter()
        .find(|file| file.path.ends_with("_jax_ode.py"))
        .expect("actual generated JAX module");
    // Exclude state-name/initial-value metadata: the contract concerns rhs
    // expansion, not the unavoidable storage inventory for N states.
    let (_, body) = file
        .content
        .split_once("def rhs(t, x, u, p):")
        .expect("public rhs signature");
    let body = body.split("if __name__").next().expect("rhs section");
    assert!(body.contains("return"), "nonempty emitted RHS");
    body.to_owned()
}

#[test]
fn issue_477_heat_rhs_does_not_expand_one_program_per_cell() {
    let small = heat_rhs(128);
    for count in [2048, 20000] {
        let large = heat_rhs(count);
        // A 16x domain increase may change numeric literals and small dispatch
        // scaffolding, but must not replicate the stencil body per cell. Both
        // bytes and lines are checked so compressing unrolled code is no escape.
        assert!(
            large.len() <= 2 * small.len(),
            "rhs bytes scale with cell count: {} -> {}",
            small.len(),
            large.len()
        );
        assert!(
            large.lines().count() <= 2 * small.lines().count(),
            "rhs lines scale with cell count: {} -> {}",
            small.lines().count(),
            large.lines().count()
        );
    }
}

#[test]
fn issue_477_compact_heat_matches_analytic_rhs_and_both_jacobians() {
    let compiled = Compiler::new()
        .model("Heat")
        .compile_str(&heat_source(8), "Heat.mo")
        .expect("checked Heat pipeline");
    let (directory, files) = rendered_model_target(&compiled, "Heat", "jax-ode");
    let module = files
        .iter()
        .find(|file| file.path.ends_with("_jax_ode.py"))
        .unwrap();
    run_python(
        &directory.path().join(&module.path),
        r#"
import importlib.util, re, sys
import jax
import jax.numpy as jnp
spec = importlib.util.spec_from_file_location('generated', sys.argv[1])
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)
# Use actual emitted storage ordering, never assume lexical array order.
cells = [int(re.search(r'u\[(\d+)\]$', name).group(1)) for name in m.STATE_NAMES]
assert sorted(cells) == list(range(1, 9))
slots = {cell: slot for slot, cell in enumerate(cells)}
x = jnp.array([cell * cell + 0.25 for cell in cells], dtype=jnp.float64)
p = jnp.asarray(m.init_params(m.DEFAULT_P), dtype=jnp.float64)
u = jnp.zeros(m.N_U, dtype=jnp.float64)
assert bool(jnp.all(jnp.isfinite(p)))
expected = jnp.zeros((8, 8), dtype=jnp.float64)
for cell, slot in slots.items():
    if cell in (1, 8):
        expected = expected.at[slot, slot].set(-1.0)
    else:
        expected = expected.at[slot, slots[cell-1]].set(1.0)
        expected = expected.at[slot, slot].set(-2.0)
        expected = expected.at[slot, slots[cell+1]].set(1.0)
reference = expected @ x
for value in [m.rhs(0.0, x, u, p), jax.jit(m.rhs)(0.0, x, u, p)]:
    assert bool(jnp.all(jnp.isfinite(value)))
    assert bool(jnp.allclose(value, reference, rtol=1e-12, atol=1e-12)), (value, reference)
for jacobian in [jax.jacfwd(m.rhs, argnums=1)(0.0, x, u, p),
                 jax.jacrev(m.rhs, argnums=1)(0.0, x, u, p)]:
    assert bool(jnp.all(jnp.isfinite(jacobian)))
    assert bool(jnp.allclose(jacobian, expected, rtol=1e-12, atol=1e-12))
seed = jnp.arange(1.0, 9.0)
_, tangent = jax.jvp(lambda state: m.rhs(0.0, state, u, p), (x,), (seed,))
assert bool(jnp.allclose(tangent, expected @ seed, rtol=1e-12, atol=1e-12))
"#,
    );
}

#[test]
fn issue_477_compact_stencil_and_lazy_scalar_boundary_keep_finite_ad() {
    let source = format!(
        r#"
function selectedRoot
  input Real u;
  output Real y;
algorithm
  if u > 0 then
    y := sqrt(u);
  else
    y := sqrt(-u);
  end if;
end selectedRoot;
{}
"#,
        heat_source(8).replace("der(u[1]) = -k*u[1];", "der(u[1]) = selectedRoot(u[1]);")
    );
    let compiled = Compiler::new()
        .model("Heat")
        .compile_str(&source, "mixed_heat.mo")
        .expect("mixed compact stencil and lazy boundary compile");
    let (directory, files) = rendered_model_target(&compiled, "Heat", "jax-ode");
    let module = files
        .iter()
        .find(|file| file.path.ends_with("_jax_ode.py"))
        .unwrap();
    assert!(
        module.content.contains("jnp.arange("),
        "compact family emitted"
    );
    assert!(
        module.content.contains("jax.lax.cond("),
        "lazy boundary emitted"
    );
    run_python(
        &directory.path().join(&module.path),
        r#"
import importlib.util, re, sys
import jax
import jax.numpy as jnp
spec = importlib.util.spec_from_file_location('generated', sys.argv[1])
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)
cells = [int(re.search(r'u\[(\d+)\]$', name).group(1)) for name in m.STATE_NAMES]
assert sorted(cells) == list(range(1, 9))
slots = {cell: slot for slot, cell in enumerate(cells)}
p = jnp.asarray(m.init_params(m.DEFAULT_P), dtype=jnp.float64)
u = jnp.zeros(m.N_U, dtype=jnp.float64)
f = lambda x: m.rhs(0.0, x, u, p)
for sign in [-1.0, 1.0]:
    x = jnp.array([float(cell) for cell in cells], dtype=jnp.float64).at[slots[1]].set(sign)
    matrix = jnp.zeros((8, 8), dtype=jnp.float64)
    for cell in range(2, 8):
        row = slots[cell]
        matrix = matrix.at[row, slots[cell-1]].set(1.0)
        matrix = matrix.at[row, row].set(-2.0)
        matrix = matrix.at[row, slots[cell+1]].set(1.0)
    matrix = matrix.at[slots[8], slots[8]].set(-1.0)
    reference = (matrix @ x).at[slots[1]].set(1.0)
    jacobian = matrix.at[slots[1], slots[1]].set(sign * 0.5)
    for actual in [f(x), jax.jit(f)(x)]:
        assert bool(jnp.all(jnp.isfinite(actual)))
        assert bool(jnp.allclose(actual, reference, rtol=1e-12, atol=1e-12))
    for actual in [jax.jacfwd(f)(x), jax.jacrev(f)(x)]:
        assert bool(jnp.all(jnp.isfinite(actual))), actual
        assert bool(jnp.allclose(actual, jacobian, rtol=1e-12, atol=1e-12)), actual
    gradient = jax.grad(lambda state: jnp.sum(f(state)))(x)
    assert bool(jnp.all(jnp.isfinite(gradient)))
    assert bool(jnp.allclose(gradient, jnp.sum(jacobian, axis=0), rtol=1e-12, atol=1e-12))
"#,
    );
}

#[test]
fn issue_477_two_dimensional_heat_matches_five_point_operator() {
    let source = r#"
model Heat2D
  parameter Integer NX=6;
  parameter Integer NY=5;
  Real u[NX,NY](each start=1.0);
equation
  for i in 1:NX loop
    der(u[i,1])=-u[i,1];
    der(u[i,NY])=-u[i,NY];
  end for;
  for j in 2:NY-1 loop
    der(u[1,j])=-u[1,j];
    der(u[NX,j])=-u[NX,j];
  end for;
  for i in 2:NX-1 loop
    for j in 2:NY-1 loop
      der(u[i,j])=u[i-1,j]+u[i+1,j]+u[i,j-1]+u[i,j+1]-4*u[i,j];
    end for;
  end for;
end Heat2D;
"#;
    let compiled = Compiler::new()
        .model("Heat2D")
        .compile_str(source, "Heat2D.mo")
        .expect("public two-dimensional heat pipeline");
    let (directory, files) = rendered_model_target(&compiled, "Heat2D", "jax-ode");
    let module = files
        .iter()
        .find(|file| file.path.ends_with("_jax_ode.py"))
        .unwrap();
    assert!(
        module.content.contains("jnp.arange("),
        "compact family emitted"
    );
    assert!(
        module.content.contains("_c1 ="),
        "rank-two interior must stay native"
    );
    run_python(
        &directory.path().join(&module.path),
        r#"
import importlib.util, re, sys
import jax
import jax.numpy as jnp
spec=importlib.util.spec_from_file_location('generated',sys.argv[1])
m=importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)
cells=[]
for name in m.STATE_NAMES:
    match=re.search(r'u\[(\d+),\s*(\d+)\]$',name)
    assert match is not None, name
    cells.append(tuple(map(int,match.groups())))
assert len(cells)==30 and set(cells)=={(i,j) for i in range(1,7) for j in range(1,6)}
slots={cell:slot for slot,cell in enumerate(cells)}
x=jnp.array([i*i+0.25*j*j+i*j for i,j in cells],dtype=jnp.float64)
p=jnp.asarray(m.init_params(m.DEFAULT_P),dtype=jnp.float64)
u=jnp.zeros(m.N_U,dtype=jnp.float64)
operator=jnp.zeros((30,30),dtype=jnp.float64)
for (i,j),slot in slots.items():
    if i in (1,6) or j in (1,5):
        operator=operator.at[slot,slot].set(-1.0)
    else:
        operator=operator.at[slot,slot].set(-4.0)
        for neighbor in [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]:
            operator=operator.at[slot,slots[neighbor]].set(1.0)
f=lambda state:m.rhs(0.0,state,u,p)
for actual in [f(x),jax.jit(f)(x)]:
    assert bool(jnp.all(jnp.isfinite(actual)))
    assert bool(jnp.allclose(actual,operator@x,rtol=1e-12,atol=1e-12)), actual
for actual in [jax.jacfwd(f)(x),jax.jacrev(f)(x)]:
    assert bool(jnp.all(jnp.isfinite(actual)))
    assert bool(jnp.allclose(actual,operator,rtol=1e-12,atol=1e-12)), actual
"#,
    );
}

#[test]
fn issue_477_single_interior_cell_matches_eager_jit_and_ad() {
    let compiled = Compiler::new()
        .model("Heat")
        .compile_str(&heat_source(3), "Heat.mo")
        .expect("three-cell Heat pipeline");
    let (directory, files) = rendered_model_target(&compiled, "Heat", "jax-ode");
    let module = files
        .iter()
        .find(|file| file.path.ends_with("_jax_ode.py"))
        .unwrap();
    run_python(
        &directory.path().join(&module.path),
        r#"
import importlib.util, re, sys
import jax
import jax.numpy as jnp
spec = importlib.util.spec_from_file_location('generated', sys.argv[1])
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)
cells = [int(re.search(r'u\[(\d+)\]$', name).group(1)) for name in m.STATE_NAMES]
assert sorted(cells) == [1, 2, 3]
slots = {cell: slot for slot, cell in enumerate(cells)}
x = jnp.asarray([0.25 + cell * cell for cell in cells], dtype=jnp.float64)
u = jnp.zeros(m.N_U, dtype=jnp.float64)
p = jnp.asarray(m.init_params(m.DEFAULT_P), dtype=jnp.float64)
a = jnp.zeros((3, 3), dtype=jnp.float64)
for cell in (1, 3):
    a = a.at[slots[cell], slots[cell]].set(-1.0)
for cell, weight in [(1, 1.0), (2, -2.0), (3, 1.0)]:
    a = a.at[slots[2], slots[cell]].set(weight)
f = lambda state: m.rhs(0.0, state, u, p)
for actual in [f(x), jax.jit(f)(x)]:
    assert actual.shape == (3,)
    assert bool(jnp.all(jnp.isfinite(actual)))
    assert bool(jnp.allclose(actual, a @ x, rtol=1e-12, atol=1e-12))
for actual in [jax.jacfwd(f)(x), jax.jacrev(f)(x)]:
    assert bool(jnp.all(jnp.isfinite(actual)))
    assert bool(jnp.allclose(actual, a, rtol=1e-12, atol=1e-12))
seed = jnp.asarray([1.0, 2.0, 3.0])
_, tangent = jax.jvp(f, (x,), (seed,))
assert bool(jnp.allclose(tangent, a @ seed, rtol=1e-12, atol=1e-12))
"#,
    );
}
