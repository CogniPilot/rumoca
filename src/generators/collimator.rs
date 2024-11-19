use crate::ast;

use tera::{Context, Tera};

static COLLIMATOR_TEMPLATE: &str = r#"

import jax.numpy as jnp
from collimator.framework import LeafSystem

{% for class in def.classes %}
class {{ class.name}} (LeafSystem):
    def __init__(
        self,
        *args,
        x0=[1.0, 0.0],
        m=1.0,
        g=9.81,
        L=1.0,
        b=0.0,
        **kwargs,
    ):
        super().__init__(*args, **kwargs)
        # Declare parameters
        # self.declare_dynamic_parameter("m", m)
        # self.declare_dynamic_parameter("g", g)
        # self.declare_dynamic_parameter("L", L)
        # self.declare_dynamic_parameter("b", b)

        self.declare_continuous_state(default_value=jnp.array(x0), ode=self.ode)

        {% for comp in class.components -%}
        self.{{ comp.name }} = ca.SX.sym('{{ comp.name }}');
        {% endfor -%}

        # Declare input port for the torque
        self.declare_input_port(name="u")

        self.declare_continuous_state_output(name="x")


    def ode(self, time, state, *inputs, **parameters):
        # Get theta and omega from the continuous part of LeafSystem state
        theta, omega = state.continuous_state

        # Get parameters
        m = parameters["m"]
        g = parameters["g"]
        L = parameters["L"]
        b = parameters["b"]

        # Get input
        tau = inputs[0]

        # Reshape to scalar if input was an array
        tau = jnp.reshape(tau, ())

        # Compute the time derivative of the state (ODE RHS)
        dot_theta = omega
        mLsq = m * L * L
        dot_omega = -(g / L) * jnp.sin(theta) - b * omega / mLsq + tau / mLsq

        # Return the derivative of the state
        return jnp.array([dot_theta, dot_omega]

{% endfor %}

"#;

pub fn generate(def: &ast::StoredDefinition) {
    let mut tera = Tera::default();
    tera.add_raw_template("template", COLLIMATOR_TEMPLATE)
        .unwrap();
    let mut context = Context::new();
    context.insert("def", def);
    println!("{}", tera.render("template", &context).unwrap());
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::generators::parse_file;

    #[test]
    fn test_generate_casadi_sx() {
        let def = parse_file("src/model.mo").expect("failed to parse");
        generate(&def);
    }
}
