//! Regression coverage for structural `Integer` parameters used as array
//! dimensions (MLS §4.5, §12.4.1).
//!
//! `Context::eval_component_dim_subscript` rejects a component whose declared
//! dimension expression cannot be folded to an integer, and reports it as
//! `EF010 unresolved component dimension`. The dimension therefore has to be
//! reachable through `Context::parameter_values`, which is seeded from the
//! instance overlay produced by the instantiate phase.
//!
//! The MSL shapes that exercise this are `Modelica.Blocks.Interfaces.MO` and
//! its subclasses: `nout` is declared in the base class, bound by a *final
//! modification on the extends clause* of the derived class, and the binding
//! calls `size()` on arrays that an *enclosing* component modifies. Every step
//! in that chain has to survive instantiation for the flatten phase to size
//! `y[nout]`.
//!
//! Twelve MSL models (`Modelica.Fluid.Examples.ControlledTankSystem.Controlled\
//! Tanks`, `Modelica.Blocks.Examples.Interaction1`, the
//! `Modelica.Clocked.Examples.CascadeControlledDrive.*` family,
//! `Modelica.Mechanics.MultiBody.Examples.Systems.RobotR3.*`, ...) regressed to
//! `EF010 ... : nout` when this propagation broke, so the two shapes below are
//! kept as a permanent guard.

use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

/// Parse, resolve, instantiate and flatten `source`, returning the flat model.
fn flatten_source(source: &str, model: &str) -> flat::Model {
    let file_name = "<structural_integer_dimensions>";
    let stored = rumoca_phase_parse::parse_to_ast(source, file_name).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let instanced =
        rumoca_phase_instantiate::instantiate(resolved, model).expect("model instantiates");
    // `flatten_ref` (not `flatten`) is the entry the driver uses: it carries the
    // simulated root name, which enclosing-class constant injection needs.
    rumoca_phase_flatten::flatten_ref(instanced.inner(), instanced.overlay(), model)
        .expect("model flattens")
}

/// Resolved dimensions of the flat variable named `name`.
fn dims_of(model: &flat::Model, name: &str) -> Vec<i64> {
    model
        .variables
        .iter()
        .find(|(var_name, _)| var_name.as_str() == name)
        .map(|(_, var)| var.dims.clone())
        .unwrap_or_else(|| {
            let known: Vec<&str> = model.variables.keys().map(|key| key.as_str()).collect();
            panic!("no flat variable `{name}`; got {known:?}")
        })
}

/// `Modelica.Blocks.Sources.CombiTimeTable` shape: the structural integer is
/// bound by a `final` modification on the extends clause, from `size()` of an
/// array whose value comes from an enclosing component's modification.
const EXTENDS_CLAUSE_FINAL_MODIFICATION: &str = r#"
    package Lib
        partial block MO
            parameter Integer nout(min = 1) = 1 "Number of outputs";
            output Real y[nout];
        end MO;

        block Table
            extends MO(final nout = max([size(columns, 1); size(offset, 1)]));
            parameter Integer columns[:] = {2, 3};
            parameter Real offset[:] = {0.0};
        equation
            for i in 1:nout loop
                y[i] = time + offset[1];
            end for;
        end Table;

        block Wrapper
            Table table(columns = {2, 3, 4});
        end Wrapper;

        model Top
            Wrapper stop;
            Real z;
        equation
            z = sum(stop.table.y);
        end Top;
    end Lib;
"#;

#[test]
fn extends_clause_final_modification_sizes_nested_array() {
    let model = flatten_source(EXTENDS_CLAUSE_FINAL_MODIFICATION, "Lib.Top");
    // `columns = {2, 3, 4}` is supplied two component levels above the
    // declaration of `y`, so `nout` is 3, not the base-class default of 1.
    assert_eq!(dims_of(&model, "stop.table.y"), vec![3]);
}

/// `Modelica.Blocks.Sources.KinematicPTP2` shape: the structural integer is a
/// `final parameter` declared in the same class, bound from `size()` of array
/// parameters that the enclosing component modifies. This is the shape behind
/// `unresolved component dimension for pathPlanning.path.q: nout`.
const LOCAL_FINAL_PARAMETER: &str = r"
    package Lib
        block Ptp
            parameter Real q_begin[:] = {0};
            parameter Real q_end[:] = {1};
            parameter Real qd_max[:] = {1};
            final parameter Integer nout = max([size(q_begin, 1); size(q_end, 1); size(qd_max, 1)]);
            output Real q[nout];
        equation
            for i in 1:nout loop
                q[i] = time;
            end for;
        end Ptp;

        block PathPlanning
            parameter Integer naxis = 2;
            parameter Real angleEnd[naxis] = {1, 2};
            Ptp path(q_end = angleEnd, q_begin = zeros(naxis), qd_max = ones(naxis));
        end PathPlanning;

        model Top
            PathPlanning pathPlanning;
            Real w;
        equation
            w = sum(pathPlanning.path.q);
        end Top;
    end Lib;
";

#[test]
fn local_final_parameter_from_size_sizes_nested_array() {
    let model = flatten_source(LOCAL_FINAL_PARAMETER, "Lib.Top");
    assert_eq!(dims_of(&model, "pathPlanning.path.q"), vec![2]);
}

/// The same structural integer, but reached through a *component array*.
///
/// Compact array instantiation (SPEC_0032 §1) instantiates one template element
/// and derives the rest by re-keying the template subtree, so the derived
/// elements never re-run class-level parameter extraction. Every element still
/// has to end up with a resolvable `nout`; if only the template element keeps
/// one, flatten reports `EF010` for `bank[2]`/`bank[3]` alone, which is exactly
/// the partial-failure signature the MSL `nout` cluster showed.
const COMPONENT_ARRAY_OF_STRUCTURAL_INTEGERS: &str = r#"
    package Lib
        partial block MO
            parameter Integer nout(min = 1) = 1 "Number of outputs";
            output Real y[nout];
        end MO;

        block Table
            extends MO(final nout = size(columns, 1));
            parameter Integer columns[:] = {2, 3};
        equation
            for i in 1:nout loop
                y[i] = time * columns[i];
            end for;
        end Table;

        block Holder
            Table table(columns = {2, 3, 4});
        end Holder;

        model Top
            Holder bank[3];
            Real z;
        equation
            z = sum(bank[1].table.y) + sum(bank[2].table.y) + sum(bank[3].table.y);
        end Top;
    end Lib;
"#;

#[test]
fn replicated_array_elements_keep_their_structural_integer() {
    let model = flatten_source(COMPONENT_ARRAY_OF_STRUCTURAL_INTEGERS, "Lib.Top");
    // Every derived element, not just the template element `bank[1]`.
    for index in 1..=3 {
        assert_eq!(
            dims_of(&model, &format!("bank[{index}].table.y")),
            vec![3],
            "derived element bank[{index}] lost its structural integer `nout`"
        );
    }
}
