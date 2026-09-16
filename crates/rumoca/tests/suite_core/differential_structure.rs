//! Source-equation checks for SPEC_0007 / STRUCT-T07 differential analysis.

use rumoca::Compiler;
use rumoca_ir_dae as dae;
use rumoca_phase_structural::{EquationRef, analyze_differential_structure};

fn compile(source: &str, name: &str) -> std::sync::Arc<dae::Dae> {
    Compiler::new()
        .model(name)
        .compile_str(source, "differential_structure.mo")
        .unwrap()
        .dae
}

#[test]
fn rotation_rate_cancellation_has_two_formal_independent_coordinates() {
    let dae = compile(
        include_str!("../fixtures/index_reduction/RateCancellation.mo"),
        "RateCancellation",
    );
    let before = serde_json::to_vec(dae.as_ref()).unwrap();
    dae.inspect(|view| {
        let structure = analyze_differential_structure(view).unwrap();
        let states: Vec<_> = view
            .variables()
            .filter(|(_, v)| v.role() == dae::VariableRole::State)
            .map(|(_, v)| (v.name().to_string(), v.scalar_count()))
            .collect();
        assert_eq!(states, [("q".to_owned(), 3), ("w".to_owned(), 3)]);
        assert_eq!(structure.formal_dimension(), 2);
        assert_eq!(structure.equation_orders().iter().copied().max(), Some(2));
        assert_eq!(
            structure.variables().len(),
            structure.equation_orders().len()
        );
    });
    assert_eq!(
        serde_json::to_vec(dae.as_ref()).unwrap(),
        before,
        "analysis must not rewrite the source tensors or initial obligations"
    );
}

#[test]
fn pendulum_keeps_tensor_and_domain_equations_with_identical_offsets() {
    let source = "model Pendulum
        Real q[2]; Real v[2]; Real lambda;
        equation der(q)=v; der(v)=-lambda*q+{0,-1}; q*q=1;
        end Pendulum;";
    let variants = [
        source.to_owned(),
        source.replace("der(q)=v;", "for i in 1:2 loop der(q[i])=v[i]; end for;"),
    ];
    for source in variants {
        let dae = compile(&source, "Pendulum");
        dae.inspect(|view| {
            let structure = analyze_differential_structure(view).unwrap();
            assert_eq!(structure.formal_dimension(), 2);
            // Canonical DAE owners place the scalar constraint before families.
            assert_eq!(structure.equation_orders(), [2, 1, 1, 0, 0]);
            let named_orders: Vec<_> = structure
                .variables()
                .iter()
                .zip(structure.variable_orders())
                .map(|(coord, &order)| {
                    (
                        view.variable(coord.variable()).unwrap().name().to_string(),
                        coord.scalar(),
                        order,
                    )
                })
                .collect();
            assert_eq!(
                named_orders,
                [
                    ("q".into(), 0, 2),
                    ("q".into(), 1, 2),
                    ("v".into(), 0, 1),
                    ("v".into(), 1, 1),
                    ("lambda".into(), 0, 0)
                ]
            );
            assert_eq!(view.variable_count(), 3);
        });
    }
}

#[test]
fn output_algebraics_and_known_inputs_share_the_source_signature() {
    let dae = compile(
        "model Driven
        input Real u; parameter Real p=2;
        Real x; output Real y;
        equation der(x)=u-p*x; y=x*x;
        end Driven;",
        "Driven",
    );
    dae.inspect(|view| {
        let structure = analyze_differential_structure(view).unwrap();
        assert_eq!(structure.formal_dimension(), 1);
        assert_eq!(structure.equation_orders(), [0, 0]);
        assert_eq!(structure.variables().len(), 2);
        let names: Vec<_> = structure
            .signature_row(EquationRef(0))
            .unwrap()
            .map(|(coord, order)| {
                (
                    view.variable(coord.variable()).unwrap().name().to_string(),
                    order,
                )
            })
            .collect();
        assert_eq!(names, [("x".to_owned(), 1)]);
    });
}

#[test]
fn algebraic_system_has_no_formal_integration_coordinates() {
    let dae = compile(
        "model Algebraic Real x; Real y; equation x+y=1; x-y=2; end Algebraic;",
        "Algebraic",
    );
    dae.inspect(|view| {
        let structure = analyze_differential_structure(view).unwrap();
        assert_eq!(structure.formal_dimension(), 0);
        assert_eq!(structure.equation_orders(), [0, 0]);
        assert_eq!(structure.variable_orders(), [0, 0]);
    });
}

#[test]
fn graph_certificate_does_not_claim_numerical_regularity() {
    // Differentiating x=y repeats the first equation. The signature certificate
    // exists but cannot authorize state reduction without a numerical rank check.
    let dae = compile(
        "model Singular Real x; Real y; equation der(x)=der(y); x=y; end Singular;",
        "Singular",
    );
    dae.inspect(|view| {
        let structure = analyze_differential_structure(view).unwrap();
        assert_eq!(structure.formal_dimension(), 1);
        assert_eq!(structure.equation_orders(), [0, 1]);
        assert_eq!(structure.variable_orders(), [1, 1]);
    });
}

#[test]
fn function_permutation_preserves_exact_derivative_coordinates() {
    let dae = compile(
        "function permute
        input Real a[2]; output Real b[2];
        algorithm b := {a[2],a[1]}; end permute;
        model Permuted Real x[2];
        equation permute(der(x))={x[2],x[1]}; end Permuted;",
        "Permuted",
    );
    dae.inspect(|view| {
        let structure = analyze_differential_structure(view).unwrap();
        assert_eq!(structure.formal_dimension(), 2);
        for (row, scalar) in [(0, 1), (1, 0)] {
            let matched = structure.matched_variable(EquationRef(row)).unwrap();
            assert_eq!(matched.scalar(), scalar);
            assert_eq!(
                view.variable(matched.variable()).unwrap().name().as_str(),
                "x"
            );
            let entries: Vec<_> = structure
                .signature_row(EquationRef(row))
                .unwrap()
                .map(|(coord, order)| (coord.scalar(), order))
                .collect();
            assert_eq!(entries, [(scalar, 1)]);
        }
    });
}

#[test]
fn only_invariant_zero_can_remove_a_derivative_edge() {
    for (qualifier, dimension) in [("", 1), ("final", 0)] {
        let dae = compile(
            &format!(
                "model ZeroCoefficient
            {qualifier} parameter Real a=0; Real x;
            equation a*der(x)+x=1; end ZeroCoefficient;"
            ),
            "ZeroCoefficient",
        );
        dae.inspect(|view| {
            let structure = analyze_differential_structure(view).unwrap();
            assert_eq!(structure.formal_dimension(), dimension);
        });
    }
}

#[test]
fn incomplete_matching_remains_a_typed_structural_failure() {
    for equations in ["der(x)=1; der(x)=2;", "x=1; x=2;"] {
        let dae = compile(
            &format!(
                "model Unmatched Real x; Real y;
            equation {equations} end Unmatched;"
            ),
            "Unmatched",
        );
        dae.inspect(|view| {
            let error = analyze_differential_structure(view).unwrap_err();
            let rumoca_phase_structural::StructuralError::Singular {
                n_equations,
                n_unknowns,
                n_matched,
                unmatched_unknowns,
                ..
            } = error
            else {
                panic!("unexpected error: {error:?}")
            };
            assert_eq!((n_equations, n_unknowns, n_matched), (2, 2, 1));
            assert_eq!(unmatched_unknowns, ["y (scalar 0)"]);
        });
    }
}

#[test]
fn rotation_offsets_extend_to_whole_tensors_without_changing_formal_dimension() {
    let dae = compile(
        include_str!("../fixtures/index_reduction/RateCancellation.mo"),
        "RateCancellation",
    );
    let before = serde_json::to_vec(dae.as_ref()).unwrap();
    dae.inspect(|view| {
        let source = analyze_differential_structure(view).unwrap();
        let refined = source
            .tensor_offsets(view)
            .unwrap()
            .expect("rotation has a whole-tensor derivative representation");
        assert!(std::ptr::eq(refined.source(), &source));
        assert_eq!(refined.source().formal_dimension(), 2);
        assert_eq!(
            refined
                .variable_orders()
                .iter()
                .map(|v| *v as usize + 1)
                .sum::<usize>(),
            131
        );
        assert_eq!(
            refined
                .equation_orders()
                .iter()
                .map(|v| *v as usize + 1)
                .sum::<usize>(),
            129
        );
        for (coordinate, &order) in source.variables().iter().zip(refined.variable_orders()) {
            let name = view
                .variable(coordinate.variable())
                .unwrap()
                .name()
                .as_str();
            let expected = match name {
                "q" | "Rx" | "Ry" | "Rz" | "R" => 2,
                "w" | "rate" => 1,
                "ax" | "az" => 0,
                _ => panic!("unexpected source variable {name}"),
            };
            assert_eq!(order, expected, "{name}[{}]", coordinate.scalar());
        }
    });
    assert_eq!(serde_json::to_vec(dae.as_ref()).unwrap(), before);
}

#[test]
fn a_tensor_with_incompatible_component_orders_keeps_its_scalar_analysis() {
    let dae = compile(
        "model MixedOrders Real x[2]; equation {der(x[1]),x[2]}={x[2],0}; end MixedOrders;",
        "MixedOrders",
    );
    dae.inspect(|view| {
        let source = analyze_differential_structure(view).unwrap();
        assert_eq!(source.formal_dimension(), 1);
        assert!(source.tensor_offsets(view).unwrap().is_none());
        assert_eq!(source.variable_orders(), [1, 0]);
    });
}
