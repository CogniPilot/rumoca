//! INST-055 (extends-clause redeclaration) and INST-056 (extends-modified
//! package constants) contract tests - MLS §7.2, §7.3

use rumoca_contracts::test_support::expect_success;

fn flat_var_exists(result: &rumoca_compile::compile::CompilationResult, name: &str) -> bool {
    result
        .flat
        .variables
        .keys()
        .any(|var_name| var_name.as_str() == name)
}

// =============================================================================
// INST-055: Extends redeclaration replaces element
// "A redeclaration in the modification of an extends-clause replaces the
// inherited element; the derived class and its descendants see the replacing
// class under the element name"
// =============================================================================

#[test]
fn inst_055_extends_modifier_redeclared_record_is_the_derived_packages_record() {
    let result = expect_success(
        r#"
        partial package PM
            replaceable record State
            end State;
        end PM;
        record SR
            Real T;
            Real p;
        end SR;
        package P
            extends PM(redeclare record State = SR);
            function f
                input Real p;
                input Real T;
                output State s;
            algorithm
                s := State(p = p, T = T);
            end f;
        end P;
        package Q
            extends P;
        end Q;
        model Test
            Q.State s = Q.f(1, 2);
        end Test;
    "#,
        "Test",
    );
    assert!(flat_var_exists(&result, "s.T"));
    assert!(flat_var_exists(&result, "s.p"));
}

// =============================================================================
// INST-056: Extends-modified package constant in sibling bindings
// =============================================================================

#[test]
fn inst_056_sibling_binding_selects_from_modified_constant() {
    let trace = rumoca_contracts::test_support::simulate_model(
        r#"
        package G1
          function fit
            input Real u[:];
            input Real y[:];
            input Integer n;
            output Real p[n + 1];
          algorithm
            for i in 1:n + 1 loop
              p[i] := sum(y)/size(y, 1)/i;
            end for;
          end fit;
          function evaluate
            input Real p[:];
            input Real x;
            output Real y;
          algorithm
            y := 0;
            for i in 1:size(p, 1) loop
              y := y*x + p[i];
            end for;
          end evaluate;
          partial package TableBased
            constant Real[:, 2] tableDensity;
            constant Integer npol = 2;
            constant Boolean hasDensity = not (size(tableDensity, 1) == 0);
            final constant Real poly_rho[:] = if hasDensity then fit(tableDensity[:, 1], tableDensity[:, 2], npol) else zeros(npol + 1);
            function density
              input Real T;
              output Real d;
            algorithm
              d := evaluate(poly_rho, T);
            end density;
          end TableBased;
          package Glycol
            extends TableBased(tableDensity = [0, 1000; 10, 1010; 20, 1030]);
          end Glycol;
          model Top
            package Medium = Glycol;
            Real d = Medium.density(time);
          end Top;
        end G1;
    "#,
        "G1.Top",
        1.0,
    );
    let expected = 3040.0 / 3.0 * (1.0 + 0.5 + 1.0 / 3.0);
    assert!((trace.final_value("d") - expected).abs() < 1e-9);
}
