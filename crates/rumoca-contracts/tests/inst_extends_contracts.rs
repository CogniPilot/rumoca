//! INST-055 (extends-clause redeclaration) contract tests - MLS §7.3

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
