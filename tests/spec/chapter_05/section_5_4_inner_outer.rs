//! MLS §5.4: Inner/Outer Components
//!
//! Tests for inner/outer component matching including:
//! - Basic inner/outer declarations
//! - Matching rules
//! - Subtype requirements
//! - Modification restrictions
//!
//! Reference: https://specification.modelica.org/master/scoping-name-lookup-and-flattening.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §5.4 BASIC INNER/OUTER
// ============================================================================

/// MLS §5.4: Basic inner/outer usage
mod basic_inner_outer {
    use super::*;

    /// Simple inner declaration
    #[test]
    fn mls_5_4_inner_declaration() {
        expect_success(
            r#"
            model System
                inner Real g = 9.81;
                Component c;
            equation
            end System;

            model Component
                outer Real g;
                Real force;
            equation
                force = g;
            end Component;
            "#,
            "System",
        );
    }

    /// Inner parameter
    #[test]
    fn mls_5_4_inner_parameter() {
        expect_success(
            r#"
            model System
                inner parameter Real k = 10;
                SubSystem sub;
            equation
            end System;

            model SubSystem
                outer parameter Real k;
                Real x = k;
            equation
            end SubSystem;
            "#,
            "System",
        );
    }

    /// Nested hierarchy with inner/outer
    #[test]
    fn mls_5_4_nested_hierarchy() {
        expect_parse_success(
            r#"
            model Top
                inner Real environment = 1;
                Middle m;
            end Top;

            model Middle
                Bottom b;
            end Middle;

            model Bottom
                outer Real environment;
                Real local = environment;
            end Bottom;
            "#,
        );
    }

    /// Multiple outer references to same inner
    #[test]
    fn mls_5_4_multiple_outer_refs() {
        expect_parse_success(
            r#"
            model System
                inner Real shared = 5;
                Component c1;
                Component c2;
            end System;

            model Component
                outer Real shared;
                Real local = shared;
            end Component;
            "#,
        );
    }

    /// Inner component (not just scalar)
    #[test]
    fn mls_5_4_inner_component() {
        expect_parse_success(
            r#"
            record Environment
                Real gravity = 9.81;
                Real temperature = 293;
            end Environment;

            model System
                inner Environment env;
                Body body;
            end System;

            model Body
                outer Environment env;
                Real weight = env.gravity;
            end Body;
            "#,
        );
    }
}

// ============================================================================
// §5.4 MATCHING RULES
// ============================================================================

/// MLS §5.4: Inner/outer matching rules
mod matching_rules {
    use super::*;

    /// Nearest enclosing inner is used
    #[test]
    fn mls_5_4_nearest_inner() {
        expect_parse_success(
            r#"
            model Top
                inner Real x = 1;
                Middle m;
            end Top;

            model Middle
                inner Real x = 2;
                Bottom b;
            end Middle;

            model Bottom
                outer Real x;
                Real local = x;
            end Bottom;
            "#,
        );
    }

    /// Inner in intermediate level
    #[test]
    fn mls_5_4_intermediate_inner() {
        expect_parse_success(
            r#"
            model Level1
                Level2 l2;
            end Level1;

            model Level2
                inner Real value = 10;
                Level3 l3;
            end Level2;

            model Level3
                outer Real value;
                Real x = value;
            end Level3;
            "#,
        );
    }

    /// Multiple different inner/outer pairs
    #[test]
    fn mls_5_4_multiple_pairs() {
        expect_parse_success(
            r#"
            model System
                inner Real gravity = 9.81;
                inner Real temperature = 300;
                Component c;
            end System;

            model Component
                outer Real gravity;
                outer Real temperature;
                Real combined = gravity + temperature;
            end Component;
            "#,
        );
    }
}

// ============================================================================
// §5.5 SIMULTANEOUS INNER/OUTER
// ============================================================================

/// MLS §5.5: Simultaneous inner/outer declarations
mod simultaneous_inner_outer {
    use super::*;

    /// Component is both inner and outer
    #[test]
    fn mls_5_5_inner_outer_simultaneous() {
        expect_parse_success(
            r#"
            model Top
                inner Real x = 100;
                Middle m;
            end Top;

            model Middle
                inner outer Real x;
                Bottom b;
            end Middle;

            model Bottom
                outer Real x;
                Real local = x;
            end Bottom;
            "#,
        );
    }
}

// ============================================================================
// NEGATIVE TESTS - Inner/outer errors
// ============================================================================

/// Inner/outer error cases
mod inner_outer_errors {
    use super::*;

    /// Missing inner for outer (should fail)
    #[test]
    #[ignore = "Missing inner detection not yet implemented"]
    fn error_missing_inner() {
        expect_failure(
            r#"
            model Component
                outer Real undefined_inner;
                Real x = undefined_inner;
            end Component;
            "#,
            "Component",
        );
    }

    /// Outer shall not have modifications
    #[test]
    #[ignore = "Outer modification restriction not yet enforced"]
    fn error_outer_with_modification() {
        expect_failure(
            r#"
            model System
                inner Real x = 1;
                Component c;
            end System;

            model Component
                outer Real x = 5;
                Real y = x;
            end Component;
            "#,
            "System",
        );
    }

    /// Outer shall not have binding equation
    #[test]
    #[ignore = "Outer binding equation restriction not yet enforced"]
    fn error_outer_with_binding() {
        expect_failure(
            r#"
            model System
                inner Real x = 1;
                Component c;
            end System;

            model Component
                outer Real x(start = 0);
                Real y = x;
            end Component;
            "#,
            "System",
        );
    }

    /// Inner must be subtype of outer
    #[test]
    #[ignore = "Subtype checking for inner/outer not yet implemented"]
    fn error_inner_not_subtype() {
        expect_failure(
            r#"
            model System
                inner Integer x = 1;
                Component c;
            end System;

            model Component
                outer Real x;
                Real y = x;
            end Component;
            "#,
            "System",
        );
    }

    /// Outer with array modification
    #[test]
    #[ignore = "Outer modification restriction not yet enforced"]
    fn error_outer_array_modification() {
        expect_failure(
            r#"
            model System
                inner Real x[3] = {1, 2, 3};
                Component c;
            end System;

            model Component
                outer Real x[3] = {0, 0, 0};
                Real y = sum(x);
            end Component;
            "#,
            "System",
        );
    }
}

// ============================================================================
// PRACTICAL EXAMPLES
// ============================================================================

/// Practical inner/outer examples from real modeling
mod practical_examples {
    use super::*;

    /// Gravity in mechanical system
    #[test]
    fn example_gravity_system() {
        expect_parse_success(
            r#"
            model World
                inner Real g = 9.81;
                Body body1;
                Body body2;
            end World;

            model Body
                outer Real g;
                parameter Real m = 1;
                Real weight = m * g;
            end Body;
            "#,
        );
    }

    /// Environment record
    #[test]
    fn example_environment_record() {
        expect_parse_success(
            r#"
            record Environment
                Real T = 293.15;
                Real p = 101325;
            end Environment;

            model System
                inner Environment env;
                Subsystem sub;
            end System;

            model Subsystem
                outer Environment env;
                Real temp = env.T;
                Real pressure = env.p;
            end Subsystem;
            "#,
        );
    }

    /// Control system with shared parameters
    #[test]
    fn example_control_shared_params() {
        expect_parse_success(
            r#"
            model ControlSystem
                inner parameter Real sampleTime = 0.01;
                Controller ctrl;
                Plant plant;
            end ControlSystem;

            model Controller
                outer parameter Real sampleTime;
                Real T = sampleTime;
            end Controller;

            model Plant
                outer parameter Real sampleTime;
                Real dt = sampleTime;
            end Plant;
            "#,
        );
    }
}
