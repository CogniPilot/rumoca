//! MLS §9.4: Overconstrained Connections
//!
//! Tests for overconstrained connection handling including:
//! - Overconstrained connector roots
//! - Connection graph handling
//! - Branch definitions
//! - Equalityconstraint functions
//!
//! Reference: https://specification.modelica.org/master/connectors-connections.html

use crate::spec::expect_parse_success;

// ============================================================================
// §9.4.1 OVERCONSTRAINED BASICS
// ============================================================================

/// MLS §9.4: Overconstrained connector basics
mod overconstrained_basics {
    use super::*;

    /// MLS: Connector with isRoot attribute
    #[test]
    fn mls_9_4_isroot_attribute() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3] "Position";
                Real R[3, 3] "Orientation matrix";
                flow Real f[3] "Force";
                flow Real t[3] "Torque";
            end Frame;

            model RigidBody
                Frame frame;
            equation
                Connections.root(frame);
            end RigidBody;
            "#,
        );
    }

    /// MLS: Connections.root usage
    #[test]
    fn mls_9_4_connections_root() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real R[3, 3];
                flow Real f[3];
                flow Real t[3];
            end Frame;

            model World
                Frame frame;
            equation
                Connections.root(frame);
                frame.r = {0, 0, 0};
            end World;
            "#,
        );
    }

    /// MLS: Connections.potentialRoot usage
    #[test]
    fn mls_9_4_potential_root() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real R[3, 3];
                flow Real f[3];
                flow Real t[3];
            end Frame;

            model Body
                Frame frame;
            equation
                Connections.potentialRoot(frame);
            end Body;
            "#,
        );
    }

    /// MLS: Connections.branch usage
    #[test]
    fn mls_9_4_connections_branch() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real R[3, 3];
                flow Real f[3];
                flow Real t[3];
            end Frame;

            model Joint
                Frame frame_a;
                Frame frame_b;
            equation
                Connections.branch(frame_a, frame_b);
            end Joint;
            "#,
        );
    }
}

// ============================================================================
// §9.4.2 OVERCONSTRAINED CONNECTOR PATTERNS
// ============================================================================

/// MLS §9.4: Overconstrained connector patterns
mod overconstrained_patterns {
    use super::*;

    /// MLS: Multibody frame connector
    #[test]
    fn mls_9_4_multibody_frame() {
        expect_parse_success(
            r#"
            connector Frame
                Real r_0[3] "Position vector";
                Real R[3, 3] "Rotation matrix";
                flow Real f[3] "Cut force";
                flow Real t[3] "Cut torque";
            end Frame;
            "#,
        );
    }

    /// MLS: Planar mechanics connector
    #[test]
    fn mls_9_4_planar_connector() {
        expect_parse_success(
            r#"
            connector PlanarFrame
                Real x;
                Real y;
                Real phi "Rotation angle";
                flow Real fx;
                flow Real fy;
                flow Real t;
            end PlanarFrame;
            "#,
        );
    }

    /// MLS: World reference frame
    #[test]
    fn mls_9_4_world_frame() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real R[3, 3];
                flow Real f[3];
                flow Real t[3];
            end Frame;

            model World
                Frame frame;
            equation
                Connections.root(frame);
                frame.r = zeros(3);
                frame.R = identity(3);
            end World;
            "#,
        );
    }
}

// ============================================================================
// §9.4.3 ROOT SELECTION
// ============================================================================

/// MLS §9.4: Root selection mechanisms
mod root_selection {
    use super::*;

    /// MLS: Priority-based root selection
    #[test]
    fn mls_9_4_root_priority() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real R[3, 3];
                flow Real f[3];
                flow Real t[3];
            end Frame;

            model Body
                Frame frame;
                parameter Integer priority = 0;
            equation
                Connections.potentialRoot(frame, priority);
            end Body;
            "#,
        );
    }

    /// MLS: Connections.isRoot query
    #[test]
    fn mls_9_4_isroot_query() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real R[3, 3];
                flow Real f[3];
                flow Real t[3];
            end Frame;

            model Body
                Frame frame;
                Boolean isWorldRoot;
            equation
                Connections.potentialRoot(frame);
                isWorldRoot = Connections.isRoot(frame);
            end Body;
            "#,
        );
    }

    /// MLS: Conditional root assignment
    #[test]
    fn mls_9_4_conditional_root() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real R[3, 3];
                flow Real f[3];
                flow Real t[3];
            end Frame;

            model Body
                Frame frame;
                parameter Boolean isWorld = false;
            equation
                if isWorld then
                    Connections.root(frame);
                else
                    Connections.potentialRoot(frame);
                end if;
            end Body;
            "#,
        );
    }
}

// ============================================================================
// §9.4.4 BRANCH HANDLING
// ============================================================================

/// MLS §9.4: Branch handling in connection graphs
mod branch_handling {
    use super::*;

    /// MLS: Simple branch between frames
    #[test]
    fn mls_9_4_simple_branch() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real R[3, 3];
                flow Real f[3];
                flow Real t[3];
            end Frame;

            model RigidBar
                Frame frame_a;
                Frame frame_b;
                parameter Real L = 1 "Length";
            equation
                Connections.branch(frame_a, frame_b);
                frame_b.r = frame_a.r + L * {1, 0, 0};
                frame_b.R = frame_a.R;
            end RigidBar;
            "#,
        );
    }

    /// MLS: Joint with branch
    #[test]
    fn mls_9_4_joint_branch() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real R[3, 3];
                flow Real f[3];
                flow Real t[3];
            end Frame;

            model RevoluteJoint
                Frame frame_a;
                Frame frame_b;
                Real phi "Rotation angle";
            equation
                Connections.branch(frame_a, frame_b);
                frame_b.r = frame_a.r;
            end RevoluteJoint;
            "#,
        );
    }

    /// MLS: Chain of branches
    #[test]
    fn mls_9_4_branch_chain() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real R[3, 3];
                flow Real f[3];
                flow Real t[3];
            end Frame;

            model Link
                Frame frame_a;
                Frame frame_b;
            equation
                Connections.branch(frame_a, frame_b);
            end Link;

            model Chain
                Frame frame_a;
                Frame frame_b;
                Link link1, link2, link3;
            equation
                connect(frame_a, link1.frame_a);
                connect(link1.frame_b, link2.frame_a);
                connect(link2.frame_b, link3.frame_a);
                connect(link3.frame_b, frame_b);
            end Chain;
            "#,
        );
    }
}

// ============================================================================
// §9.4.5 EQUALITY CONSTRAINT FUNCTION
// ============================================================================

/// MLS §9.4: Equality constraint functions
mod equality_constraint {
    use super::*;

    /// MLS: Frame equalityConstraint function
    #[test]
    #[ignore = "Operator functions in connectors not yet implemented"]
    fn mls_9_4_equality_constraint() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real R[3, 3];
                flow Real f[3];
                flow Real t[3];
            equation
                function equalityConstraint
                    input Frame frame1;
                    input Frame frame2;
                    output Real residue[12];
                algorithm
                    residue[1:3] := frame1.r - frame2.r;
                    residue[4:12] := reshape(frame1.R - frame2.R, {9});
                end equalityConstraint;
            end Frame;
            "#,
        );
    }
}

// ============================================================================
// MULTIBODY EXAMPLES
// ============================================================================

/// Multibody mechanics examples
mod multibody_examples {
    use super::*;

    /// Simple pendulum setup
    #[test]
    fn multibody_pendulum_setup() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real R[3, 3];
                flow Real f[3];
                flow Real t[3];
            end Frame;

            model World
                Frame frame;
            equation
                Connections.root(frame);
                frame.r = {0, 0, 0};
                frame.R = identity(3);
            end World;

            model Body
                Frame frame;
                parameter Real m = 1;
            equation
                Connections.potentialRoot(frame);
            end Body;

            model FixedTranslation
                Frame frame_a;
                Frame frame_b;
                parameter Real r[3] = {1, 0, 0};
            equation
                Connections.branch(frame_a, frame_b);
                frame_b.r = frame_a.r + r;
                frame_b.R = frame_a.R;
            end FixedTranslation;

            model Pendulum
                World world;
                FixedTranslation bar(r = {0.5, 0, 0});
                Body ball(m = 1);
            equation
                connect(world.frame, bar.frame_a);
                connect(bar.frame_b, ball.frame);
            end Pendulum;
            "#,
        );
    }

    /// Double link mechanism
    #[test]
    fn multibody_double_link() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real R[3, 3];
                flow Real f[3];
                flow Real t[3];
            end Frame;

            model World
                Frame frame;
            equation
                Connections.root(frame);
                frame.r = zeros(3);
                frame.R = identity(3);
            end World;

            model Link
                Frame frame_a;
                Frame frame_b;
            equation
                Connections.branch(frame_a, frame_b);
            end Link;

            model Mechanism
                World world;
                Link link1;
                Link link2;
            equation
                connect(world.frame, link1.frame_a);
                connect(link1.frame_b, link2.frame_a);
            end Mechanism;
            "#,
        );
    }
}

// ============================================================================
// PLANAR MECHANICS EXAMPLES
// ============================================================================

/// Planar mechanics examples
mod planar_examples {
    use super::*;

    /// Planar world
    #[test]
    fn planar_world() {
        expect_parse_success(
            r#"
            connector PlanarFrame
                Real x;
                Real y;
                Real phi;
                flow Real fx;
                flow Real fy;
                flow Real t;
            end PlanarFrame;

            model PlanarWorld
                PlanarFrame frame;
            equation
                Connections.root(frame);
                frame.x = 0;
                frame.y = 0;
                frame.phi = 0;
            end PlanarWorld;
            "#,
        );
    }

    /// Planar body
    #[test]
    fn planar_body() {
        expect_parse_success(
            r#"
            connector PlanarFrame
                Real x;
                Real y;
                Real phi;
                flow Real fx;
                flow Real fy;
                flow Real t;
            end PlanarFrame;

            model PlanarBody
                PlanarFrame frame;
                parameter Real m = 1;
                parameter Real I = 1;
            equation
                Connections.potentialRoot(frame);
                m * der(der(frame.x)) = frame.fx;
                m * der(der(frame.y)) = frame.fy;
                I * der(der(frame.phi)) = frame.t;
            end PlanarBody;
            "#,
        );
    }

    /// Planar bar
    #[test]
    fn planar_bar() {
        expect_parse_success(
            r#"
            connector PlanarFrame
                Real x;
                Real y;
                Real phi;
                flow Real fx;
                flow Real fy;
                flow Real t;
            end PlanarFrame;

            model PlanarBar
                PlanarFrame frame_a;
                PlanarFrame frame_b;
                parameter Real L = 1;
            equation
                Connections.branch(frame_a, frame_b);
                frame_b.x = frame_a.x + L * cos(frame_a.phi);
                frame_b.y = frame_a.y + L * sin(frame_a.phi);
                frame_b.phi = frame_a.phi;
            end PlanarBar;
            "#,
        );
    }
}
