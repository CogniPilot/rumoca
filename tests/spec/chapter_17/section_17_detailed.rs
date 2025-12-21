//! MLS §17: State Machines - Detailed Conformance Tests
//!
//! Comprehensive tests covering normative statements from MLS §17 including:
//! - §17.1: Transitions between states
//! - §17.3: State machine semantics
//!
//! Reference: https://specification.modelica.org/master/state-machines.html

use crate::spec::expect_parse_success;

// ============================================================================
// §17.1 STATE MACHINE TRANSITIONS
// ============================================================================

/// MLS §17.1: State machine transitions
mod state_machine_transitions {
    use super::*;

    /// Basic state machine with transition
    #[test]
    fn mls_17_1_basic_state_machine() {
        expect_parse_success(
            r#"
            model StateMachine
                inner Integer state(start = 1);
            equation
                if state == 1 then
                    // State 1 behavior
                else
                    // State 2 behavior
                end if;
            end StateMachine;
            "#,
        );
    }

    /// State machine using block with inner/outer
    #[test]
    fn mls_17_1_block_based_state() {
        expect_parse_success(
            r#"
            block State1
                outer output Boolean active;
            equation
                active = true;
            end State1;
            "#,
        );
    }
}

// ============================================================================
// EVENT-DRIVEN STATE TRANSITIONS
// ============================================================================

/// Event-driven state transitions using when equations
mod event_state_transitions {
    use super::*;

    /// Integer state variable with when transitions
    #[test]
    fn event_integer_state() {
        expect_parse_success(
            r#"
            model EventStateMachine
                Real x(start = 0);
                discrete Integer state(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    state = 1;
                elsewhen x > 2 then
                    state = 2;
                elsewhen x > 3 then
                    state = 0;
                end when;
            end EventStateMachine;
            "#,
        );
    }

    /// Boolean state flags
    #[test]
    fn event_boolean_states() {
        expect_parse_success(
            r#"
            model BooleanStateMachine
                Real x(start = 0);
                discrete Boolean inState1(start = true);
                discrete Boolean inState2(start = false);
            equation
                der(x) = 1;
                when x > 1 then
                    inState1 = false;
                    inState2 = true;
                end when;
            end BooleanStateMachine;
            "#,
        );
    }

    /// Hysteresis-based state switching
    #[test]
    fn hysteresis_state() {
        expect_parse_success(
            r#"
            model Hysteresis
                Real x(start = 0);
                discrete Boolean high(start = false);
            equation
                der(x) = if high then -1 else 1;
                when x > 1 then
                    high = true;
                elsewhen x < -1 then
                    high = false;
                end when;
            end Hysteresis;
            "#,
        );
    }

    /// Counter-based state machine
    #[test]
    fn counter_state_machine() {
        expect_parse_success(
            r#"
            model CounterStateMachine
                Real x(start = 0);
                discrete Integer count(start = 0);
                discrete Integer state(start = 0);
            equation
                der(x) = 1;
                when sample(0, 1) then
                    count = pre(count) + 1;
                    state = mod(count, 3);
                end when;
            end CounterStateMachine;
            "#,
        );
    }
}

// ============================================================================
// STATE-DEPENDENT DYNAMICS
// ============================================================================

/// State-dependent dynamics
mod state_dependent_dynamics {
    use super::*;

    /// Switched system dynamics
    #[test]
    fn switched_dynamics() {
        expect_parse_success(
            r#"
            model SwitchedSystem
                Real x(start = 0);
                parameter Real threshold = 1;
            equation
                der(x) = if x < threshold then 1 else -1;
            end SwitchedSystem;
            "#,
        );
    }

    /// Multi-mode system
    #[test]
    fn multi_mode_system() {
        expect_parse_success(
            r#"
            model MultiMode
                Real x(start = 0);
                discrete Integer mode(start = 1);
            equation
                der(x) = if mode == 1 then 1
                         elseif mode == 2 then -1
                         else 0;
                when x > 2 then
                    mode = 2;
                elsewhen x < -2 then
                    mode = 1;
                end when;
            end MultiMode;
            "#,
        );
    }

    /// Bouncing ball (classic hybrid system)
    #[test]
    fn bouncing_ball() {
        expect_parse_success(
            r#"
            model BouncingBall
                parameter Real e = 0.8;
                parameter Real g = 9.81;
                Real h(start = 1);
                Real v(start = 0);
            equation
                der(h) = v;
                der(v) = -g;
                when h < 0 then
                    reinit(v, -e * pre(v));
                    reinit(h, 0);
                end when;
            end BouncingBall;
            "#,
        );
    }

    /// Thermostat with on/off control
    #[test]
    fn thermostat() {
        expect_parse_success(
            r#"
            model Thermostat
                parameter Real T_set = 20;
                parameter Real hysteresis = 1;
                Real T(start = 18);
                discrete Boolean heating(start = true);
                Real Q;
            equation
                der(T) = Q - 0.1 * (T - 15);
                Q = if heating then 2 else 0;
                when T > T_set + hysteresis then
                    heating = false;
                elsewhen T < T_set - hysteresis then
                    heating = true;
                end when;
            end Thermostat;
            "#,
        );
    }
}

// ============================================================================
// COMPLEX STATE MACHINE PATTERNS
// ============================================================================

/// Complex state machine patterns
mod complex_patterns {
    use super::*;

    /// State machine with timed transitions
    #[test]
    fn timed_transitions() {
        expect_parse_success(
            r#"
            model TimedStateMachine
                Real clock(start = 0);
                discrete Integer state(start = 0);
                discrete Real stateEntryTime(start = 0);
            equation
                der(clock) = 1;
                when clock - stateEntryTime > 1 then
                    state = mod(pre(state) + 1, 3);
                    stateEntryTime = clock;
                end when;
            end TimedStateMachine;
            "#,
        );
    }

    /// Finite state machine with guards
    #[test]
    fn fsm_with_guards() {
        expect_parse_success(
            r#"
            model FSMWithGuards
                Real x(start = 0);
                Real y(start = 0);
                discrete Integer state(start = 0);
            equation
                der(x) = 1;
                der(y) = if state == 0 then 0.5 else -0.5;
                when x > 1 and y > 0 then
                    state = 1;
                elsewhen x > 2 and y < 0 then
                    state = 0;
                end when;
            end FSMWithGuards;
            "#,
        );
    }

    /// Parallel state machines
    #[test]
    fn parallel_states() {
        expect_parse_success(
            r#"
            model ParallelStates
                Real x(start = 0);
                Real y(start = 0);
                discrete Integer stateA(start = 0);
                discrete Integer stateB(start = 0);
            equation
                der(x) = 1;
                der(y) = 2;
                when x > 1 then
                    stateA = 1;
                end when;
                when y > 1 then
                    stateB = 1;
                end when;
            end ParallelStates;
            "#,
        );
    }
}
