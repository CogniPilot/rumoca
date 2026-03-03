//! State Machine structures (MLS §17).
//!
//! This module provides data structures for Modelica state machines:
//! - State definitions and attributes
//! - Transitions between states
//! - State machine runtime state

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use crate::Expression;
use crate::instance::QualifiedName;

// =============================================================================
// State Machine Definition (MLS §17.1)
// =============================================================================

/// MLS §17.1: State Machine.
///
/// A state machine is a hierarchical construct where states are represented
/// by instances (blocks) and transitions define state changes.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct StateMachine {
    /// Unique identifier for this state machine.
    pub id: u32,
    /// States in this state machine.
    pub states: IndexMap<String, State>,
    /// Transitions between states.
    pub transitions: Vec<Transition>,
    /// The initial state (exactly one state must be marked initial).
    pub initial_state: Option<String>,
    /// Qualified name of the state machine instance.
    pub qualified_name: QualifiedName,
}

impl StateMachine {
    /// Create a new state machine.
    pub fn new(id: u32, qualified_name: QualifiedName) -> Self {
        Self {
            id,
            qualified_name,
            ..Default::default()
        }
    }

    /// Add a state to this state machine.
    pub fn add_state(&mut self, name: String, state: State) {
        if state.is_initial && self.initial_state.is_none() {
            self.initial_state = Some(name.clone());
        }
        self.states.insert(name, state);
    }

    /// Add a transition to this state machine.
    pub fn add_transition(&mut self, transition: Transition) {
        self.transitions.push(transition);
    }

    /// Get the number of states.
    pub fn num_states(&self) -> usize {
        self.states.len()
    }

    /// Get the number of transitions.
    pub fn num_transitions(&self) -> usize {
        self.transitions.len()
    }
}

/// MLS §17.1: A state in a state machine.
///
/// States are represented by block instances with optional nested state machines.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct State {
    /// State name.
    pub name: String,
    /// Whether this is the initial state (from initialState()).
    /// MLS §17.1: "One and only one instance in each state machine must be marked as initial."
    pub is_initial: bool,
    /// Nested state machine (for hierarchical state machines).
    pub nested_machine: Option<Box<StateMachine>>,
    /// Reference to the block instance for this state.
    pub block_instance: QualifiedName,
}

impl State {
    /// Create a new state.
    pub fn new(name: String, block_instance: QualifiedName) -> Self {
        Self {
            name,
            block_instance,
            ..Default::default()
        }
    }

    /// Mark this state as initial.
    pub fn set_initial(mut self) -> Self {
        self.is_initial = true;
        self
    }
}

// =============================================================================
// Transition (MLS §17.1)
// =============================================================================

/// MLS §17.1: Transition Record.
///
/// "transition(from, to, condition, immediate, reset, synchronize, priority)"
///
/// Defines a transition between two states with associated condition and properties.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Transition {
    /// Source state name.
    pub from: String,
    /// Target state name.
    pub to: String,
    /// Transition condition (Boolean expression).
    pub condition: Expression,
    /// Whether the transition is immediate (fires at the same instant condition becomes true).
    /// Default: true
    pub immediate: bool,
    /// Whether the target state is reset when the transition fires.
    /// Default: true
    pub reset: bool,
    /// Whether to synchronize with sub-state machines of the target state.
    /// Default: false
    pub synchronize: bool,
    /// Priority of this transition (MLS §17.1: "priority >= 1").
    /// Lower number = higher priority.
    /// Default: 1
    pub priority: i32,
}

impl Transition {
    /// Create a new transition with default settings.
    pub fn new(from: String, to: String, condition: Expression) -> Self {
        Self {
            from,
            to,
            condition,
            immediate: true,
            reset: true,
            synchronize: false,
            priority: 1,
        }
    }

    /// Set the immediate flag.
    pub fn with_immediate(mut self, immediate: bool) -> Self {
        self.immediate = immediate;
        self
    }

    /// Set the reset flag.
    pub fn with_reset(mut self, reset: bool) -> Self {
        self.reset = reset;
        self
    }

    /// Set the synchronize flag.
    pub fn with_synchronize(mut self, synchronize: bool) -> Self {
        self.synchronize = synchronize;
        self
    }

    /// Set the priority.
    pub fn with_priority(mut self, priority: i32) -> Self {
        self.priority = priority.max(1); // MLS §17.1: "priority >= 1"
        self
    }
}

impl Default for Transition {
    fn default() -> Self {
        Self {
            from: String::new(),
            to: String::new(),
            condition: Expression::Empty,
            immediate: true,
            reset: true,
            synchronize: false,
            priority: 1,
        }
    }
}

// =============================================================================
// State Machine Runtime State (MLS §17.2)
// =============================================================================

/// MLS §17.2: State Machine State Variables.
///
/// These discrete-time variables track the state machine's runtime state.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct StateMachineState {
    /// Currently active state index.
    /// MLS §17.2: "activeState"
    pub active_state: i32,
    /// Determined next state index.
    /// MLS §17.2: "nextState"
    pub next_state: i32,
    /// Active reset status.
    /// MLS §17.2: "activeReset"
    pub active_reset: bool,
    /// Per-state reset flags.
    /// MLS §17.2: `"activeResetStates[:]"`.
    pub active_reset_states: Vec<bool>,
    /// Whether the state machine is in a final state.
    /// MLS §17.2: "stateMachineInFinalState" - no outgoing transitions can fire.
    pub in_final_state: bool,
}

impl StateMachineState {
    /// Create a new state machine state for n states.
    pub fn new(num_states: usize) -> Self {
        Self {
            active_state: 0,
            next_state: 0,
            active_reset: false,
            active_reset_states: vec![false; num_states],
            in_final_state: false,
        }
    }

    /// Reset the state machine to initial state.
    pub fn reset(&mut self, initial_state: i32) {
        self.active_state = initial_state;
        self.next_state = initial_state;
        self.active_reset = true;
        for flag in &mut self.active_reset_states {
            *flag = true;
        }
        self.in_final_state = false;
    }
}

// =============================================================================
// State Machine Collection
// =============================================================================

/// Collection of all state machines in a model.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct StateMachines {
    /// All state machines by ID.
    pub machines: IndexMap<u32, StateMachine>,
    /// Next available ID.
    next_id: u32,
}

impl StateMachines {
    /// Create a new empty collection.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a state machine and return its ID.
    pub fn add(&mut self, machine: StateMachine) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.machines.insert(id, machine);
        id
    }

    /// Get a state machine by ID.
    pub fn get(&self, id: u32) -> Option<&StateMachine> {
        self.machines.get(&id)
    }

    /// Get a mutable reference to a state machine by ID.
    pub fn get_mut(&mut self, id: u32) -> Option<&mut StateMachine> {
        self.machines.get_mut(&id)
    }

    /// Get the number of state machines.
    pub fn len(&self) -> usize {
        self.machines.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.machines.is_empty()
    }
}
