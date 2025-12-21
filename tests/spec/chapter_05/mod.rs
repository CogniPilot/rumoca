//! MLS Chapter 5: Scoping, Name Lookup, and Flattening
//!
//! This module tests conformance to MLS 3.7-dev Chapter 5, which defines:
//! - §5.1 Flattening Context
//! - §5.2 Enclosing Classes
//! - §5.3 Static Name Lookup
//! - §5.4 Inner/Outer Components
//! - §5.5 Simultaneous Inner/Outer
//! - §5.6 Flattening Process
//!
//! Reference: https://specification.modelica.org/master/scoping-name-lookup-and-flattening.html

pub mod section_5_3_lookup;
pub mod section_5_4_inner_outer;
pub mod section_5_6_flattening;
pub mod section_5_critical;
pub mod section_5_high_priority;
