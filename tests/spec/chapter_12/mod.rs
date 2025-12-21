//! MLS Chapter 12: Functions
//!
//! This module tests conformance to MLS 3.7-dev Chapter 12, which defines:
//! - §12.1 Function Declaration
//! - §12.2 Function as a Specialized Class
//! - §12.3 Pure Modelica Functions
//! - §12.4 Function Call
//! - §12.5 Built-in Functions
//! - §12.6 Record Constructor Functions
//! - §12.7 Derivatives and Inverses of Functions
//! - §12.8 Function Inlining and Event Generation
//! - §12.9 External Function Interface
//!
//! Reference: https://specification.modelica.org/master/functions.html

// Original section files
pub mod examples;
pub mod section_12_1_4;
pub mod section_12_5;
pub mod section_12_6_9;

// Detailed section-by-section tests covering every normative statement
pub mod section_12_1_detailed;
pub mod section_12_2_restrictions;
pub mod section_12_3_purity;
pub mod section_12_4_calls;
pub mod section_12_5_detailed;
pub mod section_12_6_9_detailed;

// Critical restriction tests
pub mod section_12_critical;

// Low priority tests
pub mod section_12_low_priority;
