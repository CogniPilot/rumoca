//! Modelica Language Specification Conformance Test Suite
//!
//! This test harness runs the folder-based MLS conformance tests.
//!
//! # Running Tests
//!
//! ```bash
//! # Run all spec tests
//! cargo test --test spec_tests
//!
//! # Run specific chapter
//! cargo test --test spec_tests chapter_12
//!
//! # Run specific section
//! cargo test --test spec_tests section_12_1
//! ```

mod spec;
