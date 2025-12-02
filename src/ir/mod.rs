//! Intermediate Representation (IR) for Modelica.
//!
//! This module provides the core IR types and transformation passes.

pub mod analysis;
pub mod ast;
pub mod error;
pub mod structural;
pub mod transform;
pub mod visitor;
