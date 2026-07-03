//! eFMI packaging module: typed `__content.xml` and Algorithm Code manifest
//! models, XML serialization, SHA-1 checksums, UUID/id discipline, vendored
//! XSD validation, and eFMU container layout (eFMI Standard 1.0.0 Beta 1, §2, §3.1).
//!
//! This crate is representation-agnostic packaging: it has no Rumoca IR or
//! GALEC dependencies. See `spec/SPEC_0034_GALEC_EFMI_EXPORT.md`.
