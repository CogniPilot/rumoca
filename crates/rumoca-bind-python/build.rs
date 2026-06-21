fn main() {
    // Required for extension modules on macOS: allow unresolved Python symbols
    // that are provided by the embedding Python interpreter at import time.
    pyo3_build_config::add_extension_module_link_args();

    // pyo3 0.22's `create_exception!` macro expands to `#[cfg(feature =
    // "gil-refs")]`, a pyo3-internal feature this crate never enables. Declare it
    // as a known cfg so the expansion does not trip `unexpected_cfgs`.
    println!("cargo::rustc-check-cfg=cfg(feature, values(\"gil-refs\"))");
}
