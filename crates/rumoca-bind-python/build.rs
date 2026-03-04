fn main() {
    // Required for extension modules on macOS: allow unresolved Python symbols
    // that are provided by the embedding Python interpreter at import time.
    pyo3_build_config::add_extension_module_link_args();
}
