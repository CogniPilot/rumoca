# Contributing to Rumoca

Thank you for your interest in contributing to Rumoca! This document provides guidelines and instructions for contributing.

## Code of Conduct

Be respectful and constructive in all interactions. We're here to build great software together.

## How to Contribute

### Reporting Bugs

If you find a bug, please create an issue with:

- A clear, descriptive title
- Steps to reproduce the problem
- Expected vs. actual behavior
- Your environment (OS, Rust version, Rumoca version)
- Any relevant Modelica code or error messages

### Suggesting Features

Feature suggestions are welcome! Please:

- Check if the feature has already been suggested
- Explain the use case and why it would be valuable
- Provide examples of how it would work

### Pull Requests

1. **Fork the repository** and create a branch from `main`

2. **Make your changes**:
   - Write clear, concise commit messages
   - Follow the existing code style
   - Add tests for new functionality
   - Update documentation as needed

3. **Test your changes**:
   ```bash
   cargo test
   cargo fmt --check
   cargo clippy
   ```

4. **Submit a pull request** with:
   - A clear description of the changes
   - Reference to any related issues
   - Screenshots/examples if applicable

## Development Setup

### Prerequisites

- Rust 1.70.0 or later
- Git

### Getting Started

```bash
# Clone the repository
git clone https://github.com/jgoppert/rumoca.git
cd rumoca

# Build the project
cargo build

# Run tests
cargo test

# Run examples
cargo run --example basic_usage
```

### Project Structure

```
rumoca/
├── src/
│   ├── compiler.rs      # High-level API
│   ├── dae/             # DAE representation
│   ├── ir/              # Intermediate representation
│   │   ├── ast.rs       # Abstract syntax tree
│   │   ├── constants.rs # Centralized constants
│   │   ├── create_dae.rs # DAE creation
│   │   ├── flatten.rs   # Model flattening
│   │   ├── error.rs     # Error types
│   │   └── visitors/    # AST visitors
│   ├── modelica_grammar.rs  # Parser grammar
│   └── main.rs          # CLI entry point
├── tests/               # Integration tests
│   ├── fixtures/        # Test models
│   └── templates/       # Template examples
└── examples/            # Usage examples
```

## Coding Standards

### Style

- Use `cargo fmt` to format code
- Follow Rust naming conventions:
  - `snake_case` for functions and variables
  - `PascalCase` for types and traits
  - `SCREAMING_SNAKE_CASE` for constants

### Error Handling

- Use `Result<T, E>` for fallible operations
- Use custom error types from `src/ir/error.rs` and `src/dae/error.rs`
- Provide context with `.with_context()` from anyhow
- Never use `panic!()`, `unwrap()`, or `expect()` without justification
- Document safety with `// SAFETY:` comments for justified unwrap()

### Documentation

- Add doc comments (`///`) for public items
- Include examples in doc comments where helpful
- Update README.md for significant changes

### Testing

- Add unit tests for new functions
- Add integration tests for new features
- Test files go in `tests/` directory
- Use descriptive test names: `test_feature_behavior`

Example test:

```rust
#[test]
fn test_compile_simple_model() {
    let source = r#"
model Test
    Real x;
equation
    der(x) = 1;
end Test;
"#;

    let result = Compiler::new().compile_str(source, "test.mo");
    assert!(result.is_ok());

    let dae = result.unwrap().dae();
    assert_eq!(dae.x.len(), 1);
}
```

## Areas for Contribution

### High Priority

- [ ] Connection equation expansion
- [ ] Array support
- [ ] Additional built-in functions
- [ ] More template examples
- [ ] Performance profiling and optimization

### Medium Priority

- [ ] Multi-file imports
- [ ] Package system support
- [ ] Better error messages with source locations
- [ ] Integration tests for large models

### Documentation

- [ ] Tutorial for custom templates
- [ ] Architecture deep-dive
- [ ] Modelica feature support matrix
- [ ] Performance benchmarks

## Questions?

Feel free to:

- Open an issue for discussion
- Ask questions in pull requests
- Reach out to maintainers

## License

By contributing, you agree that your contributions will be licensed under the same terms as the project (MIT/Apache-2.0 dual license).

---

Thank you for contributing to Rumoca! 🚀
