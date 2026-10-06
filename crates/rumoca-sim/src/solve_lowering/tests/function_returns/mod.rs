//! Source-to-Solve controls for function return semantics.
//! Keep these with their simulation owner so they do not require CLI transports
//! or packaging. Use the same strict compilation API as the Compiler facade.

use rumoca_compile::compile::{CompilationResult, Session, SessionConfig};

mod arguments;
mod checked;
mod dynamic;
mod lazy;

fn compile_model(model: &str, source: &str, filename: &str) -> Result<CompilationResult, String> {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(filename, source)
        .expect("fixture parses");
    session
        .compile_model_strict(model)
        .map(|compilation| compilation.into_parts().0)
        .map_err(|report| report.failure_summary(usize::MAX))
}
