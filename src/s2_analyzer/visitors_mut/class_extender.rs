use rumoca_parser::VisitorMut;

//=============================================================================
/// Expands extends clause
#[derive(Default, Debug)]
pub struct ClassExtender {}

impl VisitorMut for ClassExtender {}
