use rumoca_parser::{ast, Visitable, Visitor};

use super::dae_ast;
use super::visitors;

//=============================================================================
/// Builds a dae_ast from a flattened rumoca_parser::ast
pub fn create_dae(
    def: &ast::StoredDefinition,
    verbose: bool,
) -> Result<dae_ast::Def, Box<dyn std::error::Error>> {
    //-------------------------------------------------------------------------
    // Setup
    if verbose {
        println!("\n\n{}", "=".repeat(80));
        println!("CREATE DAE");
        println!("{}", "=".repeat(80));
    }

    //-------------------------------------------------------------------------
    // Find components
    let mut component_finder = visitors::ComponentFinder::default();
    def.accept(&mut component_finder);

    if verbose {
        println!("\n\nfind components\n=========================\n");
        println!("{:#?}", component_finder);
    }

    //-------------------------------------------------------------------------
    // Find states
    let mut state_finder = visitors::StateFinder {
        component_finder,
        ..Default::default()
    };
    def.accept(&mut state_finder);

    if verbose {
        println!("\n\nfind states\n=========================\n");
        println!("{:#?}", state_finder);
    }

    //-------------------------------------------------------------------------
    // Create DAE
    let mut dae_creator = DaeCreator {
        state_finder,
        ..Default::default()
    };
    def.accept(&mut dae_creator);

    if verbose {
        println!("\n\nbuild dae ast\n=========================\n");
        println!("{:#?}", dae_creator.def);
    }

    Ok(dae_creator.def)
}

//=============================================================================
/// Builds a dae_ast from a flattened rumoca_parser::ast
#[derive(Default, Debug)]
pub struct DaeCreator<'a> {
    def: dae_ast::Def,
    #[allow(dead_code)]
    state_finder: visitors::StateFinder<'a>,
}

impl<'a> Visitor<'a> for DaeCreator<'a> {
    fn enter_stored_definition(&mut self, node: &'a ast::StoredDefinition) {
        self.def.rumoca_parser_git = node.rumoca_parser_git.clone();
        self.def.rumoca_parser_version = node.rumoca_parser_version.clone();
        self.def.rumoca_version = env!("CARGO_PKG_VERSION").to_string();
        self.def.rumoca_git = option_env!("GIT_VER").unwrap_or("").to_string();
    }

    fn enter_class_definition(&mut self, node: &'a ast::ClassDefinition) {
        let mut class = dae_ast::Class {
            ..Default::default()
        };
        if let ast::ClassSpecifier::Long { name, .. } = &node.specifier {
            for x in self.state_finder.states.keys() {
                class.name = name.clone();
                class.x.insert(x.clone());
                class.ode.insert(
                    x.clone(),
                    (*self.state_finder.ode.get(x).expect("failed to find ode")).clone(),
                );
            }
            self.def.classes.insert(name.clone(), class);
        }
    }
}
