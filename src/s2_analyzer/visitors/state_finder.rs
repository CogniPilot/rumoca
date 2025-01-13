use super::component_finder::ComponentFinder;
use rumoca_parser::ast::{Equation, Expression};
use rumoca_parser::{ast, Visitor};
use std::collections::HashMap;

//=============================================================================
/// Finds all states in the class
#[derive(Default, Debug)]
pub struct StateFinder<'a> {
    /// A struct to traverse a tree and find all calsses and put references
    /// in a dictionary. The references have the same lifetime as the
    /// struct.s
    pub states: HashMap<String, &'a ast::ComponentDeclaration>,
    pub ode: HashMap<String, &'a ast::Expression>,

    // from component namer
    pub component_finder: ComponentFinder<'a>,
}

impl<'a> Visitor<'a> for StateFinder<'a> {
    fn enter_equation(&mut self, node: &'a Equation) {
        // looks for simple explicit ode of form der(x) = expr
        if let Equation::Simple {
            lhs: Expression::FunctionCall { comp, args },
            rhs,
            ..
        } = &node
        {
            if comp
                .parts
                .iter()
                .map(|x| x.name.clone())
                .collect::<Vec<String>>()
                .join(".")
                == "der"
            {
                if let Expression::Ref(comp) = &args[0] {
                    let comp_str = self
                        .component_finder
                        .component_ref_to_str
                        .get(&comp)
                        .expect("failed to get comp")
                        .clone();
                    let comp_decl = self
                        .component_finder
                        .str_to_component
                        .get(&comp_str)
                        .expect("failed to get str");
                    self.states.insert(comp_str.clone(), comp_decl);
                    self.ode.insert(comp_str.clone(), rhs);
                }
            }
        }
    }
}
