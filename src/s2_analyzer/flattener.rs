use rumoca_parser::ast;
use rumoca_parser::ast::StoredDefinition;
use rumoca_parser::{Visitable, VisitableMut};
use std::collections::HashMap;

use super::super::s4_generator::modelica_printer;
use super::{visitors, visitors_mut};

//=============================================================================
/// Flattens nested classes into one class (see Friztzon pg. 143)
///
/// 1. The names of declared local classes, variables, and other attributes
///     are found. Also, modifiers are merged with the local element
///     declarations, and redeclarations are applied.
///
/// 2. Extends clauses are processed by lookup and expansion of inherited
///     classes. Their contents are expanded and inserted into the current
///     class. The lookup of the inherited classes should be independent,
///     that is, the analysis and expansion of one extends clause should
///     not be dependent on another.
pub fn flatten(
    def: &ast::StoredDefinition,
    verbose: bool,
) -> Result<ast::StoredDefinition, Box<dyn std::error::Error>> {
    let bar = "=".repeat(80);
    if verbose {
        println!("\n{}", bar);
        println!("FLATTEN");
        println!("{}\n", bar);
    }

    // begin flat tree as a copy of the root class
    let mut flat_tree = StoredDefinition::default();
    flat_tree.classes.push(def.classes[0].clone());

    if verbose {
        println!("\nroot class \n{}\n", bar);
        //et mut print_visitor = PrintVisitor::default();
        //new_tree.accept(&mut print_visitor);
        modelica_printer(&flat_tree);
    }

    //
    // STEP 1 (find classes and components)
    //
    let mut class_collector = visitors::ClassCollector::default();
    def.accept(&mut class_collector);

    let mut vars = HashMap::new();
    for (class_name, class_def) in class_collector.classes.iter() {
        // for the classes that we found, find all of their components
        // and store them in a dictionary
        let mut var_collector = visitors::VarCollector::default();
        class_def.accept(&mut var_collector);
        vars.insert(class_name.clone(), var_collector.vars);
    }

    if verbose {
        println!("\nfind variables\n{}\n", bar);
        for (k, v) in vars.iter() {
            println!("{}: {:?}", k, v.keys());
        }
    }

    //
    // STEP 2 (process extends clauses etc.)
    //

    // explode all compoment classes
    let mut class_exploder = visitors_mut::ClassExploder {
        classes: class_collector.classes,
        ..Default::default()
    };
    flat_tree.accept_mut(&mut class_exploder);
    if verbose {
        println!("\nexploded classes \n{}\n", bar);
        //et mut print_visitor = PrintVisitor::default();
        //new_tree.accept(&mut print_visitor);
        modelica_printer(&flat_tree);
    }

    //
    // Return
    //
    Ok(flat_tree)
}
