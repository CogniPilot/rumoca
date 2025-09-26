//! This module provides functionality to flatten a hierarchical intermediate representation (IR)
//! of a syntax tree into a flat representation. The primary purpose of this process is to
//! simplify the structure of the IR by expanding nested components and incorporating their
//! equations and subcomponents into a single flat class definition.
//!
//! The main function in this module is `flatten`, which takes a stored definition of the IR
//! and produces a flattened class definition. The process involves:
//!
//! - Identifying the main class and other class definitions from the provided IR.
//! - Iteratively expanding components in the main class that reference other class definitions.
//! - Propagating equations and subcomponents from referenced classes into the main class.
//! - Removing expanded components from the main class to ensure a flat structure.
//!
//! This module relies on visitors such as `ScopePusher` and `SubCompNamer` to handle
//! scoping and naming during the flattening process.
//!
//! # Dependencies
//! - `anyhow::Result`: For error handling.
//! - `indexmap::IndexMap`: To maintain the order of class definitions and components.
//!

use crate::ir::visitor::Visitable;
use crate::ir::visitors::scope_pusher::ScopePusher;
use crate::ir::visitors::sub_comp_namer::SubCompNamer;
use crate::ir::{self, ast::ClassDefinition};
use anyhow::Result;
use indexmap::{IndexMap, IndexSet};

pub fn flatten(def: &ir::ast::StoredDefinition) -> Result<ir::ast::ClassDefinition> {
    // flatten the syntax tree
    let mut count = 0;
    let mut main_class_name: String = String::new();
    let mut class_dict: IndexMap<String, ClassDefinition> = IndexMap::new();

    // find all class definitions
    for (class_name, class) in &def.class_list {
        if count == 0 {
            main_class_name = class.name.text.clone();
            println!("found main class: {:#?}", main_class_name);
        } else {
            class_dict.insert(class_name.clone(), class.clone());
            println!("found class: {:#?}", class_name);
        }
        count += 1;
    }

    // get main class
    let main_class = def
        .class_list
        .get(&main_class_name)
        .expect("Main class not found");

    flatten_class(main_class, &mut class_dict)
}

pub fn flatten_class(
    class: &ClassDefinition,
    class_dict: &mut IndexMap<String, ClassDefinition>,
) -> Result<ir::ast::ClassDefinition> {
    let main_class_name: String = class.name.text.clone();

    // create flat class
    let mut fclass = ClassDefinition {
        name: class.name.clone(),
        initial_algorithms: class.initial_algorithms.clone(),
        encapsulated: class.encapsulated,
        ..Default::default()
    };

    //  handle extend clauses
    for extend in &class.extends {
        println!("handling extends clause");

        let class_name = extend.comp.to_string();
        let class = class_dict
            .get(&class_name)
            .expect(&format!("Class for extend '{}' not found", class_name));

        // add components
        for (name, comp) in &class.components {
            println!("print adding comp from extends clause {:#?}", comp);
            fclass.components.insert(name.clone(), comp.clone());
        }

        // add equations
        for eq in &class.equations {
            match eq {
                ir::ast::Equation::Connect { lhs, rhs } => {
                    // skip connection equations for now
                    println!("skipping lhs: {:#?} rhs: {:#?}", lhs, rhs);
                }
                _ => fclass.equations.push(eq.clone()),
            }
        }
    }

    // copy components from main class to flat class
    fclass.components = class.components.clone();

    // only keep equations that are not connections in flat class from main class
    for eq in &class.equations {
        match eq {
            ir::ast::Equation::Connect { lhs, rhs } => {
                // skip connection equations for now
                println!(
                    "skipping connect copy from main class lhs: {:#?} rhs: {:#?}",
                    lhs, rhs
                );
            }
            _ => fclass.equations.push(eq.clone()),
        }
    }

    // flatten the class by expanding components
    let mut scope_pusher = ScopePusher {
        global_symbols: IndexSet::from([
            "time".to_string(),
            "der".to_string(),
            "pre".to_string(),
            "cos".to_string(),
            "sin".to_string(),
            "tan".to_string(),
        ]),
        symbols: IndexSet::new(),
        comp: main_class_name.clone(),
    };

    // for each component in the main class
    for (comp_name, comp) in &class.components {
        // if the the component type is a user defined class
        if class_dict.contains_key(&comp.type_name.to_string()) {
            let comp_class = class_dict.get(&comp.type_name.to_string()).unwrap().clone();

            // add equation from component to flat class
            for eq in &comp_class.equations {
                let mut feq = eq.clone();
                scope_pusher.comp = comp_name.clone();
                feq.accept(&mut scope_pusher);

                match &feq {
                    ir::ast::Equation::Connect { lhs, rhs } => {
                        // skip connection equations for now
                        println!("skipping lhs: {:#?} rhs: {:#?}", lhs, rhs);
                    }
                    _ => {
                        fclass.equations.push(feq);
                    }
                }
            }

            // expand comp.sub_comp names to use underscores
            fclass.accept(&mut SubCompNamer {
                comp: comp_name.clone(),
            });

            // add subcomponents from component to flat class
            for (subcomp_name, subcomp) in &comp_class.components {
                let mut scomp = subcomp.clone();
                let name = format!("{}_{}", comp_name, subcomp_name);
                scomp.name = name.clone();
                fclass.components.insert(name, scomp);
            }

            // remove compoment from flat class, as it has been expanded
            fclass.components.swap_remove(comp_name);
        }
    }
    Ok(fclass)
}
