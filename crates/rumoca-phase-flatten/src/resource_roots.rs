//! The resource directory of each loaded package (MLS §13.5).

use std::path::{Path, PathBuf};

use rumoca_eval_flat::translation_reads::ResourceRoots;
use rumoca_ir_ast as ast;

/// The resource directory of every package of the class tree.
///
/// A top-level package keeps its resources in the directory of the file that
/// declares it: `A/package.mo` for a package stored as a directory, the
/// enclosing directory for a package stored as one file. MLS §13.5 places the
/// resources of a nested package `A.B` in the subdirectory `B` of `A`'s
/// directory, wherever `B` is stored.
pub(crate) fn resource_roots(tree: &ast::ClassTree) -> ResourceRoots {
    let mut roots = ResourceRoots::new();
    for (name, class) in &tree.definitions.classes {
        let Some((file, _)) = tree.source_map.get_source(class.location.source) else {
            continue;
        };
        if let Some(directory) = Path::new(file).parent() {
            insert_package(&mut roots, name.clone(), directory.to_path_buf(), class);
        }
    }
    roots
}

fn insert_package(
    roots: &mut ResourceRoots,
    name: String,
    directory: PathBuf,
    class: &ast::ClassDef,
) {
    for (nested_name, nested) in &class.classes {
        if nested.class_type == rumoca_core::ClassType::Package {
            insert_package(
                roots,
                format!("{name}.{nested_name}"),
                directory.join(nested_name),
                nested,
            );
        }
    }
    roots.insert(name, directory);
}
