use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use rustc_hash::FxHashMap;
use std::cell::RefCell;

const MAX_FUNCTION_LEAF_INDEXES: usize = 16;

thread_local! {
    static FUNCTION_LEAF_INDEX_CACHE: RefCell<FxHashMap<FunctionLeafIndexKey, FxHashMap<String, Option<DefId>>>> =
        RefCell::new(FxHashMap::default());
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct FunctionLeafIndexKey {
    tree_addr: usize,
    def_count: usize,
    name_count: usize,
}

pub(crate) fn lookup_function_definition<'a>(
    func_name: &str,
    qualified_name: Option<&str>,
    tree: &'a ast::ClassTree,
) -> Option<&'a ast::ClassDef> {
    if let Some(name) = qualified_name
        && let Some(class) = tree.get_class_by_qualified_name(name)
        && class.class_type == rumoca_core::ClassType::Function
    {
        return Some(class);
    }

    if let Some(class) = tree.get_class_by_qualified_name(func_name)
        && class.class_type == rumoca_core::ClassType::Function
    {
        return Some(class);
    }

    lookup_unique_short_function_name(func_name, tree)
}

pub(super) fn lookup_unique_short_function_name<'a>(
    func_name: &str,
    tree: &'a ast::ClassTree,
) -> Option<&'a ast::ClassDef> {
    let def_id = cached_function_leaf_def_id(tree, func_name)??;
    tree.get_class_by_def_id(def_id)
        .filter(|class| class.class_type == rumoca_core::ClassType::Function)
}

fn cached_function_leaf_def_id(tree: &ast::ClassTree, func_name: &str) -> Option<Option<DefId>> {
    let key = FunctionLeafIndexKey {
        tree_addr: std::ptr::from_ref(tree) as usize,
        def_count: tree.def_map.len(),
        name_count: tree.name_map.len(),
    };
    FUNCTION_LEAF_INDEX_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        if !cache.contains_key(&key) {
            if cache.len() >= MAX_FUNCTION_LEAF_INDEXES {
                cache.clear();
            }
            cache.insert(key, build_function_leaf_index(tree));
        }
        cache
            .get(&key)
            .and_then(|index| index.get(func_name).copied())
    })
}

fn build_function_leaf_index(tree: &ast::ClassTree) -> FxHashMap<String, Option<DefId>> {
    let mut index = FxHashMap::default();
    for (def_id, qualified) in &tree.def_map {
        let Some(class) = tree.get_class_by_qualified_name(qualified) else {
            continue;
        };
        if class.class_type != rumoca_core::ClassType::Function {
            continue;
        }
        let leaf = class.name.text.as_ref();
        index
            .entry(leaf.to_string())
            .and_modify(|existing| *existing = None)
            .or_insert(Some(*def_id));
    }
    index
}
