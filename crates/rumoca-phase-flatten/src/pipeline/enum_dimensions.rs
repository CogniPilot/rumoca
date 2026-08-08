use super::*;

pub(super) fn enum_type_dimension(expr: &ast::Expression, tree: &ClassTree) -> Option<i64> {
    let ast::Expression::ComponentReference(reference) = expr else {
        return None;
    };
    enum_literal_count_for_reference(reference, tree).and_then(|count| i64::try_from(count).ok())
}

pub(super) fn infer_enum_range_dimensions(expr: &Expression, tree: &ClassTree) -> Option<Vec<i64>> {
    let Expression::Range {
        start, step, end, ..
    } = expr
    else {
        return None;
    };
    if step.is_some() {
        return None;
    }
    let (start_type, start_ordinal) = enum_literal_ordinal(start, tree)?;
    let (end_type, end_ordinal) = enum_literal_ordinal(end, tree)?;
    if start_type != end_type {
        return None;
    }
    let len = if end_ordinal >= start_ordinal {
        end_ordinal - start_ordinal + 1
    } else {
        0
    };
    Some(vec![len])
}

fn enum_literal_ordinal(expr: &Expression, tree: &ClassTree) -> Option<(rumoca_core::DefId, i64)> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let reference = name.component_ref()?;
    let literal = reference.parts().last()?.ident.as_str();
    let enum_class = enum_class_for_literal_reference(reference, tree)?;
    let enum_def_id = enum_class.def_id?;
    let ordinal = enum_class
        .enum_literals
        .iter()
        .position(|candidate| candidate.ident.text.as_ref() == literal)? as i64
        + 1;
    Some((enum_def_id, ordinal))
}

fn enum_class_for_literal_reference<'a>(
    reference: &rumoca_core::ComponentReference,
    tree: &'a ClassTree,
) -> Option<&'a ast::ClassDef> {
    if reference.parts().len() < 2 {
        return None;
    }
    let first_def_id = reference.root_def_id();
    let mut class = tree.get_class_by_def_id(first_def_id)?;
    if !class.enum_literals.is_empty() {
        return Some(class);
    }
    for part in &reference.parts()[1..reference.parts().len() - 1] {
        class = class.classes.get(part.ident.as_str())?;
    }
    (!class.enum_literals.is_empty()).then_some(class)
}

/// Count the literals of the enumeration type a dimension reference names
/// (MLS §10.1: an enumeration type used as a dimension has as many elements as
/// the type has literals).
///
/// The reference names its type at the segment it resolves to, not at its
/// root: `Modelica.Electrical.Digital.Interfaces.Logic` resolves `Logic`
/// through four enclosing packages, and a renaming import such as
/// `import L = ...Interfaces.Logic` resolves the single segment `L` to that
/// same declaration. Both spellings carry the enumeration's exact `DefId` on
/// the segment they resolve to, so the target segment is the only one that
/// identifies the type; a root package segment identifies no type at all.
fn enum_literal_count_for_reference(
    reference: &ast::ComponentReference,
    tree: &ClassTree,
) -> Option<usize> {
    let def_id = reference.target_def_id()?;
    tree.get_class_by_def_id(def_id)
        .map(|class| class.enum_literals.len())
        .filter(|count| *count > 0)
}

#[cfg(test)]
mod enumeration_dimension_tests {
    //! `Modelica.Electrical.Digital.Gates.AndGate` reaches
    //! `Modelica.Electrical.Digital.Delay.InertialDelaySensitive`, which
    //! declares `constant Integer delayTable[L, L]` for
    //! `import L = Modelica.Electrical.Digital.Interfaces.Logic`. The
    //! declaration is instantiated several component levels below the model
    //! root, and its dimension reference reaches flatten spelling the
    //! enumeration's full package path, so
    //! `Modelica.Electrical.Digital.Examples.FullAdder` failed with
    //! `EF010 unresolved component dimension for
    //! Adder1.Adder2.AND.G2.delayTable: Modelica.Electrical.Digital.Interfaces\
    //! .Logic` while only the reference root was consulted for the
    //! enumeration's literals.

    use rumoca_ir_ast as ast;
    use rumoca_ir_flat as flat;

    const SOURCE: &str = r"
package Lib
  package Interfaces
    type Logic = enumeration(U, X, Zero, One);
  end Interfaces;

  package Other
    type Logic = enumeration(Low, High);
  end Other;

  model Gate
    import L = Lib.Interfaces.Logic;
    constant Integer aliasedTable[L, L] = zeros(4, 4);
    constant Integer qualifiedTable[Lib.Interfaces.Logic] = zeros(4);
    constant Integer otherTable[Lib.Other.Logic] = zeros(2);
    Real y;
  equation
    y = time;
  end Gate;

  model Adder
    Gate gate;
  end Adder;

  model Top
    Adder adder;
  end Top;
end Lib;
";

    fn flatten_source(model: &str) -> flat::Model {
        let file_name = "<enumeration_dimension_tests>";
        let stored =
            rumoca_phase_parse::parse_to_ast(SOURCE, file_name).expect("fixture should parse");
        let mut tree = ast::ClassTree::from_parsed(stored);
        tree.source_map.add(file_name, SOURCE);
        let resolved = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
            .expect("fixture should resolve");
        let instanced = rumoca_phase_instantiate::instantiate(resolved, model)
            .expect("fixture should instantiate");
        crate::flatten_ref(instanced.inner(), instanced.overlay(), model)
            .expect("fixture should flatten")
    }

    fn dims_of(model: &flat::Model, name: &str) -> Vec<i64> {
        model
            .variables
            .iter()
            .find(|(var_name, _)| var_name.as_str() == name)
            .map(|(_, variable)| variable.dims.clone())
            .unwrap_or_else(|| {
                let known: Vec<&str> = model.variables.keys().map(|key| key.as_str()).collect();
                panic!("no flat variable `{name}`; got {known:?}")
            })
    }

    #[test]
    fn enumeration_type_dimensions_size_nested_components() {
        let model = flatten_source("Lib.Top");
        // MLS §10.1: an enumeration-typed dimension has one element per
        // literal, whether the type is named through a renaming import or
        // through its full package path.
        assert_eq!(dims_of(&model, "adder.gate.aliasedTable"), vec![4, 4]);
        assert_eq!(dims_of(&model, "adder.gate.qualifiedTable"), vec![4]);
    }

    #[test]
    fn same_leaf_enumeration_names_size_by_their_own_declaration() {
        let model = flatten_source("Lib.Top");
        // `Lib.Interfaces.Logic` and `Lib.Other.Logic` share a leaf name and
        // differ in literal count, so a dimension may only be sized from the
        // declaration its reference resolves to.
        assert_eq!(dims_of(&model, "adder.gate.qualifiedTable"), vec![4]);
        assert_eq!(dims_of(&model, "adder.gate.otherTable"), vec![2]);
    }
}
