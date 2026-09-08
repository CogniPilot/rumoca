# Causal experiment for simpler_closures::named_call ONLY, not a translator pass.
# Apply to a disposable LLBC copy. The real fix must issue this constraint in
# Charon's declaration constructor, with owner/binder/caller mapping.
def require(condition; message):
  if condition then . else error(message) end;

def edge:
  {regions: [], skip_binder: [{Var: {Free: $longer}}, {Var: {Free: $shorter}}]};

require(.has_errors == false; "refuse erroneous input")
| require(.translated.crate_name == "simpler_closures"; "wrong source fixture")
| require([.translated.fun_decls[].def_id] == [0, 1, 2, 3, 4]; "changed function layout")
| require([.translated.trait_impls[].def_id] == [0, 1, 2]; "changed implementation layout")
| require(.translated.fun_decls[2].src.TraitImpl.impl_ref.id == 0; "expected FnOnce method")
| require(.translated.fun_decls[3].src.TraitImpl.impl_ref.id == 1; "expected FnMut method")
| require([.translated.fun_decls[2].generics.regions[].index] == [0, 1, 2]; "changed FnOnce binder")
| require([.translated.fun_decls[3].generics.regions[].index] == [0, 1, 2, 3]; "changed FnMut binder")
| require(([$longer, $shorter] == [1, 2]) or ([$longer, $shorter] == [2, 1]); "only the two measured signature positions are in scope")
| .translated.fun_decls |= map(
    if .def_id == 2 or .def_id == 3 then
      .generics.regions_outlive += [edge]
    else . end)
| .translated.trait_impls |= map(
    if .def_id == 0 or .def_id == 1 then
      .generics.regions_outlive += [edge]
    else . end)
