import IteratorOwnedSource

open Lean Aeneas.Extract

-- Inspect the registered extraction descriptors, not just printed comments.
run_meta do
  let env ← getEnv
  let specialized := "iterator_specialization.core.ops.function.FnOnceMut0T0T1T2"
  let traits := (rustTraitDecls.ext.getState env).filterMap fun (pattern, _, info) =>
    if info.extract == some specialized then some pattern else none
  unless traits == #["core::ops::function::FnOnce<&'0 mut @T0, @T1, @T2>"] do
    throwError "Specialized FnOnce has incorrect registry identity: {traits}"
  let generic := (rustTraitDecls.ext.getState env).filter fun (pattern, _, _) =>
    pattern == "core::ops::function::FnOnce"
  unless generic.size == 1 do
    throwError "Generic FnOnce registry identity is ambiguous: {generic.size}"
