import RumocaPhaseSolve

open Lean

-- A focused exposure audit, not a proof of translator correctness.
run_meta do
  let env ← getEnv
  let source := env.constants.toList.filter fun (declName, _) =>
    (`rumoca_phase_solve).isPrefixOf declName
  unless source.length > 0 do
    throwError "Expected the generated production declarations to be imported"
  let collisions := source.filterMap fun (declName, _) =>
    match declName with
    | .str _ last =>
        if ["ok", "fail", "panic", "cont", "done"].contains last then
          some declName
        else none
    | _ => none
  unless collisions.isEmpty do
    throwError "Existing pilot has potential constructor captures: {collisions}"
  logInfo m!"Checked {source.length} production-namespace declarations; no constructor-name suffix collisions"
