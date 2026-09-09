import Ti3.Funs
open Aeneas Aeneas.Std Result
namespace try_branch

/-- Err path: once `parse` fails, `twice` returns the same error. -/
theorem twice_err (x : Std.U32) (e : Std.U8) (h : parse x = ok (.Err e)) :
    twice x = ok (.Err e) := by
  simp [twice, h, core.result.Result.Insts.CoreOpsTry.branch, core.result.Result.Insts.CoreOpsTryTraitFromResidualResultInfallible.from_residual, core.convert.FromSame]

/-- Ok path: both parses succeed, the result is the second value. -/
theorem twice_ok (x a b : Std.U32) (h1 : parse x = ok (.Ok a)) (h2 : parse a = ok (.Ok b)) :
    twice x = ok (.Ok b) := by
  simp [twice, h1, h2, core.result.Result.Insts.CoreOpsTry.branch, core.result.Result.Insts.CoreOpsTryTraitFromResidualResultInfallible.from_residual, core.convert.FromSame]

end try_branch
