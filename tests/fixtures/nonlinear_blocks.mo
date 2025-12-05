// Simplified versions of Modelica.Blocks.Nonlinear blocks for balance testing
package NonlinearBlocks
  // SISO interface base
  block SISO
    input Real u;
    output Real y;
  end SISO;

  // Limiter: extends SISO, adds 1 protected variable (simplifiedExpr)
  // Should be: 2 unknowns (y, simplifiedExpr), 2 equations
  block Limiter
    extends SISO;
    parameter Real uMax = 1;
    parameter Real uMin = -uMax;
    parameter Boolean strict = false;
    Real simplifiedExpr;
  equation
    simplifiedExpr = u;
    // simplified binding
    if strict then
      y = smooth(0, noEvent(if u > uMax then uMax else if u < uMin then uMin else u));
    else
      y = smooth(0, if u > uMax then uMax else if u < uMin then uMin else u);
    end if;
  end Limiter;

  // VariableLimiter: extends SISO, adds 2 inputs and 1 protected
  // Should be: 2 unknowns (y, simplifiedExpr), 2 equations
  block VariableLimiter
    extends SISO;
    input Real limit1;
    input Real limit2;
    parameter Boolean strict = false;
    Real simplifiedExpr;
  equation
    simplifiedExpr = u;
    if strict then
      y = smooth(0, noEvent(if u > limit1 then limit1 else if u < limit2 then limit2 else u));
    else
      y = smooth(0, if u > limit1 then limit1 else if u < limit2 then limit2 else u);
    end if;
  end VariableLimiter;

  // SlewRateLimiter: extends SISO, has state y and protected val
  // Should be: 2 unknowns (y state, val algebraic), 2 equations (der(y)=..., val=...)
  block SlewRateLimiter
    extends SISO;
    parameter Real Rising = 1;
    parameter Real Falling = -Rising;
    parameter Real Td = 0.001;
    parameter Boolean strict = false;
    Real val = (u - y) / Td; // binding equation
  equation
    if strict then
      der(y) = smooth(1, if noEvent(val < Falling) then Falling else if noEvent(val > Rising) then Rising else val);
    else
      der(y) = if val < Falling then Falling else if val > Rising then Rising else val;
    end if;
  end SlewRateLimiter;

  // DeadZone: extends SISO
  // Should be: 1 unknown (y), 1 equation
  block DeadZone
    extends SISO;
    parameter Real uMax = 1;
    parameter Real uMin = -uMax;
  equation
    y = smooth(0, if u > uMax then u - uMax else if u < uMin then u - uMin else 0);
  end DeadZone;
end NonlinearBlocks;
