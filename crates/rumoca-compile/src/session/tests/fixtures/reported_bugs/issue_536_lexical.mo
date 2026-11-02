package Lex
 function f input Real x; output Real y; algorithm y:=x+1; end f;
 partial model P parameter Real k; end P;
 model Full extends P; parameter Real seed=100; end Full;
 model C
  parameter Real seed=7;
  replaceable model H=Full(k=5) constrainedby P(k(start=f(seed)));
  H h;
 end C;
end Lex;