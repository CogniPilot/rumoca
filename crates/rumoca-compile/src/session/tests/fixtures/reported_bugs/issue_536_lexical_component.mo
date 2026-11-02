package Lex
 function f input Real x; output Real y; algorithm y:=x+1; end f;
 partial model P parameter Real k; end P;
 model Full extends P; parameter Real seed=100; end Full;
 model C
  parameter Real seed=7;
  Full h(k(start=f(seed))=5);
 end C;
end Lex;