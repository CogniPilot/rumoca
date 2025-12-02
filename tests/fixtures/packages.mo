within;

package NestedTestPackage
  package MathUtils
    function polyEval
      input Real a, b, c, x;
      output Real y;
    algorithm
      y := a * x * x + b * x + c;
    end polyEval;

    function scale
      input Real k;
      input Real x;
      output Real y;
    algorithm
      y := k * x;
    end scale;
  end MathUtils;

  package Controllers
    function pid
      input Real kP, kI, kD;
      input Real e;
      input Real de;
      input Real ei;
      output Real u;
    algorithm
      u := kP * e + kD * de + kI * ei;
    end pid;

    model SimpleController
      parameter Real a = 1;
      parameter Real b = 0;
      parameter Real c = 0;
      Real x(start = 0);
      Real y;
      Real out;
      Real e;
      Real de;
      Real ei(start = 0);
    equation
      der(x) = 1;
      y = MathUtils.polyEval(a, b, c, x);
      e = y;
      de = der(e);
      der(ei) = e;
      out = pid(1, 0.1, 0.01, e, de, ei);
    end SimpleController;
  end Controllers;

  package Tests
    model CrossPackageTest
      Real x = 2;
      Real y;
      Real z;
    equation
      y = MathUtils.polyEval(1, -3, 2, x);
      z = MathUtils.scale(3, y);
    end CrossPackageTest;

    model ControllerTest
      Controllers.SimpleController ctrl;
    end ControllerTest;
  end Tests;

  model RootTest
    Tests.CrossPackageTest t1;
    Controllers.SimpleController t2;
  end RootTest;
end NestedTestPackage;
