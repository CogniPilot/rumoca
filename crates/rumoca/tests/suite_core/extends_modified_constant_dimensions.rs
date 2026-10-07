//! Array dimensions that read a package constant an extends clause modifies.
//!
//! `Xi[nXi]` in a model nested in a package reads `nXi`, which the base
//! package derives from `fixedX` and from `nS = size(names, 1)`. The derived
//! package modifies `names` and `fixedX` in its extends clause, so the values
//! come from the package that exposes the model (MLS 7.2), not from the base
//! declaration's own bindings.

use rumoca::Compiler;

const MODEL: &str = r#"
package Base
  constant String names[:] = {"a"};
  constant Boolean fixedX = true;
  final constant Integer nS = size(names, 1);
  constant Integer nXi = if fixedX then 0 else nS;
  model Props
    Real Xi[nXi];
    Real X[nS];
  equation
    X = fill(1.0, nS);
    Xi = fill(0.0, nXi);
  end Props;
end Base;

package Derived
  extends Base(names = {"a", "b", "c"}, fixedX = false);
  model Copy
    extends Props;
  end Copy;
end Derived;
"#;

#[test]
fn dimensions_read_the_constants_the_exposing_package_modifies() {
    Compiler::new()
        .model("Derived.Copy")
        .compile_str(MODEL, "Packages.mo")
        .unwrap_or_else(|error| panic!("Derived.Copy compiles: {error:?}"));
}
