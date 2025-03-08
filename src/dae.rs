use indexmap::IndexMap;

/// # DAE: Differential Algebraic Equations
///
/// v := [p; t; x_dot; x; y; z; m; pre(z); pre(m)]
///
/// 0 = fx(v, c)
///
/// z = {
///     fz(v, c) at events
///     pre(z)   otherwise
/// }
///
/// m := fm(v, c)
///
/// c := fc(relation, v)
///
/// ### where:
///
/// * `p`           : Modelica variables declared as parameters or constants,
///                   i.e., variables without any time-dependency.
/// * `t`           : Modelica variable representing time, the independent (real) variable.
/// * `x(t)`        : Modelica variables of type `Real` that appear differentiated.
/// * `y(t)`        : Continuous-time Modelica variables of type `Real` that do not
///                   appear differentiated (algebraic variables).
/// * `z(t_e)`      : Discrete-time Modelica variables of type `Real`. These
///                   variables change their value only at event instants `t_e`. `pre(z)`
///                   represents the values immediately before the current event occurred.
/// * `m(t_e)`      : Modelica variables of discrete-valued types (Boolean,
///                   Integer, etc) which are unknown. These variables change their value
///                   only at event instants
/// * `c(t_e)`      : The conditions of all if-expressions generated including
///                   when-clauses after conversion, see section 8.3.5).
/// * `relation(v)` : A relation containing variables v_i, e.g. v1 > v2, v3 >= 0.
///                   algebraic equations
pub struct Dae {
    pub p: IndexMap<String, String>,
    pub t: IndexMap<String, String>,
    pub x: IndexMap<String, String>,
    pub y: IndexMap<String, String>,
    pub z: IndexMap<String, String>,
    pub pre_z: IndexMap<String, String>,
    pub m: IndexMap<String, String>,
    pub pre_m: IndexMap<String, String>,
    pub c: IndexMap<String, String>,
    pub relation: IndexMap<String, String>,
    pub fx: IndexMap<String, String>,
    pub fz: IndexMap<String, String>,
    pub fm: IndexMap<String, String>,
    pub fc: IndexMap<String, String>,
    pub x_dot: IndexMap<String, String>,
}
