pub(super) struct Fixture {
    pub(super) name: &'static str,
    pub(super) model: &'static str,
    pub(super) source: &'static str,
    pub(super) success: bool,
}

pub(super) const FIXTURES: &[Fixture] = &[
    Fixture {
        name: "scalar",
        model: "Scalar",
        success: true,
        source: "model Scalar input Real u; output Real y; equation y=u; end Scalar;",
    },
    Fixture {
        name: "tensor",
        model: "Tensor",
        success: true,
        source: "model Tensor input Real u[2,3]; output Real y[2,3]; equation y=u; end Tensor;",
    },
    Fixture {
        name: "record",
        model: "Record",
        success: true,
        source: "record Leaf Real values[2,3]; Integer count; Boolean valid; end Leaf; model Record input Leaf previous; output Leaf next; equation next=previous; end Record;",
    },
    Fixture {
        name: "guard",
        model: "Guard",
        success: true,
        source: "model Guard parameter Integer n=2; parameter Boolean enabled=true; input Real u[n]; output Real y[n]; equation if enabled then y=u; else y=u; y=2*u; end if; end Guard;",
    },
    Fixture {
        name: "assertion",
        model: "Assertion",
        success: true,
        source: "function Check input Boolean valid; input Integer i; input Real values[2]; output Real result; algorithm assert(valid,\"first fault\"); result:=values[i]; end Check; model Assertion input Boolean valid; input Integer i; input Real values[2]; output Real y; equation y=Check(valid,i,values); end Assertion;",
    },
    Fixture {
        name: "unbalanced",
        model: "Unbalanced",
        success: false,
        source: "model Unbalanced input Real u; output Real y; equation y=u; y=0; end Unbalanced;",
    },
    Fixture {
        name: "reinit",
        model: "InvalidReinit",
        success: false,
        source: "model InvalidReinit Real x; equation x=1; when time>1 then reinit(x,0); end when; end InvalidReinit;",
    },
];
