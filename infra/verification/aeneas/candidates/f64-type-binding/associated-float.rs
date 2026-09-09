#![deny(warnings)]

pub trait ScalarKind {
    type Scalar;
}

pub struct RealKind;

impl ScalarKind for RealKind {
    type Scalar = f64;
}
