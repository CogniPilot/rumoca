//! Exact in-process identity for symbolic operations and native identity barriers.

use crate::{LinearOp, LinearOpSliceKind, SolveVisitor};
use std::{
    convert::Infallible,
    hash::{Hash, Hasher},
    sync::Arc,
};

#[derive(Debug, Clone)]
struct CallIssuer(Arc<()>);

impl PartialEq for CallIssuer {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for CallIssuer {}

impl Hash for CallIssuer {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::ptr::hash(Arc::as_ptr(&self.0), state);
    }
}

/// The wire encodes literal bits and call indices. Each call index is local
/// to its issuing owner table. Retain the issuer of each occurrence, in traversal
/// order, for as long as its key can participate in a proof.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct OperationIdentity {
    bytes: Box<[u8]>,
    issuers: Box<[CallIssuer]>,
}

impl OperationIdentity {
    pub(super) fn new(ops: &[LinearOp]) -> Option<Self> {
        let bytes = bincode::serialize(ops).ok()?.into_boxed_slice();
        let mut issuers = CallIssuers::default();
        let Ok(()) = issuers.visit_scalar_program(0, None, ops);
        Some(Self {
            bytes,
            issuers: issuers.0.into_boxed_slice(),
        })
    }
}

#[derive(Default)]
struct CallIssuers(Vec<CallIssuer>);

impl SolveVisitor for CallIssuers {
    type Error = Infallible;

    fn visit_linear_op(
        &mut self,
        _: LinearOpSliceKind,
        _: usize,
        op: &LinearOp,
    ) -> Result<(), Self::Error> {
        let issuer = match op {
            LinearOp::PureCall { site, .. } => site.proof_issuer(),
            LinearOp::PureCallDirectional { site, .. } => site.proof_issuer(),
            LinearOp::PureCallObservation { site, .. } => site.value_site().proof_issuer(),
            _ => return Ok(()),
        };
        self.0.push(CallIssuer(Arc::clone(issuer)));
        Ok(())
    }
}
