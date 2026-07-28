//! Environment threaded through constant folding, including the chain of
//! constant keys whose bindings are currently being expanded.
//!
//! A constant whose binding expands back into itself would otherwise fold
//! forever; the expansion chain lets the substituter report that as a spanned
//! diagnostic (`EF021`) instead of exhausting the stack.

use super::*;

/// One link of the chain of constant keys whose bindings are currently being
/// expanded. Stored as a stack-allocated linked list so the `Copy` environment
/// can be threaded through the recursive rewrite without allocating.
pub(super) struct ConstantExpansion<'a> {
    pub(super) key: &'a str,
    pub(super) parent: Option<&'a ConstantExpansion<'a>>,
}

impl ConstantExpansion<'_> {
    fn contains(&self, key: &str) -> bool {
        let mut frame = Some(self);
        while let Some(current) = frame {
            if current.key == key {
                return true;
            }
            frame = current.parent;
        }
        false
    }

    /// Render the expansion chain oldest-first, ending at `key`.
    fn chain_to(&self, key: &str) -> String {
        let mut keys: Vec<&str> = Vec::new();
        let mut frame = Some(self);
        while let Some(current) = frame {
            keys.push(current.key);
            frame = current.parent;
        }
        keys.reverse();
        keys.push(key);
        keys.join(" -> ")
    }
}

#[derive(Clone, Copy)]
pub(super) struct ConstantSubstitutionEnv<'a> {
    pub(super) ctx: &'a Context,
    pub(super) live_vars: &'a rustc_hash::FxHashSet<String>,
    pub(super) locals: &'a HashSet<String>,
    pub(super) scope: &'a str,
    pub(super) prefer_scoped_parameters: bool,
    pub(super) expanding: Option<&'a ConstantExpansion<'a>>,
}

impl<'a> ConstantSubstitutionEnv<'a> {
    pub(super) fn with_scope<'b>(&'b self, scope: &'b str) -> ConstantSubstitutionEnv<'b> {
        ConstantSubstitutionEnv {
            ctx: self.ctx,
            live_vars: self.live_vars,
            locals: self.locals,
            scope,
            prefer_scoped_parameters: self.prefer_scoped_parameters,
            expanding: self.expanding,
        }
    }

    /// True when `key`'s binding is already being folded further up the stack.
    pub(super) fn is_expanding(&self, key: &str) -> bool {
        self.expanding.is_some_and(|frame| frame.contains(key))
    }

    pub(super) fn expansion_chain(&self, key: &str) -> String {
        match self.expanding {
            Some(frame) => frame.chain_to(key),
            None => key.to_string(),
        }
    }
}
