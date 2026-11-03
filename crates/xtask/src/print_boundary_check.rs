//! Bounded source and Cargo dependency identities; no migration authority.

pub mod dependency_closure;
pub mod source_closure;

#[derive(Debug, PartialEq, Eq)]
pub struct Refusal {
    rule: &'static str,
    item: String,
}

impl Refusal {
    pub fn rule(&self) -> &'static str {
        self.rule
    }
    pub fn item(&self) -> &str {
        &self.item
    }
}

impl std::fmt::Display for Refusal {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}: {}", self.rule, self.item)
    }
}

impl std::error::Error for Refusal {}

pub type Checked<T> = Result<T, Refusal>;

fn refuse<T>(rule: &'static str, item: impl Into<String>) -> Checked<T> {
    Err(Refusal {
        rule,
        item: item.into(),
    })
}
