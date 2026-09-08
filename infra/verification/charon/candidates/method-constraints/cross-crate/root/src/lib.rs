pub fn named_call(value: &u32) -> &u32 {
    closure_contract_dep::named()(value)
}

pub fn higher_ranked_call(value: &u32) -> &u32 {
    closure_contract_dep::higher_ranked()(value)
}

pub fn captured_call(value: &u32) -> u32 {
    closure_contract_dep::captured(value)()
}

#[cfg(test)]
mod tests {
    #[test]
    fn both_reference_forms_preserve_the_supplied_referent() {
        let values = [13, 42];
        for value in &values {
            assert!(std::ptr::eq(super::named_call(value), value));
            assert!(std::ptr::eq(super::higher_ranked_call(value), value));
            assert_eq!(super::captured_call(value), *value);
        }
    }
}
