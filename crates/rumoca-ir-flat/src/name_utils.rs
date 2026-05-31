/// Return a component-path base name with all bracketed subscripts removed.
///
/// Examples:
/// - `x[1]` -> `x`
/// - `support[1].phi` -> `support.phi`
/// - `a[1].b[2].c` -> `a.b.c`
pub fn component_base_name(name: &str) -> Option<String> {
    rumoca_core::component_path_base_name(name)
}

#[cfg(test)]
mod tests {
    use super::component_base_name;

    #[test]
    fn test_component_base_name_simple() {
        assert_eq!(component_base_name("x").as_deref(), Some("x"));
        assert_eq!(component_base_name("x[1]").as_deref(), Some("x"));
        assert_eq!(component_base_name("x[1][2]").as_deref(), Some("x"));
    }

    #[test]
    fn test_component_base_name_mid_path() {
        assert_eq!(
            component_base_name("support[1].phi").as_deref(),
            Some("support.phi")
        );
        assert_eq!(component_base_name("a[1].b[2].c").as_deref(), Some("a.b.c"));
        assert_eq!(
            component_base_name("foo.bar[3].baz").as_deref(),
            Some("foo.bar.baz")
        );
    }

    #[test]
    fn test_component_base_name_rejects_malformed() {
        assert_eq!(component_base_name("x["), None);
        assert_eq!(component_base_name("x].y"), None);
        assert_eq!(component_base_name(".x"), None);
        assert_eq!(component_base_name("x..y"), None);
    }
}
