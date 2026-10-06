//! A panicking request discards the singleton session instead of poisoning it.
use super::*;

#[test]
fn a_panicking_request_leaves_a_fresh_session_for_the_next_request() {
    let _lock = session_test_guard();
    crate::with_singleton_session(|session| {
        session.update_document("input.mo", "model Kept Real x = 1; end Kept;");
        Ok(())
    })
    .unwrap();
    let panicked = std::panic::catch_unwind(|| {
        crate::with_singleton_session(|_| -> Result<(), WasmError> {
            panic!("request interrupted while holding the session")
        })
    });
    assert!(panicked.is_err());
    let documents = crate::with_singleton_session(|session| Ok(session.document_uris().len()))
        .expect("the next request receives a session rather than a poisoned lock");
    assert_eq!(documents, 0, "the interrupted session state is discarded");
}
