//! Shared orchestration cannot accept independently chosen capsule bytes.

use super::*;
use rumoca_sim::PreparedNativeFmiLsModel;

mod native_metadata;
#[cfg(all(
    not(target_arch = "wasm32"),
    rumoca_qualification,
    feature = "native-fmi-ls-build"
))]
mod whole_graph;

#[test]
fn native_fmi_ls_preparation_encodes_only_the_original_library_root() {
    let _lock = session_test_guard();
    with_fixture_component(|component| {
        let root = component.runtime_view().retain_assertion_root();
        let code = preparation(&component);
        let expected =
            serde_json::to_vec(&rumoca_sim::retained_fmi_component_wire(&root).unwrap()).unwrap();
        let library =
            NativeModelLibrary::from_preparation(code.clone(), deployment(component)).unwrap();
        let prepared = PreparedNativeFmiLsModel::from_library(library).unwrap();
        assert_eq!(prepared.capsule_bytes(), expected);
        assert!(std::ptr::eq(prepared.library().compiled(), code.compiled()));
        assert!(std::ptr::eq(prepared.library().inputs(), code.inputs()));
        let source = prepared
            .library()
            .deployment()
            .runtime_view()
            .retain_assertion_root();
        assert!(source.shares_root_owner(&root));
        assert!(std::ptr::eq(source.metadata(), root.metadata()));
        assert_eq!(source.resolved_start_time(), None);
        assert!(matches!(
            source.native_schedule(),
            Err(RetainedNativeScheduleError::UnresolvedStartTime)
        ));
        Ok(String::new())
    })
    .unwrap();
}

#[test]
fn native_fmi_ls_preparation_replay_cannot_adopt_the_original_emission() {
    let _lock = session_test_guard();
    with_fixture_component(|component| {
        let code = preparation(&component);
        let library =
            NativeModelLibrary::from_preparation(code.clone(), deployment(component)).unwrap();
        let prepared = PreparedNativeFmiLsModel::from_library(library).unwrap();
        let mut reader = serde_json::Deserializer::from_slice(prepared.capsule_bytes());
        let replayed = rumoca_sim::deserialize_fmi_component(&mut reader).unwrap();
        reader.end().unwrap();
        let fresh = replayed.runtime_view().retain_assertion_root();
        assert!(!fresh.shares_root_owner(prepared.library().inputs().root()));
        assert!(!std::ptr::eq(
            fresh.metadata(),
            prepared.library().inputs().root().metadata()
        ));
        let fresh_code = preparation(&replayed);
        assert!(matches!(
            NativeModelLibrary::from_preparation(code, deployment(replayed)),
            Err(RetainedNativeScheduleError::ForeignPreparation)
        ));
        assert!(fresh_code.inputs().root().shares_root_owner(&fresh));
        assert!(std::ptr::eq(
            fresh_code.inputs().root().metadata(),
            fresh.metadata()
        ));
        Ok(String::new())
    })
    .unwrap();
}
