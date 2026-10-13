use super::*;
use rumoca_compile::compile::{Session, SessionConfig};

fn prepared_return_predicates() -> PreparedNativeFmiLsModel {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "Return.mo",
            include_str!("../../../rumoca/tests/fixtures/native_return_predicates.mo"),
        )
        .unwrap();
    let dae = session.compile_model("ReturnPredicates").unwrap().dae;
    let component = crate::lower_fmi_export_component(&dae).unwrap();
    let root = component.runtime_view().retain_assertion_root();
    let code = rumoca_exec_wasm::compile_retained_native_preparation_wasm(
        root.native_preparation().unwrap(),
    )
    .unwrap();
    let deployment = component
        .into_codegen_view()
        .try_deployment(Default::default())
        .unwrap();
    PreparedNativeFmiLsModel::from_library(
        NativeModelLibrary::from_preparation(code, deployment).unwrap(),
    )
    .unwrap()
}

#[test]
fn the_prepared_pair_encodes_the_source_root_its_library_retains() {
    let prepared = prepared_return_predicates();
    let source = rumoca_eval_solve::fmi_wire::retained_fmi_component_wire(
        prepared.library().inputs().root(),
    )
    .unwrap();
    assert_eq!(
        prepared.capsule_bytes(),
        serde_json::to_vec(&source).unwrap()
    );
}
