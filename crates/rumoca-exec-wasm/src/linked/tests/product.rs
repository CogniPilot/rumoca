//! One product holding every kind of linked module, built twice from the same
//! checked sources: the browser profile and the linked profile.
use super::fixtures;
use crate::linked::{LinkedEntryId, LinkedEntryKind, LinkedProduct, LinkedProductAssembler};
use crate::{
    AdmittedPrivateProgram, PrivateProgramAdmission, compile_exact_assignment_schedule_wasm,
    compile_fmi_derivative_operation_wasm, compile_issued_assignment_rows_wasm,
};
use rumoca_ir_solve::{
    self as solve, PreparedKernelSource, VarLayout,
    fmi::{FmiComponent, FmiDerivativeCausalOperation, FmiDerivativeWorkspace},
};
use std::collections::BTreeMap;
use std::rc::Rc;

pub(super) fn id(kind: LinkedEntryKind, index: u32) -> LinkedEntryId {
    LinkedEntryId { kind, index }
}

/// The checked sources of the product, owned so operations can borrow them.
pub(super) struct Sources {
    pub(super) derivatives: [FmiComponent; 2],
    pub(super) exact: solve::SolveModel,
    pub(super) rows: solve::IssuedAssignmentRows,
    pub(super) private: Vec<solve::ScalarProgramBlock>,
    pub(super) event: solve::RetainedEventProgram,
}

impl Sources {
    pub(super) fn new() -> Self {
        Self {
            derivatives: [
                fixtures::component(fixtures::CAUSAL),
                fixtures::component(fixtures::MIXED),
            ],
            exact: fixtures::exact_model(),
            rows: fixtures::issued_rows(),
            private: fixtures::private_blocks(),
            event: fixtures::event_program(),
        }
    }
}

pub(super) fn with_operation<T>(
    component: &FmiComponent,
    body: impl FnOnce(&FmiDerivativeCausalOperation<'_>) -> T,
) -> T {
    let root = component.runtime_view().retain_assertion_root();
    let operation = root
        .try_derivative_operation()
        .unwrap()
        .try_causal_operation()
        .unwrap();
    body(&operation)
}

/// What each entry's arguments point at.
#[derive(Clone, Copy)]
pub(super) enum Shape {
    Derivative(FmiDerivativeWorkspace),
    Kernel { y: usize, p: usize, scratch: u32 },
}

/// A browser module and the entries it exports; derivative helpers are
/// linked into their own control module by role, as the browser does.
pub(super) enum BrowserModule {
    Derivative {
        helpers: Vec<(&'static str, Vec<u8>)>,
        control: Vec<u8>,
        entries: Vec<(LinkedEntryId, &'static str)>,
    },
    Kernel {
        bytes: Vec<u8>,
        entries: Vec<(LinkedEntryId, String)>,
    },
}

pub(super) struct Product {
    pub(super) linked: LinkedProduct,
    pub(super) browser: Vec<BrowserModule>,
    pub(super) shapes: BTreeMap<LinkedEntryId, Shape>,
}

/// The keys each kind's `PreparedKernelSource` constructor issues, computed
/// here independently of the assembler, in the owner's order.
pub(super) fn expected_keys(sources: &Sources) -> Vec<(LinkedEntryId, String)> {
    let mut issued = Vec::new();
    for (index, component) in (0..).zip(&sources.derivatives) {
        with_operation(component, |operation| {
            let source = operation.source();
            let key = PreparedKernelSource::new(&source.problem().layout, source.calls())
                .and_then(|key| key.fmi_derivative(operation))
                .unwrap();
            issued.push((id(LinkedEntryKind::Derivative, index), key.clone()));
            if operation.kinematic().is_some() {
                issued.push((id(LinkedEntryKind::Kinematic, index), key));
            }
        });
    }
    let model = &sources.exact;
    let exact = PreparedKernelSource::new(&model.problem.layout, &model.pure_calls)
        .and_then(|key| {
            key.exact_assignment_schedule(
                &model.problem.continuous.implicit_rhs,
                &model.problem.continuous.refresh_owners,
                fixtures::exact_schedule(model),
            )
        })
        .unwrap();
    issued.push((id(LinkedEntryKind::Exact, 0), exact));
    let rows = PreparedKernelSource::new(&fixtures::issued_layout(), &fixtures::empty_calls())
        .and_then(|key| key.issued_assignment_rows(&sources.rows))
        .unwrap();
    issued.push((id(LinkedEntryKind::Issued, 0), rows));
    let private =
        PreparedKernelSource::new(&fixtures::private_layout(), &fixtures::empty_calls()).unwrap();
    for (index, block) in (0..).zip(&sources.private) {
        let key = private.private_program(block, None).unwrap();
        issued.push((id(LinkedEntryKind::Private, index), key));
    }
    let event = &sources.event;
    let key = PreparedKernelSource::new(&event.source().model().problem.layout, event.table())
        .and_then(|key| key.retained_event(event))
        .unwrap();
    issued.push((id(LinkedEntryKind::Event, 0), key));
    issued
}

pub(super) fn private_admission(
    sources: &Sources,
) -> (PrivateProgramAdmission, Vec<AdmittedPrivateProgram>) {
    let admission =
        PrivateProgramAdmission::new(fixtures::private_layout(), Rc::new(fixtures::empty_calls()));
    let admitted = sources
        .private
        .iter()
        .map(|block| admission.admit(block.clone(), None).unwrap())
        .collect();
    (admission, admitted)
}

pub(super) fn build(sources: &Sources) -> Product {
    let mut builder = Builder {
        assembler: LinkedProductAssembler::new(),
        browser: Vec::new(),
        shapes: BTreeMap::new(),
    };
    builder.derivatives(sources);
    builder.exact(sources);
    builder.issued(sources);
    builder.private(sources);
    builder.event(sources);
    Product {
        linked: builder.assembler.finish(&[]).unwrap(),
        browser: builder.browser,
        shapes: builder.shapes,
    }
}

/// Feeds the assembler in the owner's order and builds each browser module
/// from the same checked source beside it.
struct Builder {
    assembler: LinkedProductAssembler,
    browser: Vec<BrowserModule>,
    shapes: BTreeMap<LinkedEntryId, Shape>,
}

impl Builder {
    fn derivatives(&mut self, sources: &Sources) {
        for (index, component) in (0..).zip(&sources.derivatives) {
            with_operation(component, |operation| self.derivative(index, operation));
        }
    }

    fn derivative(&mut self, index: u32, operation: &FmiDerivativeCausalOperation<'_>) {
        let issued = self.assembler.derivative(operation).unwrap();
        assert_eq!(issued[0], id(LinkedEntryKind::Derivative, index));
        let code = compile_fmi_derivative_operation_wasm(operation).unwrap();
        let helpers = [
            ("static_refresh", code.static_program()),
            ("dynamic_refresh", code.dynamic_program()),
            ("values", Some(code.values())),
            ("kinematic_values", code.kinematic()),
        ]
        .into_iter()
        .filter_map(|(role, part)| part.map(|part| (role, part.module_bytes().to_vec())))
        .collect();
        let mut entries = vec![(id(LinkedEntryKind::Derivative, index), "eval_derivatives")];
        if code.kinematic().is_some() {
            entries.push((id(LinkedEntryKind::Kinematic, index), "eval_kinematic"));
        }
        assert_eq!(
            issued,
            entries.iter().map(|(id, _)| *id).collect::<Vec<_>>()
        );
        for (entry, _) in &entries {
            self.shapes
                .insert(*entry, Shape::Derivative(code.workspace()));
        }
        self.browser.push(BrowserModule::Derivative {
            helpers,
            control: code.module_bytes().to_vec(),
            entries,
        });
    }

    /// One browser kernel module and the shapes of its entries.
    fn kernel(
        &mut self,
        bytes: &[u8],
        layout: &VarLayout,
        entries: Vec<(LinkedEntryId, String, u32)>,
    ) {
        for (entry, _, scratch) in &entries {
            let shape = Shape::Kernel {
                y: layout.y_scalars(),
                p: layout.p_scalars(),
                scratch: *scratch,
            };
            self.shapes.insert(*entry, shape);
        }
        self.browser.push(BrowserModule::Kernel {
            bytes: bytes.to_vec(),
            entries: entries
                .into_iter()
                .map(|(entry, export, _)| (entry, export))
                .collect(),
        });
    }

    fn exact(&mut self, sources: &Sources) {
        let model = &sources.exact;
        let (source, owners, schedule, layout, calls) = (
            &model.problem.continuous.implicit_rhs,
            &model.problem.continuous.refresh_owners,
            fixtures::exact_schedule(model),
            &model.problem.layout,
            &model.pure_calls,
        );
        let issued = self
            .assembler
            .exact(source, owners, schedule, layout, calls)
            .unwrap();
        assert_eq!(issued, id(LinkedEntryKind::Exact, 0));
        let code = compile_exact_assignment_schedule_wasm(source, owners, schedule, layout, calls)
            .unwrap();
        let entry = (issued, "eval_assignments".into(), code.scratch_bytes());
        self.kernel(code.module_bytes(), layout, vec![entry]);
    }

    fn issued(&mut self, sources: &Sources) {
        let (layout, calls) = (fixtures::issued_layout(), fixtures::empty_calls());
        let issued = self
            .assembler
            .issued(&sources.rows, &layout, &calls)
            .unwrap();
        assert_eq!(issued, id(LinkedEntryKind::Issued, 0));
        let code = compile_issued_assignment_rows_wasm(&sources.rows, &layout, &calls).unwrap();
        let entry = (issued, "eval_assignments".into(), code.scratch_bytes());
        self.kernel(code.module_bytes(), &layout, vec![entry]);
    }

    fn private(&mut self, sources: &Sources) {
        let (admission, admitted) = private_admission(sources);
        let programs = admitted.iter().collect::<Vec<_>>();
        let issued = self.assembler.private(&admission, &programs).unwrap();
        assert_eq!(
            issued,
            (0..3)
                .map(|index| id(LinkedEntryKind::Private, index))
                .collect::<Vec<_>>()
        );
        let private = admission.compile(&programs).unwrap();
        let entries = issued
            .into_iter()
            .zip(&private)
            .map(|(entry, program)| {
                (
                    entry,
                    program.export_name().to_string(),
                    program.scratch_bytes(),
                )
            })
            .collect();
        self.kernel(
            private[0].module_bytes(),
            &fixtures::private_layout(),
            entries,
        );
    }

    fn event(&mut self, sources: &Sources) {
        let event = &sources.event;
        let issued = self.assembler.event(event).unwrap();
        assert_eq!(issued, id(LinkedEntryKind::Event, 0));
        let layout = &event.source().model().problem.layout;
        let (code, _, _) =
            crate::emit::emit_event_program_module(event, layout, crate::emit::Linkage::Browser)
                .unwrap();
        let entry = (issued, "eval_private".into(), code.scratch_bytes());
        self.kernel(code.module_bytes(), layout, vec![entry]);
    }
}
