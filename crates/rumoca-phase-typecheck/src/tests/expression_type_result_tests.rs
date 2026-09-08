//! Witnesses for the closed expression-type result.
//!
//! Every test here names the exact mutation it fails against, and mutates only
//! that discriminator. Negative witnesses sit on the *unsafe* side of MLS
//! §10.6.13 on purpose: Integer-to-Real is a legal implicit conversion, so a
//! Real target hides the defect and only an Integer target exposes it.
//!
//! The discriminating path is the algorithm assignment, whose predicate
//! (`assignment_types_compatible` -> `builtin_roots_incompatible`) is
//! asymmetric and therefore rejects `(Integer, Real)`. The equation predicate
//! is symmetric for numeric pairs and would make every range/array witness
//! below vacuous, so no witness uses one.

use super::*;

/// Messages of the `ET002` diagnostics a source produces.
fn et002_messages(source: &str) -> Vec<String> {
    typecheck_diagnostics(source)
        .iter()
        .filter(|diagnostic| diagnostic.code.as_deref() == Some("ET002"))
        .map(|diagnostic| diagnostic.message.clone())
        .collect()
}

/// True when the source produces no error at all.
fn accepted(source: &str) -> bool {
    !typecheck_diagnostics(source).has_errors()
}

/// True when some `ET002` message contains `needle`.
///
/// Asserting on the message, not merely on the code, is what keeps these
/// witnesses non-vacuous: a shape diagnostic also carries `ET002`, so a bare
/// "an error was produced" assertion would pass for the wrong reason.
fn rejects_with(source: &str, needle: &str) -> bool {
    et002_messages(source)
        .iter()
        .any(|message| message.contains(needle))
}

/// A model whose only checked content is one algorithm section.
///
/// The algorithm assignment is used rather than an equation because
/// `assignment_types_compatible` is asymmetric and therefore rejects
/// `(Integer, Real)`; the equation predicate permits any Real/Integer pair and
/// would make every numeric witness here vacuous.
fn assignment_model(declarations: &str, statements: &str) -> String {
    format!("model Test\n{declarations}\nalgorithm\n{statements}\nend Test;\n")
}

/// A model whose only checked content is one equation section.
fn equation_model(declarations: &str, equations: &str) -> String {
    format!("model Test\n{declarations}\nequation\n{equations}\nend Test;\n")
}

// ---------------------------------------------------------------------------
// N1 / P1 / P3 - range element type is a function of every bound.
// ---------------------------------------------------------------------------

/// Fails against: reverting the `Range` arm to `start` only, and equally
/// against any repair that reads `start` and `end` but ignores `step`.
/// Discriminator mutated: the step, `1:3` -> `1:0.5:3`; nothing else moves.
#[test]
fn range_with_a_real_step_is_a_real_vector() {
    let integer_step = assignment_model("    Integer y[3];", "    y := 1:1:3;");
    let real_step = assignment_model("    Integer y[3];", "    y := 1:0.5:3;");

    assert!(
        accepted(&integer_step),
        "P1: an all-Integer range must still bind to an Integer target: {:?}",
        typecheck_diagnostics(&integer_step)
    );
    assert!(
        rejects_with(
            &real_step,
            "type mismatch: expected `Integer`, found `Real`"
        ),
        "N1: a Real step makes the range Real (MLS §10.4.3): {:?}",
        et002_messages(&real_step)
    );
}

/// Fails against: a repair that types every range `Real` "to be safe". That
/// mutant passes the negative above and is caught only here.
#[test]
fn two_bound_integer_range_stays_integer() {
    let source = assignment_model("    Integer y[3];", "    y := 1:3;");
    assert!(
        accepted(&source),
        "P1: `1:3` is an Integer vector: {:?}",
        typecheck_diagnostics(&source)
    );
}

/// Fails against: a repair that rejects any mixed-type range outright instead
/// of promoting it. Listed as a control only: §10.6.13 makes it pass before and
/// after, so it can never evidence a fix.
#[test]
fn real_target_accepts_a_promoted_range() {
    let source = assignment_model("    Real y[3];", "    y := 1:0.5:3;");
    assert!(
        accepted(&source),
        "P3: a Real target accepts the promoted range: {:?}",
        typecheck_diagnostics(&source)
    );
}

// ---------------------------------------------------------------------------
// N2 / N3 / P2 / P4 - array constructors consume every position.
// ---------------------------------------------------------------------------

/// Fails against: reverting the `Array` arm to `elements.first()`.
/// Discriminator mutated: element 2, `2` -> `2.5`.
#[test]
fn array_constructor_promotes_a_later_real_element() {
    let all_integer = assignment_model("    Integer y[2];", "    y := {1, 2};");
    let trailing_real = assignment_model("    Integer y[2];", "    y := {1, 2.5};");

    assert!(
        accepted(&all_integer),
        "P2: `{{1, 2}}` is an Integer array: {:?}",
        typecheck_diagnostics(&all_integer)
    );
    assert!(
        rejects_with(
            &trailing_real,
            "type mismatch: expected `Integer`, found `Real`"
        ),
        "N2: the maximally expanded type is Real (MLS §10.4): {:?}",
        et002_messages(&trailing_real)
    );
}

/// Fails against: a fold that stops after the first two positions. N2 alone
/// cannot catch that mutant; only a third position can.
/// Discriminator mutated: element 3, `3` -> `2.5`.
#[test]
fn array_constructor_reaches_the_third_position() {
    let all_integer = assignment_model("    Integer y[3];", "    y := {1, 2, 3};");
    let third_real = assignment_model("    Integer y[3];", "    y := {1, 2, 2.5};");

    assert!(
        accepted(&all_integer),
        "P2: three Integer elements stay Integer: {:?}",
        typecheck_diagnostics(&all_integer)
    );
    assert!(
        rejects_with(
            &third_real,
            "type mismatch: expected `Integer`, found `Real`"
        ),
        "N3: position 3 must reach the fold: {:?}",
        et002_messages(&third_real)
    );
}

/// **The P4 mutant.** A repair that merely "rejects arrays whose elements have
/// different types" is wrong: MLS §10.4 explicitly permits mixing, "Real and
/// Integer subtypes can be mixed resulting in a Real result array".
///
/// This is the only **positive** witness against that mutant, but not the only
/// witness: N2 and N3 also fail under it, precisely because their assertions
/// are on message text. Under the mutant the array becomes `Invalid`,
/// `value_identity()` yields `None`, the compatibility check returns early, and
/// the expected "type mismatch" message is never emitted. What P4 uniquely
/// establishes is that the legal program is still *accepted*, which no negative
/// witness can show.
#[test]
fn mixed_numeric_array_is_promoted_not_rejected() {
    let source = assignment_model("    Real y[2];", "    y := {1, 2.5};");
    assert!(
        accepted(&source),
        "P4: a mixed Integer/Real array promotes to Real rather than failing: {:?}",
        typecheck_diagnostics(&source)
    );
}

/// Fails against: a promotion function that folds Boolean into the numeric
/// lattice, and equally against one that returns "not inferable" for a
/// non-numeric pair, because that abstention silently suppresses the check.
/// Discriminator mutated: element 2, `false` -> `1`.
#[test]
fn boolean_and_integer_elements_are_not_type_compatible() {
    let all_boolean = assignment_model("    Boolean b[2];", "    b := {true, false};");
    let mixed = assignment_model("    Boolean b[2];", "    b := {true, 1};");

    assert!(
        accepted(&all_boolean),
        "control: a uniform Boolean array is legal: {:?}",
        typecheck_diagnostics(&all_boolean)
    );
    assert!(
        rejects_with(&mixed, "array constructor elements are not type compatible"),
        "N4: Boolean is outside the numeric lattice (MLS §6.7): {:?}",
        et002_messages(&mixed)
    );
}

// ---------------------------------------------------------------------------
// N5 / P6 - enumeration identity, never enumeration spelling.
// ---------------------------------------------------------------------------

const TWO_ENUMERATIONS: &str = "\
type Colors = enumeration(red, green);
type Sizes = enumeration(red, green);
";

/// Fails against: any scalar-domain enum with a flat `Enumeration` variant
/// carrying no identity - that is, exactly the `ScalarType` hoist this cut
/// declined. Such a carrier cannot tell `Colors` from `Sizes`.
///
/// Both enumerations declare literals with the **same spelling in the same
/// order**, so nothing but identity separates them.
/// Discriminator mutated: the owner of element 2, `Colors.red` -> `Sizes.red`.
#[test]
fn same_spelling_enumeration_literals_are_still_two_types() {
    let same_owner = format!(
        "{TWO_ENUMERATIONS}{}",
        assignment_model("    Colors c[2];", "    c := {Colors.red, Colors.green};")
    );
    let wrong_owner = format!(
        "{TWO_ENUMERATIONS}{}",
        assignment_model("    Colors c[2];", "    c := {Colors.red, Sizes.red};")
    );

    assert!(
        accepted(&same_owner),
        "P6: one enumeration composed with itself is legal: {:?}",
        typecheck_diagnostics(&same_owner)
    );
    assert!(
        rejects_with(
            &wrong_owner,
            "array constructor elements use two different enumeration types"
        ),
        "N5: `red` spelled the same in two enumerations is two identities: {:?}",
        et002_messages(&wrong_owner)
    );
}

// ---------------------------------------------------------------------------
// N8 / P5 - the iterator binder comes from its range domain.
// ---------------------------------------------------------------------------

/// Fails against: fixing the `Range` arm while leaving the iterator asserted
/// `Integer`. That short-circuit runs before every other rule, so a correct
/// range rule is masked inside the loop body unless the binder derives from it.
/// Discriminator mutated: the loop range's step, `1:1:3` -> `1:0.5:3`.
#[test]
fn iterator_over_a_real_range_binds_real() {
    let integer_range = assignment_model(
        "    Integer k;",
        "    for i in 1:1:3 loop\n        k := i;\n    end for;",
    );
    let real_range = assignment_model(
        "    Integer k;",
        "    for i in 1:0.5:3 loop\n        k := i;\n    end for;",
    );

    assert!(
        accepted(&integer_range),
        "P5: an Integer iterator is still usable in its body: {:?}",
        typecheck_diagnostics(&integer_range)
    );
    assert!(
        rejects_with(
            &real_range,
            "type mismatch: expected `Integer`, found `Real`"
        ),
        "N8: the binder is Real, so the body assignment is ill-typed: {:?}",
        et002_messages(&real_range)
    );
}

/// Fails against: the unconditional `Integer` binder. A Boolean iterator body
/// must see Boolean, and must not be assignable to an Integer.
/// Discriminator mutated: the target's declared type, `Boolean` -> `Integer`.
#[test]
fn iterator_over_a_boolean_domain_binds_boolean() {
    let boolean_target = assignment_model(
        "    Boolean k;",
        "    for b in {false, true} loop\n        k := b;\n    end for;",
    );
    let integer_target = assignment_model(
        "    Integer k;",
        "    for b in {false, true} loop\n        k := b;\n    end for;",
    );

    assert!(
        accepted(&boolean_target),
        "a Boolean binder assigns to a Boolean: {:?}",
        typecheck_diagnostics(&boolean_target)
    );
    assert!(
        rejects_with(
            &integer_target,
            "type mismatch: expected `Integer`, found `Boolean`"
        ),
        "an unconditionally Integer binder would accept this: {:?}",
        et002_messages(&integer_target)
    );
}

/// Fails against: the unconditional `Integer` binder. An enumeration iterator
/// binds that exact enumeration, which is not an Integer.
/// Discriminator mutated: the target's declared type, `Colors` -> `Integer`.
#[test]
fn iterator_over_an_enumeration_domain_binds_that_enumeration() {
    let enumeration_target = format!(
        "{TWO_ENUMERATIONS}{}",
        assignment_model(
            "    Colors k;",
            "    for c in {Colors.red, Colors.green} loop\n        k := c;\n    end for;",
        )
    );
    let integer_target = format!(
        "{TWO_ENUMERATIONS}{}",
        assignment_model(
            "    Integer k;",
            "    for c in {Colors.red, Colors.green} loop\n        k := c;\n    end for;",
        )
    );

    assert!(
        accepted(&enumeration_target),
        "an enumeration binder assigns to that enumeration: {:?}",
        typecheck_diagnostics(&enumeration_target)
    );
    assert!(
        rejects_with(
            &integer_target,
            "type mismatch: expected `Integer`, found `Colors`"
        ),
        "an unconditionally Integer binder would accept this: {:?}",
        et002_messages(&integer_target)
    );
}

// ---------------------------------------------------------------------------
// N6 - output-expression-list positions 2..n.
// ---------------------------------------------------------------------------

const TWO_OUTPUT_FUNCTION: &str = "\
function pair
    output Real y1;
    output Real y2;
algorithm
    y1 := 1.0;
    y2 := 2.0;
end pair;
";

/// Fails against: any repair that types a tuple from one element, including
/// "use the last element" and "use the promoted join of the elements" - a tuple
/// has no join. Position 1 agrees in both variants, so only position 2 moves.
/// Discriminator mutated: the declared type of the second target, `Real` -> `Boolean`.
#[test]
fn output_expression_list_checks_positions_after_the_first() {
    let matching = format!(
        "{TWO_OUTPUT_FUNCTION}\
model Test
    Real x;
    Real b;
equation
    (x, b) = pair();
end Test;
"
    );
    let mismatched = format!(
        "{TWO_OUTPUT_FUNCTION}\
model Test
    Real x;
    Boolean b;
equation
    (x, b) = pair();
end Test;
"
    );

    assert!(
        accepted(&matching),
        "control: both positions agree: {:?}",
        typecheck_diagnostics(&matching)
    );
    assert!(
        rejects_with(
            &mismatched,
            "type mismatch: expected `Boolean`, found `Real`"
        ),
        "N6: position 2 must be compared against output 2: {:?}",
        et002_messages(&mismatched)
    );
}

// ---------------------------------------------------------------------------
// Licensed numeric promotion, per operator class.
// ---------------------------------------------------------------------------

/// Every operator MLS licenses Integer/Real promotion for, checked in both
/// directions: two Integer operands stay Integer, and one Real operand makes
/// the result Real.
///
/// Fails against: a promotion function that widens to Real unconditionally
/// (the all-Integer half fails), and against one that requires identical roots
/// before promoting (the mixed half fails).
#[test]
fn licensed_numeric_promotion_holds_for_each_scalar_operator_class() {
    for operator in ["+", "-", "*"] {
        let integer_only = assignment_model("    Integer n;", &format!("    n := 3 {operator} 2;"));
        assert!(
            accepted(&integer_only),
            "`{operator}` on two Integers stays Integer: {:?}",
            typecheck_diagnostics(&integer_only)
        );

        let mixed = assignment_model("    Integer n;", &format!("    n := 3.5 {operator} 2;"));
        assert!(
            rejects_with(&mixed, "type mismatch: expected `Integer`, found `Real`"),
            "`{operator}` with one Real operand yields Real (MLS §6.7): {:?}",
            et002_messages(&mixed)
        );
    }
}

/// The elementwise operator classes carry the same promotion rule (MLS §10.6).
/// Array operands are used so the leading `.` cannot be lexed as part of a
/// numeric literal.
#[test]
fn licensed_numeric_promotion_holds_for_each_elementwise_operator_class() {
    for operator in [".+", ".-", ".*"] {
        let integer_only = assignment_model(
            "    Integer n[2];",
            &format!("    n := {{3, 4}} {operator} {{2, 1}};"),
        );
        assert!(
            accepted(&integer_only),
            "`{operator}` on two Integer arrays stays Integer: {:?}",
            typecheck_diagnostics(&integer_only)
        );

        let mixed = assignment_model(
            "    Integer n[2];",
            &format!("    n := {{3.5, 4.5}} {operator} {{2, 1}};"),
        );
        assert!(
            rejects_with(&mixed, "type mismatch: expected `Integer`, found `Real`"),
            "`{operator}` with a Real operand yields Real: {:?}",
            et002_messages(&mixed)
        );
    }
}

/// MLS §10.6.5 / SPEC_0022 TYPE-034: division is always Real, even for two
/// Integer operands. This is the branch the `.or(Some(common))` deletion must
/// preserve verbatim; N7 alone does not protect it.
#[test]
fn integer_division_is_real() {
    let real_target = assignment_model("    Real q;", "    q := 3 / 2;");
    let integer_target = assignment_model("    Integer n;", "    n := 3 / 2;");

    assert!(
        accepted(&real_target),
        "P7: `3 / 2` is Real: {:?}",
        typecheck_diagnostics(&real_target)
    );
    assert!(
        rejects_with(
            &integer_target,
            "type mismatch: expected `Integer`, found `Real`"
        ),
        "G2: `/` must not be allowed to yield Integer: {:?}",
        et002_messages(&integer_target)
    );
}

// ---------------------------------------------------------------------------
// Illegal Boolean and String operands, and the ET002 owner that already has
// them. These are the pre-change witnesses for the `.or(Some(common))`
// deletion: the fallback is only dead if this owner really rejects them.
// ---------------------------------------------------------------------------

/// **G1, the deletion gate.** `false / true` must be rejected by
/// `require_numeric_expression` with `ET002`, and by that owner alone. If this
/// ever shows the program accepted, the `.or(Some(common))` tail was live and
/// the deletion in this cut is wrong.
#[test]
fn boolean_division_is_rejected_by_the_numeric_operand_owner() {
    let source = equation_model("    Boolean b;", "    b = false / true;");
    assert!(
        rejects_with(&source, "operator `/` expects Real or Integer operand(s)"),
        "G1: the numeric operand owner rejects Boolean division: {:?}",
        et002_messages(&source)
    );
}

/// The same owner covers String operands. Deleting the fallback must not
/// change who reports this or how many times.
#[test]
fn string_division_is_rejected_by_the_numeric_operand_owner() {
    let source = equation_model("    String s;", "    s = \"a\" / \"b\";");
    assert!(
        rejects_with(&source, "operator `/` expects Real or Integer operand(s)"),
        "String operands are rejected by the numeric operand owner: {:?}",
        et002_messages(&source)
    );
}

/// P8: MLS §3.6.1 concatenation. Fails against routing all of `+` through a
/// numeric lattice, or a promotion function that rejects every non-numeric pair.
#[test]
fn string_concatenation_is_still_accepted() {
    let source = equation_model("    String s;", "    s = \"a\" + \"b\";");
    assert!(
        accepted(&source),
        "P8: `String + String` is legal: {:?}",
        typecheck_diagnostics(&source)
    );
}

/// Boolean arithmetic stays rejected by its existing owner, and the composition
/// site adds nothing: re-proving an issued fact is prohibited.
#[test]
fn boolean_addition_is_rejected_once_by_the_operand_owner() {
    let source = equation_model("    Boolean b;", "    b = true + false;");
    let messages = et002_messages(&source);
    assert!(
        messages
            .iter()
            .any(|message| message.contains("operator `+` expects Real or Integer operand(s)")),
        "Boolean arithmetic is rejected by the operand owner: {messages:?}"
    );
    assert!(
        !messages
            .iter()
            .any(|message| message.contains("binary operands are not type compatible")),
        "the composition site must not re-prove what ET002 already issued: {messages:?}"
    );
}

// ---------------------------------------------------------------------------
// The minter: `Known` cannot hold the unresolved sentinel.
//
// `ExpressionType::Known` takes a `ResolvedTypeId`, whose tuple field is
// private to the `expression_type` module and whose only producer rejects
// `TypeId::UNKNOWN`. So the fourth state "resolved to the unresolved sentinel"
// is a type error outside that module, not a guarded case. Three mutations are
// therefore refused by the compiler **today**:
//
//   * `ExpressionType::Known(TypeId::UNKNOWN)` anywhere outside the module -
//     mismatched types, `TypeId` is not `ResolvedTypeId`;
//   * `ResolvedTypeId(TypeId::UNKNOWN)` outside the module - the tuple field is
//     private, so the constructor function is not in scope;
//   * reading `.0` off a `ResolvedTypeId` to re-wrap it - same privacy error.
//
// What the compiler cannot refuse is a future edit to the declaration itself;
// no type constrains its own future shape. `minter_api_shape_forbids_*` below
// pins that, and the behavioural half is pinned by `checked_constructor_*`.
// ---------------------------------------------------------------------------

/// The checked constructor maps the unresolved sentinel to a reasoned
/// abstention rather than to a `Known`.
///
/// Fails against: dropping the `is_unknown()` test in `ResolvedTypeId::new`,
/// which is the single behavioural check behind the whole closure.
/// Discriminator mutated: the identity passed in, a resolved one -> `UNKNOWN`.
#[test]
fn checked_constructor_refuses_the_unresolved_sentinel() {
    let unresolved = ExpressionType::known(TypeId::UNKNOWN);
    assert!(
        matches!(unresolved, ExpressionType::Unknown(reason) if !reason.is_empty()),
        "an unresolved identity must become a reasoned abstention, got {unresolved:?}"
    );
    assert_eq!(
        unresolved.value_identity(),
        None,
        "an abstention must yield no identity"
    );
}

/// The source shape that makes the three compile-time refusals above possible.
///
/// This is a gate on the declaration, not on behaviour, because the mutations
/// it catches are edits to the type itself: the compiler enforces the rule but
/// cannot protect the rule's own statement.
///
/// Fails against, in order: making the tuple field visible
/// (`struct ResolvedTypeId(pub(crate) TypeId)`); adding a second producer that
/// skips the check; and re-declaring the variant to take a raw `TypeId`
/// (`Known(TypeId)`).
#[test]
fn minter_api_shape_forbids_manufacturing_a_resolved_identity() {
    let carrier = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src/expression_type.rs");
    let source = std::fs::read_to_string(&carrier)
        .unwrap_or_else(|error| panic!("read {}: {error}", carrier.display()));

    assert!(
        source.contains("pub(crate) struct ResolvedTypeId(TypeId);"),
        "the proven wrapper's field must stay private; a visible field lets any \
         caller manufacture `Known(UNKNOWN)` without the check"
    );
    assert_eq!(
        source.matches("Self(type_id)").count(),
        1,
        "`ResolvedTypeId::new` must remain the only producer; a second one \
         would be a bypassing constructor"
    );
    assert!(
        source.contains("(!type_id.is_unknown()).then_some(Self(type_id))"),
        "the sole producer must keep its sentinel test"
    );
    assert!(
        source.contains("Known(ResolvedTypeId)") && !source.contains("Known(TypeId)"),
        "the `Known` arm must keep the proven wrapper; taking a raw `TypeId` \
         restores the fourth state"
    );
}

/// End to end: an overlay identity that never resolved must not reach a user
/// as a Rust `Debug` rendering where a Modelica type name belongs.
///
/// `add_instanced_component` inserts `type_id: TypeId::UNKNOWN`, and `/` routes
/// both operands through `require_numeric_expression`, which emits `ET002`
/// **without** an `is_unknown` guard of its own. Under the pre-repair minter
/// the operand types as `Known(UNKNOWN)`, `format_type_name` finds no entry and
/// falls back to `{type_id:?}`, and the message reads
/// ``found `TypeId(4294967295)` ``.
///
/// Fails against: minting `Known(semantics.type_id)` unguarded at the
/// `SemanticLookup::Found` arm.
#[test]
fn unresolved_overlay_identity_never_leaks_a_debug_type_name() {
    let source = r#"
        model Test
            Real u;
            Real y;
        equation
            y = u / 2;
        end Test;
    "#;
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.inner().clone();
    let model = tree
        .get_class_by_qualified_name("Test")
        .expect("Test class should exist");

    let mut overlay = InstanceOverlay::new();
    for name in ["u", "y"] {
        add_instanced_component(
            &mut overlay,
            &format!("Test.{name}"),
            model.components.get(name).expect("component"),
            true,
        );
    }

    let rendered: Vec<String> =
        match typecheck_instanced_test_projection(&tree, &mut overlay, "Test") {
            Ok(()) => Vec::new(),
            Err(diagnostics) => diagnostics
                .iter()
                .map(|diagnostic| diagnostic.message.clone())
                .collect(),
        };
    assert!(
        !rendered.iter().any(|message| message.contains("TypeId(")),
        "a diagnostic rendered a Rust identity where a Modelica type name belongs: {rendered:?}"
    );
}
