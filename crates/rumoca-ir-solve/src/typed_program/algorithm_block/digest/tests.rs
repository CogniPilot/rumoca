//! The digest writer accepts every serde shape, not only the ones today's
//! block types happen to reach, so a field added to the block cannot reach an
//! untested encoding. Each shape is driven here directly.

use std::collections::BTreeMap;

use rumoca_core::SourceId;
use serde::Serialize;
use serde::ser::{Error as _, Serializer};

use super::{DigestWriter, digest_of};

fn hash<T: ?Sized + Serialize>(value: &T) -> String {
    digest_of(value)
        .expect("every probe is digestible")
        .to_string()
}

/// Calls `serialize_bytes`, which no derived impl does.
struct Bytes(&'static [u8]);

impl Serialize for Bytes {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_bytes(self.0)
    }
}

#[derive(Serialize)]
struct UnitStruct;

#[derive(Serialize)]
struct Newtype(u8);

#[derive(Serialize)]
struct TupleStruct(u8, u8);

#[derive(Serialize)]
struct Fields {
    a: u8,
    #[serde(skip_serializing_if = "Option::is_none")]
    b: Option<u8>,
}

#[derive(Serialize)]
enum Shape {
    Unit,
    Newtype(u8),
    Tuple(u8, u8),
    Struct {
        a: u8,
        #[serde(skip_serializing_if = "Option::is_none")]
        b: Option<u8>,
    },
}

/// Every shape the writer accepts has its own encoding: no two probes below,
/// one per serializer entry point, share a digest. The probes hold the same
/// small payload wherever a shape carries one, so only the shape differs.
#[test]
fn every_serde_shape_has_its_own_encoding() {
    let probes = [
        ("bool", hash(&true)),
        ("i8", hash(&1_i8)),
        ("i16", hash(&1_i16)),
        ("i32", hash(&1_i32)),
        ("i64", hash(&1_i64)),
        ("i128", hash(&1_i128)),
        ("u8", hash(&1_u8)),
        ("u16", hash(&1_u16)),
        ("u32", hash(&1_u32)),
        ("u64", hash(&1_u64)),
        ("u128", hash(&1_u128)),
        ("f32", hash(&1.0_f32)),
        ("f64", hash(&1.0_f64)),
        ("char", hash(&'\u{1}')),
        ("str", hash("\u{1}")),
        ("bytes", hash(&Bytes(&[1]))),
        ("none", hash(&None::<u8>)),
        ("some", hash(&Some(1_u8))),
        ("unit", hash(&())),
        ("unit variant", hash(&Shape::Unit)),
        ("newtype variant", hash(&Shape::Newtype(1))),
        ("source id", hash(&SourceId::from_source_name("a.mo"))),
        ("seq", hash(&vec![1_u8, 1])),
        ("tuple", hash(&(1_u8, 1_u8))),
        ("tuple variant", hash(&Shape::Tuple(1, 1))),
        ("map", hash(&BTreeMap::from([(1_u8, 1_u8)]))),
        ("struct", hash(&Fields { a: 1, b: Some(1) })),
        ("struct, field skipped", hash(&Fields { a: 1, b: None })),
        ("struct variant", hash(&Shape::Struct { a: 1, b: Some(1) })),
        (
            "struct variant, field skipped",
            hash(&Shape::Struct { a: 1, b: None }),
        ),
    ];
    for (index, (shape, digest)) in probes.iter().enumerate() {
        for (other, other_digest) in &probes[index + 1..] {
            assert_ne!(digest, other_digest, "{shape} and {other} encode alike");
        }
    }
}

/// Names are fixed by the type, so a unit struct, a newtype and a tuple struct
/// encode as their contents, and float bits (sign of zero included) are
/// content.
#[test]
fn names_are_not_content_and_float_bits_are() {
    assert_eq!(hash(&UnitStruct), hash(&()));
    assert_eq!(hash(&Newtype(7)), hash(&7_u8));
    assert_eq!(hash(&TupleStruct(1, 2)), hash(&(1_u8, 2_u8)));
    assert_ne!(hash(&0.0_f32), hash(&-0.0_f32));
    assert_ne!(hash(&0.0_f64), hash(&-0.0_f64));
}

/// A source id contributes only its tag, so the same source checked out in two
/// places digests alike.
#[test]
fn a_source_id_contributes_only_its_tag() {
    assert_eq!(
        hash(&SourceId::from_source_name("/one/checkout/a.mo")),
        hash(&SourceId::from_source_name("/another/checkout/b.mo"))
    );
}

/// A compact binary encoding, not a text one.
#[test]
fn the_writer_is_not_human_readable() {
    let mut writer = DigestWriter(blake3::Hasher::new());
    let serializer = &mut writer;
    assert!(!serializer.is_human_readable());
}

/// A `Serialize` impl that fails is reported, not digested as partial content.
#[test]
fn a_failing_serialize_impl_is_an_error() {
    struct Fails;

    impl Serialize for Fails {
        fn serialize<S: Serializer>(&self, _serializer: S) -> Result<S::Ok, S::Error> {
            Err(S::Error::custom("probe"))
        }
    }

    let error = digest_of(&Fails).expect_err("a failing impl must not produce a digest");
    assert_eq!(
        error.to_string(),
        "Solve algorithm block cannot be digested: probe"
    );
}
