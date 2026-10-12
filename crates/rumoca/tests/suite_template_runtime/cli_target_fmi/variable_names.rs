use std::ops::Range;

use quick_xml::Reader;
use quick_xml::events::Event;

fn supported_variable(version: &str, name: &[u8]) -> bool {
    match version {
        "fmi2" => name == b"ScalarVariable",
        "fmi3" => matches!(
            name,
            b"Float64" | b"Int32" | b"Int64" | b"Boolean" | b"String" | b"Enumeration"
        ),
        other => panic!("unexpected FMI version {other}"),
    }
}

fn variable_name_ranges(xml: &str, version: &str) -> Vec<Range<usize>> {
    let mut reader = Reader::from_str(xml);
    let mut parents: Vec<Vec<u8>> = Vec::new();
    let mut ranges = Vec::new();
    loop {
        let start = usize::try_from(reader.buffer_position()).expect("XML byte position");
        let event = reader.read_event().expect("valid model description XML");
        if let Event::Start(element) | Event::Empty(element) = &event {
            let direct_variable = parents.len() == 2
                && parents[0] == b"fmiModelDescription"
                && parents[1] == b"ModelVariables";
            if direct_variable && supported_variable(version, element.name().as_ref()) {
                let end = usize::try_from(reader.buffer_position()).expect("XML byte position");
                let tag = &xml[start..end];
                let name_start = start + tag.find(" name=\"").expect("variable name") + 7;
                let name_end = name_start
                    + xml[name_start..end]
                        .find('"')
                        .expect("terminated variable name");
                ranges.push(name_start..name_end);
            }
        }
        match event {
            Event::Start(element) => parents.push(element.name().as_ref().to_vec()),
            Event::End(_) => {
                parents.pop().expect("matching XML parent");
            }
            Event::Eof => break,
            _ => {}
        }
    }
    ranges.truncate(2);
    ranges
}

pub(super) fn duplicate_variable_name(xml: &str, version: &str) -> String {
    let ranges = variable_name_ranges(xml, version);
    assert_eq!(
        ranges.len(),
        2,
        "duplicate-name control needs two variables"
    );
    let mut changed = xml.to_string();
    changed.replace_range(ranges[1].clone(), &xml[ranges[0].clone()]);
    assert_ne!(xml, changed, "duplicate-name control must alter metadata");
    changed
}

#[test]
fn duplicate_name_control_preserves_mixed_kind_document_order() {
    let xml = r#"<fmiModelDescription><ModelVariables><Int32 name="n"/><Float64 name="time"/><Int32 name="y"/></ModelVariables></fmiModelDescription>"#;
    assert_eq!(
        duplicate_variable_name(xml, "fmi3"),
        xml.replacen("name=\"time\"", "name=\"n\"", 1)
    );
}

#[test]
fn duplicate_name_control_includes_enumeration_variables() {
    let xml = r#"<fmiModelDescription><ModelVariables><Enumeration name="mode"/><Boolean name="active"/></ModelVariables></fmiModelDescription>"#;
    assert_eq!(
        duplicate_variable_name(xml, "fmi3"),
        xml.replacen("name=\"active\"", "name=\"mode\"", 1)
    );
}

#[test]
fn duplicate_name_control_ignores_non_variable_and_nested_elements() {
    for (version, kind) in [("fmi2", "ScalarVariable"), ("fmi3", "Int32")] {
        let xml = format!(
            "<fmiModelDescription><Annotations><{kind} name=\"outside\"/></Annotations><ModelVariables><{kind} name=\"first\"><Annotations><{kind} name=\"nested\"/></Annotations></{kind}><{kind} name=\"second\"/></ModelVariables></fmiModelDescription>"
        );
        assert_eq!(
            duplicate_variable_name(&xml, version),
            xml.replacen("name=\"second\"", "name=\"first\"", 1)
        );
    }
}
