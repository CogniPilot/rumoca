//! Publication identity is read from the original rendered request, never minted.

use quick_xml::{
    Reader,
    events::{BytesStart, Event},
};
use rumoca_core::artifact_build::PreparedBuildRequest;

pub(super) fn token<S>(request: &PreparedBuildRequest<'_, S>) -> Result<String, String> {
    let bytes = request
        .files()
        .get(std::path::Path::new("modelDescription.xml"))
        .ok_or("paired publication has no rendered modelDescription.xml")?;
    xml_token(bytes)
}

fn xml_token(bytes: &[u8]) -> Result<String, String> {
    let mut reader = Reader::from_reader(bytes);
    let mut depth = 0_usize;
    let mut token = None;
    let mut root_seen = false;
    loop {
        match reader
            .read_event()
            .map_err(|error| format!("publication XML: {error}"))?
        {
            Event::Start(element) => {
                if depth == 0 {
                    token = Some(root_token(&element, reader.decoder(), &mut root_seen)?);
                }
                depth += 1;
            }
            Event::Empty(element) if depth == 0 => {
                token = Some(root_token(&element, reader.decoder(), &mut root_seen)?);
            }
            Event::End(_) => depth = depth.checked_sub(1).ok_or("unbalanced publication XML")?,
            Event::Text(text) if depth == 0 && !text.iter().all(u8::is_ascii_whitespace) => {
                return Err("publication XML has text outside its root".into());
            }
            Event::Eof => break,
            _ => {}
        }
    }
    if depth != 0 {
        return Err("unclosed publication XML".into());
    }
    token.ok_or("publication XML has no original instantiation token".into())
}

fn root_token(
    element: &BytesStart<'_>,
    decoder: quick_xml::encoding::Decoder,
    root_seen: &mut bool,
) -> Result<String, String> {
    if *root_seen || element.name().as_ref() != b"fmiModelDescription" {
        return Err("publication XML must have one FMI root".into());
    }
    *root_seen = true;
    let mut token = None;
    for attribute in element.attributes() {
        let attribute = attribute.map_err(|error| format!("publication XML: {error}"))?;
        if attribute.key.as_ref() == b"instantiationToken" {
            let value = attribute
                .decode_and_unescape_value(decoder)
                .map_err(|error| format!("publication token: {error}"))?;
            if value.is_empty() || token.replace(value.into_owned()).is_some() {
                return Err("publication token must be nonempty and unique".into());
            }
        }
    }
    token.ok_or("publication XML has no original instantiation token".into())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn paired_publication_retains_original_escaped_token_and_refuses_replacement_roots() {
        assert_eq!(xml_token(br#"<fmiModelDescription instantiationToken="issued&amp;token"><ModelVariables/></fmiModelDescription>"#).unwrap(), "issued&token");
        assert_eq!(
            xml_token(br#"<fmiModelDescription instantiationToken="issued"/>"#).unwrap(),
            "issued"
        );
        for bytes in [
            br#"<fmiModelDescription/>"#.as_slice(),
            br#"<fmiModelDescription instantiationToken=""/>"#,
            br#"<foreign instantiationToken="issued"/>"#,
            br#"<fmiModelDescription instantiationToken="issued"/><fmiModelDescription instantiationToken="changed"/>"#,
            br#"<fmiModelDescription instantiationToken="issued" instantiationToken="changed"/>"#,
        ] { assert!(xml_token(bytes).is_err()); }
    }
}
