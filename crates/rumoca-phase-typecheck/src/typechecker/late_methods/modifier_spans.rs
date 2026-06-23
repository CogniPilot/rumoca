use super::*;

impl TypeChecker {
    /// Find a precise span for a modifier name inside a component declaration.
    ///
    /// Falls back to expression/component spans if source text or byte ranges are unavailable.
    pub(crate) fn find_modifier_name_span(
        &self,
        comp: &Component,
        modifier_name: &str,
    ) -> Option<Span> {
        if modifier_name.is_empty() {
            return None;
        }
        let source_id = self.source_map.get_id(&comp.location.file_name)?;
        let (_name, source) = self.source_map.get_source(source_id)?;
        let start = comp.location.start as usize;
        let mut end = comp.location.end as usize;
        if start >= source.len() {
            return None;
        }
        end = end.min(source.len());
        if start >= end {
            return None;
        }
        let snippet = source.get(start..end)?;
        if let Some(rel) = Self::find_modifier_identifier(snippet, modifier_name) {
            let abs_start = start + rel;
            let abs_end = abs_start + modifier_name.len();
            return Some(Span::from_offsets(source_id, abs_start, abs_end));
        }

        // Some component locations end before class modifications; scan declaration lines.
        let start_line = comp
            .location
            .start_line
            .max(comp.name_token.location.start_line) as usize;
        let end_line = comp
            .location
            .end_line
            .max(comp.location.start_line)
            .max(comp.name_token.location.start_line) as usize;
        for line in start_line..=end_line {
            let Some((line_start, line_end)) = Self::line_byte_range(source, line) else {
                continue;
            };
            let line_text = &source[line_start..line_end];
            if let Some(rel) = Self::find_modifier_identifier(line_text, modifier_name) {
                let abs_start = line_start + rel;
                let abs_end = abs_start + modifier_name.len();
                return Some(Span::from_offsets(source_id, abs_start, abs_end));
            }
        }
        None
    }

    pub(crate) fn line_byte_range(source: &str, line_number: usize) -> Option<(usize, usize)> {
        if line_number == 0 {
            return None;
        }
        let mut current_line = 1usize;
        let mut line_start = 0usize;
        for (idx, ch) in source.char_indices() {
            if current_line == line_number && ch == '\n' {
                return Some((line_start, idx));
            }
            if ch == '\n' {
                current_line += 1;
                line_start = idx + 1;
            }
        }
        (current_line == line_number).then_some((line_start, source.len()))
    }

    pub(crate) fn find_modifier_identifier(snippet: &str, modifier_name: &str) -> Option<usize> {
        let mut offset = 0usize;
        while let Some(found) = snippet[offset..].find(modifier_name) {
            let idx = offset + found;
            if !Self::has_identifier_boundaries(snippet, idx, modifier_name.len()) {
                offset = idx + modifier_name.len();
                continue;
            }
            let after = &snippet[idx + modifier_name.len()..];
            let after_trimmed = after.trim_start();
            if after_trimmed.starts_with('=')
                || after_trimmed.starts_with('(')
                || after_trimmed.starts_with(',')
                || after_trimmed.starts_with(')')
                || after_trimmed.starts_with(';')
            {
                return Some(idx);
            }
            offset = idx + modifier_name.len();
        }
        None
    }

    pub(crate) fn has_identifier_boundaries(text: &str, start: usize, len: usize) -> bool {
        let before = text[..start].chars().next_back();
        let after = text[start + len..].chars().next();
        !before.is_some_and(Self::is_identifier_char)
            && !after.is_some_and(Self::is_identifier_char)
    }

    pub(crate) fn is_identifier_char(ch: char) -> bool {
        ch.is_ascii_alphanumeric() || ch == '_'
    }
}
