//! Markdown report tables whose columns carry their own definitions
//! (SPEC_0025 PR report).
//!
//! One [`Column`] list renders both a table's header and the legend under it,
//! so a column cannot be added, renamed, or dropped without its definition
//! moving with it.

/// Horizontal alignment of a column's cells.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Align {
    Left,
    Right,
}

/// One table column: its header, its one-line definition, and how a row
/// renders into it.
pub struct Column<'a, R> {
    pub header: &'static str,
    pub align: Align,
    pub legend: String,
    pub cell: Box<dyn Fn(&R) -> String + 'a>,
}

impl<'a, R> Column<'a, R> {
    pub fn new(
        header: &'static str,
        align: Align,
        legend: impl Into<String>,
        cell: impl Fn(&R) -> String + 'a,
    ) -> Self {
        Self {
            header,
            align,
            legend: legend.into(),
            cell: Box::new(cell),
        }
    }
}

/// The legend line that defines `header`; a reader that drops a column drops
/// the line that starts with this prefix.
pub fn legend_prefix(header: &str) -> String {
    format!("- `{header}`:")
}

/// Render `rows` under `columns`, then one legend line per distinct header.
pub fn render_markdown_table<'r, R: 'r>(
    columns: &[Column<'_, R>],
    rows: impl IntoIterator<Item = &'r R>,
) -> String {
    let mut out = String::new();
    let headers = columns.iter().map(|column| column.header);
    out.push_str(&markdown_row(headers));
    out.push_str(&markdown_row(columns.iter().map(
        |column| match column.align {
            Align::Left => "---",
            Align::Right => "---:",
        },
    )));
    for row in rows {
        out.push_str(&markdown_row(
            columns.iter().map(|column| (column.cell)(row)),
        ));
    }
    out.push('\n');
    out.push_str(&render_legend(columns));
    out
}

/// One line per distinct header, in column order.
pub fn render_legend<R>(columns: &[Column<'_, R>]) -> String {
    let mut seen = Vec::new();
    let mut out = String::new();
    for column in columns {
        if seen.contains(&column.header) {
            continue;
        }
        seen.push(column.header);
        out.push_str(&format!(
            "{} {}\n",
            legend_prefix(column.header),
            column.legend
        ));
    }
    out
}

fn markdown_row<S: AsRef<str>>(cells: impl IntoIterator<Item = S>) -> String {
    let cells = cells
        .into_iter()
        .map(|cell| cell.as_ref().to_string())
        .collect::<Vec<_>>();
    format!("| {} |\n", cells.join(" | "))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn header_and_legend_come_from_one_column_list() {
        let columns: Vec<Column<'_, (u32, u32)>> = vec![
            Column::new("Name", Align::Left, "the row", |row: &(u32, u32)| {
                row.0.to_string()
            }),
            Column::new("Value", Align::Right, "the value", |row: &(u32, u32)| {
                row.1.to_string()
            }),
            Column::new("Time", Align::Right, "seconds", |_: &(u32, u32)| {
                "1s".to_string()
            }),
            Column::new("Time", Align::Right, "seconds", |_: &(u32, u32)| {
                "2s".to_string()
            }),
        ];
        let rendered = render_markdown_table(&columns, &[(1, 2)]);
        assert_eq!(
            rendered,
            "| Name | Value | Time | Time |\n\
             | --- | ---: | ---: | ---: |\n\
             | 1 | 2 | 1s | 2s |\n\
             \n\
             - `Name`: the row\n\
             - `Value`: the value\n\
             - `Time`: seconds\n"
        );
    }
}
