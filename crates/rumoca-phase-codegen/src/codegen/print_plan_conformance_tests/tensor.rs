//! Reviewed decoders for the closed tensor print items. Each kernel has one
//! spelling here, independent of the template, over the item's issued
//! registers and counts only; a printed item conforms only when its text is
//! exactly that spelling, so every register, extent, stride, offset,
//! coordinate and operand order is checked, each number in its one canonical
//! decimal form.

use rumoca_ir_solve as solve;
use solve::{SolveBodyItem as Body, SolveTensorKernel as Kernel};

/// The exact lines one tensor kernel prints into register `d`.
fn spelling(d: usize, kernel: &Kernel) -> String {
    match kernel {
        Kernel::Fill { value, extent } => {
            format!("for (size_t k=0; k<{extent}; ++k) r{d}[k]=r{value}[0];\n")
        }
        Kernel::Identity { size: n } => format!(
            "for (size_t i=0; i<{n}; ++i) for (size_t j=0; j<{n}; ++j) r{d}[i*{n}+j]=(i==j)?1.0:0.0;\n"
        ),
        Kernel::Diagonal { operand, size: n } => format!(
            "for (size_t i=0; i<{n}; ++i) for (size_t j=0; j<{n}; ++j) r{d}[i*{n}+j]=(i==j)?r{operand}[i]:0.0;\n"
        ),
        Kernel::Concatenate { outer, parts } => {
            let mut text =
                format!("{{ size_t cursor=0;\nfor (size_t outer=0; outer<{outer}; ++outer) {{\n");
            for part in parts {
                let (operand, width) = (part.operand(), part.width());
                text.push_str(&format!(
                    "for (size_t k=0; k<{width}; ++k) r{d}[cursor++]=r{operand}[outer*{width}+k];\n"
                ));
            }
            text.push_str("} }\n");
            text
        }
        Kernel::ConstructAggregate { parts } => parts
            .iter()
            .map(|part| {
                let (operand, offset, width) = (part.operand(), part.offset(), part.width());
                format!("memcpy(&r{d}[{offset}], r{operand}, {width} * sizeof(r{operand}[0]));\n")
            })
            .collect(),
        Kernel::Scale {
            aggregate,
            scalar,
            extent,
        } => format!("for (size_t k=0; k<{extent}; ++k) r{d}[k]=r{aggregate}[k]*r{scalar}[0];\n"),
        Kernel::Transpose {
            operand,
            rows,
            columns,
            trailing,
        } => format!(
            "for (size_t i=0; i<{rows}; ++i) for (size_t j=0; j<{columns}; ++j) for (size_t k=0; k<{trailing}; ++k) r{d}[(j*{rows}+i)*{trailing}+k]=r{operand}[(i*{columns}+j)*{trailing}+k];\n"
        ),
        Kernel::MatrixMultiply {
            lhs,
            rhs,
            rows,
            inner,
            columns,
        } => format!(
            "for (size_t i=0; i<{rows}; ++i) for (size_t j=0; j<{columns}; ++j) {{ double sum=0.0; for (size_t k=0; k<{inner}; ++k) sum+=r{lhs}[i*{inner}+k]*r{rhs}[k*{columns}+j]; r{d}[i*{columns}+j]=sum; }}\n"
        ),
        Kernel::Cross { lhs: a, rhs: b } => format!(
            "r{d}[0]=r{a}[1]*r{b}[2]-r{a}[2]*r{b}[1];r{d}[1]=r{a}[2]*r{b}[0]-r{a}[0]*r{b}[2];r{d}[2]=r{a}[0]*r{b}[1]-r{a}[1]*r{b}[0];\n"
        ),
        Kernel::ProjectElement { aggregate, axes } => {
            let mut text = "{ size_t index=0;\n".to_owned();
            for axis in axes {
                text.push_str(&format!(
                    "index=index*{}+{};\n",
                    axis.extent(),
                    axis.coordinate()
                ));
            }
            text.push_str(&format!("r{d}[0]=r{aggregate}[index];\n}}\n"));
            text
        }
    }
}

/// Consumes the printed lines of one tensor item from `text`, or returns why
/// they are not the issued item.
pub(super) fn check(text: &mut &str, item: &Body) -> Result<(), String> {
    let Body::Tensor {
        destination,
        kernel,
        ..
    } = item
    else {
        return Err(format!("{item:?} is not a tensor item"));
    };
    let expected = spelling(*destination, kernel);
    match text.strip_prefix(expected.as_str()) {
        Some(rest) => {
            *text = rest;
            Ok(())
        }
        None => {
            let shown = text.get(..expected.len()).unwrap_or(text);
            Err(format!("{item:?} printed as {shown:?}"))
        }
    }
}
