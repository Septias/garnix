use lsp_types::Position;

pub(crate) fn to_lsp_range(span: &parser::Span, source: &str) -> lsp_types::Range {
    let mut start = 0;
    let mut line = 0;
    source
        .lines()
        .take_while(|source_line| {
            let new = start + source_line.len();
            if new < span.start {
                start = new;
                line += 1;
                true
            } else {
                false
            }
        })
        .for_each(|_| ());

    lsp_types::Range {
        start: Position {
            line,
            character: (span.start - start) as u32,
        },
        end: Position {
            line,
            character: (span.end - start) as u32,
        },
    }
}

pub(crate) fn _from_lsp_position(pos: Position, source: &str) -> usize {
    let start_offset = source
        .lines()
        .take(pos.line as usize)
        .fold(0, |acc, b| acc + b.len());

    start_offset + pos.character as usize
}

fn _from_lsp_range(range: &lsp_types::Range, source: &str) -> parser::Span {
    let start_offset = source
        .lines()
        .take(range.start.line as usize)
        .fold(0, |acc, b| acc + b.len());

    let end_offset = source
        .lines()
        .take(range.end.line as usize)
        .fold(0, |acc, b| acc + b.len());

    parser::Span {
        start: start_offset + range.start.character as usize,
        end: end_offset + range.end.character as usize,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_lsp_range() {
        let source = "aaaaaa\nbeeeeeee\nceeeeee";
        assert_eq!(
            to_lsp_range(&parser::Span { start: 11, end: 13 }, source),
            lsp_types::Range {
                start: Position {
                    line: 2,
                    character: 5
                },
                end: Position {
                    line: 2,
                    character: 7
                }
            }
        );
    }

    #[test]
    fn test_from_lsp_position() {
        let source = "aaaaaa\nbeeeeeee\nceeeeee";
        assert_eq!(
            _from_lsp_position(
                Position {
                    line: 1,
                    character: 5
                },
                source
            ),
            12
        );
    }
}
