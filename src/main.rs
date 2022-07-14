mod lexer;
mod parser;
mod span;

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{prelude::*, Stream};
use color_eyre::{eyre::eyre, Report as EyreReport, Section};
use lexer::lexer;
use parser::parser;
use std::{env, fs};

fn main() -> Result<(), EyreReport> {
    color_eyre::install()?;

    let src = fs::read_to_string(env::args().nth(1).ok_or(eyre!("Expected file argument"))?)
        .section("Failed to read file.")?;
    let src_len = src.chars().count();

    let (tokens, lex_errs) = lexer().parse_recovery(src.as_str());

    let (ast, parse_errs) = if let Some(tokens) = tokens {
        parser().parse_recovery(Stream::from_iter(src_len..src_len + 1, tokens.into_iter()))
    } else {
        (None, vec![])
    };

    if let Some(ast) = ast {
        println!("{ast:?}");
    }

    let error_iter = lex_errs
        .into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string())));

    let has_errors = process_errors(error_iter, &src)?;

    if has_errors {
        std::process::exit(1);
    }

    Ok(())
}

fn process_errors(
    errs: impl IntoIterator<Item = Simple<String>>,
    src: &str,
) -> Result<bool, EyreReport> {
    let mut has_errors = false;

    for err in errs {
        has_errors = true;

        let report = Report::build(ReportKind::Error, (), err.span().start);

        let report = match err.reason() {
            chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                .with_message(format!(
                    "Unclosed delimiter {}",
                    delimiter.fg(Color::Yellow)
                ))
                .with_label(
                    Label::new(span.clone())
                        .with_message(format!(
                            "Unclosed delimiter {}",
                            delimiter.fg(Color::Yellow)
                        ))
                        .with_color(Color::Yellow),
                )
                .with_label(
                    Label::new(err.span())
                        .with_message(format!(
                            "Must be closed before this {}",
                            err.found()
                                .unwrap_or(&"end of file".to_string())
                                .fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),

            chumsky::error::SimpleReason::Unexpected => report
                .with_message(format!(
                    "{}, expected {}",
                    if err.found().is_some() {
                        "Unexpected token in input"
                    } else {
                        "Unexpected end of input"
                    },
                    if err.expected().len() == 0 {
                        "something else".to_string()
                    } else {
                        err.expected()
                            .map(|expected| match expected {
                                Some(expected) => expected.escape_debug().collect::<String>(),
                                None => "end of input".to_string(),
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    }
                ))
                .with_label(
                    Label::new(err.span())
                        .with_message(format!(
                            "Unexpected token {}",
                            err.found()
                                .unwrap_or(&"end of file".to_string())
                                .fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),

            chumsky::error::SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                Label::new(err.span())
                    .with_message(format!("{}", msg.fg(Color::Red)))
                    .with_color(Color::Red),
            ),
        };

        report.finish().eprint(Source::from(&src))?;
    }

    Ok(has_errors)
}
