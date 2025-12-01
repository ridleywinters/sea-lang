use std::path::PathBuf;

use crate::commands::FileSet;
use crate::parser::ast::{ModuleAST, ParseError, SourceLocation};
use crate::parser::lexer::Lexer;
use crate::parser::parser::Parser;

pub fn parse_module(file_set: &FileSet) -> Result<ModuleAST, ParseError> {
    let mut module = ModuleAST::new();

    for file_path in &file_set.files {
        parse_file(file_path, &mut module)?;
    }

    Ok(module)
}

fn parse_file(path: &PathBuf, module: &mut ModuleAST) -> Result<(), ParseError> {
    let source = std::fs::read_to_string(path).map_err(|e| ParseError {
        message: format!("Failed to read file '{}': {}", path.display(), e),
        location: None,
        file: Some(path.clone()),
        source_line: None,
        suggestion: None,
    })?;

    let source_lines: Vec<&str> = source.lines().collect();

    let mut lexer = Lexer::new(&source, path.clone());
    let tokens = lexer.tokenize_all().map_err(|e| {
        let source_line = source_lines
            .get(e.line.saturating_sub(1))
            .map(|s| s.to_string());
        ParseError {
            message: e.message,
            location: Some(SourceLocation {
                file: e.file,
                line: e.line,
                column: e.column,
            }),
            file: Some(path.clone()),
            source_line,
            suggestion: None,
        }
    })?;

    let mut parser = Parser::new(tokens, path.clone(), &source);
    let (file_ast, functions) = parser.parse_file()?;

    for func in functions {
        module.add_function(func);
    }

    module.files.push(file_ast);

    Ok(())
}
