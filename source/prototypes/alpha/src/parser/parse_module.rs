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
    })?;

    let mut lexer = Lexer::new(&source, path.clone());
    let tokens = lexer.tokenize_all().map_err(|e| ParseError {
        message: e.message,
        location: Some(SourceLocation {
            file: e.file,
            line: e.line,
            column: e.column,
        }),
    })?;

    let mut parser = Parser::new(tokens, path.clone());
    let (file_ast, functions) = parser.parse_file()?;

    for func in functions {
        module.add_function(func);
    }

    module.files.push(file_ast);

    Ok(())
}
