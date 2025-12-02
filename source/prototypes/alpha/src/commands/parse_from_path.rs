use std::path::PathBuf;

use super::file_set::FileSet;
use crate::parser::{ModuleAST, ParseError, parse_module};

#[derive(Debug)]
pub enum ParseFromPathError {
    InvalidPath(PathBuf),
    IoError(std::io::Error),
    ParseError(ParseError),
}

impl std::fmt::Display for ParseFromPathError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseFromPathError::InvalidPath(path) => {
                write!(
                    f,
                    "Path '{}' does not exist or is not a valid file or folder",
                    path.display()
                )
            }
            ParseFromPathError::IoError(e) => write!(f, "IO error: {}", e),
            ParseFromPathError::ParseError(e) => write!(f, "{}", e),
        }
    }
}

impl std::error::Error for ParseFromPathError {}

impl From<std::io::Error> for ParseFromPathError {
    fn from(e: std::io::Error) -> Self {
        ParseFromPathError::IoError(e)
    }
}

impl From<ParseError> for ParseFromPathError {
    fn from(e: ParseError) -> Self {
        ParseFromPathError::ParseError(e)
    }
}

pub fn parse_from_path(
    path: &PathBuf,
    action: &str,
    verbose: bool,
) -> Result<ModuleAST, ParseFromPathError> {
    let file_set = if path.is_file() {
        println!("{} file: {}", action, path.display());
        FileSet::from_file(path.clone())
    } else if path.is_dir() {
        println!("{} project in folder: {}", action, path.display());
        FileSet::from_directory(path)?
    } else {
        return Err(ParseFromPathError::InvalidPath(path.clone()));
    };

    if verbose {
        println!("FileSet contains {} file(s):", file_set.files.len());
        for file in &file_set.files {
            println!("  {}", file.display());
        }
    }

    let module_ast = parse_module(&file_set)?;

    if verbose {
        println!("{:#?}", module_ast);
    }

    Ok(module_ast)
}
