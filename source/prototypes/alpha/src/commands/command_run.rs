use std::fs;
use std::path::PathBuf;

use crate::interpreter::{InterpreterOutput, RuntimeError, interpret};
use crate::parser::{ParseError, parse_module};

#[derive(Debug)]
pub enum RunError {
    InvalidPath(PathBuf),
    IoError(std::io::Error),
    ParseError(ParseError),
    RuntimeError(RuntimeError),
}

impl std::fmt::Display for RunError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunError::InvalidPath(path) => {
                write!(
                    f,
                    "Path '{}' does not exist or is not a valid file or folder",
                    path.display()
                )
            }
            RunError::IoError(e) => write!(f, "IO error: {}", e),
            RunError::ParseError(e) => write!(f, "{}", e),
            RunError::RuntimeError(e) => write!(f, "{}", e),
        }
    }
}

impl std::error::Error for RunError {}

impl From<std::io::Error> for RunError {
    fn from(e: std::io::Error) -> Self {
        RunError::IoError(e)
    }
}

impl From<ParseError> for RunError {
    fn from(e: ParseError) -> Self {
        RunError::ParseError(e)
    }
}

impl From<RuntimeError> for RunError {
    fn from(e: RuntimeError) -> Self {
        RunError::RuntimeError(e)
    }
}

pub struct FileSet {
    pub files: Vec<PathBuf>,
}

impl FileSet {
    pub fn new() -> Self {
        FileSet { files: Vec::new() }
    }

    pub fn from_file(path: PathBuf) -> Self {
        FileSet { files: vec![path] }
    }

    pub fn from_directory(path: &PathBuf) -> Result<Self, RunError> {
        let mut file_set = FileSet::new();
        file_set.collect_sea_files(path)?;
        Ok(file_set)
    }

    fn collect_sea_files(&mut self, dir: &PathBuf) -> Result<(), RunError> {
        let entries = fs::read_dir(dir)?;

        for entry in entries {
            let entry = entry?;
            let path = entry.path();

            if !path.is_file() {
                continue;
            }
            let Some(ext) = path.extension() else {
                continue;
            };
            if ext != "sea" {
                continue;
            }
            self.files.push(path);
        }

        Ok(())
    }
}

pub struct RunOptions {
    pub verbose: bool,
    pub capture_output: bool,
}

impl Default for RunOptions {
    fn default() -> Self {
        RunOptions {
            verbose: false,
            capture_output: false,
        }
    }
}

pub struct RunResult {
    pub output: InterpreterOutput,
}

pub fn command_run(path: &PathBuf, options: RunOptions) -> Result<RunResult, RunError> {
    let file_set = if path.is_file() {
        println!("Running file: {}", path.display());
        FileSet::from_file(path.clone())
    } else if path.is_dir() {
        println!("Running project in folder: {}", path.display());
        FileSet::from_directory(path)?
    } else {
        return Err(RunError::InvalidPath(path.clone()));
    };

    if options.verbose {
        println!("FileSet contains {} file(s):", file_set.files.len());
        for file in &file_set.files {
            println!("  {}", file.display());
        }
    }

    let module_ast = parse_module(&file_set)?;

    if options.verbose {
        println!("{:#?}", module_ast);
    }

    let interpreter_output = interpret(&module_ast, options.capture_output)?;

    Ok(RunResult {
        output: interpreter_output,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run_example(name: &str) -> String {
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("data/examples")
            .join(name);

        let options = RunOptions {
            capture_output: true,
            ..Default::default()
        };

        let result = command_run(&path, options).unwrap_or_else(|err| {
            eprintln!("Failed to run example '{}':", name);
            eprintln!("{}", err);
            panic!("Example execution failed")
        });
        result
            .output
            .captured_output
            .expect("Output should be captured")
    }

    #[test]
    fn test_examples() {
        let cases = [
            ("001-hello-world", "Hello, world!\n"),
            ("002-arithmetic", "Hello, world!\n10\n-106\n"),
            ("003-arithmetic-f32", "10\n"),
            ("004-control-flow", ""),
        ];

        for (name, expected) in cases {
            let output = run_example(name);
            if output != expected {
                println!("Expected output:\n{}", expected);
                println!("Actual output:\n{}", output);
            }
            assert_eq!(output, expected, "Failed for example: {}", name);
        }
    }
}
