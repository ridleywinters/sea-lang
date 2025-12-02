use std::path::PathBuf;

use super::parse_from_path::{ParseFromPathError, parse_from_path};
use crate::interpreter::{InterpreterOutput, RuntimeError, interpret};

#[derive(Debug)]
pub enum RunError {
    ParseFromPathError(ParseFromPathError),
    RuntimeError(RuntimeError),
}

impl std::fmt::Display for RunError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunError::ParseFromPathError(e) => write!(f, "{}", e),
            RunError::RuntimeError(e) => write!(f, "{}", e),
        }
    }
}

impl std::error::Error for RunError {}

impl From<ParseFromPathError> for RunError {
    fn from(e: ParseFromPathError) -> Self {
        RunError::ParseFromPathError(e)
    }
}

impl From<RuntimeError> for RunError {
    fn from(e: RuntimeError) -> Self {
        RunError::RuntimeError(e)
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
    let module_ast = parse_from_path(path, "Running", options.verbose)?;

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
