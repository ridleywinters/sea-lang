use std::path::PathBuf;

use super::parse_from_path::{ParseFromPathError, parse_from_path};
use crate::backend_c::generate_c;
use crate::backend_rust::generate_rust;
use crate::backend_typescript::generate_typescript;

#[derive(Debug, Clone)]
pub enum Backend {
    Rust,
    Typescript,
    C,
}

#[derive(Debug)]
pub enum BuildError {
    ParseFromPathError(ParseFromPathError),
}

impl std::fmt::Display for BuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuildError::ParseFromPathError(e) => write!(f, "{}", e),
        }
    }
}

impl std::error::Error for BuildError {}

impl From<ParseFromPathError> for BuildError {
    fn from(e: ParseFromPathError) -> Self {
        BuildError::ParseFromPathError(e)
    }
}

pub struct BuildOptions {
    pub backend: Backend,
    pub verbose: bool,
}

pub struct BuildResult {
    pub format: String,
    pub output: String,
}

pub fn command_build(path: &PathBuf, options: BuildOptions) -> Result<BuildResult, BuildError> {
    let module_ast = parse_from_path(path, "Building", options.verbose)?;

    let (format, output) = match options.backend {
        Backend::Typescript => ("typescript".to_string(), generate_typescript(&module_ast)),
        Backend::Rust => ("rust".to_string(), generate_rust(&module_ast)),
        Backend::C => ("c".to_string(), generate_c(&module_ast)),
    };

    Ok(BuildResult { format, output })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::process::Command;

    fn build_and_run_example(name: &str) -> String {
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("data/examples")
            .join(name);

        let options = BuildOptions {
            backend: Backend::Typescript,
            verbose: false,
        };

        let result = command_build(&path, options).unwrap_or_else(|err| {
            eprintln!("Failed to build example '{}':", name);
            eprintln!("{}", err);
            panic!("Example build failed")
        });

        let output = Command::new("deno")
            .arg("eval")
            .arg(&result.output)
            .output()
            .expect("Failed to execute deno");

        if !output.status.success() {
            eprintln!("Deno execution failed for '{}':", name);
            eprintln!("Generated TypeScript:\n{}", result.output);
            eprintln!("stderr: {}", String::from_utf8_lossy(&output.stderr));
            panic!("Deno execution failed");
        }

        String::from_utf8_lossy(&output.stdout).to_string()
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
            let output = build_and_run_example(name);
            if output != expected {
                println!("Expected output:\n{}", expected);
                println!("Actual output:\n{}", output);
            }
            assert_eq!(output, expected, "Failed for example: {}", name);
        }
    }

    #[test]
    fn test_typescript_type_checking() {
        use std::io::Write;

        let examples = [
            "001-hello-world",
            "002-arithmetic",
            "003-arithmetic-f32",
            "004-control-flow",
        ];

        for name in examples {
            let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("data/examples")
                .join(name);

            let options = BuildOptions {
                backend: Backend::Typescript,
                verbose: false,
            };

            let result = command_build(&path, options).unwrap_or_else(|err| {
                eprintln!("Failed to build example '{}':", name);
                eprintln!("{}", err);
                panic!("Example build failed")
            });

            let temp_dir = std::env::temp_dir();
            let temp_file = temp_dir.join(format!("sea_test_{}.ts", name));
            let mut file = std::fs::File::create(&temp_file).expect("Failed to create temp file");
            file.write_all(result.output.as_bytes())
                .expect("Failed to write temp file");

            let output = Command::new("deno")
                .arg("check")
                .arg(&temp_file)
                .output()
                .expect("Failed to execute deno check");

            std::fs::remove_file(&temp_file).ok();

            if !output.status.success() {
                eprintln!("TypeScript type checking failed for '{}':", name);
                eprintln!("Generated TypeScript:\n{}", result.output);
                eprintln!("stderr: {}", String::from_utf8_lossy(&output.stderr));
                panic!("TypeScript type checking failed for '{}'", name);
            }
        }
    }
}
