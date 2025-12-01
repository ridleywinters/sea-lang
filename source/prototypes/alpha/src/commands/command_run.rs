use std::fs;
use std::path::PathBuf;

use crate::interpreter::{InterpreterOutput, interpret};
use crate::parser::parse_module;

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

    pub fn from_directory(path: &PathBuf) -> Result<Self, String> {
        let mut file_set = FileSet::new();
        file_set.collect_sea_files(path)?;
        Ok(file_set)
    }

    fn collect_sea_files(&mut self, dir: &PathBuf) -> Result<(), String> {
        let entries = fs::read_dir(dir)
            .map_err(|e| format!("Failed to read directory '{}': {}", dir.display(), e))?;

        for entry in entries {
            let entry = entry.map_err(|e| format!("Failed to read directory entry: {}", e))?;
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

pub fn command_run(path: &PathBuf, options: RunOptions) -> Result<RunResult, String> {
    let file_set = if path.is_file() {
        println!("Running file: {}", path.display());
        FileSet::from_file(path.clone())
    } else if path.is_dir() {
        println!("Running project in folder: {}", path.display());
        FileSet::from_directory(path)?
    } else {
        return Err(format!(
            "Path '{}' does not exist or is not a valid file or folder",
            path.display()
        ));
    };

    if options.verbose {
        println!("FileSet contains {} file(s):", file_set.files.len());
        for file in &file_set.files {
            println!("  {}", file.display());
        }
    }

    let module_ast = parse_module(&file_set).map_err(|e| e.to_string())?;

    if options.verbose {
        println!("{:#?}", module_ast);
    }

    let interpreter_output =
        interpret(&module_ast, options.capture_output).map_err(|e| e.to_string())?;

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

        let result = command_run(&path, options).expect(&format!("Failed to run {} example", name));
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
        ];

        for (name, expected) in cases {
            let output = run_example(name);
            assert_eq!(output, expected, "Failed for example: {}", name);
        }
    }
}
