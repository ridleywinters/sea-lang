#![allow(unused)]

use clap::{Parser, Subcommand, ValueEnum};
use std::fs;
use std::path::PathBuf;

mod backend_c;
mod backend_rust;
mod backend_typescript;
mod commands;
mod interpreter;
mod parser;

use commands::{Backend as BuildBackend, BuildOptions, RunOptions, command_build, command_run};

#[derive(Parser)]
#[command(name = "alpha")]
#[command(about = "sea language compiler", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command,

    #[arg(long, value_enum, global = true)]
    backend: Option<Backend>,

    #[arg(long, short, global = true)]
    verbose: bool,
}

#[derive(Clone, ValueEnum)]
enum Backend {
    Rust,
    Typescript,
    C,
    Interpret,
}

#[derive(Subcommand)]
enum Command {
    Init,
    Build {
        /// Path to a file or folder to build
        path: PathBuf,

        /// Output file path (if not specified, prints to stdout)
        #[arg(long, short)]
        output: Option<PathBuf>,
    },
    Run {
        /// Path to a file or folder to run
        path: PathBuf,
    },
    Check,
    Add,
}

fn main() {
    let cli = Cli::parse();

    if let Some(backend) = &cli.backend {
        println!(
            "Using backend: {}",
            match backend {
                Backend::Rust => "rust",
                Backend::Typescript => "typescript",
                Backend::C => "c",
                Backend::Interpret => "interpret",
            }
        );
    }

    match cli.command {
        Command::Init => {
            println!("Initializing project...");
            // TODO: Implement init
        }
        Command::Build { path, output } => {
            let backend = match &cli.backend {
                Some(Backend::Typescript) => BuildBackend::Typescript,
                Some(Backend::Rust) => BuildBackend::Rust,
                Some(Backend::C) => BuildBackend::C,
                Some(Backend::Interpret) => {
                    eprintln!("Error: 'interpret' is not a valid build backend");
                    std::process::exit(1);
                }
                None => {
                    eprintln!(
                        "Error: No backend specified. Use --backend=<backend> to specify a target."
                    );
                    std::process::exit(1);
                }
            };
            let options = BuildOptions {
                backend,
                verbose: cli.verbose,
            };
            match command_build(&path, options) {
                Ok(result) => {
                    if let Some(output_path) = output {
                        if let Err(e) = fs::write(&output_path, &result.output) {
                            eprintln!("Error writing output file: {}", e);
                            std::process::exit(1);
                        }
                        println!("Build output written to {}", output_path.display());
                    } else {
                        print!("{}", result.output);
                    }
                }
                Err(e) => {
                    if let Some(output_path) = output {
                        let _ = fs::remove_file(&output_path);
                    }
                    eprintln!("Error: {}", e);
                    std::process::exit(1);
                }
            }
        }
        Command::Run { path } => {
            let options = RunOptions {
                verbose: cli.verbose,
                ..Default::default()
            };
            if let Err(e) = command_run(&path, options) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
        }
        Command::Check => {
            println!("Checking project...");
            // TODO: Implement check
        }
        Command::Add => {
            println!("Adding dependency...");
            // TODO: Implement add
        }
    }
}
