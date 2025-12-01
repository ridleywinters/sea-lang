#![allow(unused)]

use clap::{Parser, Subcommand, ValueEnum};
use std::path::PathBuf;

mod commands;
mod interpreter;
mod parser;

use commands::{RunOptions, command_run};

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
    Build,
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
        Command::Build => {
            println!("Building project...");
            // TODO: Implement build
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
