mod builtins;
mod interpreter;

pub use interpreter::{InterpreterOutput, RuntimeError, interpret};
use interpreter::{OutputWriter, Value};
