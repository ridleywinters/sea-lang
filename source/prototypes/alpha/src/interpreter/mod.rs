mod builtins;
mod interpreter;
mod value;

pub use interpreter::{interpret, InterpreterOutput};
pub use value::{RuntimeError, Value};
