use std::path::PathBuf;

#[derive(Debug)]
pub struct ModuleAST {
    pub files: Vec<FileAST>,
    pub functions: Vec<FunctionDef>,
}

impl ModuleAST {
    pub fn new() -> Self {
        ModuleAST {
            files: Vec::new(),
            functions: Vec::new(),
        }
    }

    pub fn add_function(&mut self, func: FunctionDef) {
        self.functions.push(func);
    }
}

#[derive(Debug)]
pub struct FileAST {
    pub path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub location: SourceLocation,
    pub params: Vec<Parameter>,
    pub return_type: Option<TypeExpr>,
    pub body: Block,
    pub is_exported: bool,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub type_expr: TypeExpr,
}

#[derive(Debug, Clone)]
pub enum TypeExpr {
    Named(String),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    VariableDecl {
        name: String,
        type_expr: TypeExpr,
        initializer: Expr,
    },
    Expression(Expr),
    Return(Option<Expr>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Identifier(String),
    Call {
        function: Box<Expr>,
        arguments: Vec<Expr>,
    },
    BinaryOp {
        left: Box<Expr>,
        op: BinaryOperator,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub file: PathBuf,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub location: Option<SourceLocation>,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.location {
            Some(loc) => write!(
                f,
                "{}:{}:{}: {}",
                loc.file.display(),
                loc.line,
                loc.column,
                self.message
            ),
            None => write!(f, "{}", self.message),
        }
    }
}
