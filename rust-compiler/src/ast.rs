// SAIL AST. Captures enough structure to reproduce std/std.solc.

#[derive(Debug, Clone)]
pub struct CompUnit {
    pub imports: Vec<Import>,
    pub decls: Vec<TopDecl>,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub path: Vec<String>,           // segments of the module path
    pub external: bool,              // true if @package.
    pub from_lib: bool,              // true if lib.
    pub kind: ImportKind,
}

#[derive(Debug, Clone)]
pub enum ImportKind {
    Whole(Option<String>),                            // import m; or import m as Alias;
    Select(Vec<ImportItem>, Vec<String>),             // import m.{...}; with optional hiding
}

#[derive(Debug, Clone)]
pub enum ImportItem {
    Wildcard,
    Name(String),
    NameStar(String),                                 // X(*) - constructors
    NameList(String, Vec<String>),                    // X(A, B)
}

#[derive(Debug, Clone)]
pub enum TopDecl {
    Contract(Contract),
    Func(Function),
    Class(ClassDef),
    Instance(InstDef),
    Data(DataDef),
    TypeSyn(TypeSynonym),
    Export(ExportDecl),
    Pragma(Pragma),
}

#[derive(Debug, Clone)]
pub struct Pragma {
    pub kind: String,           // e.g. "no-coverage-condition"
    pub targets: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ExportDecl {
    pub items: Vec<ExportItem>,
    // re-export "from" not used in std/std.solc, but reserved here
    pub from: Option<Vec<String>>,
}

#[derive(Debug, Clone)]
pub enum ExportItem {
    Wildcard,
    Name(String),
    NameStar(String),
    NameList(String, Vec<String>),
    ModuleAll(Vec<String>),
}

#[derive(Debug, Clone)]
pub struct DataDef {
    pub name: String,
    pub params: Vec<String>,
    pub ctors: Vec<DataCtor>,
}

#[derive(Debug, Clone)]
pub struct DataCtor {
    pub name: String,
    pub fields: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct TypeSynonym {
    pub name: String,
    pub params: Vec<String>,
    pub rhs: Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    Named(Vec<String>, Vec<Type>),           // qualified name + args (e.g. Option.Foo(a,b))
    Fun(Vec<Type>, Box<Type>),               // (T,...) -> T
    Tuple(Vec<Type>),                        // () or (T,...)
    Proxy(Box<Type>),                        // @T
    Var(String),                             // unbound name; resolved later
}

#[derive(Debug, Clone)]
pub struct Constraint {
    pub ty: Type,
    pub class_name: Vec<String>,
    pub args: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct SigPrefix {
    pub vars: Vec<String>,
    pub constraints: Vec<Constraint>,
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub prefix: Option<SigPrefix>,
    pub name: String,
    pub params: Vec<Param>,
    pub ret: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub sig: Signature,
    pub body: Option<Body>,                  // None for class method signatures
    pub short: Option<Expr>,                 // for `fn f() = expr` short form
}

#[derive(Debug, Clone)]
pub struct Body {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let(String, Option<Type>, Option<Expr>),
    Assign(Expr, Expr),
    AddAssign(Expr, Expr),
    SubAssign(Expr, Expr),
    Return(Option<Expr>),
    Expr(Expr),
    Match(Vec<Expr>, Vec<Equation>),
    If(Expr, Body, Option<Body>),
    Asm(AsmBlock),
}

#[derive(Debug, Clone)]
pub struct Equation {
    pub patterns: Vec<Pattern>,
    pub body: Body,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Var(String),
    Int(String),
    Str(String),
    Bool(bool),
    Tuple(Vec<Pattern>),
    Constr(Vec<String>, Vec<Pattern>),       // qualified ctor with args
    DotConstr(String, Vec<Pattern>),         // contextual .Name(args)
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(String),
    Str(String),
    Bool(bool),
    Var(Vec<String>),                        // qualified name (e.g. Module.fn)
    Tuple(Vec<Expr>),                        // (), (e), (e1, e2, ...)
    Call(Box<Expr>, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Annot(Box<Expr>, Type),                  // expr : Type
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    DotCtor(String, Vec<Expr>),              // .Name or .Name(args)
    ProxyTy(Type),                           // `@T` form? Unused in std but reserved
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Lt, Gt, Le, Ge, Eq, Ne, And, Or, Add, Sub, Mul, Div, Mod,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnOp { Not, Neg }

#[derive(Debug, Clone)]
pub struct ClassDef {
    pub prefix: Option<SigPrefix>,
    pub self_var: String,
    pub class_name: String,
    pub aux_args: Vec<String>,
    pub methods: Vec<Signature>,
}

#[derive(Debug, Clone)]
pub struct InstDef {
    pub prefix: Option<SigPrefix>,
    pub is_default: bool,
    pub head_ty: Type,
    pub class_name: Vec<String>,
    pub class_args: Vec<Type>,
    pub methods: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Contract {
    pub name: String,
    pub params: Vec<String>,
    pub decls: Vec<ContractDecl>,
}

#[derive(Debug, Clone)]
pub enum ContractDecl {
    Field(String, Type, Option<Expr>),
    Data(DataDef),
    Func(Function),
    Constructor(Vec<Param>, Body),
}

// --- Inline assembly (Yul) ----------------------------------------------------

#[derive(Debug, Clone)]
pub struct AsmBlock {
    pub stmts: Vec<YulStmt>,
}

#[derive(Debug, Clone)]
pub enum YulStmt {
    Let(Vec<String>, Option<YulExpr>),
    Assign(Vec<YulExprLhs>, YulExpr),
    ExprStmt(YulExpr),
    If(YulExpr, Vec<YulStmt>),
    Switch(YulExpr, Vec<YulCase>, Option<Vec<YulStmt>>),
    For(Vec<YulStmt>, YulExpr, Vec<YulStmt>, Vec<YulStmt>),
    Break,
    Continue,
    Leave,
    FunctionDef(String, Vec<String>, Vec<String>, Vec<YulStmt>),
    Block(Vec<YulStmt>),
}

#[derive(Debug, Clone)]
pub struct YulExprLhs(pub String);

#[derive(Debug, Clone)]
pub struct YulCase {
    pub value: YulLit,
    pub body: Vec<YulStmt>,
}

#[derive(Debug, Clone)]
pub enum YulExpr {
    Lit(YulLit),
    Var(String),
    Call(String, Vec<YulExpr>),
}

#[derive(Debug, Clone)]
pub enum YulLit {
    Int(String),
    Str(String),
}
