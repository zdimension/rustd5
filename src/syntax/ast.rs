extern crate derive_more;
extern crate peg;

use std::fmt;
use std::fmt::Display;

use derive_more::Constructor;
use itertools::Itertools;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct VarDeclStmt(pub Vec<VarDecl>);

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum DeclOrExpr
{
    Decl(Box<VarDeclStmt>),
    Expr(Box<Expr>)
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Stmt
{
    Empty,
    Discard(Box<Expr>),
    VarDeclList(VarDeclStmt),
    TypeDeclList(Vec<TypeDecl>),
    TupleAssign { names: Vec<String>, values: Vec<Expr> },
    ConstDecl { name: String, value: Box<Expr> },
    Print(Box<Expr>),
    Return(Option<Box<Expr>>),
    Assert(Box<Expr>),
    Break(Option<Box<Expr>>),
    Continue,
    While { condition: Box<Expr>, code: Box<Stmt> },
    Loop { code: Box<Stmt> },
    For { init: Option<DeclOrExpr>, condition: Option<Box<Expr>>, step: Option<Box<Expr>>, code: Box<Stmt> },
    ForEach { variable: String, iterable: Box<Expr>, code: Box<Stmt> },
    If { condition: Box<Expr>, code: Box<Stmt>, code_else: Option<Box<Stmt>> },
    DoWhile { code: Box<Stmt>, condition: Box<Expr> },
    Impl { type_name: String, methods: Vec<FnDecl> },
    FnDecl(Box<FnDecl>),
    Block(Vec<Stmt>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
#[derive(Constructor)]
pub struct FuncSignature
{
    pub name: String,
    pub args: Vec<TypedVar>,
    pub ret: TypeSpec,
    pub generic_info: Option<GenericInfo>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct GenericInfo
{
    pub type_params: Vec<String>,
    pub constraints: Vec<Expr>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum VarDeclType
{
    Scalar(Option<Box<TypeSpec>>, Option<Box<Expr>>),
    Array(Option<Box<Expr>>, Option<String>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct VarDecl
{
    pub name: String,
    pub decl_type: VarDeclType,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypeDecl
{
    pub name: String,
    pub type_spec: TypeSpec,
    pub generic_info: Option<GenericInfo>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct FnDecl(pub FuncSignature, pub Stmt);

impl Display for FnDecl
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}\n{}", self.0, self.1)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TypeSpec
{
    Named(String),
    SelfType,
    GenericInstanciation(String, Vec<TypeSpec>),
    Pointer(Box<TypeSpec>),
    Global(Box<TypeSpec>),
    Array(Box<TypeSpec>, Box<Expr>),
    TypeOf(Box<Expr>),
    ScalarOf(Box<Expr>),
    Struct(Vec<TypedVar>),
    Interface(Vec<FuncSignature>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct MatchArm(pub Box<Pattern>, pub Box<Expr>);

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Pattern
{
    Value(Box<Expr>),
    Range(Box<Expr>, Box<Expr>),
    RangeInclusive(Box<Expr>, Box<Expr>),
    Or(Vec<Pattern>),
    Wildcard,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum BinOpArith
{
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Shl,
    Shr,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum BinOpRel
{
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum BinOp
{
    Arith(BinOpArith),
    Rel(BinOpRel),
    Assign(Option<BinOpArith>),
    Member,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum UnOp
{
    Neg,
    Not,
    Deref,
    PreInc,
    PreDec,
    PostInc,
    PostDec,
    Ref,
}

pub enum UnOpPosition
{
    Prefix,
    Postfix,
}

impl UnOp
{
    pub(crate) fn get_position(&self) -> UnOpPosition
    {
        match self
        {
            UnOp::PostInc | UnOp::PostDec => UnOpPosition::Postfix,
            _ => UnOpPosition::Prefix
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StructLiteralNamedField(pub String, pub Box<Expr>);

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expr
{
    Number(i64, Option<u64>),
    SizeOf(Box<TypeSpec>),
    BitsOf(Box<TypeSpec>),
    New(Box<TypeSpec>),
    Ident(String),
    Compound(Vec<Stmt>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Stmt>),
    Match(Box<Expr>, Vec<MatchArm>),
    StructLiteral(Box<TypeSpec>, Vec<Expr>),
    StructLiteralNamed(Box<TypeSpec>, Vec<StructLiteralNamedField>),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    Is(Box<Expr>, Box<Pattern>),
    UnOp(UnOp, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Type(Box<TypeSpec>),
    Pattern(Box<Pattern>)
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypedVar(pub String, pub TypeSpec);

