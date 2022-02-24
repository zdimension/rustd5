extern crate derive_more;
extern crate peg;

use derive_more::Constructor;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct VarDeclStmt(pub Vec<VarDecl>);

/// Variable declaration or expression
///
/// Used in the initialization part of the C-style for loop
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum DeclOrExpr
{
    Decl(Box<VarDeclStmt>),
    Expr(Box<Expr>)
}

/// Statement
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Stmt
{
    /// Empty statement
    /// ```
    /// ;
    /// ```
    Empty,
    /// Expression statement
    /// ```
    /// a = 5;
    /// b++;
    /// ```
    Discard(Box<Expr>),
    /// Variable declaration list
    /// ```
    /// var x = 5, y = 6;
    /// ```
    VarDeclList(VarDeclStmt),
    /// Type declaration list
    /// ```
    /// type point = struct { ... }, point_ptr = point*;
    /// ```
    TypeDeclList(Vec<TypeDecl>),
    /// Tuple assignment
    /// ```
    /// (x, y) = (5, 6);
    /// ```
    TupleAssign { names: Vec<String>, values: Vec<Expr> },
    /// Constant declaration
    /// ```
    /// const size = 123;
    /// ```
    ConstDecl { name: String, value: Box<Expr> },
    /// Print statement
    /// ```
    /// print foo();
    /// ```
    Print(Box<Expr>),
    /// Return statement
    /// ```
    /// return;
    /// return foo();
    /// ```
    Return(Option<Box<Expr>>),
    /// Assert statement
    ///
    /// The expression must be a compile-time boolean constant.
    /// ```
    /// assert sizeof(type) == 4;
    /// ```
    Assert(Box<Expr>),
    /// Loop break statement
    /// ```
    /// break;
    /// break foo();
    /// ```
    Break(Option<Box<Expr>>),
    /// Loop continue statement
    /// ```
    /// continue;
    /// ```
    Continue,
    /// While loop
    /// ```
    /// while (condition)
    /// {
    ///     foo();
    /// }
    /// ```
    While { condition: Box<Expr>, code: Box<Stmt> },
    /// Simple loop
    /// ```
    /// loop
    /// {
    ///    foo();
    /// }
    /// ```
    Loop { code: Box<Stmt> },
    /// C-style for loop
    /// ```
    /// for (var x = 0; x < 10; x++)
    /// {
    ///     foo(x);
    /// }
    /// ```
    For { init: Option<DeclOrExpr>, condition: Option<Box<Expr>>, step: Option<Box<Expr>>, code: Box<Stmt> },
    /// Iterable foreach loop
    /// ```
    /// for (var x in 0..10)
    /// {
    ///     foo(x);
    /// }
    /// ```
    ForEach { variable: String, iterable: Box<Expr>, code: Box<Stmt> },
    /// If statement
    /// ```
    /// if (x == 5)
    /// {
    ///     foo();
    /// }
    /// ```
    If { condition: Box<Expr>, code: Box<Stmt>, code_else: Option<Box<Stmt>> },
    /// Do-while loop
    /// ```
    /// do
    /// {
    ///    foo();
    /// } while (x == 5);
    DoWhile { code: Box<Stmt>, condition: Box<Expr> },
    /// Implementation statement
    /// ```
    /// type Point = struct { x: u32; y: u32; };
    ///
    /// impl Point
    /// {
    ///     func norm_squared(p: self*): u32 { return p.x * p.x + p.y * p.y; }
    /// }
    Impl { type_name: String, methods: Vec<FnDecl> },
    /// Function declaration
    /// ```
    /// func foo(x: u32, y: u32): u32 { return x + y; }
    /// ```
    FnDecl(Box<FnDecl>),
    /// Code block
    /// ```
    /// {
    ///     foo();
    ///     bar();
    /// }
    /// ```
    Block(Vec<Stmt>),
}

/// Function signature
#[derive(Clone, PartialEq, Eq, Debug)]
#[derive(Constructor)]
pub struct FuncSignature
{
    pub name: String,
    pub args: Vec<TypedVar>,
    pub ret: TypeSpec,
    pub generic_info: Option<GenericInfo>,
}

/// Information about a generic code object (type or function)
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct GenericInfo
{
    /// Names of the type parameters
    pub type_params: Vec<String>,
    /// List of generic constraints, i.e. compile-time boolean conditions
    ///
    /// Example:
    /// ```
    /// type foo<T> where (sizeof(T) == 4) = ...;
    ///
    /// func bar<T>(param: T): T
    ///     where (sizeof(T) == 4)
    /// { ... }
    /// ```
    pub constraints: Vec<Expr>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum VarDeclType
{
    /// Scalar declaration
    ///
    /// ```
    /// var x: u32 = 123;
    /// ```
    Scalar(Option<Box<TypeSpec>>, Option<Box<Expr>>),
    /// Array declaration
    ///
    /// ```
    /// var x[10] = "ABC";
    /// ```
    Array(Option<Box<Expr>>, Option<String>),
}

/// Variable declaration
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct VarDecl
{
    pub name: String,
    pub decl_type: VarDeclType,
}

/// Type declaration
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypeDecl
{
    pub name: String,
    pub type_spec: TypeSpec,
    pub generic_info: Option<GenericInfo>,
}

/// Function declaration
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct FnDecl {
    pub signature: FuncSignature,
    pub code: Stmt,
}

/// Type specification
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TypeSpec
{
    /// Named type
    /// ```
    /// u32
    /// ```
    Named(String),
    /// Reference to self in `impl` block
    /// ```
    /// impl Point
    /// {
    ///     func distance(p: self*): u32
    /// ```
    SelfType,
    /// Generic instanciation
    /// ```
    /// type Point<T> = struct { x: T, y: T };
    /// type Pu32 = Point!<u32>;
    /// ```
    GenericInstanciation(String, Vec<TypeSpec>),
    /// Pointer type
    /// ```
    /// type u32ptr = u32*;
    /// ```
    Pointer(Box<TypeSpec>),
    /// Global pointer type
    /// ```
    /// type u32ptrglob = u32* global;
    /// ```
    Global(Box<TypeSpec>),
    /// Array type
    /// ```
    /// type u32arr = u32[10];
    /// ```
    Array(Box<TypeSpec>, Box<Expr>),
    /// Typeof
    /// ```
    /// var x = 123;
    /// type x_type = typeof(x);
    /// ```
    TypeOf(Box<Expr>),
    /// Create scalar type with specified size
    /// ```
    /// type dyn_number = scalarof(2 * 16);
    /// // dyn_number is u32
    /// ```
    ScalarOf(Box<Expr>),
    /// Structure type
    /// ```
    /// type Point = struct { x: u32, y: u32 };
    /// ```
    Struct(Vec<TypedVar>),
    /// Interface type
    ///
    /// TODO
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

/// Arithmetic / binary operators
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

/// Relational operators
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

/// Binary operators
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum BinOp
{
    Arith(BinOpArith),
    Rel(BinOpRel),
    Assign(Option<BinOpArith>),
    Member,
}

/// Unary operators
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

/// Unary operator position
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

/// Named field in a struct literal expression
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StructLiteralNamedField(pub String, pub Box<Expr>);

/// Base expression node
///
/// These nodes are used in an expression context and are assigned a type by the analyzer.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expr
{
    /// Number literal (optionally sized)
    /// ```
    /// var x = 123;
    /// var y = 456u16;
    /// ```
    Number(i64, Option<u64>),
    /// Size (in cells) of type
    /// ```
    /// var x = sizeof(u32);
    /// ```
    SizeOf(Box<TypeSpec>),
    /// Size (in bits) of type
    /// ```
    /// var x = bitsof(u32);
    /// ```
    BitsOf(Box<TypeSpec>),
    /// Allocation expression
    /// ```
    /// var x = new(point);
    /// ```
    New(Box<TypeSpec>),
    /// Identifier
    /// ```
    /// foo
    /// ```
    Ident(String),
    /// Compound expression
    /// ```
    /// var x = { var v = foo(); v + 1 };
    /// ```
    Compound(Vec<Stmt>, Box<Expr>),
    /// Conditional expression
    /// ```
    /// var x = if (foo()) { 1 } else { 2 };
    /// ```
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    /// Loop expression
    /// ```
    /// var x = loop { var y = foo(); if (y > 10) { break y; } };
    /// ```
    Loop(Box<Stmt>),
    /// Match expression
    /// ```
    /// var res = match (p)
    /// {
    ///     point { x: 0..5, y: 0..5 } => 1,
    ///     point { x: 5..10, y: 5..10 } => 2,
    ///     _ => 3
    /// };
    /// ```
    Match(Box<Expr>, Vec<MatchArm>),
    /// Structure literal
    /// ```
    /// var x = point { 5, 6 };
    /// ```
    StructLiteral(Box<TypeSpec>, Vec<Expr>),
    /// Structure literal with explicitely named fields
    /// ```
    /// var x = point { x: 5, y: 6 };
    /// ```
    StructLiteralNamed(Box<TypeSpec>, Vec<StructLiteralNamedField>),
    /// Binary operation
    /// ```
    /// var x = foo() + bar();
    /// ```
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    /// Inline pattern matching expression
    /// ```
    /// var x = foo() is point { x: 0..5 };
    /// ```
    Is(Box<Expr>, Box<Pattern>),
    /// Unary operation
    /// ```
    /// var x = -foo();
    /// ```
    UnOp(UnOp, Box<Expr>),
    /// Function call expression
    /// ```
    /// var x = foo(1, 2, 3);
    /// var y = bar(x);
    /// var z = x |> bar; // equivalent to previous line
    /// ```
    Call(Box<Expr>, Vec<Expr>),
    /// Internal expression type, used for storing type specs in expression contexts
    Type(Box<TypeSpec>),
    /// Internal expression type, used for storing patterns in expression contexts
    Pattern(Box<Pattern>)
}

/// Typed variable declaration
///
/// Example:
/// ```
/// foo: u32
/// ```
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypedVar(pub String, pub TypeSpec);

