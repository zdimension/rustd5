extern crate derive_more;
extern crate peg;

use std::borrow::{Borrow};
use std::cell::{RefCell, RefMut};
use std::fmt::{Debug, Display};
use std::ops::{Deref, Range};
use std::rc::Rc;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use derive_more::Constructor;
use crate::analysis::analyze_expr;
use crate::analysis::typing::{Type, TypeKind};
use crate::StackFrame;

#[derive(Debug, Clone)]
pub struct NodeReal<T> {
    pub node: T,
    pub meta: NodeMetadata,
}

pub struct Erc<T>
{
    expr: Rc<RefCell<NodeReal<T>>>,
}

#[derive(Debug, Clone)]
pub struct NodeMetadata
{
    pub start: usize,
    pub end: usize,
    pub type_: Option<Rc<Type>>,
}

impl Erc<Expr>
{
    pub fn get_type(&self, frame: &Rc<StackFrame>) -> Result<Rc<Type>, Diagnostic<()>> {
        if let Some(t) = self.meta().type_ {
            Ok(t)
        } else {
            analyze_expr(&self, &frame)?;
            self.meta().type_.ok_or_else(|| {
                Diagnostic::error()
                    .with_message("unable to analyze type")
                    .with_labels(vec![
                        Label::primary((), self.range())
                    ])
            })
        }
    }

    pub fn expect_non_void(&self, frame: &Rc<StackFrame>) -> Result<(), Diagnostic<()>> {
        let t = self.get_type(frame)?;
        if t.kind == TypeKind::Void {
            Err(Diagnostic::error()
                .with_message("void type not allowed")
                .with_labels(vec![
                    Label::primary((), self.range())
                ])
            )
        } else {
            Ok(())
        }
    }

}

impl<T: Clone> Erc<T>
{
    pub fn new(expr: T, start: usize, end: usize) -> Self
    {
        Self {
            expr: Rc::new(RefCell::new(NodeReal {
                node: expr,
                meta: NodeMetadata { start, end, type_: None },
            }))
        }
    }

    pub fn meta(&self) -> NodeMetadata
    {
        self.expr.deref().borrow().meta.clone()
    }

    pub fn code(&self) -> T
    {
        self.expr.deref().borrow().node.clone()
    }

    pub fn set_type(&self, type_: Type)
    {
        self.expr.deref().borrow_mut().meta.type_ = Some(Rc::new(type_));
    }

    pub fn range(&self) -> Range<usize>
    {
        let meta = self.meta();
        let val = meta.borrow();
        val.start..val.end
    }

    pub fn borrow_mut(&self) -> RefMut<'_, NodeReal<T>>
    {
        self.expr.deref().borrow_mut()
    }
}

impl<T> Clone for Erc<T>
    where
        T: Clone
{
    fn clone(&self) -> Self
    {
        Self { expr: Rc::clone(&self.expr) }
    }
}

impl<T> AsRef<RefCell<NodeReal<T>>> for Erc<T>
{
    fn as_ref(&self) -> &RefCell<NodeReal<T>>
    {
        &self.expr.as_ref()
    }
}

impl<T> Display for Erc<T>
    where
        T: Display
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        std::fmt::Display::fmt(&self.expr.deref().borrow().node, f)
    }
}

impl<T> Deref for Erc<T>
{
    type Target = RefCell<NodeReal<T>>;

    fn deref(&self) -> &RefCell<NodeReal<T>>
    {
        self.expr.deref()
    }
}

impl<T> Debug for Erc<T>
    where
        T: Debug
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        std::fmt::Debug::fmt(&self.expr, f)
    }
}

impl<T: PartialEq> PartialEq for Erc<T>
{
    fn eq(&self, other: &Self) -> bool
    {
        Rc::ptr_eq(&self.expr, &other.expr)
    }
}

impl<T: Eq> Eq for Erc<T>
{}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct VarDeclStmt(pub Vec<Erc<VarDecl>>);

/// Variable declaration or expression
///
/// Used in the initialization part of the C-style for loop
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum DeclOrExpr
{
    Decl(Erc<VarDeclStmt>),
    Expr(Erc<Expr>),
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
    Discard(Erc<Expr>),
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
    TupleAssign { names: Vec<String>, values: Vec<Erc<Expr>> },
    /// Constant declaration
    /// ```
    /// const size = 123;
    /// ```
    ConstDecl { name: String, value: Erc<Expr> },
    /// Print statement
    /// ```
    /// print foo();
    /// ```
    Print(Erc<Expr>),
    /// Return statement
    /// ```
    /// return;
    /// return foo();
    /// ```
    Return(Option<Erc<Expr>>),
    /// Assert statement
    ///
    /// The expression must be a compile-time boolean constant.
    /// ```
    /// assert sizeof(type) == 4;
    /// ```
    Assert(Erc<Expr>),
    /// Loop break statement
    /// ```
    /// break;
    /// break foo();
    /// ```
    Break(Option<Erc<Expr>>),
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
    While { condition: Erc<Expr>, code: Erc<Stmt> },
    /// Simple loop
    /// ```
    /// loop
    /// {
    ///    foo();
    /// }
    /// ```
    Loop { code: Erc<Stmt> },
    /// C-style for loop
    /// ```
    /// for (var x = 0; x < 10; x++)
    /// {
    ///     foo(x);
    /// }
    /// ```
    For { init: Option<DeclOrExpr>, condition: Option<Erc<Expr>>, step: Option<Erc<Expr>>, code: Erc<Stmt> },
    /// Iterable foreach loop
    /// ```
    /// for (var x in 0..10)
    /// {
    ///     foo(x);
    /// }
    /// ```
    ForEach { variable: String, iterable: Erc<Expr>, code: Erc<Stmt> },
    /// If statement
    /// ```
    /// if (x == 5)
    /// {
    ///     foo();
    /// }
    /// ```
    If { condition: Erc<Expr>, code: Erc<Stmt>, code_else: Option<Erc<Stmt>> },
    /// Do-while loop
    /// ```
    /// do
    /// {
    ///    foo();
    /// } while (x == 5);
    DoWhile { code: Erc<Stmt>, condition: Erc<Expr> },
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
    FnDecl(Erc<FnDecl>),
    /// Code block
    /// ```
    /// {
    ///     foo();
    ///     bar();
    /// }
    /// ```
    Block(Vec<Erc<Stmt>>),
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
    Scalar(Option<Erc<TypeSpec>>, Option<Erc<Expr>>),
    /// Array declaration
    ///
    /// ```
    /// var x[10] = "ABC";
    /// ```
    Array(Option<Erc<Expr>>, Option<String>),
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
    Pointer(Erc<TypeSpec>),
    /// Global pointer type
    /// ```
    /// type u32ptrglob = u32* global;
    /// ```
    Global(Erc<TypeSpec>),
    /// Array type
    /// ```
    /// type u32arr = u32[10];
    /// ```
    Array(Erc<TypeSpec>, Erc<Expr>),
    /// Typeof
    /// ```
    /// var x = 123;
    /// type x_type = typeof(x);
    /// ```
    TypeOf(Erc<Expr>),
    /// Create scalar type with specified size
    /// ```
    /// type dyn_number = scalarof(2 * 16);
    /// // dyn_number is u32
    /// ```
    ScalarOf(Erc<Expr>),
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
pub struct MatchArm(pub Erc<Pattern>, pub Erc<Expr>);

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Pattern
{
    Value(Erc<Expr>),
    Range(Erc<Expr>, Erc<Expr>),
    RangeInclusive(Erc<Expr>, Erc<Expr>),
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
pub struct StructLiteralNamedField(pub String, pub Erc<Expr>);

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
    Number(usize, Option<u8>),
    /// Size (in cells) of type
    /// ```
    /// var x = sizeof(u32);
    /// ```
    SizeOf(Erc<TypeSpec>),
    /// Size (in bits) of type
    /// ```
    /// var x = bitsof(u32);
    /// ```
    BitsOf(Erc<TypeSpec>),
    /// Allocation expression
    /// ```
    /// var x = new(point);
    /// ```
    New(Erc<TypeSpec>),
    /// Identifier
    /// ```
    /// foo
    /// ```
    Ident(String),
    /// Compound expression
    /// ```
    /// var x = { var v = foo(); v + 1 };
    /// ```
    Compound(Vec<Erc<Stmt>>, Erc<Expr>),
    /// Conditional expression
    /// ```
    /// var x = if (foo()) { 1 } else { 2 };
    /// ```
    If(Erc<Expr>, Erc<Expr>, Erc<Expr>),
    /// Loop expression
    /// ```
    /// var x = loop { var y = foo(); if (y > 10) { break y; } };
    /// ```
    Loop(Erc<Stmt>),
    /// Match expression
    /// ```
    /// var res = match (p)
    /// {
    ///     point { x: 0..5, y: 0..5 } => 1,
    ///     point { x: 5..10, y: 5..10 } => 2,
    ///     _ => 3
    /// };
    /// ```
    Match(Erc<Expr>, Vec<MatchArm>),
    /// Structure literal
    /// ```
    /// var x = point { 5, 6 };
    /// ```
    StructLiteral(Erc<TypeSpec>, Vec<Erc<Expr>>),
    /// Structure literal with explicitely named fields
    /// ```
    /// var x = point { x: 5, y: 6 };
    /// ```
    StructLiteralNamed(Erc<TypeSpec>, Vec<StructLiteralNamedField>),
    /// Binary operation
    /// ```
    /// var x = foo() + bar();
    /// ```
    BinOp(Erc<Expr>, BinOp, Erc<Expr>),
    /// Inline pattern matching expression
    /// ```
    /// var x = foo() is point { x: 0..5 };
    /// ```
    Is(Erc<Expr>, Erc<Pattern>),
    /// Unary operation
    /// ```
    /// var x = -foo();
    /// ```
    UnOp(UnOp, Erc<Expr>),
    /// Function call expression
    /// ```
    /// var x = foo(1, 2, 3);
    /// var y = bar(x);
    /// var z = x |> bar; // equivalent to previous line
    /// ```
    Call(Erc<Expr>, Vec<Erc<Expr>>),
    /// Internal expression type, used for storing type specs in expression contexts
    Type(Erc<TypeSpec>),
    /// Internal expression type, used for storing patterns in expression contexts
    Pattern(Erc<Pattern>),
}

/// Typed variable declaration
///
/// Example:
/// ```
/// foo: u32
/// ```
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypedVar(pub String, pub TypeSpec);

