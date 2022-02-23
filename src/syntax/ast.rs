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

impl Display for DeclOrExpr
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self
        {
            DeclOrExpr::Decl(decl) => write!(f, "{}", decl),
            DeclOrExpr::Expr(expr) => write!(f, "{}", expr)
        }
    }
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

fn to_string_if_some<T>(x: &Option<T>) -> String
where
    T: Display,
{
    match x {
        Some(x) => format!("{}", x),
        None => String::new(),
    }
}

impl Display for VarDeclStmt
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "var {}", self.0.iter().map(|decl| decl.to_string()).join(", "))
    }
}

impl Display for Stmt
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        fn gen_tab(indent: i32) -> String
        {
            let mut tab = String::new();
            for _ in 0..indent {
                tab.push('\t');
            }
            tab
        }

        fn stringify(s: &Stmt, indent: i32, expect_block: bool) -> String
        {
            let (indent, prefix) = if expect_block && matches!(s, Stmt::Block(_)) { (indent - 1, "") } else { (indent, "\t") };
            let tab = gen_tab(indent);
            let line = match s
            {
                Stmt::Empty => String::from(";"),
                Stmt::Discard(expr) => format!("{};", expr),
                Stmt::VarDeclList(decls) => format!("{};", decls),
                Stmt::TypeDeclList(decls) => format!("type {};", decls.iter().map(|decl| decl.to_string()).join(", ")),
                Stmt::TupleAssign { names, values } => format!("({}) = ({});", names.iter().join(", "), values.iter().map(|expr| expr.to_string()).join(", ")),
                Stmt::ConstDecl { name, value } => format!("const {} = {};", name, value),
                Stmt::Print(expr) => format!("print {};", expr),
                Stmt::Return(None) => String::from("return;"),
                Stmt::Return(Some(expr)) => format!("return {};", expr),
                Stmt::Assert(expr) => format!("assert {};", expr),
                Stmt::Break(None) => String::from("break;"),
                Stmt::Break(Some(expr)) => format!("break {};", expr),
                Stmt::Continue => String::from("continue;"),
                Stmt::While { condition, code } => format!("while ({})\n{}{}", condition, tab, stringify(code, indent + 1, true)),
                Stmt::Loop { code } => format!("loop\n{}{}", tab, stringify(code, indent + 1, true)),
                Stmt::For { init, condition, step, code } => format!("for ({}; {}; {})\n{}{}", to_string_if_some(init), to_string_if_some(condition), to_string_if_some(step), tab, stringify(code, indent + 1, true)),
                Stmt::ForEach { variable, iterable, code } => format!("for {} in {}\n{}{}", variable, iterable, tab, stringify(code, indent + 1, true)),
                Stmt::If { condition, code, code_else } => format!("if ({})\n{}{}{}", condition, tab, stringify(code, indent + 1, true), match code_else
                {
                    None => String::new(),
                    Some(code) => format!("\n{}else\n{}{}", tab, tab, stringify(code, indent + 1, true)),
                }),
                Stmt::DoWhile { code, condition } => format!("do\n{}{}\n{}while ({});", tab, stringify(code, indent + 1, true), tab, condition),
                Stmt::Impl { type_name, methods } => format!("impl {}\n{}{{\n{}\t{}\n{}}}", type_name, tab, tab, methods.iter().map(|method| method.to_string()).join("\n").replace("\n", &*format!("\n{}\t", tab)), tab),
                Stmt::FnDecl(decl) => decl.to_string().replace("\n", &*format!("\n{}", tab)),
                Stmt::Block(stmts) => return format!("{{\n{}\n{}}}", stmts.iter().map(|stmt| format!("{}{}", tab, stringify(stmt, indent + 1, true))).join("\n"), tab),
            };
            format!("{}{}", prefix, line)
        }
        write!(f, "{}", stringify(self, 0, false))
    }
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

impl Display for FuncSignature
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "func {}", self.name)?;
        if let Some(generic_info) = &self.generic_info
        {
            write!(f, "<{}>", generic_info.type_params.join(", "))?;
        }
        write!(f, "({})", self.args.iter().map(|i| i.to_string()).collect_vec().join(", "))?;
        write!(f, ": {}", self.ret)?;
        if let Some(generic_info) = &self.generic_info
        {
            write!(f, " where ({})", generic_info.constraints.iter().map(|i| i.to_string()).collect_vec().join(", "))?;
        }
        Ok(())
    }
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

impl Display for VarDecl
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", self.name)?;
        match &self.decl_type
        {
            VarDeclType::Scalar(scalar_type, scalar_value) =>
                {
                    if let Some(scalar_type) = scalar_type
                    {
                        write!(f, ": {}", scalar_type)?;
                    }
                    if let Some(scalar_value) = scalar_value
                    {
                        write!(f, " = {}", scalar_value)?;
                    }
                }
            VarDeclType::Array(array_size, array_init) =>
                {
                    if let Some(array_size) = array_size
                    {
                        write!(f, "[{}]", array_size)?;
                    }
                    if let Some(array_init) = array_init
                    {
                        write!(f, " = \"{}\"", array_init)?;
                    }
                }
        }
        Ok(())
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypeDecl
{
    pub name: String,
    pub type_spec: TypeSpec,
    pub generic_info: Option<GenericInfo>,
}

impl Display for TypeDecl
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", self.name)?;
        if let Some(generic_info) = &self.generic_info
        {
            write!(f, "<{}>", generic_info.type_params.join(", "))?;
        }
        write!(f, " = {}", self.type_spec)
    }
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

impl Display for BinOpArith
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self
        {
            BinOpArith::Add => write!(f, "+"),
            BinOpArith::Sub => write!(f, "-"),
            BinOpArith::Mul => write!(f, "*"),
            BinOpArith::Div => write!(f, "/"),
            BinOpArith::And => write!(f, "&&"),
            BinOpArith::Or => write!(f, "||"),
            BinOpArith::Shl => write!(f, "<<"),
            BinOpArith::Shr => write!(f, ">>"),
        }
    }
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

impl Display for BinOpRel
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self
        {
            BinOpRel::Eq => write!(f, "=="),
            BinOpRel::Ne => write!(f, "!="),
            BinOpRel::Lt => write!(f, "<"),
            BinOpRel::Le => write!(f, "<="),
            BinOpRel::Gt => write!(f, ">"),
            BinOpRel::Ge => write!(f, ">="),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum BinOp
{
    Arith(BinOpArith),
    Rel(BinOpRel),
    Assign(Option<BinOpArith>),
    Member,
}

impl Display for BinOp
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self
        {
            BinOp::Arith(op) => write!(f, "{}", op),
            BinOp::Rel(op) => write!(f, "{}", op),
            BinOp::Assign(op) =>
                {
                    if let Some(op) = op
                    {
                        write!(f, "{}", op)?;
                    }
                    write!(f, "=")
                }
            BinOp::Member => write!(f, "."),
        }
    }
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
    fn get_position(&self) -> UnOpPosition
    {
        match self
        {
            UnOp::PostInc | UnOp::PostDec => UnOpPosition::Postfix,
            _ => UnOpPosition::Prefix
        }
    }
}

impl Display for UnOp
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self
        {
            UnOp::Neg => write!(f, "-"),
            UnOp::Not => write!(f, "~"),
            UnOp::Deref => write!(f, "*"),
            UnOp::PreInc | UnOp::PostInc => write!(f, "++"),
            UnOp::PreDec | UnOp::PostDec => write!(f, "--"),
            UnOp::Ref => write!(f, "&"),
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

impl Display for TypeSpec
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self
        {
            TypeSpec::Named(name) => write!(f, "{}", name),
            TypeSpec::Pointer(inner) => write!(f, "*{}", inner),
            TypeSpec::Global(inner) => write!(f, "{} global", inner),
            TypeSpec::Array(inner, size) => write!(f, "{}[{}]", inner, size),
            TypeSpec::TypeOf(inner) => write!(f, "typeof({})", inner),
            TypeSpec::ScalarOf(inner) => write!(f, "scalarof({})", inner),
            TypeSpec::Struct(fields) => write!(f, "struct {{ {}}}", fields.iter().map(|i| format!("{}; ", i)).collect_vec().concat()),
            TypeSpec::Interface(methods) => write!(f, "interface {{ {}}}", methods.iter().map(|i| format!("{}; ", i)).collect_vec().concat()),
            TypeSpec::SelfType => write!(f, "self"),
            TypeSpec::GenericInstanciation(name, args) => write!(f, "{}!<{}>", name, args.iter().map(|a| a.to_string()).collect_vec().join(", ")),
        }
    }
}


impl Display for Pattern
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self
        {
            Pattern::Value(expr) => write!(f, "{}", expr),
            Pattern::Range(start, end) => write!(f, "{}..{}", start, end),
            Pattern::RangeInclusive(start, end) => write!(f, "{}..={}", start, end),
            Pattern::Or(patterns) => write!(f, "{}", patterns.iter().map(|p| format!("{}", p)).collect::<Vec<String>>().join(" | ")),
            Pattern::Wildcard => write!(f, "_"),
        }
    }
}

impl Display for Expr
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self
        {
            Expr::Number(n, None) => write!(f, "{}", n),
            Expr::Number(n, Some(t)) => write!(f, "{}u{}", n, t),
            Expr::SizeOf(t) => write!(f, "sizeof({})", t),
            Expr::BitsOf(t) => write!(f, "bitsof({})", t),
            Expr::New(t) => write!(f, "new {}", t),
            Expr::Ident(name) => write!(f, "{}", name),
            Expr::Compound(stmts, expr) => write!(f, "{{ {} {} }}", stmts.iter().map(|s| format!("{}", s)).collect::<Vec<String>>().join(""), expr),
            Expr::If(cond, then, els) => write!(f, "if ({}) t {} else {}", cond, then, els),
            Expr::Loop(body) => write!(f, "loop {}", body), // TODO: incorrect indentation
            Expr::Match(expr, arms) => write!(f, "match {} {{ {} }}", expr, arms.iter().map(|a| format!("{} => {}", a.0, a.1)).collect::<Vec<String>>().join(", ")),
            Expr::StructLiteral(name, fields) => write!(f, "{} {{ {} }}", name, fields.iter().map(Expr::to_string).collect::<Vec<String>>().join(", ")),
            Expr::StructLiteralNamed(name, fields) => write!(f, "{} {{ {} }}", name, fields.iter().map(|f| format!("{}: {}", f.0, f.1)).collect::<Vec<String>>().join(", ")),
            Expr::BinOp(lhs, op, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::Is(lhs, rhs) => write!(f, "({} is {})", lhs, rhs),
            Expr::UnOp(op, expr) => match op.get_position()
            {
                UnOpPosition::Prefix => write!(f, "({}{})", op, expr),
                UnOpPosition::Postfix => write!(f, "({}{})", expr, op),
            },
            Expr::Call(name, args) => write!(f, "{}({})", name, args.iter().map(|a| a.to_string()).collect::<Vec<String>>().join(", ")),
            Expr::Type(t) => write!(f, "{}", t),
            Expr::Pattern(p) => write!(f, "{}", p),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypedVar(pub String, pub TypeSpec);

impl Display for TypedVar
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        write!(f, "{}: {}", self.0, self.1)
    }
}

