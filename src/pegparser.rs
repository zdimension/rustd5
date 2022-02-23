extern crate derive_more;
extern crate peg;

use std::fmt;
use std::fmt::Display;

use derive_more::Constructor;
use itertools::Itertools;
use peg::parser;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct VarDeclStmt(Vec<VarDecl>);

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
    Read(Box<Expr>),
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
                Stmt::ConstDecl { name, value } => format!("const {} = {};", name, value.to_string()),
                Stmt::Print(expr) => format!("print {};", expr.to_string()),
                Stmt::Read(expr) => format!("read {};", expr.to_string()),
                Stmt::Return(None) => String::from("return;"),
                Stmt::Return(Some(expr)) => format!("return {};", expr.to_string()),
                Stmt::Assert(expr) => format!("assert {};", expr.to_string()),
                Stmt::Break(None) => String::from("break;"),
                Stmt::Break(Some(expr)) => format!("break {};", expr.to_string()),
                Stmt::Continue => String::from("continue;"),
                Stmt::While { condition, code } => format!("while ({})\n{}{}", condition.to_string(), tab, stringify(code, indent + 1, true)),
                Stmt::Loop { code } => format!("loop\n{}{}", tab, stringify(code, indent + 1, true)),
                Stmt::For { init, condition, step, code } => format!("for ({}; {}; {})\n{}{}", to_string_if_some(init), to_string_if_some(condition), to_string_if_some(step), tab, stringify(code, indent + 1, true)),
                Stmt::ForEach { variable, iterable, code } => format!("for {} in {}\n{}{}", variable, iterable.to_string(), tab, stringify(code, indent + 1, true)),
                Stmt::If { condition, code, code_else } => format!("if ({})\n{}{}{}", condition.to_string(), tab, stringify(code, indent + 1, true), match code_else
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
    name: String,
    decl_type: VarDeclType,
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
                        write!(f, ": {}", scalar_type.to_string())?;
                    }
                    if let Some(scalar_value) = scalar_value
                    {
                        write!(f, " = {}", scalar_value.to_string())?;
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
    name: String,
    type_spec: TypeSpec,
    generic_info: Option<GenericInfo>,
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
        write!(f, " = {}", self.type_spec.to_string())
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
    NamedTypeSpec(String),
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
pub struct MatchArm(Box<Pattern>, Box<Expr>);

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
    IsExpr(Box<Expr>, Box<Pattern>),
    UnOp(UnOp, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Type(Box<TypeSpec>),
}

impl Display for TypeSpec
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self
        {
            TypeSpec::NamedTypeSpec(name) => write!(f, "{}", name),
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
            Expr::Compound(stmts, expr) => write!(f, "{}{}", stmts.iter().map(|s| format!("{}", s)).collect::<Vec<String>>().join("; "), expr),
            Expr::If(cond, then, els) => write!(f, "if ({}) t {} else {}", cond, then, els),
            Expr::Loop(body) => write!(f, "loop {}", body),
            Expr::Match(expr, arms) => write!(f, "match {} {{ {} }}", expr, arms.iter().map(|a| format!("{} => {}", a.0, a.1)).collect::<Vec<String>>().join("; ")),
            Expr::StructLiteral(name, fields) => write!(f, "{} {{ {} }}", name, fields.iter().map(Expr::to_string).collect::<Vec<String>>().join(", ")),
            Expr::StructLiteralNamed(name, fields) => write!(f, "{} {{ {} }}", name, fields.iter().map(|f| format!("{}: {}", f.0, f.1)).collect::<Vec<String>>().join(", ")),
            Expr::BinOp(lhs, op, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::IsExpr(lhs, rhs) => write!(f, "({} is {})", lhs, rhs),
            Expr::UnOp(op, expr) => match op.get_position()
            {
                UnOpPosition::Prefix => write!(f, "({}{})", op, expr),
                UnOpPosition::Postfix => write!(f, "({}{})", expr, op),
            },
            Expr::Call(name, args) => write!(f, "{}({})", name, args.iter().map(|a| a.to_string()).collect::<Vec<String>>().join(", ")),
            Expr::Type(t) => write!(f, "{}", t),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypedVar(String, TypeSpec);

impl Display for TypedVar
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        write!(f, "{}: {}", self.0, self.1)
    }
}

parser! {
    pub grammar td5() for str
    {
        pub rule expr() -> Expr
            = assign_expr()

        rule whitespace() = [' ' | '\n' | '\t' | '\r']
        rule line_comment() = "//" (!"\n" [_])* ("\n" / ![_])
        rule block_comment() = "/*" (!"*/" [_])* "*/"
        rule _ = quiet!{ (whitespace() / line_comment() / block_comment())* }

        rule semi() = _ ";" _
        rule comma() = _ "," _

        rule escape_char() -> char
            = "\\\\" { '\\' }
            / "\\'" { '\'' }
            / "\\\"" { '"' }
            / "\\n" { '\n' }
            / "\\t" { '\t' }
            / "\\r" { '\r' }
            / "\\0" { '\0' }

        rule string_literal() -> String
            = "\"" chars:(escape_char() / [^'\"'])* "\"" { chars.iter().collect() }

        rule char_literal() -> char
            = "\'" c:(escape_char() / [^'\"']) "\'" { c }

        rule var_typed() -> TypedVar
            = i:ident() _ ":" _ t:type_spec() { TypedVar(i.to_string(), t) }

        rule func_prototype() -> FuncSignature
            = "func" _ i:ident() _ "(" _ args:(var_typed() ** comma()) _ ")" _ ":" _ ret:type_spec() { FuncSignature { name: i.to_string(), args, ret, generic_info: None } }
            / "func" _ i:ident() _ p:type_params() _ "(" _ args:(var_typed() ** comma()) _ ")" _ ":" _ ret:type_spec() _ c:generic_constraints()? { FuncSignature { name: i.to_string(), args, ret, generic_info: Some(GenericInfo { type_params: p, constraints: c.unwrap_or_default() }) } }

        rule func_prototype_decl() -> FuncSignature
            = _ f:func_prototype() semi() _ { f }

        rule struct_field() -> TypedVar
            = _ v:var_typed() semi() _ { v }

        rule generic_instanciation() -> TypeSpec
            = i:ident() _ "!<" _ params:(type_spec() ++ comma()) _ ">" { TypeSpec::GenericInstanciation(i.to_string(), params) }

        rule type_spec_named() -> TypeSpec
            = "self" { TypeSpec::SelfType }
            / generic_instanciation()
            / i:ident() { TypeSpec::NamedTypeSpec(i.to_string()) }

        #[cache_left_rec]
        rule type_spec() -> TypeSpec
            = t:type_spec() _ "*" { TypeSpec::Pointer(Box::new(t)) }
            / t:type_spec() _ "global" { TypeSpec::Global(Box::new(t)) }
            / t:type_spec() _ "[" _ e:expr() _ "]" { TypeSpec::Array(Box::new(t), Box::new(e)) }
            / "typeof" _ "(" _ e:expr() _ ")" { TypeSpec::TypeOf(Box::new(e)) }
            / "scalarof" _ "(" _ e:expr() _ ")" { TypeSpec::ScalarOf(Box::new(e)) }
            / "struct" _ "{" _ fields:(struct_field() ** _) _ "}" { TypeSpec::Struct(fields) }
            / "interface" _ "{" _ funcs:(func_prototype_decl() ** _) _ "}" { TypeSpec::Interface(funcs) }
            / type_spec_named()

        rule ident() -> String
            = s:$(['a'..='z' | 'A' ..= 'Z' | '_']['a'..='z' | 'A' ..= 'Z' | '_' | '0'..='9']*) { s.to_string() }
            / "`" s:$([^'`']+) "`" { s.to_string() }

        rule number() -> Expr
            = n:$(['0'..='9']+) "u" s:$(['0'..='9']+) { Expr::Number(n.parse().unwrap(), Some(s.parse().unwrap())) }
            / n:$(['0'..='9']+) { Expr::Number(n.parse().unwrap(), None) }

        rule var() -> Expr
            = i:ident() { Expr::Ident(i) }

        rule pattern_basic() -> Pattern
            = e:l_and_expr() { Pattern::Value(Box::new(e)) }
            / l:l_and_expr() _ ".." _ r:l_and_expr() { Pattern::Range(Box::new(l), Box::new(r)) }
            / l:l_and_expr() _ "..=" _ r:l_and_expr() { Pattern::RangeInclusive(Box::new(l), Box::new(r)) }

        rule pattern() -> Pattern
            = p:(pattern_basic() ** (_ "|" _)) { Pattern::Or(p) }

        rule assign_expr() -> Expr
            = l:unary_expr() _ "=" _ r:assign_expr() { Expr::BinOp(Box::new(l), BinOp::Assign(None), Box::new(r)) }
            / l:unary_expr() _ "+=" _ r:assign_expr() { Expr::BinOp(Box::new(l), BinOp::Assign(Some(BinOpArith::Add)), Box::new(r)) }
            / l:unary_expr() _ "-=" _ r:assign_expr() { Expr::BinOp(Box::new(l), BinOp::Assign(Some(BinOpArith::Sub)), Box::new(r)) }
            / l:unary_expr() _ "*=" _ r:assign_expr() { Expr::BinOp(Box::new(l), BinOp::Assign(Some(BinOpArith::Mul)), Box::new(r)) }
            / l:unary_expr() _ "/=" _ r:assign_expr() { Expr::BinOp(Box::new(l), BinOp::Assign(Some(BinOpArith::Div)), Box::new(r)) }
            / is_expr()

        #[cache_left_rec]
        rule is_expr() -> Expr
            = l:is_expr() _ "is" _ r:pattern() { Expr::IsExpr(Box::new(l), Box::new(r)) }
            / pipe_expr()

        #[cache_left_rec]
        rule pipe_expr() -> Expr
            = l:pipe_expr() _ "|>" _ r:l_or_expr() { Expr::Call(Box::new(r), vec![l]) }
            / l_or_expr()

        #[cache_left_rec]
        rule l_or_expr() -> Expr
            = l:l_or_expr() _ "||" _ r:l_and_expr() { Expr::BinOp(Box::new(l), BinOp::Arith(BinOpArith::Or), Box::new(r)) }
            / l_and_expr()

        #[cache_left_rec]
        rule l_and_expr() -> Expr
            = l:l_and_expr() _ "&&" _ r:eq_expr() { Expr::BinOp(Box::new(l), BinOp::Arith(BinOpArith::And), Box::new(r)) }
            / eq_expr()

        #[cache_left_rec]
        rule eq_expr() -> Expr
            = l:eq_expr() _ "==" _ r:rel_expr() { Expr::BinOp(Box::new(l), BinOp::Rel(BinOpRel::Eq), Box::new(r)) }
            / l:eq_expr() _ "!=" _ r:rel_expr() { Expr::BinOp(Box::new(l), BinOp::Rel(BinOpRel::Ne), Box::new(r)) }
            / rel_expr()

        #[cache_left_rec]
        rule rel_expr() -> Expr
            = l:rel_expr() _ "<" _ r:shift_expr() { Expr::BinOp(Box::new(l), BinOp::Rel(BinOpRel::Lt), Box::new(r)) }
            / l:rel_expr() _ "<=" _ r:shift_expr() { Expr::BinOp(Box::new(l), BinOp::Rel(BinOpRel::Le), Box::new(r)) }
            / l:rel_expr() _ ">" _ r:shift_expr() { Expr::BinOp(Box::new(l), BinOp::Rel(BinOpRel::Gt), Box::new(r)) }
            / l:rel_expr() _ ">=" _ r:shift_expr() { Expr::BinOp(Box::new(l), BinOp::Rel(BinOpRel::Ge), Box::new(r)) }
            / shift_expr()

        #[cache_left_rec]
        rule shift_expr() -> Expr
            = l:shift_expr() _ "<<" _ r:add_expr() { Expr::BinOp(Box::new(l), BinOp::Arith(BinOpArith::Shl), Box::new(r)) }
            / l:shift_expr() _ ">>" _ r:add_expr() { Expr::BinOp(Box::new(l), BinOp::Arith(BinOpArith::Shr), Box::new(r)) }
            / add_expr()

        #[cache_left_rec]
        rule add_expr() -> Expr
            = l:add_expr() _ "+" _ r:mul_expr() { Expr::BinOp(Box::new(l), BinOp::Arith(BinOpArith::Add), Box::new(r)) }
            / l:add_expr() _ "-" _ r:mul_expr() { Expr::BinOp(Box::new(l), BinOp::Arith(BinOpArith::Sub), Box::new(r)) }
            / mul_expr()

        #[cache_left_rec]
        rule mul_expr() -> Expr
            = l:mul_expr() _ "*" _ r:unary_expr() { Expr::BinOp(Box::new(l), BinOp::Arith(BinOpArith::Mul), Box::new(r)) }
            / l:mul_expr() _ "/" _ r:unary_expr() { Expr::BinOp(Box::new(l), BinOp::Arith(BinOpArith::Div), Box::new(r)) }
            / unary_expr()

        #[cache_left_rec]
        rule unary_expr() -> Expr
            = "~" _ e:unary_expr() { Expr::UnOp(UnOp::Not, Box::new(e)) }
            / "*" _ e:unary_expr() { Expr::UnOp(UnOp::Deref, Box::new(e)) }
            / "&" _ e:unary_expr() { Expr::UnOp(UnOp::Ref, Box::new(e)) }
            / "++" _ e:unary_expr() { Expr::UnOp(UnOp::PreInc, Box::new(e)) }
            / "--" _ e:unary_expr() { Expr::UnOp(UnOp::PreDec, Box::new(e)) }
            / "-" _ e:unary_expr() { Expr::UnOp(UnOp::Neg, Box::new(e)) }
            / postfix_expr()

        #[cache_left_rec]
        rule postfix_expr() -> Expr
            = e:postfix_expr() _ "++" { Expr::UnOp(UnOp::PostInc, Box::new(e)) }
            / e:postfix_expr() _ "--" { Expr::UnOp(UnOp::PostDec, Box::new(e)) }
            / e:postfix_expr() _ "[" _ e2:expr() _ "]" { Expr::UnOp(UnOp::Deref, Box::new(Expr::BinOp(Box::new(e), BinOp::Arith(BinOpArith::Add), Box::new(e2)))) }
            / e:postfix_expr() _ "." _ m:var() { Expr::BinOp(Box::new(e), BinOp::Member, Box::new(m)) }
            / e:postfix_expr() _ "(" _ args:(expr() ** comma()) _ ")" { Expr::Call(Box::new(e), args) }
            / e:basic_expr()

        rule match_arm() -> MatchArm
            = p:pattern() _ "=>" _ e:expr() { MatchArm(Box::new(p), Box::new(e)) }

        rule named_literal_field() -> StructLiteralNamedField
            = m:ident() _ ":" _ e:expr() { StructLiteralNamedField(m, Box::new(e)) }

        rule basic_expr() -> Expr
            = number()
            / c:char_literal() { Expr::Number(c as i64, None) }
            / "sizeof" _ "(" _ t:type_spec() _ ")" { Expr::SizeOf(Box::new(t)) }
            / "bitsof" _ "(" _ t:type_spec() _ ")" { Expr::BitsOf(Box::new(t)) }
            / "new" _ "(" _ t:type_spec() _ ")" { Expr::New(Box::new(t)) }
            / "(" _ e:expr() _ ")" { e }
            / "if" _ "(" _ e1:expr() _ ")" _ "{" _ s1:expr() _ "}" _ "else" _ "{" _ s2:expr() _ "}" { Expr::If(Box::new(e1), Box::new(s1), Box::new(s2)) }
            / "loop" _ "{" _ s:statement() _ "}" { Expr::Loop(Box::new(s)) }
            / "match" _ "(" _ e:expr() _ ")" _ "{" _ arms:(match_arm() ** comma()) _ "}" { Expr::Match(Box::new(e), arms) }
            / t:type_spec_named() _ "{" _ fields:(expr() ** comma()) _ "}" { Expr::StructLiteral(Box::new(t), fields) }
            / t:type_spec_named() _ "{" _ fields:(named_literal_field() ** comma()) _ "}" { Expr::StructLiteralNamed(Box::new(t), fields) }
            / t:generic_instanciation() { Expr::Type(Box::new(t)) }
            / v:var() { v }

        rule var_decl() -> VarDecl
            = i:ident() _ ":" _ t:type_spec() { VarDecl { name: i.to_string(), decl_type: VarDeclType::Scalar(Some(Box::new(t)), None) }}
            / i:ident() _ ":" _ t:type_spec() _ "=" _ e:expr() { VarDecl { name: i.to_string(), decl_type: VarDeclType::Scalar(Some(Box::new(t)), Some(Box::new(e))) }}
            / i:ident() _ "=" _ e:expr() { VarDecl { name: i.to_string(), decl_type: VarDeclType::Scalar(None, Some(Box::new(e))) }}
            / i:ident() _ "[" _ e:expr() _ "]" { VarDecl { name: i.to_string(), decl_type: VarDeclType::Array(Some(Box::new(e)), None) }}
            / i:ident() _ "[" _ e:expr()? _ "]" _ "=" _ e2:string_literal() { VarDecl { name: i.to_string(), decl_type: VarDeclType::Array(e.map(Box::new), Some(e2)) }}

        rule type_params() -> Vec<String>
            = "<" _ params:(ident() ++ comma()) _ ">" { params }

        rule generic_constraints() -> Vec<Expr>
            = "where" _ "(" _ constraints:(expr() ++ comma()) _ ")" { constraints }

        rule type_decl() -> TypeDecl
            = i:ident() _ "=" _ t:type_spec() { TypeDecl { name: i.to_string(), type_spec: t, generic_info: None }}
            / i:ident() _ p:type_params() _ c:generic_constraints()? _ "=" _ t:type_spec() { TypeDecl { name: i.to_string(), type_spec: t, generic_info: Some(GenericInfo { type_params: p, constraints: c.unwrap_or_default() }) }}

        rule func_decl() -> FnDecl
            = f:func_prototype() s:block() { FnDecl(f, s) }

        rule block() -> Stmt
            = _ "{" _ s:(statement() ** _) _ "}" _ { Stmt::Block(s) }

        rule var_decl_stmt() -> VarDeclStmt
            = "var" _ vars:(var_decl() ** comma()) { VarDeclStmt(vars) }

        rule decl_or_expr() -> DeclOrExpr
            = v:var_decl_stmt() { DeclOrExpr::Decl(Box::new(v)) }
            / e:expr() { DeclOrExpr::Expr(Box::new(e)) }

        pub rule statement() -> Stmt
            = _ s:statement_inner() _ { s }

        rule statement_inner() -> Stmt
            = ";" { Stmt::Empty }
            / f:func_decl() { Stmt::FnDecl(Box::new(f)) }
            / "impl" _ i:ident() _ "{" _ f:(func_decl() ** _) _ "}" { Stmt::Impl { type_name: i.to_string(), methods: f } }
            / "if" _ "(" _ c:expr() _ ")" s:statement() !"else" { Stmt::If { condition: Box::new(c), code: Box::new(s), code_else: None } }
            / "if" _ "(" _ c:expr() _ ")" s:statement() "else" e:statement() { Stmt::If { condition: Box::new(c), code: Box::new(s), code_else: Some(Box::new(e)) } }
            / "const" _ i:ident() _ "=" _ e:expr() semi() { Stmt::ConstDecl { name: i.to_string(), value: Box::new(e) } }
            / s:var_decl_stmt() semi() { Stmt::VarDeclList(s) }
            / "type" _ types:(type_decl() ** comma()) semi() { Stmt::TypeDeclList(types) }
            / block()
            / "return" _ e:expr()? semi() { Stmt::Return(e.map(Box::new)) }
            / "break" _ e:expr()? semi() { Stmt::Break(e.map(Box::new)) }
            / "assert" _ e:expr() semi() { Stmt::Assert(Box::new(e)) }
            / "print" _ e:expr() semi() { Stmt::Print(Box::new(e)) }
            / "read" _ e:expr() semi() { Stmt::Read(Box::new(e)) }
            / "continue" semi() { Stmt::Continue }
            / "while" _ "(" _ e:expr() _ ")" _ s:statement() { Stmt::While { condition:Box::new(e), code: Box::new(s) } }
            / "loop" _ s:statement() { Stmt::Loop { code: Box::new(s) } }
            / "for" _ "(" _ "var" _ i:ident() _ "in" _ e:expr() _ ")" _ s:statement() { Stmt::ForEach { variable: i.to_string(), iterable: Box::new(e), code: Box::new(s) } }
            / "for" _ "(" _ e1:decl_or_expr()? semi() _ e2:expr()? semi() _ e3:expr()? _ ")" _ s:statement() { Stmt::For{init:e1, condition: e2.map(Box::new), step:e3.map(Box::new), code:Box::new(s)} }
            / e:expr() semi() { Stmt::Discard(Box::new(e)) }

    }
}

