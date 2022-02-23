use std::fmt;
use std::fmt::Display;
use itertools::Itertools;
use crate::syntax::ast::*;

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

impl Display for TypedVar
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        write!(f, "{}: {}", self.0, self.1)
    }
}