use peg::parser;
use crate::syntax::ast::*;

parser!
{
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
            / "func" _ i:ident() _ type_params:type_params() _ "(" _ args:(var_typed() ** comma()) _ ")" _ ":" _ ret:type_spec() _ c:generic_constraints()? { FuncSignature { name: i.to_string(), args, ret, generic_info: Some(GenericInfo { type_params, constraints: c.unwrap_or_default() }) } }

        rule func_prototype_decl() -> FuncSignature
            = _ f:func_prototype() semi() _ { f }

        rule struct_field() -> TypedVar
            = _ v:var_typed() semi() _ { v }

        rule generic_instanciation() -> TypeSpec
            = i:ident() _ "!<" _ params:(type_spec() ++ comma()) _ ">" { TypeSpec::GenericInstanciation(i.to_string(), params) }

        rule type_spec_named() -> TypeSpec
            = "self" { TypeSpec::SelfType }
            / generic_instanciation()
            / i:ident() { TypeSpec::Named(i.to_string()) }

        #[cache_left_rec]
        rule type_spec() -> TypeSpec
            = t:type_spec() _ "*" { TypeSpec::Pointer(Box::new(t)) }
            / t:type_spec() _ "global" { TypeSpec::Global(Box::new(t)) }
            / t:type_spec() _ "[" _ e:expr_box() _ "]" { TypeSpec::Array(Box::new(t), e) }
            / "typeof" _ "(" _ e:expr_box() _ ")" { TypeSpec::TypeOf(e) }
            / "scalarof" _ "(" _ e:expr_box() _ ")" { TypeSpec::ScalarOf(e) }
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

        rule pattern_expr() -> Expr
            = p:pattern() { Expr::Pattern(Box::new(p)) }

        rule named_literal_field_pattern() -> StructLiteralNamedField
            = m:ident() _ ":" _ e:pattern_expr() { StructLiteralNamedField(m, Box::new(e)) }

        rule pattern_basic() -> Pattern
            = t:type_spec_named() _ "{" _ fields:(pattern_expr() ** comma()) _ "}" { Pattern::Value(Box::new(Expr::StructLiteral(Box::new(t), fields))) }
            / t:type_spec_named() _ "{" _ fields:(named_literal_field_pattern() ** comma()) _ "}" { Pattern::Value(Box::new(Expr::StructLiteralNamed(Box::new(t), fields))) }
            / l:l_and_expr() _ "..=" _ r:l_and_expr() { Pattern::RangeInclusive(Box::new(l), Box::new(r)) }
            / l:l_and_expr() _ ".." _ r:l_and_expr() { Pattern::Range(Box::new(l), Box::new(r)) }
            / e:l_and_expr() { Pattern::Value(Box::new(e)) }

        rule pattern() -> Pattern
            = "_" { Pattern::Wildcard }
            / p:(pattern_basic() ** (_ "|" _)) { Pattern::Or(p) }

        rule assign_op() -> Option<BinOpArith>
            = "=" { None }
            / "+=" { Some(BinOpArith::Add) }
            / "-=" { Some(BinOpArith::Sub) }
            / "*=" { Some(BinOpArith::Mul) }
            / "/=" { Some(BinOpArith::Div) }

        rule assign_expr() -> Expr
            = l:unary_expr() _ o:assign_op() _ r:assign_expr() { Expr::BinOp(Box::new(l), BinOp::Assign(o), Box::new(r)) }
            / is_expr()

        #[cache_left_rec]
        rule is_expr() -> Expr
            = l:is_expr() _ "is" _ r:pattern() { Expr::Is(Box::new(l), Box::new(r)) }
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

        rule eq_op() -> BinOpRel
            = "==" { BinOpRel::Eq }
            / "!=" { BinOpRel::Ne }

        #[cache_left_rec]
        rule eq_expr() -> Expr
            = l:eq_expr() _ o:eq_op() _ r:rel_expr() { Expr::BinOp(Box::new(l), BinOp::Rel(o), Box::new(r)) }
            / rel_expr()

        rule rel_op() -> BinOpRel
            = "<=" { BinOpRel::Le }
            / "<" { BinOpRel::Lt }
            / ">=" { BinOpRel::Ge }
            / ">" { BinOpRel::Gt }

        #[cache_left_rec]
        rule rel_expr() -> Expr
            = l:rel_expr() _ o:rel_op() _ r:shift_expr() { Expr::BinOp(Box::new(l), BinOp::Rel(o), Box::new(r)) }
            / shift_expr()

        rule shift_op() -> BinOpArith
            = "<<" { BinOpArith::Shl }
            / ">>" { BinOpArith::Shr }

        #[cache_left_rec]
        rule shift_expr() -> Expr
            = l:shift_expr() _ o:shift_op() _ r:add_expr() { Expr::BinOp(Box::new(l), BinOp::Arith(o), Box::new(r)) }
            / add_expr()

        rule add_op() -> BinOpArith
            = "+" { BinOpArith::Add }
            / "-" { BinOpArith::Sub }

        #[cache_left_rec]
        rule add_expr() -> Expr
            = l:add_expr() _ o:add_op() _ r:mul_expr() { Expr::BinOp(Box::new(l), BinOp::Arith(o), Box::new(r)) }
            / mul_expr()

        rule mul_op() -> BinOpArith
            = "*" { BinOpArith::Mul }
            / "/" { BinOpArith::Div }

        #[cache_left_rec]
        rule mul_expr() -> Expr
            = l:mul_expr() _ o:mul_op() _ r:unary_expr() { Expr::BinOp(Box::new(l), BinOp::Arith(o), Box::new(r)) }
            / unary_expr()

        rule unary_op() -> UnOp
            = "~" { UnOp::Not }
            / "*" { UnOp::Deref }
            / "&" { UnOp::Ref }
            / "++" { UnOp::PreInc }
            / "--" { UnOp::PreDec }
            / "-" { UnOp::Neg }

        rule unary_expr() -> Expr
            = o:unary_op() _ e:unary_expr() { Expr::UnOp(o, Box::new(e)) }
            / postfix_expr()

        #[cache_left_rec]
        rule postfix_expr() -> Expr
            = e:postfix_expr() _ "++" { Expr::UnOp(UnOp::PostInc, Box::new(e)) }
            / e:postfix_expr() _ "--" { Expr::UnOp(UnOp::PostDec, Box::new(e)) }
            / e:postfix_expr() _ "[" _ e2:expr_box() _ "]" { Expr::UnOp(UnOp::Deref, Box::new(Expr::BinOp(Box::new(e), BinOp::Arith(BinOpArith::Add), e2))) }
            / e:postfix_expr() _ "." _ m:var() { Expr::BinOp(Box::new(e), BinOp::Member, Box::new(m)) }
            / e:postfix_expr() _ "(" _ args:(expr() ** comma()) _ ")" { Expr::Call(Box::new(e), args) }
            / e:basic_expr()

        rule match_arm() -> MatchArm
            = p:pattern() _ "=>" _ e:expr_box() { MatchArm(Box::new(p), e) }

        rule named_literal_field() -> StructLiteralNamedField
            = m:ident() _ ":" _ e:expr_box() { StructLiteralNamedField(m, e) }

        rule basic_expr() -> Expr
            = number()
            / c:char_literal() { Expr::Number(c as i64, None) }
            / "sizeof" _ "(" _ t:type_spec() _ ")" { Expr::SizeOf(Box::new(t)) }
            / "bitsof" _ "(" _ t:type_spec() _ ")" { Expr::BitsOf(Box::new(t)) }
            / "new" _ "(" _ t:type_spec() _ ")" { Expr::New(Box::new(t)) }
            / "(" _ e:expr() _ ")" { e }
            / "{" _ s:(statement() ** _) _ e:expr_box() _ "}" { Expr::Compound(s, e) }
            / "if" _ "(" _ e1:expr_box() _ ")" _ "{" _ s1:expr_box() _ "}" _ "else" _ "{" _ s2:expr_box() _ "}" { Expr::If(e1, s1, s2) }
            / "loop" _ s:statement_box() { Expr::Loop(s) }
            / "match" _ "(" _ e:expr_box() _ ")" _ "{" _ arms:(match_arm() ** comma()) _ "}" { Expr::Match(e, arms) }
            / t:type_spec_named() _ "{" _ fields:(expr() ** comma()) _ "}" { Expr::StructLiteral(Box::new(t), fields) }
            / t:type_spec_named() _ "{" _ fields:(named_literal_field() ** comma()) _ "}" { Expr::StructLiteralNamed(Box::new(t), fields) }
            / t:generic_instanciation() { Expr::Type(Box::new(t)) }
            / v:var() { v }

        rule var_decl() -> VarDecl
            = i:ident() _ ":" _ t:type_spec() { VarDecl { name: i.to_string(), decl_type: VarDeclType::Scalar(Some(Box::new(t)), None) }}
            / i:ident() _ ":" _ t:type_spec() _ "=" _ e:expr_box() { VarDecl { name: i.to_string(), decl_type: VarDeclType::Scalar(Some(Box::new(t)), Some(e)) }}
            / i:ident() _ "=" _ e:expr_box() { VarDecl { name: i.to_string(), decl_type: VarDeclType::Scalar(None, Some(e)) }}
            / i:ident() _ "[" _ e:expr_box() _ "]" { VarDecl { name: i.to_string(), decl_type: VarDeclType::Array(Some(e), None) }}
            / i:ident() _ "[" _ e:expr_box()? _ "]" _ "=" _ e2:string_literal() { VarDecl { name: i.to_string(), decl_type: VarDeclType::Array(e, Some(e2)) }}

        rule type_params() -> Vec<String>
            = "<" _ params:(ident() ++ comma()) _ ">" { params }

        rule generic_constraints() -> Vec<Expr>
            = "where" _ "(" _ constraints:(expr() ++ comma()) _ ")" { constraints }

        rule type_decl() -> TypeDecl
            = i:ident() _ "=" _ t:type_spec() { TypeDecl { name: i.to_string(), type_spec: t, generic_info: None }}
            / i:ident() _ p:type_params() _ c:generic_constraints()? _ "=" _ t:type_spec() { TypeDecl { name: i.to_string(), type_spec: t, generic_info: Some(GenericInfo { type_params: p, constraints: c.unwrap_or_default() }) }}

        rule func_decl() -> FnDecl
            = signature:func_prototype() code:block() { FnDecl { signature, code } }

        rule block() -> Stmt
            = _ "{" _ s:(statement() ** _) _ "}" _ { Stmt::Block(s) }

        rule var_decl_stmt() -> VarDeclStmt
            = "var" _ vars:(var_decl() ** comma()) { VarDeclStmt(vars) }

        rule decl_or_expr() -> DeclOrExpr
            = v:var_decl_stmt() { DeclOrExpr::Decl(Box::new(v)) }
            / e:expr_box() { DeclOrExpr::Expr(e) }

        pub rule statement() -> Stmt
            = _ s:statement_inner() _ { s }

        rule statement_box() -> Box<Stmt>
            = s:statement() { Box::new(s) }

        rule expr_box() -> Box<Expr>
            = e:expr() { Box::new(e) }

        rule statement_inner() -> Stmt
            = ";" { Stmt::Empty }
            / "(" _ names:(ident() ++ comma()) _ ")" _ "=" _ "(" _ values:(expr() ++ comma()) _ ")" _ ";" { Stmt::TupleAssign { names, values } }
            / f:func_decl() { Stmt::FnDecl(Box::new(f)) }
            / "impl" _ i:ident() _ "{" _ methods:(func_decl() ** _) _ "}" { Stmt::Impl { type_name: i.to_string(), methods } }
            / "if" _ "(" _ condition:expr_box() _ ")" code:statement_box() !"else" { Stmt::If { condition, code, code_else: None } }
            / "if" _ "(" _ condition:expr_box() _ ")" code:statement_box() "else" code_else:statement_box() { Stmt::If { condition, code, code_else: Some(code_else) } }
            / "const" _ i:ident() _ "=" _ value:expr_box() semi() { Stmt::ConstDecl { name: i.to_string(), value } }
            / s:var_decl_stmt() semi() { Stmt::VarDeclList(s) }
            / "type" _ types:(type_decl() ** comma()) semi() { Stmt::TypeDeclList(types) }
            / block()
            / "return" _ e:expr_box()? semi() { Stmt::Return(e) }
            / "break" _ e:expr_box()? semi() { Stmt::Break(e) }
            / "assert" _ e:expr_box() semi() { Stmt::Assert(e) }
            / "print" _ e:expr_box() semi() { Stmt::Print(e) }
            / "continue" semi() { Stmt::Continue }
            / "while" _ "(" _ condition:expr_box() _ ")" _ code:statement_box() { Stmt::While { condition, code } }
            / "do" _ code:statement_box() _ "while" _ "(" _ condition:expr_box() _ ")" semi() { Stmt::DoWhile { condition, code } }
            / "loop" _ code:statement_box() { Stmt::Loop { code } }
            / "for" _ "(" _ "var" _ i:ident() _ "in" _ iterable:expr_box() _ ")" _ code:statement_box() { Stmt::ForEach { variable: i.to_string(), iterable, code } }
            / "for" _ "(" _ init:decl_or_expr()? semi() condition:expr_box()? semi() step:expr_box()? _ ")" _ code:statement_box() { Stmt::For{init, condition, step, code} }
            / e:expr_box() semi() { Stmt::Discard(e) }

    }
}

