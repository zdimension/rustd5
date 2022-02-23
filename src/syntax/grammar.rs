use peg::parser;
use crate::syntax::ast::*;

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
            / i:ident() { TypeSpec::Named(i.to_string()) }

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

        rule assign_expr() -> Expr
            = l:unary_expr() _ "=" _ r:assign_expr() { Expr::BinOp(Box::new(l), BinOp::Assign(None), Box::new(r)) }
            / l:unary_expr() _ "+=" _ r:assign_expr() { Expr::BinOp(Box::new(l), BinOp::Assign(Some(BinOpArith::Add)), Box::new(r)) }
            / l:unary_expr() _ "-=" _ r:assign_expr() { Expr::BinOp(Box::new(l), BinOp::Assign(Some(BinOpArith::Sub)), Box::new(r)) }
            / l:unary_expr() _ "*=" _ r:assign_expr() { Expr::BinOp(Box::new(l), BinOp::Assign(Some(BinOpArith::Mul)), Box::new(r)) }
            / l:unary_expr() _ "/=" _ r:assign_expr() { Expr::BinOp(Box::new(l), BinOp::Assign(Some(BinOpArith::Div)), Box::new(r)) }
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
            / "{" _ s:(statement() ** _) _ e:expr() _ "}" { Expr::Compound(s, Box::new(e)) }
            / "if" _ "(" _ e1:expr() _ ")" _ "{" _ s1:expr() _ "}" _ "else" _ "{" _ s2:expr() _ "}" { Expr::If(Box::new(e1), Box::new(s1), Box::new(s2)) }
            / "loop" _ s:statement() { Expr::Loop(Box::new(s)) }
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
            / "(" _ v:(ident() ++ comma()) _ ")" _ "=" _ "(" _ e:(expr() ++ comma()) _ ")" _ ";" { Stmt::TupleAssign { names: v, values: e } }
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
            / "continue" semi() { Stmt::Continue }
            / "while" _ "(" _ e:expr() _ ")" _ s:statement() { Stmt::While { condition:Box::new(e), code: Box::new(s) } }
            / "do" _ s:statement() _ "while" _ "(" _ e:expr() _ ")" semi() { Stmt::DoWhile { condition:Box::new(e), code: Box::new(s) } }
            / "loop" _ s:statement() { Stmt::Loop { code: Box::new(s) } }
            / "for" _ "(" _ "var" _ i:ident() _ "in" _ e:expr() _ ")" _ s:statement() { Stmt::ForEach { variable: i.to_string(), iterable: Box::new(e), code: Box::new(s) } }
            / "for" _ "(" _ e1:decl_or_expr()? semi() _ e2:expr()? semi() _ e3:expr()? _ ")" _ s:statement() { Stmt::For{init:e1, condition: e2.map(Box::new), step:e3.map(Box::new), code:Box::new(s)} }
            / e:expr() semi() { Stmt::Discard(Box::new(e)) }

    }
}

