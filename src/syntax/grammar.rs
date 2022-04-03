use peg::parser;
use crate::syntax::ast::*;

parser!
{
    pub grammar td5() for str
    {
        pub rule erc_expr() -> Erc<Expr>
            = e:erc(<expr()>) { e }

        rule expr() -> Expr
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

        rule erc<T>(x: rule<T>) -> Erc<T>
            = s:position!() v:x() e:position!() {Erc::new(v, s, e)}

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
            = t:erc(<type_spec()>) _ "*" { TypeSpec::Pointer(t) }
            / t:erc(<type_spec()>) _ "global" { TypeSpec::Global(t) }
            / t:erc(<type_spec()>) _ "[" _ e:erc(<expr()>) _ "]" { TypeSpec::Array(t, e) }
            / "typeof" _ "(" _ e:erc(<expr()>) _ ")" { TypeSpec::TypeOf(e) }
            / "scalarof" _ "(" _ e:erc(<expr()>) _ ")" { TypeSpec::ScalarOf(e) }
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
            = p:erc(<pattern()>) { Expr::Pattern(p) }

        rule named_literal_field_pattern() -> StructLiteralNamedField
            = m:ident() _ ":" _ e:erc(<pattern_expr()>) { StructLiteralNamedField(m, e) }

        rule pattern_basic() -> Pattern
            = s:position!() t:erc(<type_spec_named()>) _ "{" _ fields:(erc(<pattern_expr()>) ** comma()) _ "}" e:position!() { Pattern::Value(Erc::new(Expr::StructLiteral(t, fields), s, e)) }
            / s:position!() t:erc(<type_spec_named()>) _ "{" _ fields:(named_literal_field_pattern() ** comma()) _ "}" e:position!() { Pattern::Value(Erc::new(Expr::StructLiteralNamed(t, fields), s, e)) }
            / l:erc(<l_and_expr()>) _ "..=" _ r:erc(<l_and_expr()>) { Pattern::RangeInclusive(l, r) }
            / l:erc(<l_and_expr()>) _ ".." _ r:erc(<l_and_expr()>) { Pattern::Range(l, r) }
            / e:erc(<l_and_expr()>) { Pattern::Value(e) }

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
            = l:erc(<unary_expr()>) _ o:assign_op() _ r:erc(<assign_expr()>) { Expr::BinOp(l, BinOp::Assign(o), r) }
            / is_expr()

        #[cache_left_rec]
        rule is_expr() -> Expr
            = l:erc(<is_expr()>) _ "is" _ r:erc(<pattern()>) { Expr::Is(l, r) }
            / pipe_expr()

        #[cache_left_rec]
        rule pipe_expr() -> Expr
            = l:erc(<pipe_expr()>) _ "|>" _ r:erc(<l_or_expr()>) { Expr::Call(r, vec![l]) }
            / l_or_expr()

        #[cache_left_rec]
        rule l_or_expr() -> Expr
            = l:erc(<l_or_expr()>) _ "||" _ r:erc(<l_and_expr()>) { Expr::BinOp(l, BinOp::Arith(BinOpArith::Or), r) }
            / l_and_expr()

        #[cache_left_rec]
        rule l_and_expr() -> Expr
            = l:erc(<l_and_expr()>) _ "&&" _ r:erc(<eq_expr()>) { Expr::BinOp(l, BinOp::Arith(BinOpArith::And), r) }
            / eq_expr()

        rule eq_op() -> BinOpRel
            = "==" { BinOpRel::Eq }
            / "!=" { BinOpRel::Ne }

        #[cache_left_rec]
        rule eq_expr() -> Expr
            = l:erc(<eq_expr()>) _ o:eq_op() _ r:erc(<rel_expr()>) { Expr::BinOp(l, BinOp::Rel(o), r) }
            / rel_expr()

        rule rel_op() -> BinOpRel
            = "<=" { BinOpRel::Le }
            / "<" { BinOpRel::Lt }
            / ">=" { BinOpRel::Ge }
            / ">" { BinOpRel::Gt }

        #[cache_left_rec]
        rule rel_expr() -> Expr
            = l:erc(<rel_expr()>) _ o:rel_op() _ r:erc(<shift_expr()>) { Expr::BinOp(l, BinOp::Rel(o), r) }
            / shift_expr()

        rule shift_op() -> BinOpArith
            = "<<" { BinOpArith::Shl }
            / ">>" { BinOpArith::Shr }

        #[cache_left_rec]
        rule shift_expr() -> Expr
            = l:erc(<shift_expr()>) _ o:shift_op() _ r:erc(<add_expr()>) { Expr::BinOp(l, BinOp::Arith(o), r) }
            / add_expr()

        rule add_op() -> BinOpArith
            = "+" { BinOpArith::Add }
            / "-" { BinOpArith::Sub }

        #[cache_left_rec]
        rule add_expr() -> Expr
            = l:erc(<add_expr()>) _ o:add_op() _ r:erc(<mul_expr()>) { Expr::BinOp(l, BinOp::Arith(o), r) }
            / mul_expr()

        rule mul_op() -> BinOpArith
            = "*" { BinOpArith::Mul }
            / "/" { BinOpArith::Div }

        #[cache_left_rec]
        rule mul_expr() -> Expr
            = l:erc(<mul_expr()>) _ o:mul_op() _ r:erc(<unary_expr()>) { Expr::BinOp(l, BinOp::Arith(o), r) }
            / unary_expr()

        rule unary_op() -> UnOp
            = "~" { UnOp::Not }
            / "*" { UnOp::Deref }
            / "&" { UnOp::Ref }
            / "++" { UnOp::PreInc }
            / "--" { UnOp::PreDec }
            / "-" { UnOp::Neg }

        rule unary_expr() -> Expr
            = o:unary_op() _ e:erc(<unary_expr()>) { Expr::UnOp(o, e) }
            / postfix_expr()

        #[cache_left_rec]
        rule postfix_expr() -> Expr
            = e:erc(<postfix_expr()>) _ "++" { Expr::UnOp(UnOp::PostInc, e) }
            / e:erc(<postfix_expr()>) _ "--" { Expr::UnOp(UnOp::PostDec, e) }
            / e:erc(<postfix_expr()>) _ "[" _ s:position!() e2:erc(<expr()>) end:position!() _ "]" { Expr::UnOp(UnOp::Deref, Erc::new(Expr::BinOp(e, BinOp::Arith(BinOpArith::Add), e2), s, end)) }
            / e:erc(<postfix_expr()>) _ "." _ m:erc(<var()>) { Expr::BinOp(e, BinOp::Member, m) }
            / e:erc(<postfix_expr()>) _ "(" _ args:(erc(<expr()>) ** comma()) _ ")" { Expr::Call(e, args) }
            / e:basic_expr()

        rule match_arm() -> MatchArm
            = p:erc(<pattern()>) _ "=>" _ e:erc(<expr()>) { MatchArm(p, e) }

        rule named_literal_field() -> StructLiteralNamedField
            = m:ident() _ ":" _ e:erc(<expr()>) { StructLiteralNamedField(m, e) }

        rule basic_expr() -> Expr
            = number()
            / c:char_literal() { Expr::Number(c as u64, None) }
            / "sizeof" _ "(" _ t:erc(<type_spec()>) _ ")" { Expr::SizeOf(t) }
            / "bitsof" _ "(" _ t:erc(<type_spec()>) _ ")" { Expr::BitsOf(t) }
            / "new" _ "(" _ t:erc(<type_spec()>) _ ")" { Expr::New(t) }
            / "(" _ e:expr() _ ")" { e }
            / "{" _ s:(statement() ** _) _ e:erc(<expr()>) _ "}" { Expr::Compound(s, e) }
            / "if" _ "(" _ e1:erc(<expr()>) _ ")" _ "{" _ s1:erc(<expr()>) _ "}" _ "else" _ "{" _ s2:erc(<expr()>) _ "}" { Expr::If(e1, s1, s2) }
            / "loop" _ s:erc(<statement()>) { Expr::Loop(s) }
            / "match" _ "(" _ e:erc(<expr()>) _ ")" _ "{" _ arms:(match_arm() ** comma()) _ "}" { Expr::Match(e, arms) }
            / t:erc(<type_spec_named()>) _ "{" _ fields:(erc(<expr()>) ** comma()) _ "}" { Expr::StructLiteral(t, fields) }
            / t:erc(<type_spec_named()>) _ "{" _ fields:(named_literal_field() ** comma()) _ "}" { Expr::StructLiteralNamed(t, fields) }
            / t:erc(<generic_instanciation()>) { Expr::Type(t) }
            / v:var() { v }

        rule var_decl() -> VarDecl
            = i:ident() _ ":" _ t:erc(<type_spec()>) { VarDecl { name: i.to_string(), decl_type: VarDeclType::Scalar(Some(t), None) }}
            / i:ident() _ ":" _ t:erc(<type_spec()>) _ "=" _ e:erc(<expr()>) { VarDecl { name: i.to_string(), decl_type: VarDeclType::Scalar(Some(t), Some(e)) }}
            / i:ident() _ "=" _ e:erc(<expr()>) { VarDecl { name: i.to_string(), decl_type: VarDeclType::Scalar(None, Some(e)) }}
            / i:ident() _ "[" _ e:erc(<expr()>) _ "]" { VarDecl { name: i.to_string(), decl_type: VarDeclType::Array(Some(e), None) }}
            / i:ident() _ "[" _ e:erc(<expr()>)? _ "]" _ "=" _ e2:string_literal() { VarDecl { name: i.to_string(), decl_type: VarDeclType::Array(e, Some(e2)) }}

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
            = v:erc(<var_decl_stmt()>) { DeclOrExpr::Decl(v) }
            / e:erc(<expr()>) { DeclOrExpr::Expr(e) }

        pub rule statement() -> Stmt
            = _ s:statement_inner() _ { s }

        rule statement_inner() -> Stmt
            = ";" { Stmt::Empty }
            / "(" _ names:(ident() ++ comma()) _ ")" _ "=" _ "(" _ values:(erc(<expr()>) ++ comma()) _ ")" _ ";" { Stmt::TupleAssign { names, values } }
            / f:erc(<func_decl()>) { Stmt::FnDecl(f) }
            / "impl" _ i:ident() _ "{" _ methods:(func_decl() ** _) _ "}" { Stmt::Impl { type_name: i.to_string(), methods } }
            / "if" _ "(" _ condition:erc(<expr()>) _ ")" code:erc(<statement()>) !"else" { Stmt::If { condition, code, code_else: None } }
            / "if" _ "(" _ condition:erc(<expr()>) _ ")" code:erc(<statement()>) "else" code_else:erc(<statement()>) { Stmt::If { condition, code, code_else: Some(code_else) } }
            / "const" _ i:ident() _ "=" _ value:erc(<expr()>) semi() { Stmt::ConstDecl { name: i.to_string(), value } }
            / s:var_decl_stmt() semi() { Stmt::VarDeclList(s) }
            / "type" _ types:(type_decl() ** comma()) semi() { Stmt::TypeDeclList(types) }
            / block()
            / "return" _ e:erc(<expr()>)? semi() { Stmt::Return(e) }
            / "break" _ e:erc(<expr()>)? semi() { Stmt::Break(e) }
            / "assert" _ e:erc(<expr()>) semi() { Stmt::Assert(e) }
            / "print" _ e:erc(<expr()>) semi() { Stmt::Print(e) }
            / "continue" semi() { Stmt::Continue }
            / "while" _ "(" _ condition:erc(<expr()>) _ ")" _ code:erc(<statement()>) { Stmt::While { condition, code } }
            / "do" _ code:erc(<statement()>) _ "while" _ "(" _ condition:erc(<expr()>) _ ")" semi() { Stmt::DoWhile { condition, code } }
            / "loop" _ code:erc(<statement()>) { Stmt::Loop { code } }
            / "for" _ "(" _ "var" _ i:ident() _ "in" _ iterable:erc(<expr()>) _ ")" _ code:erc(<statement()>) { Stmt::ForEach { variable: i.to_string(), iterable, code } }
            / "for" _ "(" _ init:decl_or_expr()? semi() condition:erc(<expr()>)? semi() step:erc(<expr()>)? _ ")" _ code:erc(<statement()>) { Stmt::For{init, condition, step, code} }
            / e:erc(<expr()>) semi() { Stmt::Discard(e) }

    }
}

