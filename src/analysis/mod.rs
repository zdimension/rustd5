use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::analysis::typing::{Type, TypeKind};
use crate::syntax::ast::{DeclOrExpr, Erc, Expr, Stmt, TypeSpec, VarDecl, VarDeclStmt, VarDeclType};

pub mod typing;

#[derive(Debug, Clone)]
struct CallSite
{
    pub id: u32,
    pub addr_return: u32,
    pub addr_arg_alloc: u32,
}

#[derive(Debug, Clone)]
enum FunctionData
{
    Normal {
        return_type: Rc<Type>,
        code: Erc<Stmt>,
        call_sites: Vec<CallSite>,
    },
    Generic {
        type_params: Vec<String>,
        definition: Erc<Stmt>,
        instances: Vec<Rc<Function>>,
    },
    Builtin,
}

#[derive(Debug, Clone)]
struct Function
{
    name: String,
    parameters: Vec<(String, Rc<Type>)>,
    data: FunctionData,
}

#[derive(Debug, Clone)]
struct LoopInfo
{
    pub address: u32,
    pub node: Erc<Stmt>,
}

#[derive(Debug, Clone)]
enum VariableData
{
    Const(usize),
    Variable {
        position: usize,

    },
}

#[derive(Debug, Clone)]
struct Variable
{
    ty: Rc<Type>,
}

#[derive(Debug, Clone)]
pub struct StackFrame
{
    function: Option<Rc<Function>>,
    loop_info: Option<Rc<LoopInfo>>,
    is_root: bool,
    variables: RefCell<HashMap<String, Variable>>,
    types: RefCell<HashMap<String, Rc<Type>>>,
    parent: Option<Rc<StackFrame>>,
    impl_parent: Option<Rc<Type>>,
    assign_target: Option<Erc<Expr>>,
    is_typeof: bool,
}

impl StackFrame
{
    pub fn empty() -> Self
    {
        Self {
            function: None,
            loop_info: None,
            is_root: false,
            variables: RefCell::new(HashMap::new()),
            types: RefCell::new(HashMap::new()),
            parent: None,
            impl_parent: None,
            assign_target: None,
            is_typeof: false,
        }
    }

    pub fn set_typeof(&self, is_typeof: bool) -> Self
    {
        Self {
            is_typeof,
            ..self.clone()
        }
    }
}

fn make_child_frame(frame: &Rc<StackFrame>) -> StackFrame
{
    StackFrame
    {
        function: frame.function.clone(),
        loop_info: frame.loop_info.clone(),
        parent: Some(frame.clone()),
        impl_parent: frame.impl_parent.clone(),
        assign_target: frame.assign_target.clone(),
        is_typeof: frame.is_typeof,
        ..StackFrame::empty()
    }
}

pub fn static_eval(node: &Erc<Expr>, frame: &Rc<StackFrame>) -> Result<i64, Diagnostic<()>> {
    Ok(0)
}

pub fn decode_typespec(spec: &Erc<TypeSpec>, frame: &Rc<StackFrame>) -> Result<Rc<Type>, Diagnostic<()>> {
    let frame = Rc::new(StackFrame { is_typeof: false, ..make_child_frame(frame) });
    match spec.code() {
        TypeSpec::Named(ref name) => {
            frame.types.borrow().get(name).map(Rc::clone).ok_or_else(|| Diagnostic::error()
                .with_message(format!("unknown type: {}", name))
                .with_labels(vec![
                    Label::primary((), spec.range())
                ]))
        },
        TypeSpec::SelfType => {
            frame.impl_parent.as_ref().map(Rc::clone).ok_or_else(|| Diagnostic::error()
                .with_message("self type used outside of an impl block".to_string())
                .with_labels(vec![
                    Label::primary((), spec.range())
                ]))
        },
        TypeSpec::Pointer(ref inner) => {
            let inner = decode_typespec(inner, &frame)?;
            Ok(Rc::new(Type::anonymous(TypeKind::Pointer { target: inner, is_global: frame.function.is_none() })))
        },
        TypeSpec::Global(ref inner) => {
            let inner = decode_typespec(inner, &frame)?;
            if let Type { kind: TypeKind::Pointer { target: ref ty, is_global: glob }, .. } = inner.borrow() {
                if *glob {
                    Ok(ty.clone())
                } else {
                    Ok(Rc::new(Type::anonymous(TypeKind::Pointer { target: Rc::clone(ty), is_global: true })))
                }
            } else {
                Err(Diagnostic::error()
                    .with_message("global type must be a pointer".to_string())
                    .with_labels(vec![
                        Label::primary((), spec.range())
                    ]))
            }
        },
        TypeSpec::Array(ref ty, ref lenval) => {
            let ty = decode_typespec(ty, &frame)?;
            let len = static_eval(lenval, &frame)?;
            if len < 0 {
                Err(Diagnostic::error()
                    .with_message("array length must be non-negative".to_string())
                    .with_labels(vec![
                        Label::primary((), lenval.range())
                    ]))
            } else {
                Ok(Rc::new(Type::anonymous(TypeKind::Array { target: ty, length: len as usize })))
            }
        },
        TypeSpec::TypeOf(ref expr) => {
            let frame = Rc::new(frame.set_typeof(true));
            analyze_expr(expr, &frame)?;
            expr.meta().type_.ok_or_else(|| Diagnostic::error()
                .with_message("typeof expression must have a type".to_string())
                .with_labels(vec![
                    Label::primary((), expr.range())
                ]))
        },
        TypeSpec::ScalarOf(ref expr) => {
            let frame = Rc::new(frame.set_typeof(true));
            analyze_expr(expr, &frame)?;
            let size = static_eval(expr, &frame)?;
            if !(0..=256).contains(&size) {
                Err(Diagnostic::error()
                    .with_message("scalar size must be in range [0, 256]".to_string())
                    .with_labels(vec![
                        Label::primary((), expr.range())
                    ]))
            } else {
                Ok(Rc::new(Type::anonymous(TypeKind::Scalar(size as u8))))
            }
        },
        _ => Err(Diagnostic::error()
        .with_message("can't analyze type")
        .with_labels(vec![
            Label::primary((), spec.range())
        ]))
    }
}

pub fn analyze_expr(node: &Erc<Expr>, frame: &Rc<StackFrame>) -> Result<(), Diagnostic<()>> {
    match node.code() {
        Expr::Number(val, size) => {
            if let Some(n) = size {
                if n < 64 && val > (1 << n) {
                    return Err(Diagnostic::error()
                        .with_message("number is too large")
                        .with_labels(vec![
                            Label::primary((), node.range())
                        ])
                        .with_notes(vec![format!("expected 0 ≤ value < {}", 1 << n)]));
                }
                node.set_type(Type::anonymous(TypeKind::Scalar(n)));
            } else {
                node.set_type(Type::anonymous(TypeKind::Scalar(0)));
            }
        }
        Expr::SizeOf(ref ty) => {
            node.borrow_mut().node = Expr::Number(decode_typespec(ty, frame)?.size_cells(), None);
        },
        Expr::BitsOf(ref ty) => {
            node.borrow_mut().node = Expr::Number(decode_typespec(ty, frame)?.size_bits(), None);
        },
        Expr::New(ref ty) => {
            node.set_type(Type::anonymous(TypeKind::Pointer { target: decode_typespec(ty, frame)?, is_global: frame.function.is_none() }));
        },
        _ => {
            return Err(Diagnostic::error()
                .with_message(format!("unsupported expression: {:?}", node.code()))
                .with_labels(vec![
                    Label::primary((), node.range())
                ]));
        }
    }
    Ok(())
}

fn analyze_var_decl(stmt: &VarDeclStmt, frame: &Rc<StackFrame>) -> Result<(), Diagnostic<()>> {
    for decl in &stmt.0 {
        let VarDecl { name, decl_type } = &decl.code();
        if frame.variables.borrow().contains_key(name) {
            return Err(Diagnostic::error()
                .with_message(format!("variable '{}' already declared", name))
                .with_labels(vec![
                    Label::primary((), decl.range())
                ]));
        }

        frame.variables.borrow_mut().insert(name.clone(), match decl_type {
            VarDeclType::Scalar(ref spec, ref val) => {
                let ty = if let Some(ref spec) = *spec {
                    decode_typespec(spec, frame)?
                } else if let Some(val) = val {
                    val.get_type(frame)?
                } else {
                    return Err(Diagnostic::error()
                        .with_message("cannot infer type of uninitialized variable".to_string())
                        .with_labels(vec![
                            Label::primary((), decl.range())
                        ]));
                };

                Variable { ty }
            }
            VarDeclType::Array(ref len, ref init) => {
                let len = if let Some(ref expr) = len {
                    static_eval(expr, frame)?
                } else if let Some(ref val) = init {
                    val.len() as i64
                } else {
                    return Err(Diagnostic::error()
                        .with_message("uninitialized array must have a length".to_string())
                        .with_labels(vec![
                            Label::primary((), decl.range())
                        ]));
                };

                if len < 0 {
                    return Err(Diagnostic::error()
                        .with_message("array length must be non-negative".to_string())
                        .with_labels(vec![
                            Label::primary((), decl.range())
                        ]));
                }

                Variable { ty: Rc::new(Type::anonymous(TypeKind::Array { target: Rc::new(Type::anonymous(TypeKind::Scalar(8))), length: len as usize })) }
            }
        });
    }
    Ok(())
}

pub fn analyze_stmt(node: &Erc<Stmt>, frame: &Rc<StackFrame>) -> Result<(), Diagnostic<()>> {
    node.set_type(Type::anonymous(TypeKind::Void));
    match node.code() {
        Stmt::Empty => {

        },
        Stmt::Discard(ref expr) => {
            analyze_expr(expr, frame)?;
        },
        Stmt::Block(ref stmts) => {
            let frame = Rc::new(make_child_frame(frame));
            for stmt in stmts {
                analyze_stmt(stmt, &frame)?;
            }
        },
        Stmt::VarDeclList(ref vars) => {
            analyze_var_decl(vars, frame)?;
        },
        Stmt::For { ref init, ref condition, ref step, ref code } => {
            let frame = Rc::new(make_child_frame(frame));
            if let Some(ref init) = *init {
                match init {
                    DeclOrExpr::Decl(ref decl) => {
                        analyze_var_decl(&decl.code(), &frame)?;
                    },
                    DeclOrExpr::Expr(ref expr) => {
                        analyze_expr(expr, &frame)?;
                    }
                }
            }
            if let Some(ref condition) = *condition {
                analyze_expr(condition, &frame)?;
            }
            if let Some(ref step) = *step {
                analyze_expr(step, &frame)?;
            }
            analyze_stmt(code, &frame)?;
        },
        Stmt::While { ref condition, ref code } => {
            let frame = Rc::new(make_child_frame(frame));
            condition.expect_non_void(&frame);
            analyze_stmt(code, &frame)?;
        },
        Stmt::Print(ref expr) => {
            expr.expect_non_void(frame)?;
        },
        _ => {
            return Err(Diagnostic::error()
                .with_message(format!("unsupported statement: {:?}", node.code()))
                .with_labels(vec![
                    Label::primary((), node.range())
                ]));
        }
    }
    Ok(())
}