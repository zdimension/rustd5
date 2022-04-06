use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::rc::Rc;

use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::analysis::typing::{Type, TypeKind};
use crate::syntax::ast::{Erc, Expr, Stmt, TypeSpec};

pub mod typing;

struct CallSite
{
    pub id: u32,
    pub addr_return: u32,
    pub addr_arg_alloc: u32,
}

enum FunctionData
{
    Normal {
        return_type: Type,
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

struct Function
{
    name: String,
    parameters: Vec<(String, Type)>,
    data: FunctionData,
}

struct LoopInfo
{
    pub address: u32,
    pub node: Erc<Stmt>,
}

enum VariableData
{
    Const(usize),
    Variable {
        position: usize,

    },
}

struct Variable
{
    name: String,
    ty: Type,
}

pub struct StackFrame
{
    function: Option<Rc<Function>>,
    loop_info: Option<Rc<LoopInfo>>,
    is_root: bool,
    size: usize,
    variables: HashMap<String, Variable>,
    types: HashMap<String, Rc<Type>>,
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
            size: 0,
            variables: HashMap::new(),
            types: HashMap::new(),
            parent: None,
            impl_parent: None,
            assign_target: None,
            is_typeof: false,
        }
    }
}

fn make_child_frame(frame: &Rc<StackFrame>) -> Rc<StackFrame>
{
    Rc::new(StackFrame
    {
        function: frame.function.clone(),
        loop_info: frame.loop_info.clone(),
        is_root: false,
        size: 0,
        variables: HashMap::new(),
        types: HashMap::new(),
        parent: Some(frame.clone()),
        impl_parent: frame.impl_parent.clone(),
        assign_target: frame.assign_target.clone(),
        is_typeof: frame.is_typeof,
    })
}

pub fn decode_typespec(spec: &Erc<TypeSpec>, frame: &Rc<StackFrame>) -> Result<Rc<Type>, Diagnostic<()>> {
    let frame = {
        let mut tmp = make_child_frame(frame);
        tmp.borrow_mut().is_typeof = false;
        tmp
    };
    match spec.code() {
        TypeSpec::Named(ref name) => {
            frame.types.get(name).map(Rc::clone).ok_or(Diagnostic::error()
                .with_message(format!("unknown type: {}", name))
                .with_labels(vec![
                    Label::primary((), spec.range())
                ]))
        },
        _ => Err(Diagnostic::error()
        .with_message("can't analyze type")
        .with_labels(vec![
            Label::primary((), spec.range())
        ]))
    }
}

pub fn analysis(node: Erc<Expr>, frame: &Rc<StackFrame>) -> Result<(), Diagnostic<()>> {
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
                node.borrow_mut().meta.type_ = Rc::new(Some(Type::anonymous(TypeKind::Scalar(n))))
            } else {
                node.borrow_mut().meta.type_ = Rc::new(Some(Type::anonymous(TypeKind::Scalar(0))));
            }
        }
        Expr::SizeOf(ref ty) => {
            node.borrow_mut().node = Expr::Number(decode_typespec(ty, frame)?.size_cells(), None);
        }
        _ => {}
    }
    Ok(())
}