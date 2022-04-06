pub mod typing;

use std::rc::Rc;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use crate::analysis::typing::{Type, TypeKind};
use crate::syntax::ast::{Erc, Expr};

pub fn analysis(node: Erc<Expr>) -> Result<(), Diagnostic<()>> {
    match *node {
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
                node.meta().borrow_mut().type_ = Rc::new(Some(Type::anonymous(TypeKind::Scalar(n))))
            } else {
                node.meta().borrow_mut().type_ = Rc::new(Some(Type::anonymous(TypeKind::Scalar(0))));
            }
        },
        Expr::SizeOf(ref type_) => {

        },
        _ => {},
    }
    Ok(())
}