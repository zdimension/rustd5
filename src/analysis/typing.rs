use std::rc::Rc;

const POINTER_BITS: usize = 8;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind
{
    Void,
    Scalar(u8),
    Array { target: Rc<Type>, length: usize },
    Pointer { target: Rc<Type>, is_global: bool },
    Composite { fields: Vec<(Variable, Rc<Type>)> },
    Generic,
    GenericVariable,
    Alias
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type
{
    pub name: Option<String>,
    pub kind: TypeKind,
}

impl Type
{
    pub fn named(name: String, kind: TypeKind) -> Self
    {
        Self { name: Some(name), kind }
    }

    pub fn anonymous(kind: TypeKind) -> Self
    {
        Self { name: None, kind }
    }

    pub fn size_cells(&self) -> usize
    {
        match &self.kind
        {
            TypeKind::Scalar(size) =>
                {
                    if *size == 0
                    {
                        panic!("Internal: number literal type missing");
                    }
                    1
                },
            TypeKind::Array { target, length } =>
                {
                    target.size_cells() * length
                },
            TypeKind::Pointer { .. } => 1,
            TypeKind::Composite { fields } =>
                {
                    fields.iter().map(|field| field.1.size_cells()).sum()
                },
            TypeKind::Generic => panic!("Internal: generic type must be instanciated"),
            _ => panic!("Internal: type size unknown")
        }
    }

    pub fn size_bits(&self) -> usize
    {
        match &self.kind
        {
            TypeKind::Scalar(size) => *size as usize,
            TypeKind::Pointer { .. } => POINTER_BITS,
            TypeKind::Generic => panic!("Internal: generic type must be instanciated"),
            _ => panic!("Internal: type size unknown")
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable
{
    pub name: String
}