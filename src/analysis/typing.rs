#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind
{
    Void,
    Scalar(u8),
    Array { target: Box<Type>, length: usize },
    Pointer { target: Box<Type>, is_global: bool },
    Composite { fields: Vec<Variable> },
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable
{
    pub name: String
}