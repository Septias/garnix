use std::collections::HashMap;
use strum::EnumTryAs;
use strum_macros::{AsRefStr, Display, EnumDiscriminants};

use crate::{infer_error, InferResult};

pub trait TypeScheme {
    fn instantiate(self, lvl: usize) -> Type;
}

pub trait SimpleType: TypeScheme {
    fn level(&self) -> usize;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum Primitives {
    Number,
    Bool,
    String,
    Path,
    Null,
    Undefined,
}
/// A single identifier.
/// The name should be a debrujin index and the path is used for set accesses.
#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
pub struct Var {
    pub lower_bounds: Vec<Type>,
    pub upper_bounds: Vec<Type>,
    pub debrujin: usize,
    pub level: usize,
}

pub type PolarVar = (Var, bool);

/// A nix language type.
#[derive(Debug, Clone, PartialEq, Eq, Display, EnumDiscriminants, EnumTryAs)]
#[strum_discriminants(derive(AsRefStr, Display))]
#[strum_discriminants(name(TypeName))]
pub enum Type {
    Top,
    Bottom,
    Primitive(Primitives),
    Var(Var),
    Function(Box<Type>, Box<Type>),
    List(Vec<Type>),
    Record(HashMap<String, Type>),
    Optional(Box<Type>),

    // Complexe types only created by simplification
    Union(Box<Type>, Box<Type>),
    Inter(Box<Type>, Box<Type>),
}

impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // TODO: proper implementation
        core::mem::discriminant(self).hash(state);
    }
}

struct PolymorphicType {
    body: Type,
    level: usize,
}

impl TypeScheme for Type {
    fn instantiate(self, lvl: usize) -> Type {
        self
    }
}

impl SimpleType for Type {
    fn level(&self) -> usize {
        match self {
            Type::Var(ident) => ident.level,
            Type::Optional(ty) => ty.level(),
            Type::Function(lhs, rhs) => lhs.level().max(rhs.level()),
            Type::List(ls) => ls.iter().map(|t| t.level()).max().unwrap_or(0),
            Type::Record(rc) => rc.values().map(|t| t.level()).max().unwrap_or(0),
            Type::Primitive(_) => 0,
            Type::Top | Type::Bottom | Type::Union(..) | Type::Inter(..) => {
                panic!("Not a simple type")
            }
        }
    }
}

impl Type {
    /// Try to convert this type to a list.
    pub fn into_list(self) -> InferResult<Vec<Type>> {
        match self {
            Type::List(elems) => Ok(elems),
            t => infer_error(TypeName::List, t.get_name()),
        }
    }

    /// Try to convert this type to an identifier.
    pub fn into_debrujin(self) -> InferResult<usize> {
        match self {
            Type::Var(ident) => Ok(ident.debrujin),
            t => infer_error(TypeName::Var, t.get_name()),
        }
    }

    /// Try to convert this type to a function.
    pub fn into_function(self) -> InferResult<(Type, Type)> {
        match self {
            Type::Function(box lhs, box rhs) => Ok((lhs, rhs)),
            t => infer_error(TypeName::Function, t.get_name()),
        }
    }

    pub fn is_var(&self) -> bool {
        matches!(self, Type::Var(_))
    }

    /// Returns the enum descriminant of this type.
    pub fn get_name(&self) -> TypeName {
        match self {
            Type::Var(_) => TypeName::Var,
            Type::Primitive(_) => TypeName::Primitive,
            Type::List(_) => TypeName::List,
            Type::Function(_, _) => TypeName::Function,
            Type::Union(_, _) => TypeName::Union,
            Type::Record(_) => TypeName::Record,
            Type::Top => TypeName::Top,
            Type::Bottom => TypeName::Bottom,
            Type::Inter(_, _) => TypeName::Inter,
            Type::Optional(_) => TypeName::Optional,
        }
    }

    pub fn show(&self) -> String {
        match self {
            Type::Var(ident) => format!("Var({})", ident.debrujin),
            Type::Primitive(p) => format!("Primitive({})", p),
            Type::List(elems) => format!("List({:?})", elems),
            Type::Function(lhs, rhs) => format!("Function({:?}, {:?})", lhs, rhs),
            Type::Union(lhs, rhs) => format!("Union({:?}, {:?})", lhs, rhs),
            Type::Record(fields) => format!("Record({:?})", fields),
            Type::Top => "Top".to_string(),
            Type::Bottom => "Bottom".to_string(),
            Type::Inter(lhs, rhs) => format!("Inter({:?}, {:?})", lhs, rhs),
            Type::Optional(t) => format!("Optional({:?})", t),
        }
    }
}
