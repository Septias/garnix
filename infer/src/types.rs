use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use strum_macros::{AsRefStr, Display, EnumDiscriminants};

use crate::{infer_error, InferResult};

/// A single identifier.
/// The name should be a debrujin index and the path is used for set accesses.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Var {
    pub level: usize,
    pub name: String,
    pub id: usize,
    pub lower_bounds: Rc<RefCell<Vec<Type>>>,
    pub upper_bounds: Rc<RefCell<Vec<Type>>>,
}

impl std::hash::Hash for Var {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl Var {
    pub fn new(level: usize, id: usize) -> Self {
        Self {
            level,
            id,
            ..Default::default()
        }
    }

    pub fn as_record(&self) -> Option<HashMap<String, Type>> {
        let up = self.upper_bounds.borrow();
        if up.len() == 1 {
            match up.first().unwrap() {
                Type::Record(rc) => Some(rc.clone()),
                _ => None,
            }
        } else {
            None
        }
    }
}

pub type PolarVar<'a> = (Var, bool);

/// A nix language type.
#[derive(Debug, Clone, PartialEq, Eq, Display, EnumDiscriminants, AsRefStr)]
#[strum_discriminants(derive(AsRefStr, Display))]
#[strum_discriminants(name(TypeName))]
pub enum Type {
    Top,
    Bottom,

    Number,
    Bool,
    String,
    Path,
    Null,
    Undefined,

    Var(Var),
    Function(Box<Type>, Box<Type>),
    List(Vec<Type>),
    Record(HashMap<String, Type>),
    Optional(Box<Type>),
    Pattern(HashMap<String, Type>, bool),

    // Complexe Types only created by simplification
    Union(Box<Type>, Box<Type>),
    Inter(Box<Type>, Box<Type>),
    Recursive(Var, Box<Type>),
}

impl Type {
    pub fn get_var(&self) -> InferResult<&Var> {
        match self {
            Type::Var(ident) => Ok(ident),
            t => infer_error(TypeName::Var, t.get_name()),
        }
    }

    pub fn into_var(self) -> Option<Var> {
        match self {
            Type::Var(ident) => Some(ident),
            _ => None,
        }
    }
}

impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // TODO: proper implementation
        core::mem::discriminant(self).hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct PolymorphicType {
    pub body: Type,
    pub level: usize,
}

impl PolymorphicType {
    pub fn new(body: Type, level: usize) -> Self {
        Self { body, level }
    }
}

impl Type {
    pub fn instantiate(&self) -> Type {
        self.clone()
    }

    pub fn level(&self) -> usize {
        match self {
            Type::Var(ident) => ident.level,
            Type::Optional(ty) => ty.level(),
            Type::Function(lhs, rhs) => lhs.level().max(rhs.level()),
            Type::List(ls) => ls.iter().map(|t| t.level()).max().unwrap_or(0),
            Type::Record(rc) => rc.values().map(|t| t.level()).max().unwrap_or(0),
            Type::Bool
            | Type::Number
            | Type::String
            | Type::Path
            | Type::Null
            | Type::Pattern(..)
            | Type::Undefined => 0,
            Type::Top | Type::Bottom | Type::Union(..) | Type::Inter(..) | Type::Recursive(..) => {
                panic!("Not a simple type")
            }
        }
    }


    pub fn is_var(&self) -> bool {
        matches!(self, Type::Var(_))
    }

    /// Returns the enum descriminant of this type.
    pub fn get_name(&self) -> TypeName {
        match self {
            Type::Var(_) => TypeName::Var,
            Type::List(_) => TypeName::List,
            Type::Function(_, _) => TypeName::Function,
            Type::Union(_, _) => TypeName::Union,
            Type::Record(_) => TypeName::Record,
            Type::Top => TypeName::Top,
            Type::Bottom => TypeName::Bottom,
            Type::Inter(_, _) => TypeName::Inter,
            Type::Optional(_) => TypeName::Optional,
            Type::Number => TypeName::Number,
            Type::Bool => TypeName::Bool,
            Type::String => TypeName::String,
            Type::Path => TypeName::Path,
            Type::Null => TypeName::Null,
            Type::Undefined => TypeName::Undefined,
            Type::Recursive(_, _) => TypeName::Recursive,
            Type::Pattern(_, _) => TypeName::Pattern,
        }
    }

    pub fn show(&self) -> String {
        match self {
            Type::Var(ident) => format!("Var({})", ident.id),
            t @ Type::Number
            | t @ Type::Bool
            | t @ Type::String
            | t @ Type::Path
            | t @ Type::Null
            | t @ Type::Undefined => t.as_ref().to_string(),
            Type::List(elems) => format!("[ {} ]", elems.iter().map(|t| t.show()).join(" ")),
            Type::Function(lhs, rhs) => format!("{} -> ({})", lhs.show(), rhs.show()),
            Type::Union(lhs, rhs) => format!("{} ∨ {}", lhs, rhs),
            Type::Record(fields) => fields
                .iter()
                .map(|(k, v)| format!("{}: {}", k, v.show()))
                .join(", "),
            Type::Top => TypeName::Top.to_string(),
            Type::Bottom => TypeName::Bottom.to_string(),
            Type::Inter(lhs, rhs) => format!("{} ∧ {}", lhs.show(), rhs.show()),
            Type::Optional(t) => format!("Optional<{}>", t.show()),
            Type::Recursive(ident, ty) => format!("Rec({}) {}", ident.id, ty.show()),
            Type::Pattern(fields, _) => format!(
                "{{ {} }}",
                fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v.show()))
                    .join(", ")
            ),
        }
    }
}
