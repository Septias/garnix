use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use strum_macros::{AsRefStr, Display, EnumDiscriminants};

use crate::{type_mismatch, InferResult};

/// A single identifier.
/// The name should be a debrujin index and the path is used for set accesses.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Var {
    pub level: usize,
    pub id: usize,
    pub lower_bounds: Rc<RefCell<Vec<Ty>>>,
    pub upper_bounds: Rc<RefCell<Vec<Ty>>>,
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

    pub fn as_record(&self) -> Option<HashMap<String, Ty>> {
        let up = self.upper_bounds.borrow();
        if up.len() == 1 {
            match up.first().unwrap() {
                Ty::Record(rc) => Some(rc.clone()),
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn as_list(&self) -> Option<Vec<Ty>> {
        let up = self.upper_bounds.borrow();
        if up.len() == 1 {
            match up.first().unwrap() {
                Ty::List(ls) => Some(ls.clone()),
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
pub enum Ty {
    Top,
    Bottom,

    Number,
    Bool,
    String,
    Path,
    Null,
    Undefined,

    // Var(Var),
    Function(Box<Ty>, Box<Ty>),
    List(Vec<Ty>),
    Record(HashMap<String, Ty>),
    Optional(Box<Ty>),
    Pattern(HashMap<String, (Ty, Option<Ty>)>, bool),

    // Complex Types only created by simplification
    Union(Box<Ty>, Box<Ty>),
    Inter(Box<Ty>, Box<Ty>),
    // Recursive(Var, Box<Ty>),
}

impl Ty {
    pub fn get_var(&self) -> InferResult<&Var> {
        match self {
            Ty::Var(ident) => Ok(ident),
            t => type_mismatch(TypeName::Var, t.get_name()),
        }
    }

    pub fn into_var(self) -> Option<Var> {
        match self {
            Ty::Var(ident) => Some(ident),
            _ => None,
        }
    }
}

impl std::hash::Hash for Ty {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct PolymorphicType {
    pub body: Ty,
    pub level: usize,
}

impl PolymorphicType {
    pub fn new(body: Ty, level: usize) -> Self {
        Self { body, level }
    }
}

impl Ty {
    pub fn instantiate(&self) -> Ty {
        self.clone()
    }

    pub fn level(&self) -> usize {
        match self {
            Ty::Var(ident) => ident.level,
            Ty::Optional(ty) => ty.level(),
            Ty::Function(lhs, rhs) => lhs.level().max(rhs.level()),
            Ty::List(ls) => ls.iter().map(|t| t.level()).max().unwrap_or(0),
            Ty::Record(rc) => rc.values().map(|t| t.level()).max().unwrap_or(0),
            Ty::Pattern(fields, _) => fields.values().map(|(t, _)| t.level()).max().unwrap_or(0),
            Ty::Union(left, right) => left.level().max(right.level()),
            Ty::Bool | Ty::Number | Ty::String | Ty::Path | Ty::Null | Ty::Undefined => 0,
            Ty::Top | Ty::Bottom | Ty::Inter(..) | Ty::Recursive(..) => {
                panic!("Not a simple type")
            }
        }
    }

    pub fn is_var(&self) -> bool {
        matches!(self, Ty::Var(_))
    }

    /// Returns the enum descriminant of this type.
    pub fn get_name(&self) -> TypeName {
        match self {
            Ty::Var(_) => TypeName::Var,
            Ty::List(_) => TypeName::List,
            Ty::Function(_, _) => TypeName::Function,
            Ty::Union(_, _) => TypeName::Union,
            Ty::Record(_) => TypeName::Record,
            Ty::Top => TypeName::Top,
            Ty::Bottom => TypeName::Bottom,
            Ty::Inter(_, _) => TypeName::Inter,
            Ty::Optional(_) => TypeName::Optional,
            Ty::Number => TypeName::Number,
            Ty::Bool => TypeName::Bool,
            Ty::String => TypeName::String,
            Ty::Path => TypeName::Path,
            Ty::Null => TypeName::Null,
            Ty::Undefined => TypeName::Undefined,
            Ty::Recursive(_, _) => TypeName::Recursive,
            Ty::Pattern(_, _) => TypeName::Pattern,
        }
    }

    pub fn show(&self) -> String {
        match self {
            Ty::Var(ident) => format!("Var({})", ident.id),
            t @ Ty::Number
            | t @ Ty::Bool
            | t @ Ty::String
            | t @ Ty::Path
            | t @ Ty::Null
            | t @ Ty::Undefined => t.as_ref().to_string(),
            Ty::List(elems) => format!("[ {} ]", elems.iter().map(|t| t.show()).join(" ")),
            Ty::Function(lhs, rhs) => format!("{} -> ({})", lhs.show(), rhs.show()),
            Ty::Union(lhs, rhs) => format!("{} ∨ {}", lhs, rhs),
            Ty::Record(fields) => fields
                .iter()
                .map(|(k, v)| format!("{{{}: {}}}", k, v.show()))
                .join(", "),
            Ty::Top => TypeName::Top.to_string(),
            Ty::Bottom => TypeName::Bottom.to_string(),
            Ty::Inter(lhs, rhs) => format!("{} ∧ {}", lhs.show(), rhs.show()),
            Ty::Optional(t) => format!("Optional<{}>", t.show()),
            Ty::Recursive(ident, ty) => format!("Rec({}) {}", ident.id, ty.show()),
            Ty::Pattern(fields, _) => format!(
                "Pat: {{ {} }}",
                fields
                    .iter()
                    .map(|(k, (v, _))| format!("{}: {}", k, v.show()))
                    .join(", ")
            ),
        }
    }
}
