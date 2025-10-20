use crate::{
    module::Name,
    parreaux::freshen_above,
    types::{PolymorphicType, Ty},
};

pub type Scope<'a> = Vec<(Name<'a>, ContextType)>;

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ContextType {
    Type(Ty),
    PolymorhicType(PolymorphicType),
}

impl ContextType {
    fn instantiate(&self, context: &Context, lvl: usize) -> Ty {
        match self {
            ContextType::Type(ty) => ty.instantiate(),
            ContextType::PolymorhicType(pty) => freshen_above(context, &pty.body, pty.level, lvl),
        }
    }

    fn into_type(self) -> Option<Ty> {
        match self {
            ContextType::Type(ty) => Some(ty),
            ContextType::PolymorhicType(_) => None,
        }
    }
}

/// Context to save variables and their types.
#[derive(Debug)]
#[salsa::tracked]
pub struct Context<'a> {
    // pub bindings: Vec<Scope>,
}

// impl<'a> Context<'a> {
//     /// Create a new scope and run a function with it.
//     pub(crate) fn with_scope<T>(
//         &self,
//         db: &dyn salsa::Database,
//         scope: Vec<(Name, ContextType)>,
//         f: impl FnOnce(Vec<Scope>) -> T,
//     ) -> T {
//         // let mut new = self.bindings(db).clone();
//         // new.push(scope);
//         // f(new)
//         todo!()
//     }

//     /// Lookup a [Var] by it's name.
//     pub(crate) fn lookup(&self, db: &dyn salsa::Database, name: Name) -> Option<ContextType> {
//         for scope in self.bindings(db).iter().rev() {
//             for (item_name, item) in scope.iter().rev() {
//                 if *item_name == name {
//                     return Some(item.clone());
//                 }
//             }
//         }
//         None
//     }
// }
