//! Everything related to parreax' type inference algorithm.

use std::collections::{HashMap, HashSet};

use crate::{
    context::Context,
    module::Expr,
    types::{PolarVar, Ty, Var},
    InferResult,
};

// pub fn constrain(context: &Context, lhs: &(Ty, Expr), rhs: &(Ty, Expr)) {
//     constrain_inner(context, lhs, rhs, &mut HashSet::new())
// }

// fn constrain_inner<'a>(
//     context: &Context,
//     lhs: &'a (Ty, Expr),
//     rhs: &'a (Ty, Expr),
//     cache: &mut HashSet<(Ty, Ty)>,
// ) {
//     let (lhs, lexpr) = lhs;
//     let (rhs, rexpr) = rhs;

//     if lhs == rhs {
//         return;
//     }
//     let lhs_rhs = (lhs.clone(), rhs.clone());

//     match (lhs, rhs) {
//         (Ty::Var(..), _) | (_, Ty::Var(..)) => {
//             if cache.contains(&lhs_rhs) {
//                 return;
//             }
//             cache.insert(lhs_rhs.clone());
//         }
//         _ => (),
//     }

//     match (lhs, rhs) {
//         (Ty::Bool, Ty::Bool)
//         | (Ty::Number, Ty::Number)
//         | (Ty::String, Ty::String)
//         | (Ty::Path, Ty::Path)
//         | (Ty::Null, Ty::Null)
//         | (Ty::Undefined, _) => (),

//         (Ty::Function(l0, r0), Ty::Function(l1, r1)) => {
//             constrain_inner(context, l1, l0, cache);
//             constrain_inner(context, r0, r1, cache);
//         }

//         (Ty::Record(fs0), Ty::Record(fs1)) => {
//             for (n1, t1) in fs1 {
//                 match fs0.iter().find(|(n0, _)| *n0 == n1) {
//                     Some((_, t0)) => constrain_inner(context, t0, t1, cache)?,
//                     None => return Err(InferError::MissingRecordField { field: n1.clone() }),
//                 }
//             }
//         }

//         (Ty::Record(rcd), Ty::Pattern(pat, wildcart)) => {
//             if *wildcart {
//                 for (pat, (t1, optional)) in pat.iter() {
//                     match rcd.iter().find(|(n0, _)| *n0 == pat) {
//                         Some((_, t0)) => constrain_inner(context, t0, t1, cache)?,
//                         None => {
//                             if optional.is_none() {
//                                 return Err(InferError::MissingRecordField { field: pat.clone() });
//                             }
//                         }
//                     }
//                 }
//             } else {
//                 let mut keys = rcd.iter().map(|(n, _)| n).collect::<HashSet<_>>();
//                 for (n1, (t1, optional)) in pat.iter() {
//                     match rcd.iter().find(|(n0, _)| *n0 == n1) {
//                         Some((_, t0)) => {
//                             if let Some(opt) = optional {
//                                 constrain_inner(context, t0, opt, cache)?;
//                             }
//                             constrain_inner(context, t0, t1, cache)?;
//                             keys.remove(n1);
//                         }
//                         None => {
//                             if optional.is_none() {
//                                 return Err(InferError::MissingRecordField { field: n1.clone() });
//                             }
//                         }
//                     }
//                 }
//                 if !keys.is_empty() {
//                     return Err(InferError::TooManyField {
//                         field: keys.into_iter().next().unwrap().clone(),
//                     });
//                 }
//             }
//         }

//         (Ty::Optional(o1), Ty::Optional(o0)) => {
//             constrain_inner(context, o0, o1, cache)?;
//         }

//         (Ty::List(ls1), Ty::List(ls2)) if ls1.len() == 1 && ls2.len() == 1 => {
//             constrain_inner(context, &ls1[0], &ls2[0], cache)?;
//         }

//         // application
//         // function constraints
//         // selection
//         (Ty::Var(lhs), rhs) if rhs.level() <= lhs.level => {
//             lhs.upper_bounds.borrow_mut().push(rhs.clone());
//             for lower_bound in lhs.lower_bounds.borrow().iter() {
//                 constrain_inner(context, lower_bound, rhs, cache)?;
//             }
//         }

//         (Ty::Pattern(pat, _), rhs @ Ty::Var(_)) => {
//             constrain_inner(
//                 context,
//                 &Ty::Record(
//                     pat.clone()
//                         .into_iter()
//                         .map(|(name, (_ty, opt))| (name, opt.unwrap_or(Ty::Undefined)))
//                         .collect(),
//                 ),
//                 rhs,
//                 cache,
//             )?;
//         }

//         // let-binding
//         // record typing
//         (lhs, Ty::Var(rhs)) if lhs.level() <= rhs.level => {
//             rhs.lower_bounds.borrow_mut().push(lhs.clone());
//             for upper_bound in rhs.upper_bounds.borrow().iter() {
//                 constrain_inner(context, lhs, upper_bound, cache)?;
//             }
//         }
//         (Ty::Var(_), rhs) => {
//             let rhs_extruded = extrude(context, rhs, false, lhs.level(), &mut HashMap::new());
//             constrain_inner(context, lhs, &rhs_extruded, cache)?;
//         }
//         (lhs, Ty::Var(_)) => {
//             let lhs_extruded = extrude(context, lhs, true, rhs.level(), &mut HashMap::new());
//             constrain_inner(context, &lhs_extruded, rhs, cache)?;
//         }

//         _ => {
//             return Err(InferError::CannotConstrain {
//                 lhs: lhs.clone(),
//                 rhs: rhs.clone(),
//             })
//         }
//     }

//     Ok(())
// }

// fn fresh_var(level: usize) -> Var {
//     Var {
//         level,
//         ..Default::default()
//     }
// }

// /// Extrude an expression.
// /// A type is problematic, if the levls that occur during constraining violate the level assupmtions. Meaning the expression is expected to have at least level x, but some
// /// variables have a level < x. ???
// /// Make a copy of the problematic type such that the copy has the requested level and is a subtype (if pol == false) or a supertype (if pol == true) of the original type.
// fn extrude(
//     context: &Context,
//     ty: &Ty,
//     pol: bool,
//     lvl: usize,
//     c: &mut HashMap<PolarVar, Var>,
// ) -> Ty {
//     if ty.level() <= lvl {
//         return ty.clone();
//     }

//     match ty {
//         Ty::Number | Ty::Bool | Ty::String | Ty::Path | Ty::Null | Ty::Undefined => ty.clone(),

//         Ty::Var(vs) => {
//             let pol_var = (vs.clone(), pol);
//             if let Some(nvs) = c.get(&pol_var) {
//                 Ty::Var(nvs.clone())
//             } else {
//                 let nvs = fresh_var(lvl);
//                 c.insert(pol_var, nvs.clone());

//                 if pol {
//                     vs.upper_bounds.borrow_mut().push(Ty::Var(nvs.clone()));
//                     *nvs.lower_bounds.borrow_mut() = vs
//                         .lower_bounds
//                         .borrow()
//                         .iter()
//                         .map(|t| extrude(context, t, pol, lvl, c))
//                         .collect();
//                 } else {
//                     vs.lower_bounds.borrow_mut().push(Ty::Var(nvs.clone()));
//                     *nvs.upper_bounds.borrow_mut() = vs
//                         .upper_bounds
//                         .borrow()
//                         .iter()
//                         .map(|t| extrude(context, t, pol, lvl, c))
//                         .collect();
//                 }

//                 Ty::Var(nvs)
//             }
//         }

//         Ty::Function(l, r) => Ty::Function(
//             Box::new(extrude(context, l, !pol, lvl, c)),
//             Box::new(extrude(context, r, pol, lvl, c)),
//         ),
//         Ty::Record(fs) => Ty::Record(
//             fs.iter()
//                 .map(|(name, t)| (name.clone(), extrude(context, t, pol, lvl, c)))
//                 .collect(),
//         ),

//         Ty::List(ls) => Ty::List(
//             ls.iter()
//                 .map(|t| extrude(context, t, pol, lvl, c))
//                 .collect(),
//         ),
//         Ty::Optional(ty) => Ty::Optional(Box::new(extrude(context, ty, pol, lvl, c))),
//         Ty::Union(lhs, rhs) => Ty::Union(
//             Box::new(extrude(context, lhs, pol, lvl, c)),
//             Box::new(extrude(context, rhs, pol, lvl, c)),
//         ),
//         Ty::Pattern(lhs, rhs) => {
//             let mut lhs = lhs.clone();
//             for (_, (ty, opt)) in lhs.iter_mut() {
//                 *ty = extrude(context, ty, !pol, lvl, c);
//                 if let Some(opt) = opt {
//                     *opt = extrude(context, opt, pol, lvl, c);
//                 }
//             }
//             Ty::Pattern(lhs, *rhs)
//         }
//         Ty::Top | Ty::Bottom | Ty::Inter(..) | Ty::Recursive(..) => {
//             panic!("Not a simple type")
//         }
//     }
// }

// pub(crate) fn freshen_above(context: &Context, ty: &Ty, lim: usize, lvl: usize) -> Ty {
//     freshen(context, ty, lim, lvl, &mut HashMap::new())
// }

// fn freshen(
//     context: &Context,
//     ty: &Ty,
//     lim: usize,
//     lvl: usize,
//     freshened: &mut HashMap<Var, Var>,
// ) -> Ty {
//     if ty.level() <= lim {
//         return ty.clone();
//     }
//     match ty {
//         Ty::Var(var) => Ty::Var(freshened.get(var).cloned().unwrap_or_else(|| {
//             let new_v = fresh_var(lvl);
//             let lower = var
//                 .lower_bounds
//                 .borrow()
//                 .iter()
//                 .map(|ty| freshen(context, ty, lim, lvl, freshened))
//                 .collect();
//             let upper = var
//                 .upper_bounds
//                 .borrow()
//                 .iter()
//                 .map(|ty| freshen(context, ty, lim, lvl, freshened))
//                 .collect();
//             *new_v.lower_bounds.borrow_mut() = lower;
//             *new_v.upper_bounds.borrow_mut() = upper;
//             freshened.insert(var.clone(), new_v.clone());
//             new_v
//         })),
//         Ty::Function(ty1, ty2) => {
//             let left = freshen(context, ty1, lim, lvl, freshened);
//             let right = freshen(context, ty2, lim, lvl, freshened);
//             Ty::Function(Box::new(left), Box::new(right))
//         }
//         Ty::List(list) => Ty::List(
//             list.iter()
//                 .map(|t| freshen(context, t, lim, lvl, freshened))
//                 .collect(),
//         ),
//         Ty::Record(rc) => Ty::Record(
//             rc.iter()
//                 .map(|(name, ty)| (name.clone(), freshen(context, ty, lim, lvl, freshened)))
//                 .collect(),
//         ),
//         Ty::Pattern(pat, wildcart) => Ty::Pattern(
//             pat.iter()
//                 .map(|(name, (ty, opt))| {
//                     (
//                         name.clone(),
//                         (
//                             freshen(context, ty, lim, lvl, freshened),
//                             opt.as_ref()
//                                 .map(|opt| freshen(context, opt, lim, lvl, freshened)),
//                         ),
//                     )
//                 })
//                 .collect(),
//             *wildcart,
//         ),

//         Ty::Optional(opt) => Ty::Optional(Box::new(freshen(context, opt, lim, lvl, freshened))),
//         Ty::Number | Ty::Bool | Ty::String | Ty::Path | Ty::Null | Ty::Undefined => ty.clone(),
//         Ty::Union(left, right) => Ty::Union(
//             Box::new(freshen(context, left, lim, lvl, freshened)),
//             Box::new(freshen(context, right, lim, lvl, freshened)),
//         ),
//         Ty::Top | Ty::Bottom | Ty::Inter(_, _) | Ty::Recursive(..) => {
//             unreachable!()
//         }
//     }
// }
