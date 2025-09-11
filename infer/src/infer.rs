use crate::{
    add_diag,
    module::{Bindings, Expr, ExprData, NameId},
    parreaux::{constrain, freshen_above},
    types::{PolarVar, PolymorphicType, Ty, Var},
    Diagnostic, InferError,
};
use itertools::{Either, Itertools};
use parser2::ast::BinOp;
use std::collections::{HashMap, HashSet};

pub type Scope = Vec<(NameId, ContextType)>;

#[derive(Debug, Clone, PartialEq)]
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
    pub bindings: Vec<Scope>,
}

impl<'a> Context<'a> {
    /// Create a new scope and run a function with it.
    pub(crate) fn with_scope<T>(
        &self,
        db: &dyn salsa::Database,
        scope: Vec<(NameId, ContextType)>,
        f: impl FnOnce(Vec<Scope>) -> T,
    ) -> T {
        let mut new = self.bindings(db).clone();
        new.push(scope);
        f(new)
    }

    /// Lookup a [Var] by it's name.
    pub(crate) fn lookup(&self, db: &dyn salsa::Database, name: NameId) -> Option<ContextType> {
        for scope in self.bindings(db).iter().rev() {
            for (item_name, item) in scope.iter().rev() {
                if *item_name == name {
                    return Some(item.clone());
                }
            }
        }
        None
    }
}

/// Infer the type of an expression.
#[salsa::tracked]
fn type_term<'a>(
    db: &'a dyn salsa::Database,
    expr: Expr<'a>, // Should this be ExprId?
    ctx: Context<'a>,
    with: &'a [Ty],
    lvl: usize,
    count: usize,
) -> Option<Ty> {
    use Ty::*;

    Ok(match expr.data(db) {
        ExprData::Reference(smol_str) => {
            let name = todo!();
            if let Some(var) = ctx.lookup(db, name) {
                return Ok(var.instantiate(&ctx, lvl));
            }

            // Handle with-statement which could be used to supply vars
            if let Some(with) = with {
                if let ty @ Ty::Var(var) = with {
                    if let Some(rec) = var.as_record() {
                        if let Some(ty) = rec.get(name).cloned() {
                            return Ok(ty);
                        }
                    } else {
                        let res = Ty::Var(ctx.fresh_var(lvl));
                        constrain(
                            ctx,
                            ty,
                            &Ty::Record([(name.to_string(), res.clone())].into()),
                        )
                        .map_err(|e| e.span(span))?;
                        return Ok(res);
                    }
                }
            }

            Err(InferError::UnknownIdentifier.span(span))
        }

        ExprData::UnaryOp { rhs, .. } => type_term(ctx, rhs, lvl),

        ExprData::BinaryOp { op, lhs, rhs, span } => {
            match op {
                BinOp::HasAttribute => {
                    let ty1 = type_term(ctx, lhs, lvl)?;
                    let name = rhs
                        .as_identifier_str()
                        .map_err(|e| e.span(rhs.get_span()))?;
                    if let Ty::Var(_) = &ty1 {
                        constrain(
                            ctx,
                            &ty1,
                            &Record([(name, Ty::Optional(Box::new(Ty::Undefined)))].into()),
                        )
                        .map_err(|e| e.span(lhs.get_span()))?;
                    };
                    return Ok(Bool);
                }
                BinOp::AttributeSelection => {
                    let ty = type_term(ctx, lhs, lvl)?;
                    let name = rhs
                        .as_identifier_str()
                        .map_err(|e| e.span(rhs.get_span()))?;

                    return match ty {
                        Ty::Var(_) => {
                            let res = Ty::Var(ctx.fresh_var(lvl));
                            constrain(ctx, &ty, &Ty::Record([(name, res.clone())].into()))
                                .map_err(|e| e.span(lhs.get_span()))?;
                            Ok(res)
                        }
                        Record(rc) => {
                            let name = rhs
                                .as_identifier_str()
                                .map_err(|e| e.span(rhs.get_span()))?;
                            if let Some(ty) = rc.get(&name) {
                                Ok(ty.clone())
                            } else {
                                Err(SpannedError {
                                    error: InferError::MissingRecordField { field: name },
                                    span: span.clone(),
                                })
                            }
                        }
                        _ => Err(SpannedError {
                            error: InferError::TypeMismatch {
                                expected: TypeName::Record,
                                found: ty.get_name(),
                            },
                            span: lhs.get_span().clone(),
                        }),
                    };
                }
                _ => (),
            }
            let ty1 = type_term(ctx, lhs, lvl)?;
            let ty2 = type_term(ctx, rhs, lvl)?;

            match op {
                // Application etc.
                BinOp::Application => {
                    let res = Ty::Var(ctx.fresh_var(lvl));
                    constrain(
                        ctx,
                        &ty1,
                        &Ty::Function(Box::new(ty2), Box::new(res.clone())),
                    )
                    .map_err(|e| e.span(lhs.get_span()))?;
                    Ok(res)
                }

                // Object modifications
                BinOp::ListConcat => match (&ty1, &ty2) {
                    (Ty::List(l), Ty::List(l2)) => Ok(Ty::List([l.clone(), l2.clone()].concat())),
                    (var1 @ Ty::Var(v1), var2 @ Ty::Var(v2)) => {
                        constrain(ctx, var1, &Ty::List(vec![]))
                            .map_err(|e| e.span(lhs.get_span()))?;
                        constrain(ctx, var2, &Ty::List(vec![]))
                            .map_err(|e| e.span(rhs.get_span()))?;
                        Ok(Ty::List(
                            [
                                v1.as_list().unwrap_or_default(),
                                v2.as_list().unwrap_or_default(),
                            ]
                            .concat(),
                        ))
                    }

                    (Ty::List(l), var @ Ty::Var(_)) | (var @ Ty::Var(_), Ty::List(l)) => {
                        constrain(ctx, var, &Ty::List(vec![]))
                            .map_err(|e| e.span(rhs.get_span()))?;
                        Ok(Ty::List(l.clone()))
                    }
                    (Ty::List(_), ty2) => Err(SpannedError {
                        error: InferError::TypeMismatch {
                            expected: TypeName::List,
                            found: ty2.get_name(),
                        },
                        span: rhs.get_span().clone(),
                    }),
                    (ty1, _) => Err(SpannedError {
                        error: InferError::TypeMismatch {
                            expected: TypeName::List,
                            found: ty1.get_name(),
                        },
                        span: lhs.get_span().clone(),
                    }),
                },

                BinOp::Update => match (&ty1, &ty2) {
                    (Ty::Record(rc1), Ty::Record(rc2)) => {
                        let mut rc = rc1.clone();
                        rc.extend(rc2.clone());
                        Ok(Ty::Record(rc))
                    }

                    (Ty::Var(v1), Ty::Var(v2)) => {
                        constrain(ctx, &ty1, &Ty::Record(HashMap::new()))
                            .map_err(|e| e.span(lhs.get_span()))?;
                        constrain(ctx, &ty2, &Ty::Record(HashMap::new()))
                            .map_err(|e| e.span(rhs.get_span()))?;

                        let mut rc1 = v1.as_record().unwrap_or_default();
                        rc1.extend(v2.as_record().unwrap_or_default());
                        Ok(Ty::Record(rc1))
                    }

                    (Ty::Record(rc1), var @ Ty::Var(_)) | (var @ Ty::Var(_), Ty::Record(rc1)) => {
                        constrain(ctx, var, &Ty::Record(HashMap::new()))
                            .map_err(|e| e.span(rhs.get_span()))?;
                        Ok(Ty::Record(rc1.clone()))
                    }
                    (Ty::Record(_), ty2) => Err(SpannedError {
                        error: InferError::TypeMismatch {
                            expected: TypeName::Record,
                            found: ty2.get_name(),
                        },
                        span: rhs.get_span().clone(),
                    }),

                    (ty1, _) => Err(SpannedError {
                        error: InferError::TypeMismatch {
                            expected: TypeName::Record,
                            found: ty1.get_name(),
                        },
                        span: lhs.get_span().clone(),
                    }),
                },

                // Primitives
                BinOp::Mul | BinOp::Div | BinOp::Sub => {
                    constrain(ctx, &ty1, &Ty::Number).map_err(|e| e.span(lhs.get_span()))?;
                    constrain(ctx, &ty2, &Ty::Number).map_err(|e| e.span(rhs.get_span()))?;
                    Ok(Ty::Number)
                }
                BinOp::Add => {
                    match (&ty1, &ty2) {
                        (Ty::Number, Ty::Number) => Ok(Ty::Number),
                        (Ty::String | Ty::Path, Ty::String | Ty::Path) => Ok(Ty::String),
                        (Ty::Var(v1), Ty::Var(v2)) => {
                            // TODO: is this correct?
                            v1.lower_bounds
                                .borrow_mut()
                                .extend([Ty::Number, Ty::String, Ty::Path]);

                            v2.lower_bounds
                                .borrow_mut()
                                .extend([Ty::Number, Ty::String, Ty::Path]);

                            Ok(Ty::Union(
                                Box::new(Ty::Number),
                                Box::new(Ty::Union(Box::new(Ty::String), Box::new(Ty::Path))),
                            ))
                        }

                        (var @ Ty::Var(_), Ty::Number) | (Ty::Number, var @ Ty::Var(_)) => {
                            constrain(ctx, var, &Ty::Number).map_err(|e| e.span(lhs.get_span()))?;
                            Ok(Ty::Number)
                        }
                        (var @ Ty::Var(_), Ty::String) | (Ty::String, var @ Ty::Var(_)) => {
                            constrain(ctx, var, &Ty::String).map_err(|e| e.span(lhs.get_span()))?;
                            Ok(Ty::String)
                        }
                        (var @ Ty::Var(_), Ty::Path) | (Ty::Path, var @ Ty::Var(_)) => {
                            constrain(ctx, var, &Ty::Path).map_err(|e| e.span(lhs.get_span()))?;
                            Ok(Ty::Path)
                        }
                        (Ty::Number | Ty::Path | Ty::String, ty2) => Err(SpannedError {
                            error: InferError::TypeMismatch {
                                // TODO: this should be union of Number, String, Path
                                expected: TypeName::Number,
                                found: ty2.get_name(),
                            },
                            span: rhs.get_span().clone(),
                        }),
                        (ty1, _) => Err(SpannedError {
                            error: InferError::TypeMismatch {
                                // TODO: this should be union of Number, String, Path
                                expected: TypeName::Number,
                                found: ty1.get_name(),
                            },
                            span: lhs.get_span().clone(),
                        }),
                    }
                }

                // Misc
                BinOp::AttributeFallback => {
                    constrain(ctx, &ty1, &ty2).map_err(|e| e.span(lhs.get_span()))?;
                    constrain(ctx, &ty1, &ty2).map_err(|e| e.span(rhs.get_span()))?;
                    Ok(Ty::Union(Box::new(ty1), Box::new(ty2)))
                }

                // Comparisons
                BinOp::LessThan
                | BinOp::LessThanEqual
                | BinOp::GreaterThan
                | BinOp::GreaterThanEqual
                | BinOp::Equal
                | BinOp::NotEqual => match (&ty1, &ty2) {
                    (ty @ Ty::Var(_), _) | (_, ty @ Ty::Var(_)) => {
                        constrain(ctx, ty, &ty2).map_err(|e| e.span(lhs.get_span()))?;
                        Ok(Bool)
                    }
                    (ty1, ty2) if ty1 != ty2 => Err(SpannedError {
                        error: InferError::TypeMismatch {
                            expected: ty1.get_name(),
                            found: ty2.get_name(),
                        },
                        span: rhs.get_span().clone(),
                    }),
                    _ => Ok(Bool),
                },

                // Logical oprators
                BinOp::And | BinOp::Or | BinOp::Implication => {
                    constrain(ctx, &ty1, &Ty::Bool).map_err(|e| e.span(lhs.get_span()))?;
                    constrain(ctx, &ty2, &Ty::Bool).map_err(|e| e.span(rhs.get_span()))?;
                    Ok(Bool)
                }
                _ => panic!("unimplemented binary operator: {:?}", op),
            }
        }

        // Language constructs
        ExprData::AttrSet(Bindings {
            attrs,
            inherit,
            dynamics,
            is_recursive,
        }) => {
            if *is_recursive {
                let vars: Vec<_> = attrs
                    .iter()
                    .map(|(ident, _expr)| {
                        (
                            ident.name.to_string(),
                            ContextType::Type(Ty::Var(ctx.fresh_var(lvl))),
                        )
                    })
                    .collect();

                let mut inherits = load_inherit(ctx, span.clone(), lvl, inherit)?;
                inherits.extend(vars.clone());

                ctx.with_scope(inherits, db, |ctx| {
                    let names = vars
                        .into_iter()
                        .map(|(name, var)| (name, var.into_type().unwrap().into_var().unwrap()));
                    let expressions = attrs.iter().map(|(_, expr)| expr);

                    let (ok, errs): (Vec<_>, Vec<_>) = names
                        .into_iter()
                        .zip(expressions)
                        .map(move |((name, e_ty), rhs)| {
                            let ty = ctx.with_scope(
                                vec![(name.to_string(), ContextType::Type(Ty::Var(e_ty.clone())))],
                                |ctx| type_term(ctx, rhs, lvl + 1),
                            )?;
                            constrain(ctx, &ty, &Ty::Var(e_ty.clone()))
                                .map_err(|e| e.span(rhs.get_span()))?;
                            Ok((name, Ty::Var(e_ty)))
                        })
                        .partition_map(|r| match r {
                            Ok(v) => Either::Left(v),
                            Err(v) => Either::Right(v),
                        });

                    if errs.is_empty() {
                        Ok(Ty::Record(ok.into_iter().collect()))
                    } else {
                        add_diag(db, span, error);
                    }
                })
            } else {
                let mut vars: HashMap<_, _> = attrs
                    .iter()
                    .map(|(ident, expr)| {
                        (ident.name.to_string(), type_term(ctx, expr, lvl).unwrap())
                    })
                    .collect();
                let ok = load_inherit(ctx, span.clone(), lvl, inherit);
                vars.extend(
                    ok?.into_iter()
                        .map(|(name, ty)| (name.to_string(), ty.instantiate(ctx, lvl))),
                );
                Ok(Record(vars))
            }
        }

        ExprData::LetAttrset(Bindings {
            attrs,
            inherit,
            dynamics,
            is_recursive,
        }) => {
            let binds: Vec<_> = bindings
                .iter()
                .map(|(name, _)| {
                    (
                        name.name.to_string(),
                        ContextType::Type(Ty::Var(ctx.fresh_var(lvl + 1))),
                    )
                })
                .collect();

            let mut inherits = load_inherit(ctx, span.clone(), lvl, inherit)?;
            inherits.extend(binds.clone());
            let (ok, err): (Vec<_>, Vec<_>) = ctx
                .with_scope(inherits, |ctx| {
                    let names = binds
                        .into_iter()
                        .map(|(name, var)| (name, var.into_type().unwrap().into_var().unwrap()));
                    let expressions = bindings.iter().map(|(_, expr)| expr);

                    names
                        .into_iter()
                        .zip(expressions)
                        .map(move |((name, e_ty), rhs)| {
                            let ty = ctx.with_scope(
                                vec![(name.to_string(), ContextType::Type(Ty::Var(e_ty.clone())))],
                                |ctx| type_term(ctx, rhs, lvl + 1),
                            )?;
                            let bind = bindings.iter().find(|(n, _)| n.name == *name).unwrap();
                            bind.0.var.set(coalesc_type(ctx, &ty)).unwrap();
                            constrain(ctx, &ty, &Ty::Var(e_ty.clone()))
                                .map_err(|e| e.span(rhs.get_span()))?;
                            Ok((
                                name,
                                ContextType::PolymorhicType(PolymorphicType::new(
                                    Ty::Var(e_ty),
                                    lvl,
                                )),
                            ))
                        })
                        .collect_vec()
                })
                .into_iter()
                .partition_map(|r| match r {
                    Ok(v) => Either::Left(v),
                    Err(v) => Either::Right(v),
                });

            if !err.is_empty() {
                return Err(SpannedError {
                    error: InferError::MultipleErrors(err),
                    span: span.clone(),
                });
            }

            let ret = ctx.with_scope(ok, |ctx| type_term(ctx, body, lvl))?;
            Ok(ret)
        }

        ExprData::Lambda(name, pat, expr) => {
            let mut added = vec![];
            let ty = match pattern {
                crate::ast::Pattern::Record {
                    patterns,
                    is_wildcard,
                    name,
                } => {
                    let mut item = vec![];

                    for pattern in patterns {
                        match pattern {
                            PatternElement::Identifier(ident) => {
                                let var = Ty::Var(ctx.fresh_var(lvl));
                                item.push((ident.name.clone(), (var.clone(), None)));
                                added.push((ident.name.to_string(), ContextType::Type(var)));
                            }
                            PatternElement::DefaultIdentifier(name, expr) => {
                                let ty = type_term(ctx, expr, lvl)?;
                                let var = Ty::Var(ctx.fresh_var(lvl));
                                constrain(ctx, &var, &ty).map_err(|e| e.span(span))?;

                                item.push((name.name.clone(), (var.clone(), Some(ty))));
                                added.push((name.name.to_string(), ContextType::Type(var)));
                            }
                        }
                    }

                    let ty = Ty::Pattern(item.clone().into_iter().collect(), *is_wildcard);
                    if let Some(name) = name {
                        let var = ctx.fresh_var(lvl);
                        if *is_wildcard {
                            constrain(
                                ctx,
                                &Ty::Var(var.clone()),
                                &Ty::Record(
                                    item.into_iter()
                                        .map(|(name, (var, _))| (name, var))
                                        .collect(),
                                ),
                            )
                            .map_err(|e| e.span(span))?;
                        } else {
                            constrain(ctx, &Ty::Var(var.clone()), &ty).map_err(|e| e.span(span))?;
                        }
                        added.push((name.to_string(), ContextType::Type(Ty::Var(var))));
                    }
                    ty
                }
                crate::ast::Pattern::Identifier(Identifier { name, .. }) => {
                    let ty = Ty::Var(ctx.fresh_var(lvl));
                    added.push((name.to_string(), ContextType::Type(ty.clone())));
                    ty
                }
            };
            let ret = ctx.with_scope(added, |context| type_term(context, body, lvl))?;
            Ok(Function(Box::new(ty), Box::new(ret)))
        }

        ExprData::With(set, body) => {
            let ty = type_term(ctx, set, lvl)?;
            match ty {
                var @ Ty::Var(_) => {
                    ctx.set_with(var);
                    let ret = type_term(ctx, body, lvl);
                    ctx.remove_with();
                    ret
                }
                Ty::Record(rc) => ctx.with_scope(
                    rc.iter()
                        .filter(|(name, _)| ctx.with.is_some() || ctx.lookup(name).is_none())
                        .map(|(name, ty)| (name.to_string(), ContextType::Type(ty.clone())))
                        .collect(),
                    |ctx| type_term(ctx, body, lvl),
                ),
                _ => Err(SpannedError {
                    error: InferError::TypeMismatch {
                        expected: TypeName::Record,
                        found: ty.get_name(),
                    },
                    span: span.clone(),
                }),
            }
        }

        ExprData::Conditional(cond, expr1, expr2) => {
            let ty = type_term(ctx, condition, lvl)?;
            match ty {
                Ty::Var(_) => {
                    constrain(ctx, &ty, &Ty::Bool).map_err(|e| e.span(condition.get_span()))?;
                }
                Ty::Bool => (),
                _ => {
                    return Err(SpannedError {
                        error: InferError::TypeMismatch {
                            expected: TypeName::Bool,
                            found: ty.get_name(),
                        },
                        span: span.clone(),
                    })
                }
            }
            let ty1 = type_term(ctx, expr1, lvl)?;
            let ty2 = type_term(ctx, expr2, lvl)?;
            if ty1 != ty2 {
                Ok(Union(Box::new(ty1), Box::new(ty2)))
            } else {
                Ok(ty1)
            }
        }
        ExprData::Assert(cond, expr) => {
            let ty = type_term(ctx, condition, lvl)?;
            match ty {
                Ty::Bool => (),
                Ty::Var(_) => {
                    constrain(ctx, &ty, &Ty::Bool).map_err(|e| e.span(condition.get_span()))?;
                }
                _ => {
                    return Err(SpannedError {
                        error: InferError::TypeMismatch {
                            expected: TypeName::Bool,
                            found: ty.get_name(),
                        },
                        span: condition.get_span().clone(),
                    });
                }
            }

            type_term(ctx, expr, lvl)
        }

        ExprData::List(expr) => Ok({
            let expr = exprs
                .iter()
                .flat_map(|ast| type_term(ctx, ast, lvl))
                .collect_vec();

            if let Some(fst) = expr.first() {
                let homo = expr.iter().all(|r| r == fst);
                if homo {
                    return Ok(Ty::List(vec![fst.clone()]));
                }
            }

            Ty::List(expr)
        }),
        // Primitives
        // Ast::NixString(_) => Ok(String),
        // Ast::NixPath(_) => Ok(Path),
        // Ast::Null(_) => Ok(Null),
        // Ast::Bool { .. } => Ok(Bool),
        // Ast::Int { .. } | Ast::Float { .. } => Ok(Number),
        // Ast::Comment(_) | Ast::DocComment(_) | Ast::LineComment(_) => unimplemented!(),
    })
}

/// Load the bindings from an `inherit (x) arar` statement.
#[salsa::tracked]
fn load_inherit<'db>(
    db: &'db dyn salsa::Database,
    ctx: Context<'db>,
    lvl: usize,
    inherit: &[Inherit],
) -> Vec<(String, ContextType)> {
    let (ok, err): (Vec<_>, Vec<_>) = inherit
        .iter()
        .map(|Inherit { name, items }| {
            if let Some(expr) = name {
                let ty = type_term(ctx, expr, lvl)?;

                match &ty {
                    ty @ Ty::Var(_) => {
                        let vars = items
                            .iter()
                            .map(|(_span, name)| (name.to_string(), Ty::Var(ctx.fresh_var(lvl))))
                            .collect_vec();
                        let record = Ty::Record(vars.clone().into_iter().collect());

                        constrain(ctx, ty, &record).map_err(|e| e.span(expr.get_span()))?;

                        Ok(vars
                            .into_iter()
                            .map(|(name, ty)| (name, ContextType::Type(ty)))
                            .collect())
                    }
                    Ty::Record(rc_items) => {
                        let (ok, err): (Vec<_>, Vec<_>) = items
                            .iter()
                            .map(|(range, name)| {
                                Ok((
                                    name.to_string(),
                                    ContextType::Type(
                                        rc_items
                                            .get(name)
                                            .ok_or(
                                                InferError::MissingRecordField {
                                                    field: name.clone(),
                                                }
                                                .span(range),
                                            )?
                                            .clone(),
                                    ),
                                ))
                            })
                            .partition_map(|r| match r {
                                Ok(ty) => Either::Left(ty),
                                Err(e) => Either::Right(e),
                            });
                        if err.is_empty() {
                            Ok(ok)
                        } else {
                            Err(SpannedError {
                                error: InferError::MultipleErrors(err),
                                span: span.clone(),
                            })
                        }
                    }
                    _ => Err(SpannedError {
                        error: InferError::TypeMismatch {
                            expected: TypeName::Record,
                            found: ty.get_name(),
                        },
                        span: span.clone(),
                    }),
                }
            } else {
                let (ok, err): (Vec<_>, Vec<_>) = items
                    .iter()
                    .map(|(range, name)| {
                        Ok((
                            name.to_string(),
                            ctx.lookup(name)
                                .ok_or(InferError::UnknownIdentifier.span(range))?
                                .clone(),
                        ))
                    })
                    .partition_map(|r| match r {
                        Ok(v) => Either::Left(v),
                        Err(v) => Either::Right(v),
                    });

                if err.is_empty() {
                    Ok(ok)
                } else {
                    Err(SpannedError {
                        error: InferError::MultipleErrors(err),
                        span: span.clone(),
                    })
                }
            }
        })
        .partition_map(|r| match r {
            Ok(v) => Either::Left(v),
            Err(v) => Either::Right(v),
        });

    ok.into_iter().flatten().collect()
}

pub fn coalesc_type(context: &Context, ty: &Ty) -> Ty {
    coalesce_type_inner(context, ty, true, &mut HashMap::new(), HashSet::new())
}

fn coalesce_type_inner(
    context: &Context,
    ty: &Ty,
    polarity: bool,
    rec: &mut HashMap<PolarVar, Var>,
    mut processing: HashSet<PolarVar>,
) -> Ty {
    match ty {
        Ty::Number | Ty::Bool | Ty::String | Ty::Path | Ty::Null | Ty::Undefined => ty.clone(),
        tyvar @ Ty::Var(var) => {
            let pol_var = (var.clone(), polarity);
            if processing.contains(&pol_var) {
                return if let Some(var) = rec.get(&pol_var) {
                    Ty::Var(var.clone())
                } else {
                    rec.insert(pol_var.clone(), context.fresh_var(0));
                    tyvar.clone()
                };
            } else {
                let bounds = if polarity {
                    &var.lower_bounds
                } else {
                    &var.upper_bounds
                };
                processing.insert(pol_var.clone());
                let bound_types = bounds
                    .borrow()
                    .iter()
                    .map(|t| coalesce_type_inner(context, t, polarity, rec, processing.clone()))
                    .collect_vec();
                let res = if polarity {
                    bound_types
                        .into_iter()
                        .fold(tyvar.clone(), |a, b| Ty::Union(Box::new(a), Box::new(b)))
                } else {
                    bound_types
                        .into_iter()
                        .fold(tyvar.clone(), |a, b| Ty::Inter(Box::new(a), Box::new(b)))
                };
                if let Some(rec) = rec.get(&pol_var) {
                    Ty::Recursive(rec.clone(), Box::new(res))
                } else {
                    res
                }
            }
        }
        Ty::Function(l, r) => Ty::Function(
            Box::new(coalesce_type_inner(
                context,
                l,
                !polarity,
                rec,
                processing.clone(),
            )),
            Box::new(coalesce_type_inner(context, r, polarity, rec, processing)),
        ),
        Ty::List(l) => Ty::List(
            l.iter()
                .map(|t| coalesce_type_inner(context, t, polarity, rec, processing.clone()))
                .collect(),
        ),
        Ty::Record(r) => Ty::Record(
            r.iter()
                .map(|(n, t)| {
                    (
                        n.clone(),
                        coalesce_type_inner(context, t, polarity, rec, processing.clone()),
                    )
                })
                .collect(),
        ),
        Ty::Optional(o) => Ty::Optional(Box::new(coalesce_type_inner(
            context, o, polarity, rec, processing,
        ))),

        Ty::Union(u1, u2) => {
            let u1 = coalesce_type_inner(context, u1, polarity, rec, processing.clone());
            let u2 = coalesce_type_inner(context, u2, polarity, rec, processing);
            Ty::Union(Box::new(u1), Box::new(u2))
        }

        Ty::Pattern(elem, widcart) => {
            let mut elem = elem.clone();
            for (_, (ty, opt)) in elem.iter_mut() {
                *ty = coalesce_type_inner(context, ty, !polarity, rec, processing.clone());
                if let Some(opt) = opt {
                    *opt = coalesce_type_inner(context, opt, polarity, rec, processing.clone());
                }
            }
            Ty::Pattern(elem, *widcart)
        }

        Ty::Top | Ty::Bottom | Ty::Inter(_, _) | Ty::Recursive(_, _) => unreachable!(),
    }
}

/// Infer the type of an expression.
/// Insert constraints for all [Identifier]s on the way.
#[salsa::tracked]
pub fn infer<'db>(db: &'db dyn salsa::Database, expr: Expr<'db>) -> Vec<Diagnostic> {
    let mut context = Context::new(db, vec![]);
    type_term(db, expr, context, vec![], 0, 0);
}
