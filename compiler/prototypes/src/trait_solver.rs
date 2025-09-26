use std::collections::HashMap;

use crate::constraints::TraitConstraint;
use crate::diagnostics::{primary_label, Diagnostic};
use crate::types::Type;

#[derive(Debug, Clone, Default)]
pub struct TraitEnvironment {
    impls: HashMap<String, Vec<Type>>,
}

impl TraitEnvironment {
    pub fn new() -> Self {
        let mut env = Self::default();
        env.install_prelude();
        env
    }

    fn install_prelude(&mut self) {
        use crate::types::PrimitiveType;

        let display_types = vec![
            Type::Primitive(PrimitiveType::Int32),
            Type::Primitive(PrimitiveType::Int64),
            Type::Primitive(PrimitiveType::Float32),
            Type::Primitive(PrimitiveType::Float64),
            Type::Primitive(PrimitiveType::Bool),
            Type::Primitive(PrimitiveType::String),
            Type::Primitive(PrimitiveType::Unit),
        ];

        for ty in display_types {
            self.add_impl(vec!["Display".into()], ty.clone());
            self.add_impl(vec!["Debug".into()], ty);
        }
    }

    pub fn add_impl(&mut self, trait_path: Vec<String>, ty: Type) {
        let key = trait_key(&trait_path);
        let entry = self.impls.entry(key).or_default();
        if !entry.iter().any(|existing| existing == &ty) {
            entry.push(ty);
        }
    }

    pub fn supports(&self, trait_path: &[String], ty: &Type) -> bool {
        self.impls
            .get(&trait_key(trait_path))
            .map(|types| types.iter().any(|existing| existing == ty))
            .unwrap_or(false)
    }

    pub fn merge(&mut self, other: TraitEnvironment) {
        for (key, types) in other.impls {
            for ty in types {
                let entry = self.impls.entry(key.clone()).or_default();
                if !entry.iter().any(|existing| existing == &ty) {
                    entry.push(ty);
                }
            }
        }
    }
}

pub struct TraitSolver<'env> {
    env: &'env TraitEnvironment,
}

impl<'env> TraitSolver<'env> {
    pub fn new(env: &'env TraitEnvironment) -> Self {
        Self { env }
    }

    pub fn solve(&self, constraints: &[TraitConstraint]) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        for constraint in constraints {
            let trait_path = constraint.trait_path.clone();
            let subject = constraint.subject.clone();
            if matches!(subject, Type::Var(_)) {
                continue;
            }
            if !self.env.supports(&trait_path, &subject) {
                let mut diagnostic = Diagnostic::error(format!(
                    "type `{}` does not implement `{}`",
                    fmt_type(&subject),
                    trait_path.join("::")
                ))
                .with_label(primary_label(
                    constraint.span,
                    "required trait bound not satisfied",
                ));
                if let Some(function) = &constraint.function {
                    diagnostic =
                        diagnostic.with_note(format!("required by function `{}`", function));
                }
                diagnostics.push(diagnostic);
            }
        }
        diagnostics
    }
}

fn fmt_type(ty: &Type) -> String {
    match ty {
        Type::Primitive(p) => format!("{:?}", p),
        Type::Var(id) => format!("T{}", id.0),
        Type::Tuple(elems) => {
            let inner = elems.iter().map(fmt_type).collect::<Vec<_>>();
            format!("({})", inner.join(", "))
        }
        Type::Function { .. } => "fn".to_string(),
        Type::Reference { inner, .. } => format!("&{}", fmt_type(inner)),
        Type::RawPointer { inner, .. } => format!("*{}", fmt_type(inner)),
        Type::TraitObject { path, .. } => path.join("::"),
    }
}

fn trait_key(path: &[String]) -> String {
    path.join("::")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        Attribute, AttributeArg, Block, Expr, FunctionDecl, FunctionSig, GenericParams, Item,
        Literal, Module, NodeId, PathExpr, PathSegment, PathType, TypeExpr, Visibility,
        WherePredicate, ZoneDecl,
    };
    use crate::constraints::Generator;
    use crate::hir::HirBuilder;
    use crate::span::Span;
    use crate::types::{InferenceContext, TypeEnv};

    fn render_module() -> Module {
        Module {
            id: NodeId::new(0),
            span: Span::default(),
            docs: Vec::new(),
            items: vec![Item::Function(FunctionDecl {
                attributes: vec![Attribute {
                    name: "zone".into(),
                    args: vec![AttributeArg::Expr(Expr::path(PathExpr {
                        leading_colon: false,
                        segments: vec![PathSegment {
                            ident: "gpu".into(),
                            generics: Vec::new(),
                        }],
                        span: Span::default(),
                    }))],
                    span: Span::default(),
                }],
                docs: Vec::new(),
                visibility: crate::ast::Visibility::Private,
                signature: FunctionSig {
                    constness: false,
                    asyncness: false,
                    name: "render".into(),
                    generics: GenericParams::default(),
                    params: vec![crate::ast::Param {
                        name: "value".into(),
                        ty: crate::ast::TypeExpr::Path(PathType {
                            leading_colon: false,
                            segments: vec![PathSegment {
                                ident: "Int32".into(),
                                generics: Vec::new(),
                            }],
                        }),
                    }],
                    return_type: Some(crate::ast::TypeExpr::Unit),
                    effect: None,
                    where_clause: vec![crate::ast::WherePredicate::Bound {
                        target: crate::ast::TypeExpr::Path(PathType {
                            leading_colon: false,
                            segments: vec![PathSegment {
                                ident: "Int32".into(),
                                generics: Vec::new(),
                            }],
                        }),
                        bounds: vec![PathType {
                            leading_colon: false,
                            segments: vec![PathSegment {
                                ident: "GPUKernel".into(),
                                generics: Vec::new(),
                            }],
                        }],
                    }],
                },
                body: Block {
                    statements: Vec::new(),
                    tail: Some(Box::new(Expr::literal(Literal::Unit))),
                    span: Span::default(),
                },
                span: Span::default(),
                id: NodeId::new(1),
            })],
        }
    }

    #[test]
    fn trait_solver_reports_missing_impls() {
        let module = render_module();
        let mut env = TypeEnv::new();
        env.push_scope();
        let mut ctx = InferenceContext::default();
        let generator = Generator::new(&mut env, &mut ctx);
        let analysis = generator.generate_module(&module);
        let solved = analysis.solve().expect("analysis solved");
        let builder = HirBuilder::new(&solved);
        let _ = builder.build_module(&module);

        let solver = TraitSolver::new(&solved.trait_env);
        let diagnostics = solver.solve(&solved.trait_constraints);
        assert_eq!(diagnostics.len(), 2);
        assert!(diagnostics
            .iter()
            .any(|diag| diag.message.contains("GPUKernel")));
        assert!(diagnostics
            .iter()
            .any(|diag| diag.message.contains("ZoneCapability::gpu")));
    }

    #[test]
    fn zone_capability_impl_is_visible_to_trait_solver() {
        let mut next_id = 200u32;
        let mut fresh_id = || {
            let id = NodeId::new(next_id);
            next_id += 1;
            id
        };

        let module = Module {
            id: fresh_id(),
            span: Span::default(),
            docs: Vec::new(),
            items: vec![
                Item::Zone(ZoneDecl {
                    attributes: vec![Attribute {
                        name: "doc".into(),
                        args: vec![AttributeArg::Expr(Expr::literal(Literal::String(
                            "GPU kernel zone".into(),
                        )))],
                        span: Span::default(),
                    }],
                    docs: Vec::new(),
                    name: "gpu".into(),
                    args: Vec::new(),
                    body: Block {
                        statements: Vec::new(),
                        tail: None,
                        span: Span::default(),
                    },
                    span: Span::default(),
                    id: fresh_id(),
                }),
                Item::Function(FunctionDecl {
                    attributes: vec![Attribute {
                        name: "zone".into(),
                        args: vec![AttributeArg::Expr(Expr::path(PathExpr {
                            leading_colon: false,
                            segments: vec![PathSegment {
                                ident: "gpu".into(),
                                generics: Vec::new(),
                            }],
                            span: Span::default(),
                        }))],
                        span: Span::default(),
                    }],
                    docs: Vec::new(),
                    visibility: Visibility::Private,
                    signature: FunctionSig {
                        constness: false,
                        asyncness: false,
                        name: "use_gpu".into(),
                        generics: GenericParams::default(),
                        params: vec![],
                        return_type: Some(TypeExpr::Unit),
                        effect: None,
                        where_clause: vec![WherePredicate::Bound {
                            target: TypeExpr::Path(PathType {
                                leading_colon: false,
                                segments: vec![
                                    PathSegment {
                                        ident: "Zone".into(),
                                        generics: Vec::new(),
                                    },
                                    PathSegment {
                                        ident: "gpu".into(),
                                        generics: Vec::new(),
                                    },
                                ],
                            }),
                            bounds: vec![PathType {
                                leading_colon: false,
                                segments: vec![
                                    PathSegment {
                                        ident: "ZoneCapability".into(),
                                        generics: Vec::new(),
                                    },
                                    PathSegment {
                                        ident: "gpu".into(),
                                        generics: Vec::new(),
                                    },
                                ],
                            }],
                        }],
                    },
                    body: Block {
                        statements: Vec::new(),
                        tail: Some(Box::new(Expr::literal(Literal::Unit))),
                        span: Span::default(),
                    },
                    span: Span::default(),
                    id: fresh_id(),
                }),
            ],
        };

        let mut env = TypeEnv::new();
        env.push_scope();
        let mut ctx = InferenceContext::default();
        let generator = Generator::new(&mut env, &mut ctx);
        let analysis = generator.generate_module(&module);
        let solved = analysis.solve().expect("analysis solved");
        let diagnostics = TraitSolver::new(&solved.trait_env).solve(&solved.trait_constraints);
        assert!(
            diagnostics.is_empty(),
            "zone capability impl should satisfy constraints"
        );
    }
}
