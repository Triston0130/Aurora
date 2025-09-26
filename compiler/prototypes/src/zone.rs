//! Utilities for converting parsed attributes into zone descriptors.

use crate::ast::{Attribute, AttributeArg, Expr};
use crate::const_eval;
use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ZoneDescriptor {
    pub zone_type: String,
    pub params: Vec<ZoneParam>,
    pub origin: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ZoneParam {
    Ident(String),
    KeyValue { key: String, value: String },
}

impl ZoneDescriptor {
    pub fn from_attribute(attr: &Attribute) -> Option<Self> {
        if attr.name != "zone" {
            return None;
        }
        let mut args_iter = attr.args.iter();
        let zone_type = match args_iter.next() {
            Some(AttributeArg::Expr(expr)) => expr_to_simple_string(expr)?,
            Some(AttributeArg::KeyValue { key, value }) if key == "kind" => {
                expr_to_simple_string(value)?
            }
            None => "default".to_owned(),
            _ => return None,
        };
        let mut params = Vec::new();
        for arg in attr.args.iter().skip(1) {
            match arg {
                AttributeArg::Expr(expr) => {
                    let value = expr_to_simple_string(expr)?;
                    params.push(ZoneParam::Ident(value));
                }
                AttributeArg::KeyValue { key, value } => {
                    let value = expr_to_simple_string(value)?;
                    params.push(ZoneParam::KeyValue {
                        key: key.clone(),
                        value,
                    });
                }
            }
        }
        Some(ZoneDescriptor {
            zone_type,
            params,
            origin: attr.span,
        })
    }
}

pub(crate) fn expr_to_simple_string(expr: &Expr) -> Option<String> {
    const_eval::eval(expr)?.as_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinaryOp, Expr, ExprKind, Literal, PathExpr};

    fn attr(name: &str, args: Vec<AttributeArg>) -> Attribute {
        Attribute {
            name: name.to_string(),
            args,
            span: Span::default(),
        }
    }

    #[test]
    fn descriptor_from_zone_attribute() {
        let attribute = attr(
            "zone",
            vec![
                AttributeArg::Expr(expr_ident("gpu")),
                AttributeArg::KeyValue {
                    key: "memory".into(),
                    value: Expr::literal(Literal::String("256MB".into())),
                },
            ],
        );
        let descriptor = ZoneDescriptor::from_attribute(&attribute).expect("zone descriptor");
        assert_eq!(descriptor.zone_type, "gpu");
        assert_eq!(
            descriptor.params,
            vec![ZoneParam::KeyValue {
                key: "memory".into(),
                value: "256MB".into(),
            }]
        );
        assert_eq!(descriptor.origin, Span::default());
    }

    #[test]
    fn zone_attribute_constant_folding() {
        let attribute = attr(
            "zone",
            vec![
                AttributeArg::Expr(expr_ident("realtime")),
                AttributeArg::KeyValue {
                    key: "deadline_ms".into(),
                    value: Expr::dummy(ExprKind::Binary {
                        op: BinaryOp::Mul,
                        left: Box::new(Expr::literal(Literal::Integer("20".into()))),
                        right: Box::new(Expr::literal(Literal::Integer("50".into()))),
                    }),
                },
            ],
        );
        let descriptor = ZoneDescriptor::from_attribute(&attribute).expect("zone descriptor");
        assert_eq!(
            descriptor.params,
            vec![ZoneParam::KeyValue {
                key: "deadline_ms".into(),
                value: "1000".into(),
            }]
        );
        assert_eq!(descriptor.origin, Span::default());
    }

    #[test]
    fn non_zone_attribute_returns_none() {
        let attribute = attr(
            "doc",
            vec![AttributeArg::Expr(Expr::literal(Literal::String(
                "hello".into(),
            )))],
        );
        assert!(ZoneDescriptor::from_attribute(&attribute).is_none());
    }

    fn expr_ident(name: &str) -> Expr {
        Expr::path(PathExpr {
            leading_colon: false,
            segments: vec![crate::ast::PathSegment {
                ident: name.into(),
                generics: Vec::new(),
            }],
            span: Span::default(),
        })
    }
}
