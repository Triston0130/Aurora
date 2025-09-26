use std::cmp::Ordering;
use std::convert::TryFrom;

use crate::ast::{BinaryOp, Expr, ExprKind, Literal, PathExpr, UnaryOp};

#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    Int(i128),
    Float(f64),
    Bool(bool),
    String(String),
    Char(char),
    Ident(String),
    Unit,
    Tuple(Vec<ConstValue>),
    Array(Vec<ConstValue>),
}

impl ConstValue {
    pub fn as_string(&self) -> Option<String> {
        match self {
            ConstValue::Int(value) => Some(value.to_string()),
            ConstValue::Float(value) => Some(float_to_string(*value)),
            ConstValue::Bool(value) => Some(value.to_string()),
            ConstValue::String(value) => Some(value.clone()),
            ConstValue::Char(value) => Some(value.to_string()),
            ConstValue::Ident(value) => Some(value.clone()),
            ConstValue::Unit => Some("()".into()),
            ConstValue::Tuple(_) | ConstValue::Array(_) => None,
        }
    }

    fn as_int(&self) -> Option<i128> {
        match self {
            ConstValue::Int(value) => Some(*value),
            _ => None,
        }
    }

    fn as_float(&self) -> Option<f64> {
        match self {
            ConstValue::Float(value) => Some(*value),
            ConstValue::Int(value) => Some(*value as f64),
            _ => None,
        }
    }

    fn as_bool(&self) -> Option<bool> {
        match self {
            ConstValue::Bool(value) => Some(*value),
            _ => None,
        }
    }
}

pub fn eval(expr: &Expr) -> Option<ConstValue> {
    match &expr.kind {
        ExprKind::Literal(lit) => Some(match lit {
            Literal::Integer(value) => ConstValue::Int(parse_int_literal(value)?),
            Literal::Float(value) => ConstValue::Float(parse_float_literal(value)?),
            Literal::String(value) => ConstValue::String(value.clone()),
            Literal::Char(value) => ConstValue::Char(*value),
            Literal::Boolean(value) => ConstValue::Bool(*value),
            Literal::Unit => ConstValue::Unit,
        }),
        ExprKind::Path(path) => Some(ConstValue::Ident(path_to_string(path)?)),
        ExprKind::Unary { op, expr } => {
            let value = eval(expr)?;
            apply_unary(*op, value)
        }
        ExprKind::Binary { op, left, right } => {
            let lhs = eval(left)?;
            let rhs = eval(right)?;
            apply_binary(*op, lhs, rhs)
        }
        ExprKind::Block(block) => {
            if block.statements.is_empty() {
                if let Some(tail) = &block.tail {
                    eval(tail)
                } else {
                    Some(ConstValue::Unit)
                }
            } else {
                None
            }
        }
        ExprKind::Tuple(elems) => {
            if elems.is_empty() {
                Some(ConstValue::Unit)
            } else {
                let mut values = Vec::new();
                for elem in elems {
                    values.push(eval(elem)?);
                }
                Some(ConstValue::Tuple(values))
            }
        }
        ExprKind::Array(elems) => {
            let mut values = Vec::new();
            for elem in elems {
                values.push(eval(elem)?);
            }
            Some(ConstValue::Array(values))
        }
        _ => None,
    }
}

pub fn apply_unary(op: UnaryOp, value: ConstValue) -> Option<ConstValue> {
    match (op, value) {
        (UnaryOp::Neg, ConstValue::Int(int)) => Some(ConstValue::Int(-int)),
        (UnaryOp::Neg, ConstValue::Float(float)) => Some(ConstValue::Float(-float)),
        (UnaryOp::Not, ConstValue::Bool(flag)) => Some(ConstValue::Bool(!flag)),
        (UnaryOp::Ref, _) | (UnaryOp::RefMut, _) | (UnaryOp::Deref, _) => None,
        _ => None,
    }
}

pub fn apply_binary(op: BinaryOp, lhs: ConstValue, rhs: ConstValue) -> Option<ConstValue> {
    match op {
        BinaryOp::Add => {
            if let (Some(l), Some(r)) = (lhs.as_int(), rhs.as_int()) {
                return Some(ConstValue::Int(l + r));
            }
            if let (Some(l), Some(r)) = (lhs.as_float(), rhs.as_float()) {
                return Some(ConstValue::Float(l + r));
            }
            match (&lhs, &rhs) {
                (ConstValue::String(l), ConstValue::String(r)) => {
                    let mut combined = l.clone();
                    combined.push_str(r);
                    Some(ConstValue::String(combined))
                }
                (ConstValue::String(l), ConstValue::Char(r)) => {
                    let mut combined = l.clone();
                    combined.push(*r);
                    Some(ConstValue::String(combined))
                }
                (ConstValue::Char(l), ConstValue::String(r)) => {
                    let mut combined = l.to_string();
                    combined.push_str(r);
                    Some(ConstValue::String(combined))
                }
                _ => None,
            }
        }
        BinaryOp::Sub => apply_numeric(op, lhs, rhs),
        BinaryOp::Mul => apply_numeric(op, lhs, rhs),
        BinaryOp::Div => apply_numeric(op, lhs, rhs),
        BinaryOp::Rem => apply_numeric(op, lhs, rhs),
        BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor | BinaryOp::Shl | BinaryOp::Shr => {
            let l = lhs.as_int()?;
            let r = rhs.as_int()?;
            let result = match op {
                BinaryOp::BitAnd => l & r,
                BinaryOp::BitOr => l | r,
                BinaryOp::BitXor => l ^ r,
                BinaryOp::Shl => {
                    let shift = u32::try_from(r).ok()?;
                    l.checked_shl(shift)?
                }
                BinaryOp::Shr => {
                    let shift = u32::try_from(r).ok()?;
                    l.checked_shr(shift)?
                }
                _ => unreachable!(),
            };
            Some(ConstValue::Int(result))
        }
        BinaryOp::And => Some(ConstValue::Bool(lhs.as_bool()? && rhs.as_bool()?)),
        BinaryOp::Or => Some(ConstValue::Bool(lhs.as_bool()? || rhs.as_bool()?)),
        BinaryOp::Eq => Some(ConstValue::Bool(lhs == rhs)),
        BinaryOp::NotEq => Some(ConstValue::Bool(lhs != rhs)),
        BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
            if let (Some(l), Some(r)) = (lhs.as_float(), rhs.as_float()) {
                Some(ConstValue::Bool(match op {
                    BinaryOp::Lt => l < r,
                    BinaryOp::Le => l <= r,
                    BinaryOp::Gt => l > r,
                    BinaryOp::Ge => l >= r,
                    _ => unreachable!(),
                }))
            } else if let (ConstValue::String(l), ConstValue::String(r)) = (&lhs, &rhs) {
                let ordering = l.cmp(r);
                Some(ConstValue::Bool(match op {
                    BinaryOp::Lt => ordering == Ordering::Less,
                    BinaryOp::Le => ordering != Ordering::Greater,
                    BinaryOp::Gt => ordering == Ordering::Greater,
                    BinaryOp::Ge => ordering != Ordering::Less,
                    _ => unreachable!(),
                }))
            } else {
                None
            }
        }
    }
}

fn apply_numeric(op: BinaryOp, lhs: ConstValue, rhs: ConstValue) -> Option<ConstValue> {
    if let (Some(l), Some(r)) = (lhs.as_int(), rhs.as_int()) {
        let result = match op {
            BinaryOp::Sub => l - r,
            BinaryOp::Mul => l * r,
            BinaryOp::Div => {
                if r == 0 {
                    return None;
                }
                l / r
            }
            BinaryOp::Rem => {
                if r == 0 {
                    return None;
                }
                l % r
            }
            _ => unreachable!(),
        };
        return Some(ConstValue::Int(result));
    }

    let l = lhs.as_float()?;
    let r = rhs.as_float()?;
    match op {
        BinaryOp::Sub => Some(ConstValue::Float(l - r)),
        BinaryOp::Mul => Some(ConstValue::Float(l * r)),
        BinaryOp::Div => {
            if r == 0.0 {
                None
            } else {
                Some(ConstValue::Float(l / r))
            }
        }
        BinaryOp::Rem => None,
        _ => None,
    }
}

fn path_to_string(path: &PathExpr) -> Option<String> {
    if path.segments.iter().any(|seg| !seg.generics.is_empty()) {
        return None;
    }
    let mut result = String::new();
    if path.leading_colon {
        result.push_str("::");
    }
    let mut first = true;
    for segment in &path.segments {
        if !first {
            result.push_str("::");
        }
        first = false;
        result.push_str(&segment.ident);
    }
    Some(result)
}

fn parse_int_literal(raw: &str) -> Option<i128> {
    raw.replace('_', "").parse::<i128>().ok()
}

fn parse_float_literal(raw: &str) -> Option<f64> {
    raw.replace('_', "").parse::<f64>().ok()
}

fn float_to_string(value: f64) -> String {
    let mut s = value.to_string();
    if !s.contains('.') && !s.contains('e') && !s.contains('E') {
        s.push_str(".0");
    }
    s
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Block, Expr, ExprKind, Literal};
    use crate::span::Span;

    #[test]
    fn evaluates_integer_expression() {
        let expr = Expr::dummy(ExprKind::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expr::literal(Literal::Integer("4".into()))),
            right: Box::new(Expr::literal(Literal::Integer("2".into()))),
        });
        assert_eq!(eval(&expr), Some(ConstValue::Int(6)));
    }

    #[test]
    fn evaluates_string_literal() {
        let expr = Expr::literal(Literal::String("hello".into()));
        assert_eq!(eval(&expr), Some(ConstValue::String("hello".into())));
    }

    #[test]
    fn evaluates_block_tail() {
        let block = Block {
            statements: Vec::new(),
            tail: Some(Box::new(Expr::literal(Literal::Boolean(true)))),
            span: Span::default(),
        };
        let expr = Expr::dummy(ExprKind::Block(block));
        assert_eq!(eval(&expr), Some(ConstValue::Bool(true)));
    }
}
