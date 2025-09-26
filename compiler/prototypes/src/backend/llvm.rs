use std::collections::HashMap;
use std::fmt::Write;

use crate::ast::{BinaryOp, Literal};
use crate::const_eval::ConstValue;
use crate::mir::{MirFunction, MirModule, MirOperand, MirRvalue, MirStatement, MirTerminator};
use crate::types::{PrimitiveType, Type};
use crate::zone::{ZoneDescriptor, ZoneParam};

#[derive(Debug, Clone)]
pub struct BackendOptions {
    pub cpu_triple: String,
    pub emit_gpu: bool,
}

impl Default for BackendOptions {
    fn default() -> Self {
        Self {
            cpu_triple: "x86_64-unknown-linux-gnu".into(),
            emit_gpu: true,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BackendArtifacts {
    pub cpu_module: String,
    pub gpu_modules: Vec<GpuModule>,
    pub realtime_metadata: Vec<RealtimeAnnotation>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GpuModule {
    pub function: String,
    pub zone: String,
    pub ir: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RealtimeAnnotation {
    pub function: String,
    pub zone: String,
    pub deadline_ms: Option<u32>,
}

pub fn emit_module(
    module: &MirModule,
    options: &BackendOptions,
    consts: &HashMap<String, ConstValue>,
) -> BackendArtifacts {
    let mut cpu_ir = String::new();
    writeln!(cpu_ir, "; ModuleID = 'aurora'").unwrap();
    writeln!(cpu_ir, "target triple = \"{}\"\n", options.cpu_triple).unwrap();

    let mut gpu_modules = Vec::new();
    let mut realtime_annotations = Vec::new();

    for function in &module.functions {
        let function_ir = emit_function(function);
        cpu_ir.push_str(&function_ir);
        cpu_ir.push('\n');

        if let Some(descriptor) = &function.zone {
            if descriptor.zone_type == "gpu" && options.emit_gpu {
                gpu_modules.push(GpuModule {
                    function: function.name.clone(),
                    zone: descriptor.zone_type.clone(),
                    ir: emit_gpu_stub(function, descriptor),
                });
            }
            if descriptor.zone_type == "realtime" {
                realtime_annotations.push(RealtimeAnnotation {
                    function: function.name.clone(),
                    zone: descriptor.zone_type.clone(),
                    deadline_ms: parse_realtime_deadline(descriptor),
                });
            }
        }
    }

    if !consts.is_empty() {
        writeln!(cpu_ir, "; const fn results").unwrap();
        for (name, value) in consts.iter() {
            writeln!(cpu_ir, ";   {} = {}", name, format_const_value(value)).unwrap();
        }
    }

    BackendArtifacts {
        cpu_module: cpu_ir.trim_end().to_string(),
        gpu_modules,
        realtime_metadata: realtime_annotations,
    }
}

fn emit_function(function: &MirFunction) -> String {
    let mut out = String::new();
    let ret_ty = llvm_type(&function.return_type);
    let params = function
        .params
        .iter()
        .map(|param| format!("{} %{}", llvm_type(&param.ty), param.name))
        .collect::<Vec<_>>()
        .join(", ");

    let mut attrs = Vec::new();
    if function.zone.is_some() {
        if let Some(zone) = &function.zone {
            attrs.push(format!("\"aurora.zone\"=\"{}\"", zone.zone_type));
        }
    }

    writeln!(
        out,
        "define dso_local {} @{}({}){} {{",
        ret_ty,
        function.name,
        params,
        if attrs.is_empty() {
            String::new()
        } else {
            " #0".to_string()
        }
    )
    .unwrap();

    let type_map = collect_type_map(function);

    for block in &function.body.blocks {
        writeln!(out, "bb{}:", block.id.0).unwrap();
        if !block.phi_nodes.is_empty() {
            for phi in &block.phi_nodes {
                let ty = type_map
                    .get(&phi.target)
                    .cloned()
                    .unwrap_or_else(|| "ptr".into());
                let sources = phi
                    .sources
                    .iter()
                    .map(|(block, operand)| {
                        format!("[ {}, %bb{} ]", emit_operand(operand), block.0)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                writeln!(out, "  %{} = phi {} {}", phi.target, ty, sources).unwrap();
            }
        }
        for stmt in &block.statements {
            match stmt {
                MirStatement::Assign { target, value } => {
                    let ty = type_map
                        .get(target)
                        .cloned()
                        .unwrap_or_else(|| infer_type_for_value(value, &type_map));
                    writeln!(
                        out,
                        "  %{} = {}",
                        target,
                        emit_rvalue(value, &ty, &type_map)
                    )
                    .unwrap();
                }
                MirStatement::Expr(value) => {
                    writeln!(out, "  {}", emit_rvalue(value, "void", &type_map)).unwrap();
                }
            }
        }
        writeln!(out, "  {}", emit_terminator(&block.terminator, &type_map)).unwrap();
        out.push('\n');
    }
    writeln!(out, "}}\n").unwrap();
    if !attrs.is_empty() {
        writeln!(out, "attributes #0 = {{{}}}\n", attrs.join(" ")).unwrap();
    }
    out
}

fn collect_type_map(function: &MirFunction) -> HashMap<String, String> {
    let mut map = HashMap::new();
    for param in &function.params {
        map.insert(param.name.clone(), llvm_type(&param.ty));
    }
    for local in &function.body.locals {
        let ty = local
            .ty
            .as_ref()
            .map(llvm_type)
            .unwrap_or_else(|| "ptr".into());
        map.insert(local.name.clone(), ty);
    }
    map
}

fn infer_type_for_value(value: &MirRvalue, type_map: &HashMap<String, String>) -> String {
    match value {
        MirRvalue::Literal(lit) => literal_type(lit),
        MirRvalue::Local(name) => type_map.get(name).cloned().unwrap_or_else(|| "ptr".into()),
        MirRvalue::Call { .. } => "ptr".into(),
        MirRvalue::Binary { left, .. } => operand_type(left, type_map),
    }
}

fn emit_rvalue(value: &MirRvalue, ty: &str, type_map: &HashMap<String, String>) -> String {
    match value {
        MirRvalue::Literal(lit) => emit_literal_expression(lit, ty),
        MirRvalue::Local(name) => format!("add {} 0, %{}", ty, name),
        MirRvalue::Call { callee, args } => {
            let args_str = args
                .iter()
                .map(|arg| {
                    let arg_ty = operand_type(arg, type_map);
                    format!("{} {}", arg_ty, emit_operand(arg))
                })
                .collect::<Vec<_>>()
                .join(", ");
            if ty == "void" {
                format!("call void @{}({})", callee, args_str)
            } else {
                format!("call {} @{}({})", ty, callee, args_str)
            }
        }
        MirRvalue::Binary { op, left, right } => emit_binary(op, left, right, ty, type_map),
    }
}

fn emit_binary(
    op: &BinaryOp,
    left: &MirOperand,
    right: &MirOperand,
    ty: &str,
    type_map: &HashMap<String, String>,
) -> String {
    let left_val = emit_operand(left);
    let right_val = emit_operand(right);
    let op_ty = match op_category(op) {
        BinaryCategory::Arithmetic | BinaryCategory::Bitwise => ty.to_string(),
        BinaryCategory::Comparison => operand_type(left, type_map),
    };
    match op_category(op) {
        BinaryCategory::Arithmetic => format!(
            "{} {} {}, {}",
            arithmetic_op(op),
            op_ty,
            left_val,
            right_val
        ),
        BinaryCategory::Bitwise => {
            format!("{} {} {}, {}", bitwise_op(op), op_ty, left_val, right_val)
        }
        BinaryCategory::Comparison => {
            format!("icmp {} {} {}, {}", cmp_op(op), op_ty, left_val, right_val)
        }
    }
}

fn arithmetic_op(op: &BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "add",
        BinaryOp::Sub => "sub",
        BinaryOp::Mul => "mul",
        BinaryOp::Div => "sdiv",
        BinaryOp::Rem => "srem",
        _ => "add",
    }
}

fn bitwise_op(op: &BinaryOp) -> &'static str {
    match op {
        BinaryOp::And => "and",
        BinaryOp::Or => "or",
        BinaryOp::BitAnd => "and",
        BinaryOp::BitOr => "or",
        BinaryOp::BitXor => "xor",
        BinaryOp::Shl => "shl",
        BinaryOp::Shr => "ashr",
        _ => "and",
    }
}

fn cmp_op(op: &BinaryOp) -> &'static str {
    match op {
        BinaryOp::Eq => "eq",
        BinaryOp::NotEq => "ne",
        BinaryOp::Lt => "slt",
        BinaryOp::Le => "sle",
        BinaryOp::Gt => "sgt",
        BinaryOp::Ge => "sge",
        _ => "eq",
    }
}

enum BinaryCategory {
    Arithmetic,
    Bitwise,
    Comparison,
}

fn op_category(op: &BinaryOp) -> BinaryCategory {
    match op {
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
            BinaryCategory::Arithmetic
        }
        BinaryOp::And
        | BinaryOp::Or
        | BinaryOp::BitAnd
        | BinaryOp::BitOr
        | BinaryOp::BitXor
        | BinaryOp::Shl
        | BinaryOp::Shr => BinaryCategory::Bitwise,
        _ => BinaryCategory::Comparison,
    }
}

fn emit_literal_expression(lit: &Literal, ty: &str) -> String {
    match lit {
        Literal::Integer(value) => format!("add {} 0, {}", ty, value),
        Literal::Float(value) => format!("fadd {} 0.0, {}", ty, value),
        Literal::Boolean(value) => {
            let rhs = if *value { "1" } else { "0" };
            format!("xor {} 0, {}", ty, rhs)
        }
        Literal::Unit => "call void @llvm.donothing()".into(),
        Literal::String(value) => {
            let sanitized = value.replace('"', "\\22");
            format!("call ptr @__aurora_string_literal(ptr c\"{}\")", sanitized)
        }
        Literal::Char(value) => format!("add {} 0, {}", ty, *value as u32),
    }
}

fn emit_operand(operand: &MirOperand) -> String {
    match operand {
        MirOperand::Literal(lit) => literal_value(lit),
        MirOperand::Local(name) => format!("%{}", name),
    }
}

fn operand_type(operand: &MirOperand, type_map: &HashMap<String, String>) -> String {
    match operand {
        MirOperand::Literal(lit) => literal_type(lit),
        MirOperand::Local(name) => type_map.get(name).cloned().unwrap_or_else(|| "ptr".into()),
    }
}

fn emit_terminator(terminator: &MirTerminator, type_map: &HashMap<String, String>) -> String {
    match terminator {
        MirTerminator::Return(Some(op)) => {
            let op_ty = operand_type(op, type_map);
            format!("ret {} {}", op_ty, emit_operand(op))
        }
        MirTerminator::Return(None) => "ret void".into(),
        MirTerminator::Jump { target } => format!("br label %bb{}", target.0),
        MirTerminator::Branch {
            cond,
            then_block,
            else_block,
        } => {
            let cond_ty = operand_type(cond, type_map);
            format!(
                "br {} {}, label %bb{}, label %bb{}",
                cond_ty,
                emit_operand(cond),
                then_block.0,
                else_block.0
            )
        }
    }
}

fn llvm_type(ty: &Type) -> String {
    match ty {
        Type::Primitive(PrimitiveType::Unit) => "void".into(),
        Type::Primitive(PrimitiveType::Bool) => "i1".into(),
        Type::Primitive(PrimitiveType::Int32) => "i32".into(),
        Type::Primitive(PrimitiveType::Int64) => "i64".into(),
        Type::Primitive(PrimitiveType::Float32) => "float".into(),
        Type::Primitive(PrimitiveType::Float64) => "double".into(),
        Type::Primitive(PrimitiveType::String) => "ptr".into(),
        _ => "ptr".into(),
    }
}

fn literal_type(lit: &Literal) -> String {
    match lit {
        Literal::Integer(_) => "i32".into(),
        Literal::Float(_) => "double".into(),
        Literal::Boolean(_) => "i1".into(),
        Literal::Unit => "void".into(),
        Literal::String(_) => "ptr".into(),
        Literal::Char(_) => "i32".into(),
    }
}

fn literal_value(lit: &Literal) -> String {
    match lit {
        Literal::Integer(value) => value.clone(),
        Literal::Float(value) => value.clone(),
        Literal::Boolean(value) => {
            if *value {
                "1".into()
            } else {
                "0".into()
            }
        }
        Literal::Unit => "void".into(),
        Literal::String(value) => format!("\"{}\"", value),
        Literal::Char(value) => format!("{}", *value as u32),
    }
}

fn format_const_value(value: &ConstValue) -> String {
    match value {
        ConstValue::Int(i) => i.to_string(),
        ConstValue::Float(f) => f.to_string(),
        ConstValue::Bool(b) => b.to_string(),
        ConstValue::String(s) => format!("\"{}\"", s),
        ConstValue::Char(c) => format!("'{}'", c),
        ConstValue::Ident(id) => id.clone(),
        ConstValue::Unit => "()".into(),
        ConstValue::Tuple(values) => {
            let elems: Vec<String> = values.iter().map(format_const_value).collect();
            format!("({})", elems.join(", "))
        }
        ConstValue::Array(values) => {
            let elems: Vec<String> = values.iter().map(format_const_value).collect();
            format!("[{}]", elems.join(", "))
        }
    }
}

fn emit_gpu_stub(function: &MirFunction, zone: &ZoneDescriptor) -> String {
    let mut stub = String::new();
    writeln!(
        stub,
        "; PTX placeholder for @{} (zone: {})",
        function.name, zone.zone_type
    )
    .unwrap();
    writeln!(stub, ".visible .entry {}() {{}}", function.name).unwrap();
    stub
}

fn parse_realtime_deadline(zone: &ZoneDescriptor) -> Option<u32> {
    for param in &zone.params {
        if let ZoneParam::KeyValue { key, value } = param {
            if key == "deadline_ms" {
                if let Ok(parsed) = value.parse::<u32>() {
                    return Some(parsed);
                }
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mir::{BlockId, ControlFlowGraph, DropSchedule, MirBlock, MirBody, MirTerminator};
    use aurora_effect_solver::EffectRow;
    use std::collections::HashMap;

    fn sample_function() -> MirFunction {
        MirFunction {
            name: "main".into(),
            params: vec![],
            return_type: Type::Primitive(PrimitiveType::Unit),
            effects: EffectRow::empty(),
            zone: None,
            body: MirBody {
                locals: vec![],
                entry: BlockId(0),
                blocks: vec![MirBlock {
                    id: BlockId(0),
                    phi_nodes: Vec::new(),
                    statements: Vec::new(),
                    terminator: MirTerminator::Return(None),
                    span: crate::span::Span::default(),
                }],
                cfg: ControlFlowGraph {
                    predecessors: vec![Vec::new()],
                    successors: vec![Vec::new()],
                },
                drop_schedule: DropSchedule {
                    per_block: vec![Vec::new()],
                },
            },
            trait_obligations: Vec::new(),
        }
    }

    #[test]
    fn emits_cpu_ir_for_simple_function() {
        let module = MirModule {
            functions: vec![sample_function()],
        };
        let artifacts = emit_module(&module, &BackendOptions::default(), &HashMap::new());
        assert!(artifacts
            .cpu_module
            .contains("define dso_local void @main()"));
        assert!(artifacts.cpu_module.contains("ret void"));
    }

    #[test]
    fn emits_gpu_stub_for_gpu_zone() {
        let mut gpu_function = sample_function();
        gpu_function.name = "kernel".into();
        gpu_function.zone = Some(ZoneDescriptor {
            zone_type: "gpu".into(),
            params: Vec::new(),
            origin: crate::span::Span::default(),
        });
        let module = MirModule {
            functions: vec![gpu_function],
        };
        let artifacts = emit_module(&module, &BackendOptions::default(), &HashMap::new());
        assert_eq!(artifacts.gpu_modules.len(), 1);
        assert!(artifacts.gpu_modules[0].ir.contains(".entry kernel"));
    }

    #[test]
    fn records_realtime_metadata() {
        let mut rt_function = sample_function();
        rt_function.zone = Some(ZoneDescriptor {
            zone_type: "realtime".into(),
            params: vec![ZoneParam::KeyValue {
                key: "deadline_ms".into(),
                value: "5".into(),
            }],
            origin: crate::span::Span::default(),
        });
        let module = MirModule {
            functions: vec![rt_function],
        };
        let artifacts = emit_module(&module, &BackendOptions::default(), &HashMap::new());
        assert_eq!(artifacts.realtime_metadata.len(), 1);
        assert_eq!(artifacts.realtime_metadata[0].deadline_ms, Some(5));
    }
}
