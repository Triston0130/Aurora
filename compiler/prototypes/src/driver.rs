use std::collections::HashMap;

use crate::ast::Module;
use crate::const_eval::ConstValue;
use crate::constraints::{Generator, SolvedAnalysis};
use crate::diagnostics::{render_all, Diagnostic};
use crate::hir::{HirBuilder, HirModule};
use crate::mir::{HirToMir, MirModule};
use crate::parser::{self, ParsedModule};
use crate::resolver::{self, ResolvedModule};
use crate::types::{InferenceContext, TypeEnv};

#[derive(Debug)]
pub struct CompilationResult {
    pub parsed: ParsedModule,
    pub ast: Module,
    pub resolved: ResolvedModule,
    pub solved: SolvedAnalysis,
    pub hir: HirModule,
    pub mir: MirModule,
    pub ctfe_results: HashMap<String, ConstValue>,
    pub diagnostics: Vec<Diagnostic>,
}

pub fn compile_module(name: &str, source: &str) -> Result<CompilationResult, Vec<Diagnostic>> {
    let parsed = parser::parse_module_with_source(name, source)
        .map_err(|err| vec![err.into_diagnostic()])?;

    let mut diagnostics = Vec::new();

    let (expanded_module, mut macro_diagnostics) =
        crate::macro_expander::expand_module(&parsed.module);
    diagnostics.append(&mut macro_diagnostics);

    let resolved = resolver::resolve_module(&expanded_module);
    diagnostics.extend(resolved.diagnostics.clone());

    let mut env = TypeEnv::new();
    env.push_scope();
    let mut ctx = InferenceContext::default();
    let generator = Generator::new(&mut env, &mut ctx);
    let analysis = generator.generate_module(&expanded_module);

    diagnostics.extend(analysis.constraints.zone_diagnostics.clone());

    let solved = match analysis.clone().solve() {
        Ok(solved) => solved,
        Err(error) => {
            diagnostics.push(error.into_diagnostic());
            return Err(diagnostics);
        }
    };

    let hir = HirBuilder::new(&solved).build_module(&expanded_module);

    diagnostics.extend(hir.borrow_diagnostics.clone());
    diagnostics.extend(hir.trait_diagnostics.clone());

    let mir = HirToMir::new(&solved).lower_module(&hir);
    let ctfe = crate::ctfe::evaluate_module(&hir);
    diagnostics.extend(ctfe.diagnostics.clone());

    Ok(CompilationResult {
        parsed,
        ast: expanded_module,
        resolved,
        solved,
        hir,
        mir,
        ctfe_results: ctfe.values,
        diagnostics,
    })
}

pub fn render_diagnostics(result: &CompilationResult) -> String {
    crate::diagnostics::render_all_with_sources(&result.parsed.source_map, &result.diagnostics)
}

pub fn compile_and_render(name: &str, source: &str) -> Result<(CompilationResult, String), String> {
    match compile_module(name, source) {
        Ok(result) => {
            let rendered = render_diagnostics(&result);
            Ok((result, rendered))
        }
        Err(diags) => Err(render_all(&diags)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compile_module_reports_zone_violation() {
        let source = r#"
            @zone(realtime)
            fn main() -> Unit ! IOError {
                return;
            }
        "#;
        let result = compile_module("test", source).expect("compilation succeeds");
        assert!(!result.diagnostics.is_empty());
        assert!(result
            .diagnostics
            .iter()
            .any(|diag| diag.message.contains("realtime")));
    }

    #[test]
    fn compile_module_propagates_parse_errors() {
        let source = "fn"; // incomplete function declaration
        let diagnostics = compile_module("test", source).unwrap_err();
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("expected"));
    }
}
