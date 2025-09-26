use std::collections::{HashMap, HashSet};

use crate::ast::{Item, Module, ModuleDecl, PathSegment, UseTree};
use crate::diagnostics::{primary_label, secondary_label, Diagnostic};
use crate::span::Span;

type ResolveResult<T> = Result<T, Box<Diagnostic>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    Module,
    Function,
    Struct,
    Enum,
    Trait,
    Zone,
    UseAlias,
}

#[derive(Debug, Clone)]
pub enum SymbolTarget {
    Definition,
    Module { module: ModuleId },
    Alias { target: SymbolId },
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub id: SymbolId,
    pub name: String,
    pub kind: SymbolKind,
    pub span: Span,
    pub module: ModuleId,
    pub target: SymbolTarget,
}

#[derive(Debug, Clone)]
pub struct ModuleScope {
    pub id: ModuleId,
    pub name: Option<String>,
    pub span: Span,
    pub parent: Option<ModuleId>,
    pub symbols: HashMap<String, SymbolId>,
    pub children: HashMap<String, ModuleId>,
    pub self_symbol: Option<SymbolId>,
}

#[derive(Debug)]
pub struct ResolvedModule {
    pub root: ModuleId,
    pub modules: Vec<ModuleScope>,
    pub symbols: Vec<Symbol>,
    pub diagnostics: Vec<Diagnostic>,
}

struct PendingUse {
    module: ModuleId,
    leading_colon: bool,
    segments: Vec<PathSegment>,
    glob: bool,
    alias: Option<String>,
    span: Span,
}

enum ResolvedTarget {
    Module(ModuleId),
    Symbol(SymbolId),
}

struct Resolver {
    modules: Vec<ModuleScope>,
    symbols: Vec<Symbol>,
    diagnostics: Vec<Diagnostic>,
    pending_uses: Vec<PendingUse>,
    root: ModuleId,
}

impl Resolver {
    fn new() -> Self {
        let mut modules = Vec::new();
        let root = ModuleId(0);
        modules.push(ModuleScope {
            id: root,
            name: None,
            span: Span::default(),
            parent: None,
            symbols: HashMap::new(),
            children: HashMap::new(),
            self_symbol: None,
        });
        Resolver {
            modules,
            symbols: Vec::new(),
            diagnostics: Vec::new(),
            pending_uses: Vec::new(),
            root,
        }
    }

    fn resolve(mut self, module: &Module) -> ResolvedModule {
        self.collect_module(module, self.root);
        self.resolve_pending_uses();
        ResolvedModule {
            root: self.root,
            modules: self.modules,
            symbols: self.symbols,
            diagnostics: self.diagnostics,
        }
    }

    fn collect_module(&mut self, module: &Module, module_id: ModuleId) {
        for item in &module.items {
            match item {
                Item::Function(func) => {
                    self.insert_symbol(
                        module_id,
                        &func.signature.name,
                        SymbolKind::Function,
                        func.span,
                        SymbolTarget::Definition,
                    );
                }
                Item::Struct(decl) => {
                    self.insert_symbol(
                        module_id,
                        &decl.name,
                        SymbolKind::Struct,
                        decl.span,
                        SymbolTarget::Definition,
                    );
                }
                Item::Enum(decl) => {
                    self.insert_symbol(
                        module_id,
                        &decl.name,
                        SymbolKind::Enum,
                        decl.span,
                        SymbolTarget::Definition,
                    );
                }
                Item::Trait(decl) => {
                    self.insert_symbol(
                        module_id,
                        &decl.name,
                        SymbolKind::Trait,
                        decl.span,
                        SymbolTarget::Definition,
                    );
                }
                Item::Zone(zone) => {
                    self.insert_symbol(
                        module_id,
                        &zone.name,
                        SymbolKind::Zone,
                        zone.span,
                        SymbolTarget::Definition,
                    );
                }
                Item::Macro(_) => {
                    // Macro declarations do not currently introduce symbols.
                }
                Item::Module(decl) => {
                    self.handle_module_decl(module_id, decl);
                }
                Item::Use(use_decl) => {
                    self.enqueue_use_tree(module_id, &use_decl.tree, use_decl.span);
                }
                Item::Extern(_) | Item::Impl(_) => {
                    // impl blocks and extern blocks do not introduce named symbols.
                }
            }
        }
    }

    fn handle_module_decl(&mut self, parent_id: ModuleId, decl: &ModuleDecl) {
        let child_id = self.create_module(Some(decl.name.clone()), decl.span, Some(parent_id));
        let symbol_id = match self.insert_symbol(
            parent_id,
            &decl.name,
            SymbolKind::Module,
            decl.span,
            SymbolTarget::Module { module: child_id },
        ) {
            Some(id) => id,
            None => return,
        };
        self.modules[child_id.0].self_symbol = Some(symbol_id);
        self.modules[parent_id.0]
            .children
            .insert(decl.name.clone(), child_id);
        if let Some(body) = &decl.body {
            self.collect_module(body, child_id);
        }
    }

    fn create_module(
        &mut self,
        name: Option<String>,
        span: Span,
        parent: Option<ModuleId>,
    ) -> ModuleId {
        let id = ModuleId(self.modules.len());
        self.modules.push(ModuleScope {
            id,
            name,
            span,
            parent,
            symbols: HashMap::new(),
            children: HashMap::new(),
            self_symbol: None,
        });
        id
    }

    fn insert_symbol(
        &mut self,
        module_id: ModuleId,
        name: &str,
        kind: SymbolKind,
        span: Span,
        target: SymbolTarget,
    ) -> Option<SymbolId> {
        if let Some(existing) = self.modules[module_id.0].symbols.get(name) {
            let existing_symbol = &self.symbols[existing.0];
            let diagnostic = Diagnostic::error(format!("duplicate definition of `{}`", name))
                .with_label(primary_label(span, "redefinition occurs here"))
                .with_label(secondary_label(
                    existing_symbol.span,
                    "previous definition is located here",
                ));
            self.diagnostics.push(diagnostic);
            return None;
        }
        let id = SymbolId(self.symbols.len());
        self.symbols.push(Symbol {
            id,
            name: name.to_string(),
            kind,
            span,
            module: module_id,
            target: target.clone(),
        });
        self.modules[module_id.0]
            .symbols
            .insert(name.to_string(), id);
        if let SymbolTarget::Module { module } = &target {
            self.modules[module_id.0]
                .children
                .insert(name.to_string(), *module);
        }
        Some(id)
    }

    fn resolve_pending_uses(&mut self) {
        let pending = std::mem::take(&mut self.pending_uses);
        for use_item in pending {
            self.resolve_use(use_item);
        }
    }

    fn enqueue_use_tree(&mut self, module: ModuleId, tree: &UseTree, span: Span) {
        match tree {
            UseTree::Path {
                leading_colon,
                segments,
                alias,
            } => {
                self.pending_uses.push(PendingUse {
                    module,
                    leading_colon: *leading_colon,
                    segments: segments.clone(),
                    glob: false,
                    alias: alias.clone(),
                    span,
                });
            }
            UseTree::Glob {
                leading_colon,
                segments,
            } => {
                self.pending_uses.push(PendingUse {
                    module,
                    leading_colon: *leading_colon,
                    segments: segments.clone(),
                    glob: true,
                    alias: None,
                    span,
                });
            }
            UseTree::Group {
                leading_colon,
                prefix,
                items,
            } => {
                for item in items {
                    self.enqueue_use_group_item(module, *leading_colon, prefix, item, span);
                }
            }
        }
    }

    fn enqueue_use_group_item(
        &mut self,
        module: ModuleId,
        parent_leading: bool,
        prefix: &[PathSegment],
        item: &UseTree,
        span: Span,
    ) {
        match item {
            UseTree::Path {
                leading_colon,
                segments,
                alias,
            } => {
                let (leading, combined) =
                    self.combine_use_segments(parent_leading, prefix, *leading_colon, segments);
                self.pending_uses.push(PendingUse {
                    module,
                    leading_colon: leading,
                    segments: combined,
                    glob: false,
                    alias: alias.clone(),
                    span,
                });
            }
            UseTree::Glob {
                leading_colon,
                segments,
            } => {
                let (leading, combined) =
                    self.combine_use_segments(parent_leading, prefix, *leading_colon, segments);
                self.pending_uses.push(PendingUse {
                    module,
                    leading_colon: leading,
                    segments: combined,
                    glob: true,
                    alias: None,
                    span,
                });
            }
            UseTree::Group {
                leading_colon,
                prefix: child_prefix,
                items,
            } => {
                let (leading, combined_prefix) =
                    self.combine_group_prefix(parent_leading, prefix, *leading_colon, child_prefix);
                for child in items {
                    self.enqueue_use_group_item(module, leading, &combined_prefix, child, span);
                }
            }
        }
    }

    fn combine_use_segments(
        &self,
        parent_leading: bool,
        prefix: &[PathSegment],
        leading_colon: bool,
        segments: &[PathSegment],
    ) -> (bool, Vec<PathSegment>) {
        if leading_colon {
            return (true, segments.to_vec());
        }

        if segments.len() == 1 && segments[0].ident == "self" {
            if prefix.is_empty() {
                return (
                    parent_leading,
                    vec![PathSegment {
                        ident: "self".into(),
                        generics: Vec::new(),
                    }],
                );
            } else {
                return (parent_leading, prefix.to_vec());
            }
        }

        let mut combined = prefix.to_vec();
        combined.extend(segments.iter().cloned());
        (parent_leading, combined)
    }

    fn combine_group_prefix(
        &self,
        parent_leading: bool,
        parent_prefix: &[PathSegment],
        child_leading: bool,
        child_prefix: &[PathSegment],
    ) -> (bool, Vec<PathSegment>) {
        if child_leading {
            return (true, child_prefix.to_vec());
        }
        let mut combined = parent_prefix.to_vec();
        combined.extend(child_prefix.iter().cloned());
        (parent_leading, combined)
    }

    fn resolve_use(&mut self, pending: PendingUse) {
        let start_module = if pending.leading_colon {
            self.root
        } else {
            pending.module
        };
        match self.resolve_path(start_module, &pending.segments, pending.span) {
            Ok(ResolvedTarget::Symbol(target)) => {
                if pending.glob {
                    let target_id = self.resolve_alias(target);
                    let symbol = &self.symbols[target_id.0];
                    let diagnostic = Diagnostic::error("cannot glob-import from a value")
                        .with_label(primary_label(pending.span, "glob imports require a module"))
                        .with_label(secondary_label(symbol.span, "this item is not a module"));
                    self.diagnostics.push(diagnostic);
                    return;
                }
                let target_id = self.resolve_alias(target);
                let alias_name = pending.alias.clone().unwrap_or_else(|| {
                    pending
                        .segments
                        .last()
                        .map(|seg| seg.ident.clone())
                        .unwrap_or_else(|| "_".into())
                });
                let kind = self.symbols[target_id.0].kind.clone();
                self.insert_symbol(
                    pending.module,
                    &alias_name,
                    kind,
                    pending.span,
                    SymbolTarget::Alias { target: target_id },
                );
            }
            Ok(ResolvedTarget::Module(target_module)) => {
                if pending.glob {
                    self.import_glob(pending.module, target_module, pending.span);
                } else {
                    let alias_name = pending.alias.clone().unwrap_or_else(|| {
                        pending
                            .segments
                            .last()
                            .map(|seg| seg.ident.clone())
                            .unwrap_or_else(|| "_".into())
                    });
                    let kind = SymbolKind::Module;
                    self.insert_symbol(
                        pending.module,
                        &alias_name,
                        kind,
                        pending.span,
                        SymbolTarget::Module {
                            module: target_module,
                        },
                    );
                }
            }
            Err(diag) => self.diagnostics.push(*diag),
        }
    }

    fn import_glob(&mut self, into: ModuleId, from: ModuleId, span: Span) {
        let names: Vec<String> = self.modules[from.0].symbols.keys().cloned().collect();
        for name in names {
            if let Some(&symbol_id) = self.modules[from.0].symbols.get(&name) {
                let target_id = self.resolve_alias(symbol_id);
                let kind = self.symbols[target_id.0].kind.clone();
                self.insert_symbol(
                    into,
                    &name,
                    kind,
                    span,
                    SymbolTarget::Alias { target: target_id },
                );
            }
        }
    }

    fn resolve_alias(&self, mut id: SymbolId) -> SymbolId {
        let mut seen = HashSet::new();
        while let SymbolTarget::Alias { target } = self.symbols[id.0].target {
            if !seen.insert(id) {
                break;
            }
            id = target;
        }
        id
    }

    fn resolve_path(
        &self,
        start: ModuleId,
        segments: &[PathSegment],
        span: Span,
    ) -> ResolveResult<ResolvedTarget> {
        if segments.is_empty() {
            return Err(Box::new(
                Diagnostic::error("empty import path")
                    .with_label(primary_label(span, "expected a path")),
            ));
        }
        let mut index = 0;
        let mut current = start;
        while index < segments.len() {
            let ident = segments[index].ident.as_str();
            if index == 0 {
                match ident {
                    "crate" => {
                        current = self.root;
                        index += 1;
                        continue;
                    }
                    "self" => {
                        index += 1;
                        continue;
                    }
                    "super" => {
                        if let Some(parent) = self.modules[current.0].parent {
                            current = parent;
                            index += 1;
                            continue;
                        } else {
                            return Err(Box::new(
                                Diagnostic::error("`super` refers past crate root")
                                    .with_label(primary_label(span, "cannot go past crate root")),
                            ));
                        }
                    }
                    _ => {}
                }
            }

            if index + 1 < segments.len() {
                current = match self.lookup_child_module(current, ident) {
                    Some(child) => child,
                    None => {
                        return Err(Box::new(self.unresolved_path(segments, span)));
                    }
                };
                index += 1;
                continue;
            }

            // Last segment
            if let Some(module_id) = self.lookup_child_module(current, ident) {
                return Ok(ResolvedTarget::Module(module_id));
            }
            if let Some(symbol_id) = self.modules[current.0].symbols.get(ident) {
                let resolved = self.resolve_alias(*symbol_id);
                return Ok(ResolvedTarget::Symbol(resolved));
            }
            return Err(Box::new(self.unresolved_path(segments, span)));
        }
        Ok(ResolvedTarget::Module(current))
    }

    fn unresolved_path(&self, segments: &[PathSegment], span: Span) -> Diagnostic {
        let path: Vec<_> = segments.iter().map(|seg| seg.ident.as_str()).collect();
        Diagnostic::error(format!("unresolved path `{}`", path.join("::")))
            .with_label(primary_label(span, "cannot find this path"))
    }

    fn lookup_child_module(&self, module_id: ModuleId, name: &str) -> Option<ModuleId> {
        if let Some(child) = self.modules[module_id.0].children.get(name) {
            return Some(*child);
        }
        if let Some(symbol_id) = self.modules[module_id.0].symbols.get(name) {
            if let SymbolTarget::Module { module } = self.symbols[symbol_id.0].target {
                return Some(module);
            }
        }
        None
    }
}

pub fn resolve_module(module: &Module) -> ResolvedModule {
    Resolver::new().resolve(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        Attribute, AttributeArg, Block, FunctionDecl, FunctionSig, GenericParams, Item, Literal,
        Module as AstModule, ModuleDecl, NodeId, PathSegment, Statement, UseDecl, UseTree,
        ZoneDecl,
    };
    use crate::span::Span;

    fn simple_module() -> AstModule {
        AstModule {
            id: NodeId::new(0),
            span: Span::default(),
            docs: Vec::new(),
            items: vec![Item::Function(FunctionDecl {
                attributes: Vec::new(),
                docs: Vec::new(),
                visibility: crate::ast::Visibility::Private,
                signature: FunctionSig {
                    constness: false,
                    asyncness: false,
                    name: "main".into(),
                    generics: GenericParams::default(),
                    params: vec![],
                    return_type: None,
                    effect: None,
                    where_clause: Vec::new(),
                },
                body: Block {
                    statements: vec![Statement::Expr(
                        crate::ast::Expr::literal(Literal::Unit),
                        Span::default(),
                    )],
                    tail: None,
                    span: Span::default(),
                },
                span: Span::default(),
                id: NodeId::new(1),
            })],
        }
    }

    #[test]
    fn collects_function_symbol() {
        let resolved = resolve_module(&simple_module());
        assert!(resolved.diagnostics.is_empty());
        let root = &resolved.modules[resolved.root.0];
        assert!(root.symbols.contains_key("main"));
        let symbol_id = root.symbols["main"];
        let symbol = &resolved.symbols[symbol_id.0];
        assert_eq!(symbol.kind, SymbolKind::Function);
    }

    #[test]
    fn detects_duplicate_symbols() {
        let module = AstModule {
            id: NodeId::new(0),
            span: Span::default(),
            docs: Vec::new(),
            items: vec![
                Item::Function(FunctionDecl {
                    attributes: Vec::new(),
                    docs: Vec::new(),
                    visibility: crate::ast::Visibility::Private,
                    signature: FunctionSig {
                        constness: false,
                        asyncness: false,
                        name: "f".into(),
                        generics: GenericParams::default(),
                        params: vec![],
                        return_type: None,
                        effect: None,
                        where_clause: Vec::new(),
                    },
                    body: Block {
                        statements: vec![Statement::Expr(
                            crate::ast::Expr::literal(Literal::Unit),
                            Span::default(),
                        )],
                        tail: None,
                        span: Span::default(),
                    },
                    span: Span::new(1, 1),
                    id: NodeId::new(1),
                }),
                Item::Function(FunctionDecl {
                    attributes: Vec::new(),
                    docs: Vec::new(),
                    visibility: crate::ast::Visibility::Private,
                    signature: FunctionSig {
                        constness: false,
                        asyncness: false,
                        name: "f".into(),
                        generics: GenericParams::default(),
                        params: vec![],
                        return_type: None,
                        effect: None,
                        where_clause: Vec::new(),
                    },
                    body: Block {
                        statements: vec![Statement::Expr(
                            crate::ast::Expr::literal(Literal::Unit),
                            Span::default(),
                        )],
                        tail: None,
                        span: Span::default(),
                    },
                    span: Span::new(2, 1),
                    id: NodeId::new(2),
                }),
            ],
        };
        let resolved = resolve_module(&module);
        assert_eq!(resolved.diagnostics.len(), 1);
        assert!(resolved.modules[resolved.root.0].symbols.contains_key("f"));
    }

    #[test]
    fn resolves_use_alias() {
        let mut next_id = 0u32;
        let mut fresh_id = || {
            let id = NodeId::new(next_id);
            next_id += 1;
            id
        };

        let module = AstModule {
            id: fresh_id(),
            span: Span::default(),
            docs: Vec::new(),
            items: vec![
                Item::Module(ModuleDecl {
                    attributes: Vec::new(),
                    docs: Vec::new(),
                    visibility: crate::ast::Visibility::Private,
                    name: "inner".into(),
                    body: Some(AstModule {
                        id: fresh_id(),
                        span: Span::default(),
                        docs: Vec::new(),
                        items: vec![Item::Zone(ZoneDecl {
                            attributes: vec![Attribute {
                                name: "doc".into(),
                                args: vec![AttributeArg::Expr(crate::ast::Expr::literal(
                                    Literal::String("hello".into()),
                                ))],
                                span: Span::default(),
                            }],
                            docs: Vec::new(),
                            name: "kernel".into(),
                            args: Vec::new(),
                            body: Block {
                                statements: Vec::new(),
                                tail: None,
                                span: Span::default(),
                            },
                            span: Span::default(),
                            id: fresh_id(),
                        })],
                    }),
                    span: Span::default(),
                    id: fresh_id(),
                }),
                Item::Use(UseDecl {
                    attributes: Vec::new(),
                    docs: Vec::new(),
                    visibility: crate::ast::Visibility::Private,
                    tree: UseTree::Path {
                        leading_colon: false,
                        segments: vec![PathSegment {
                            ident: "inner".into(),
                            generics: Vec::new(),
                        }],
                        alias: Some("renamed".into()),
                    },
                    span: Span::default(),
                    id: fresh_id(),
                }),
            ],
        };
        let resolved = resolve_module(&module);
        assert!(resolved.diagnostics.is_empty());
        let root = &resolved.modules[resolved.root.0];
        assert!(root.symbols.contains_key("renamed"));
        let symbol_id = root.symbols["renamed"];
        assert_eq!(resolved.symbols[symbol_id.0].kind, SymbolKind::Module);
    }

    #[test]
    fn reports_unresolved_use() {
        let module = simple_module();
        let module_with_use = AstModule {
            id: NodeId::new(0),
            span: Span::default(),
            docs: Vec::new(),
            items: vec![
                Item::Use(UseDecl {
                    attributes: Vec::new(),
                    docs: Vec::new(),
                    visibility: crate::ast::Visibility::Private,
                    tree: UseTree::Path {
                        leading_colon: false,
                        segments: vec![PathSegment {
                            ident: "does_not_exist".into(),
                            generics: Vec::new(),
                        }],
                        alias: None,
                    },
                    span: Span::new(1, 1),
                    id: NodeId::new(1),
                }),
                module.items[0].clone(),
            ],
        };
        let resolved = resolve_module(&module_with_use);
        assert!(!resolved.diagnostics.is_empty());
    }

    #[test]
    fn resolves_use_group() {
        let mut next_id = 0u32;
        let mut fresh_id = || {
            let id = NodeId::new(next_id);
            next_id += 1;
            id
        };

        let module = AstModule {
            id: fresh_id(),
            span: Span::default(),
            docs: Vec::new(),
            items: vec![
                Item::Module(ModuleDecl {
                    attributes: Vec::new(),
                    docs: Vec::new(),
                    visibility: crate::ast::Visibility::Private,
                    name: "prelude".into(),
                    body: Some(AstModule {
                        id: fresh_id(),
                        span: Span::default(),
                        docs: Vec::new(),
                        items: vec![Item::Function(FunctionDecl {
                            attributes: Vec::new(),
                            docs: Vec::new(),
                            visibility: crate::ast::Visibility::Private,
                            signature: FunctionSig {
                                constness: false,
                                asyncness: false,
                                name: "helper".into(),
                                generics: GenericParams::default(),
                                params: vec![],
                                return_type: None,
                                effect: None,
                                where_clause: Vec::new(),
                            },
                            body: Block {
                                statements: vec![Statement::Expr(
                                    crate::ast::Expr::literal(Literal::Unit),
                                    Span::default(),
                                )],
                                tail: None,
                                span: Span::default(),
                            },
                            span: Span::default(),
                            id: fresh_id(),
                        })],
                    }),
                    span: Span::default(),
                    id: fresh_id(),
                }),
                Item::Module(ModuleDecl {
                    attributes: Vec::new(),
                    docs: Vec::new(),
                    visibility: crate::ast::Visibility::Private,
                    name: "runtime".into(),
                    body: Some(AstModule {
                        id: fresh_id(),
                        span: Span::default(),
                        docs: Vec::new(),
                        items: vec![Item::Module(ModuleDecl {
                            attributes: Vec::new(),
                            docs: Vec::new(),
                            visibility: crate::ast::Visibility::Private,
                            name: "scheduler".into(),
                            body: Some(AstModule {
                                id: fresh_id(),
                                span: Span::default(),
                                docs: Vec::new(),
                                items: vec![Item::Function(FunctionDecl {
                                    attributes: Vec::new(),
                                    docs: Vec::new(),
                                    visibility: crate::ast::Visibility::Private,
                                    signature: FunctionSig {
                                        constness: false,
                                        asyncness: false,
                                        name: "Scheduler".into(),
                                        generics: GenericParams::default(),
                                        params: vec![],
                                        return_type: None,
                                        effect: None,
                                        where_clause: Vec::new(),
                                    },
                                    body: Block {
                                        statements: vec![Statement::Expr(
                                            crate::ast::Expr::literal(Literal::Unit),
                                            Span::default(),
                                        )],
                                        tail: None,
                                        span: Span::default(),
                                    },
                                    span: Span::default(),
                                    id: fresh_id(),
                                })],
                            }),
                            span: Span::default(),
                            id: fresh_id(),
                        })],
                    }),
                    span: Span::default(),
                    id: fresh_id(),
                }),
                Item::Use(UseDecl {
                    attributes: Vec::new(),
                    docs: Vec::new(),
                    visibility: crate::ast::Visibility::Private,
                    tree: UseTree::Group {
                        leading_colon: false,
                        prefix: vec![PathSegment {
                            ident: "crate".into(),
                            generics: Vec::new(),
                        }],
                        items: vec![
                            UseTree::Glob {
                                leading_colon: false,
                                segments: vec![PathSegment {
                                    ident: "prelude".into(),
                                    generics: Vec::new(),
                                }],
                            },
                            UseTree::Path {
                                leading_colon: false,
                                segments: vec![
                                    PathSegment {
                                        ident: "runtime".into(),
                                        generics: Vec::new(),
                                    },
                                    PathSegment {
                                        ident: "scheduler".into(),
                                        generics: Vec::new(),
                                    },
                                    PathSegment {
                                        ident: "Scheduler".into(),
                                        generics: Vec::new(),
                                    },
                                ],
                                alias: Some("Sched".into()),
                            },
                        ],
                    },
                    span: Span::default(),
                    id: fresh_id(),
                }),
            ],
        };

        let resolved = resolve_module(&module);
        assert!(resolved.diagnostics.is_empty());
        let root = &resolved.modules[resolved.root.0];
        assert!(root.symbols.contains_key("helper"));
        assert!(root.symbols.contains_key("Sched"));
    }
}
