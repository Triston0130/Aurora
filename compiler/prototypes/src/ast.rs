//! Abstract syntax tree definitions for the Aurora parser prototype.
use crate::span::Span;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct NodeId(pub u32);

impl NodeId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocCommentKind {
    Outer,
    Inner,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DocComment {
    pub kind: DocCommentKind,
    pub text: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub id: NodeId,
    pub span: Span,
    pub docs: Vec<DocComment>,
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Use(UseDecl),
    Module(ModuleDecl),
    Macro(MacroDecl),
    Function(FunctionDecl),
    Struct(StructDecl),
    Enum(EnumDecl),
    Trait(TraitDecl),
    Impl(ImplDecl),
    Extern(ExternBlock),
    Zone(ZoneDecl),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum Visibility {
    #[default]
    Private,
    Pub,
    PubCrate,
    PubSuper,
    PubSelf,
    PubIn(Vec<String>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: String,
    pub args: Vec<AttributeArg>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AttributeArg {
    Expr(Expr),
    KeyValue { key: String, value: Expr },
}

#[derive(Debug, Clone, PartialEq)]
pub struct UseDecl {
    pub attributes: Vec<Attribute>,
    pub docs: Vec<DocComment>,
    pub visibility: Visibility,
    pub tree: UseTree,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UseTree {
    Path {
        leading_colon: bool,
        segments: Vec<PathSegment>,
        alias: Option<String>,
    },
    Glob {
        leading_colon: bool,
        segments: Vec<PathSegment>,
    },
    Group {
        leading_colon: bool,
        prefix: Vec<PathSegment>,
        items: Vec<UseTree>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleDecl {
    pub attributes: Vec<Attribute>,
    pub docs: Vec<DocComment>,
    pub visibility: Visibility,
    pub name: String,
    pub body: Option<Module>,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MacroDecl {
    pub attributes: Vec<Attribute>,
    pub docs: Vec<DocComment>,
    pub visibility: Visibility,
    pub name: String,
    pub rules: Vec<MacroRule>,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MacroRule {
    pub pattern: Vec<MacroPattern>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MacroPattern {
    Fragment {
        name: String,
        kind: MacroFragmentKind,
        repeat: MacroRepeatKind,
    },
    Group {
        elements: Vec<MacroPattern>,
        separator: Option<String>,
        repeat: MacroRepeatKind,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacroFragmentKind {
    Expr,
    Ident,
    Block,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacroRepeatKind {
    None,
    Optional,
    ZeroOrMore,
    OneOrMore,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ZoneDecl {
    pub attributes: Vec<Attribute>,
    pub docs: Vec<DocComment>,
    pub name: String,
    pub args: Vec<ZoneArg>,
    pub body: Block,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    pub attributes: Vec<Attribute>,
    pub docs: Vec<DocComment>,
    pub visibility: Visibility,
    pub signature: FunctionSig,
    pub body: Block,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSig {
    pub constness: bool,
    pub asyncness: bool,
    pub name: String,
    pub generics: GenericParams,
    pub params: Vec<Param>,
    pub return_type: Option<TypeExpr>,
    pub effect: Option<EffectExpr>,
    pub where_clause: Vec<WherePredicate>,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct GenericParams {
    pub params: Vec<GenericParam>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericParam {
    pub name: String,
    pub kind: GenericParamKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum GenericParamKind {
    Type { bounds: Vec<PathType> },
    Const { ty: Box<TypeExpr> },
    Lifetime { bounds: Vec<String> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum WherePredicate {
    Bound {
        target: TypeExpr,
        bounds: Vec<PathType>,
    },
    Equality {
        path: PathType,
        ty: TypeExpr,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: TypeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EffectExpr {
    pub labels: Vec<EffectLabel>,
    pub tail: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EffectLabel {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ZoneArg {
    pub key: String,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    pub attributes: Vec<Attribute>,
    pub docs: Vec<DocComment>,
    pub visibility: Visibility,
    pub name: String,
    pub generics: GenericParams,
    pub kind: StructKind,
    pub where_clause: Vec<WherePredicate>,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StructKind {
    Unit,
    Tuple(Vec<StructField>),
    Record(Vec<StructField>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub attributes: Vec<Attribute>,
    pub docs: Vec<DocComment>,
    pub visibility: Visibility,
    pub name: Option<String>,
    pub ty: TypeExpr,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDecl {
    pub attributes: Vec<Attribute>,
    pub docs: Vec<DocComment>,
    pub visibility: Visibility,
    pub name: String,
    pub generics: GenericParams,
    pub variants: Vec<EnumVariant>,
    pub where_clause: Vec<WherePredicate>,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub attributes: Vec<Attribute>,
    pub docs: Vec<DocComment>,
    pub name: String,
    pub kind: EnumVariantKind,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumVariantKind {
    Unit,
    Tuple(Vec<StructField>),
    Struct(Vec<StructField>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitDecl {
    pub attributes: Vec<Attribute>,
    pub docs: Vec<DocComment>,
    pub visibility: Visibility,
    pub name: String,
    pub generics: GenericParams,
    pub super_traits: Vec<PathType>,
    pub items: Vec<TraitItem>,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TraitItem {
    Function {
        signature: Box<FunctionSig>,
        default: Option<Box<Block>>,
        attributes: Vec<Attribute>,
        docs: Vec<DocComment>,
        span: Span,
        id: NodeId,
    },
    AssociatedType {
        name: String,
        attributes: Vec<Attribute>,
        docs: Vec<DocComment>,
        span: Span,
        id: NodeId,
    },
    AssociatedEffect {
        name: String,
        effect: EffectExpr,
        attributes: Vec<Attribute>,
        docs: Vec<DocComment>,
        span: Span,
        id: NodeId,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplDecl {
    pub attributes: Vec<Attribute>,
    pub docs: Vec<DocComment>,
    pub generics: GenericParams,
    pub trait_ref: Option<PathType>,
    pub for_type: TypeExpr,
    pub where_clause: Vec<WherePredicate>,
    pub items: Vec<ImplItem>,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImplItem {
    Function {
        signature: Box<FunctionSig>,
        body: Box<Block>,
        attributes: Vec<Attribute>,
        docs: Vec<DocComment>,
        span: Span,
        id: NodeId,
    },
    AssociatedType {
        name: String,
        ty: TypeExpr,
        attributes: Vec<Attribute>,
        docs: Vec<DocComment>,
        span: Span,
        id: NodeId,
    },
    AssociatedEffect {
        name: String,
        effect: EffectExpr,
        attributes: Vec<Attribute>,
        docs: Vec<DocComment>,
        span: Span,
        id: NodeId,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternBlock {
    pub attributes: Vec<Attribute>,
    pub docs: Vec<DocComment>,
    pub abi: Option<String>,
    pub items: Vec<ExternItem>,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExternItem {
    Function {
        signature: FunctionSig,
        attributes: Vec<Attribute>,
        docs: Vec<DocComment>,
        span: Span,
        id: NodeId,
    },
    Static {
        name: String,
        ty: TypeExpr,
        mutable: bool,
        attributes: Vec<Attribute>,
        docs: Vec<DocComment>,
        span: Span,
        id: NodeId,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard,
    Ident(String),
    Binding {
        name: String,
        mutable: bool,
        is_move: bool,
        subpattern: Box<Pattern>,
    },
    Move(Box<Pattern>),
    Or(Vec<Pattern>),
    Tuple(Vec<Pattern>),
    Slice {
        prefix: Vec<Pattern>,
        rest: Option<Box<Pattern>>,
        suffix: Vec<Pattern>,
    },
    Struct {
        path: PathExpr,
        fields: Vec<StructPatternField>,
        rest: bool,
    },
    Enum {
        path: PathExpr,
        inner: Option<Box<Pattern>>,
    },
    Literal(Literal),
    Reference {
        mutable: bool,
        inner: Box<Pattern>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructPatternField {
    pub name: String,
    pub pattern: Option<Pattern>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub tail: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {
        pattern: Pattern,
        ty: Option<TypeExpr>,
        value: Option<Expr>,
        span: Span,
    },
    Return {
        value: Option<Expr>,
        span: Span,
    },
    Expr(Expr, Span),
    Item(Item, Span),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn dummy(kind: ExprKind) -> Self {
        Self {
            kind,
            span: Span::default(),
        }
    }

    pub fn simple_literal(lit: Literal, span: Span) -> Self {
        Self::new(ExprKind::Literal(lit), span)
    }

    pub fn simple_path(path: PathExpr, span: Span) -> Self {
        Self::new(ExprKind::Path(path), span)
    }

    pub fn literal(lit: Literal) -> Self {
        Self::dummy(ExprKind::Literal(lit))
    }

    pub fn path(path: PathExpr) -> Self {
        Self::dummy(ExprKind::Path(path))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Literal(Literal),
    Path(PathExpr),
    Tuple(Vec<Expr>),
    Array(Vec<Expr>),
    Struct(StructExpr),
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    MacroCall {
        name: String,
        args: Vec<Expr>,
    },
    MethodCall {
        receiver: Box<Expr>,
        method: String,
        turbofish: Vec<TypeExpr>,
        args: Vec<Expr>,
    },
    Field {
        base: Box<Expr>,
        field: String,
    },
    Index {
        base: Box<Expr>,
        index: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    If {
        condition: Box<Expr>,
        then_branch: Block,
        else_branch: Option<Box<Expr>>,
    },
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Loop(Block),
    While {
        condition: Box<Expr>,
        body: Block,
    },
    For {
        pattern: Pattern,
        iterator: Box<Expr>,
        body: Block,
    },
    Async(Block),
    Await(Box<Expr>),
    Spawn(Box<Expr>),
    Actor(ActorLiteral),
    Zone {
        name: String,
        args: Vec<ZoneArg>,
        body: Block,
    },
    Handle {
        body: Box<Expr>,
        handlers: Vec<EffectHandler>,
    },
    Block(Block),
    Closure {
        params: Vec<String>,
        body: Block,
    },
    Break(Option<Box<Expr>>),
    Continue,
    Assignment {
        left: Box<Expr>,
        op: AssignmentOp,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructExpr {
    pub path: PathExpr,
    pub fields: Vec<StructExprField>,
    pub shorthand: bool,
    pub spread: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructExprField {
    pub name: String,
    pub expr: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ActorLiteral {
    pub path: PathExpr,
    pub fields: Vec<ActorField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ActorField {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EffectHandler {
    pub label: String,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(String),
    Float(String),
    String(String),
    Char(char),
    Boolean(bool),
    Unit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Eq,
    NotEq,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
    Ref,
    RefMut,
    Deref,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignmentOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpr {
    Path(PathType),
    Tuple(Vec<TypeExpr>),
    Function {
        params: Vec<TypeExpr>,
        effect: Option<EffectExpr>,
        ret: Box<TypeExpr>,
    },
    Reference {
        lifetime: Option<String>,
        mutable: bool,
        inner: Box<TypeExpr>,
    },
    RawPointer {
        mutable: bool,
        inner: Box<TypeExpr>,
    },
    Array {
        element: Box<TypeExpr>,
        length: Box<Option<Expr>>,
    },
    Slice(Box<TypeExpr>),
    TraitObject(PathType),
    Lifetime(String),
    Unit,
    Never,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathType {
    pub leading_colon: bool,
    pub segments: Vec<PathSegment>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathExpr {
    pub leading_colon: bool,
    pub segments: Vec<PathSegment>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathSegment {
    pub ident: String,
    pub generics: Vec<TypeExpr>,
}

impl fmt::Display for PathExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.leading_colon {
            write!(f, "::")?;
        }
        let mut first = true;
        for segment in &self.segments {
            if !first {
                write!(f, "::")?;
            }
            first = false;
            write!(f, "{}", segment.ident)?;
        }
        Ok(())
    }
}

impl From<PathExpr> for PathType {
    fn from(value: PathExpr) -> Self {
        PathType {
            leading_colon: value.leading_colon,
            segments: value.segments,
        }
    }
}
