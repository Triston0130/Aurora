use insta::assert_snapshot;
use std::fs;

fn read_example(name: &str) -> String {
    fs::read_to_string(format!("examples/{}.aur", name)).expect("example file")
}

#[test]
fn parse_actor_pipeline() {
    let source = read_example("actor_pipeline");
    let ast = aurora_compiler_prototypes::parser::parse_module(&source).expect("parse");
    assert_snapshot!(format!("{ast:#?}"));
}

#[test]
fn parse_gpu_zone() {
    let source = read_example("gpu_zone");
    let ast = aurora_compiler_prototypes::parser::parse_module(&source).expect("parse");
    assert_snapshot!(format!("{ast:#?}"));
}

#[test]
fn parse_effectful_trait() {
    let source = read_example("effectful_trait");
    let ast = aurora_compiler_prototypes::parser::parse_module(&source).expect("parse");
    assert_snapshot!(format!("{ast:#?}"));
}

#[test]
fn parse_zone_decl() {
    let source = read_example("zone_decl");
    let ast = aurora_compiler_prototypes::parser::parse_module(&source).expect("parse");
    assert_snapshot!(format!("{ast:#?}"));
}

#[test]
fn parse_patterns() {
    let source = read_example("patterns");
    let ast = aurora_compiler_prototypes::parser::parse_module(&source).expect("parse");
    assert_snapshot!(format!("{ast:#?}"));
}

#[test]
fn parse_types_features() {
    let source = read_example("types_features");
    let ast = aurora_compiler_prototypes::parser::parse_module(&source).expect("parse");
    assert_snapshot!(format!("{ast:#?}"));
}

#[test]
fn parse_use_groups() {
    let source = read_example("use_groups");
    let ast = aurora_compiler_prototypes::parser::parse_module(&source).expect("parse");
    assert_snapshot!(format!("{ast:#?}"));
}

#[test]
fn parse_effect_handler() {
    let source = read_example("effect_handler");
    let ast = aurora_compiler_prototypes::parser::parse_module(&source).expect("parse");
    assert_snapshot!(format!("{ast:#?}"));
}
