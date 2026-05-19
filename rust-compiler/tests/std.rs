// End-to-end smoke tests: the std/*.solc files must lex, parse and emit
// a non-empty Yul object.

use std::path::PathBuf;

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("rust-compiler should sit inside the workspace")
        .to_path_buf()
}

#[path = "../src/lexer.rs"]
mod lexer;
#[path = "../src/ast.rs"]
mod ast;
#[path = "../src/parser.rs"]
mod parser;
#[path = "../src/codegen.rs"]
mod codegen;

fn compile(path: &PathBuf) -> String {
    let src = std::fs::read_to_string(path).expect("read source");
    let toks = lexer::Lexer::new(&src).tokenize().expect("lex");
    let mut p = parser::Parser::new(toks);
    let unit = p.parse_unit().expect("parse");
    let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("module");
    let mut em = codegen::Emitter::new();
    em.emit_unit(&unit, stem)
}

#[test]
fn std_files_compile() {
    let root = workspace_root();
    let names = [
        "std.solc",
        "NumLib.solc",
        "assign.solc",
        "dispatch.solc",
        "rtdispatch.solc",
        "types.solc",
    ];
    for n in names {
        let path = root.join("std").join(n);
        let yul = compile(&path);
        assert!(yul.contains("object \""), "no Yul object emitted for {}", n);
        assert!(yul.contains("runtime"), "no runtime sub-object for {}", n);
    }
}
