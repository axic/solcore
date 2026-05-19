#![allow(dead_code)]
// sailc: a simple SAIL → Yul compiler.
//
// Usage:
//     sailc <input.solc> [-o <output.yul>]
//     sailc --parse-only <input.solc>
//
// The default action parses the input and emits a (syntactically valid) Yul
// object. The full SAIL pipeline (type-class resolution, monomorphisation,
// match compilation) is *not* implemented; non-assembly statement bodies are
// effectively stubbed. Inline `assembly { ... }` blocks are reproduced
// verbatim in the output.

mod ast;
mod lexer;
mod parser;
mod codegen;

use std::path::Path;
use std::process::ExitCode;

fn usage() -> ! {
    eprintln!("usage: sailc <input.solc> [-o <output.yul>] [--parse-only]");
    std::process::exit(2);
}

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 { usage(); }
    let mut input: Option<String> = None;
    let mut output: Option<String> = None;
    let mut parse_only = false;
    let mut i = 1;
    while i < args.len() {
        let a = &args[i];
        match a.as_str() {
            "-o" => { i += 1; if i >= args.len() { usage(); } output = Some(args[i].clone()); }
            "--parse-only" => parse_only = true,
            "-h" | "--help" => usage(),
            s if s.starts_with('-') => {
                eprintln!("unknown option: {}", s);
                usage();
            }
            s => {
                if input.is_some() { usage(); }
                input = Some(s.to_string());
            }
        }
        i += 1;
    }
    let input = match input { Some(s) => s, None => usage() };
    let src = match std::fs::read_to_string(&input) {
        Ok(s) => s,
        Err(e) => { eprintln!("cannot read {}: {}", input, e); return ExitCode::from(2); }
    };

    // Lex.
    let toks = match lexer::Lexer::new(&src).tokenize() {
        Ok(t) => t,
        Err(e) => { eprintln!("{}: {}", input, e); return ExitCode::from(1); }
    };

    // Parse.
    let mut parser = parser::Parser::new(toks);
    let unit = match parser.parse_unit() {
        Ok(u) => u,
        Err(e) => { eprintln!("{}: {}", input, e); return ExitCode::from(1); }
    };

    eprintln!("{}: parsed OK — {} imports, {} top-level decls",
        input, unit.imports.len(), unit.decls.len());

    if parse_only { return ExitCode::SUCCESS; }

    let stem = Path::new(&input)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module");
    let mut em = codegen::Emitter::new();
    let yul = em.emit_unit(&unit, stem);

    match output {
        Some(path) => {
            if let Err(e) = std::fs::write(&path, &yul) {
                eprintln!("cannot write {}: {}", path, e);
                return ExitCode::from(2);
            }
            eprintln!("wrote {}", path);
        }
        None => print!("{}", yul),
    }

    ExitCode::SUCCESS
}
