use ::infer::Ast;
use infer::infer;
use parser::{lex, lexer::NixTokens, map_err};

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if let Some(filename) = &args.get(1) {
        let input = std::fs::read_to_string(filename)?;
        let tokens = lex(&input);
        match parser::parser::expr(NixTokens(&tokens)) {
            Ok((_, ast)) => {
                let ast = Ast::from_parser_ast(ast, &input);
                println!("ast: {:#?}", ast);
                match infer::coalesced(&ast) {
                    Err(e) => {
                        eprintln!("[Inference] Error: {:?}", e);
                    }
                    Ok(ty) => println!("Inferred type: {:?}", ty.show()),
                };
            }
            Err(e) => match e {
                err @ nom::Err::Error(_) | err @ nom::Err::Failure(_) => {
                    eprintln!("[Parser] Error: {:?}", map_err(err))
                }
                nom::Err::Incomplete(e) => println!("{:?}", e),
            },
        }
    } else {
        eprintln!("You have to supply a filename")
    }
    Ok(())
}
