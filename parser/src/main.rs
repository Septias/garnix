use parser::{lex, lexer::NixTokens, map_err};

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if let Some(filename) = &args.get(1) {
        let input = std::fs::read_to_string(filename)?;
        let tokens = lex(&input);
        match map_err(parser::parser::lambda(NixTokens(&tokens)), &input) {
            Ok(ast) => println!("{:#?}", ast),
            Err(e) => match e {
                nom::Err::Error(e) | nom::Err::Failure(e) => println!("{}", e),
                nom::Err::Incomplete(e) => println!("{:?}", e),
            },
        }
    } else {
        println!("You have to supply a filename")
    }
    Ok(())
}
