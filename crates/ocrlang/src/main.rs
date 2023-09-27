use std::io::{self, Write};

fn main() -> io::Result<()> {
    let mut input = String::new();
    let mut stdout = io::stdout();
    let stdin = io::stdin();

    loop {
        write!(stdout, "> ")?;
        stdout.flush()?;
        stdin.read_line(&mut input)?;

        let parse = parser::parse(&input);
        writeln!(stdout, "Parse tree:")?;
        println!("{}", parse.debug_tree());

        let syntax = parse.syntax();

        let root = ast::Root::cast(syntax).unwrap();

        writeln!(stdout, "AST:")?;
        dbg!(root.stmts().collect::<Vec<_>>());

        writeln!(stdout, "HIR:")?;
        dbg!(hir::lower(root));

        input.clear();
    }
}
