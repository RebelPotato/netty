pub mod net;
pub mod filling;
use filling::ast::to_rom;
use filling::parser::parse;
use filling::{load_def, Net, Redex};

const PROGRAM: &str = r"
foo = (z z)
main = (a b)
  where
    (b a) ~ (x (y *))
    {y x} ~ @foo
";

fn main() -> Result<(),  Box<dyn std::error::Error>> {
    let nodes = parse(PROGRAM)?;
    let rom = to_rom(nodes);
    println!("{}", rom);

    let mut net = Net::new(0xf);
    let mut redex = Redex::new();
    load_def(&mut redex, &mut net, &rom.defs[1]);
    load_def(&mut redex, &mut net, &rom.defs[0]);

    println!("{}", net);
    println!("{}", redex);

    Ok(())
}
