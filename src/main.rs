pub mod net;
pub mod filling;
use filling::parser::parse_rom;
use filling::{load_def, Net, Redex};

const PROGRAM: &str = r"
foo = (z z)
main = (a b)
  where
    (b a) ~ (x (y *))
    {y x} ~ @foo
";

fn main() -> Result<(),  Box<dyn std::error::Error>> {
    let rom = parse_rom(PROGRAM)?;
    println!("{}", rom);

    let mut net = Net::new(0xf);
    let mut redex = Redex::new();
    load_def(&mut redex, &mut net, &rom.defs[1]);
    load_def(&mut redex, &mut net, &rom.defs[0]);

    println!("{}", net);
    println!("{}", redex);

    Ok(())
}
