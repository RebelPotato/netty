pub mod net;
pub mod filling;
use filling::parser::parse_rom;

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
    Ok(())
}
