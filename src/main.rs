pub mod achievements;
pub mod bowl;
pub mod filling;

use std::fs::read_to_string;
use std::path::Path;
use bowl::parser::parse;
use filling::{step, Alloc, Net, RBag};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // TODO: 
    // bowl is necessary if it provides: lambda and application syntax
    // switch expressions
    // tuples and pattern matching for tuples
    // "connections", like lets but no new variables
    // flying lambdas? are they dynamic variables?
    let source = read_to_string(Path::new("examples/test.bol"))?;
    let program = parse(&source)?;
    println!("{}", program);

    Ok(())
}

fn try_filling () -> Result<(), Box<dyn std::error::Error>> {
    let program = read_to_string(Path::new("examples/test.fil"))?;
    let nodes = filling::parse(&program)?;
    let rom = nodes.into_rom();
    println!("{}", rom);

    let mut net = Net::new(0xf);
    let mut redex = RBag::new();
    let mut alloc = Alloc::new();

    let main = rom.load_to(&mut redex, &mut alloc, &mut net).unwrap();

    println!("{}", net);
    println!("{}", redex);

    while step(&mut redex, &mut alloc, &mut net, &rom) {
        println!("{}", net);
        println!("{}", redex);
    }

    Ok(())
}