pub mod achievements;
pub mod bowl;
pub mod filling;

use std::fs::read_to_string;
use std::path::Path;
use filling::parser::parse;
use filling::{step, Alloc, Net, RBag};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = read_to_string(Path::new("examples/test.fil"))?;
    let nodes = parse(&program)?;
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

    // now to extract the result from the net

    Ok(())
}
