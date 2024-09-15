pub mod net;
use std::rc::Rc;
use net::{parse_eqs, Name};

const PROGRAM: &str = r"
id = \x. x
one = \f1. \x1. f1 x1

double = \nd. \fd. \xd. nfd0 (nfd1 xd)
{nfd0 nfd1} = nd fd

// two = d1 one
// four = d2 two
// eight = d3 four
// t16 = d4 eight
// t32 = d5 t16
// t64 = d6 t32
// {d1 *} = double
// {d2 d21} = d11
// {d3 d31} = d21
// {d4 d41} = d31
// {d5 d51} = d41
// d6 = d51

{d0 *} = double
d0
";

fn main() {
    let result = Rc::new(Name::new(0));
    let mut eqs = parse_eqs(PROGRAM, result.clone()).unwrap();

    let mut steps = 0u64;
    net::dump(&eqs);
    while eqs.len() > 0 {
        net::step(&mut eqs);
        steps += 1;
        net::dump(&eqs);
    }
    
    println!("{}", result);
    println!("Steps: {}", steps);
}
