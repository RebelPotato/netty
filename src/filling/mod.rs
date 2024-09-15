// reference implementation for filling VM
pub mod parser;

use std::{collections::VecDeque, fmt::{self, Display, Formatter}};

/* Ports and Pairs */
// 3-bit tag
pub type Tag = u8;

pub const VAR: Tag = 0x0; // variable
pub const REF: Tag = 0x1; // reference
pub const ERA: Tag = 0x2; // eraser
pub const NUM: Tag = 0x3; // number
pub const CON: Tag = 0x4; // constructor
pub const DUP: Tag = 0x5; // duplicator
pub const OPR: Tag = 0x6; // operator
pub const SWI: Tag = 0x7; // switch

// 13-bit address
pub type Addr = u16;

// 8-bit rule
pub type Rule = u8;

pub const LINK: Rule = 0x0;
pub const CALL: Rule = 0x1;
pub const VOID: Rule = 0x2;
pub const ERAS: Rule = 0x3;
pub const ANNI: Rule = 0x4;
pub const COMM: Rule = 0x5;
pub const OPER: Rule = 0x6;
pub const SWIT: Rule = 0x7;

pub fn is_high_priority(rule: Rule) -> bool {
    // LINK, VOID, ERAS, ANNI
    (0b00011101 >> rule) & 1 != 0
}
// 16-bit port: tag ~ addr
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Port(u16);

pub const FREE: Port = Port(0x0000);
pub const ROOT: Port = Port(0xFFF8);
pub const NONE: Port = Port(0xFFFF);

// 32-bit pair: left port ~ right port
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Pair(u32);

impl Port {
    pub fn new(tag: Tag, addr: Addr) -> Port {
        Port((tag as u16) << 13 | addr)
    }
    pub fn tag(&self) -> Tag {
        (self.0 >> 13) as Tag
    }
    pub fn addr(&self) -> Addr {
        self.0 & 0x1FFF
    }

    pub fn rule_for(a: Port, b: Port) -> Rule {
        const TABLE: [[Rule; 8]; 8] = [
            //VAR  REF  ERA  NUM  CON  DUP  OPR  SWI
            [LINK, LINK, LINK, LINK, LINK, LINK, LINK, LINK], // VAR
            [LINK, VOID, VOID, VOID, CALL, CALL, CALL, CALL], // REF
            [LINK, VOID, VOID, VOID, ERAS, ERAS, ERAS, ERAS], // ERA
            [LINK, VOID, VOID, VOID, ERAS, ERAS, OPER, SWIT], // NUM
            [LINK, CALL, ERAS, ERAS, ANNI, COMM, COMM, COMM], // CON
            [LINK, CALL, ERAS, ERAS, COMM, ANNI, COMM, COMM], // DUP
            [LINK, CALL, ERAS, OPER, COMM, COMM, ANNI, COMM], // OPR
            [LINK, CALL, ERAS, SWIT, COMM, COMM, COMM, ANNI], // SWI
        ];
        return TABLE[a.tag() as usize][b.tag() as usize];
    }
}

impl Pair {
    pub fn new(left: Port, right: Port) -> Pair {
        Pair((left.0 as u32) << 16 | right.0 as u32)
    }
    pub fn left(&self) -> Port {
        Port((self.0 >> 16) as u16)
    }
    pub fn right(&self) -> Port {
        Port(self.0 as u16)
    }
}

// Numbers

// 5-bit tag
pub type NumTag = u8; 

pub const U8: NumTag = 0x0; // just an 8-bit number

pub type NumValue = u8; // 8-bit value
pub const NUM_MAX: NumValue = 0xFF;
pub struct Num(u16); // tag ~ value

impl Num {
    pub fn new(tag: NumTag, value: NumValue) -> Num {
        Num((tag as u16) << 8 | value as u16)
    }
    pub fn tag(&self) -> NumTag {
        (self.0 >> 8) as NumTag
    }
    pub fn value(&self) -> NumValue {
        self.0 as NumValue
    }
    pub fn as_port(&self) -> Port {
        Port::new(NUM, self.0 as Addr)
    }
}

// Program memory
#[derive(Debug)]
pub struct Def {
    pub name: String,    // def name
    pub safe: bool,      // if safe, has no dups
    pub root: Port,      // root port
    pub rbag: Vec<Pair>, // def redex bag
    pub node: Vec<Pair>, // def node buffer
}

// Definitions
#[derive(Debug)]
pub struct ROM {
    pub defs: Vec<Def>,
}

// VM memory
pub struct RBag {
    pub redex: VecDeque<Pair>,
}

impl RBag {
    pub fn new() -> RBag {
        RBag {
            redex: VecDeque::new(),
        }
    }

    pub fn push_redex(&mut self, redex: Pair) {
        // let rule = Port::rule_for(redex.left(), redex.right());
        // if is_high_priority(rule) {
        //     self.redex.push_front(redex);
        // } else {
        //     self.redex.push_back(redex);
        // }
        self.redex.push_back(redex);
    }

    pub fn pop_redex(&mut self) -> Option<Pair> {
        self.redex.pop_front()
    }

    pub fn len(&self) -> usize {
        self.redex.len()
    }
}

pub struct Net {
    pub node: Vec<Pair>,
}

impl Net {
    pub fn new(node: Vec<Pair>) -> Self {
        Net { node }
    }
}

pub struct State {
    pub nput: usize, // next node allocation index
    pub vput: usize, // next vars allocation index
    pub nloc: Vec<usize>, // allocated node locations
    pub vloc: Vec<usize>, // allocated vars locations
    redex: RBag, // local redex bag
}

// impl State {
//     pub fn new() -> Self {
//         State {
//             nput: 0,
//             vput: 0,
//             nloc: vec![0; 0xFFF], // FIXME: move to a constant
//             vloc: vec![0; 0xFFF],
//             redex: RBag::new()
//         }
//     }
//     pub fn node_alloc(&mut self, net: &Net, num: usize) -> usize {
//         let mut got = 0;
//         let nlen = net.node.len();
//         for _ in 0..net.node.len() {
//           self.nput += 1; // index 0 reserved
//           if self.nput < nlen || net.node_is_free(self.nput % nlen) {
//             self.nloc[got] = self.nput % nlen;
//             got += 1;
//             //println!("ALLOC NODE {} {}", got, self.nput);
//           }
//           if got >= num {
//             break;
//           }
//         }
//         return got
//       }
    
//     pub fn interact_void(&mut self, _net: &Net, _a: Port, _b: Port) -> bool {
//         true
//     }
//     pub fn step(&mut self, net: &Net) -> bool {
//         let redex = self.redex.pop_redex();
//         if let Some(redex) = redex {
//             let a = redex.left();
//             let b = redex.right();
//             let rule = Port::rule_for(a, b);
//             let success = match rule {
//                 LINK => self.interact_link(net, a, b),
//                 CALL => self.interact_call(net, a, b),
//                 VOID => self.interact_void(net, a, b),
//                 ERAS => self.interact_eras(net, a, b),
//                 ANNI => self.interact_anni(net, a, b),
//                 COMM => self.interact_comm(net, a, b),
//                 OPER => self.interact_oper(net, a, b),
//                 SWIT => self.interact_swit(net, a, b),
//                 _ => unreachable!(),
//             };
//             if success {
//                 true
//             } else {
//                 self.redex.push_redex(redex);
//                 false
//             }
//         } else {
//             false
//         }
//     }
// }

// Visualizations

fn tag_to_str(tag: Tag) -> &'static str {
    match tag {
        VAR => "VAR",
        REF => "REF",
        ERA => "ERA",
        NUM => "NUM",
        CON => "CON",
        DUP => "DUP",
        OPR => "OPR",
        SWI => "SWI",
        _ => unreachable!(),
    }
}

impl Display for Port {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if *self == FREE {
            write!(f, "  FREE  ")
        } else {
            write!(f, "{} {:04X}", tag_to_str(self.tag()), self.addr())
        }
    }
}

impl Display for Pair {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "[{}|{}]", self.left(), self.right())
    }
}

impl Display for Def {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.safe {
            write!(f, " (unsafe)")?;
        }
        write!(f, " = {}\n========= RBAG =========\n", self.root)?;
        for (i, pair) in self.rbag.iter().enumerate() {
            write!(f, "{:04X} {}\n", i, pair)?;
        }
        write!(f, "========= NODE =========\n")?;
        for (i, pair) in self.node.iter().enumerate() {
            write!(f, "{:04X} {}\n", i, pair)?;
        }
        write!(f, "========================\n")
    }
}

impl Display for ROM {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for (i, def) in self.defs.iter().enumerate() {
            write!(f, "Definition {:04X}:\n\n{}\n", i, def)?;
        }
        write!(f, "")
    }
}