// reference implementation for filling VM
pub mod parser;
pub mod ast;

use std::{
    collections::{HashMap, VecDeque},
    fmt::{self, Display, Formatter},
};

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
pub const MAX_ADDR: Addr = 0x1FFF;

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

pub const NSYM: NumTag = 0x00; // a symbol
pub const NTU8: NumTag = 0x01; // just an 8-bit number
pub const NADD: NumTag = 0x02; // addition
pub const NSUB: NumTag = 0x03; // subtraction
pub const NMUL: NumTag = 0x04; // multiplication
pub const NDIV: NumTag = 0x05; // division
pub const NREM: NumTag = 0x06; // remainder
pub const NEQU: NumTag = 0x07; // equality
                               // TODO: more operations

pub type NumValue = u8; // 8-bit value
pub const NUM_MAX: NumValue = 0xFF;
pub struct Num(u16); // 13-bit tag ~ value

impl Num {
    pub fn new(tag: NumTag, value: NumValue) -> Num {
        Num((tag as u16) << 8 | value as u16)
    }
    pub fn new_u8(value: NumValue) -> Num {
        Num::new(NTU8, value)
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
    pub fn partial(a: Self, b: Self) -> Self {
        // a: SYM OP, b: U8 NUM
        // return: OP NUM
        Num::new(a.value(), b.value())
    }
    pub fn is_num(&self) -> bool {
        self.tag() == NTU8
    }
    pub fn is_sym(&self) -> bool {
        self.tag() == NSYM
    }
    pub fn is_op(&self) -> bool {
        self.tag() >= NADD
    }
    pub fn operate(a: Self, b: Self) -> Self {
        match (a.is_sym(), b.is_sym()) {
            (true, true) => Num::new_u8(0),
            (true, false) => Num::partial(a, b),
            (false, true) => Num::partial(b, a),
            (false, false) => match (a.is_op(), b.is_op()) {
                (true, true) => Num::new_u8(0),
                (false, false) => Num::new_u8(0),
                (true, false) => Self::apply(a, b.value()),
                (false, true) => Self::apply(b, a.value()),
            },
        }
    }
    fn apply(op: Self, y: NumValue) -> Self {
        let x = op.value();
        match op.tag() {
            NADD => Num::new_u8(x.wrapping_add(y)),
            NSUB => Num::new_u8(x.wrapping_sub(y)),
            NMUL => Num::new_u8(x.wrapping_mul(y)),
            NDIV => Num::new_u8(x.wrapping_div(y)),
            NREM => Num::new_u8(x.wrapping_rem(y)),
            NEQU => Num::new_u8((x == y) as NumValue),
            _ => unreachable!(),
        }
    }
}

// Program memory
#[derive(Debug)]
pub struct Def {
    pub name: String,    // name
    pub safe: bool,      // if safe, has no dups
    pub root: Port,      // root port
    pub rbag: Vec<Pair>, // redex bag
    pub node: Vec<Pair>, // node buffer without variable nodes
}

// Definitions
#[derive(Debug)]
pub struct ROM {
    pub defs: Vec<Def>,
}

// VM memory
#[derive(Debug)]
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
    node: Vec<Pair>,
}

impl Net {
    pub fn new(size: usize) -> Self {
        Net {
            node: vec![Pair(0); size],
        }
    }
    pub fn node_is_free(&self, i: usize) -> bool {
        self.node[i] == Pair(0)
    }
    pub fn get(&self, i: usize) -> Pair {
        self.node[i].clone()
    }
    pub fn take(&mut self, i: usize) -> Pair {
        let pair = self.node[i].clone();
        self.node[i] = Pair(0);
        pair
    }
    pub fn put(&mut self, i: usize, pair: Pair) {
        self.node[i] = pair;
    }
}

pub struct Redex {
    nput: usize,      // next node allocation index
    nloc: Vec<usize>, // allocated node locations
    store: RBag,      // local redex bag
}

pub const MAX_ALLOC: usize = 0xFFF;
impl Redex {
    pub fn new() -> Self {
        Redex {
            nput: 0,
            nloc: vec![0; MAX_ALLOC],
            store: RBag::new(),
        }
    }
    pub fn push_redex(&mut self, redex: Pair) {
        self.store.push_redex(redex);
    }
    pub fn pop_redex(&mut self) -> Option<Pair> {
        self.store.pop_redex()
    }
    // allocate `num` nodes, return true if successful
    pub fn node_alloc(&mut self, net: &Net, num: usize) -> bool {
        let mut got = 0;
        let nlen = net.node.len();
        for _ in 0..net.node.len() {
            self.nput += 1; // index 0 reserved
            if self.nput < nlen || net.node_is_free(self.nput % nlen) {
                self.nloc[got] = self.nput % nlen;
                got += 1;
            }
            if got >= num {
                break;
            }
        }
        return got >= num;
    }
}

pub fn interact_link(redex: &mut Redex, net: &mut Net, a: Port, b: Port) -> bool {
    let (v, c) = if a.tag() == VAR { (a, b) } else { (b, a) };
    let vnode = net.take(v.addr() as usize);
    if vnode.right() == FREE {
        // this node is an indirection node and has been cleared
        redex.push_redex(Pair::new(vnode.left(), c));
        true
    } else {
        // this node becomes an indirection node
        let other = if c == vnode.left() { vnode.right() } else { vnode.left() };
        net.put(v.addr() as usize, Pair::new(other, FREE));
        false
    }
}
pub fn interact_void(_redex: &mut Redex, _net: &Net, _a: Port, _b: Port) -> bool {
    true
}
pub fn step(redex: &mut Redex, net: &mut Net) -> bool {
    let top = redex.pop_redex();
    if let Some(top) = top {
        let a = top.left();
        let b = top.right();
        let rule = Port::rule_for(a.clone(), b.clone());
        let success = match rule {
            VOID => interact_void(redex, net, a, b),
            _ => todo!(),
            // _ => unreachable!(),
        };
        if success {
            true
        } else {
            redex.push_redex(top);
            false
        }
    } else {
        false
    }
}

// loads a definition into memory,
// returning the def's root address in memory
pub fn load_def(redex: &mut Redex, net: &mut Net, def: &Def) -> Option<Port> {
    // calculate number of variables
    let node_count = def.node.len();
    let mut counter = node_count;
    let mut var_map = HashMap::new();
    for pair in def.node.iter() {
        let left = pair.left();
        let right = pair.right();
        if left.tag() == VAR && !var_map.contains_key(&left.addr()) {
            var_map.insert(left.addr(), counter);
            counter += 1;
        }
        if right.tag() == VAR && !var_map.contains_key(&right.addr()) {
            var_map.insert(right.addr(), counter);
            counter += 1;
        }
    }

    fn transfer_port(nloc: &Vec<usize>, p: Port, var_map: &HashMap<u16, usize>) -> Port {
        if p.tag() == VAR {
            Port::new(VAR, nloc[*var_map.get(&p.addr()).unwrap()] as Addr)
        } else {
            Port::new(p.tag(), nloc[p.addr() as usize] as Addr)
        }
    }
    fn transfer_pair(nloc: &Vec<usize>, pair: &Pair, var_map: &HashMap<u16, usize>) -> Pair {
        Pair::new(
            transfer_port(nloc, pair.left(), var_map),
            transfer_port(nloc, pair.right(), var_map),
        )
    }

    // variables are mapped to the bottom of the node buffer
    // this is an arbitrary choice
    if redex.node_alloc(net, counter) {
        // node i maps to nloc[i]
        // var i maps to nloc[var_map[i]]
        // load node buffer, then redex bag
        for (i, pair) in def.node.iter().enumerate() {
            net.put(redex.nloc[i], transfer_pair(&redex.nloc, pair, &var_map));
            // FREE is the initial value for variables, so no need to put them in manually
        }
        for (_, v) in var_map.iter() {
            // the variables need to be marked as something other than Port(0)
            // so that latter allocations won't overwrite them
            let pair = Pair::new(FREE, NONE);
            net.put(redex.nloc[*v], pair);
        }
        for pair in def.rbag.iter() {
            redex
                .store
                .push_redex(transfer_pair(&redex.nloc, pair, &var_map));
        }
        Some(transfer_port(&redex.nloc, def.root.clone(), &var_map))
    } else {
        None
    }
}

pub fn load_rom(redex: &mut Redex, net: &mut Net, rom: &ROM) {
    // for def in rom.defs.iter() {
    //     load_def(redex, net, def);
    // }
    todo!()
}

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
        } else if *self == NONE {
            write!(f, "  NONE  ")
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
            write!(f, "[{:04X}]\n{}\n", i, def)?;
        }
        write!(f, "")
    }
}

impl Display for RBag {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for (i, pair) in self.redex.iter().enumerate() {
            write!(f, "{:04X} {}\n", i, pair)?;
        }
        write!(f, "")
    }
}

impl Display for Net {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for (i, pair) in self.node.iter().enumerate() {
            write!(f, "{:04X} {}\n", i, pair)?;
        }
        write!(f, "")
    }
}

impl Display for Redex {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "REDEX\n")?;
        write!(f, "NLOC: ")?;
        for (i, loc) in self.nloc.iter().enumerate() {
            if i >= self.nput {
                break;
            }
            write!(f, "{:04X} ", loc)?;
            if i % 16 == 15 {
                write!(f, "\n")?;
            }
        }
        write!(f, "\n")?;
        write!(f, "========= STORE ========\n{}", self.store)?;
        write!(f, "========================\n")
    }
}
