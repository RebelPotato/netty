// reference implementation for filling VM
pub mod ast;
pub mod parser;

use std::{
    collections::VecDeque,
    fmt::{self, Display, Formatter},
};

use crate::achievements::unlock_achievement;

pub use parser::parse;

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
pub const COPY: Rule = 0x3;
pub const ANNI: Rule = 0x4;
pub const COMM: Rule = 0x5;
pub const OPER: Rule = 0x6;
pub const SWIT: Rule = 0x7;

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
    pub fn is_literal(&self) -> bool {
        let tag = self.tag();
        tag == NUM || tag == ERA || tag == REF
    }

    pub fn rule_for(a: Port, b: Port) -> Rule {
        const TABLE: [[Rule; 8]; 8] = [
            //VAR   REF   ERA   NUM   CON   DUP   OPR   SWI
            [LINK, LINK, LINK, LINK, LINK, LINK, LINK, LINK], // VAR
            [LINK, VOID, VOID, VOID, CALL, CALL, CALL, CALL], // REF
            [LINK, VOID, VOID, VOID, COPY, COPY, COPY, COPY], // ERA
            [LINK, VOID, VOID, VOID, COPY, COPY, OPER, SWIT], // NUM
            [LINK, CALL, COPY, COPY, ANNI, COMM, COMM, COMM], // CON
            [LINK, CALL, COPY, COPY, COMM, ANNI, COMM, COMM], // DUP
            [LINK, CALL, COPY, OPER, COMM, COMM, ANNI, COMM], // OPR
            [LINK, CALL, COPY, SWIT, COMM, COMM, COMM, ANNI], // SWI
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

pub const NREV: NumTag = 0x10; // set fifth bit to reverse the operation

pub type NumValue = u8; // 8-bit value
pub const NUM_MAX: NumValue = 0xFF;

#[derive(Debug)]
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
    pub fn to_port(&self) -> Port {
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
    pub fn is_reversed(&self) -> bool {
        self.tag() & NREV != 0
    }
    pub fn reversed(&self) -> Self {
        Num(self.0 ^ (NREV as u16) << 8)
    }
    pub fn combine(a: Self, b: Self) -> Self {
        match (a.is_sym(), b.is_sym()) {
            (true, true) => {
                unlock_achievement(
                    "combining operators",
                    r#"
You tried to merge two operators together, which is clearly absurd.
"#,
                );
                Num::new_u8(0)
            }
            (true, false) => Num::partial(a, b),
            (false, true) => Num::partial(b, a),
            (false, false) => match (a.is_op(), b.is_op()) {
                (true, true) => {
                    unlock_achievement(
                        "combining partially applied operators",
                        r#"
You tried to merge two partially applied operators together, which is obviously
wrong.
"#,
                    );
                    Num::new_u8(0)
                }
                (false, false) => {
                    unlock_achievement(
                        "combining numbers",
                        r#"
You tried to merge two numbers together. What are you even thinking?
"#,
                    );
                    Num::new_u8(0)
                }
                (true, false) => Self::apply(a, b.value()),
                (false, true) => Self::apply(b, a.value()),
            },
        }
    }
    fn apply(op: Self, y: NumValue) -> Self {
        let x = op.value();
        let tag = op.tag() & !NREV;
        let (x, y) = if op.is_reversed() { (y, x) } else { (x, y) };
        match tag {
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

// loads a definition into memory,
// returning the def's root address in memory
pub fn load_def(redex: &mut RBag, alloc: &mut Alloc, net: &mut Net, def: &Def) -> Option<Port> {
    fn transfer_port(loc: &Vec<Addr>, p: Port) -> Port {
        if p.is_literal() || p == FREE {
            // literal nodes are left as is, as well as special meaning nodes
            p
        } else {
            Port::new(p.tag(), loc[(p.addr() - 1) as usize] as Addr)
        }
    }
    fn transfer_pair(loc: &Vec<Addr>, pair: &Pair) -> Pair {
        Pair::new(
            transfer_port(loc, pair.left()),
            transfer_port(loc, pair.right()),
        )
    }

    if alloc.request(net, def.node.len() - 1) {
        // node i (starting from 1) is mapped to loc[i-1]
        for (i, pair) in def.node.iter().enumerate().skip(1) {
            net.put(alloc.loc[i - 1], transfer_pair(&alloc.loc, pair));
        }
        for pair in def.rbag.iter() {
            redex.push_redex(transfer_pair(&alloc.loc, pair));
        }
        Some(transfer_port(&alloc.loc, def.root.clone()))
    } else {
        None
    }
}

impl ROM {
    pub fn load_def(
        &self,
        redex: &mut RBag,
        alloc: &mut Alloc,
        net: &mut Net,
        i: Addr,
    ) -> Option<Port> {
        load_def(redex, alloc, net, &self.defs[i as usize])
    }
    pub fn load_to(&self, redex: &mut RBag, alloc: &mut Alloc, net: &mut Net) -> Option<Port> {
        self.load_def(redex, alloc, net, 0)
    }
    pub fn is_safe(&self, i: Addr) -> bool {
        self.defs[i as usize].safe
    }
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
    pub fn node_is_free(&self, i: Addr) -> bool {
        // address 0 is reserved to represent FREE
        i != 0 && self.node[i as usize] == Pair(0)
    }
    pub fn get(&self, i: Addr) -> &Pair {
        &self.node[i as usize]
    }
    pub fn swap(&mut self, i: Addr, pair: Pair) -> Pair {
        let old = self.node[i as usize].clone();
        self.node[i as usize] = pair;
        old
    }
    pub fn take(&mut self, i: Addr) -> Pair {
        self.swap(i, Pair(0))
    }
    pub fn put(&mut self, i: Addr, pair: Pair) {
        self.node[i as usize] = pair;
    }
}

pub struct Alloc {
    nput: Addr,     // next node allocation index
    loc: Vec<Addr>, // allocated node locations
}

pub const MAX_ALLOC: usize = 0xFFF;
impl Alloc {
    pub fn new() -> Self {
        Alloc {
            nput: 0,
            loc: vec![0; MAX_ALLOC],
        }
    }
    // allocate `num` nodes, return true if successful
    pub fn request(&mut self, net: &Net, num: usize) -> bool {
        let mut got = 0;
        let nlen = net.node.len() as Addr;
        for _ in 0..nlen {
            self.nput += 1; // start allocation from 1
            if net.node_is_free(self.nput % nlen) {
                self.loc[got] = self.nput % nlen;
                got += 1;
            }
            if got >= num {
                break;
            }
        }
        return got >= num;
    }
}

fn interact_link(redex: &mut RBag, net: &mut Net, a: Port, b: Port) -> bool {
    let (v, c) = if a.tag() == VAR { (a, b) } else { (b, a) };
    let vnode = net.take(v.addr());
    if vnode.left() != FREE {
        // this node is an indirection node
        redex.push_redex(Pair::new(vnode.left(), c));
        true
    } else {
        // this node becomes an indirection node
        net.put(v.addr(), Pair::new(c, FREE));
        true
    }
}
fn interact_call(
    redex: &mut RBag,
    alloc: &mut Alloc,
    net: &mut Net,
    a: Port,
    b: Port,
    rom: &ROM,
) -> bool {
    let (r, c) = if a.tag() == REF { (a, b) } else { (b, a) };

    // this is copied from HVM, which calls it a "copy optimization"
    // cloning references means that references are lazily loaded and eagerly cloned
    // instead of being eagerly loaded and lazily cloned
    // optimization for recursive functions?
    // avoidable if loading incrementally using addresses?
    if c.tag() == DUP {
        if rom.is_safe(r.addr()) {
            interact_copy(redex, net, r, c)
        } else {
            unlock_achievement(
                "dup-rev violation",
                r#"
You have tried to duplicate an unsafe reference. While this is perfectly valid
on IC semantics (i.e. if you know what you're doing), this can lead to unsound 
reductions when compiling lambda terms to Filling. Maybe we'll add an "-unsafe"
flag to allow this in the future?
"#,
            );
            false
        }
    } else {
        let def = rom.load_def(redex, alloc, net, r.addr());
        if let Some(def) = def {
            redex.push_redex(Pair::new(def, c));
            true
        } else {
            false
        }
    }
}
fn interact_copy(redex: &mut RBag, net: &mut Net, a: Port, b: Port) -> bool {
    let (l, c) = if a.is_literal() { (a, b) } else { (b, a) };
    let cnode = net.take(c.addr());
    redex.push_redex(Pair::new(l.clone(), cnode.left()));
    redex.push_redex(Pair::new(l, cnode.right()));
    true
}
fn interact_anni(redex: &mut RBag, net: &mut Net, a: Port, b: Port) -> bool {
    let anode = net.take(a.addr());
    let bnode = net.take(b.addr());
    redex.push_redex(Pair::new(anode.left(), bnode.left()));
    redex.push_redex(Pair::new(anode.right(), bnode.right()));
    true
}
fn interact_comm(redex: &mut RBag, alloc: &mut Alloc, net: &mut Net, a: Port, b: Port) -> bool {
    if !alloc.request(net, 6) {
        return false;
    }

    let anode = net.take(a.addr());
    let bnode = net.take(b.addr());

    let node0 = a.addr();
    let node1 = b.addr();
    let node2 = alloc.loc[0];
    let node3 = alloc.loc[1];

    let port0 = Port::new(b.tag(), node0);
    let port1 = Port::new(b.tag(), node1);
    let port2 = Port::new(a.tag(), node2);
    let port3 = Port::new(a.tag(), node3);

    let var0 = alloc.loc[2];
    let var1 = alloc.loc[3];
    let var2 = alloc.loc[4];
    let var3 = alloc.loc[5];

    redex.push_redex(Pair::new(port0.clone(), anode.left()));
    redex.push_redex(Pair::new(port1.clone(), anode.right()));
    redex.push_redex(Pair::new(port2.clone(), bnode.left()));
    redex.push_redex(Pair::new(port3.clone(), bnode.right()));

    net.put(node0, Pair::new(Port::new(VAR, var0), Port::new(VAR, var1)));
    net.put(node1, Pair::new(Port::new(VAR, var2), Port::new(VAR, var3)));
    net.put(node2, Pair::new(Port::new(VAR, var0), Port::new(VAR, var2)));
    net.put(node3, Pair::new(Port::new(VAR, var1), Port::new(VAR, var3)));

    net.put(var0, Pair::new(port0.clone(), port2.clone()));
    net.put(var1, Pair::new(port0, port3.clone()));
    net.put(var2, Pair::new(port1.clone(), port2));
    net.put(var3, Pair::new(port1, port3));

    true
}
fn interact_oper(redex: &mut RBag, net: &mut Net, a: Port, b: Port) -> bool {
    let (o, c) = if a.tag() == OPR { (a, b) } else { (b, a) };
    let onode = net.take(o.addr());
    let d = onode.left();
    if d.tag() == NUM {
        let cnum = Num(c.addr());
        let dnum = Num(d.addr());
        let result = Num::combine(cnum, dnum);
        redex.push_redex(Pair::new(result.to_port(), onode.right()));
        true
    } else {
        net.put(o.addr(), Pair::new(c, onode.right()));
        redex.push_redex(Pair::new(o, d));
        true
    }
}
fn interact_swit(redex: &mut RBag, alloc: &mut Alloc, net: &mut Net, a: Port, b: Port) -> bool {
    let (s, c) = if a.tag() == SWI { (a, b) } else { (b, a) };
    let snode = net.take(s.addr());
    let cnum = Num(c.addr());
    if !cnum.is_num() {
        unlock_achievement(
            "matching on operators",
            r#"
You have passed an operator (like [+] or [2+]) to a switch node, which gets you
a type error! Switch nodes are only supposed to match on numbers, you know.
"#,
        )
    }
    if !alloc.request(net, 1) {
        return false;
    }
    let cv = cnum.value();
    let val = Num::new_u8(cv >> 1).to_port();
    let node = alloc.loc[0];

    // snode turns into ((val C) *) or (* (val C))
    redex.push_redex(Pair::new(snode.left(), Port::new(CON, s.addr())));

    let pair = if cv & 1 == 0 {
        Pair::new(Port::new(CON, node), Port::new(ERA, 0))
    } else {
        Pair::new(Port::new(ERA, 0), Port::new(CON, node))
    };
    net.put(s.addr(), pair);
    net.put(node, Pair::new(val, c));
    true
}

pub fn step(redex: &mut RBag, alloc: &mut Alloc, net: &mut Net, rom: &ROM) -> bool {
    let top = redex.pop_redex();
    if let Some(top) = top {
        let a = top.left();
        let b = top.right();
        let rule = Port::rule_for(a.clone(), b.clone());
        let success = match rule {
            LINK => interact_link(redex, net, a, b),
            CALL => interact_call(redex, alloc, net, a, b, rom),
            VOID => true, // interact_void clears two nodes by doing nothing
            COPY => interact_copy(redex, net, a, b),
            ANNI => interact_anni(redex, net, a, b),
            COMM => interact_comm(redex, alloc, net, a, b),
            OPER => interact_oper(redex, net, a, b),
            SWIT => interact_swit(redex, alloc, net, a, b),
            _ => unreachable!(),
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

// Visualizations
fn num_tag_to_str(tag: NumTag) -> &'static str {
    match tag {
        NSYM => "OP",
        NTU8 => "U8",
        NADD => "+ ",
        NSUB => "- ",
        NMUL => "* ",
        NDIV => "/ ",
        NREM => "% ",
        NEQU => "==",
        _ => unreachable!(),
    }
}

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

impl Display for Num {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let tag = self.tag();
        if tag < NREV {
            if tag == NSYM {
                if self.value() < NREV {
                    write!(f, "{}", num_tag_to_str(self.value()))?;
                } else {
                    write!(
                        f,
                        ":{}",
                        num_tag_to_str(self.value() & !NREV).chars().nth(0).unwrap()
                    )?;
                }
            } else {
                write!(f, "{:02X}", self.value())?;
            }
            write!(f, "{}", num_tag_to_str(tag))
        } else {
            let tag = tag & !NREV;
            write!(f, "{}", num_tag_to_str(tag))?;
            if tag == NSYM {
                write!(f, "{}", num_tag_to_str(self.value()))
            } else {
                write!(f, "{:02X}", self.value())
            }
        }
    }
}

impl Display for Port {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if *self == FREE {
            write!(f, "  FREE  ")
        } else if *self == NONE {
            write!(f, "  NONE  ")
        } else if self.tag() == NUM {
            write!(f, "NUM {}", Num(self.addr()))
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
        Ok(())
    }
}

impl Display for RBag {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for (i, pair) in self.redex.iter().enumerate() {
            write!(f, "{:04X} {}\n", i, pair)?;
        }
        Ok(())
    }
}

impl Display for Net {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for (i, pair) in self.node.iter().enumerate() {
            write!(f, "{:04X} {}\n", i, pair)?;
        }
        Ok(())
    }
}

impl Display for Alloc {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "REDEX\n")?;
        write!(f, "loc: ")?;
        for (i, loc) in self.loc.iter().enumerate() {
            if i as Addr >= self.nput {
                break;
            }
            write!(f, "{:04X} ", loc)?;
            if i % 16 == 15 {
                write!(f, "\n")?;
            }
        }
        write!(f, "\n")
    }
}
