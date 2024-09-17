use super::{Def, Num, Pair, Port, Tag, FREE, ROM};
use std::collections::HashMap;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Addr(pub usize);

#[derive(Debug)]
pub enum Node {
    VAR(String),
    REF(String),
    ERA,
    NUM(Num),
    CON(Addr, Addr),
    DUP(Addr, Addr),
    OPR(Addr, Addr),
    SWI(Addr, Addr),
}

#[derive(Debug)]
pub struct DefNode {
    pub name: String,
    pub safe: bool,              // if safe, has no dups
    pub root: Addr,              // root port
    pub rbag: Vec<(Addr, Addr)>, // redex bag
    pub store: Vec<Node>,        // node buffer without variable nodes
}

fn push_in(x: Pair, t: Port) -> Pair {
    Pair::new(t, x.left())
}
fn push_node(acc: &mut Vec<Pair>, pair: Pair) -> super::Addr {
    let addr = acc.len();
    acc.push(pair);
    addr as super::Addr
}

impl DefNode {
    pub fn get(&self, addr: Addr) -> &Node {
        &self.store[addr.0]
    }
    pub fn get_mut(&mut self, addr: Addr) -> &mut Node {
        &mut self.store[addr.0]
    }
    pub fn verify(&self) {
        let mut count = HashMap::new();
        for node in &self.store {
            match node {
                Node::VAR(str) => {
                    let count = count.entry(str).or_insert(0);
                    *count += 1;
                }
                _ => (),
            }
        }
        for (str, c) in count {
            if c != 2 {
                panic!("Variable {} should be used two times only", str);
            }
        }
    }

    fn translate_pair(
        &self,
        tag: Tag,
        a: &Addr,
        b: &Addr,
        acc: &mut Vec<Pair>,
        var_map: &mut HashMap<String, Port>,
        def_map: &HashMap<String, super::Addr>,
    ) -> Port {
        let left = self.translate_node(a.clone(), acc, var_map, def_map);
        let right = self.translate_node(b.clone(), acc, var_map, def_map);
        let addr = push_node(acc, Pair::new(left.clone(), right.clone()));
        let port = Port::new(tag, addr as super::Addr);

        // variable nodes record the ports it connects to
        if left.tag() == super::VAR {
            let v = acc[left.addr() as usize].clone();
            acc[left.addr() as usize] = push_in(v, port.clone());
        }
        if right.tag() == super::VAR {
            let v = acc[right.addr() as usize].clone();
            acc[right.addr() as usize] = push_in(v, port.clone());
        }
        port
    }

    fn translate_node(
        &self,
        addr: Addr,
        acc: &mut Vec<Pair>,
        var_map: &mut HashMap<String, Port>,
        def_map: &HashMap<String, super::Addr>,
    ) -> Port {
        let node = self.get(addr);

        match node {
            Node::CON(a, b) => self.translate_pair(super::CON, a, b, acc, var_map, def_map),
            Node::DUP(a, b) => self.translate_pair(super::DUP, a, b, acc, var_map, def_map),
            Node::OPR(a, b) => self.translate_pair(super::OPR, a, b, acc, var_map, def_map),
            Node::SWI(a, b) => self.translate_pair(super::SWI, a, b, acc, var_map, def_map),
            Node::VAR(name) => {
                if var_map.contains_key(name) {
                    var_map[name].clone()
                } else {
                    let addr = push_node(acc, Pair::new(FREE, FREE));
                    let port = Port::new(super::VAR, addr as super::Addr);
                    var_map.insert(name.clone(), port.clone());
                    port
                }
            }
            Node::REF(name) => Port::new(super::REF, def_map[name]),
            Node::ERA => Port::new(super::ERA, 0),
            Node::NUM(num) => num.to_port(),
        }
    }

    pub fn to_def(&self, def_map: &HashMap<String, super::Addr>) -> Def {
        let mut acc = vec![Pair::new(FREE, FREE)];
        let mut var_map = HashMap::new();
        let mut rbag = vec![];
        for (a, b) in &self.rbag {
            let left = self.translate_node(a.clone(), &mut acc, &mut var_map, def_map);
            let right = self.translate_node(b.clone(), &mut acc, &mut var_map, def_map);
            rbag.push(Pair::new(left, right));
        }
        let root = self.translate_node(self.root.clone(), &mut acc, &mut var_map, def_map);
        Def {
            name: self.name.clone(),
            safe: self.safe,
            root,
            node: acc,
            rbag,
        }
    }
}

pub fn to_rom(nodes: Vec<DefNode>) -> ROM {
    let mut def_map = HashMap::new();
    let mut defs = vec![];
    for (i, node) in nodes.iter().enumerate() {
        node.verify();
        def_map.insert(node.name.clone(), i as super::Addr);
        defs.push(node.to_def(&def_map));
    }
    ROM { defs }
}
// visualization

// #[derive(Debug)]
// struct Names {
//     map: HashMap<String, Addr>,
//     count: HashMap<String, u8>,
//     counter: Addr,
// }
// impl Names {
//     fn new() -> Names {
//         Names {
//             map: HashMap::new(),
//             count: HashMap::new(),
//             counter: 0,
//         }
//     }
//     fn has(&self, name: &str) -> bool {
//         self.map.contains_key(name)
//     }
//     fn get(&mut self, name: &str) -> Addr {
//         let count = self.count.get_mut(name).unwrap();
//         *count += 1;
//         if *count > 2 {
//             panic!("Variable {} should be used two times only", name);
//         }
//         *self.map.get(name).unwrap()
//     }
//     fn put(&mut self, name: &str) -> Addr {
//         self.counter += 1;
//         self.map.insert(name.to_string(), self.counter);
//         self.count.insert(name.to_string(), 1);
//         self.counter
//     }
// }
