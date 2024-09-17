use super::ast::{Addr, DefNode, Node, ROMNode};
use super::{Num, NumTag};
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "filling/grammar.pest"]
struct Filling;

fn parse_pair(pair: Pair<Rule>, store: &mut Vec<Node>) -> (Node, Node, bool) {
    let mut pair = pair.into_inner();
    let left = parse_tree(pair.next().unwrap(), store);
    let right = parse_tree(pair.next().unwrap(), store);
    (left.0, right.0, left.1 && right.1)
}

fn push_node(node: Node, store: &mut Vec<Node>) -> Addr {
    let addr = Addr(store.len());
    store.push(node);
    addr
}

fn parse_op(s: &str) -> NumTag {
    match s {
        "+" => super::NADD,
        "-" => super::NSUB,
        "*" => super::NMUL,
        "/" => super::NDIV,
        "%" => super::NREM,
        "==" => super::NEQU,
        _ => panic!("Invalid operator"),
    }
}

fn parse_num(pair: Pair<Rule>) -> Num {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::u8 => Num::new_u8(pair.as_str().parse().unwrap()),
        Rule::opn => {
            let inner = pair.into_inner().next().unwrap();
            match inner.as_rule() {
                Rule::lop => {
                    let mut inner = inner.into_inner();
                    let num = inner.next().unwrap().as_str().parse().unwrap();
                    let op = parse_op(inner.next().unwrap().as_str());
                    Num::new(op, num)
                },
                Rule::rop => {
                    let mut inner = inner.into_inner();
                    let op = parse_op(inner.next().unwrap().as_str());
                    let num = inner.next().unwrap().as_str().parse().unwrap();
                    Num::new(op, num).reversed()
                },
                Rule::oprev => {
                    let op = parse_op(inner.into_inner().next().unwrap().as_str());
                    Num::new(super::NSYM, op | super::NREV)
                },
                Rule::prim => {
                    let op = parse_op(inner.as_str());
                    Num::new(super::NSYM, op)
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!()
    }
}

// returns the node and if it is safe
fn parse_node(pair: Pair<Rule>, store: &mut Vec<Node>) -> (Node, bool) {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::era => (Node::ERA, true),
        Rule::r#ref => {
            let name = pair.into_inner().next().unwrap().as_str().to_string();
            (Node::REF(name), true)
        }
        Rule::num => (Node::NUM(parse_num(pair)), true),
        Rule::con => {
            let (left, right, safe) = parse_pair(pair, store);
            (
                Node::CON(push_node(left, store), push_node(right, store)),
                safe,
            )
        }
        Rule::dup => {
            let (left, right, _) = parse_pair(pair, store);
            (
                Node::DUP(push_node(left, store), push_node(right, store)),
                false,
            )
        }
        Rule::opr => {
            let (left, right, safe) = parse_pair(pair, store);
            (
                Node::OPR(push_node(left, store), push_node(right, store)),
                safe,
            )
        }
        Rule::swi => {
            let (left, right, safe) = parse_pair(pair, store);
            (
                Node::SWI(push_node(left, store), push_node(right, store)),
                safe,
            )
        }
        _ => unreachable!(),
    }
}

fn parse_tree(pair: Pair<Rule>, store: &mut Vec<Node>) -> (Node, bool) {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::name => {
            let name = pair.as_str().to_string();
            (Node::VAR(name), true)
        }
        Rule::node => parse_node(pair, store),
        _ => unreachable!(),
    }
}

fn parse_exp(pair: Pair<Rule>, store: &mut Vec<Node>, rbag: &mut Vec<(Addr, Addr)>) -> (Addr, bool) {
    let mut pair = pair.into_inner();
    let (root, safe) = parse_tree(pair.next().unwrap(), store);
    let mut safe = safe;
    for conn in pair {
        let mut conn = conn.into_inner();
        let (left, lsafe) = parse_tree(conn.next().unwrap(), store);
        let (right, rsafe) = parse_tree(conn.next().unwrap(), store);
        rbag.push((push_node(left, store), push_node(right, store)));
        safe = safe && lsafe && rsafe;
    }
    (push_node(root, store), safe)
}

fn parse_def(pair: Pair<Rule>) -> DefNode {
    let mut pair = pair.into_inner();
    let name = pair.next().unwrap().as_str().to_string();

    let mut node = vec![];
    let mut rbag = vec![];
    let (root, safe) = parse_exp(pair.next().unwrap(), &mut node, &mut rbag);

    DefNode {
        name,
        safe,
        root,
        rbag,
        store: node,
    }
}
pub fn parse(s: &str) -> Result<ROMNode, pest::error::Error<Rule>> {
    let parsed = Filling::parse(Rule::main, s)?.next().unwrap();
    let inner = parsed.into_inner();

    let mut main = None;
    let mut defs = Vec::new();
    for def in inner {
        if let Rule::def = def.as_rule() {
            let def = parse_def(def);
            if def.name == "main" {
                main = Some(def);
            } else {
                defs.push(def);
            }
        }
    }
    if let Some(main) = main {
        Ok(ROMNode::new(main, defs))
    } else {
        panic!("No main definition found");
    }
}
